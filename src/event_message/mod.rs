pub mod event_msg_builder;
pub mod serialization_info;
pub mod serializer;
pub mod signed_event_message;
pub mod signature;
pub mod dummy_event;

use std::cmp::Ordering;

use crate::{
    error::Error,
    event::{
        event_data::EventData,
        Event,
        sections::seal::SourceSeal,
    },
    prefix::{AttachedSignaturePrefix, IdentifierPrefix, SelfAddressingPrefix},
    state::{EventSemantics, IdentifierState}, derivation::{self_addressing::SelfAddressing },
};
use chrono::{DateTime, Local};
use serde::{Deserialize, Serialize, Serializer};
use serialization_info::*;

use self::{signed_event_message::SignedEventMessage, dummy_event::{DummyInceptionEvent, DummyEventMessage}};

pub trait CommonEvent {
    fn get_type(&self) -> String;
}

#[derive(Deserialize, Debug, Clone, PartialEq)]
pub struct EventMessage<D: CommonEvent> {
    /// Serialization Information
    ///
    /// Encodes the version, size and serialization format of the event
    #[serde(rename = "v")]
    pub serialization_info: SerializationInfo,

    #[serde(rename = "d")]
    pub digest: SelfAddressingPrefix,

    #[serde(flatten)]
    pub event: D,
}

impl<D: CommonEvent + Serialize + Clone> Serialize for EventMessage<D> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer {
        // Helper struct for adding `t` field to EventMessage serialization
        #[derive(Serialize)]
        struct TypedEventMessage<D> {
            v: SerializationInfo,
            t: String,
            d: SelfAddressingPrefix,
            #[serde(flatten)]
            event: D

        }
        impl<D: CommonEvent +  Clone> From<&EventMessage<D>> for TypedEventMessage<D> {
            fn from(em: &EventMessage<D>) -> Self {
                TypedEventMessage {
                    v: em.serialization_info,
                    t: em.event.get_type(),
                    d: em.digest.clone(),
                    event: em.event.clone(),
                }
            }
        }

        let tem: TypedEventMessage<_> = self.into();
        tem.serialize(serializer)
    }
}

#[derive(Serialize, Deserialize, PartialEq)]
pub struct TimestampedEventMessage {
    pub timestamp: DateTime<Local>,
    pub event_message: EventMessage<Event>,
}

impl TimestampedEventMessage {
    pub fn new(event: EventMessage<Event>) -> Self {
        Self {
            timestamp: Local::now(),
            event_message: event,
        }
    }
}

impl PartialOrd for TimestampedEventMessage {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(
            match self.event_message.event.sn == other.event_message.event.sn {
                true => Ordering::Equal,
                false => match self.event_message.event.sn > other.event_message.event.sn {
                    true => Ordering::Greater,
                    false => Ordering::Less,
                },
            },
        )
    }
}

impl Ord for TimestampedEventMessage {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.event_message.event.sn == other.event_message.event.sn {
            true => Ordering::Equal,
            false => match self.event_message.event.sn > other.event_message.event.sn {
                true => Ordering::Greater,
                false => Ordering::Less,
            },
        }
    }
}

impl Eq for TimestampedEventMessage {}

impl From<TimestampedEventMessage> for EventMessage<Event> {
    fn from(event: TimestampedEventMessage) -> EventMessage<Event> {
        event.event_message
    }
}

/// WARNING: timestamp will change on conversion to current time
impl From<EventMessage<Event>> for TimestampedEventMessage {
    fn from(event: EventMessage<Event>) -> TimestampedEventMessage {
        TimestampedEventMessage::new(event)
    }
}

impl<T: Clone + Serialize + CommonEvent> EventMessage<T> {
    pub fn new(event: T, format: SerializationFormats, derivation: &SelfAddressing) -> Result<Self, Error> {
        let dummy_event = DummyEventMessage::dummy_event(event.clone(), format, derivation)?;
        let digest = derivation.derive(&dummy_event.serialize()?);
        Ok(Self {
            serialization_info: dummy_event.serialization_info,
            event: event,
            digest,
        })
    }

    pub fn serialization(&self) -> SerializationFormats {
        self.serialization_info.kind
    }

    /// Serialize
    ///
    /// returns the serialized event message
    /// NOTE: this method, for deserialized events, will be UNABLE to preserve ordering
    pub fn serialize(&self) -> Result<Vec<u8>, Error> {
        self.serialization().encode(self)
    }
}

impl EventMessage<Event> {
    pub fn sign(
        &self,
        sigs: Vec<AttachedSignaturePrefix>,
        delegator_seal: Option<SourceSeal>,
    ) -> SignedEventMessage {
        SignedEventMessage::new(self, sigs, delegator_seal)
    }
}

impl EventSemantics for EventMessage<Event> {
    fn apply_to(&self, state: IdentifierState) -> Result<IdentifierState, Error> {
        // Update state.last with serialized current event message.
        match self.event.event_data {
            EventData::Icp(_) | EventData::Dip(_) => {
                if verify_identifier_binding(self)? {
                    self.event.apply_to(IdentifierState {
                        last: self.serialize()?,
                        ..state
                    })
                } else {
                    Err(Error::SemanticError(
                        "Invalid Identifier Prefix Binding".into(),
                    ))
                }
            }
            EventData::Rot(ref rot) => {
                if let Some(_) = state.delegator {
                    Err(Error::SemanticError(
                        "Applying non-delegated rotation to delegated state.".into(),
                    ))
                } else {
                    // Event may be out of order or duplicated, so before checking
                    // previous event hash binding and update state last, apply it
                    // to the state. It will return EventOutOfOrderError or
                    // EventDuplicateError in that cases.
                    self.event.apply_to(state.clone()).and_then(|next_state| {
                        if rot.previous_event_hash.verify_binding(&state.last) {
                            Ok(IdentifierState {
                                last: self.serialize()?,
                                ..next_state
                            })
                        } else {
                            Err(Error::SemanticError(
                                "Last event does not match previous event".into(),
                            ))
                        }
                    })
                }
            }
            EventData::Drt(ref drt) => self.event.apply_to(state.clone()).and_then(|next_state| {
                if let None = state.delegator {
                    Err(Error::SemanticError(
                        "Applying delegated rotation to non-delegated state.".into(),
                    ))
                } else if drt.previous_event_hash.verify_binding(&state.last) {
                    Ok(IdentifierState {
                        last: self.serialize()?,
                        ..next_state
                    })
                } else {
                    Err(Error::SemanticError(
                        "Last event does not match previous event".into(),
                    ))
                }
            }),
            EventData::Ixn(ref inter) => {
                self.event.apply_to(state.clone()).and_then(|next_state| {
                    if inter.previous_event_hash.verify_binding(&state.last) {
                        Ok(IdentifierState {
                            last: self.serialize()?,
                            ..next_state
                        })
                    } else {
                        Err(Error::SemanticError(
                            "Last event does not match previous event".to_string(),
                        ))
                    }
                })
            }

            _ => self.event.apply_to(state),
        }
    }
}

pub fn verify_identifier_binding(icp_event: &EventMessage<Event>) -> Result<bool, Error> {
    let event_data = &icp_event.event.event_data;
    match event_data {
        EventData::Icp(icp) => match &icp_event.event.prefix {
            IdentifierPrefix::Basic(bp) => Ok(icp.key_config.public_keys.len() == 1
                && bp == icp.key_config.public_keys.first().unwrap()),
            // TODO update with new inception process
            IdentifierPrefix::SelfAddressing(sap) => {
                Ok(sap.verify_binding(&DummyInceptionEvent::dummy_inception_data(
                    icp.clone(),
                    &sap.derivation,
                    icp_event.serialization(),
                )?))
            }
            IdentifierPrefix::SelfSigning(_ssp) => todo!(),
        },
        EventData::Dip(dip) => match &icp_event.event.prefix {
            IdentifierPrefix::SelfAddressing(sap) => Ok(sap.verify_binding(
                &DummyInceptionEvent::dummy_delegated_inception_data(
                    dip.clone(),
                    &sap.derivation,
                    icp_event.serialization(),
                )?,
            )),
            _ => todo!(),
        },
        _ => Err(Error::SemanticError("Not an ICP or DIP event".into())),
    }
}

#[cfg(test)]
mod tests {
    mod test_utils;

    use self::{test_utils::test_mock_event_sequence};
    use super::*;
    use crate::{
        derivation::{basic::Basic, self_addressing::SelfAddressing, self_signing::SelfSigning},
        event::{
            event_data::{inception::InceptionEvent, EventData},
            sections::KeyConfig,
            sections::{threshold::SignatureThreshold, InceptionWitnessConfig},
        },
        keys::{PrivateKey, PublicKey},
        prefix::{AttachedSignaturePrefix, IdentifierPrefix, Prefix}, state::KeyEventType,
    };
    use ed25519_dalek::Keypair;
    use rand::rngs::OsRng;

    #[test]
    fn basic_create() -> Result<(), Error> {
        // hi Ed!
        let kp0 = Keypair::generate(&mut OsRng);
        let kp1 = Keypair::generate(&mut OsRng);

        // get two ed25519 keypairs
        let pub_key0 = PublicKey::new(kp0.public.to_bytes().to_vec());
        let priv_key0 = PrivateKey::new(kp0.secret.to_bytes().to_vec());
        let (pub_key1, _priv_key1) = (
            PublicKey::new(kp1.public.to_bytes().to_vec()),
            PrivateKey::new(kp1.secret.to_bytes().to_vec()),
        );

        // initial signing key prefix
        let pref0 = Basic::Ed25519.derive(pub_key0);

        // initial control key hash prefix
        let pref1 = Basic::Ed25519.derive(pub_key1);
        let nxt = SelfAddressing::Blake3_256.derive(pref1.to_str().as_bytes());

        // create a simple inception event
        let icp = Event {
            prefix: IdentifierPrefix::Basic(pref0.clone()),
            sn: 0,
            event_data: EventData::Icp(InceptionEvent {
                key_config: KeyConfig::new(
                    vec![pref0.clone()],
                    Some(nxt.clone()),
                    Some(SignatureThreshold::Simple(1)),
                ),
                witness_config: InceptionWitnessConfig::default(),
                inception_configuration: vec![],
                data: vec![],
            }),
        };

        let icp_m = icp.to_message(SerializationFormats::JSON, &SelfAddressing::Blake3_256)?;

        // serialised message
        let ser = icp_m.serialize()?;

        // sign
        let sig = priv_key0.sign_ed(&ser)?;
        let attached_sig = AttachedSignaturePrefix::new(SelfSigning::Ed25519Sha512, sig, 0);

        assert!(pref0.verify(&ser, &attached_sig.signature)?);

        let signed_event = icp_m.sign( vec![attached_sig], None);

        let s_ = IdentifierState::default();

        let s0 = s_.apply(&signed_event)?;

        assert!(s0.current.verify(&ser, &signed_event.signatures)?);

        assert_eq!(s0.prefix, IdentifierPrefix::Basic(pref0.clone()));
        assert_eq!(s0.sn, 0);
        assert_eq!(s0.last, icp_m.serialize()?);
        assert_eq!(s0.current.public_keys.len(), 1);
        assert_eq!(s0.current.public_keys[0], pref0);
        assert_eq!(s0.current.threshold, SignatureThreshold::Simple(1));
        assert_eq!(s0.current.threshold_key_digest, Some(nxt));
        assert_eq!(s0.witnesses, vec![]);
        assert_eq!(s0.tally, 0);
        assert_eq!(s0.delegates, vec![]);

        Ok(())
    }

    #[test]
    fn self_addressing_create() -> Result<(), Error> {
        // hi Ed!
        let kp0 = Keypair::generate(&mut OsRng);
        let kp1 = Keypair::generate(&mut OsRng);
        let kp2 = Keypair::generate(&mut OsRng);

        // get two ed25519 keypairs
        let pub_key0 = PublicKey::new(kp0.public.to_bytes().to_vec());
        let priv_key0 = PrivateKey::new(kp0.secret.to_bytes().to_vec());
        let (pub_key1, sig_key_1) = (
            PublicKey::new(kp1.public.to_bytes().to_vec()),
            PrivateKey::new(kp1.secret.to_bytes().to_vec()),
        );

        // hi X!
        // let x = XChaCha20Poly1305::new((&priv_key0.into_bytes()[..]).into());

        // get two X25519 keypairs
        let (enc_key_0, _enc_priv_0) = (PublicKey::new(kp2.public.to_bytes().to_vec()), sig_key_1);
        let (enc_key_1, _enc_priv_1) = (
            PublicKey::new(kp2.public.to_bytes().to_vec()),
            PrivateKey::new(kp2.secret.to_bytes().to_vec()),
        );

        // initial key set
        let sig_pref_0 = Basic::Ed25519.derive(pub_key0);
        let enc_pref_0 = Basic::X25519.derive(enc_key_0);

        // next key set
        let sig_pref_1 = Basic::Ed25519.derive(pub_key1);
        let enc_pref_1 = Basic::X25519.derive(enc_key_1);

        // next key set pre-commitment
        let nexter_pref = SelfAddressing::Blake3_256.derive(
            [sig_pref_1.to_str(), enc_pref_1.to_str()]
                .join("")
                .as_bytes(),
        );

        let icp = InceptionEvent::new(
            KeyConfig::new(
                vec![sig_pref_0.clone(), enc_pref_0.clone()],
                Some(nexter_pref.clone()),
                Some(SignatureThreshold::default()),
            ),
            None,
            None,
        )
        .incept_self_addressing(SelfAddressing::Blake3_256, SerializationFormats::JSON)?;

        // serialised
        let serialized = icp.serialize()?;

        // sign
        let sk = priv_key0;
        let sig = sk.sign_ed(&serialized)?;
        let attached_sig = AttachedSignaturePrefix::new(SelfSigning::Ed25519Sha512, sig, 0);

        assert!(sig_pref_0.verify(&serialized, &attached_sig.signature)?);

        let signed_event = icp.sign(vec![attached_sig], None);

        let s_ = IdentifierState::default();

        let s0 = s_.apply(&signed_event)?;

        assert!(s0.current.verify(&serialized, &signed_event.signatures)?);

        assert_eq!(s0.prefix, icp.event.prefix);
        assert_eq!(s0.sn, 0);
        assert_eq!(s0.last, icp.serialize()?);
        assert_eq!(s0.current.public_keys.len(), 2);
        assert_eq!(s0.current.public_keys[0], sig_pref_0);
        assert_eq!(s0.current.public_keys[1], enc_pref_0);
        assert_eq!(s0.current.threshold, SignatureThreshold::default());
        assert_eq!(s0.current.threshold_key_digest, Some(nexter_pref));
        assert_eq!(s0.witnesses, vec![]);
        assert_eq!(s0.tally, 0);
        assert_eq!(s0.delegates, vec![]);

        Ok(())
    }

    #[test]
    fn test_basic_establishment_sequence() -> Result<(), Error> {
        // Sequence should contain Inception Event.
        let no_inception_seq = vec![KeyEventType::Rot, KeyEventType::Rot];
        assert!(test_mock_event_sequence(no_inception_seq).is_err());

        // Sequence can't start with Rotation Event.
        let rotation_first_seq = vec![KeyEventType::Rot, KeyEventType::Icp];
        assert!(test_mock_event_sequence(rotation_first_seq).is_err());

        // Sequence should contain exacly one Inception Event.
        let wrong_seq = vec![
            KeyEventType::Icp,
            KeyEventType::Rot,
            KeyEventType::Rot,
            KeyEventType::Icp,
        ];
        assert!(test_mock_event_sequence(wrong_seq).is_err());

        let ok_seq = vec![
            KeyEventType::Icp,
            KeyEventType::Rot,
            KeyEventType::Rot,
        ];
        assert!(test_mock_event_sequence(ok_seq).is_ok());

        // Wrong delegated events sequence.
        let wrong_delegated_sequence = vec![
            KeyEventType::Dip,
            KeyEventType::Drt,
            KeyEventType::Rot,
        ];
        assert!(test_mock_event_sequence(wrong_delegated_sequence).is_err());

        // Delegated events sequence.
        let delegated_sequence = vec![
            KeyEventType::Dip,
            KeyEventType::Drt,
            KeyEventType::Ixn,
        ];
        assert!(test_mock_event_sequence(delegated_sequence).is_ok());

        Ok(())
    }

    #[test]
    fn test_basic_sequence() -> Result<(), Error> {
        let ok_seq = vec![
            KeyEventType::Icp,
            KeyEventType::Ixn,
            KeyEventType::Ixn,
            KeyEventType::Ixn,
            KeyEventType::Rot,
            KeyEventType::Ixn,
        ];
        assert!(test_mock_event_sequence(ok_seq).is_ok());

        let delegated_sequence = vec![
            KeyEventType::Dip,
            KeyEventType::Drt,
            KeyEventType::Ixn,
            KeyEventType::Drt,
        ];
        assert!(test_mock_event_sequence(delegated_sequence).is_ok());

        Ok(())
    }
}
