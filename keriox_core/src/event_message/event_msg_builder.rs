use crate::{
    derivation::{basic::Basic, self_addressing::SelfAddressing},
    error::Error,
    event::sections::key_config::nxt_commitment,
    event::{
        event_data::{
            delegated::DelegatedInceptionEvent, interaction::InteractionEvent,
            rotation::RotationEvent,
        },
        sections::{
            key_config::NextKeysData, threshold::SignatureThreshold, RotationWitnessConfig,
        },
        SerializationFormats,
    },
    event::{
        event_data::{inception::InceptionEvent, EventData},
        receipt::Receipt,
        sections::seal::Seal,
        sections::InceptionWitnessConfig,
        sections::KeyConfig,
        Event, EventMessage,
    },
    keys::PublicKey,
    prefix::{BasicPrefix, IdentifierPrefix, SelfAddressingPrefix},
};
use ed25519_dalek::Keypair;
use rand::rngs::OsRng;

use super::{EventTypeTag, KeyEvent};

pub struct EventMsgBuilder {
    event_type: EventTypeTag,
    prefix: IdentifierPrefix,
    sn: u64,
    key_threshold: SignatureThreshold,
    next_key_threshold: SignatureThreshold,
    keys: Vec<BasicPrefix>,
    next_keys: Vec<BasicPrefix>,
    next_keys_hashes: Option<Vec<SelfAddressingPrefix>>,
    prev_event: SelfAddressingPrefix,
    data: Vec<Seal>,
    delegator: IdentifierPrefix,
    witness_threshold: SignatureThreshold,
    witnesses: Vec<BasicPrefix>,
    witness_to_add: Vec<BasicPrefix>,
    witness_to_remove: Vec<BasicPrefix>,
    format: SerializationFormats,
    derivation: SelfAddressing,
}

impl EventMsgBuilder {
    pub fn new(event_type: EventTypeTag) -> Self {
        let mut rng = OsRng {};
        let kp = Keypair::generate(&mut rng);
        let nkp = Keypair::generate(&mut rng);
        let pk = PublicKey::new(kp.public.to_bytes().to_vec());
        let npk = PublicKey::new(nkp.public.to_bytes().to_vec());
        let basic_pref = Basic::Ed25519.derive(pk);
        EventMsgBuilder {
            event_type,
            prefix: IdentifierPrefix::default(),
            keys: vec![basic_pref],
            next_keys: vec![Basic::Ed25519.derive(npk)],
            key_threshold: SignatureThreshold::default(),
            next_key_threshold: SignatureThreshold::default(),
            sn: 1,
            prev_event: SelfAddressing::Blake3_256.derive(&[0u8; 32]),
            data: vec![],
            delegator: IdentifierPrefix::default(),
            witness_threshold: SignatureThreshold::Simple(0),
            witnesses: vec![],
            witness_to_add: vec![],
            witness_to_remove: vec![],
            format: SerializationFormats::JSON,
            derivation: SelfAddressing::Blake3_256,
            next_keys_hashes: None,
        }
    }

    pub fn with_prefix(self, prefix: &IdentifierPrefix) -> Self {
        EventMsgBuilder {
            prefix: prefix.clone(),
            ..self
        }
    }

    pub fn with_keys(self, keys: Vec<BasicPrefix>) -> Self {
        EventMsgBuilder { keys, ..self }
    }

    pub fn with_next_keys(self, next_keys: Vec<BasicPrefix>) -> Self {
        EventMsgBuilder { next_keys, ..self }
    }

    pub fn with_next_keys_hashes(self, next_keys: Vec<SelfAddressingPrefix>) -> Self {
        EventMsgBuilder {
            next_keys_hashes: Some(next_keys),
            ..self
        }
    }

    pub fn with_sn(self, sn: u64) -> Self {
        EventMsgBuilder { sn, ..self }
    }
    pub fn with_previous_event(self, prev_event: &SelfAddressingPrefix) -> Self {
        EventMsgBuilder {
            prev_event: prev_event.clone(),
            ..self
        }
    }

    pub fn with_seal(mut self, seals: Vec<Seal>) -> Self {
        self.data.extend(seals);
        EventMsgBuilder { ..self }
    }

    pub fn with_delegator(self, delegator: &IdentifierPrefix) -> Self {
        EventMsgBuilder {
            delegator: delegator.clone(),
            ..self
        }
    }

    pub fn with_threshold(self, threshold: &SignatureThreshold) -> Self {
        EventMsgBuilder {
            key_threshold: threshold.clone(),
            ..self
        }
    }

    pub fn with_next_threshold(self, threshold: &SignatureThreshold) -> Self {
        EventMsgBuilder {
            next_key_threshold: threshold.clone(),
            ..self
        }
    }

    pub fn with_witness_list(self, witnesses: &[BasicPrefix]) -> Self {
        EventMsgBuilder {
            witnesses: witnesses.to_vec(),
            ..self
        }
    }

    pub fn with_witness_to_add(self, witness_to_add: &[BasicPrefix]) -> Self {
        EventMsgBuilder {
            witness_to_add: witness_to_add.to_vec(),
            ..self
        }
    }

    pub fn with_witness_to_remove(self, witness_to_remove: &[BasicPrefix]) -> Self {
        EventMsgBuilder {
            witness_to_remove: witness_to_remove.to_vec(),
            ..self
        }
    }

    pub fn with_witness_threshold(self, witness_threshold: &SignatureThreshold) -> Self {
        EventMsgBuilder {
            witness_threshold: witness_threshold.clone(),
            ..self
        }
    }

    pub fn build(self) -> Result<EventMessage<KeyEvent>, Error> {
        let next_key_hash = if let Some(hashes) = self.next_keys_hashes {
            NextKeysData {
                threshold: self.next_key_threshold,
                next_key_hashes: hashes,
            }
        } else {
            nxt_commitment(self.next_key_threshold, &self.next_keys, &self.derivation)
        };
        let key_config = KeyConfig::new(self.keys, next_key_hash, Some(self.key_threshold));
        let prefix = if self.prefix == IdentifierPrefix::default() {
            let icp_data = InceptionEvent::new(key_config.clone(), None, None)
                .incept_self_addressing(self.derivation.clone(), self.format)?;
            icp_data.event.get_prefix()
        } else {
            self.prefix
        };

        Ok(match self.event_type {
            EventTypeTag::Icp => {
                let icp_event = InceptionEvent {
                    key_config,
                    witness_config: InceptionWitnessConfig {
                        tally: self.witness_threshold,
                        initial_witnesses: self.witnesses,
                    },
                    inception_configuration: vec![],
                    data: vec![],
                };

                match prefix {
                    IdentifierPrefix::Basic(_) => Event::new(prefix, 0, EventData::Icp(icp_event))
                        .to_message(self.format, &self.derivation)?,
                    IdentifierPrefix::SelfAddressing(_) => {
                        icp_event.incept_self_addressing(self.derivation, self.format)?
                    }
                    _ => todo!(),
                }
            }

            EventTypeTag::Rot => Event::new(
                prefix,
                self.sn,
                EventData::Rot(RotationEvent {
                    previous_event_hash: self.prev_event,
                    key_config,
                    witness_config: RotationWitnessConfig {
                        tally: self.witness_threshold,
                        prune: self.witness_to_remove,
                        graft: self.witness_to_add,
                    },
                    data: self.data,
                }),
            )
            .to_message(self.format, &self.derivation)?,
            EventTypeTag::Ixn => Event::new(
                prefix,
                self.sn,
                EventData::Ixn(InteractionEvent {
                    previous_event_hash: self.prev_event,
                    data: self.data,
                }),
            )
            .to_message(self.format, &self.derivation)?,
            EventTypeTag::Dip => {
                let icp_data = InceptionEvent {
                    key_config,
                    witness_config: InceptionWitnessConfig::default(),
                    inception_configuration: vec![],
                    data: vec![],
                };
                DelegatedInceptionEvent {
                    inception_data: icp_data,
                    delegator: self.delegator,
                }
                .incept_self_addressing(self.derivation, self.format)?
            }
            EventTypeTag::Drt => {
                let rotation_data = RotationEvent {
                    previous_event_hash: self.prev_event,
                    key_config,
                    witness_config: RotationWitnessConfig::default(),
                    data: self.data,
                };
                Event::new(prefix, self.sn, EventData::Drt(rotation_data))
                    .to_message(self.format, &self.derivation)?
            }
            _ => return Err(Error::SemanticError("Not key event".into())),
        })
    }
}

pub struct ReceiptBuilder {
    format: SerializationFormats,
    receipted_event: EventMessage<KeyEvent>,
}

impl Default for ReceiptBuilder {
    fn default() -> Self {
        let default_event = EventMsgBuilder::new(EventTypeTag::Icp).build().unwrap();
        Self {
            format: SerializationFormats::JSON,
            receipted_event: default_event,
        }
    }
}

impl ReceiptBuilder {
    pub fn with_format(self, format: SerializationFormats) -> Self {
        Self { format, ..self }
    }

    pub fn with_receipted_event(self, receipted_event: EventMessage<KeyEvent>) -> Self {
        Self {
            receipted_event,
            ..self
        }
    }

    pub fn build(&self) -> Result<EventMessage<Receipt>, Error> {
        let prefix = self.receipted_event.event.get_prefix();
        let sn = self.receipted_event.event.get_sn();
        let receipted_event_digest = self.receipted_event.get_digest();
        Receipt {
            receipted_event_digest,
            sn,
            prefix,
        }
        .to_message(self.format)
    }
}

#[test]
fn test_multisig_prefix_derivation() {
    // Keys taken from keripy: keripy/tests/core/test_eventing.py::test_multisig_digprefix (line 2255)
    let expected_event = br#"{"v":"KERI10JSON0001e7_","t":"icp","d":"EZrJQSdhdiyXNpEzHo-dR0EEbLfcIopBSImdLnQGOKkg","i":"EZrJQSdhdiyXNpEzHo-dR0EEbLfcIopBSImdLnQGOKkg","s":"0","kt":"2","k":["DSuhyBcPZEZLK-fcw5tzHn2N46wRCG_ZOoeKtWTOunRA","DVcuJOOJF1IE8svqEtrSuyQjGTd2HhfAkt9y2QkUtFJI","DT1iAhBWCkvChxNWsby2J0pJyxBIxbAtbLA0Ljx-Grh8"],"nt":"2","n":["E_IkdcjsIFrFba-LS1sJDjpec_4vM3XtIPa6D51GcUIw","EU28GjHFKeXzncPxgwlHQZ0iO7f09Y89vy-3VkZ23bBI","E2PRzip7UZ5UTA_1ucb5eoAzxeRS3sIThrSbZhdRaZY8"],"bt":"0","b":[],"c":[],"a":[]}"#;
    let keys: Vec<BasicPrefix> = vec![
        "DSuhyBcPZEZLK-fcw5tzHn2N46wRCG_ZOoeKtWTOunRA"
            .parse()
            .unwrap(),
        "DVcuJOOJF1IE8svqEtrSuyQjGTd2HhfAkt9y2QkUtFJI"
            .parse()
            .unwrap(),
        "DT1iAhBWCkvChxNWsby2J0pJyxBIxbAtbLA0Ljx-Grh8"
            .parse()
            .unwrap(),
    ];
    let next_keys: Vec<BasicPrefix> = vec![
        "DKPE5eeJRzkRTMOoRGVd2m18o8fLqM2j9kaxLhV3x8AQ"
            .parse()
            .unwrap(),
        "D1kcBE7h0ImWW6_Sp7MQxGYSshZZz6XM7OiUE5DXm0dU"
            .parse()
            .unwrap(),
        "D4JDgo3WNSUpt-NG14Ni31_GCmrU0r38yo7kgDuyGkQM"
            .parse()
            .unwrap(),
    ];

    let msg_builder = EventMsgBuilder::new(EventTypeTag::Icp)
        .with_keys(keys)
        .with_next_keys(next_keys)
        .with_threshold(&SignatureThreshold::Simple(2))
        .with_next_threshold(&SignatureThreshold::Simple(2));
    let msg = msg_builder.build().unwrap();

    assert_eq!(expected_event.to_vec(), msg.serialize().unwrap());
}