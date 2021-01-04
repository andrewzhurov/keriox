use super::{super::sections::seal::LocationSeal, EventData};
use super::{InceptionEvent, RotationEvent};
use crate::{
    derivation::self_addressing::SelfAddressing,
    error::Error,
    event::{Event, EventMessage, SerializationFormats},
    prefix::IdentifierPrefix,
    state::{EventSemantics, IdentifierState},
};
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct DelegatedInceptionEvent {
    #[serde(flatten)]
    pub inception_data: InceptionEvent,

    pub seal: LocationSeal,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct DelegatedRotationEvent {
    #[serde(flatten)]
    pub rotation_data: RotationEvent,

    pub seal: LocationSeal,
}

impl DelegatedInceptionEvent {
    /// Incept Self Addressing
    ///
    /// Takes the inception data and creates an EventMessage based on it, with
    /// using the given format and deriving a Self Addressing Identifier with the
    /// given derivation method
    pub fn incept_self_addressing(
        self,
        derivation: SelfAddressing,
        format: SerializationFormats,
    ) -> Result<EventMessage, Error> {
        let prefix = IdentifierPrefix::SelfAddressing(derivation.derive(
            &EventMessage::get_delegated_inception_data(&self, derivation, format)?,
        ));
        EventMessage::new(
            Event {
                prefix,
                sn: 0,
                event_data: EventData::Dip(self),
            },
            format,
        )
    }
}

impl EventSemantics for DelegatedInceptionEvent {
    fn apply_to(&self, state: IdentifierState) -> Result<IdentifierState, Error> {
        Ok(IdentifierState {
            delegator: Some(self.seal.prefix.clone()),
            ..self.inception_data.apply_to(state)?
        })
    }
}
impl EventSemantics for DelegatedRotationEvent {
    fn apply_to(&self, state: IdentifierState) -> Result<IdentifierState, Error> {
        if state.delegator == Some(self.seal.prefix.clone()) {
            self.rotation_data.apply_to(state)
        } else {
            Err(Error::SemanticError("Wrong delegator".into()))
        }
    }
}
