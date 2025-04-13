pub mod delegated;
pub mod inception;
pub mod interaction;
pub mod rotation;

use crate::{
    error::Error,
    event_message::{EventTypeTag, Typeable},
    state::{EventSemantics, IdentifierState},
};
use serde::{Deserialize, Serialize};

pub use self::{
    delegated::DelegatedInceptionEvent, inception::InceptionEvent, interaction::InteractionEvent,
    rotation::RotationEvent,
};

/// Event Data
///
/// Event Data conveys the semantic content of a KERI event.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(untagged, rename_all = "lowercase")]
pub enum EventData {
    Dip(DelegatedInceptionEvent),
    Icp(InceptionEvent),
    Rot(RotationEvent),
    Ixn(InteractionEvent),
    Drt(RotationEvent),
}

use crate::event::sections::KeyConfig;
impl EventData {
    pub fn get_key_config(&self) -> Option<&KeyConfig> {
        match self {
            Self::Dip(dip) => Some(&dip.inception_data.key_config),
            Self::Icp(icp) => Some(&icp.key_config),
            Self::Rot(rot) => Some(&rot.key_config),
            Self::Drt(drt) => Some(&drt.key_config),
            _ => None,
        }
    }
}

impl EventSemantics for EventData {
    fn apply_to(&self, state: IdentifierState) -> Result<IdentifierState, Error> {
        match self {
            Self::Icp(e) => e.apply_to(state),
            Self::Rot(e) => e.apply_to(state),
            Self::Ixn(e) => e.apply_to(state),
            Self::Dip(e) => e.apply_to(state),
            Self::Drt(e) => e.apply_to(state),
        }
    }
}

impl From<EventData> for EventTypeTag {
    fn from(ed: EventData) -> Self {
        match ed {
            EventData::Icp(_) => EventTypeTag::Icp,
            EventData::Rot(_) => EventTypeTag::Rot,
            EventData::Ixn(_) => EventTypeTag::Ixn,
            EventData::Dip(_) => EventTypeTag::Dip,
            EventData::Drt(_) => EventTypeTag::Drt,
        }
    }
}

impl Typeable for EventData {
    type TypeTag = EventTypeTag;
    fn get_type(&self) -> EventTypeTag {
        self.into()
    }
}
