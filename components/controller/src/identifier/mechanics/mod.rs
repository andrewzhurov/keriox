use keri_core::{oobi::error::OobiError, prefix::IdentifierPrefix, transport::TransportError};

use crate::communication::SendingError;

use self::{broadcast::BroadcastingError, query_mailbox::ResponseProcessingError};


pub mod query_mailbox;
pub mod kel_managing;
pub mod broadcast;
mod mailbox;
pub mod delegate;
pub mod group;
pub mod notify_witness;
pub mod watcher_configuration;
pub mod tel_managing;


#[derive(Debug, thiserror::Error)]
pub enum MechanicsError {
    #[error(transparent)]
    SendingError(#[from] SendingError),

    #[error("Transport error: {0}")]
    Transport(#[from] TransportError),

    #[error("Can't lock")]
    LockingError,

    #[error("transparent")]
    EventProcessingError(#[from] keri_core::error::Error),

    #[error(transparent)]
    ResponseProcessingError(#[from] ResponseProcessingError),

    #[error("No kel events for {0} saved")]
    UnknownIdentifierError(IdentifierPrefix),

    #[error("Can't generate event: {0}")]
    EventGenerationError(String),

    #[error("Not group participant")]
    NotGroupParticipantError,

    #[error("Error: {0}")]
    OtherError(String),

    #[error("Wrong event type")]
    WrongEventTypeError,

    #[error("Wrong event format")]
    EventFormatError,

    #[error("Inception event error: {0}")]
    InceptionError(String),

    #[error("Improper witness prefix, should be basic prefix")]
    WrongWitnessPrefixError,

    #[error("Oobi error: {0}")]
    OobiError(#[from] OobiError),

    #[error("Broadcasting error: {0}")]
    BroadcastingError(#[from] BroadcastingError)
}

