use std::sync::Arc;

use crate::event_message::signed_event_message::{Message, SignedEventMessage};
#[cfg(feature = "query")]
use crate::{database::sled::SledEventDatabase, error::Error};

use super::{
    notification::{Notification, NotificationBus},
    validator::EventValidator,
    EventProcessor, Processor,
};

pub struct BasicProcessor(EventProcessor);

impl Processor for BasicProcessor {
    fn process(&self, message: Message) -> Result<(), Error> {
        self.process(message)
    }

    fn new(db: Arc<SledEventDatabase>) -> Self {
        Self::new(db)
    }
}

impl BasicProcessor {
    pub fn new(db: Arc<SledEventDatabase>) -> Self {
        let processor = EventProcessor::new(db, NotificationBus::default());
        Self(processor)
    }

    fn basic_processing_strategy(
        db: Arc<SledEventDatabase>,
        publisher: &NotificationBus,
        signed_event: SignedEventMessage,
    ) -> Result<(), Error> {
        let id = &signed_event.event_message.event.get_prefix();
        let validator = EventValidator::new(db.clone());
        match validator.validate_event(&signed_event) {
            Ok(_) => {
                db.add_kel_finalized_event(signed_event.clone(), id)?;
                publisher.notify(&Notification::KeyEventAdded(signed_event))
            }
            Err(Error::EventOutOfOrderError) => {
                publisher.notify(&Notification::OutOfOrder(signed_event))
            }
            Err(Error::NotEnoughReceiptsError) => {
                publisher.notify(&Notification::PartiallyWitnessed(signed_event))
            }
            Err(Error::NotEnoughSigsError) => {
                publisher.notify(&Notification::PartiallySigned(signed_event))
            }
            Err(Error::EventDuplicateError) => {
                db.add_duplicious_event(signed_event.clone(), id)?;
                publisher.notify(&Notification::DupliciousEvent(signed_event))
            }
            Err(e) => Err(e),
        }
    }

    /// Process
    ///
    /// Process a deserialized KERI message.
    /// Ignore not fully witness error and accept not fully witnessed events.
    pub fn process(&self, message: Message) -> Result<(), Error> {
        self.0
            .process(message, BasicProcessor::basic_processing_strategy)?;
        Ok(())
    }
}
