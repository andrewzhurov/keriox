use std::sync::Arc;

// #[cfg(feature = "async")]
// pub mod async_processing;
pub mod basic_processor;
pub mod escrow;
pub mod event_storage;
pub mod notification;
pub mod responder;
#[cfg(test)]
mod tests;
pub mod validator;
pub mod witness_processor;

use crate::{
    database::sled::SledEventDatabase,
    error::Error,
    event_message::signed_event_message::{
        Message, SignedEventMessage, TimestampedSignedEventMessage,
    },
    prefix::IdentifierPrefix,
    query::reply_event::ReplyRoute,
    state::IdentifierState,
};

use self::{
    notification::{Notification, NotificationBus},
    validator::EventValidator,
};

pub trait Processor {
    fn new(db_path: Arc<SledEventDatabase>) -> Self;
    fn process(&self, message: Message) -> Result<(), Error>;
}

pub struct EventProcessor {
    db: Arc<SledEventDatabase>,
    validator: EventValidator,
    publisher: NotificationBus,
}

impl EventProcessor {
    pub fn new(db: Arc<SledEventDatabase>, publisher: NotificationBus) -> Self {
        let validator = EventValidator::new(db.clone());
        Self {
            db,
            validator,
            publisher,
        }
    }

    /// Process
    ///
    /// Process a deserialized KERI message
    /// Update database based on event validation result.
    pub fn process<F>(&self, message: Message, processing_strategy: F) -> Result<(), Error>
    where
        F: Fn(Arc<SledEventDatabase>, &NotificationBus, SignedEventMessage) -> Result<(), Error>,
    {
        match message {
            Message::Event(signed_event) => {
                processing_strategy(self.db.clone(), &self.publisher, signed_event)
            }
            Message::NontransferableRct(rct) => {
                let id = &rct.body.event.prefix;
                match self.validator.validate_witness_receipt(&rct) {
                    Ok(_) => {
                        self.db.add_receipt_nt(rct.to_owned(), id)?;
                        self.publisher.notify(&Notification::ReceiptAccepted)
                    }
                    Err(Error::MissingEvent) => self
                        .publisher
                        .notify(&Notification::ReceiptOutOfOrder(rct.clone())),
                    Err(e) => Err(e),
                }
            }
            Message::TransferableRct(vrc) => {
                match self.validator.validate_validator_receipt(&vrc) {
                    Ok(_) => {
                        self.db.add_receipt_t(vrc.clone(), &vrc.body.event.prefix)?;
                        self.publisher.notify(&Notification::ReceiptAccepted)
                    }
                    Err(Error::MissingEvent) | Err(Error::EventOutOfOrderError) => self
                        .publisher
                        .notify(&Notification::TransReceiptOutOfOrder(vrc.clone())),
                    Err(e) => Err(e),
                }
            }
            #[cfg(feature = "query")]
            Message::Reply(rpy) => match rpy.reply.get_route() {
                ReplyRoute::Ksn(_, _) => match self.validator.process_signed_ksn_reply(&rpy) {
                    Ok(_) => {
                        self.db
                            .update_accepted_reply(rpy.clone(), &rpy.reply.get_prefix())
                        // self.publisher.notify(&Notification::ReplyUpdated)
                    }
                    Err(Error::EventOutOfOrderError) => {
                        self.publisher.notify(&Notification::KsnOutOfOrder(rpy))
                    }
                    Err(anything) => Err(anything),
                },
                #[cfg(feature = "oobi")]
                ReplyRoute::EndRoleAdd(_)
                | ReplyRoute::EndRoleCut(_)
                | ReplyRoute::LocScheme(_) => {
                    // check signature
                    self.validator
                        .verify(&rpy.reply.serialize()?, &rpy.signature)?;
                    // check digest
                    rpy.reply.check_digest()?;
                    // self.publisher.notify(&Notification::GotOobi(rpy))
                    Ok(())
                }
            },
            #[cfg(feature = "query")]
            Message::Query(_) => {
                // TODO should do nothing?
                // It doesn't update database
                Ok(())
            }
        }
    }
}

/// Compute State for Prefix
///
/// Returns the current State associated with
/// the given Prefix
pub fn compute_state(
    db: Arc<SledEventDatabase>,
    id: &IdentifierPrefix,
) -> Result<Option<IdentifierState>, Error> {
    if let Some(events) = db.get_kel_finalized_events(id) {
        // start with empty state
        let mut state = IdentifierState::default();
        // we sort here to get inception first
        let mut sorted_events = events.collect::<Vec<TimestampedSignedEventMessage>>();
        // TODO why identifier is in database if there are no events for it?
        if sorted_events.is_empty() {
            return Ok(None);
        };
        sorted_events.sort();
        for event in sorted_events {
            state = match state.clone().apply(&event.signed_event_message) {
                Ok(s) => s,
                // will happen when a recovery has overridden some part of the KEL,
                Err(e) => match e {
                    // skip out of order and partially signed events
                    Error::EventOutOfOrderError | Error::NotEnoughSigsError => continue,
                    // stop processing here
                    _ => break,
                },
            };
        }
        Ok(Some(state))
    } else {
        // no inception event, no state
        Ok(None)
    }
}
