use std::{sync::Arc, time::Duration};

use super::{
    event_storage::EventStorage,
    notification::{JustNotification, Notification, NotificationBus, Notifier},
    validator::EventValidator,
};
#[cfg(feature = "query")]
use crate::query::reply_event::ReplyRoute;
use crate::{
    database::{escrow::{Escrow, EscrowDb}, SledEventDatabase},
    error::Error,
    event_message::signed_event_message::SignedEventMessage,
    prefix::IdentifierPrefix,
};

pub fn default_escrow_bus(event_db: Arc<SledEventDatabase>, escrow_db: Arc<EscrowDb>) -> (NotificationBus, Arc<OutOfOrderEscrow>) {
    let mut bus = NotificationBus::new();

    // Register out of order escrow, to save and reprocess out of order events
    let ooo_escrow = Arc::new(OutOfOrderEscrow::new(
        event_db.clone(),
        escrow_db,
        Duration::from_secs(10),
    ));
    bus.register_observer(ooo_escrow.clone(), vec![
            JustNotification::OutOfOrder,
            JustNotification::KeyEventAdded,
        ]);

    bus.register_observer(
        Arc::new(PartiallySignedEscrow::new(event_db.clone())),
        vec![JustNotification::PartiallySigned],
    );

    bus.register_observer(
        Arc::new(PartiallyWitnessedEscrow::new(event_db.clone())),
        vec![
            JustNotification::PartiallyWitnessed,
            JustNotification::ReceiptEscrowed,
            JustNotification::ReceiptAccepted,
            JustNotification::ReceiptOutOfOrder,
        ],
    );

    bus.register_observer(
        Arc::new(NontransReceiptsEscrow::new(event_db.clone())),
        vec![
            JustNotification::KeyEventAdded,
            JustNotification::ReceiptOutOfOrder,
            JustNotification::PartiallyWitnessed,
        ],
    );

    bus.register_observer(
        Arc::new(TransReceiptsEscrow::new(event_db.clone())),
        vec![
            JustNotification::KeyEventAdded,
            JustNotification::TransReceiptOutOfOrder,
        ],
    );

    (bus, ooo_escrow)
}

pub struct OutOfOrderEscrow {
    db: Arc<SledEventDatabase>,
    pub escrowed_out_of_order: Escrow<SignedEventMessage>,
}

impl OutOfOrderEscrow {
    pub fn new(db: Arc<SledEventDatabase>, escrow_db: Arc<EscrowDb>, duration: Duration) -> Self {
        let escrow = Escrow::new(b"ooes", duration, escrow_db);
        Self {
            db,
            escrowed_out_of_order: escrow,
        }
    }
}
impl Notifier for OutOfOrderEscrow {
    fn notify(&self, notification: &Notification, bus: &NotificationBus) -> Result<(), Error> {
        match notification {
            Notification::KeyEventAdded(ev_message) => {
                let id = ev_message.event_message.event.get_prefix();
                self.process_out_of_order_events(bus, &id)?;
            }
            Notification::OutOfOrder(signed_event) => {
                // ignore events with no signatures
                if !signed_event.signatures.is_empty() {
                    let id = match signed_event.event_message.event.get_event_data() {
                        crate::event::event_data::EventData::Dip(dip) => dip.delegator,
                        crate::event::event_data::EventData::Drt(_) => {
                            let id = signed_event.event_message.event.get_prefix();
                            if let Some(state) =
                                EventStorage::new(self.db.clone()).get_state(&id)?
                            {
                                match state.delegator {
                                    Some(id) => id,
                                    None => id,
                                }
                            } else {
                                id
                            }
                        }
                        _ => signed_event.event_message.event.get_prefix(),
                    };
                    self.escrowed_out_of_order.add(&id, signed_event.clone())?;
                }
            }
            _ => return Err(Error::SemanticError("Wrong notification".into())),
        }

        Ok(())
    }
}

impl OutOfOrderEscrow {
    pub fn process_out_of_order_events(
        &self,
        bus: &NotificationBus,
        id: &IdentifierPrefix,
    ) -> Result<(), Error> {
        if let Some(esc) = self.escrowed_out_of_order.get(id) {
            for event in esc {
                let validator = EventValidator::new(self.db.clone());
                match validator.validate_event(&event) {
                    Ok(_) => {
                        // add to kel
                        self.db.add_kel_finalized_event(event.clone(), id)?;
                        // remove from escrow
                        self.escrowed_out_of_order.remove(id, &event)?;
                        bus.notify(&Notification::KeyEventAdded(event))?;
                        // stop processing the escrow if kel was updated. It needs to start again.
                        break;
                    }
                    Err(Error::SignatureVerificationError) => {
                        // remove from escrow
                        self.escrowed_out_of_order.remove(id, &event)?;
                    }
                    Err(_e) => (), // keep in escrow,
                }
            }
        };

        Ok(())
    }
}

#[derive(Clone)]
pub struct PartiallySignedEscrow {
    db: Arc<SledEventDatabase>,
}

impl PartiallySignedEscrow {
    pub fn new(db: Arc<SledEventDatabase>) -> Self {
        Self { db }
    }
}
impl Notifier for PartiallySignedEscrow {
    fn notify(&self, notification: &Notification, bus: &NotificationBus) -> Result<(), Error> {
        match notification {
            Notification::PartiallySigned(ev) => {
                if ev.signatures.is_empty() {
                    // ignore events with no signatures
                    Ok(())
                } else {
                    self.process_partially_signed_events(bus, ev)
                }
            }
            _ => Err(Error::SemanticError("Wrong notification".into())),
        }
    }
}

impl PartiallySignedEscrow {
    pub fn process_partially_signed_events(
        &self,
        bus: &NotificationBus,
        signed_event: &SignedEventMessage,
    ) -> Result<(), Error> {
        let id = signed_event.event_message.event.get_prefix();
        if let Some(esc) = self
            .db
            .get_partially_signed_events(signed_event.event_message.clone())
        {
            let new_sigs = esc
                .flat_map(|ev| ev.signatures)
                .chain(signed_event.signatures.clone().into_iter())
                .collect();

            let new_event = SignedEventMessage {
                signatures: new_sigs,
                ..signed_event.to_owned()
            };

            let validator = EventValidator::new(self.db.clone());
            match validator.validate_event(&new_event) {
                Ok(_) => {
                    // add to kel
                    self.db.add_kel_finalized_event(new_event.clone(), &id)?;
                    // remove from escrow
                    self.db
                        .remove_partially_signed_event(&id, &new_event.event_message)?;
                    bus.notify(&Notification::KeyEventAdded(new_event))?;
                }
                Err(_e) => {
                    //keep in escrow and save new partially signed event
                    self.db
                        .add_partially_signed_event(signed_event.clone(), &id)?;
                }
            }
        } else {
            self.db
                .add_partially_signed_event(signed_event.clone(), &id)?;
        };

        Ok(())
    }
}

#[derive(Clone)]
pub struct PartiallyWitnessedEscrow {
    db: Arc<SledEventDatabase>,
}

impl PartiallyWitnessedEscrow {
    pub fn new(db: Arc<SledEventDatabase>) -> Self {
        Self { db }
    }
}
impl Notifier for PartiallyWitnessedEscrow {
    fn notify(&self, notification: &Notification, bus: &NotificationBus) -> Result<(), Error> {
        match notification {
            Notification::ReceiptAccepted
            | Notification::ReceiptEscrowed
            | Notification::ReceiptOutOfOrder(_) => {
                self.process_partially_witnessed_events(bus)?;
            }
            Notification::PartiallyWitnessed(signed_event) => {
                // ignore events with no signatures
                if !signed_event.signatures.is_empty() {
                    let id = &signed_event.event_message.event.get_prefix();
                    self.db
                        .add_partially_witnessed_event(signed_event.clone(), id)?;
                }
            }
            _ => return Err(Error::SemanticError("Wrong notification".into()))
        }

        Ok(())
    }
}

impl PartiallyWitnessedEscrow {
    pub fn process_partially_witnessed_events(&self, bus: &NotificationBus) -> Result<(), Error> {
        if let Some(esc) = self.db.get_all_partially_witnessed() {
            for event in esc {
                let id = event.event_message.event.get_prefix();

                let validator = EventValidator::new(self.db.clone());
                match validator.validate_event(&event) {
                    Ok(_) => {
                        // add to kel
                        self.db.add_kel_finalized_event(event.clone(), &id)?;
                        // remove from escrow
                        self.db.remove_partially_witnessed_event(&id, &event)?;
                        bus.notify(&Notification::KeyEventAdded(event))?;
                        // stop processing the escrow if kel was updated. It needs to start again.
                        break;
                    }
                    Err(Error::SignatureVerificationError) => {
                        // remove from escrow
                        self.db.remove_partially_witnessed_event(&id, &event)?;
                    }
                    Err(_e) => (), // keep in escrow,
                }
            }
        };

        Ok(())
    }
}

#[derive(Clone)]
pub struct NontransReceiptsEscrow {
    db: Arc<SledEventDatabase>,
}
impl NontransReceiptsEscrow {
    pub fn new(db: Arc<SledEventDatabase>) -> Self {
        Self { db }
    }
}
impl Notifier for NontransReceiptsEscrow {
    fn notify(&self, notification: &Notification, bus: &NotificationBus) -> Result<(), Error> {
        match notification {
            Notification::KeyEventAdded(_) | Notification::PartiallyWitnessed(_) => {
                self.process_nt_receipts_escrow(bus)
            }
            Notification::ReceiptOutOfOrder(receipt) => {
                if receipt.couplets.is_none() && receipt.indexed_sigs.is_none() {
                    // ignore events with no signatures
                    Ok(())
                } else {
                    let id = &receipt.body.event.prefix;
                    self.db.add_escrow_nt_receipt(receipt.clone(), id)?;
                    bus.notify(&Notification::ReceiptEscrowed)
                }
            }
            _ => Err(Error::SemanticError("Wrong notification".into())),
        }
    }
}

impl NontransReceiptsEscrow {
    pub fn process_nt_receipts_escrow(&self, bus: &NotificationBus) -> Result<(), Error> {
        if let Some(esc) = self.db.get_all_escrow_nt_receipts() {
            for signed_receipt in esc {
                let id = signed_receipt.body.event.prefix.clone();
                let validator = EventValidator::new(self.db.clone());
                match validator.validate_witness_receipt(&signed_receipt) {
                    Ok(_) => {
                        // add to receipts
                        self.db.add_receipt_nt(signed_receipt.clone(), &id)?;
                        // remove from escrow
                        self.db.remove_escrow_nt_receipt(&id, &signed_receipt)?;
                        bus.notify(&Notification::ReceiptAccepted)?;
                    }
                    Err(Error::SignatureVerificationError) => {
                        // remove from escrow
                        self.db.remove_escrow_nt_receipt(&id, &signed_receipt)?;
                    }
                    Err(e) => return Err(e), // keep in escrow,
                }
            }
        };

        Ok(())
    }
}

#[derive(Clone)]
pub struct TransReceiptsEscrow {
    db: Arc<SledEventDatabase>,
}
impl TransReceiptsEscrow {
    pub fn new(db: Arc<SledEventDatabase>) -> Self {
        Self { db }
    }
}
impl Notifier for TransReceiptsEscrow {
    fn notify(&self, notification: &Notification, bus: &NotificationBus) -> Result<(), Error> {
        match notification {
            Notification::KeyEventAdded(event) => {
                self.process_t_receipts_escrow(&event.event_message.event.get_prefix(), bus)?;
            }
            Notification::TransReceiptOutOfOrder(receipt) => {
                // ignore events with no signatures
                if !receipt.signatures.is_empty() {
                    let id = receipt.validator_seal.prefix.clone();
                    self.db.add_escrow_t_receipt(receipt.to_owned(), &id)?;
                }
            }
            _ => return Err(Error::SemanticError("Wrong notification".into())),
        }
        Ok(())
    }
}
impl TransReceiptsEscrow {
    pub fn process_t_receipts_escrow(
        &self,
        id: &IdentifierPrefix,
        bus: &NotificationBus,
    ) -> Result<(), Error> {
        if let Some(esc) = self.db.get_escrow_t_receipts(id) {
            for timestamped_receipt in esc {
                let validator = EventValidator::new(self.db.clone());
                match validator.validate_validator_receipt(&timestamped_receipt) {
                    Ok(_) => {
                        // add to receipts
                        self.db.add_receipt_t(timestamped_receipt.clone(), &id)?;
                        // remove from escrow
                        self.db.remove_escrow_t_receipt(&id, &timestamped_receipt)?;
                        bus.notify(&Notification::ReceiptAccepted)?;
                    }
                    Err(Error::SignatureVerificationError) => {
                        // remove from escrow
                        self.db.remove_escrow_t_receipt(&id, &timestamped_receipt)?;
                    }
                    Err(e) => return Err(e), // keep in escrow,
                }
            }
        };

        Ok(())
    }
}

#[cfg(feature = "query")]
#[derive(Clone)]
pub struct ReplyEscrow(Arc<SledEventDatabase>);
#[cfg(feature = "query")]
impl ReplyEscrow {
    pub fn new(db: Arc<SledEventDatabase>) -> Self {
        Self(db)
    }
}
#[cfg(feature = "query")]
impl Notifier for ReplyEscrow {
    fn notify(&self, notification: &Notification, bus: &NotificationBus) -> Result<(), Error> {
        match notification {
            Notification::KsnOutOfOrder(rpy) => {
                if let ReplyRoute::Ksn(id, _ksn) = rpy.reply.get_route() {
                    // let id = ksn.state.prefix;
                    self.0.add_escrowed_reply(rpy.clone(), &id)?;
                };
                Ok(())
            }
            &Notification::KeyEventAdded(_) => self.process_reply_escrow(bus),
            _ => Ok(()),
        }
    }
}

#[cfg(feature = "query")]
impl ReplyEscrow {
    pub fn process_reply_escrow(&self, _bus: &NotificationBus) -> Result<(), Error> {
        use crate::query::QueryError;

        if let Some(esc) = self.0.get_all_escrowed_replys() {
            for sig_rep in esc {
                let validator = EventValidator::new(self.0.clone());
                let id = if let ReplyRoute::Ksn(_id, ksn) = sig_rep.reply.get_route() {
                    Ok(ksn.state.prefix)
                } else {
                    Err(Error::SemanticError("Wrong event type".into()))
                }?;
                match validator.process_signed_ksn_reply(&sig_rep) {
                    Ok(_) => {
                        self.0.remove_escrowed_reply(&id, &sig_rep)?;
                        self.0.update_accepted_reply(sig_rep, &id)?;
                    }
                    Err(Error::SignatureVerificationError)
                    | Err(Error::QueryError(QueryError::StaleRpy)) => {
                        // remove from escrow
                        self.0.remove_escrowed_reply(&id, &sig_rep)?;
                    }
                    Err(Error::EventOutOfOrderError) => (), // keep in escrow,
                    Err(e) => return Err(e),
                };
            }
        };
        Ok(())
    }
}
