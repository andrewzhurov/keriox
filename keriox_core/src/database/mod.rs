pub mod escrow;
#[cfg(feature = "mailbox")]
pub mod mailbox;
pub(crate) mod tables;
pub(crate) mod timestamped;

use crate::actor::prelude::{HashFunctionCode, SerializationFormats};
use said::sad::SAD;
use std::{
    path::{Path, PathBuf},
    sync::Arc,
};

use serde::{Deserialize, Serialize};

#[cfg(feature = "mailbox")]
use self::mailbox::MailboxData;
use self::tables::{SledEventTree, SledEventTreeVec};

#[cfg(feature = "query")]
use crate::query::reply_event::SignedReply;
use crate::{
    event::KeyEvent,
    event_message::{
        msg::KeriEvent,
        signed_event_message::{
            SignedEventMessage, SignedNontransferableReceipt, SignedTransferableReceipt,
        },
        TimestampedEventMessage,
    },
    prefix::IdentifierPrefix,
};

use self::timestamped::TimestampedSignedEventMessage;

use said::SelfAddressingIdentifier;
pub type SAID = SelfAddressingIdentifier;

#[derive(Hash, Clone, Debug, Serialize, Deserialize, SAD)]
pub struct Membership {
    pub member_aids: Vec<SAID>,     // aligned with threshold weights
    pub member_aids_kes: Vec<SAID>, // aligned with member_aids

    #[said]
    pub digest: Option<SAID>,
}

#[derive(Hash, Clone, Debug, Serialize, Deserialize)]
pub enum Mailbox {
    DIDComm { did_peer: String },
}

#[derive(Hash, Clone, Debug, Serialize, Deserialize)]
pub enum Control {
    Device(DeviceControl),
    Group(GroupControl),
}

#[derive(Hash, Clone, Debug, Serialize, Deserialize, SAD)]
pub struct DeviceControl {
    pub mailbox: Mailbox,

    #[said]
    pub digest: Option<SAID>,
}

#[derive(Hash, Clone, Debug, Serialize, Deserialize, SAD)]
pub struct GroupControl {
    pub device_aids: Vec<SAID>,     // aligned with threshold
    pub device_aids_kes: Vec<SAID>, // aligned with device_aids, did_peer inside ke
    pub mailboxes: Vec<Mailbox>, // aligned with device_aids; Could be in device KEL, but let's keep it simple

    #[said]
    pub digest: Option<SAID>,
}

pub struct SledEventDatabase {
    db: Arc<sled::Db>,
    // "iids" tree
    // this thing is expensive, but everything else is cheeeeeep
    identifiers: SledEventTree<IdentifierPrefix>,
    // "kels" tree
    key_event_logs: SledEventTreeVec<TimestampedSignedEventMessage>,
    // "vrcs" tree
    receipts_t: SledEventTreeVec<SignedTransferableReceipt>,
    // "rcts" tree
    receipts_nt: SledEventTreeVec<SignedNontransferableReceipt>,
    #[cfg(feature = "query")]
    accepted_rpy: SledEventTreeVec<SignedReply>,

    // "ldes" tree
    likely_duplicious_events: SledEventTreeVec<TimestampedEventMessage>,
    // "dels" tree
    duplicitous_events: SledEventTreeVec<TimestampedSignedEventMessage>,

    #[cfg(feature = "query")]
    escrowed_replys: SledEventTreeVec<SignedReply>,

    #[cfg(feature = "mailbox")]
    mailbox: MailboxData,

    membership: SledEventTree<Membership>,
    control: SledEventTree<Control>,
}

// TODO: remove all the `.ok()`s
impl SledEventDatabase {
    pub fn new(path: impl AsRef<Path>) -> Result<Self, DbError> {
        let mut events_path = PathBuf::new();
        events_path.push(path);
        let mut escrow_path = events_path.clone();

        events_path.push("events");
        escrow_path.push("escrow");

        let db = Arc::new(sled::open(events_path.as_path())?);

        Ok(Self {
            identifiers: SledEventTree::new(db.open_tree(b"iids")?),
            receipts_t: SledEventTreeVec::new(db.open_tree(b"vrcs")?),
            receipts_nt: SledEventTreeVec::new(db.open_tree(b"rcts")?),
            key_event_logs: SledEventTreeVec::new(db.open_tree(b"kels")?),
            likely_duplicious_events: SledEventTreeVec::new(db.open_tree(b"ldes")?),
            duplicitous_events: SledEventTreeVec::new(db.open_tree(b"dels")?),
            #[cfg(feature = "query")]
            accepted_rpy: SledEventTreeVec::new(db.open_tree(b"knas")?),
            #[cfg(feature = "mailbox")]
            mailbox: MailboxData::new(db.clone())?,

            #[cfg(feature = "query")]
            escrowed_replys: SledEventTreeVec::new(db.open_tree(b"knes")?),

            membership: SledEventTree::new(db.open_tree(b"memb")?),
            control: SledEventTree::new(db.open_tree(b"ctrl")?),
            db,
        })
    }

    pub fn add_membership(
        &self,
        id: &IdentifierPrefix,
        membership: &Membership,
    ) -> Result<(), DbError> {
        self.membership
            .insert(self.identifiers.designated_key(id)?, membership)?;
        self.db.flush()?;
        Ok(())
    }

    pub fn get_membership(&self, id: &IdentifierPrefix) -> Option<Membership> {
        self.membership
            .get(self.identifiers.designated_key(id).ok()?)
            .ok()?
    }

    pub fn add_control(&self, id: &IdentifierPrefix, control: &Control) -> Result<(), DbError> {
        self.control
            .insert(self.identifiers.designated_key(id)?, control)?;
        self.db.flush()?;
        Ok(())
    }

    pub fn get_control(&self, id: &IdentifierPrefix) -> Option<Control> {
        self.control
            .get(self.identifiers.designated_key(id).ok()?)
            .ok()?
    }

    pub fn add_kel_finalized_event(
        &self,
        event: SignedEventMessage,
        id: &IdentifierPrefix,
    ) -> Result<(), DbError> {
        self.key_event_logs
            .push(self.identifiers.designated_key(id)?, event.into())?;
        self.db.flush()?;
        Ok(())
    }

    pub fn get_kel_finalized_events(
        &self,
        id: &IdentifierPrefix,
    ) -> Option<impl DoubleEndedIterator<Item = TimestampedSignedEventMessage>> {
        self.key_event_logs
            .iter_values(self.identifiers.designated_key(id).ok()?)
    }

    pub fn remove_kel_finalized_event(
        &self,
        id: &IdentifierPrefix,
        event: &SignedEventMessage,
    ) -> Result<(), DbError> {
        self.key_event_logs
            .remove(self.identifiers.designated_key(id)?, &event.into())?;
        self.db.flush()?;
        Ok(())
    }

    pub fn add_receipt_t(
        &self,
        receipt: SignedTransferableReceipt,
        id: &IdentifierPrefix,
    ) -> Result<(), DbError> {
        self.receipts_t
            .push(self.identifiers.designated_key(id)?, receipt)?;
        self.db.flush()?;
        Ok(())
    }

    pub fn get_receipts_t(
        &self,
        id: &IdentifierPrefix,
    ) -> Option<impl DoubleEndedIterator<Item = SignedTransferableReceipt>> {
        self.receipts_t
            .iter_values(self.identifiers.designated_key(id).ok()?)
    }

    pub fn add_receipt_nt(
        &self,
        receipt: SignedNontransferableReceipt,
        id: &IdentifierPrefix,
    ) -> Result<(), DbError> {
        let designated_key = self.identifiers.designated_key(id)?;
        match self.receipts_nt.get(designated_key)? {
            Some(receipts) => {
                // Update existing receipt with new signature or insert new if doesn't exist.
                let (found, mut to_insert) =
                    receipts
                        .into_iter()
                        .fold((false, vec![]), |(_, mut acc), mut rct| {
                            if rct.body.eq(&receipt.body) {
                                for sig in &receipt.signatures {
                                    if !rct.signatures.contains(&sig) {
                                        rct.signatures.push(sig.clone())
                                    }
                                }
                                acc.push(rct);
                                (true, acc)
                            } else {
                                acc.push(rct);
                                (false, acc)
                            }
                        });

                if !found {
                    to_insert.push(receipt)
                };
                self.receipts_nt.put(designated_key, to_insert)?;
                self.db.flush()?;
                Ok(())
            }
            None => {
                self.receipts_nt.push(designated_key, receipt)?;
                self.db.flush()?;
                Ok(())
            }
        }
    }

    pub fn get_receipts_nt(
        &self,
        id: &IdentifierPrefix,
    ) -> Option<impl DoubleEndedIterator<Item = SignedNontransferableReceipt>> {
        self.receipts_nt
            .iter_values(self.identifiers.designated_key(id).ok()?)
    }

    pub fn remove_receipts_nt(&self, id: &IdentifierPrefix) -> Result<(), DbError> {
        if let Some(receipts) = self.get_receipts_nt(id) {
            for receipt in receipts {
                self.receipts_nt
                    .remove(self.identifiers.designated_key(id)?, &receipt)?;
            }
        }
        self.db.flush()?;
        Ok(())
    }

    pub fn add_likely_duplicious_event(
        &self,
        event: KeriEvent<KeyEvent>,
        id: &IdentifierPrefix,
    ) -> Result<(), DbError> {
        self.likely_duplicious_events
            .push(self.identifiers.designated_key(id)?, event.into())?;
        self.db.flush()?;
        Ok(())
    }

    pub fn get_likely_duplicitous_events(
        &self,
        id: &IdentifierPrefix,
    ) -> Option<impl DoubleEndedIterator<Item = TimestampedEventMessage>> {
        self.likely_duplicious_events
            .iter_values(self.identifiers.designated_key(id).ok()?)
    }

    pub fn add_duplicious_event(
        &self,
        event: SignedEventMessage,
        id: &IdentifierPrefix,
    ) -> Result<(), DbError> {
        self.duplicitous_events
            .push(self.identifiers.designated_key(id)?, event.into())
    }

    pub fn get_duplicious_events(
        &self,
        id: &IdentifierPrefix,
    ) -> Option<impl DoubleEndedIterator<Item = TimestampedSignedEventMessage>> {
        self.duplicitous_events
            .iter_values(self.identifiers.designated_key(id).ok()?)
    }

    #[cfg(feature = "query")]
    pub fn update_accepted_reply(
        &self,
        rpy: SignedReply,
        id: &IdentifierPrefix,
    ) -> Result<(), DbError> {
        use crate::query::reply_event::ReplyRoute;

        match self
            .accepted_rpy
            .iter_values(self.identifiers.designated_key(id)?)
        {
            Some(rpys) => {
                let filtered = rpys
                    .filter(|s| match (s.reply.get_route(), rpy.reply.get_route()) {
                        (ReplyRoute::Ksn(id1, _), ReplyRoute::Ksn(id2, _)) => id1 != id2,
                        _ => true,
                    })
                    .chain(Some(rpy.clone()).into_iter())
                    .collect();
                self.accepted_rpy
                    .put(self.identifiers.designated_key(id)?, filtered)
            }
            None => self
                .accepted_rpy
                .push(self.identifiers.designated_key(id)?, rpy),
        }?;
        self.db.flush()?;
        Ok(())
    }

    #[cfg(feature = "query")]
    pub fn get_accepted_replys(
        &self,
        id: &IdentifierPrefix,
    ) -> Option<impl DoubleEndedIterator<Item = SignedReply>> {
        self.accepted_rpy
            .iter_values(self.identifiers.designated_key(id).ok()?)
    }

    #[cfg(feature = "query")]
    pub fn remove_accepted_reply(
        &self,
        id: &IdentifierPrefix,
        rpy: SignedReply,
    ) -> Result<(), DbError> {
        self.accepted_rpy
            .remove(self.identifiers.designated_key(id)?, &rpy)?;
        self.db.flush()?;
        Ok(())
    }

    #[cfg(feature = "query")]
    pub fn add_escrowed_reply(
        &self,
        rpy: SignedReply,
        id: &IdentifierPrefix,
    ) -> Result<(), DbError> {
        self.escrowed_replys
            .push(self.identifiers.designated_key(id)?, rpy)?;
        self.db.flush()?;
        Ok(())
    }

    #[cfg(feature = "query")]
    pub fn get_escrowed_replys(
        &self,
        id: &IdentifierPrefix,
    ) -> Option<impl DoubleEndedIterator<Item = SignedReply>> {
        self.escrowed_replys
            .iter_values(self.identifiers.designated_key(id).ok()?)
    }

    #[cfg(feature = "query")]
    pub fn remove_escrowed_reply(
        &self,
        id: &IdentifierPrefix,
        rpy: &SignedReply,
    ) -> Result<(), DbError> {
        self.escrowed_replys
            .remove(self.identifiers.designated_key(id)?, rpy)?;
        self.db.flush()?;
        Ok(())
    }

    #[cfg(feature = "query")]
    pub fn get_all_escrowed_replys(&self) -> Option<impl DoubleEndedIterator<Item = SignedReply>> {
        self.escrowed_replys.get_all()
    }

    #[cfg(feature = "mailbox")]
    pub fn add_mailbox_receipt(
        &self,
        receipt: SignedNontransferableReceipt,
        id: &IdentifierPrefix,
    ) -> Result<(), DbError> {
        self.mailbox
            .add_mailbox_receipt(self.identifiers.designated_key(id)?, receipt)?;
        self.db.flush()?;
        Ok(())
    }

    #[cfg(feature = "mailbox")]
    pub fn get_mailbox_receipts(
        &self,
        id: &IdentifierPrefix,
    ) -> Option<impl DoubleEndedIterator<Item = SignedNontransferableReceipt>> {
        self.mailbox
            .get_mailbox_receipts(self.identifiers.designated_key(id).ok()?)
    }

    #[cfg(feature = "mailbox")]
    pub fn add_mailbox_reply(
        &self,
        reply: SignedEventMessage,
        id: &IdentifierPrefix,
    ) -> Result<(), DbError> {
        self.mailbox
            .add_mailbox_reply(self.identifiers.designated_key(id)?, reply)?;
        self.db.flush()?;
        Ok(())
    }

    #[cfg(feature = "mailbox")]
    pub fn get_mailbox_replies(
        &self,
        id: &IdentifierPrefix,
    ) -> Option<impl DoubleEndedIterator<Item = SignedEventMessage>> {
        self.mailbox
            .get_mailbox_replies(self.identifiers.designated_key(id).ok()?)
    }

    #[cfg(feature = "mailbox")]
    pub fn add_mailbox_multisig(
        &self,
        event: SignedEventMessage,
        target_id: &IdentifierPrefix,
    ) -> Result<(), DbError> {
        self.mailbox
            .add_mailbox_multisig(self.identifiers.designated_key(target_id)?, event)?;
        self.db.flush()?;
        Ok(())
    }

    #[cfg(feature = "mailbox")]
    pub fn get_mailbox_multisig(
        &self,
        id: &IdentifierPrefix,
    ) -> Option<impl DoubleEndedIterator<Item = TimestampedSignedEventMessage>> {
        self.mailbox
            .get_mailbox_multisig(self.identifiers.designated_key(id).ok()?)
    }

    #[cfg(feature = "mailbox")]
    pub fn add_mailbox_delegate(
        &self,
        event: SignedEventMessage,
        target_id: &IdentifierPrefix,
    ) -> Result<(), DbError> {
        self.mailbox
            .add_mailbox_delegate(self.identifiers.designated_key(target_id)?, event)?;
        self.db.flush()?;
        Ok(())
    }

    #[cfg(feature = "mailbox")]
    pub fn get_mailbox_delegate(
        &self,
        id: &IdentifierPrefix,
    ) -> Option<impl DoubleEndedIterator<Item = TimestampedSignedEventMessage>> {
        self.mailbox
            .get_mailbox_delegate(self.identifiers.designated_key(id).ok()?)
    }
}

#[derive(Debug, thiserror::Error, Serialize, Deserialize)]
pub enum DbError {
    // TODO: more variants
    #[error("sled error")]
    Sled,
    #[error("serde error")]
    Serde,
}

impl From<sled::Error> for DbError {
    fn from(_: sled::Error) -> Self {
        DbError::Sled
    }
}

impl From<serde_cbor::Error> for DbError {
    fn from(_: serde_cbor::Error) -> Self {
        DbError::Serde
    }
}
