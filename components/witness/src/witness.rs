use std::{
    path::{Path, PathBuf},
    sync::Arc,
};

use keri::{
    actor::{
        parse_notice_stream, parse_op_stream, prelude::*, process_reply, process_signed_exn,
        process_signed_query, simple_controller::PossibleResponse,
    },
    derivation::{basic::Basic, self_addressing::SelfAddressing, self_signing::SelfSigning},
    error::Error,
    event::EventMessage,
    event_message::{
        event_msg_builder::ReceiptBuilder,
        key_event_message::KeyEvent,
        signed_event_message::{Notice, Op, SignedNontransferableReceipt},
    },
    oobi::{LocationScheme, OobiManager},
    prefix::{BasicPrefix, IdentifierPrefix},
    processor::notification::{Notification, NotificationBus, Notifier},
    query::{
        query_event::{MailboxResponse, QueryArgsMbx, QueryTopics},
        reply_event::{ReplyEvent, ReplyRoute, SignedReply},
        ReplyType,
    },
    signer::Signer,
};

use crate::witness_processor::WitnessProcessor;

pub struct WitnessReceiptGenerator {
    pub prefix: BasicPrefix,
    pub signer: Arc<Signer>,
    pub storage: EventStorage,
}

impl Notifier for WitnessReceiptGenerator {
    fn notify(&self, notification: &Notification, bus: &NotificationBus) -> Result<(), Error> {
        match notification {
            Notification::KeyEventAdded(event) => {
                let non_trans_receipt =
                    self.respond_to_key_event(&event.event_message, self.signer.clone())?;
                let prefix = &non_trans_receipt.body.event.prefix.clone();
                self.storage
                    .db
                    .add_receipt_nt(non_trans_receipt.clone(), prefix)?;
                bus.notify(&Notification::ReceiptAccepted)?;
                self.storage.add_mailbox_receipt(non_trans_receipt)?;
                Ok(())
            }
            Notification::PartiallyWitnessed(prt) => {
                self.storage
                    .db
                    .add_kel_finalized_event(prt.clone(), &prt.event_message.event.get_prefix())?;
                bus.notify(&Notification::KeyEventAdded(prt.clone()))?;
                let non_trans_receipt =
                    self.respond_to_key_event(&prt.event_message, self.signer.clone())?;
                let prefix = &non_trans_receipt.body.event.prefix.clone();
                self.storage
                    .db
                    .add_receipt_nt(non_trans_receipt.clone(), prefix)?;
                bus.notify(&Notification::ReceiptAccepted)?;
                self.storage.add_mailbox_receipt(non_trans_receipt)
            }
            _ => Ok(()),
        }
    }
}

impl WitnessReceiptGenerator {
    pub fn new(signer: Arc<Signer>, db: Arc<SledEventDatabase>) -> Self {
        let storage = EventStorage::new(db);
        let prefix = Basic::Ed25519.derive(signer.public_key());
        Self {
            prefix,
            signer,
            storage,
        }
    }

    fn respond_to_key_event(
        &self,
        event_message: &EventMessage<KeyEvent>,
        signer: Arc<Signer>,
    ) -> Result<SignedNontransferableReceipt, Error> {
        // Create witness receipt and add it to db
        let ser = event_message.serialize()?;
        let signature = signer.sign(&ser)?;
        let rcp = ReceiptBuilder::default()
            .with_receipted_event(event_message.clone())
            .build()?;

        let signature = SelfSigning::Ed25519Sha512.derive(signature);

        let signed_rcp = SignedNontransferableReceipt::new(
            &rcp,
            Some(vec![(self.prefix.clone(), signature)]),
            None,
        );

        Ok(signed_rcp)
    }
}

pub struct Witness {
    pub prefix: BasicPrefix,
    pub processor: WitnessProcessor,
    pub event_storage: EventStorage,
    pub oobi_manager: OobiManager,
    pub signer: Arc<Signer>,
    pub receipt_generator: Arc<WitnessReceiptGenerator>,
}

impl Witness {
    pub fn new(signer: Arc<Signer>, event_path: &Path, oobi_path: &Path) -> Result<Self, Error> {
        use keri::{database::escrow::EscrowDb, processor::notification::JustNotification};
        let mut events_path = PathBuf::new();
        events_path.push(&event_path);
        let mut escrow_path = events_path.clone();

        events_path.push("events");
        escrow_path.push("escrow");

        let prefix = Basic::Ed25519.derive(signer.public_key());
        let db = Arc::new(SledEventDatabase::new(events_path.as_path())?);
        let escrow_db = Arc::new(EscrowDb::new(escrow_path.as_path())?);
        let mut witness_processor = WitnessProcessor::new(db.clone(), escrow_db);
        let event_storage = EventStorage::new(db.clone());

        let receipt_generator = Arc::new(WitnessReceiptGenerator::new(signer.clone(), db.clone()));
        witness_processor.register_observer(
            receipt_generator.clone(),
            &[
                JustNotification::KeyEventAdded,
                JustNotification::PartiallyWitnessed,
            ],
        )?;
        Ok(Self {
            prefix,
            processor: witness_processor,
            signer: signer,
            event_storage,
            receipt_generator,
            oobi_manager: OobiManager::new(oobi_path),
        })
    }
    pub fn setup(
        public_address: url::Url,
        event_db_path: &Path,
        oobi_db_path: &Path,
        priv_key: Option<String>,
    ) -> Result<Self, Error> {
        let signer = Arc::new(
            priv_key
                .map(|key| Signer::new_with_seed(&key.parse()?))
                .unwrap_or(Ok(Signer::new()))?,
        );
        let prefix = Basic::Ed25519.derive(signer.public_key());
        let witness = Witness::new(signer.clone(), event_db_path, oobi_db_path)?;
        // construct witness loc scheme oobi
        let loc_scheme = LocationScheme::new(
            IdentifierPrefix::Basic(prefix.clone()),
            public_address.scheme().parse().unwrap(),
            public_address.clone(),
        );
        let reply = ReplyEvent::new_reply(
            ReplyRoute::LocScheme(loc_scheme),
            SelfAddressing::Blake3_256,
            SerializationFormats::JSON,
        )?;
        let signed_reply = SignedReply::new_nontrans(
            reply.clone(),
            prefix.clone(),
            SelfSigning::Ed25519Sha512.derive(signer.sign(reply.serialize()?)?),
        );
        witness.oobi_manager.save_oobi(&signed_reply)?;
        Ok(witness)
    }

    pub fn get_loc_scheme_for_id(
        &self,
        eid: &IdentifierPrefix,
    ) -> Result<Option<Vec<SignedReply>>, Error> {
        Ok(match self.oobi_manager.get_loc_scheme(eid)? {
            Some(oobis_to_sign) => Some(
                oobis_to_sign
                    .iter()
                    .map(|oobi_to_sing| {
                        let signature =
                            self.signer.sign(oobi_to_sing.serialize().unwrap()).unwrap();
                        SignedReply::new_nontrans(
                            oobi_to_sing.clone(),
                            self.prefix.clone(),
                            SelfSigning::Ed25519Sha512.derive(signature),
                        )
                    })
                    .collect(),
            ),
            None => None,
        })
    }

    pub fn get_signed_ksn_for_prefix(
        &self,
        prefix: &IdentifierPrefix,
        signer: Arc<Signer>,
    ) -> Result<SignedReply, Error> {
        let ksn = self
            .event_storage
            .get_ksn_for_prefix(prefix, SerializationFormats::JSON)?;
        let rpy = ReplyEvent::new_reply(
            ReplyRoute::Ksn(IdentifierPrefix::Basic(self.prefix.clone()), ksn),
            SelfAddressing::Blake3_256,
            SerializationFormats::JSON,
        )?;

        let signature = SelfSigning::Ed25519Sha512.derive(signer.sign(&rpy.serialize()?)?);
        Ok(SignedReply::new_nontrans(
            rpy,
            self.prefix.clone(),
            signature,
        ))
    }

    pub fn process_notice(&self, notice: Notice) -> Result<(), Error> {
        self.processor.process_notice(&notice)
    }

    pub fn process_op(&self, op: Op) -> Result<Option<PossibleResponse>, WitnessError> {
        match op {
            Op::Query(qry) => {
                let response = process_signed_query(qry, &self.event_storage)?;
                Ok(match response {
                    ReplyType::Ksn(ksn) => {
                        let rpy = ReplyEvent::new_reply(
                            ReplyRoute::Ksn(IdentifierPrefix::Basic(self.prefix.clone()), ksn),
                            SelfAddressing::Blake3_256,
                            SerializationFormats::JSON,
                        )?;

                        let signature =
                            SelfSigning::Ed25519Sha512.derive(self.signer.sign(&rpy.serialize()?)?);
                        let reply = SignedReply::new_nontrans(rpy, self.prefix.clone(), signature);
                        Some(PossibleResponse::Ksn(reply))
                    }
                    ReplyType::Kel(msgs) => Some(PossibleResponse::Kel(msgs)),
                    ReplyType::Mbx(mailbox_response) => {
                        Some(PossibleResponse::Mbx(mailbox_response))
                    }
                })
            }
            Op::Reply(rpy) => {
                process_reply(
                    rpy,
                    &self.oobi_manager,
                    &self.processor,
                    &self.event_storage,
                )?;
                Ok(None)
            }
            Op::Exchange(exn) => {
                process_signed_exn(exn, &self.event_storage)?;
                Ok(None)
            }
        }
    }

    pub fn parse_and_process_notices(&self, input_stream: &[u8]) -> Result<(), Error> {
        parse_notice_stream(input_stream)?
            .into_iter()
            .map(|notice| self.process_notice(notice))
            .collect()
    }

    pub fn parse_and_process_ops(
        &self,
        input_stream: &[u8],
    ) -> Result<Vec<PossibleResponse>, WitnessError> {
        parse_op_stream(input_stream)?
            .into_iter()
            .map(|op| self.process_op(op))
            .filter_map(|op| match op {
                Ok(Some(res)) => Some(Ok(res)),
                Ok(None) => None,
                Err(e) => Some(Err(e)),
            })
            .collect()
    }

    pub fn get_mailbox_messages(&self, id: &IdentifierPrefix) -> Result<MailboxResponse, Error> {
        self.event_storage.get_mailbox_messages(QueryArgsMbx {
            i: IdentifierPrefix::Basic(self.prefix.clone()),
            pre: id.clone(),
            src: IdentifierPrefix::Basic(self.prefix.clone()),
            topics: QueryTopics {
                credential: 0,
                receipt: 0,
                replay: 0,
                multisig: 0,
                delegate: 0,
                reply: 0,
            },
        })
    }
}

#[derive(Debug, derive_more::Display, derive_more::Error, derive_more::From)]
pub enum WitnessError {
    KeriError(keri::error::Error),
    DbError(keri::database::DbError),
    QueryFailed(keri::actor::SignedQueryError),
}