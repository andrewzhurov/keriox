use std::{collections::HashSet, sync::Arc};

use keri::{
    actor::{event_generator, prelude::Message, simple_controller::PossibleResponse},
    event::{
        event_data::EventData,
        sections::{
            seal::{EventSeal, Seal},
            threshold::SignatureThreshold,
        },
        EventMessage, SerializationFormats,
    },
    event_message::{
        cesr_adapter::EventType,
        exchange::{Exchange, ExchangeMessage, ForwardTopic, FwdArgs, SignedExchange},
        key_event_message::KeyEvent,
        signature::{Nontransferable, Signature, SignerData},
        signed_event_message::{Notice, Op},
        Digestible,
    },
    event_parsing::{parsers::parse_payload, path::MaterialPath, primitives::CesrPrimitive},
    oobi::{LocationScheme, Role, Scheme},
    prefix::{AttachedSignaturePrefix, BasicPrefix, IdentifierPrefix, SelfSigningPrefix},
    query::{
        query_event::{QueryArgsMbx, QueryEvent, QueryRoute, QueryTopics, SignedQuery},
        reply_event::ReplyRoute,
    },
    sai::{derivation::SelfAddressing, SelfAddressingPrefix},
};

use crate::{error::ControllerError, mailbox_updating::MailboxReminder, Controller};

use super::mailbox_updating::ActionRequired;

pub struct IdentifierController {
    pub id: IdentifierPrefix,
    pub source: Arc<Controller>,
    last_asked_index: MailboxReminder,
    last_asked_groups_index: MailboxReminder,
    broadcasted_rcts: HashSet<(SelfAddressingPrefix, BasicPrefix)>,
}

impl IdentifierController {
    pub fn new(id: IdentifierPrefix, kel: Arc<Controller>) -> Self {
        Self {
            id,
            source: kel,
            last_asked_index: MailboxReminder::default(),
            last_asked_groups_index: MailboxReminder::default(),
            broadcasted_rcts: HashSet::new(),
        }
    }

    pub fn get_kel(&self) -> Result<String, ControllerError> {
        String::from_utf8(
            self.source
                .storage
                .get_kel(&self.id)?
                .ok_or(ControllerError::UnknownIdentifierError)?,
        )
        .map_err(|_e| ControllerError::EventFormatError)
    }

    pub fn get_last_establishment_event_seal(&self) -> Result<EventSeal, ControllerError> {
        self.source
            .storage
            .get_last_establishment_event_seal(&self.id)?
            .ok_or(ControllerError::UnknownIdentifierError)
    }

    pub async fn rotate(
        &self,
        current_keys: Vec<BasicPrefix>,
        new_next_keys: Vec<BasicPrefix>,
        witness_to_add: Vec<LocationScheme>,
        witness_to_remove: Vec<BasicPrefix>,
        witness_threshold: u64,
    ) -> Result<String, ControllerError> {
        self.source
            .rotate(
                self.id.clone(),
                current_keys,
                new_next_keys,
                witness_to_add,
                witness_to_remove,
                witness_threshold,
            )
            .await
    }

    pub fn anchor(&self, payload: &[SelfAddressingPrefix]) -> Result<String, ControllerError> {
        self.source.anchor(self.id.clone(), payload)
    }

    /// Generates delegating event (ixn) and exchange event that contains
    /// delegated event which will be send to delegate after ixn finalization.
    pub fn delegate(
        &self,
        delegated_event: &EventMessage<KeyEvent>,
    ) -> Result<(EventMessage<KeyEvent>, ExchangeMessage), ControllerError> {
        let delegate = delegated_event.event.get_prefix();
        let delegated_seal = {
            let event_digest = delegated_event.get_digest();
            let sn = delegated_event.event.get_sn();
            Seal::Event(EventSeal {
                prefix: delegate.clone(),
                sn,
                event_digest,
            })
        };
        let delegating_event = self.source.anchor_with_seal(&self.id, &[delegated_seal])?;
        let exn_message = Exchange::Fwd {
            args: FwdArgs {
                recipient_id: delegate.clone(),
                topic: ForwardTopic::Delegate,
            },
            to_forward: delegating_event.clone(),
        }
        .to_message(SerializationFormats::JSON, SelfAddressing::Blake3_256)?;
        Ok((delegating_event, exn_message))
    }

    pub fn anchor_with_seal(
        &self,
        seal_list: &[Seal],
    ) -> Result<EventMessage<KeyEvent>, ControllerError> {
        self.source.anchor_with_seal(&self.id, seal_list)
    }

    pub fn anchor_group(
        &self,
        group_id: &IdentifierPrefix,
        seal_list: &[Seal],
    ) -> Result<EventMessage<KeyEvent>, ControllerError> {
        self.source.anchor_with_seal(group_id, seal_list)
    }

    /// Generates reply event with `end_role_add` route.
    pub fn add_watcher(&self, watcher_id: IdentifierPrefix) -> Result<String, ControllerError> {
        String::from_utf8(
            event_generator::generate_end_role(&self.id, &watcher_id, Role::Watcher, true)?
                .serialize()?,
        )
        .map_err(|_e| ControllerError::EventFormatError)
    }

    /// Generates reply event with `end_role_cut` route.
    pub fn remove_watcher(&self, watcher_id: IdentifierPrefix) -> Result<String, ControllerError> {
        String::from_utf8(
            event_generator::generate_end_role(&self.id, &watcher_id, Role::Watcher, false)?
                .serialize()?,
        )
        .map_err(|_e| ControllerError::EventFormatError)
    }

    /// Checks signatures and updates database.
    /// Must call [`IdentifierController::notify_witnesses`] after calling this function if event is a key event.
    pub async fn finalize_event(
        &self,
        event: &[u8],
        sig: SelfSigningPrefix,
    ) -> Result<(), ControllerError> {
        let parsed_event = parse_payload::<EventType>(event)
            .map_err(|_e| ControllerError::EventParseError)?
            .1;
        match parsed_event {
            EventType::KeyEvent(ke) => {
                let index = self.get_index(&ke.event)?;
                self.source.finalize_key_event(&ke, &sig, index)
            }
            EventType::Rpy(rpy) => match rpy.get_route() {
                ReplyRoute::EndRoleAdd(_) => Ok(self
                    .source
                    .finalize_add_role(&self.id, rpy, vec![sig])
                    .await?),
                ReplyRoute::EndRoleCut(_) => todo!(),
                _ => Err(ControllerError::WrongEventTypeError),
            },
            EventType::Qry(_) => todo!(),
            EventType::Receipt(_) => todo!(),
            EventType::Exn(_) => todo!(),
        }
    }

    /// Init group identifier
    ///
    /// Returns serialized group icp and list of exchange messages to sign.
    /// Exchanges are ment to be send to witness and forwarded to group
    /// participants.
    /// If `delegator` parameter is provided, it will generate delegated
    /// inception and append delegation request to exchange messages.
    pub fn incept_group(
        &self,
        participants: Vec<IdentifierPrefix>,
        signature_threshold: u64,
        initial_witness: Option<Vec<BasicPrefix>>,
        witness_threshold: Option<u64>,
        delegator: Option<IdentifierPrefix>,
    ) -> Result<(String, Vec<String>), ControllerError> {
        let key_config = self
            .source
            .storage
            .get_state(&self.id)?
            .ok_or(ControllerError::UnknownIdentifierError)?
            .current;

        let mut pks = key_config.public_keys;
        let mut npks = key_config.next_keys_data.next_key_hashes;
        for participant in &participants {
            let state = self
                .source
                .storage
                .get_state(&participant)?
                .ok_or(ControllerError::UnknownIdentifierError)?;
            pks.append(&mut state.clone().current.public_keys);
            npks.append(&mut state.clone().current.next_keys_data.next_key_hashes);
        }

        let icp = event_generator::incept_with_next_hashes(
            pks,
            &SignatureThreshold::Simple(signature_threshold),
            npks,
            initial_witness.unwrap_or_default(),
            witness_threshold.unwrap_or(0),
            delegator.as_ref(),
        )?;

        let serialized_icp = String::from_utf8(icp.serialize()?)
            .map_err(|e| ControllerError::EventGenerationError(e.to_string()))?;

        let mut exchanges = participants
            .iter()
            .map(|id| -> Result<_, _> {
                let exn =
                    event_generator::exchange(id, &icp, ForwardTopic::Multisig)?.serialize()?;
                String::from_utf8(exn).map_err(|_e| ControllerError::EventFormatError)
            })
            .collect::<Result<Vec<String>, ControllerError>>()?;

        if let Some(delegator) = delegator {
            let delegation_request = String::from_utf8(
                event_generator::exchange(&delegator, &icp, ForwardTopic::Delegate)?.serialize()?,
            )
            .map_err(|_e| ControllerError::EventFormatError)?;
            exchanges.push(delegation_request);
        }

        Ok((serialized_icp, exchanges))
    }

    pub async fn finalize_exchange(
        &self,
        exchange: &[u8],
        exn_signature: SelfSigningPrefix,
        data_signature: SelfSigningPrefix,
    ) -> Result<(), ControllerError> {
        // Join exn messages with their signatures and send it to witness.
        let material_path = MaterialPath::to_path("-a".into());
        // let attached_sig = sigs;
        let (_, parsed_exn) =
            parse_payload::<EventType>(exchange).map_err(|_e| ControllerError::EventFormatError)?;
        if let EventType::Exn(exn) = parsed_exn {
            let Exchange::Fwd {
                args: _,
                to_forward,
            } = exn.event.content.data.clone();

            let sigs: Vec<_> = if let Some(receipts) = self.source.storage.get_nt_receipts(
                &to_forward.event.get_prefix(),
                to_forward.event.get_sn(),
                &to_forward.event.get_digest(),
            )? {
                receipts
                    .signatures
                    .iter()
                    .map(|c| Signature::NonTransferable(c.clone()))
                    .chain([Signature::Transferable(
                        SignerData::JustSignatures,
                        vec![AttachedSignaturePrefix {
                            // TODO
                            index: 0,
                            signature: data_signature,
                        }]
                        .into(),
                    )])
                    .collect::<Vec<_>>()
            } else {
                vec![Signature::Transferable(
                    SignerData::JustSignatures,
                    vec![AttachedSignaturePrefix {
                        // TODO
                        index: 0,
                        signature: data_signature,
                    }],
                )]
            };

            let signature = vec![Signature::Transferable(
                SignerData::LastEstablishment(self.id.clone()),
                vec![AttachedSignaturePrefix {
                    // TODO
                    index: 0,
                    signature: exn_signature,
                }],
            )];
            let signer_exn = Message::Op(Op::Exchange(SignedExchange {
                exchange_message: exn,
                signature,
                data_signature: (material_path.clone(), sigs.clone()),
            }));
            let wits = self.source.get_witnesses_at_event(&to_forward)?;
            // TODO for now get first witness
            if let Some(wit) = wits.get(0) {
                self.source
                    .send_message_to(
                        &IdentifierPrefix::Basic(wit.clone()),
                        keri::oobi::Scheme::Http,
                        signer_exn,
                    )
                    .await?;
            }
            Ok(())
        } else {
            Ok(())
        }
    }

    /// Finalizes group identifier.
    /// Joins event with signature and verifies them.
    /// Must call [`IdentifierController::notify_witnesses`] after calling this function
    /// to send signed exn messages to witness to be forwarded to group participants.
    pub async fn finalize_group_incept(
        &mut self,
        group_event: &[u8],
        sig: SelfSigningPrefix,
        exchanges: Vec<(Vec<u8>, SelfSigningPrefix)>,
    ) -> Result<IdentifierPrefix, ControllerError> {
        // Join icp event with signature
        let (_, key_event) = parse_payload::<EventType>(&group_event)
            .map_err(|_e| ControllerError::EventFormatError)?;
        let icp = if let EventType::KeyEvent(icp) = key_event {
            icp
        } else {
            return Err(ControllerError::WrongEventTypeError);
        };
        let own_index = self.get_index(&icp.event)?;
        let group_prefix = icp.event.get_prefix();

        self.source.finalize_key_event(&icp, &sig, own_index)?;

        let signature = AttachedSignaturePrefix {
            index: own_index as u16,
            signature: sig,
        };

        let sigs: Vec<_> = if let Some(receipts) = self.source.storage.get_nt_receipts(
            &icp.event.get_prefix(),
            icp.event.get_sn(),
            &icp.event.get_digest(),
        )? {
            let couplets = receipts.signatures;
            couplets
                .into_iter()
                .map(|c| Signature::NonTransferable(c))
                .chain([Signature::Transferable(
                    SignerData::JustSignatures,
                    vec![signature],
                )])
                .collect::<Vec<_>>()
        } else {
            vec![Signature::Transferable(
                SignerData::JustSignatures,
                vec![signature],
            )]
        };

        // Join exn messages with their signatures and send it to witness.
        let material_path = MaterialPath::to_path("-a".into());
        let attached_sig = sigs;
        for (exn, signature) in exchanges {
            let (_, parsed_exn) =
                parse_payload::<EventType>(&exn).map_err(|_e| ControllerError::EventFormatError)?;
            let exn = if let EventType::Exn(exn) = parsed_exn {
                exn
            } else {
                return Err(ControllerError::WrongEventTypeError);
            };
            let signature = vec![Signature::Transferable(
                SignerData::LastEstablishment(self.id.clone()),
                vec![AttachedSignaturePrefix {
                    // TODO
                    index: 0,
                    signature,
                }],
            )];
            let signer_exn = Message::Op(Op::Exchange(SignedExchange {
                exchange_message: exn,
                signature,
                data_signature: (material_path.clone(), attached_sig.clone()),
            }));
            let wits = self.source.get_witnesses_at_event(&icp)?;
            // TODO for now get first witness
            if let Some(wit) = wits.get(0) {
                self.source
                    .send_message_to(
                        &IdentifierPrefix::Basic(wit.clone()),
                        keri::oobi::Scheme::Http,
                        signer_exn,
                    )
                    .await?;
            }
        }
        Ok(group_prefix)
    }

    pub async fn notify_witnesses(&self) -> Result<usize, ControllerError> {
        let mut n = 0;
        let evs = self
            .source
            .partially_witnessed_escrow
            .get_partially_witnessed_events();

        for ev in evs {
            // Elect the leader
            // Leader is identifier with minimal index among all participants who
            // sign event. He will send message to witness.
            let id_idx = self.get_index(&ev.event_message.event).unwrap_or_default();
            let min_sig_idx =
                ev.signatures
                    .iter()
                    .map(|at| at.index)
                    .min()
                    .expect("event should have at least one signature") as usize;
            if min_sig_idx == id_idx {
                let witnesses = self.source.get_witnesses_at_event(&ev.event_message)?;
                self.source.publish(&witnesses, &ev).await?;
                n += 1;
            }
        }
        Ok(n)
    }

    /// Helper function for getting the position of identifier's public key in
    /// group's current keys list.
    pub(crate) fn get_index(&self, group_event: &KeyEvent) -> Result<usize, ControllerError> {
        match &group_event.content.event_data {
            EventData::Icp(icp) => {
                // TODO what if group participant is a group and has more than one
                // public key?
                let own_pk = &self
                    .source
                    .storage
                    .get_state(&self.id)?
                    .ok_or(ControllerError::UnknownIdentifierError)?
                    .current
                    .public_keys[0];
                icp.key_config
                    .public_keys
                    .iter()
                    .position(|pk| pk == own_pk)
            }
            EventData::Rot(rot) => {
                let own_npk = &self
                    .source
                    .storage
                    .get_state(&self.id)?
                    .ok_or(ControllerError::UnknownIdentifierError)?
                    .current
                    .next_keys_data
                    .next_key_hashes[0];
                rot.key_config
                    .public_keys
                    .iter()
                    .position(|pk| own_npk.verify_binding(pk.to_str().as_bytes()))
            }
            EventData::Dip(dip) => {
                // TODO what if group participant is a group and has more than one
                // public key?
                let own_pk = &self
                    .source
                    .storage
                    .get_state(&self.id)?
                    .ok_or(ControllerError::UnknownIdentifierError)?
                    .current
                    .public_keys[0];
                dip.inception_data
                    .key_config
                    .public_keys
                    .iter()
                    .position(|pk| pk == own_pk)
            }
            EventData::Drt(drt) => {
                let own_npk = &self
                    .source
                    .storage
                    .get_state(&self.id)?
                    .ok_or(ControllerError::UnknownIdentifierError)?
                    .current
                    .next_keys_data
                    .next_key_hashes[0];
                drt.key_config
                    .public_keys
                    .iter()
                    .position(|pk| own_npk.verify_binding(pk.to_str().as_bytes()))
            }
            EventData::Ixn(_ixn) => {
                let own_pk = &self
                    .source
                    .storage
                    .get_state(&self.id)?
                    .ok_or(ControllerError::UnknownIdentifierError)?
                    .current
                    .public_keys[0];
                self.source
                    .storage
                    .get_state(&group_event.get_prefix())?
                    .ok_or(ControllerError::UnknownIdentifierError)?
                    .current
                    .public_keys
                    .iter()
                    .position(|pk| pk == own_pk)
            }
        }
        .ok_or(ControllerError::NotGroupParticipantError)
    }

    /// Generates query message of route `mbx` to query own identifier mailbox.
    pub fn query_mailbox(
        &self,
        identifier: &IdentifierPrefix,
        witnesses: &[BasicPrefix],
    ) -> Result<Vec<QueryEvent>, ControllerError> {
        Ok(witnesses
            .into_iter()
            .map(|wit| {
                QueryEvent::new_query(
                    QueryRoute::Mbx {
                        args: QueryArgsMbx {
                            // about who
                            i: identifier.clone(),
                            // who is asking
                            pre: self.id.clone(),
                            // who will get the query
                            src: IdentifierPrefix::Basic(wit.clone()),
                            topics: QueryTopics {
                                credential: 0,
                                receipt: 0,
                                replay: 0,
                                multisig: 0,
                                delegate: 0,
                                reply: 0,
                            },
                        },
                        reply_route: "".to_string(),
                    },
                    SerializationFormats::JSON,
                    SelfAddressing::Blake3_256,
                )
            })
            .collect::<Result<_, _>>()?)
    }

    /// Joins query events with their signatures, sends it to witness and
    /// process its response. If user action is needed to finalize process,
    /// returns proper notification.
    pub async fn finalize_mailbox_query(
        &mut self,
        queries: Vec<(QueryEvent, SelfSigningPrefix)>,
    ) -> Result<Vec<ActionRequired>, ControllerError> {
        let self_id = self.id.clone();
        let mut actions = Vec::new();
        for (qry, sig) in queries {
            let signatures = vec![AttachedSignaturePrefix {
                index: 0,
                signature: sig,
            }];
            let (receipient, about_who, from_who) = match &qry.event.content.data.route {
                QueryRoute::Log {
                    reply_route: _,
                    args,
                } => (
                    args.src.clone().ok_or(ControllerError::QueryArgumentError(
                        "Missing query receipient identifier".into(),
                    ))?,
                    None,
                    None,
                ),
                QueryRoute::Ksn {
                    reply_route: _,
                    args,
                } => (
                    args.src.clone().ok_or(ControllerError::QueryArgumentError(
                        "Missing query receipient identifier".into(),
                    ))?,
                    None,
                    None,
                ),
                QueryRoute::Mbx {
                    reply_route: _,
                    args,
                } => (args.src.clone(), Some(&args.i), Some(&args.pre)),
            };
            let query = SignedQuery::new(qry.clone(), self_id.clone(), signatures);
            let res = self
                .source
                .send_query_to(&receipient, Scheme::Http, query)
                .await?;
            println!("\nresponse: {:?}", res);
            // TODO what if other reponse than mailbox?
            let res = if let PossibleResponse::Mbx(res) = res {
                res
            } else {
                todo!()
            };
            let req = if from_who == about_who {
                // process own mailbox
                let req = self.process_own_mailbox(&res, &self.last_asked_index)?;
                // TODO: update last seen index
                // self.last_asked_index = MailboxReminder {
                //     receipt: res.receipt.len(),
                //     multisig: res.multisig.len(),
                //     delegate: res.delegate.len(),
                // };
                req
            } else {
                // process group mailbox
                let group_req = self
                    .process_group_mailbox(
                        &res,
                        about_who.ok_or(ControllerError::QueryArgumentError(
                            "Missing query receipient identifier".into(),
                        ))?,
                        &self.last_asked_groups_index,
                    )
                    .await?;
                self.last_asked_groups_index = MailboxReminder {
                    receipt: res.receipt.len(),
                    multisig: res.multisig.len(),
                    delegate: res.delegate.len(),
                };
                group_req
            };
            actions.extend(req)
        }
        Ok(actions)
    }

    /// Retrieve receipts for given `id` from the database and sends them to witnesses with IDs `wits`.
    pub async fn broadcast_receipts(
        &mut self,
        wits: &[IdentifierPrefix],
    ) -> Result<usize, ControllerError> {
        let receipts = self
            .source
            .storage
            .db
            .get_receipts_nt(&self.id)
            .into_iter()
            .flatten()
            .collect::<Vec<_>>();
        let n = receipts.len();

        // TODO: don't send the same receipt twice.
        for rct in receipts {
            for wit in wits {
                // TODO: don't send receipt to witness who created it.
                self.source
                    .send_message_to(
                        wit,
                        Scheme::Http,
                        Message::Notice(Notice::NontransferableRct(rct.clone())),
                    )
                    .await?;
            }

            // Remember event digest and witness ID to avoid sending the same receipt twice.
            let digest = rct.body.event.receipted_event_digest;
            for sig in rct.signatures {
                match sig {
                    Nontransferable::Indexed(sigs) => {
                        for sig in sigs {
                            let wits = self.source.storage.get_witnesses_at_event(
                                rct.body.event.sn,
                                &self.id,
                                &digest,
                            )?;
                            self.broadcasted_rcts
                                .insert((digest.clone(), wits[sig.index as usize].clone()));
                        }
                    }
                    Nontransferable::Couplet(sigs) => {
                        for (wit_id, _sig) in sigs {
                            self.broadcasted_rcts.insert((digest.clone(), wit_id));
                        }
                    }
                }
            }
        }

        Ok(n)
    }
}
