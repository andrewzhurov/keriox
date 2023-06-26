use std::sync::Arc;

use std::path::Path;

use keri::actor::prelude::{HashFunctionCode, SelfAddressingIdentifier};
use keri::event::sections::seal::{EventSeal, Seal};
use teliox::event::verifiable_event::VerifiableEvent;
use teliox::seal::{AttachedSourceSeal, EventSourceSeal};
use teliox::tel::Tel;

use crate::error::ControllerError;

use super::IdentifierController;

impl IdentifierController {
    /// Generate `vcp` event and `ixn` event with  seal to `vcp`. To finalize
    /// the process, `ixn` need to be signed confirmed with `finalize_event`
    /// function.
    pub fn incept_registry(
        &mut self,
        tel_db_path: impl AsRef<Path>,
    ) -> Result<Vec<u8>, ControllerError> {
        // Create tel
        let tel = self.source.tel.clone();

        let vcp = tel
            .make_inception_event(
                self.id.clone(),
                vec![teliox::event::manager_event::Config::NoBackers],
                0,
                vec![],
            )
            .unwrap();

        let id = vcp.get_prefix();
        let seal = Seal::Event(EventSeal {
            prefix: vcp.get_prefix(),
            sn: vcp.get_sn(),
            event_digest: vcp.get_digest().unwrap(),
        });
        let ixn = self.anchor_with_seal(&[seal])?;
        let source_seal = EventSourceSeal {
            sn: ixn.data.sn,
            digest: ixn.digest()?,
        };
        let encoded = ixn.encode()?;

        let verifiable_event = VerifiableEvent {
            event: vcp,
            seal: AttachedSourceSeal { seal: source_seal },
        };

        tel.processor.process(verifiable_event).unwrap();
        self.registry_id = Some(id);

        Ok(encoded)
    }

    /// Generate `iss` event and `ixn` event with  seal to `iss`. To finalize
    /// the process, `ixn` need to be signed confirmed with `finalize_event`
    /// function.
    pub fn issue(&self, credential: &str) -> Result<Vec<u8>, ControllerError> {
        match self.registry_id.as_ref() {
            Some(registry_id) => {
                let tel = self.source.tel.clone();
                let iss = tel
                    .make_issuance_event(registry_id, HashFunctionCode::Blake3_256, credential)
                    .unwrap();

                let seal = Seal::Event(EventSeal {
                    prefix: iss.get_prefix(),
                    sn: iss.get_sn(),
                    event_digest: iss.get_digest().unwrap(),
                });
                let ixn = self.anchor_with_seal(&[seal])?;

                let source_seal = EventSourceSeal {
                    sn: ixn.data.sn,
                    digest: ixn.digest()?,
                };
                let encoded_ixn = ixn.encode()?;

                let verifiable_event = VerifiableEvent {
                    event: iss,
                    seal: AttachedSourceSeal { seal: source_seal },
                };
                tel.processor.process(verifiable_event).unwrap();

                Ok(encoded_ixn)
            }
            None => Err(ControllerError::OtherError("Tel not incepted".into())),
        }
    }

    /// Generate `rev` event and `ixn` event with  seal to `rev`. To finalize
    /// the process, `ixn` need to be signed confirmed with `finalize_event`
    /// function.
    pub fn revoke(
        &self,
        credential_sai: &SelfAddressingIdentifier,
    ) -> Result<Vec<u8>, ControllerError> {
        match &self.registry_id {
            Some(registry_id) => {
                let tel = self.source.tel.clone();
                let rev = tel.make_revoke_event(registry_id, credential_sai).unwrap();

                let seal = Seal::Event(EventSeal {
                    prefix: rev.get_prefix(),
                    sn: rev.get_sn(),
                    event_digest: rev.get_digest().unwrap(),
                });
                let ixn = self.anchor_with_seal(&[seal])?;

                let source_seal = EventSourceSeal {
                    sn: ixn.data.sn,
                    digest: ixn.digest()?,
                };
                let encoded_ixn = ixn.encode()?;

                let verifiable_event = VerifiableEvent {
                    event: rev,
                    seal: AttachedSourceSeal { seal: source_seal },
                };
                tel.processor.process(verifiable_event).unwrap();

                Ok(encoded_ixn)
            }
            None => Err(ControllerError::OtherError("Tel not incepted".into())),
        }
    }
}
