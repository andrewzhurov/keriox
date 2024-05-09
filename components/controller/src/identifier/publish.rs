use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
};

use keri_core::{mailbox::MailboxResponse, prefix::IdentifierPrefix};

use crate::{error::ControllerError, mailbox_updating::MailboxReminder};

use super::{mechanics::MechanicsError, Identifier};

impl Identifier {
    pub async fn notify_witnesses(&mut self) -> Result<usize, MechanicsError> {
        let mut n = 0;
        while let Some(ev) = self.to_notify.pop() {
            // Elect the leader
            // Leader is identifier with minimal index among all participants who
            // sign event. He will send message to witness.
            let id_idx = self.get_index(&ev.event_message.data).unwrap_or_default();
            let min_sig_idx =
                ev.signatures
                    .iter()
                    .map(|at| at.index.current())
                    .min()
                    .expect("event should have at least one signature") as usize;
            if min_sig_idx == id_idx {
                let witnesses = self
                    .known_events
                    .find_witnesses_at_event(&ev.event_message)?;
                self.communication.publish(&witnesses, &ev).await?;
                n += 1;
            }
        }
        Ok(n)
    }
}
