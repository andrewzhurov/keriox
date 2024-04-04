use std::sync::Arc;
#[cfg(test)]
mod test_watcher;

use keri_core::{
    event::{event_data::EventData, sections::seal::EventSeal}, event_message::signed_event_message::SignedEventMessage, prefix::{BasicPrefix, IdentifierPrefix}, state::IdentifierState
};

use crate::{communication::Communication, error::ControllerError, known_events::KnownEvents};

use self::publishing::QueryCache;
pub mod kel_managing;
pub mod publishing;
pub mod query;
mod mailbox;

pub struct Identifier {
    id: IdentifierPrefix,
    known_events: Arc<KnownEvents>,
    communication: Arc<Communication>,
    pub to_notify: Vec<SignedEventMessage>,
    query_cache: QueryCache,
    cached_state: IdentifierState,
}

impl Identifier {
    pub fn new(
        id: IdentifierPrefix,
        known_events: Arc<KnownEvents>,
        communication: Arc<Communication>,
    ) -> Self {
        // Load events that need to be notified to witnesses
        let events_to_notice: Vec<_> = known_events
            .partially_witnessed_escrow
            .get_partially_witnessed_events()
            .iter()
            .filter(|ev| ev.event_message.data.prefix == id)
            .cloned()
            .collect();
        // Cache state. It can be not fully witnessed.
        let state = if let Ok(state) = known_events.get_state(&id) {
            state
        } else {
            let not_accepted_incept = events_to_notice.iter().find_map(|ev| {
                if let EventData::Icp(_icp) = &ev.event_message.data.event_data {
                    Some(ev.event_message.clone())
                } else {
                    None
                }
            });
            IdentifierState::default()
                .apply(&not_accepted_incept.unwrap())
                .unwrap()
        };
        Self {
            id,
            known_events,
            communication,
            to_notify: events_to_notice,
            query_cache: QueryCache::new(),
            cached_state: state
        }
    }

    pub fn id(&self) -> &IdentifierPrefix {
        &self.id
    }

    pub fn find_state(&self, id: &IdentifierPrefix) -> Result<IdentifierState, ControllerError> {
        self.known_events.get_state(id)
    }

    pub fn current_public_keys(&self) -> Result<Vec<BasicPrefix>, ControllerError> {
        self.known_events.current_public_keys(&self.id)
    }


    pub fn get_last_establishment_event_seal(&self) -> Result<EventSeal, ControllerError> {
        self.known_events
            .storage
            .get_last_establishment_event_seal(&self.id)?
            .ok_or(ControllerError::UnknownIdentifierError)
    }
}
