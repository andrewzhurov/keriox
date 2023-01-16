use std::{collections::HashMap, error::Error, sync::Arc};

use serde::Deserialize;

use super::{Transport, TransportError};
use crate::{
    actor::{error::ActorError, simple_controller::PossibleResponse},
    event_message::signed_event_message::{Message, Op},
    oobi::{LocationScheme, Oobi, Role},
    prefix::IdentifierPrefix,
    query::query_event::SignedQuery,
};

#[async_trait::async_trait]
pub trait TestActor<E: Error = ActorError> {
    async fn send_message(&self, msg: Message) -> Result<(), E>;
    async fn send_query(&self, query: SignedQuery) -> Result<PossibleResponse, E>;
    async fn request_loc_scheme(&self, eid: IdentifierPrefix) -> Result<Vec<Op>, E>;
    async fn request_end_role(
        &self,
        cid: IdentifierPrefix,
        role: Role,
        eid: IdentifierPrefix,
    ) -> Result<Vec<Message>, E>;
    async fn resolve_oobi(&self, msg: Oobi) -> Result<(), E>;
}

pub type TestActorMap<E = ActorError> =
    HashMap<(url::Host, u16), Arc<dyn TestActor<E> + Send + Sync>>;

/// Used in tests to connect directly to actors without going through the network.
pub struct TestTransport<E> {
    actors: Arc<TestActorMap<E>>,
    _phantom: std::marker::PhantomData<E>,
}

impl<E> TestTransport<E> {
    pub fn new(actors: TestActorMap<E>) -> Self {
        Self {
            actors: Arc::new(actors),
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<E> Clone for TestTransport<E> {
    fn clone(&self) -> Self {
        Self {
            actors: Arc::clone(&self.actors),
            _phantom: std::marker::PhantomData,
        }
    }
}

#[async_trait::async_trait]
impl<E> Transport<E> for TestTransport<E>
where
    E: for<'a> Deserialize<'a> + Send + Sync + std::error::Error + 'static,
{
    async fn send_message(
        &self,
        loc: LocationScheme,
        msg: Message,
    ) -> Result<(), TransportError<E>> {
        let (host, port) = match loc.url.origin() {
            url::Origin::Tuple(_scheme, host, port) => (host, port),
            _ => return Err(TransportError::NetworkError),
        };

        self.actors
            .get(&(host, port))
            .ok_or(TransportError::NetworkError)?
            .send_message(msg)
            .await
            .map_err(|err| TransportError::RemoteError(err))?;

        Ok(())
    }

    async fn send_query(
        &self,
        loc: LocationScheme,
        qry: SignedQuery,
    ) -> Result<PossibleResponse, TransportError<E>> {
        let (host, port) = match loc.url.origin() {
            url::Origin::Tuple(_scheme, host, port) => (host, port),
            _ => return Err(TransportError::NetworkError),
        };

        let resp = self
            .actors
            .get(&(host, port))
            .ok_or(TransportError::NetworkError)?
            .send_query(qry)
            .await
            .map_err(|err| TransportError::RemoteError(err))?;

        Ok(resp)
    }

    async fn request_loc_scheme(&self, loc: LocationScheme) -> Result<Vec<Op>, TransportError<E>> {
        let (host, port) = match loc.url.origin() {
            url::Origin::Tuple(_scheme, host, port) => (host, port),
            _ => return Err(TransportError::NetworkError),
        };

        let ops = self
            .actors
            .get(&(host, port))
            .ok_or(TransportError::NetworkError)?
            .request_loc_scheme(loc.eid)
            .await
            .map_err(|_| TransportError::NetworkError)?;

        Ok(ops)
    }

    async fn request_end_role(
        &self,
        loc: LocationScheme,
        cid: IdentifierPrefix,
        role: Role,
        eid: IdentifierPrefix,
    ) -> Result<Vec<Message>, TransportError<E>> {
        let (host, port) = match loc.url.origin() {
            url::Origin::Tuple(_scheme, host, port) => (host, port),
            _ => return Err(TransportError::NetworkError),
        };

        let ops = self
            .actors
            .get(&(host, port))
            .ok_or(TransportError::NetworkError)?
            .request_end_role(cid, role, eid)
            .await
            .map_err(|_| TransportError::NetworkError)?;

        Ok(ops)
    }

    async fn resolve_oobi(&self, loc: LocationScheme, oobi: Oobi) -> Result<(), TransportError<E>> {
        let (host, port) = match loc.url.origin() {
            url::Origin::Tuple(_scheme, host, port) => (host, port),
            _ => return Err(TransportError::NetworkError),
        };

        self.actors
            .get(&(host, port))
            .ok_or(TransportError::NetworkError)?
            .resolve_oobi(oobi)
            .await
            .map_err(|err| TransportError::RemoteError(err))?;
        Ok(())
    }
}