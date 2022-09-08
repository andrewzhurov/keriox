use crate::{
    derivation::self_addressing::SelfAddressing,
    event::{
        sections::{
            seal::{DigestSeal, Seal},
            threshold::{SignatureThreshold, WeightedThreshold},
        },
        EventMessage, SerializationFormats,
    },
    event_message::{
        event_msg_builder::EventMsgBuilder,
        exchange::{Exchange, ExchangeMessage, ForwardTopic, FwdArgs},
        key_event_message::KeyEvent,
        EventTypeTag,
    },
    oobi::{EndRole, Role},
    prefix::{BasicPrefix, IdentifierPrefix, SelfAddressingPrefix},
    query::reply_event::{ReplyEvent, ReplyRoute},
    state::IdentifierState,
};

use crate::controller::error::ControllerError;

// todo add setting signing threshold
pub fn incept(
    public_keys: Vec<BasicPrefix>,
    next_pub_keys: Vec<BasicPrefix>,
    witnesses: Vec<BasicPrefix>,
    witness_threshold: u64,
    delegator_id: Option<&IdentifierPrefix>,
) -> Result<String, ControllerError> {
    let event_builder = match delegator_id {
        Some(delegator) => EventMsgBuilder::new(EventTypeTag::Dip).with_delegator(delegator),
        None => EventMsgBuilder::new(EventTypeTag::Icp),
    };
    let serialized_icp = event_builder
        .with_keys(public_keys)
        .with_next_keys(next_pub_keys)
        .with_witness_list(witnesses.as_slice())
        .with_witness_threshold(&SignatureThreshold::Simple(witness_threshold))
        .build()
        .map_err(|e| ControllerError::EventGenerationError(e.to_string()))?
        .serialize()
        .map_err(|e| ControllerError::EventGenerationError(e.to_string()))?;

    let icp = String::from_utf8(serialized_icp)
        .map_err(|e| ControllerError::EventGenerationError(e.to_string()))?;
    Ok(icp)
}

pub fn incept_with_next_hashes(
    public_keys: Vec<BasicPrefix>,
    signature_threshold: &SignatureThreshold,
    next_pub_keys: Vec<SelfAddressingPrefix>,
    witnesses: Vec<BasicPrefix>,
    witness_threshold: u64,
    delegator_id: Option<&IdentifierPrefix>,
) -> Result<EventMessage<KeyEvent>, ControllerError> {
    // Check if threshold is possible to achive
    match signature_threshold {
        SignatureThreshold::Simple(t) => {
            if t > &(public_keys.len() as u64) {
                return Err(ControllerError::EventGenerationError(
                    "Improper threshold".into(),
                ));
            }
        }
        SignatureThreshold::Weighted(w) => {
            let length = match w {
                WeightedThreshold::Single(s) => s.length(),
                WeightedThreshold::Multi(m) => m.length(),
            };
            if length > public_keys.len() {
                return Err(ControllerError::EventGenerationError(
                    "Improper threshold".into(),
                ));
            }
        }
    };
    let event_builder = match delegator_id {
        Some(delegator) => EventMsgBuilder::new(EventTypeTag::Dip).with_delegator(delegator),
        None => EventMsgBuilder::new(EventTypeTag::Icp),
    };
   event_builder
        .with_keys(public_keys)
        .with_threshold(signature_threshold)
        .with_next_keys_hashes(next_pub_keys)
        .with_witness_list(witnesses.as_slice())
        .with_witness_threshold(&SignatureThreshold::Simple(witness_threshold))
        .build()
        .map_err(|e| ControllerError::EventGenerationError(e.to_string()))
}

pub fn rotate(
    state: IdentifierState,
    current_keys: Vec<BasicPrefix>,
    new_next_keys: Vec<BasicPrefix>,
    witness_to_add: Vec<BasicPrefix>,
    witness_to_remove: Vec<BasicPrefix>,
    witness_threshold: u64,
) -> Result<String, ControllerError> {
    let rot = make_rotation(
        state,
        current_keys,
        new_next_keys,
        witness_to_add,
        witness_to_remove,
        witness_threshold,
    )?
    .serialize()
    .map_err(|e| ControllerError::EventGenerationError(e.to_string()))?;
    String::from_utf8(rot).map_err(|e| ControllerError::EventGenerationError(e.to_string()))
}

fn make_rotation(
    state: IdentifierState,
    current_keys: Vec<BasicPrefix>,
    new_next_keys: Vec<BasicPrefix>,
    witness_to_add: Vec<BasicPrefix>,
    witness_to_remove: Vec<BasicPrefix>,
    witness_threshold: u64,
) -> Result<EventMessage<KeyEvent>, ControllerError> {
    EventMsgBuilder::new(EventTypeTag::Rot)
        .with_prefix(&state.prefix)
        .with_sn(state.sn + 1)
        .with_previous_event(&state.last_event_digest)
        .with_keys(current_keys)
        .with_next_keys(new_next_keys)
        .with_witness_to_add(&witness_to_add)
        .with_witness_to_remove(&witness_to_remove)
        .with_witness_threshold(&SignatureThreshold::Simple(witness_threshold))
        .build()
        .map_err(|e| ControllerError::EventGenerationError(e.to_string()))
}

pub fn anchor(
    state: IdentifierState,
    payload: &[SelfAddressingPrefix],
) -> Result<String, ControllerError> {
    let seal_list = payload
        .iter()
        .map(|seal| {
            Seal::Digest(DigestSeal {
                dig: seal.to_owned(),
            })
        })
        .collect();
    let ixn = EventMsgBuilder::new(EventTypeTag::Ixn)
        .with_prefix(&state.prefix)
        .with_sn(state.sn + 1)
        .with_previous_event(&state.last_event_digest)
        .with_seal(seal_list)
        .build()
        .map_err(|e| ControllerError::EventGenerationError(e.to_string()))?
        .serialize()
        .map_err(|e| ControllerError::EventGenerationError(e.to_string()))?;
    String::from_utf8(ixn).map_err(|e| ControllerError::EventGenerationError(e.to_string()))
}

pub fn anchor_with_seal(
    state: IdentifierState,
    seal_list: &[Seal],
) -> Result<EventMessage<KeyEvent>, ControllerError> {
    let ev = EventMsgBuilder::new(EventTypeTag::Ixn)
        .with_prefix(&state.prefix)
        .with_sn(state.sn + 1)
        .with_previous_event(&state.last_event_digest)
        .with_seal(seal_list.to_owned())
        .build()
        .map_err(|e| ControllerError::EventGenerationError(e.to_string()))?;
    Ok(ev)
}

/// Generate reply event used to add role to given identifier.
pub fn generate_end_role(
    controller_id: &IdentifierPrefix,
    watcher_id: &IdentifierPrefix,
    role: Role,
    enabled: bool,
) -> Result<ReplyEvent, ControllerError> {
    let end_role = EndRole {
        cid: controller_id.clone(),
        role,
        eid: watcher_id.clone(),
    };
    let reply_route = if enabled {
        ReplyRoute::EndRoleAdd(end_role)
    } else {
        ReplyRoute::EndRoleCut(end_role)
    };
    ReplyEvent::new_reply(
        reply_route,
        // TODO set algo and serialization
        SelfAddressing::Blake3_256,
        SerializationFormats::JSON,
    )
    .map_err(|e| ControllerError::EventGenerationError(e.to_string()))
}

pub fn exchange(
    receipient: &IdentifierPrefix,
    data: &EventMessage<KeyEvent>,
    topic: ForwardTopic,
) -> Result<ExchangeMessage, ControllerError> {
    Exchange::Fwd {
        args: FwdArgs {
            recipient_id: receipient.clone(),
            topic,
        },
        to_forward: data.clone(),
    }
    .to_message(SerializationFormats::JSON, &SelfAddressing::Blake3_256)
    .map_err(|e| ControllerError::EventGenerationError(e.to_string()))
}
