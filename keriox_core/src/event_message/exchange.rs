use serde::{Deserialize, Serialize};

use crate::{
    derivation::self_addressing::SelfAddressing, error::Error, event::SerializationFormats,
    event_parsing::path::MaterialPath, prefix::IdentifierPrefix,
};

use super::{
    key_event_message::KeyEvent, signature::Signature, EventMessage, EventTypeTag, SaidEvent,
    Typeable,
};

pub type ExchangeMessage = EventMessage<SaidEvent<Exchange>>;

#[derive(Debug, Clone, PartialEq)]
pub struct SignedExchange {
    pub exchange_message: ExchangeMessage,
    pub signature: Vec<Signature>,
    // signature of event anchored in exn message in `a` field
    pub data_signature: (MaterialPath, Vec<Signature>),
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(tag = "r")]
pub enum Exchange {
    #[serde(rename = "/fwd")]
    Fwd {
        #[serde(rename = "q")]
        args: FwdArgs,
        #[serde(rename = "a")]
        to_forward: EventMessage<KeyEvent>,
    },
}

impl Exchange {
    pub fn to_message(
        self,
        format: SerializationFormats,
        derivation: &SelfAddressing,
    ) -> Result<EventMessage<SaidEvent<Exchange>>, Error> {
        SaidEvent::<Exchange>::to_message(self, format, derivation)
    }
}

impl Exchange {
    pub fn get_prefix(&self) -> IdentifierPrefix {
        match self {
            Exchange::Fwd {
                args,
                to_forward: _,
            } => args.recipient_id.clone(),
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct FwdArgs {
    #[serde(rename = "pre")]
    pub recipient_id: IdentifierPrefix,
    pub topic: ForwardTopic,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(rename_all = "lowercase")]
pub enum ForwardTopic {
    Multisig,
    Delegate,
}

impl Typeable for Exchange {
    fn get_type(&self) -> EventTypeTag {
        EventTypeTag::Exn
    }
}

#[test]
fn test_exn_serialization() -> Result<(), Error> {
    let exn_event = r#"{"v":"KERI10JSON0002c9_","t":"exn","d":"Eru6l4p3-r6KJkT1Ac8r5XWuQMsD91-c80hC7lASOoZI","r":"/fwd","q":{"pre":"E-4-PsMBN0YEKyTl3zL0zulWcBehdaaG6Go5cMc0BzQ8","topic":"multisig"},"a":{"v":"KERI10JSON000215_","t":"icp","d":"EOWwyMU3XA7RtWdelFt-6waurOTH_aW_Z9VTaU-CshGk","i":"EOWwyMU3XA7RtWdelFt-6waurOTH_aW_Z9VTaU-CshGk","s":"0","kt":"2","k":["DQKeRX-2dXdSWS-EiwYyiQdeIwesvubEqnUYC5vsEyjo","D-U6Sc6VqQC3rDuD2wLF3oR8C4xQyWOTMp4zbJyEnRlE"],"nt":"2","n":["ENVtv0_G68psQhfWB-ZyVH1lndLli2LSmfSxxszNufoI","E6UpCouA9mZA03hMFJLrhA0SvwR4HVNqf2wrZM-ydTSI"],"bt":"3","b":["BGKVzj4ve0VSd8z_AmvhLg4lqcC_9WYX90k03q-R_Ydo","BuyRFMideczFZoapylLIyCjSdhtqVb31wZkRKvPfNqkw","Bgoq68HCmYNUDgOz4Skvlu306o_NY-NrYuKAVhk3Zh9c"],"c":[],"a":[]}}"#;

    let parsed: ExchangeMessage = serde_json::from_str(exn_event).unwrap();
    let ser_deser = serde_json::to_string(&parsed).unwrap();

    assert_eq!(exn_event, ser_deser);

    let exchange = r#"{"v":"KERI10JSON0002fd_","t":"exn","d":"E4eI8YwFFm3FgrFetNtk9n5uunKaacICRdmuIe9cX_VE","r":"/fwd","q":{"pre":"E8AKUcbZyik8EdkOwXgnyAxO5mSIPJWGZ_o7zMhnNnjo","topic":"delegate"},"a":{"v":"KERI10JSON000249_","t":"dip","d":"EcUCJqbf43LPLDlo2ulpsPQCxhuYwaiiHoTuU9cET89s","i":"EcUCJqbf43LPLDlo2ulpsPQCxhuYwaiiHoTuU9cET89s","s":"0","kt":"2","k":["DQKeRX-2dXdSWS-EiwYyiQdeIwesvubEqnUYC5vsEyjo","D-U6Sc6VqQC3rDuD2wLF3oR8C4xQyWOTMp4zbJyEnRlE"],"nt":"2","n":["ENVtv0_G68psQhfWB-ZyVH1lndLli2LSmfSxxszNufoI","E6UpCouA9mZA03hMFJLrhA0SvwR4HVNqf2wrZM-ydTSI"],"bt":"3","b":["BGKVzj4ve0VSd8z_AmvhLg4lqcC_9WYX90k03q-R_Ydo","BuyRFMideczFZoapylLIyCjSdhtqVb31wZkRKvPfNqkw","Bgoq68HCmYNUDgOz4Skvlu306o_NY-NrYuKAVhk3Zh9c"],"c":[],"a":[],"di":"E8AKUcbZyik8EdkOwXgnyAxO5mSIPJWGZ_o7zMhnNnjo"}}"#;//-HABEozYHef4je02EkMOA1IKM65WkIdSjfrL7XWDk_JzJL9o-AABAAUK--bPQ_gS3mk0HrcDFjbw5dp-JpDC1Ls9vj-bNjcI6slpJp3dZVkK2H4_P41jHPpq4S3MSIo-eQ5cADywo-Aw-LAv5AABAA-a-AACAASjyXhePLrCsV2u1kIXlcUivNgiqj8ynoCSThEdPPlSPQkclk6ZWZLJuS9vYZ9fXl5nNouHw1CIOYJGld2kcTDgABfSFUCJiJ5A388FDWMRO-tw4e8jEL3ughYb-Lm2ZNZ3AggCcHsPEBNVRIFOMiXW8focmzrSNJzfPIsodUarvPCw"#;

    let parsed: ExchangeMessage = serde_json::from_str(exchange).unwrap();
    let ser_deser = serde_json::to_string(&parsed).unwrap();

    assert_eq!(exchange, ser_deser);
    Ok(())
}
