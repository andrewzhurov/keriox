use core::str::FromStr;

use serde::{Deserialize, Deserializer, Serialize, Serializer};

use super::{verify, Prefix, SelfSigningPrefix};
use crate::{
    derivation::{basic::Basic, DerivationCode},
    event_parsing::parsing::from_text_to_bytes,
    keys::PublicKey,
};
use super::error::Error;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct BasicPrefix {
    pub derivation: Basic,
    pub public_key: PublicKey,
}

impl BasicPrefix {
    pub fn new(code: Basic, public_key: PublicKey) -> Self {
        Self {
            derivation: code,
            public_key,
        }
    }

    pub fn verify(&self, data: &[u8], signature: &SelfSigningPrefix) -> Result<bool, Error> {
        verify(data, self, signature)
    }

    pub fn is_transferable(&self) -> bool {
        self.derivation.is_transferable()
    }
}

impl FromStr for BasicPrefix {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let code = Basic::from_str(s)?;

        if s.len() == code.prefix_b64_len() {
            let k_vec =
                from_text_to_bytes(s[code.code_len()..].as_bytes())?[code.code_len()..].to_vec();
            Ok(Self::new(code, PublicKey::new(k_vec)))
        } else {
            Err(Error::IncorrectLengthError(s.into()))
        }
    }
}

impl Prefix for BasicPrefix {
    fn derivative(&self) -> Vec<u8> {
        self.public_key.key()
    }
    fn derivation_code(&self) -> String {
        self.derivation.to_str()
    }
}

/// Serde compatible Serialize
impl Serialize for BasicPrefix {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&self.to_str())
    }
}

/// Serde compatible Deserialize
impl<'de> Deserialize<'de> for BasicPrefix {
    fn deserialize<D>(deserializer: D) -> Result<BasicPrefix, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;

        BasicPrefix::from_str(&s).map_err(serde::de::Error::custom)
    }
}

#[test]
fn serialize_deserialize() {
    use ed25519_dalek::Keypair;
    use rand::rngs::OsRng;

    let kp = Keypair::generate(&mut OsRng);

    let bp = BasicPrefix {
        derivation: Basic::Ed25519,
        public_key: PublicKey::new(kp.public.to_bytes().to_vec()),
    };

    let serialized = serde_json::to_string(&bp);
    assert!(serialized.is_ok());

    let deserialized = serde_json::from_str(&serialized.unwrap());

    assert!(deserialized.is_ok());
    assert_eq!(bp, deserialized.unwrap());
}

#[test]
fn to_from_string() {
    use ed25519_dalek::Keypair;
    use rand::rngs::OsRng;

    use crate::keys::PrivateKey;

    let kp = Keypair::generate(&mut OsRng);

    let signer = PrivateKey::new(kp.secret.to_bytes().to_vec());

    let message = b"hello there";
    let sig = SelfSigningPrefix::new(
        crate::derivation::self_signing::SelfSigning::Ed25519Sha512,
        signer.sign_ed(message).unwrap(),
    );

    let bp = BasicPrefix {
        derivation: Basic::Ed25519,
        public_key: PublicKey::new(kp.public.to_bytes().to_vec()),
    };

    assert!(bp.verify(message, &sig).unwrap());

    let string = bp.to_str();

    let from_str = BasicPrefix::from_str(&string);

    assert!(from_str.is_ok());
    let deser = from_str.unwrap();
    assert_eq!(bp, deser);

    assert!(deser.verify(message, &sig).unwrap());
}
