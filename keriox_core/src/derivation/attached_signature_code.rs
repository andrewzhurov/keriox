use super::{self_signing::SelfSigning, DerivationCode};
use crate::{
    error::Error,
    event_parsing::parsing::{from_bytes_to_text, from_text_to_bytes},
};
use core::str::FromStr;

/// Attached Signature Derivation Codes
///
/// A self signing prefix derivation outputs a signature as its derivative (2.3.5)
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct AttachedSignatureCode {
    pub index: u16,
    pub code: SelfSigning,
}

impl AttachedSignatureCode {
    pub fn new(code: SelfSigning, index: u16) -> Self {
        Self { index, code }
    }
}

impl DerivationCode for AttachedSignatureCode {
    // TODO, this will only work with indicies up to 63
    fn to_str(&self) -> String {
        [
            match self.code {
                SelfSigning::Ed25519Sha512 => "A",
                SelfSigning::ECDSAsecp256k1Sha256 => "B",
                SelfSigning::Ed448 => "0AA",
            },
            &num_to_b64(self.index),
        ]
        .join("")
    }

    fn code_len(&self) -> usize {
        match self.code {
            SelfSigning::Ed25519Sha512 | SelfSigning::ECDSAsecp256k1Sha256 => 2,
            SelfSigning::Ed448 => 4,
        }
    }

    fn derivative_b64_len(&self) -> usize {
        match self.code {
            SelfSigning::Ed25519Sha512 | SelfSigning::ECDSAsecp256k1Sha256 => 86,
            SelfSigning::Ed448 => 152,
        }
    }
}

impl FromStr for AttachedSignatureCode {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match &s[..1] {
            "A" => Ok(Self::new(
                SelfSigning::Ed25519Sha512,
                b64_to_num(&s.as_bytes()[1..2])?,
            )),
            "B" => Ok(Self::new(
                SelfSigning::ECDSAsecp256k1Sha256,
                b64_to_num(&s.as_bytes()[1..2])?,
            )),
            "0" => match &s[1..3] {
                "AA" => Ok(Self::new(
                    SelfSigning::Ed448,
                    b64_to_num(&s.as_bytes()[3..4])?,
                )),
                _ => Err(Error::DeserializeError("Unknows signature code".into())),
            },
            _ => Err(Error::DeserializeError("Unknown attachment code".into())),
        }
    }
}

/// Parses the number from radix 64 using digits from url-safe base64 (`A` = 0, `_` = 63)
pub fn b64_to_num(b64: &[u8]) -> Result<u16, Error> {
    let slice = from_text_to_bytes(b64)?;
    let len = slice.len();

    Ok(u16::from_be_bytes(match len {
        0 => [0u8; 2],
        1 => [0, slice[0]],
        _ => [slice[len - 2], slice[len - 1]],
    }))
}

/// Formats the number in radix 64 using digits from url-safe base64 (`A` = 0, `_` = 63)
pub fn num_to_b64(num: u16) -> String {
    let b64 = from_bytes_to_text(&num.to_be_bytes().to_vec());
    // remove leading A's
    if num < 64 {
        b64[3..].to_string()
    } else if num < 4096 {
        b64[2..].to_string()
    } else {
        todo!()
    }
}

#[test]
fn num_to_b64_test() {
    assert_eq!("A", num_to_b64(0));
    assert_eq!("B", num_to_b64(1));
    assert_eq!("C", num_to_b64(2));
    assert_eq!("D", num_to_b64(3));
    assert_eq!("b", num_to_b64(27));
    assert_eq!("BQ", num_to_b64(80));
    assert_eq!("__", num_to_b64(4095));
}

#[test]
fn b64_to_num_test() {
    assert_eq!(b64_to_num("AAAA".as_bytes()).unwrap(), 0);
    assert_eq!(b64_to_num("A".as_bytes()).unwrap(), 0);
    assert_eq!(b64_to_num("B".as_bytes()).unwrap(), 1);
    assert_eq!(b64_to_num("C".as_bytes()).unwrap(), 2);
    assert_eq!(b64_to_num("D".as_bytes()).unwrap(), 3);
    assert_eq!(b64_to_num("b".as_bytes()).unwrap(), 27);
    assert_eq!(b64_to_num("BQ".as_bytes()).unwrap(), 80);
    assert_eq!(b64_to_num("__".as_bytes()).unwrap(), 4095);
    assert_eq!(b64_to_num("_".as_bytes()).unwrap(), 63);
    assert_eq!(b64_to_num("-".as_bytes()).unwrap(), 62);
}
