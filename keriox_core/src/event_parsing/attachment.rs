use std::convert::TryFrom;

use chrono::{DateTime, FixedOffset};
use nom::{
    bytes::complete::take,
    combinator::map,
    error::ErrorKind,
    multi::{count, many0},
    Needed,
};

use crate::{
    derivation::attached_signature_code::b64_to_num,
    event::sections::seal::{EventSeal, SourceSeal},
    event_message::signature::Signature,
    event_parsing::payload_size::PayloadType,
    prefix::{AttachedSignaturePrefix, BasicPrefix, IdentifierPrefix, SelfSigningPrefix},
};

use super::{
    path::MaterialPath,
    prefix::{
        attached_signature, attached_sn, basic_prefix, prefix, self_addressing_prefix,
        self_signing_prefix,
    },
    Attachment,
};

/// returns attached source seals
fn source_seal(s: &[u8]) -> nom::IResult<&[u8], Vec<SourceSeal>> {
    let (rest, sc) = b64_count(s)?;

    let (rest, attachment) = count(
        nom::sequence::tuple((attached_sn, self_addressing_prefix)),
        sc as usize,
    )(rest)?;
    Ok((
        rest,
        attachment
            .into_iter()
            .map(|(sn, digest)| SourceSeal::new(sn, digest))
            .collect(),
    ))
}

fn event_seal(s: &[u8]) -> nom::IResult<&[u8], EventSeal> {
    let (rest, identifier) = prefix(s)?;

    let (rest, sn) = attached_sn(rest)?;
    let (rest, event_digest) = self_addressing_prefix(rest)?;
    let seal = EventSeal {
        prefix: identifier,
        sn,
        event_digest,
    };

    Ok((rest, seal))
}

pub(crate) fn b64_count(s: &[u8]) -> nom::IResult<&[u8], u16> {
    let (rest, t) = map(nom::bytes::complete::take(2u8), |b64_count| {
        b64_to_num(b64_count).map_err(|_| nom::Err::Failure((s, ErrorKind::IsNot)))
    })(s)?;

    Ok((rest, t?))
}

fn signatures(s: &[u8]) -> nom::IResult<&[u8], Vec<AttachedSignaturePrefix>> {
    let (rest, sc) = b64_count(s)?;
    count(attached_signature, sc as usize)(rest)
}

fn couplets(s: &[u8]) -> nom::IResult<&[u8], Vec<(BasicPrefix, SelfSigningPrefix)>> {
    let (rest, sc) = b64_count(s)?;

    count(
        nom::sequence::tuple((basic_prefix, self_signing_prefix)),
        sc as usize,
    )(rest)
}

fn indexed_signatures(input: &[u8]) -> nom::IResult<&[u8], Vec<AttachedSignaturePrefix>> {
    attachment(input).map(|(rest, att)| match att {
        Attachment::AttachedSignatures(sigs) => Ok((rest, sigs)),
        _ => Err(nom::Err::Error((rest, ErrorKind::IsNot))),
    })?
}

fn identifier_signatures(
    s: &[u8],
) -> nom::IResult<&[u8], Vec<(IdentifierPrefix, Vec<AttachedSignaturePrefix>)>> {
    let (rest, sc) = b64_count(s)?;
    count(
        nom::sequence::tuple((prefix, indexed_signatures)),
        sc as usize,
    )(rest)
}

fn seal_signatures(
    s: &[u8],
) -> nom::IResult<&[u8], Vec<(EventSeal, Vec<AttachedSignaturePrefix>)>> {
    let (rest, sc) = b64_count(s)?;
    count(
        nom::sequence::tuple((event_seal, indexed_signatures)),
        sc as usize,
    )(rest)
}

fn first_seen_sn(s: &[u8]) -> nom::IResult<&[u8], Vec<(u64, DateTime<FixedOffset>)>> {
    let (rest, sc) = b64_count(s)?;
    count(nom::sequence::tuple((attached_sn, timestamp)), sc as usize)(rest)
}

pub fn timestamp(s: &[u8]) -> nom::IResult<&[u8], DateTime<FixedOffset>> {
    let (more, type_c) = take(4u8)(s)?;

    match type_c {
        b"1AAG" => {
            let (rest, parsed_timestamp) = take(32u8)(more)?;

            let timestamp = {
                let dt_str = String::from_utf8(parsed_timestamp.to_vec())
                    .map_err(|_e| nom::Err::Error((s, ErrorKind::IsNot)))?
                    .replace('c', ":")
                    .replace('d', ".")
                    .replace('p', "+");
                dt_str
                    .parse::<DateTime<FixedOffset>>()
                    .map_err(|_e| nom::Err::Error((s, ErrorKind::IsNot)))?
            };

            Ok((rest, timestamp))
        }
        _ => Err(nom::Err::Error((type_c, ErrorKind::IsNot))),
    }
}

pub fn material_path(s: &[u8]) -> nom::IResult<&[u8], MaterialPath> {
    let (more, type_c) = take(2u8)(s)?;

    let payload_type = match type_c {
        b"4A" => PayloadType::A4,
        b"5A" => PayloadType::A5,
        b"6A" => PayloadType::A6,
        _ => {
            todo!()
        }
    };
    // parse amount of quadruplets
    let (more, soft_part) = b64_count(more)?;
    let full_size = soft_part * 4;
    // parse full path
    let (more, base) = take(full_size)(more)?;

    let path = MaterialPath::new(
        payload_type,
        String::from_utf8(base.to_vec()).unwrap_or_default(),
    );

    Ok((more, path))
}

pub fn attachment(s: &[u8]) -> nom::IResult<&[u8], Attachment> {
    let (rest, payload_type) = take(2u8)(s)?;
    let payload_type: PayloadType = PayloadType::try_from(
        std::str::from_utf8(payload_type).map_err(|_e| nom::Err::Failure((s, ErrorKind::IsNot)))?,
    )
    // Can't parse payload type
    .map_err(|_e| nom::Err::Error((s, ErrorKind::IsNot)))?;
    match payload_type {
        PayloadType::MG => {
            let (rest, source_seals) = source_seal(rest)?;
            Ok((rest, Attachment::SealSourceCouplets(source_seals)))
        }
        PayloadType::MF => {
            let (rest, event_seals) = seal_signatures(rest)?;
            Ok((rest, Attachment::SealSignaturesGroups(event_seals)))
        }
        PayloadType::MA => {
            let (rest, sigs) = signatures(rest)?;
            Ok((rest, Attachment::AttachedSignatures(sigs)))
        }
        PayloadType::MB => {
            let (rest, sigs) = signatures(rest)?;
            Ok((rest, Attachment::AttachedWitnessSignatures(sigs)))
        }
        PayloadType::MC => {
            let (rest, couplets) = couplets(rest)?;
            Ok((rest, Attachment::ReceiptCouplets(couplets)))
        }
        PayloadType::MH => {
            let (rest, identifier_sigs) = identifier_signatures(rest)?;
            Ok((rest, Attachment::LastEstSignaturesGroups(identifier_sigs)))
        }
        PayloadType::MV => {
            let (rest, sc) = b64_count(rest)?;
            // sc * 4 is all attachments length
            match nom::bytes::complete::take(sc * 4)(rest) {
                Ok((rest, total)) => {
                    let (extra, atts) = many0(attachment)(total)?;
                    if !extra.is_empty() {
                        // something is wrong, should not happend
                        Err(nom::Err::Incomplete(Needed::Size(
                            (sc * 4) as usize - rest.len(),
                        )))
                    } else {
                        Ok((rest, Attachment::Frame(atts)))
                    }
                }
                Err(nom::Err::Error((rest, _))) => Err(nom::Err::Incomplete(Needed::Size(
                    (sc * 4) as usize - rest.len(),
                ))),
                Err(e) => Err(e),
            }
        }
        PayloadType::ME => {
            let (rest, sc) = first_seen_sn(rest)?;
            Ok((rest, Attachment::FirstSeenReply(sc)))
        }
        PayloadType::ML => {
            let (rest, sc) = b64_count(rest)?;
            match nom::bytes::complete::take(sc * 4)(rest) {
                Ok((rest, total)) => {
                    let (extra, mp) = material_path(total)?;
                    let (_extra, attachment) = attachment(extra)?;
                    let sigs = match attachment {
                        // TODO allow more than one signature
                        Attachment::AttachedSignatures(sigs) => Signature::Transferable(None, sigs),
                        Attachment::ReceiptCouplets(sigs) => sigs
                            .into_iter()
                            .map(|(bp, sp)| Signature::NonTransferable(bp, sp))
                            .next()
                            .unwrap(),
                        Attachment::SealSignaturesGroups(sigs) => sigs
                            .into_iter()
                            .map(|(es, sigs)| Signature::Transferable(Some(es), sigs))
                            .next()
                            .unwrap(),
                        _ => todo!(),
                    };
                    Ok((rest, Attachment::PathedMaterialQuadruplet(mp, sigs)))
                }
                Err(e) => Err(e),
            }
        }
        _ => todo!(),
    }
}

#[test]
fn test_b64_count() {
    assert_eq!(b64_count("AA".as_bytes()), Ok(("".as_bytes(), 0u16)));
    assert_eq!(b64_count("BA".as_bytes()), Ok(("".as_bytes(), 64u16)));
    assert_eq!(
        b64_count("ABextra data and stuff".as_bytes(),),
        Ok(("extra data and stuff".as_bytes(), 1u16))
    );
}

#[test]
fn test_sigs() {
    use crate::{derivation::self_signing::SelfSigning, prefix::AttachedSignaturePrefix};

    assert_eq!(
        attachment("-AABAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA".as_bytes()),
        Ok(("".as_bytes(), Attachment::AttachedSignatures(vec![AttachedSignaturePrefix::new(SelfSigning::Ed25519Sha512, vec![0u8; 64], 0)])))
    );

    assert!(attachment("-AABAA0Q7bqPvenjWXo_YIikMBKOg-pghLKwBi1Plm0PEqdv67L1_c6dq9bll7OFnoLp0a74Nw1cBGdjIPcu-yAllHAw".as_bytes()).is_ok());

    assert_eq!(
        attachment("-AACAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA0AACAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAextra data".as_bytes()),
        Ok(("extra data".as_bytes(), Attachment::AttachedSignatures(vec![
            AttachedSignaturePrefix::new(SelfSigning::Ed25519Sha512, vec![0u8; 64], 0),
            AttachedSignaturePrefix::new(SelfSigning::Ed448, vec![0u8; 114], 2)
        ])))
    );

    assert_eq!(
        attachment("-AACAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA0AACAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA".as_bytes()),
        Ok(("".as_bytes(), Attachment::AttachedSignatures(vec![
            AttachedSignaturePrefix::new(SelfSigning::Ed25519Sha512, vec![0u8; 64], 0),
            AttachedSignaturePrefix::new(SelfSigning::Ed448, vec![0u8; 114], 2)
        ])))
    )
}

#[test]
fn test_attachement() {
    let attached_str = "-GAC0AAAAAAAAAAAAAAAAAAAAAAQE3fUycq1G-P1K1pL2OhvY6ZU-9otSa3hXiCcrxuhjyII0AAAAAAAAAAAAAAAAAAAAAAQE3fUycq1G-P1K1pL2OhvY6ZU-9otSa3hXiCcrxuhjyII";
    let (_rest, attached_sn_dig) = attachment(attached_str.as_bytes()).unwrap();
    assert_eq!(
        attached_sn_dig,
        Attachment::SealSourceCouplets(vec![
            SourceSeal {
                sn: 1,
                digest: "E3fUycq1G-P1K1pL2OhvY6ZU-9otSa3hXiCcrxuhjyII"
                    .parse()
                    .unwrap()
            },
            SourceSeal {
                sn: 1,
                digest: "E3fUycq1G-P1K1pL2OhvY6ZU-9otSa3hXiCcrxuhjyII"
                    .parse()
                    .unwrap()
            }
        ])
    );

    let attached_str = "-FABED9EB3sA5u2vCPOEmX3d7bEyHiSh7Xi8fjew2KMl3FQM0AAAAAAAAAAAAAAAAAAAAAAAEeGqW24EnxUgO_wfuFo6GR_vii-RNv5iGo8ibUrhe6Z0-AABAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA";
    let (_rest, seal) = attachment(attached_str.as_bytes()).unwrap();
    assert_eq!(
        seal,
        Attachment::SealSignaturesGroups(vec![
            (
                EventSeal {
                    prefix: "ED9EB3sA5u2vCPOEmX3d7bEyHiSh7Xi8fjew2KMl3FQM"
                        .parse()
                        .unwrap(),
                    sn: 0,
                    event_digest: "EeGqW24EnxUgO_wfuFo6GR_vii-RNv5iGo8ibUrhe6Z0"
                        .parse()
                        .unwrap()
                },
                vec!["AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA".parse().unwrap()]
        )
        ])
    );

    let attached_str = "-CABBed2Tpxc8KeCEWoq3_RKKRjU_3P-chSser9J4eAtAK6I0B8npsG58rX1ex73gaGe-jvRnw58RQGsDLzoSXaGn-kHRRNu6Kb44zXDtMnx-_8CjnHqskvDbz6pbEbed3JTOnCQ";
    let (_rest, seal) = attachment(attached_str.as_bytes()).unwrap();
    assert_eq!(seal, Attachment::ReceiptCouplets(
        vec![
            ("Bed2Tpxc8KeCEWoq3_RKKRjU_3P-chSser9J4eAtAK6I".parse().unwrap(), "0B8npsG58rX1ex73gaGe-jvRnw58RQGsDLzoSXaGn-kHRRNu6Kb44zXDtMnx-_8CjnHqskvDbz6pbEbed3JTOnCQ".parse().unwrap())
            ]
        )
    );

    let cesr_attachment = "-VAj-HABE4YPqsEOaPNaZxVIbY-Gx2bJgP-c7AH_K7pEE-YfcI9E-AABAAMX88afPpEfF_HF-E-1uZKyv8b_TdILi2x8vC3Yi7Q7yzHn2fR6Bkl2yn-ZxPqmsTfV3f-H_VQwMgk7jYEukVCA";
    let (rest, att) = attachment(cesr_attachment.as_bytes()).unwrap();
    assert!(matches!(att, Attachment::Frame(_)));
    assert!(rest.is_empty());
}

#[test]
fn test_pathed_material() {
    let attached_str = "-LAZ5AABAA-a-AABAAFjjD99-xy7J0LGmCkSE_zYceED5uPF4q7l8J23nNQ64U-oWWulHI5dh3cFDWT4eICuEQCALdh8BO5ps-qx0qBA";
    let (_rest, attached_material) = attachment(attached_str.as_bytes()).unwrap();
    assert!(matches!(
        attached_material,
        Attachment::PathedMaterialQuadruplet(_, _)
    ));
}

#[test]
fn test_pathed_materialg() {
    let attached_str = "6AABAAA-";
    let (_rest, attached_material) = material_path(attached_str.as_bytes()).unwrap();
    assert_eq!(
        attached_material,
        MaterialPath::new(PayloadType::A6, "-".into())
    );
}
