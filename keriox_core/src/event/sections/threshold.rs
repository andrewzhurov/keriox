use std::{fmt, str::FromStr};

use fraction::{Fraction, One, Zero};
use itertools::Itertools;
use serde::ser::{SerializeMap, Serializer};
use serde::{
    de,
    de::{MapAccess, Visitor},
    Deserialize, Deserializer, Serialize,
};
use serde_hex::{Compact, SerHex};
use std::ops::ControlFlow;

#[derive(Debug, thiserror::Error, Serialize, Deserialize)]
pub enum ThresholdError {
    #[error("Error parsing numerical value")]
    ParseIntError,
    #[error("Wrong threshold value. Should be fraction")]
    FractionExpected,
}

impl From<core::num::ParseIntError> for ThresholdError {
    fn from(_: core::num::ParseIntError) -> Self {
        ThresholdError::ParseIntError
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ThresholdFraction {
    fraction: Fraction,
}

impl ThresholdFraction {
    pub fn new(n: u64, d: u64) -> Self {
        Self {
            fraction: Fraction::new(n, d),
        }
    }
}

impl fmt::Display for ThresholdFraction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.fraction)
    }
}

impl FromStr for ThresholdFraction {
    type Err = ThresholdError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let f: Vec<_> = s.split('/').collect();
        if f.len() > 2 {
            Err(ThresholdError::FractionExpected)
        } else if f.len() == 1 {
            let a = f[0].parse::<u64>()?;
            Ok(ThresholdFraction {
                fraction: Fraction::new(a, 1u64),
            })
        } else {
            let a = f[0].parse::<u64>()?;
            let b = f[1].parse::<u64>()?;
            Ok(ThresholdFraction {
                fraction: Fraction::new(a, b),
            })
        }
    }
}
impl<'de> Deserialize<'de> for ThresholdFraction {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        FromStr::from_str(&s).map_err(de::Error::custom)
    }
}

impl Serialize for ThresholdFraction {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(untagged)]
pub enum SignatureThreshold {
    #[serde(with = "SerHex::<Compact>")]
    Simple(u64),
    Claused(ClausedThreshold),
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(untagged)]
pub enum ClausedThreshold {
    Single(ThresholdClause),
    Multi(ThresholdClauses),
}

#[derive(Serialize, Debug, Clone, PartialEq)]
#[serde(transparent)]
pub struct ThresholdClauses(
    #[serde(skip)] usize,
    #[serde(skip)] Vec<(usize, usize)>,
    Vec<ThresholdClause>,
);

#[derive(Serialize, Debug, Clone, PartialEq)]
#[serde(transparent)]
pub struct ThresholdClause(
    #[serde(skip)] usize,
    #[serde(skip)] Vec<(usize, usize)>,
    Vec<ThresholdClauseEl>,
);

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(untagged)]
pub enum ThresholdClauseEl {
    Weight(ThresholdFraction),
    #[serde(
        serialize_with = "serialize_weighted_claused_threshold",
        deserialize_with = "deserialize_weighted_claused_threshold"
    )]
    WeightedClausedThreshold(ThresholdFraction, ClausedThreshold),
}

impl ClausedThreshold {
    pub fn len(&self) -> usize {
        match self {
            ClausedThreshold::Single(ref threshold_clause) => threshold_clause.len(),
            ClausedThreshold::Multi(ref threshold_clauses) => threshold_clauses.len(),
        }
    }
}

impl ThresholdClauses {
    pub fn len(&self) -> usize {
        self.0
    }
    pub fn idxes(&self) -> &Vec<(usize, usize)> {
        &self.1
    }
    pub fn clauses(&self) -> &Vec<ThresholdClause> {
        &self.2
    }
}

impl ThresholdClause {
    pub fn len(&self) -> usize {
        self.0
    }
    pub fn idxes(&self) -> &Vec<(usize, usize)> {
        &self.1
    }
    pub fn clause(&self) -> &Vec<ThresholdClauseEl> {
        &self.2
    }
}

fn serialize_weighted_claused_threshold<S>(
    weight: &ThresholdFraction,
    claused_threshold: &ClausedThreshold,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    let mut map = serializer.serialize_map(Some(1))?;
    map.serialize_entry(weight, claused_threshold)?;
    map.end()
}

fn deserialize_weighted_claused_threshold<'de, D>(
    deserializer: D,
) -> Result<(ThresholdFraction, ClausedThreshold), D::Error>
where
    D: Deserializer<'de>,
{
    struct ClauseVisitor;

    impl<'de> Visitor<'de> for ClauseVisitor {
        type Value = (ThresholdFraction, ClausedThreshold);

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str("a map of {weight: clause}")
        }

        fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
        where
            A: MapAccess<'de>,
        {
            let (weight, claused_threshold) = map
                .next_entry()?
                .ok_or(serde::de::Error::missing_field("weight"))?;
            Ok((weight, claused_threshold))
        }
    }

    deserializer.deserialize_map(ClauseVisitor)
}

impl<'de> Deserialize<'de> for ThresholdClauses {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let clauses = Vec::<ThresholdClause>::deserialize(deserializer)?;
        Ok(Self::from(clauses))
    }
}

impl<'de> Deserialize<'de> for ThresholdClause {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let clause_els = Vec::<ThresholdClauseEl>::deserialize(deserializer)?;
        Ok(Self::from(clause_els))
    }
}

pub fn create_sig_mask<I: IntoIterator<Item = usize>>(idxes: I, len: usize) -> Vec<bool> {
    let mut mask = vec![false; len];
    for idx in idxes {
        if idx < len {
            mask[idx] = true;
        }
    }
    mask
}

impl SignatureThreshold {
    pub fn enough_signatures<I: IntoIterator<Item = usize>>(&self, indexes_iter: I) -> bool {
        match self {
            SignatureThreshold::Simple(ref t) => {
                indexes_iter.into_iter().unique().count() as u64 >= *t
            }
            SignatureThreshold::Claused(ref claused_threshold) => {
                let sig_mask = create_sig_mask(indexes_iter, claused_threshold.len());
                claused_threshold.enough_signatures(&sig_mask)
            }
        }
    }
}

impl ClausedThreshold {
    pub fn enough_signatures(&self, sig_mask: &[bool]) -> bool {
        match self {
            ClausedThreshold::Single(ref threshold_clause) => {
                threshold_clause.enough_signatures(sig_mask)
            }
            ClausedThreshold::Multi(ref threshold_clauses) => {
                threshold_clauses.enough_signatures(sig_mask)
            }
        }
    }
}

impl ThresholdClauses {
    pub fn enough_signatures(&self, sig_mask: &[bool]) -> bool {
        self.clauses().iter().enumerate().all(|(idx, clause)| {
            let (from_idx, to_idx) = self.idxes()[idx];
            clause.enough_signatures(&sig_mask[from_idx..to_idx])
        })
    }
}

impl ThresholdClause {
    pub fn enough_signatures(&self, sig_mask: &[bool]) -> bool {
        let res = self
            .clause()
            .iter()
            .enumerate()
            .filter_map(|(el_idx, clause_el)| {
                let (from_idx, to_idx) = self.idxes()[el_idx];
                if clause_el.enough_signatures(&sig_mask[from_idx..to_idx]) {
                    Some(clause_el.weight())
                } else {
                    None
                }
            })
            .try_fold(Zero::zero(), |acc: Fraction, el| {
                let new_acc = acc + el.fraction;
                if new_acc >= One::one() {
                    ControlFlow::Break(true)
                } else {
                    ControlFlow::Continue(new_acc)
                }
            });
        res.is_break()
    }
}

impl ThresholdClauseEl {
    fn weight(&self) -> &ThresholdFraction {
        match self {
            Self::Weight(weight) => weight,
            Self::WeightedClausedThreshold(weight, _) => weight,
        }
    }

    fn enough_signatures(&self, sig_mask: &[bool]) -> bool {
        match self {
            Self::Weight(_) => sig_mask[0],
            Self::WeightedClausedThreshold(_, claused_threshold) => {
                claused_threshold.enough_signatures(sig_mask)
            }
        }
    }
}

impl Default for SignatureThreshold {
    fn default() -> Self {
        Self::Simple(1)
    }
}

impl From<u64> for SignatureThreshold {
    fn from(n: u64) -> Self {
        Self::Simple(n)
    }
}

// ClausedThreshold::Single
impl From<Vec<(u64, u64)>> for SignatureThreshold {
    fn from(fracts: Vec<(u64, u64)>) -> Self {
        Self::Claused(ClausedThreshold::from(fracts))
    }
}

impl From<Vec<ThresholdClauseEl>> for SignatureThreshold {
    fn from(threshold_clause_els: Vec<ThresholdClauseEl>) -> Self {
        Self::Claused(ClausedThreshold::from(threshold_clause_els))
    }
}

impl From<Vec<(u64, u64)>> for ClausedThreshold {
    fn from(fracts: Vec<(u64, u64)>) -> Self {
        Self::Single(ThresholdClause::from(fracts))
    }
}

impl From<Vec<ThresholdClauseEl>> for ClausedThreshold {
    fn from(threshold_clause_els: Vec<ThresholdClauseEl>) -> Self {
        Self::Single(ThresholdClause::from(threshold_clause_els))
    }
}

impl From<Vec<(u64, u64)>> for ThresholdClause {
    fn from(fracts: Vec<(u64, u64)>) -> Self {
        let threshold_clause_els = fracts
            .into_iter()
            .map(|(n, d)| ThresholdClauseEl::Weight(ThresholdFraction::new(n, d)))
            .collect::<Vec<ThresholdClauseEl>>();
        Self::from(threshold_clause_els)
    }
}

impl From<Vec<ThresholdClauseEl>> for ThresholdClause {
    fn from(clause: Vec<ThresholdClauseEl>) -> Self {
        let (len, idxes) = clause.iter().fold(
            (0, Vec::with_capacity(clause.len())),
            |(len_acc, mut idxes_acc), clause_el| {
                let clause_el_len = match clause_el {
                    ThresholdClauseEl::Weight(_) => 1,
                    ThresholdClauseEl::WeightedClausedThreshold(_, claused_threshold) => {
                        claused_threshold.len()
                    }
                };
                let from = idxes_acc.last().map(|(_, prev_to)| *prev_to).unwrap_or(0);
                let to = from + clause_el_len;
                idxes_acc.push((from, to));
                (len_acc + clause_el_len, idxes_acc)
            },
        );
        Self(len, idxes, clause)
    }
}

// ClausedThreshold::Multi
impl From<Vec<Vec<(u64, u64)>>> for SignatureThreshold {
    fn from(fracts_vec: Vec<Vec<(u64, u64)>>) -> Self {
        Self::Claused(ClausedThreshold::from(fracts_vec))
    }
}

impl From<Vec<Vec<(u64, u64)>>> for ClausedThreshold {
    fn from(fracts_vec: Vec<Vec<(u64, u64)>>) -> Self {
        Self::Multi(ThresholdClauses::from(fracts_vec))
    }
}

impl From<Vec<Vec<(u64, u64)>>> for ThresholdClauses {
    fn from(fracts_vec: Vec<Vec<(u64, u64)>>) -> Self {
        let threshold_clauses = fracts_vec
            .into_iter()
            .map(ThresholdClause::from)
            .collect::<Vec<ThresholdClause>>();
        Self::from(threshold_clauses)
    }
}

impl From<Vec<ThresholdClause>> for ThresholdClauses {
    fn from(clauses: Vec<ThresholdClause>) -> Self {
        let (len, idxes) = clauses.iter().fold(
            (0, Vec::with_capacity(clauses.len())),
            |(len_acc, mut idxes_acc), clause| {
                let from = idxes_acc
                    .last()
                    .map(|(_prev_from, prev_to)| *prev_to)
                    .unwrap_or(0);
                let to = from + clause.len();
                idxes_acc.push((from, to));
                (len_acc + clause.len(), idxes_acc)
            },
        );
        Self(len, idxes, clauses)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_threshold() -> Result<(), Box<dyn std::error::Error>> {
        let s1 = SignatureThreshold::Simple(1);
        let s1_ser = serde_json::to_string(&s1)?;
        assert_eq!(r#""1""#, s1_ser);
        let s1_deser: SignatureThreshold = serde_json::from_str(&s1_ser)?;
        assert_eq!(s1, s1_deser);

        assert!(!s1.enough_signatures(vec![]));
        assert!(s1.enough_signatures(vec![0]));
        assert!(s1.enough_signatures(vec![0, 1]));
        assert!(s1.enough_signatures(vec![1, 3, 4]));

        let s2 = SignatureThreshold::Simple(2);
        let s2_ser = serde_json::to_string(&s2)?;
        assert_eq!(r#""2""#, s2_ser);
        let s2_deser: SignatureThreshold = serde_json::from_str(&s2_ser)?;
        assert_eq!(s2, s2_deser);

        assert!(!s2.enough_signatures(vec![]));
        assert!(!s2.enough_signatures(vec![0]));
        assert!(s2.enough_signatures(vec![0, 1]));
        assert!(s2.enough_signatures(vec![1, 2]));
        assert!(s2.enough_signatures(vec![0, 2, 3]));

        Ok(())
    }

    #[test]
    fn test_single_clause_threshold() -> Result<(), Box<dyn std::error::Error>> {
        let s1 = SignatureThreshold::from(vec![(1, 2), (1, 2), (1, 2)]);
        let s1_ser = serde_json::to_string(&s1)?;
        assert_eq!(r#"["1/2","1/2","1/2"]"#, s1_ser);
        let s1_deser: SignatureThreshold = serde_json::from_str(&s1_ser)?;
        assert_eq!(s1, s1_deser);

        assert!(!s1.enough_signatures(vec![]));
        assert!(!s1.enough_signatures(vec![0]));
        assert!(s1.enough_signatures(vec![0, 1]));
        assert!(s1.enough_signatures(vec![0, 1, 2]));
        assert!(s1.enough_signatures(vec![0, 1, 2, 3]));
        assert!(s1.enough_signatures(vec![1, 2, 3]));
        assert!(!s1.enough_signatures(vec![2, 3, 4])); // 3 and 4 are not correct idxes, not counted
        assert!(s1.enough_signatures(vec![2, 0]));
        assert!(s1.enough_signatures(vec![3, 0, 1]));

        Ok(())
    }

    #[test]
    fn test_multi_clause_threshold() -> Result<(), Box<dyn std::error::Error>> {
        let s1 = SignatureThreshold::from(vec![vec![(1, 2), (1, 2), (1, 2)], vec![(1, 1)]]);
        let s1_ser = serde_json::to_string(&s1)?;
        assert_eq!(r#"[["1/2","1/2","1/2"],["1"]]"#, s1_ser);
        let s1_deser: SignatureThreshold = serde_json::from_str(&s1_ser)?;
        assert_eq!(s1, s1_deser);

        assert!(!s1.enough_signatures(vec![]));
        assert!(!s1.enough_signatures(vec![0]));
        assert!(!s1.enough_signatures(vec![0, 1]));
        assert!(!s1.enough_signatures(vec![0, 1, 2]));
        assert!(s1.enough_signatures(vec![0, 1, 2, 3]));
        assert!(s1.enough_signatures(vec![1, 2, 3]));
        assert!(!s1.enough_signatures(vec![2, 3, 4])); // 4 is not a idx, not counted
        assert!(s1.enough_signatures(vec![2, 3, 0]));
        assert!(s1.enough_signatures(vec![3, 0, 1]));
        assert!(s1.enough_signatures(vec![4, 3, 0, 1]));

        Ok(())
    }

    #[test]
    fn test_single_clause_threshold_with_weighted_clause() -> Result<(), Box<dyn std::error::Error>>
    {
        let s1 = SignatureThreshold::from(vec![
            ThresholdClauseEl::Weight(ThresholdFraction::new(1, 2)),
            ThresholdClauseEl::WeightedClausedThreshold(
                ThresholdFraction::new(1, 2),
                ClausedThreshold::from(vec![(1, 2), (1, 2), (1, 2)]),
            ),
            ThresholdClauseEl::WeightedClausedThreshold(
                ThresholdFraction::new(1, 2),
                ClausedThreshold::from(vec![(1, 2), (1, 2), (1, 2)]),
            ),
        ]);
        let s1_ser = serde_json::to_string(&s1)?;
        assert_eq!(
            r#"["1/2",{"1/2":["1/2","1/2","1/2"]},{"1/2":["1/2","1/2","1/2"]}]"#,
            s1_ser
        );
        let s1_deser: SignatureThreshold = serde_json::from_str(&s1_ser)?;
        assert_eq!(s1, s1_deser);

        assert!(!s1.enough_signatures(vec![]));
        assert!(!s1.enough_signatures(vec![0]));
        assert!(!s1.enough_signatures(vec![0, 1]));
        assert!(s1.enough_signatures(vec![0, 1, 2]));
        assert!(s1.enough_signatures(vec![0, 1, 2, 3]));
        assert!(s1.enough_signatures(vec![0, 1, 2, 3, 4]));
        assert!(s1.enough_signatures(vec![0, 1, 2, 3, 4]));
        assert!(s1.enough_signatures(vec![0, 1, 2, 3, 4, 5]));
        assert!(s1.enough_signatures(vec![0, 1, 2, 3, 4, 5, 6]));
        assert!(s1.enough_signatures(vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 9]));

        assert!(!s1.enough_signatures(vec![0, 1, 4]));
        assert!(!s1.enough_signatures(vec![1, 2, 4]));
        assert!(!s1.enough_signatures(vec![5, 1, 6]));

        assert!(s1.enough_signatures(vec![2, 1, 4, 5]));
        assert!(s1.enough_signatures(vec![0, 4, 6]));

        Ok(())
    }

    #[test]
    fn test_single_clause_threshold_with_deeply_nested_weighted_clauses(
    ) -> Result<(), Box<dyn std::error::Error>> {
        let s1 = SignatureThreshold::from(vec![
            ThresholdClauseEl::Weight(ThresholdFraction::new(1, 2)),
            ThresholdClauseEl::WeightedClausedThreshold(
                ThresholdFraction::new(1, 2),
                ClausedThreshold::from(vec![
                    ThresholdClauseEl::Weight(ThresholdFraction::new(1, 2)),
                    ThresholdClauseEl::WeightedClausedThreshold(
                        ThresholdFraction::new(1, 2),
                        ClausedThreshold::from(vec![(1, 2), (1, 2)]),
                    ),
                    ThresholdClauseEl::WeightedClausedThreshold(
                        ThresholdFraction::new(1, 2),
                        ClausedThreshold::from(vec![(1, 2), (1, 2)]),
                    ),
                ]),
            ),
            ThresholdClauseEl::WeightedClausedThreshold(
                ThresholdFraction::new(1, 2),
                ClausedThreshold::from(vec![(1, 2), (1, 2), (1, 2)]),
            ),
        ]);
        let s1_ser = serde_json::to_string(&s1)?;

        assert_eq!(
            //    0             1             2     3               4     5                 6     7     8
            r#"["1/2",{"1/2":["1/2",{"1/2":["1/2","1/2"]},{"1/2":["1/2","1/2"]}]},{"1/2":["1/2","1/2","1/2"]}]"#,
            s1_ser
        );
        let s1_deser: SignatureThreshold = serde_json::from_str(&s1_ser)?;
        assert_eq!(s1, s1_deser);

        assert!(!s1.enough_signatures(vec![]));
        assert!(!s1.enough_signatures(vec![0]));
        assert!(!s1.enough_signatures(vec![0, 1]));
        assert!(!s1.enough_signatures(vec![0, 1, 2]));
        assert!(s1.enough_signatures(vec![0, 1, 2, 3]));

        assert!(!s1.enough_signatures(vec![0, 1, 2, 4]));
        assert!(s1.enough_signatures(vec![0, 1, 2, 4, 5]));

        assert!(!s1.enough_signatures(vec![0, 1, 2, 4, 6]));
        assert!(s1.enough_signatures(vec![0, 1, 2, 4, 6, 7]));

        assert!(!s1.enough_signatures(vec![1, 2, 4, 6, 7, 8]));
        assert!(s1.enough_signatures(vec![1, 2, 3, 4, 7, 8]));

        assert!(s1.enough_signatures(vec![2, 3, 4, 5, 6, 7]));
        assert!(!s1.enough_signatures(vec![2, 3, 4, 5, 6]));
        assert!(!s1.enough_signatures(vec![2, 3, 4, 6, 7]));
        assert!(!s1.enough_signatures(vec![3, 4, 5, 6, 7]));

        assert!(s1.enough_signatures(vec![0, 2, 3, 4, 5]));
        assert!(s1.enough_signatures(vec![0, 7, 8]));

        Ok(())
    }

    // 'threshold' is an 'and' - all threshold's clauses must be satisfied
    #[test]
    fn test_threshold_with_with_nested_threshold() -> Result<(), Box<dyn std::error::Error>> {
        let s1 = SignatureThreshold::from(vec![
            ThresholdClauseEl::WeightedClausedThreshold(
                ThresholdFraction::new(1, 2),
                ClausedThreshold::from(vec![vec![(1, 1)], vec![(1, 2), (1, 2), (1, 2)]]),
            ),
            ThresholdClauseEl::WeightedClausedThreshold(
                ThresholdFraction::new(1, 2),
                ClausedThreshold::from(vec![(1, 2), (1, 2), (1, 2)]),
            ),
            ThresholdClauseEl::WeightedClausedThreshold(
                ThresholdFraction::new(1, 2),
                ClausedThreshold::from(vec![(1, 2), (1, 2), (1, 2)]),
            ),
        ]);
        let s1_ser = serde_json::to_string(&s1)?;

        assert_eq!(
            //         Alice                               Bob                         Carol
            //            0      1     2     3                4     5     6               7     8     9
            r#"[{"1/2":[["1"],["1/2","1/2","1/2"]]},{"1/2":["1/2","1/2","1/2"]},{"1/2":["1/2","1/2","1/2"]}]"#,
            s1_ser
        );
        let s1_deser: SignatureThreshold = serde_json::from_str(&s1_ser)?;
        assert_eq!(s1, s1_deser);

        assert!(!s1.enough_signatures(vec![]));
        assert!(!s1.enough_signatures(vec![0]));
        assert!(!s1.enough_signatures(vec![0, 1]));
        assert!(!s1.enough_signatures(vec![0, 1, 2]));
        assert!(!s1.enough_signatures(vec![0, 1, 2, 3]));
        assert!(!s1.enough_signatures(vec![0, 1, 2, 3, 4])); // Alice can't satisfy on her own (duhh)
        assert!(!s1.enough_signatures(vec![0, 1, 2, 3, 4, 7])); // one sig per Bob and Carol is not enough, 2 min
        assert!(s1.enough_signatures(vec![0, 1, 2, 3, 4, 5])); // Alice + Bob can satisfy
        assert!(s1.enough_signatures(vec![0, 1, 2, 4, 5])); // only 2 of 1,2,3 is enough to satisfy the second clause of Alice's threshold/'and'
        assert!(s1.enough_signatures(vec![0, 2, 3, 4, 5])); // only 2 of 1,2,3

        assert!(!s1.enough_signatures(vec![1, 2, 3, 4, 5])); // Alice's 0 sig is required her threshold/'and' pass
        assert!(s1.enough_signatures(vec![5, 6, 8, 9])); // Bob and Carol can satisfy without Alice
        Ok(())
    }

    #[test]
    fn test_threshold_enough_sigs() {
        // Threshold: [[1/1], [1/2, 1/2, 1/2], [1/2,1/2]]
        let wt = SignatureThreshold::from(vec![vec![(1, 1)], vec![(1, 2), (1, 2), (1, 2)]]);
        let sigs_indexes: Vec<usize> = vec![0, 1, 2, 3];

        // All signatures.
        assert!(wt.enough_signatures(sigs_indexes));

        // Enough signatures.
        let enough = vec![0, 1, 3];
        assert!(wt.enough_signatures(enough));

        let not_enough = vec![0];
        assert!(!wt.enough_signatures(not_enough));
    }

    #[test]
    pub fn test_multi_claused_treshold_serialization() {
        let multi_threshold = r#"[["1/1"],["1/2","1/2","1/2"]]"#.to_string();
        let wt: SignatureThreshold = serde_json::from_str(&multi_threshold).unwrap();
        assert_eq!(
            wt,
            SignatureThreshold::Claused(ClausedThreshold::Multi(ThresholdClauses(
                4,
                vec![(0, 1), (1, 4)],
                vec![
                    ThresholdClause(
                        1,
                        vec![(0, 1)],
                        vec![ThresholdClauseEl::Weight(ThresholdFraction::new(1, 1))]
                    ),
                    ThresholdClause(
                        3,
                        vec![(0, 1), (1, 2), (2, 3)],
                        vec![
                            ThresholdClauseEl::Weight(ThresholdFraction::new(1, 2)),
                            ThresholdClauseEl::Weight(ThresholdFraction::new(1, 2)),
                            ThresholdClauseEl::Weight(ThresholdFraction::new(1, 2)),
                        ]
                    )
                ],
            )))
        );
        // assert_eq!(serde_json::to_string(&wt).unwrap(), multi_threshold);
        assert_eq!(
            serde_json::to_string(&wt).unwrap(),
            r#"[["1"],["1/2","1/2","1/2"]]"#.to_string()
        );

        let single_threshold = r#"["1/2","1/2","1/2"]"#.to_string();
        let wt: SignatureThreshold = serde_json::from_str(&single_threshold).unwrap();
        assert_eq!(
            wt,
            SignatureThreshold::Claused(ClausedThreshold::Single(ThresholdClause(
                3,
                vec![(0, 1), (1, 2), (2, 3)],
                vec![
                    ThresholdClauseEl::Weight(ThresholdFraction::new(1, 2)),
                    ThresholdClauseEl::Weight(ThresholdFraction::new(1, 2)),
                    ThresholdClauseEl::Weight(ThresholdFraction::new(1, 2)),
                ],
            )))
        );
        assert_eq!(serde_json::to_string(&wt).unwrap(), single_threshold);
    }
}
