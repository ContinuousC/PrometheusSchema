/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::{
    collections::BTreeSet,
    ops::{BitAnd, BitOr, Sub},
};

use lazy_static::lazy_static;

use prometheus_core::{LabelName, LE_LABEL, QUANTILE_LABEL};

use super::labelmap::LabelMap;

#[derive(Default, PartialEq, PartialOrd, Eq, Ord, Clone, Debug)]
pub(crate) struct LabelSet<'a>(BTreeSet<&'a LabelName>);

lazy_static! {
    pub(crate) static ref QUANTILE_LABELSET: LabelSet<'static> =
        LabelSet(BTreeSet::from_iter([&QUANTILE_LABEL]));
    pub(crate) static ref LE_LABELSET: LabelSet<'static> =
        LabelSet(BTreeSet::from_iter([&LE_LABEL]));
    pub(crate) static ref SPECIAL_LABELSET: LabelSet<'static> =
        LabelSet(BTreeSet::from_iter([&QUANTILE_LABEL, &LE_LABEL]));
}

impl<'a> LabelSet<'a> {
    pub(crate) fn new() -> Self {
        Self(BTreeSet::new())
    }

    pub(crate) fn contains(&self, label: &LabelName) -> bool {
        self.0.contains(label)
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = &'a LabelName> + '_ {
        self.0.iter().copied()
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub(crate) fn intersection_of_maps<'b>(
        lss: impl IntoIterator<Item = &'b LabelMap<'a>>,
    ) -> LabelSet<'a>
    where
        'a: 'b,
    {
        let mut lss = lss.into_iter();
        if let Some(first) = lss.next() {
            lss.fold(first.labelset(), |mut a, b| {
                a.0.retain(|x| b.contains(x));
                a
            })
        } else {
            Self::default()
        }
    }

    fn union_of_maps<'b>(lss: impl IntoIterator<Item = &'b LabelMap<'a>>) -> LabelSet<'a>
    where
        'a: 'b,
    {
        Self(lss.into_iter().fold(BTreeSet::new(), |mut a, b| {
            a.extend(b.labels());
            a
        }))
    }

    pub(crate) fn common<'b>(lss: impl IntoIterator<Item = &'b Self>) -> Self
    where
        'a: 'b,
    {
        let mut lss = lss.into_iter();
        if let Some(first) = lss.next().cloned() {
            lss.fold(first, |mut a, b| {
                a.0.retain(|label| b.0.contains(label));
                a
            })
        } else {
            LabelSet::default()
        }
    }

    pub(crate) fn union<'b>(lss: impl IntoIterator<Item = &'b Self>) -> Self
    where
        'a: 'b,
    {
        Self(lss.into_iter().fold(BTreeSet::new(), |mut a, b| {
            a.extend(b.0.iter().copied());
            a
        }))
    }

    pub(crate) fn len(&self) -> usize {
        self.0.len()
    }

    pub(crate) fn is_subset(&self, other: &Self) -> bool {
        self.0.is_subset(&other.0)
    }

    pub(crate) fn is_superset(&self, other: &Self) -> bool {
        self.0.is_superset(&other.0)
    }

    pub(crate) fn is_strict_subset(&self, other: &Self) -> bool {
        self.len() < other.len() && self.is_subset(other)
    }

    pub(crate) fn is_strict_superset(&self, other: &Self) -> bool {
        self.len() > other.len() && self.is_superset(other)
    }

    pub(crate) fn is_disjoint(&self, other: &Self) -> bool {
        self.0.is_disjoint(&other.0)
    }

    pub(crate) fn insert(&mut self, label: &'a LabelName) -> bool {
        self.0.insert(label)
    }

    pub(crate) fn remove(&mut self, label: &'a LabelName) -> bool {
        self.0.remove(label)
    }
}

impl<'a> From<BTreeSet<&'a LabelName>> for LabelSet<'a> {
    fn from(value: BTreeSet<&'a LabelName>) -> Self {
        Self(value)
    }
}

impl<'a> FromIterator<&'a LabelName> for LabelSet<'a> {
    fn from_iter<T: IntoIterator<Item = &'a LabelName>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}

impl<'a, 'b: 'a> Sub<&LabelSet<'b>> for LabelSet<'a> {
    type Output = LabelSet<'a>;
    fn sub(mut self, rhs: &LabelSet<'b>) -> Self::Output {
        self.0.retain(|label| !rhs.0.contains(label));
        self
    }
}

impl<'a> BitOr<&LabelSet<'a>> for LabelSet<'a> {
    type Output = LabelSet<'a>;
    fn bitor(mut self, rhs: &LabelSet<'a>) -> Self::Output {
        self.0.extend(rhs.iter());
        self
    }
}

impl<'a, 'b> BitAnd<&LabelSet<'b>> for LabelSet<'a> {
    type Output = LabelSet<'a>;
    fn bitand(mut self, rhs: &LabelSet<'b>) -> Self::Output {
        self.0.retain(|label| rhs.0.contains(label));
        self
    }
}
