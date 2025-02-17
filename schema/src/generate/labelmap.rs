/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::{
    borrow::Borrow,
    collections::{BTreeMap, BTreeSet},
};

use prometheus_core::LabelName;

use super::labelset::LabelSet;

#[derive(Default, PartialEq, PartialOrd, Eq, Ord, Clone, Debug)]
pub(crate) struct LabelMap<'a>(BTreeMap<&'a LabelName, &'a str>);

impl<'a> LabelMap<'a> {
    pub(crate) fn new() -> Self {
        Self(BTreeMap::new())
    }

    pub(crate) fn contains(&self, label: &LabelName) -> bool {
        self.0.contains_key(label)
    }

    pub(crate) fn get(&self, label: &LabelName) -> Option<&'a str> {
        self.0.get(label).copied()
    }

    pub(crate) fn labels(&self) -> impl Iterator<Item = &'a LabelName> + '_ {
        self.0.keys().copied()
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = (&'a LabelName, &'a str)> + '_ {
        self.0.iter().map(|(label, value)| (*label, *value))
    }

    pub(crate) fn labelset(&self) -> LabelSet<'a> {
        LabelSet::from(self.0.keys().copied().collect::<BTreeSet<_>>())
    }

    pub(crate) fn labelset_excluding(&self, other: &LabelSet<'a>) -> LabelSet<'a> {
        self.labels()
            .filter(|label| !other.contains(label))
            .collect()
    }

    pub(crate) fn insert(&mut self, label: &'a LabelName, value: &'a str) -> Option<&'a str> {
        self.0.insert(label, value)
    }

    pub(crate) fn remove<Q>(&mut self, label: &Q) -> Option<&'a str>
    where
        &'a LabelName: Borrow<Q> + Ord,
        Q: Ord + ?Sized,
    {
        self.0.remove(label)
    }
}

impl<'a> FromIterator<(&'a LabelName, &'a str)> for LabelMap<'a> {
    fn from_iter<T: IntoIterator<Item = (&'a LabelName, &'a str)>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}

impl<'a> From<BTreeMap<&'a LabelName, &'a str>> for LabelMap<'a> {
    fn from(value: BTreeMap<&'a LabelName, &'a str>) -> Self {
        Self(value)
    }
}
