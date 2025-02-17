/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::borrow::Borrow;
use std::collections::btree_map::Entry;
use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Display;

use serde::{Deserialize, Serialize};

use prometheus_core::LabelName;

use crate::digest::Digestible;

#[derive(Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord, Default, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(
    feature = "tsify",
    tsify(
        from_wasm_abi,
        into_wasm_abi,
        type = "{ [key: string]: LabelSelector }"
    )
)]
#[serde(rename_all = "snake_case")]
pub struct MetricSelector(pub BTreeMap<LabelName, LabelSelector>);

#[derive(Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
#[serde(rename_all = "snake_case")]
pub enum LabelSelector {
    /// Label is optional. This selector can be used to accept any
    /// value for the label, while making sure it is matched by the
    /// schema.
    Opt,
    /// Match all non-empty values (label should be set).
    Set,
    /// Match the empty value (Label should not be set).
    Unset,
    /// Match a specific value.
    Eq(String),
    /// Exclude a specific value.
    Ne(String),
    /// Match a set of values.
    In(BTreeSet<String>),
    /// Exclude a set of values.
    NotIn(BTreeSet<String>),
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug)]
pub enum LabelSelectorPrim<'a> {
    /// Match a set of values.
    In(BTreeSet<&'a str>),
    /// Exclude a set of values.
    NotIn(BTreeSet<&'a str>),
}

impl MetricSelector {
    pub fn new() -> Self {
        Self(BTreeMap::new())
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn get<Q>(&self, label: &Q) -> Option<&LabelSelector>
    where
        LabelName: Borrow<Q> + Ord,
        Q: Ord + ?Sized,
    {
        self.0.get(label)
    }

    pub fn iter(&self) -> impl Iterator<Item = (&LabelName, &LabelSelector)> {
        self.0.iter()
    }

    pub fn keys(&self) -> impl Iterator<Item = &LabelName> {
        self.0.keys()
    }

    pub fn matches(&self, metric: &BTreeMap<LabelName, String>) -> bool {
        self.0.iter().all(|(label, selector)| {
            selector.matches(metric.get(label).map(|s| s.as_str()).unwrap_or(""))
        })
    }

    pub fn matched_labels(
        &self,
        metric: &BTreeMap<LabelName, String>,
    ) -> Option<BTreeMap<LabelName, String>> {
        self.matches(metric).then(|| {
            metric
                .iter()
                .filter(|(label, _)| self.0.contains_key(*label))
                .map(|(label, value)| (label.clone(), value.clone()))
                .collect()
        })
    }

    pub fn excludes(&self, other: &Self) -> bool {
        self.0.iter().any(|(label, selector)| {
            other
                .0
                .get(label)
                .is_some_and(|other| selector.excludes(other))
        })
    }

    pub fn is_many(&self) -> bool {
        self.0.values().any(|selector| selector.is_many())
    }

    /// Check if this query is more specific than `other`.
    pub fn is_subset(&self, other: &Self) -> bool {
        other.iter().all(|(label, other)| {
            let ours = self.0.get(label).map_or_else(
                || LabelSelectorPrim::NotIn(BTreeSet::new()),
                |selector| selector.as_prim(),
            );
            ours.is_subset(&other.as_prim())
        })
    }

    /// Check if this query is less specific than `other`.
    pub fn is_superset(&self, other: &Self) -> bool {
        self.iter().all(|(label, ours)| {
            let other = other.0.get(label).map_or_else(
                || LabelSelectorPrim::NotIn(BTreeSet::new()),
                |selector| selector.as_prim(),
            );
            ours.as_prim().is_superset(&other)
        })
    }
}

impl std::ops::BitAndAssign<&MetricSelector> for MetricSelector {
    fn bitand_assign(&mut self, rhs: &MetricSelector) {
        for (label, selector) in rhs {
            match self.0.entry(label.clone()) {
                Entry::Vacant(ent) => {
                    ent.insert(selector.clone());
                }
                Entry::Occupied(mut ent) => {
                    ent.insert(ent.get().and(selector));
                }
            }
        }
    }
}

impl std::ops::BitAnd<&MetricSelector> for MetricSelector {
    type Output = MetricSelector;
    fn bitand(mut self, rhs: &MetricSelector) -> Self::Output {
        self &= rhs;
        self
    }
}

impl LabelSelector {
    pub fn matches(&self, label: &str) -> bool {
        match self {
            Self::Opt => true,
            Self::Set => !label.is_empty(),
            Self::Unset => label.is_empty(),
            Self::Eq(value) => label == value,
            Self::Ne(value) => label != value,
            Self::In(values) => values.iter().any(|value| label == value),
            Self::NotIn(values) => values.iter().all(|value| label != value),
        }
    }

    pub(crate) fn as_prim(&self) -> LabelSelectorPrim<'_> {
        match self {
            Self::Opt => LabelSelectorPrim::NotIn(BTreeSet::new()),
            Self::Set => LabelSelectorPrim::NotIn(BTreeSet::from_iter([""])),
            Self::Unset => LabelSelectorPrim::In(BTreeSet::from_iter([""])),
            Self::Eq(value) => LabelSelectorPrim::In(BTreeSet::from_iter([value.as_str()])),
            Self::Ne(value) => LabelSelectorPrim::NotIn(BTreeSet::from_iter([value.as_str()])),
            Self::In(values) => {
                LabelSelectorPrim::In(BTreeSet::from_iter(values.iter().map(|s| s.as_str())))
            }
            Self::NotIn(values) => {
                LabelSelectorPrim::NotIn(BTreeSet::from_iter(values.iter().map(|s| s.as_str())))
            }
        }
    }

    pub fn is_many(&self) -> bool {
        match self {
            LabelSelector::Opt
            | LabelSelector::Set
            | LabelSelector::Ne(_)
            | LabelSelector::NotIn(_) => true,
            LabelSelector::Unset | LabelSelector::Eq(_) => false,
            LabelSelector::In(vs) => vs.len() > 1,
        }
    }

    pub fn excludes(&self, other: &Self) -> bool {
        self.as_prim().excludes(&other.as_prim())
    }

    pub fn is_subset(&self, other: &Self) -> bool {
        self.as_prim().is_subset(&other.as_prim())
    }

    pub fn is_superset(&self, other: &Self) -> bool {
        self.as_prim().is_superset(&other.as_prim())
    }

    pub fn and(&self, other: &Self) -> Self {
        self.as_prim().and(&other.as_prim()).to_selector()
    }

    #[allow(unused)]
    fn or(&self, other: &Self) -> Self {
        self.as_prim().or(&other.as_prim()).to_selector()
    }

    #[allow(unused)]
    fn not(&self) -> Self {
        self.as_prim().not().to_selector()
    }
}

impl LabelSelectorPrim<'_> {
    fn to_selector(&self) -> LabelSelector {
        match self {
            LabelSelectorPrim::In(values) => match values.len() {
                1 => match *values.iter().next().unwrap() {
                    "" => LabelSelector::Unset,
                    value => LabelSelector::Eq(value.to_string()),
                },
                _ => LabelSelector::In(values.iter().map(|s| s.to_string()).collect()),
            },
            LabelSelectorPrim::NotIn(values) => match values.len() {
                0 => LabelSelector::Opt,
                1 => match *values.iter().next().unwrap() {
                    "" => LabelSelector::Set,
                    value => LabelSelector::Ne(value.to_string()),
                },
                _ => LabelSelector::NotIn(values.iter().map(|s| s.to_string()).collect()),
            },
        }
    }

    fn excludes(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::In(bs), Self::In(cs)) => bs.is_disjoint(cs),
            (Self::In(bs), Self::NotIn(cs)) | (Self::NotIn(cs), Self::In(bs)) => bs.is_subset(cs),
            (Self::NotIn(_), Self::NotIn(_)) => false,
        }
    }

    fn is_subset(&self, other: &Self) -> bool {
        match (self, other) {
            (LabelSelectorPrim::In(vs), LabelSelectorPrim::In(ws)) => vs.is_subset(ws),
            (LabelSelectorPrim::In(vs), LabelSelectorPrim::NotIn(ws)) => vs.is_disjoint(ws),
            (LabelSelectorPrim::NotIn(_), LabelSelectorPrim::In(_)) => false,
            (LabelSelectorPrim::NotIn(vs), LabelSelectorPrim::NotIn(ws)) => ws.is_subset(vs),
        }
    }

    fn is_superset(&self, other: &Self) -> bool {
        match (self, other) {
            (LabelSelectorPrim::In(vs), LabelSelectorPrim::In(ws)) => vs.is_superset(ws),
            (LabelSelectorPrim::In(_), LabelSelectorPrim::NotIn(_)) => false,
            (LabelSelectorPrim::NotIn(vs), LabelSelectorPrim::In(ws)) => vs.is_disjoint(ws),
            (LabelSelectorPrim::NotIn(vs), LabelSelectorPrim::NotIn(ws)) => ws.is_superset(vs),
        }
    }

    fn or(&self, other: &Self) -> Self {
        match (self, other) {
            (Self::In(bs), Self::In(cs)) => Self::In(bs.union(cs).copied().collect()),
            (Self::NotIn(bs), Self::NotIn(cs)) => {
                Self::NotIn(bs.intersection(cs).copied().collect())
            }
            (Self::In(bs), Self::NotIn(cs)) | (Self::NotIn(cs), Self::In(bs)) => {
                Self::NotIn(cs.difference(bs).copied().collect())
            }
        }
    }

    fn and(&self, other: &Self) -> Self {
        match (self, other) {
            (Self::In(bs), Self::In(cs)) => Self::In(bs.intersection(cs).copied().collect()),
            (Self::NotIn(bs), Self::NotIn(cs)) => Self::NotIn(bs.union(cs).copied().collect()),
            (Self::In(bs), Self::NotIn(cs)) | (Self::NotIn(cs), Self::In(bs)) => {
                Self::In(bs.difference(cs).copied().collect())
            }
        }
    }

    fn not(&self) -> Self {
        match self {
            Self::In(values) => Self::NotIn(values.clone()),
            Self::NotIn(values) => Self::In(values.clone()),
        }
    }
}

impl IntoIterator for MetricSelector {
    type Item = (LabelName, LabelSelector);
    type IntoIter = std::collections::btree_map::IntoIter<LabelName, LabelSelector>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a> IntoIterator for &'a MetricSelector {
    type Item = (&'a LabelName, &'a LabelSelector);
    type IntoIter = std::collections::btree_map::Iter<'a, LabelName, LabelSelector>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl FromIterator<(LabelName, LabelSelector)> for MetricSelector {
    fn from_iter<T: IntoIterator<Item = (LabelName, LabelSelector)>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}

impl Display for MetricSelector {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let named = if let Some(LabelSelector::Eq(metric)) = self.0.get("__name__") {
            write!(f, "{metric} ")?;
            true
        } else {
            false
        };
        write!(f, "{{")?;
        self.0
            .iter()
            .filter(|(label, _)| {
                !named || <LabelName as std::borrow::Borrow<str>>::borrow(label) != "__name__"
            })
            .enumerate()
            .try_for_each(|(i, (label, selector))| {
                if i > 0 {
                    write!(f, ", ")?
                }
                write!(f, "{label}{selector}")
            })?;
        write!(f, "}}")
    }
}

impl Display for LabelSelector {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Opt => write!(f, "=~\".*\""),
            Self::Set => write!(f, "=~\".+\""),
            Self::Unset => write!(f, "=\"\""),
            Self::Eq(value) => write!(f, "=\"{}\"", ShowValue(value)),
            Self::Ne(value) => write!(f, "!=\"{}\"", ShowValue(value)),
            Self::In(values) => write!(f, "=~\"{}\"", ShowValues(values)),
            Self::NotIn(values) => write!(f, "!~\"{}\"", ShowValues(values)),
        }
    }
}

struct ShowValue<'a>(&'a str);

impl Display for ShowValue<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.chars().try_for_each(|c| match c {
            '"' | '\\' => write!(f, "\\{c}"),
            '\n' => write!(f, "\\n"),
            '\t' => write!(f, "\\t"),
            _ => write!(f, "{c}"),
        })
    }
}

struct ShowValues<'a>(&'a BTreeSet<String>);

impl Display for ShowValues<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.iter().enumerate().try_for_each(|(i, value)| {
            if i > 0 {
                write!(f, "|")?;
            }
            value.chars().try_for_each(|c| match c {
                '.' | '|' | '(' | ')' | '[' | ']' | '{' | '}' => write!(f, "\\\\{c}"),
                '"' => write!(f, "\\\""),      // ???
                '\\' => write!(f, "\\\\\\\\"), // ???
                '\n' => write!(f, "\\n"),
                '\t' => write!(f, "\\t"),
                _ => write!(f, "{c}"),
            })
        })
    }
}

impl Digestible for MetricSelector {
    type Data<'a> = ();

    fn digest(&self, ctx: &mut ring::digest::Context, version: u64, _data: Self::Data<'_>) {
        self.0.len().digest(ctx, version, ());
        for (name, value) in &self.0 {
            name.digest(ctx, version, ());
            value.digest(ctx, version, ());
        }
    }
}

impl Digestible for LabelSelector {
    type Data<'a> = ();

    fn digest(&self, ctx: &mut ring::digest::Context, version: u64, _data: Self::Data<'_>) {
        match self {
            LabelSelector::Opt => {
                "opt".digest(ctx, version, ());
            }
            LabelSelector::Set => {
                "set".digest(ctx, version, ());
            }
            LabelSelector::Unset => {
                "unset".digest(ctx, version, ());
            }
            LabelSelector::Eq(v) => {
                "eq".digest(ctx, version, ());
                v.as_str().digest(ctx, version, ());
            }
            LabelSelector::Ne(v) => {
                "ne".digest(ctx, version, ());
                v.as_str().digest(ctx, version, ());
            }
            LabelSelector::In(vs) => {
                "in".digest(ctx, version, ());
                vs.len().digest(ctx, version, ());
                for v in vs {
                    v.as_str().digest(ctx, version, ());
                }
            }
            LabelSelector::NotIn(vs) => {
                "not_in".digest(ctx, version, ());
                vs.len().digest(ctx, version, ());
                for v in vs {
                    v.as_str().digest(ctx, version, ());
                }
            }
        }
    }
}
