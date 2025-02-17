/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::{
    borrow::Cow,
    collections::{BTreeMap, BTreeSet},
    fmt::Display,
    sync::Mutex,
};

use crate::query::LabelSelectorPrim;

/// Local `LabelName` with optimized (incompatible, non-alphabetical)
/// Ord and (compatible) Eq.
#[derive(Clone, Debug)]
pub(super) struct LabelName<'a>(usize, &'a str);

/// Local `LabelValue` with optimized (incompatible, non-alphabetical)
/// Ord and (compatible) Eq.
#[derive(Clone, Debug)]
pub(super) struct LabelValue<'a>(usize, &'a str);

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Copy, Debug)]
pub(super) enum LabelSelector<'a> {
    ByPresence(bool),
    ByValue(&'a LabelValue<'a>),
}

pub(super) type LabelSet<'a> = BTreeSet<&'a LabelName<'a>>;
pub(super) type LabelMap<'a> = BTreeMap<LabelName<'a>, LabelValue<'a>>;
pub(super) type Query<'a> = BTreeMap<&'a LabelName<'a>, LabelSelector<'a>>;
pub(super) type Union<'a> = BTreeMap<&'a LabelName<'a>, BTreeSet<LabelSelector<'a>>>;
pub(super) type QueryPrim<'a> = BTreeMap<LabelName<'a>, LabelSelectorPrim<'a>>;

/* LabelName impls. */

static STATE: Mutex<BTreeMap<String, usize>> = Mutex::new(BTreeMap::new());

macro_rules! labelname_impls {
    ($t:ident) => {
        impl<'a> $t<'a> {
            pub(crate) fn new(name: &'a str) -> Self {
                let mut s = STATE.lock().unwrap();
                let i = match s.get(name) {
                    Some(i) => *i,
                    None => {
                        let i = s.len();
                        s.insert(name.to_string(), i);
                        i
                    }
                };
                Self(i, name)
            }

            pub(crate) fn as_str(&self) -> &'a str {
                self.1
            }
        }

        impl std::borrow::Borrow<str> for &$t<'_> {
            fn borrow(&self) -> &str {
                self.as_str()
            }
        }

        // impl From<String> for $t {
        //     fn from(value: String) -> Self {
        //         Self::new(value)
        //     }
        // }

        // impl From<$t> for String {
        //     fn from(val: $t) -> Self {
        //         val.1
        //     }
        // }

        impl Display for $t<'_> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", self.1)
            }
        }

        impl PartialEq for $t<'_> {
            fn eq(&self, other: &Self) -> bool {
                self.0 == other.0 // && self.1 == other.1
            }
        }

        impl Eq for $t<'_> {}

        impl PartialOrd for $t<'_> {
            fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
                Some(self.cmp(other))
            }
        }

        impl Ord for $t<'_> {
            fn cmp(&self, other: &Self) -> std::cmp::Ordering {
                self.0.cmp(&other.0)
            }
        }
    };
}

labelname_impls!(LabelName);
labelname_impls!(LabelValue);

/* Operations on label maps. */

pub(super) fn labelset_intersection<'a: 'b, 'b>(
    mask: &LabelSet<'a>,
    lss: impl IntoIterator<Item = &'b Query<'a>> + Clone,
) -> Query<'a> {
    mask.iter()
        .copied()
        .filter_map(|label| {
            let mut iter = lss.clone().into_iter();
            let first = iter.next()?;
            let value = first
                .get(label)
                .copied()
                .unwrap_or(LabelSelector::ByPresence(false));
            iter.all(|ls| {
                ls.get(label)
                    .copied()
                    .unwrap_or(LabelSelector::ByPresence(false))
                    == value
            })
            .then_some((label, value))

            // if by_value.contains(label) {
            //     let mut iter = lss.clone().into_iter();
            //     let first = iter.next()?;
            //     let value = first
            //         .get(label)
            //         .map_or(LabelSelector::ByPresence(false), LabelSelector::ByValue);
            //     iter.all(|ls| {
            //         ls.get(label)
            //             .map_or(LabelSelector::ByPresence(false), LabelSelector::ByValue)
            //             == value
            //     })
            //     .then_some((label, value))
            // } else {
            //     let mut iter = lss.clone().into_iter();
            //     let first = iter.next()?;
            //     let value = LabelSelector::ByPresence(first.contains_key(label));
            //     iter.all(|ls| LabelSelector::ByPresence(ls.contains_key(label)) == value)
            //         .then_some((label, value))
            // }
        })
        .collect()
}

pub(super) fn labelset_union<'a: 'b, 'b>(
    mask: &LabelSet<'a>,
    lss: impl IntoIterator<Item = &'b Query<'a>> + Clone,
) -> Union<'a> {
    mask.iter()
        .copied()
        .map(|label| {
            (
                label,
                lss.clone()
                    .into_iter()
                    .map(|ls| {
                        ls.get(label)
                            .copied()
                            .unwrap_or(LabelSelector::ByPresence(false))
                    })
                    .collect(),
                // if by_value.contains(label) {
                //     lss.clone()
                //         .into_iter()
                //         .map(|ls| {
                //             ls.get(label)
                //                 .map_or(LabelSelector::ByPresence(false), LabelSelector::ByValue)
                //         })
                //         .collect()
                // } else {
                //     lss.clone()
                //         .into_iter()
                //         .map(|ls| LabelSelector::ByPresence(ls.contains_key(label)))
                //         .collect()
                // },
            )
        })
        .collect()
}

pub(super) fn union_of_unions<'a>(mut a: Union<'a>, b: &Union<'a>) -> Union<'a> {
    a.iter_mut().for_each(|(label, values)| {
        if let Some(bs) = b.get(label) {
            values.extend(bs.iter().copied());
        }
    });
    a
}

pub(super) fn cumulative_union_of_unions<'a: 'b, 'b>(
    unions: impl IntoIterator<Item = &'b Union<'a>>,
) -> Vec<Cow<'b, Union<'a>>> {
    unions.into_iter().fold(Vec::new(), |mut unions, union| {
        let union = if let Some(last) = unions.last() {
            Cow::Owned(union_of_unions(last.as_ref().clone(), union))
        } else {
            Cow::Borrowed(union)
        };
        unions.push(union);
        unions
    })
}

// pub(super) fn labelmap_matches(query: &Query, lm: &LabelMap) -> bool {
//     query.iter().all(|(label, selector)| match selector {
//         LabelSelector::ByPresence(true) => lm.contains_key(label),
//         LabelSelector::ByPresence(false) => !lm.contains_key(label),
//         LabelSelector::ByValue(v) => lm.get(label).map_or(false, |w| *v == w),
//     })
// }

pub(super) fn labelset_matches(query: &Query, ls: &Query) -> bool {
    query.iter().all(|(label, selector)| {
        *selector
            == ls
                .get(label)
                .copied()
                .unwrap_or(LabelSelector::ByPresence(false))
    })
}

pub(super) fn query_as_prim<'a>(query: &Query<'a>) -> QueryPrim<'a> {
    query
        .iter()
        .map(|(label, selector)| {
            (
                (*label).clone(),
                match selector {
                    LabelSelector::ByPresence(true) => {
                        LabelSelectorPrim::NotIn(BTreeSet::from_iter([""]))
                    }
                    LabelSelector::ByPresence(false) => {
                        LabelSelectorPrim::In(BTreeSet::from_iter([""]))
                    }
                    LabelSelector::ByValue(v) => LabelSelectorPrim::In(BTreeSet::from_iter([v.1])),
                },
            )
        })
        .collect()
}
