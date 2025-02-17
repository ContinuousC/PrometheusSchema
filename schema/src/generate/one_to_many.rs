/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::{
    borrow::Borrow,
    collections::{BTreeMap, BTreeSet},
};

use serde::{Deserialize, Serialize};

/// A doubly-indexed map where every L is related to one or more R.
#[derive(Clone, Debug)]
pub struct OneToMany<L, R> {
    left: BTreeMap<L, BTreeSet<R>>,
    right: BTreeMap<R, L>,
}

impl<L, R> OneToMany<L, R> {
    pub fn new() -> Self {
        Self {
            left: BTreeMap::new(),
            right: BTreeMap::new(),
        }
    }

    /// Insert `l` <-> `r`, moving `r` from the previous `l` if necessary.
    pub fn insert_updating_one(&mut self, l: L, r: R) -> Option<L>
    where
        L: Ord + Clone,
        R: Ord + Clone,
    {
        let pl = self.right.insert(r.clone(), l.clone());
        if let Some(pl) = pl.as_ref().filter(|pl| *pl != &l) {
            let removed = self.left.get_mut(pl).unwrap().remove(&r);
            assert!(removed);
        }
        self.left.entry(l).or_default().insert(r);
        pl
    }

    /// Insert `l` <-> `r`, updating the old `l` if necessary.
    pub fn insert_updating_all(&mut self, l: L, r: R) -> Option<L>
    where
        L: Ord + Clone,
        R: Ord + Clone,
    {
        match self.right.insert(r.clone(), l.clone()) {
            Some(pl) => {
                let prs = self.left.remove(&pl).unwrap();
                prs.iter().filter(|pr| &r != *pr).for_each(|r| {
                    let removed = self.right.insert(r.clone(), l.clone()).unwrap();
                    assert!(removed == pl);
                });
                self.left.entry(l).or_default().extend(prs);
                Some(pl)
            }
            None => {
                let added = self.left.entry(l).or_default().insert(r);
                assert!(added);
                None
            }
        }
    }

    pub fn remove_left<Q>(&mut self, l: &Q) -> Option<BTreeSet<R>>
    where
        L: Borrow<Q> + Ord,
        Q: Ord + ?Sized,
        R: Ord,
    {
        if let Some(rs) = self.left.remove(l) {
            rs.iter().for_each(|r| {
                let removed = self.right.remove(r).unwrap();
                assert!(removed.borrow() == l);
            });
            Some(rs)
        } else {
            None
        }
    }

    pub fn remove_right<Q>(&mut self, r: &Q) -> Option<L>
    where
        R: Borrow<Q> + Ord,
        Q: Ord + ?Sized,
        L: Ord,
    {
        if let Some(l) = self.right.remove(r) {
            let rs = self.left.get_mut(&l).unwrap();
            let removed = rs.remove(r);
            assert!(removed);
            if rs.is_empty() {
                self.left.remove(&l).unwrap();
            }
            Some(l)
        } else {
            None
        }
    }

    pub fn remove<P, Q>(&mut self, l: &P, r: &Q) -> bool
    where
        L: Borrow<P> + Ord,
        R: Borrow<Q> + Ord,
        P: Ord + ?Sized,
        Q: Ord + ?Sized,
    {
        if self.right.get(r).is_some_and(|cl| cl.borrow() == l) {
            let removed_l = self.right.remove(r).is_some();
            let rs = self.left.get_mut(l).unwrap();
            let removed_r = rs.remove(r);
            assert!(removed_l && removed_r);
            if rs.is_empty() {
                self.left.remove(l).unwrap();
            }
            true
        } else {
            false
        }
    }

    pub fn get_left<Q>(&self, l: &Q) -> Option<&BTreeSet<R>>
    where
        L: Borrow<Q> + Ord,
        Q: Ord + ?Sized,
    {
        self.left.get(l)
    }

    pub fn get_right<Q>(&self, r: &Q) -> Option<&L>
    where
        R: Borrow<Q> + Ord,
        Q: Ord + ?Sized,
    {
        self.right.get(r)
    }

    pub fn is_empty(&self) -> bool {
        self.right.is_empty()
    }
}

impl<L, R> Default for OneToMany<L, R> {
    fn default() -> Self {
        Self {
            left: BTreeMap::new(),
            right: BTreeMap::new(),
        }
    }
}

impl<L: Ord + Clone, R: Ord + Clone> FromIterator<(L, BTreeSet<R>)> for OneToMany<L, R> {
    fn from_iter<T: IntoIterator<Item = (L, BTreeSet<R>)>>(iter: T) -> Self {
        iter.into_iter().fold(Self::new(), |mut m, (l, rs)| {
            rs.iter().for_each(|r| {
                m.right.insert(r.clone(), l.clone());
            });
            m.left.insert(l, rs);
            m
        })
    }
}

impl<L: Serialize, R: Serialize> Serialize for OneToMany<L, R> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.left.serialize(serializer)
    }
}

impl<'de, L: Deserialize<'de> + Ord + Clone, R: Deserialize<'de> + Ord + Clone> Deserialize<'de>
    for OneToMany<L, R>
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let left = BTreeMap::<L, BTreeSet<R>>::deserialize(deserializer)?;
        Ok(Self {
            right: left
                .iter()
                .flat_map(|(l, rs)| rs.iter().map(|r| (r.clone(), l.clone())))
                .collect(),
            left,
        })
    }
}

#[cfg(feature = "schemars")]
impl<L: schemars::JsonSchema, R: schemars::JsonSchema> schemars::JsonSchema for OneToMany<L, R> {
    fn schema_name() -> String {
        BTreeMap::<L, BTreeSet<R>>::schema_name()
    }

    fn json_schema(gen: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
        BTreeMap::<L, BTreeSet<R>>::json_schema(gen)
    }
}
