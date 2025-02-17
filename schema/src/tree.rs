/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::{
    collections::{btree_map::Entry, BTreeMap, BTreeSet},
    fmt::{Display, Write},
};

use itertools::Itertools;
use serde::{Deserialize, Serialize};

use prometheus_api::GenericMetric;
use prometheus_core::{LabelName, MetricName, METRIC_LABEL};

use crate::{
    path::Path,
    query::{LabelSelector, MetricSelector},
};

/// Tree-based primitive prometheus schema.

#[derive(Serialize, Deserialize, Clone)]
#[serde(rename_all = "snake_case")]
pub enum Tree {
    Branch(BTreeMap<String, Branch>),
    Fork(BTreeMap<String, Branch>),
    Leaf(Leaf),
}

#[derive(Serialize, Deserialize, Clone)]
pub struct Branch {
    pub query: MetricSelector,
    pub assert: MetricSelector,
    pub keys: BTreeSet<LabelName>,
    pub item: Option<String>,
    pub tree: Tree,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct Leaf {
    // pub item: String,
}

#[derive(Serialize, Deserialize, Clone, Default)]
pub struct ItemInfo {
    #[serde(default)]
    pub keys: BTreeSet<LabelName>,
    pub meta: BTreeSet<LabelName>,
    pub metrics: BTreeSet<MetricName>,
    pub children: BTreeSet<String>,
    pub parents: BTreeSet<String>,
    pub siblings: BTreeSet<String>,
    pub paths: BTreeSet<String>,
    pub queries: BTreeMap<String, BTreeMap<String, MetricSelector>>,
}

#[derive(thiserror::Error, Debug)]
pub enum SieveError {
    #[error("unmatched")]
    NoMatch,
    #[error("multiple matches")]
    MultipleMatches,
    #[error("assertion failed")]
    AssertionFailed,
}

#[derive(thiserror::Error, Debug)]
pub enum VerifyError {
    #[error("branches are not mutually exclusive: {1} conflicts with {2} at {0}")]
    NotMutuallyExclusive(String, String, String),
    #[error("branch query is not a subset of the parent query: {0}")]
    BranchQueryMoreGeneral(String),
    #[error("re-used key(s): {0}: {}", .1.iter().join(", "))]
    ReusedKeys(String, BTreeSet<LabelName>),
}

type TPath = String;
type Key<'a> = BTreeMap<&'a LabelName, &'a str>;
type Labels<'a> = BTreeSet<&'a LabelName>;

impl Tree {
    pub fn verify(&self) -> Result<(), Vec<VerifyError>> {
        let mut errors = Vec::new();
        self.verify_internal(
            Path::root(),
            MetricSelector::new(),
            BTreeSet::new(),
            &mut errors,
        );
        errors.is_empty().then_some(()).ok_or(errors)
    }

    fn verify_internal(
        &self,
        path: Path<'_>,
        query: MetricSelector,
        keys: BTreeSet<LabelName>,
        errors: &mut Vec<VerifyError>,
    ) {
        match self {
            Tree::Branch(branches) => {
                branches.iter().enumerate().for_each(|(i, (a, branch))| {
                    branches.iter().skip(i + 1).for_each(|(b, other)| {
                        if !branch.excludes(other) {
                            errors.push(VerifyError::NotMutuallyExclusive(
                                path.to_string(),
                                a.to_string(),
                                b.to_string(),
                            ));
                        }
                    })
                });
                branches.iter().for_each(|(name, branch)| {
                    let branch_path = path.step(name.as_str());
                    let branch_query = query
                        .iter()
                        .chain(&branch.query)
                        .map(|(k, v)| (k.clone(), v.clone()))
                        .collect::<MetricSelector>();

                    // if !branch_query.is_subset(&query) {
                    //     errors.push(VerifyError::BranchQueryMoreGeneral(branch_path.to_string()));
                    // }

                    if !branch.keys.is_disjoint(&keys) {
                        errors.push(VerifyError::ReusedKeys(
                            branch_path.to_string(),
                            &branch.keys & &keys,
                        ));
                    }

                    branch.tree.verify_internal(
                        branch_path,
                        branch_query,
                        &keys | &branch.keys,
                        errors,
                    );
                })
            }
            Tree::Fork(_) => { /* TODO */ }
            Tree::Leaf(_) => {}
        }
    }

    pub fn sieve<T>(
        &self,
        metrics: &[GenericMetric<T>],
    ) -> (Vec<String>, usize, BTreeSet<LabelName>) {
        let mut errors = Vec::new();
        let metrics = metrics.iter().map(|m| &m.metric).collect::<Vec<_>>();

        let mut unmatched = 0;
        let mut unmatched_labels: BTreeSet<LabelName> = BTreeSet::new();
        let mut matches: BTreeMap<_, BTreeMap<_, u16>> = BTreeMap::new();

        metrics.into_iter().for_each(|metric| {
            match self.sieve_metric(metric, Path::root(), BTreeMap::new(), BTreeSet::new()) {
                Ok((path, key, labels)) => {
                    let unmatched_metric_labels = metric
                        .keys()
                        .filter(|label| !labels.contains(label.as_str()))
                        .cloned()
                        .collect::<Vec<_>>();
                    if unmatched_metric_labels.is_empty() {
                        // println!(
                        //     "matched: {}",
                        //     ShowMetric(metric, Some(&labels))
                        // );
                    } else {
                        errors.push(format!(
                            "unmatched labels: {} @ {path}",
                            ShowMetric(metric, Some(&labels)),
                        ));
                        unmatched_labels.extend(unmatched_metric_labels)
                    }
                    *matches.entry(path).or_default().entry(key).or_default() += 1;
                }
                Err((path, labels, e)) => {
                    errors.push(format!(
                        "{e}: {} @ {path}",
                        ShowMetric(metric, Some(&labels))
                    ));
                    unmatched += 1;
                }
            }
        });

        for (path, keys) in matches {
            for (key, n) in keys {
                if n > 1 {
                    errors.push(format!(
                        "Duplicate key @ {path}: {} ({n} matches)",
                        ShowKey(&key)
                    ));
                }
            }
        }

        (errors, unmatched, unmatched_labels)
    }

    fn sieve_metric<'a>(
        &'a self,
        metric: &'a BTreeMap<LabelName, String>,
        path: Path<'_>,
        mut key: BTreeMap<&'a LabelName, &'a str>,
        mut matched: BTreeSet<&'a LabelName>,
    ) -> Result<(TPath, Key<'a>, Labels<'a>), (TPath, Labels<'a>, SieveError)> {
        match self {
            Tree::Branch(branch) => {
                let matches = branch
                    .iter()
                    .filter(|(_, branch)| branch.matches(metric))
                    .collect::<Vec<_>>();
                match matches.len() {
                    0 => Err((path.to_string(), matched, SieveError::NoMatch)),
                    1 => {
                        let (name, branch) = matches.first().unwrap();
                        let path = path.step(name.as_str());
                        if branch.assert.matches(metric) {
                            key.extend(branch.keys.iter().map(|label| {
                                (label, metric.get(label).map(|s| s.as_str()).unwrap_or(""))
                            }));
                            matched.extend(branch.query.keys().chain(branch.assert.keys()));
                            branch.tree.sieve_metric(metric, path, key, matched)
                        } else {
                            Err((path.to_string(), matched, SieveError::AssertionFailed))
                        }
                    }
                    _ => Err((path.to_string(), matched, SieveError::MultipleMatches)),
                }
            }
            Tree::Fork(_) => todo!(),
            Tree::Leaf(_) => Ok((path.step("leaf").to_string(), key, matched)),
        }
    }

    pub fn items(&self) -> BTreeMap<String, ItemInfo> {
        let mut info = BTreeMap::new();
        self.get_items(Path::root(), None, &BTreeMap::new(), &mut info);
        info
    }

    fn get_items(
        &self,
        path: Path<'_>,
        last_item: Option<&str>,
        query: &BTreeMap<String, MetricSelector>,
        info: &mut BTreeMap<String, ItemInfo>,
    ) {
        match self {
            Tree::Branch(branches) | Tree::Fork(branches) => {
                branches.iter().for_each(|(name, branch)| {
                    branch.get_items(path.step(name), last_item, query, info)
                })
            }
            Tree::Leaf(_) => {}
        }
    }
}

impl Branch {
    fn matches(&self, metric: &BTreeMap<LabelName, String>) -> bool {
        self.query.matches(metric)
    }

    fn excludes(&self, other: &Self) -> bool {
        self.query.excludes(&other.query)
    }

    fn get_items(
        &self,
        path: Path<'_>,
        last_item: Option<&str>,
        query: &BTreeMap<String, MetricSelector>,
        info: &mut BTreeMap<String, ItemInfo>,
    ) {
        let mut query = query.clone();

        if let Some(name) = &self.item {
            query.insert(name.to_string(), self.query.clone());

            let item = info.entry(name.clone()).or_default();
            item.paths.insert(path.to_string());
            item.queries.insert(path.to_string(), query.clone());
            item.keys.extend(
                self.query
                    .keys()
                    .filter(|name| *name == &METRIC_LABEL)
                    .cloned(),
            );

            if let Some(rel_name) = last_item {
                if self.query.is_many() {
                    item.parents.insert(rel_name.to_owned());
                    let parent = info.get_mut(rel_name).unwrap();
                    parent.children.insert(name.clone());
                } else {
                    item.siblings.insert(rel_name.to_owned());
                    let sibling = info.get_mut(rel_name).unwrap();
                    sibling.siblings.insert(name.clone());
                }
            }
        }

        let last_item = self.item.as_deref().or(last_item);

        if let Tree::Leaf(_) = &self.tree {
            if let Some(name) = last_item {
                let item = info.get_mut(name).unwrap();
                item.meta.extend(
                    self.query
                        .keys()
                        .filter(|name| *name != &METRIC_LABEL)
                        .cloned(),
                );
                // item.queries.insert(path.to_string(), query.clone());
                match self.query.get(&METRIC_LABEL) {
                    Some(LabelSelector::Eq(value)) => {
                        item.metrics.insert(MetricName::new(value.clone()).unwrap());
                    }
                    Some(LabelSelector::In(values)) => {
                        // TODO: return error on invalid metric name?
                        item.metrics
                            .extend(values.iter().cloned().filter_map(|name| name.parse().ok()));
                    }
                    _ => {}
                }
            }
        }

        self.tree.get_items(path, last_item, &query, info);
    }
}

struct ShowMetric<'a>(
    &'a BTreeMap<LabelName, String>,
    Option<&'a BTreeSet<&'a LabelName>>,
);

impl Display for ShowMetric<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(name) = self.0.get(&METRIC_LABEL) {
            write!(f, "{} ", name)?;
        }
        let mut labels = self
            .0
            .iter()
            .filter(|(name, _)| *name != &METRIC_LABEL)
            .filter(|(name, _)| self.1.map_or(true, |matched| !matched.contains(*name)));
        if let Some((name, value)) = labels.next() {
            write!(f, "{{ {}=\"{}\"", name, value)?;
            for (name, value) in labels {
                write!(f, ", {}=\"{}\"", name, value)?;
            }
            write!(f, " }}")?;
        }
        Ok(())
    }
}

struct ShowKey<'a>(&'a BTreeMap<&'a LabelName, &'a str>);

impl Display for ShowKey<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut iter = self.0.iter();
        iter.next()
            .map(|(label, value)| write!(f, "{label}={value}"))
            .unwrap_or(Ok(()))?;
        iter.try_for_each(|(label, value)| write!(f, ", {label}={value}"))
    }
}

pub fn make_query(labels: &BTreeMap<String, MetricSelector>) -> String {
    let labels =
        labels
            .values()
            .flatten()
            .fold(BTreeMap::new(), |mut labels, (label, selector)| {
                match labels.entry(label) {
                    Entry::Vacant(ent) => {
                        ent.insert(selector.clone());
                    }
                    Entry::Occupied(mut ent) => {
                        ent.insert(ent.get().and(selector));
                    }
                }
                labels
            });

    let mut query = String::new();
    write!(query, "{{").unwrap();
    labels.iter().enumerate().for_each(|(i, (name, selector))| {
        // TODO: escape values!
        if i > 0 {
            write!(query, ",").unwrap();
        }
        write!(query, " {name}").unwrap();
        match selector {
            LabelSelector::Opt => write!(query, "=\".*\"").unwrap(),
            LabelSelector::Set => write!(query, "!=\"\"").unwrap(),
            LabelSelector::Unset => write!(query, "=\"\"").unwrap(),
            LabelSelector::Eq(value) => write!(query, "=\"{value}\"").unwrap(),
            LabelSelector::Ne(value) => write!(query, "!=\"{value}\"").unwrap(),
            LabelSelector::In(values) => {
                write!(query, "=~\"").unwrap();
                values.iter().enumerate().for_each(|(i, value)| {
                    if i > 0 {
                        write!(query, "|").unwrap();
                    }
                    write!(query, "{value}").unwrap();
                });
                write!(query, "\"").unwrap();
            }
            LabelSelector::NotIn(values) => {
                write!(query, "!~\"").unwrap();
                values.iter().enumerate().for_each(|(i, value)| {
                    if i > 0 {
                        write!(query, "|").unwrap();
                    }
                    write!(query, "{value}").unwrap();
                });
                write!(query, "\"").unwrap();
            }
        }
    });
    write!(query, " }}").unwrap();
    query
}
