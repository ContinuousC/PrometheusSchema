/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::{
    collections::{BTreeMap, BTreeSet},
    path::{Path, PathBuf},
};

use async_recursion::async_recursion;
use futures::StreamExt;
use serde::{Deserialize, Serialize};

use crate::{query::MetricSelector, schema};

use super::path;
use super::query::LabelSelector;
use super::tree::{self, Leaf};

/// Opinionated prometheus schema (in progress).

#[derive(Serialize, Deserialize, Clone)]
pub struct Module<T = Loaded> {
    pub name: String,
    pub version: String,
    #[serde(default = "BTreeMap::new", skip_serializing_if = "BTreeMap::is_empty")]
    pub interfaces: BTreeMap<String, Interface<T>>,
    #[serde(default = "BTreeMap::new", skip_serializing_if = "BTreeMap::is_empty")]
    pub items: BTreeMap<String, T>,
}

#[derive(Serialize, Deserialize, Clone)]
#[serde(rename_all = "snake_case")]
enum Loadable {
    Load(Load),
    Item(Item<Loadable>),
}

#[derive(Serialize, Deserialize, Clone)]
#[serde(rename_all = "snake_case")]
pub struct Loaded(pub Item<Loaded>);

#[derive(Serialize, Deserialize, Clone)]
pub struct Load {
    file: PathBuf,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct Interface<T = Loaded> {
    #[serde(default, skip_serializing_if = "BTreeSet::is_empty")]
    pub implements: BTreeSet<String>,
    #[serde(default, skip_serializing_if = "MetricSelector::is_empty")]
    pub assert: MetricSelector,
    #[serde(default = "BTreeMap::new", skip_serializing_if = "BTreeMap::is_empty")]
    pub items: BTreeMap<String, T>,
    #[serde(default = "BTreeMap::new", skip_serializing_if = "BTreeMap::is_empty")]
    pub metrics: BTreeMap<String, Metric>,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct Item<T = Loaded> {
    #[serde(default)]
    pub implements: BTreeSet<String>,
    #[serde(default)]
    pub query: MetricSelector,
    #[serde(default)]
    pub assert: MetricSelector,
    #[serde(default)]
    pub keys: BTreeSet<String>,
    #[serde(default = "BTreeMap::new")]
    pub items: BTreeMap<String, T>,
    #[serde(default)]
    pub metrics: BTreeMap<String, Metric>,
}

#[derive(Serialize, Deserialize, Clone)]
#[serde(rename_all = "snake_case")]
pub enum Metric {
    Scalar(Scalar),
    Histogram(Histogram),
    Summary(Summary),
}

#[derive(Serialize, Deserialize, Clone)]
pub struct Scalar {
    #[serde(default, skip_serializing_if = "MetricSelector::is_empty")]
    pub labels: MetricSelector,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct Histogram {
    #[serde(default, skip_serializing_if = "MetricSelector::is_empty")]
    pub labels: MetricSelector,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct Summary {
    #[serde(default, skip_serializing_if = "MetricSelector::is_empty")]
    pub labels: MetricSelector,
}

pub enum Branch<'a> {
    Item(&'a Item),
    Metric(&'a Metric),
}

/* Load functions: convert schema spanning multiple files into one
 * fully loaded schema. */

impl Module<Loadable> {
    async fn load(self, base: &Path) -> Module<Loaded> {
        Module {
            name: self.name,
            version: self.version,
            interfaces: futures::stream::iter(self.interfaces)
                .then(|(name, interface)| async { (name, interface.load(base).await) })
                .collect()
                .await,
            items: futures::stream::iter(self.items)
                .then(|(name, item)| async { (name, item.load(base).await) })
                .collect()
                .await,
        }
    }
}

impl Loadable {
    #[async_recursion]
    async fn load(self, base: &Path) -> Loaded {
        match self {
            Loadable::Load(load) => Loaded(load.load(base).await),
            Loadable::Item(item) => Loaded(item.load(base).await),
        }
    }
}

impl Item<Loadable> {
    async fn load(self, base: &Path) -> Item<Loaded> {
        Item {
            implements: self.implements,
            query: self.query,
            assert: self.assert,
            keys: self.keys,
            items: futures::stream::iter(self.items)
                .then(|(name, branch)| async { (name, branch.load(base).await) })
                .collect()
                .await,
            metrics: self.metrics,
        }
    }
}

impl Interface<Loadable> {
    async fn load(self, base: &Path) -> Interface<Loaded> {
        Interface {
            implements: self.implements,
            assert: self.assert,
            items: futures::stream::iter(self.items)
                .then(|(name, branch)| async { (name, branch.load(base).await) })
                .collect()
                .await,
            metrics: self.metrics,
        }
    }
}

impl Load {
    async fn load(self, base: &Path) -> Item {
        let data = tokio::fs::read(base.join(&self.file)).await.unwrap();
        serde_yaml::from_slice(&data).unwrap()
    }
}

/* Conversion to tree schema. */

impl Module {
    pub async fn load(path: &Path) -> Self {
        let data = tokio::fs::read(path).await.unwrap();
        serde_yaml::from_slice::<Module<Loadable>>(&data)
            .unwrap()
            .load(path.parent().unwrap())
            .await
    }

    pub fn gather<'a, F, S>(&'a self, path: &[&str], init: S, mut f: F) -> Option<S>
    where
        F: FnMut(S, &Branch<'a>) -> S,
    {
        let mut path = path.iter();
        let branch = self.walk(path.next()?)?;
        let state = f(init, &branch);
        let res = path.try_fold((state, branch), |(state, branch), name| {
            let branch = branch.walk(self, name)?;
            let state = f(state, &branch);
            Some((state, branch))
        })?;
        Some(res.0)
    }

    pub fn fold_items<'a, F, S>(&'a self, init: S, mut f: F) -> S
    where
        F: for<'c> FnMut(S, &'c path::Path<'c>, &'a Item) -> S,
    {
        let path = path::Path::root();
        self.items.iter().fold(init, |state, (name, item)| {
            let path = path.step(name);
            let state = f(state, &path, &item.0);
            item.0.fold_items(state, &path, &mut f)
        })
    }

    pub fn walk(&self, name: &str) -> Option<Branch> {
        self.items.get(name).map(|item| Branch::Item(&item.0))
    }

    pub fn to_primitive(&self) -> tree::Tree {
        tree::Tree::Branch(
            self.items
                .iter()
                .map(|(name, item)| (name.clone(), item.to_primitive(self, name)))
                .collect(),
        )
    }

    pub fn to_schema2(&self) -> schema::ModuleSerial {
        let mut items = BTreeMap::new();
        let root = schema::ItemSerial {
            query: MetricSelector::default(),
            assert: MetricSelector::default(),
            keys: BTreeSet::new(),
            implements: BTreeSet::new(),
            items: self
                .interfaces
                .iter()
                .map(|(name, interface)| interface.to_schema2(name, &mut items))
                .collect::<BTreeSet<_>>()
                .into_iter()
                .chain(
                    self.items
                        .iter()
                        .map(|(name, item)| item.0.to_schema2(name, &mut items)),
                )
                .map(|name| schema::ItemRef::new(None, name))
                .collect(),
            metrics: BTreeMap::new(),
        };

        items.insert(schema::ItemName::new("root".to_string()), root);
        schema::ModuleSerial {
            version: schema::ModuleVersion::new(self.version.to_string()),
            requires: BTreeMap::new(),
            items,
        }
    }

    fn interfaces(&self, name: &str) -> Box<dyn Iterator<Item = &Interface> + '_> {
        let interface = self.interfaces.get(name).unwrap();
        Box::new(
            std::iter::once(interface).chain(
                interface
                    .implements
                    .iter()
                    .flat_map(|name| self.interfaces(name)),
            ),
        )
    }
}

impl Loaded {
    fn to_primitive(&self, module: &Module<Loaded>, name: &str) -> tree::Branch {
        self.0.to_primitive(module, name)
    }
}

impl Interface {
    fn walk<'a>(&'a self, module: &'a Module, name: &str) -> Option<Branch<'a>> {
        self.items
            .get(name)
            .map(|item| Branch::Item(&item.0))
            .or_else(|| self.metrics.get(name).map(Branch::Metric))
            .or_else(|| {
                self.implements.iter().find_map(|interface| {
                    module.interfaces.get(interface).unwrap().walk(module, name)
                })
            })
    }

    fn branches(&self) -> impl Iterator<Item = (&str, Branch<'_>)> {
        self.items
            .iter()
            .map(|(name, item)| (name.as_str(), Branch::Item(&item.0)))
            .chain(
                self.metrics
                    .iter()
                    .map(|(name, metric)| (name.as_str(), Branch::Metric(metric))),
            )
    }

    fn labels(&self) -> impl Iterator<Item = (&str, &LabelSelector)> {
        self.assert
            .iter()
            .map(|(name, selector)| (name.as_str(), selector))
    }

    fn to_schema2(
        &self,
        name: &str,
        items: &mut BTreeMap<schema::ItemName, schema::ItemSerial>,
    ) -> schema::ItemName {
        let item = schema::ItemSerial {
            query: MetricSelector::default(),
            assert: self.assert.clone(),
            keys: BTreeSet::new(),
            implements: self
                .implements
                .iter()
                .map(|name| schema::ItemRef::new(None, schema::ItemName::new(name.to_string())))
                .collect(),
            items: self
                .items
                .iter()
                .map(|(name, item)| schema::ItemRef::new(None, item.0.to_schema2(name, items)))
                .collect(),
            metrics: self
                .metrics
                .iter()
                .map(|(name, metric)| (name.to_string(), metric.clone()))
                .collect(),
        };

        let unique_name = std::iter::once(schema::ItemName::new(name.to_string()))
            .chain((1..).map(|i| schema::ItemName::new(format!("{name}{i}"))))
            .find(|name| !items.contains_key(name))
            .unwrap();
        items.insert(unique_name.clone(), item);
        unique_name
    }
}

impl Item {
    pub fn walk<'a>(&'a self, module: &'a Module, name: &str) -> Option<Branch<'a>> {
        self.items
            .get(name)
            .map(|item| Branch::Item(&item.0))
            .or_else(|| self.metrics.get(name).map(Branch::Metric))
            .or_else(|| {
                self.implements.iter().find_map(|interface| {
                    module.interfaces.get(interface).unwrap().walk(module, name)
                })
            })
    }

    pub fn fold_items<'a, 'b, F, S>(&'a self, init: S, path: &'b path::Path<'b>, f: &mut F) -> S
    where
        F: for<'c> FnMut(S, &'c path::Path<'c>, &'a Item) -> S,
    {
        self.items.iter().fold(init, |state, (name, item)| {
            let path = path.step(name);
            let state = f(state, &path, &item.0);
            item.0.fold_items(state, &path, f)
        })
    }

    pub fn interfaces<'a>(&'a self, module: &'a Module) -> impl Iterator<Item = &'a Interface> {
        self.implements
            .iter()
            .flat_map(|interface| module.interfaces(interface))
    }

    pub fn branches(&self) -> impl Iterator<Item = (&str, Branch)> {
        self.items
            .iter()
            .map(|(name, item)| (name.as_str(), Branch::Item(&item.0)))
            .chain(
                self.metrics
                    .iter()
                    .map(|(name, metric)| (name.as_str(), Branch::Metric(metric))),
            )
    }

    pub fn labels(&self) -> impl Iterator<Item = (&str, &LabelSelector)> {
        self.query
            .iter()
            .chain(&self.assert)
            .map(|(name, selector)| (name.as_str(), selector))
    }

    pub fn all_labels<'a>(
        &'a self,
        module: &'a Module,
    ) -> impl Iterator<Item = (&str, &LabelSelector)> {
        self.labels().chain(
            self.interfaces(module)
                .flat_map(|interface| interface.labels()),
        )
    }

    pub fn all_branches<'a>(
        &'a self,
        module: &'a Module,
    ) -> impl Iterator<Item = (&'a str, Branch<'a>)> {
        self.branches().chain(
            self.interfaces(module)
                .flat_map(|interface| interface.branches()),
        )
    }

    pub fn names(&self, module: &Module) -> BTreeSet<String> {
        self.all_branches(module)
            .flat_map(|(name, branch)| branch.names(module, name))
            .collect()
    }

    pub fn to_primitive(&self, module: &Module<Loaded>, name: &str) -> tree::Branch {
        tree::Branch {
            labels: self
                .all_labels(module)
                .map(|(label, selector)| (label.to_string(), selector.clone()))
                .chain([(
                    String::from("__name__"),
                    LabelSelector::In(self.names(module)),
                )])
                .collect(),
            item: Some(name.to_string()),
            tree: tree::Tree::Branch(
                self.all_branches(module)
                    .map(|(name, branch)| (name.to_string(), branch.to_primitive(module, name)))
                    .collect(),
            ),
        }
    }

    fn to_schema2(
        &self,
        name: &str,
        items: &mut BTreeMap<schema::ItemName, schema::ItemSerial>,
    ) -> schema::ItemName {
        let item = schema::ItemSerial {
            query: self.query.clone(),
            assert: self.assert.clone(),
            keys: self.keys.clone(),
            implements: self
                .implements
                .iter()
                .map(|name| schema::ItemRef::new(None, schema::ItemName::new(name.to_string())))
                .collect(),
            items: self
                .items
                .iter()
                .map(|(name, item)| schema::ItemRef::new(None, item.0.to_schema2(name, items)))
                .collect(),
            metrics: self
                .metrics
                .iter()
                .map(|(name, metric)| (name.to_string(), metric.clone()))
                .collect(),
        };

        let unique_name = std::iter::once(schema::ItemName::new(name.to_string()))
            .chain((1..).map(|i| schema::ItemName::new(format!("{name}{i}"))))
            .find(|name| !items.contains_key(name))
            .unwrap();
        items.insert(unique_name.clone(), item);
        unique_name
    }
}

impl<'a> Branch<'a> {
    pub fn walk(&self, module: &'a Module, name: &str) -> Option<Branch<'a>> {
        match self {
            Branch::Item(item) => item.walk(module, name),
            Branch::Metric(_) => None,
        }
    }

    pub fn names(&self, module: &Module, name: &str) -> BTreeSet<String> {
        match self {
            Branch::Item(item) => item.names(module),
            Branch::Metric(metric) => metric.names(name),
        }
    }

    pub fn to_primitive(&self, module: &Module, name: &str) -> tree::Branch {
        match self {
            Branch::Item(item) => item.to_primitive(module, name),
            Branch::Metric(metric) => metric.to_primitive(name),
        }
    }
}

impl Metric {
    pub fn labels(&self) -> impl Iterator<Item = (&str, &LabelSelector)> {
        let labels = match self {
            Metric::Scalar(metric) => &metric.labels,
            Metric::Histogram(metric) => &metric.labels,
            Metric::Summary(metric) => &metric.labels,
        };
        labels
            .iter()
            .map(|(name, selector)| (name.as_str(), selector))
    }

    pub fn names(&self, name: &str) -> BTreeSet<String> {
        match self {
            Metric::Scalar(metric) => metric.names(name),
            Metric::Histogram(metric) => metric.names(name),
            Metric::Summary(metric) => metric.names(name),
        }
    }

    pub(crate) fn to_primitive(&self, name: &str) -> tree::Branch {
        match self {
            Metric::Scalar(metric) => metric.to_primitive(name),
            Metric::Histogram(metric) => metric.to_primitive(name),
            Metric::Summary(metric) => metric.to_primitive(name),
        }
    }
}

impl Scalar {
    #[allow(unused)]
    pub fn labels(&self) -> impl Iterator<Item = (&str, &LabelSelector)> {
        self.labels
            .iter()
            .map(|(name, selector)| (name.as_str(), selector))
    }

    fn names(&self, name: &str) -> BTreeSet<String> {
        BTreeSet::from_iter([name.to_owned()])
    }

    fn to_primitive(&self, name: &str) -> tree::Branch {
        tree::Branch {
            labels: self
                .labels
                .clone()
                .into_iter()
                .chain([(
                    String::from("__name__"),
                    LabelSelector::In(self.names(name)),
                )])
                .collect(),
            item: None,
            tree: tree::Tree::Leaf(Leaf {}),
        }
    }
}

impl Histogram {
    #[allow(unused)]
    pub fn labels(&self) -> impl Iterator<Item = (&str, &LabelSelector)> {
        self.labels
            .iter()
            .map(|(name, selector)| (name.as_str(), selector))
    }

    fn names(&self, name: &str) -> BTreeSet<String> {
        BTreeSet::from_iter([
            format!("{name}_count"),
            format!("{name}_sum"),
            format!("{name}_bucket"),
        ])
    }

    pub fn to_primitive(&self, name: &str) -> tree::Branch {
        tree::Branch {
            labels: self
                .labels
                .clone()
                .into_iter()
                .chain([(
                    String::from("__name__"),
                    LabelSelector::In(self.names(name)),
                )])
                .collect(),
            item: None,
            tree: tree::Tree::Branch(BTreeMap::from_iter([
                (
                    String::from("stats"),
                    tree::Branch {
                        labels: MetricSelector::from_iter([(
                            String::from("__name__"),
                            LabelSelector::In(BTreeSet::from_iter([
                                format!("{name}_count"),
                                format!("{name}_sum"),
                            ])),
                        )]),
                        item: None,
                        tree: tree::Tree::Leaf(Leaf {}),
                    },
                ),
                (
                    String::from("buckets"),
                    tree::Branch {
                        labels: MetricSelector::from_iter([
                            (
                                String::from("__name__"),
                                LabelSelector::Eq(format!("{name}_bucket")),
                            ),
                            (String::from("le"), LabelSelector::Set),
                        ]),
                        item: Some(format!("{name}_bucket")),
                        tree: tree::Tree::Leaf(Leaf {}),
                    },
                ),
            ])),
        }
    }
}

impl Summary {
    #[allow(unused)]
    pub fn labels(&self) -> impl Iterator<Item = (&str, &LabelSelector)> {
        self.labels
            .iter()
            .map(|(name, selector)| (name.as_str(), selector))
    }

    fn names(&self, name: &str) -> BTreeSet<String> {
        BTreeSet::from_iter([
            name.to_owned(),
            format!("{name}_count"),
            format!("{name}_sum"),
        ])
    }

    pub fn to_primitive(&self, name: &str) -> tree::Branch {
        tree::Branch {
            labels: self
                .labels
                .clone()
                .into_iter()
                .chain([(
                    String::from("__name__"),
                    LabelSelector::In(self.names(name)),
                )])
                .collect(),
            item: None,
            tree: tree::Tree::Branch(BTreeMap::from_iter([
                (
                    String::from("stats"),
                    tree::Branch {
                        labels: MetricSelector::from_iter([(
                            String::from("__name__"),
                            LabelSelector::In(BTreeSet::from_iter([
                                format!("{name}_count"),
                                format!("{name}_sum"),
                            ])),
                        )]),
                        item: None,
                        tree: tree::Tree::Leaf(Leaf {}),
                    },
                ),
                (
                    String::from("quantiles"),
                    tree::Branch {
                        labels: MetricSelector::from_iter([
                            (String::from("__name__"), LabelSelector::Eq(name.to_owned())),
                            (String::from("quantile"), LabelSelector::Set),
                        ]),
                        item: Some(format!("{name}_quantile")),
                        tree: tree::Tree::Leaf(Leaf {}),
                    },
                ),
            ])),
        }
    }
}
