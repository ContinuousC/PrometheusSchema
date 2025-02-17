/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::collections::{BTreeMap, BTreeSet};

use bimap::BiBTreeMap;
use graph::{BTreeGraph, Ref, RefBy};
use serde::{Deserialize, Serialize};

use prometheus_core::{LabelName, MetricName};

use crate::{
    digest::Digestible,
    ids::{ItemName, ItemRef, ModuleName, ModuleVersion, ModuleVersionReq, QualifiedItemName},
    query::MetricSelector,
    schema::{self, ResolveError, Universe},
    Sha256,
};

#[derive(Serialize, Deserialize, Clone)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi))]
pub struct Root {
    #[serde(default = "root_item")] //, skip_serializing_if = "is_root_item")]
    pub root: ItemRef,
    #[serde(default)] //, skip_serializing_if = "BTreeMap::is_empty")]
    pub requires: BTreeMap<ModuleName, ModuleVersionReq>,
    #[serde(default)] //, skip_serializing_if = "BTreeMap::is_empty")]
    pub items: BTreeMap<ItemName, Item>,
}

fn root_item() -> ItemRef {
    ItemRef {
        module: None,
        item: ItemName::new(String::from("root")),
    }
}

// fn is_root_item(item: &ItemRef) -> bool {
//     item.module
//         .as_ref()
//         .map_or(true, |name| name.0.as_str() == "root")
//         && item.item.0.as_str() == "root"
// }

#[derive(Serialize, Deserialize, Clone)]
#[cfg_attr(feature = "apistos", derive(apistos::ApiComponent))]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi))]
pub struct Module {
    // pub name: ModuleName,
    pub version: ModuleVersion,
    #[serde(default)] //, skip_serializing_if = "BTreeMap::is_empty")]
    pub requires: BTreeMap<ModuleName, ModuleVersionReq>,
    #[serde(default)] //, skip_serializing_if = "BTreeMap::is_empty")]
    pub items: BTreeMap<ItemName, Item>,
}

#[derive(Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord, Clone, Default)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
pub struct Item {
    #[serde(default)] //, skip_serializing_if = "MetricSelector::is_empty")]
    pub query: MetricSelector,
    #[serde(default)] //, skip_serializing_if = "MetricSelector::is_empty")]
    pub assert: MetricSelector,
    #[serde(default)] //, skip_serializing_if = "BTreeSet::is_empty")]
    pub keys: BTreeSet<LabelName>,
    #[serde(default)] //, skip_serializing_if = "BTreeSet::is_empty")]
    pub items: BTreeSet<ItemRef>,
    #[serde(default)] //, skip_serializing_if = "BTreeMap::is_empty")]
    pub metrics: BTreeMap<MetricName, Metric>,
    #[serde(default)] //, skip_serializing_if = "is_false")]
    pub fork: bool,
}

// const fn is_false(v: &bool) -> bool {
//     !*v
// }

// #[derive(Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord, Clone, Default)]
// #[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
// pub struct Metrics {
//     #[serde(default, skip_serializing_if = "MetricSelector::is_empty")]
//     pub query: MetricSelector,
//     #[serde(default, skip_serializing_if = "MetricSelector::is_empty")]
//     pub assert: MetricSelector,
//     #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
//     pub metrics: BTreeMap<MetricName, Metric>,
// }

#[derive(Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[serde(rename_all = "snake_case")]
pub enum Metric {
    Scalar(Scalar),
    Histogram(Histogram),
    Summary(Summary),
}

#[derive(Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord, Clone, Default, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[serde(rename_all = "snake_case")]
pub enum ScalarType {
    Counter,
    #[default]
    Gauge,
}

#[derive(Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord, Clone, Default)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
pub struct Scalar {
    #[serde(default)] //, skip_serializing_if = "MetricSelector::is_empty")]
    pub query: MetricSelector,
    #[serde(default)] //, skip_serializing_if = "MetricSelector::is_empty")]
    pub labels: MetricSelector,
    #[serde(default)] //, skip_serializing_if = "Option::is_none")]
    pub unit: Option<String>,
    #[serde(default)] //, skip_serializing_if = "Option::is_none")]
    pub r#type: Option<ScalarType>,
}

#[derive(Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord, Clone, Default)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
pub struct Histogram {
    #[serde(default)] //, skip_serializing_if = "MetricSelector::is_empty")]
    pub query: MetricSelector,
    #[serde(default)] //, skip_serializing_if = "MetricSelector::is_empty")]
    pub labels: MetricSelector,
    //#[serde(skip_serializing_if = "Option::is_none")]
    pub unit: Option<String>,
}

#[derive(Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord, Clone, Default)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
pub struct Summary {
    #[serde(default)] //, skip_serializing_if = "MetricSelector::is_empty")]
    pub query: MetricSelector,
    #[serde(default)] //, skip_serializing_if = "MetricSelector::is_empty")]
    pub labels: MetricSelector,
    //#[serde(skip_serializing_if = "Option::is_none")]
    pub unit: Option<String>,
}

impl Root {
    pub fn resolve(
        self,
        mut modules: BTreeMap<ModuleName, Module>,
    ) -> Result<Universe, ResolveError> {
        modules.insert(
            ModuleName(String::from("root")),
            Module {
                version: ModuleVersion(semver::Version::new(1, 0, 0)),
                requires: self.requires,
                items: self.items,
            },
        );

        let mut module_graph = BTreeGraph::new();
        let mut item_graph = BTreeGraph::new();

        let keys = modules
            .iter()
            .map(|(module_name, module)| {
                (
                    module_graph.promise(module_name.clone()),
                    module
                        .items
                        .keys()
                        .map(|item_name| {
                            item_graph
                                .promise(QualifiedItemName(module_name.clone(), item_name.clone()))
                        })
                        .collect::<Vec<_>>(),
                )
            })
            .collect::<Vec<_>>();

        keys.into_iter().zip(modules).try_for_each(
            |((module_key, item_keys), (module_name, module))| {
                let module =
                    module.resolve(item_keys, &module_name, &module_graph, &mut item_graph)?;
                module_graph.create(&module_key, module);
                Ok(())
            },
        )?;

        let parents = item_graph
            .iter_ref()
            .flat_map(|(name, parent)| {
                item_graph
                    .borrow(parent)
                    .items
                    .values()
                    .map(|child| (child.clone(), name.clone(), parent.clone()))
            })
            .collect::<Vec<_>>();

        parents.into_iter().for_each(|(child, name, parent)| {
            item_graph.borrow_mut(&child).parents.insert(name, parent);
        });

        let (root, root_ref) = self
            .root
            .resolve(&ModuleName(String::from("root")), &item_graph)?;

        let metrics = item_graph.iter_ref().fold(
            BTreeMap::<_, BTreeMap<_, _>>::new(),
            |mut metrics, (item_name, item_ref)| {
                let item = item_graph.borrow(item_ref);
                item.metrics.keys().for_each(|metric_name| {
                    metrics
                        .entry(metric_name.clone())
                        .or_default()
                        .insert(item_name.clone(), item_ref.clone());
                });
                metrics
            },
        );

        Ok(Universe {
            root: RefBy::new(root, root_ref),
            modules: module_graph,
            items: item_graph,
            metrics,
        })
    }
}

impl Module {
    fn resolve(
        self,
        item_keys: Vec<Ref<schema::Item>>,
        module_name: &ModuleName,
        modules: &BTreeGraph<ModuleName, schema::Module>,
        items: &mut BTreeGraph<QualifiedItemName, schema::Item>,
    ) -> Result<schema::Module, ResolveError> {
        Ok(schema::Module {
            version: self.version,
            requires: self
                .requires
                .into_iter()
                .map(|(req_name, req)| {
                    let req_module = modules
                        .get_ref(&req_name)
                        .ok_or_else(|| ResolveError::MissingModule(req_name.clone()))?
                        .clone();
                    Ok((req_name, (req_module, req)))
                })
                .collect::<Result<_, ResolveError>>()?,
            items: item_keys
                .into_iter()
                .zip(self.items)
                .map(|(item_key, (item_name, item))| {
                    items.create(&item_key, item.resolve(module_name, items)?);
                    Ok((item_name, item_key))
                })
                .collect::<Result<_, ResolveError>>()?,
        })
    }
}

impl Item {
    fn resolve(
        self,
        module_name: &ModuleName,
        items: &BTreeGraph<QualifiedItemName, schema::Item>,
    ) -> Result<schema::Item, ResolveError> {
        Ok(schema::Item {
            query: self.query,
            assert: self.assert,
            keys: self.keys,
            metrics: self
                .metrics
                .into_iter()
                .map(|(name, metric)| (name, metric.resolve()))
                .collect(),
            parents: BTreeMap::new(),
            items: self
                .items
                .into_iter()
                .map(|name| name.resolve(module_name, items))
                .collect::<Result<_, ResolveError>>()?,
            fork: self.fork,
        })
    }
}

impl Metric {
    fn resolve(self) -> schema::Metric {
        match self {
            Metric::Scalar(m) => schema::Metric {
                query: m.query,
                labels: m.labels,
                unit: m.unit,
                r#type: schema::MetricType::Scalar(schema::Scalar { r#type: m.r#type }),
            },
            Metric::Histogram(m) => schema::Metric {
                query: m.query,
                labels: m.labels,
                unit: m.unit,
                r#type: schema::MetricType::Histogram(schema::Histogram {}),
            },
            Metric::Summary(m) => schema::Metric {
                query: m.query,
                labels: m.labels,
                unit: m.unit,
                r#type: schema::MetricType::Summary(schema::Summary {}),
            },
        }
    }
}

impl Digestible for Item {
    type Data<'a> = (&'a BiBTreeMap<ItemName, Item>, Option<&'a ModuleName>);

    fn digest(&self, ctx: &mut ring::digest::Context, version: u64, data: Self::Data<'_>) {
        self.query.digest(ctx, version, ());
        self.assert.digest(ctx, version, ());
        self.keys.digest(ctx, version, ());

        self.items.len().digest(ctx, version, ());
        self.items
            .iter()
            .fold(Sha256::default(), |mut sum, item_ref| {
                if let Some(item) = item_ref
                    .module
                    .as_ref()
                    .map_or(true, |module| (data.1 == Some(module)))
                    .then_some(())
                    // Silently ignore missing items...
                    .and_then(|_| data.0.get_by_left(&item_ref.item))
                {
                    sum ^= item.digest_sha256(version, data);
                } else if let Some(module) = item_ref.module.as_ref() {
                    sum ^= item_ref.digest_sha256(version, module);
                }
                sum
            })
            .digest(ctx, version, ());

        self.metrics.digest(ctx, version, ((), ()));
        self.fork.digest(ctx, version, ());
    }
}

impl Digestible for Metric {
    type Data<'a> = ();

    fn digest(&self, ctx: &mut ring::digest::Context, version: u64, _data: Self::Data<'_>) {
        match self {
            Metric::Scalar(v) => {
                "scalar".digest(ctx, version, ());
                v.digest(ctx, version, ());
            }
            Metric::Histogram(v) => {
                "histogram".digest(ctx, version, ());
                v.digest(ctx, version, ());
            }
            Metric::Summary(v) => {
                "summary".digest(ctx, version, ());
                v.digest(ctx, version, ());
            }
        }
    }
}

impl Digestible for Scalar {
    type Data<'a> = ();

    fn digest(&self, ctx: &mut ring::digest::Context, version: u64, _data: Self::Data<'_>) {
        self.query.digest(ctx, version, ());
        self.labels.digest(ctx, version, ());
        self.unit.digest(ctx, version, ());
        self.r#type.digest(ctx, version, ());
    }
}

impl Digestible for ScalarType {
    type Data<'a> = ();

    fn digest(&self, ctx: &mut ring::digest::Context, version: u64, _data: Self::Data<'_>) {
        match self {
            ScalarType::Counter => {
                "counter".digest(ctx, version, ());
            }
            ScalarType::Gauge => {
                "gauge".digest(ctx, version, ());
            }
        }
    }
}

impl Digestible for Histogram {
    type Data<'a> = ();

    fn digest(&self, ctx: &mut ring::digest::Context, version: u64, _data: Self::Data<'_>) {
        self.query.digest(ctx, version, ());
        self.labels.digest(ctx, version, ());
        self.unit.digest(ctx, version, ());
    }
}

impl Digestible for Summary {
    type Data<'a> = ();

    fn digest(&self, ctx: &mut ring::digest::Context, version: u64, _data: Self::Data<'_>) {
        self.query.digest(ctx, version, ());
        self.labels.digest(ctx, version, ());
        self.unit.digest(ctx, version, ());
    }
}
