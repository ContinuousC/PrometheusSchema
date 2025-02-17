/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::collections::{BTreeMap, BTreeSet};
use std::fmt::{Display, Write};
use std::path::PathBuf;
use std::str::FromStr;

#[cfg(all(not(target_family = "wasm"), feature = "tokio"))]
use async_recursion::async_recursion;
use graph::{BTreeGraph, Ref, RefBy};

use prometheus_core::{LabelName, MetricName, LE_LABEL, METRIC_LABEL, QUANTILE_LABEL};

use crate::ids::{ItemName, ModuleName, ModuleVersion, ModuleVersionReq, QualifiedItemName};
use crate::query::{LabelSelector, MetricSelector};
use crate::serial::{self, ScalarType};
use crate::tree::{self, Leaf, Tree};
use crate::{Digestible, Sha256};

/* In-memory representation. */

pub struct Universe {
    pub(crate) modules: BTreeGraph<ModuleName, Module>,
    pub(crate) items: BTreeGraph<QualifiedItemName, Item>,
    pub(crate) metrics: BTreeMap<MetricName, BTreeMap<QualifiedItemName, Ref<Item>>>,
    pub(crate) root: RefBy<QualifiedItemName, Item>,
}

#[derive(Clone, Debug)]
pub struct Module {
    pub version: ModuleVersion,
    pub requires: BTreeMap<ModuleName, (Ref<Module>, ModuleVersionReq)>,
    pub items: BTreeMap<ItemName, Ref<Item>>,
}

#[derive(Clone, Debug)]
pub struct Item {
    pub query: MetricSelector,
    pub assert: MetricSelector,
    pub keys: BTreeSet<LabelName>,
    pub parents: BTreeMap<QualifiedItemName, Ref<Item>>,
    pub items: BTreeMap<QualifiedItemName, Ref<Item>>,
    pub metrics: BTreeMap<MetricName, Metric>,
    pub fork: bool,
}

#[derive(Clone, Debug)]
pub struct Metric {
    pub query: MetricSelector,
    pub labels: MetricSelector,
    pub unit: Option<String>,
    pub r#type: MetricType,
}

#[derive(Clone, Debug)]
pub enum MetricType {
    Scalar(Scalar),
    Histogram(Histogram),
    Summary(Summary),
}

#[derive(Clone, Debug)]
pub struct Scalar {
    pub r#type: Option<ScalarType>,
}

#[derive(Clone, Debug)]
pub struct Histogram {}

#[derive(Clone, Debug)]
pub struct Summary {}

#[derive(Clone, Copy)]
pub enum Branch<'a> {
    Item(&'a QualifiedItemName, &'a Item),
    Metric(&'a MetricName, &'a Metric),
}

#[derive(Clone)]
pub enum Path<'a, Step = PathStep<'a>> {
    Root,
    Step(Step, &'a Self),
}

pub struct Reversed<T>(T);

#[derive(PartialEq, Eq, Clone)]
pub enum PathStep<'a> {
    Item(&'a QualifiedItemName),
    Metric(&'a str),
}

pub type WalkResult<T> = Result<T, WalkError>;

#[derive(thiserror::Error, Debug)]
pub enum WalkError {
    #[error("element not found: {0}")]
    ElemNotFound(String),
}

impl Universe {
    #[cfg(all(not(target_family = "wasm"), feature = "tokio"))]
    pub async fn load_async(path: &std::path::Path) -> Result<Self, LoadSchemaError> {
        let (root, modules) = Self::load_sources_async(path).await?;
        Self::load_data(root, modules)
    }

    #[cfg(not(target_family = "wasm"))]
    pub fn load_sync(path: &std::path::Path) -> Result<Self, LoadSchemaError> {
        let (root, modules) = Self::load_sources_sync(path)?;
        Self::load_data(root, modules)
    }

    pub fn load_data(
        root: serial::Root,
        modules: BTreeMap<ModuleName, serial::Module>,
    ) -> Result<Self, LoadSchemaError> {
        root.resolve(modules).map_err(LoadSchemaError::Resolve)
    }

    #[cfg(all(not(target_family = "wasm"), feature = "tokio"))]
    pub async fn load_sources_async(
        path: &std::path::Path,
    ) -> Result<(serial::Root, BTreeMap<ModuleName, serial::Module>), LoadSchemaError> {
        let root = serde_yaml::from_slice::<serial::Root>(
            &tokio::fs::read(&path)
                .await
                .map_err(|e| LoadSchemaError::ReadFile(path.to_path_buf(), e))?,
        )
        .map_err(|e| LoadSchemaError::DeserializeFile(path.to_path_buf(), e))?;

        let mut modules = BTreeMap::new();
        for (mod_name, mod_version_req) in &root.requires {
            Self::load_module(
                mod_name,
                mod_version_req,
                path.parent().unwrap_or(std::path::Path::new(".")),
                &mut modules,
            )
            .await?;
        }
        Ok((root, modules))
    }

    #[cfg(not(target_family = "wasm"))]
    pub fn load_sources_sync(
        path: &std::path::Path,
    ) -> Result<(serial::Root, BTreeMap<ModuleName, serial::Module>), LoadSchemaError> {
        let root = serde_yaml::from_slice::<serial::Root>(
            &std::fs::read(path).map_err(|e| LoadSchemaError::ReadFile(path.to_path_buf(), e))?,
        )
        .map_err(|e| LoadSchemaError::DeserializeFile(path.to_path_buf(), e))?;

        let mut modules = BTreeMap::new();
        for (mod_name, mod_version_req) in &root.requires {
            Self::load_module_sync(
                mod_name,
                mod_version_req,
                path.parent().unwrap_or(std::path::Path::new(".")),
                &mut modules,
            )?;
        }
        Ok((root, modules))
    }

    pub fn mock_load(name: &str, files: &BTreeMap<&str, &str>) -> Result<Self, LoadSchemaError> {
        let path = format!("{name}.root.yaml");
        let root =
            serde_yaml::from_str::<serial::Root>(files.get(&path.as_str()).ok_or_else(|| {
                LoadSchemaError::MockFileNotFound(std::path::Path::new(&path).to_path_buf())
            })?)
            .map_err(|e| {
                LoadSchemaError::DeserializeFile(std::path::Path::new(&path).to_path_buf(), e)
            })?;

        let mut modules = BTreeMap::new();
        for (mod_name, mod_version_req) in &root.requires {
            Self::mock_load_module(mod_name, mod_version_req, files, &mut modules)?;
        }
        Self::load_data(root, modules)
    }

    #[cfg(all(not(target_family = "wasm"), feature = "tokio"))]
    #[async_recursion]
    async fn load_module(
        mod_name: &ModuleName,
        mod_version_req: &ModuleVersionReq,
        base: &std::path::Path,
        modules: &mut BTreeMap<ModuleName, serial::Module>,
    ) -> Result<(), LoadSchemaError> {
        if !modules.contains_key(mod_name) {
            let path = base.join(format!("{mod_name}.schema.yaml"));
            let module = serde_yaml::from_slice::<serial::Module>(
                &tokio::fs::read(&path)
                    .await
                    .map_err(|e| LoadSchemaError::ReadFile(path.clone(), e))?,
            )
            .map_err(|e| LoadSchemaError::DeserializeFile(path.clone(), e))?;

            mod_version_req
                .0
                .matches(&module.version.0)
                .then_some(())
                .ok_or_else(|| {
                    LoadSchemaError::IncompatibleModuleVersion(
                        mod_name.clone(),
                        mod_version_req.clone(),
                        module.version.clone(),
                    )
                })?;

            let requires = module.requires.clone();
            modules.insert(mod_name.clone(), module);

            for (mod_name, mod_version_req) in &requires {
                Self::load_module(mod_name, mod_version_req, base, modules).await?
            }
        }
        Ok(())
    }

    #[cfg(not(target_family = "wasm"))]
    fn load_module_sync(
        mod_name: &ModuleName,
        mod_version_req: &ModuleVersionReq,
        base: &std::path::Path,
        modules: &mut BTreeMap<ModuleName, serial::Module>,
    ) -> Result<(), LoadSchemaError> {
        if !modules.contains_key(mod_name) {
            let path = base.join(format!("{mod_name}.schema.yaml"));
            let module = serde_yaml::from_slice::<serial::Module>(
                &std::fs::read(&path).map_err(|e| LoadSchemaError::ReadFile(path.clone(), e))?,
            )
            .map_err(|e| LoadSchemaError::DeserializeFile(path.clone(), e))?;

            mod_version_req
                .0
                .matches(&module.version.0)
                .then_some(())
                .ok_or_else(|| {
                    LoadSchemaError::IncompatibleModuleVersion(
                        mod_name.clone(),
                        mod_version_req.clone(),
                        module.version.clone(),
                    )
                })?;

            let requires = module.requires.clone();
            modules.insert(mod_name.clone(), module);

            for (mod_name, mod_version_req) in &requires {
                Self::load_module_sync(mod_name, mod_version_req, base, modules)?
            }
        }
        Ok(())
    }

    fn mock_load_module(
        mod_name: &ModuleName,
        mod_version_req: &ModuleVersionReq,
        files: &BTreeMap<&str, &str>,
        modules: &mut BTreeMap<ModuleName, serial::Module>,
    ) -> Result<(), LoadSchemaError> {
        if !modules.contains_key(mod_name) {
            let path = format!("{mod_name}.schema.yaml");
            let module =
                serde_yaml::from_str::<serial::Module>(files.get(&path.as_str()).ok_or_else(
                    || LoadSchemaError::MockFileNotFound(std::path::Path::new(&path).to_path_buf()),
                )?)
                .map_err(|e| {
                    LoadSchemaError::DeserializeFile(std::path::Path::new(&path).to_path_buf(), e)
                })?;

            mod_version_req
                .0
                .matches(&module.version.0)
                .then_some(())
                .ok_or_else(|| {
                    LoadSchemaError::IncompatibleModuleVersion(
                        mod_name.clone(),
                        mod_version_req.clone(),
                        module.version.clone(),
                    )
                })?;

            let requires = module.requires.clone();
            modules.insert(mod_name.clone(), module);

            for (mod_name, mod_version_req) in &requires {
                Self::mock_load_module(mod_name, mod_version_req, files, modules)?
            }
        }
        Ok(())
    }

    pub fn iter_modules(&self) -> impl Iterator<Item = (&ModuleName, &Module)> {
        self.modules.iter()
    }

    pub fn get_module(&self, module: &Ref<Module>) -> &Module {
        self.modules.borrow(module)
    }

    pub fn get_item(&self, item: &Ref<Item>) -> &Item {
        self.items.borrow(item)
    }

    pub fn lookup_module(&self, name: &ModuleName) -> Option<&Module> {
        self.modules.get(name)
    }

    pub fn lookup_item(&self, name: &QualifiedItemName) -> Option<&Item> {
        self.items.get(name)
    }

    pub fn lookup_item_ref(&self, name: &QualifiedItemName) -> Option<Ref<Item>> {
        self.items.get_ref(name).cloned()
    }

    pub fn lookup_module_entry(&self, name: &ModuleName) -> Option<(&ModuleName, &Module)> {
        self.modules.get_key_value(name)
    }

    pub fn lookup_item_entry(
        &self,
        name: &QualifiedItemName,
    ) -> Option<(&QualifiedItemName, &Item)> {
        self.items.get_key_value(name)
    }

    pub fn tree(&self) -> Tree {
        Tree::Branch(
            std::iter::once((
                self.root.key().to_string(),
                self.get_item(self.root.value_ref())
                    .tree(self, self.root.key()),
            ))
            .collect(),
        )
    }

    pub fn root_name(&self) -> &QualifiedItemName {
        self.root.key()
    }

    pub fn root(&self) -> &Item {
        self.get_item(self.root.value_ref())
    }

    pub fn root_branch(&self) -> Branch {
        Branch::Item(self.root_name(), self.root())
    }

    pub fn walk_from_root<'a, 'b, T>(
        &'a self,
        elems: T,
    ) -> impl Iterator<Item = WalkResult<Branch<'a>>>
    where
        T: IntoIterator<Item = &'b str>,
    {
        let mut elems = elems.into_iter();
        std::iter::successors::<WalkResult<Branch>, _>(
            Some(Ok(self.root_branch())),
            move |branch| {
                let elem = elems.next()?;
                let next = branch
                    .as_ref()
                    .ok()?
                    .walk(self, elem)
                    .ok_or_else(|| WalkError::ElemNotFound(elem.to_string()));
                Some(next)
            },
        )
    }

    pub fn gather<'a, F, S>(&'a self, path: &[&str], init: S, mut f: F) -> Option<S>
    where
        F: FnMut(S, &Branch<'a>) -> S,
    {
        let mut path = path.iter();
        match path.next() {
            Some(name) => {
                let branch = self.walk(name)?;
                let state = f(init, &branch);
                let res = path.try_fold((state, branch), |(state, branch), name| {
                    let branch = branch.walk(self, name)?;
                    let state = f(state, &branch);
                    Some((state, branch))
                })?;
                Some(res.0)
            }
            None => Some(init),
        }
    }

    // pub fn fold_items<'a, F, S>(&'a self, init: S, mut f: F) -> S
    // where
    //     F: for<'c> FnMut(S, &'c Path<'c>, &'a Item) -> S,
    // {
    //     let path = Path::root();
    //     self.items.iter().fold(init, |state, (name, item)| {
    //         let path = path.item(name);
    //         let state = f(state, &path, &item);
    //         self.get_item(item).fold_items(state, &path, &mut f)
    //     })
    // }

    pub fn walk(&self, name: &str) -> Option<Branch> {
        let name = QualifiedItemName::from_str(name).ok()?;
        let (name, item) = self.lookup_item_entry(&name)?;
        Some(Branch::Item(name, item))
    }
}

impl Item {
    pub fn tree(&self, universe: &Universe, name: &QualifiedItemName) -> tree::Branch {
        tree::Branch {
            query: self.query.clone(),
            assert: self.assert.clone(),
            keys: self.keys.clone(),
            item: Some(name.to_string()),
            tree: Tree::Branch(
                self.items(universe)
                    .map(|(name, item)| (name.to_string(), item.tree(universe, name)))
                    .chain(
                        self.metrics()
                            .map(|(name, metric)| (name.to_string(), metric.tree(name))),
                    )
                    .collect(),
            ),
        }
    }

    pub fn walk<'a>(&'a self, universe: &'a Universe, name: &str) -> Option<Branch<'a>> {
        QualifiedItemName::from_str(name)
            .ok()
            .and_then(|name| {
                self.items
                    .get_key_value(&name)
                    .map(|(name, item)| Branch::Item(name, universe.get_item(item)))
            })
            .or_else(|| {
                self.metrics
                    .get_key_value(name)
                    .map(|(name, metric)| Branch::Metric(name, metric))
            })
    }

    pub fn labels(&self) -> impl Iterator<Item = (&LabelName, &LabelSelector)> {
        self.query.iter().chain(&self.assert)
    }

    pub fn keys(&self) -> impl Iterator<Item = &LabelName> {
        self.keys.iter()
    }

    pub fn items<'a>(
        &'a self,
        universe: &'a Universe,
    ) -> impl Iterator<Item = (&'a QualifiedItemName, &'a Item)> {
        self.items
            .iter()
            .map(|(name, item)| (name, universe.get_item(item)))
    }

    pub fn parents<'a>(
        &'a self,
        universe: &'a Universe,
    ) -> impl Iterator<Item = (&'a QualifiedItemName, &'a Item)> {
        self.parents
            .iter()
            .map(|(name, item)| (name, universe.get_item(item)))
    }

    pub fn parents_recursive<'a>(
        &'a self,
        universe: &'a Universe,
    ) -> Box<dyn Iterator<Item = (&'a QualifiedItemName, &'a Item)> + 'a> {
        Box::new(self.parents(universe).flat_map(|(parent_name, parent)| {
            parent
                .parents_recursive(universe)
                .chain(std::iter::once((parent_name, parent)))
        }))
    }

    pub fn svc_queries(
        &self,
        name: &QualifiedItemName,
        universe: &Universe,
    ) -> BTreeMap<String, MetricSelector> {
        let mut sources = BTreeMap::new();
        self.add_svc_queries(
            name,
            Vec::new(),
            &MetricSelector::new(),
            universe,
            &mut sources,
        );
        sources
    }

    fn add_svc_queries(
        &self,
        name: &QualifiedItemName,
        path: Vec<&QualifiedItemName>,
        query: &MetricSelector,
        universe: &Universe,
        queries: &mut BTreeMap<String, MetricSelector>,
    ) {
        let query = self.query(universe) & query;
        if name == universe.root_name() {
            let id = if path.is_empty() {
                String::from("default")
            } else {
                let mut s = String::new();
                let mut ps = path.into_iter();
                ps.next().map(|p| write!(s, "{p}")).transpose().unwrap();
                ps.for_each(|p| write!(s, "/{p}").unwrap());
                s
            };
            queries.insert(id, query);
        } else if self.parents.len() == 1 {
            let (parent_name, parent_ref) = self.parents.first_key_value().unwrap();
            let parent = universe.items.borrow(parent_ref);
            parent.add_svc_queries(parent_name, path, &query, universe, queries)
        } else {
            self.parents.iter().for_each(|(parent_name, parent_ref)| {
                let parent = universe.items.borrow(parent_ref);
                let mut path = path.clone();
                path.push(parent_name);
                parent.add_svc_queries(parent_name, path, &query, universe, queries)
            });
        }
    }

    pub fn metric_queries(&self, _universe: &Universe) -> BTreeMap<MetricName, MetricSelector> {
        self.metrics()
            .map(|(metric_name, metric)| {
                (
                    metric_name.clone(),
                    metric
                        .labels()
                        .map(|(name, selector)| (name.clone(), selector.clone()))
                        .collect(),
                )
            })
            .collect()
    }

    pub fn metrics(&self) -> impl Iterator<Item = (&MetricName, &Metric)> {
        self.metrics.iter()
    }

    fn branches<'a>(&'a self, universe: &'a Universe) -> impl Iterator<Item = Branch<'a>> {
        self.items(universe)
            .map(|(name, item)| Branch::Item(name, item))
            .chain(
                self.metrics()
                    .map(|(name, metric)| Branch::Metric(name, metric)),
            )
    }

    pub fn query(&self, _universe: &Universe) -> MetricSelector {
        self.query.clone()
    }

    #[deprecated(note = "'implements' was removed")]
    pub fn all_labels<'a>(
        &'a self,
        _universe: &'a Universe,
    ) -> impl Iterator<Item = (&'a LabelName, &'a LabelSelector)> {
        self.labels()
    }

    #[deprecated(note = "'implements' was removed")]
    pub fn all_keys<'a>(&'a self, _universe: &'a Universe) -> impl Iterator<Item = &'a LabelName> {
        self.keys()
    }

    #[deprecated(note = "'implements' was removed")]
    pub fn all_items<'a>(
        &'a self,
        universe: &'a Universe,
    ) -> impl Iterator<Item = (&'a QualifiedItemName, &'a Item)> {
        self.items(universe)
    }

    #[deprecated(note = "'implements' was removed")]
    pub fn all_metrics<'a>(
        &'a self,
        _universe: &'a Universe,
    ) -> impl Iterator<Item = (&'a MetricName, &'a Metric)> {
        self.metrics()
    }

    pub fn names(&self, universe: &Universe) -> BTreeSet<String> {
        self.branches(universe)
            .flat_map(|branch| branch.names(universe))
            .collect()
    }

    pub fn paths<'a>(&'a self, name: &'a QualifiedItemName, universe: &'a Universe) -> Vec<String> {
        self.paths_internal(name, &Path::Root, universe, &mut |path| {
            path.reversed().to_string()
        })
    }

    pub fn with_paths<'a, F, R>(
        &'a self,
        name: &'a QualifiedItemName,
        universe: &'a Universe,
        mut f: F,
    ) -> Vec<R>
    where
        F: FnMut(&Path<(&'a QualifiedItemName, &'a Item)>) -> R,
    {
        self.paths_internal(name, &Path::Root, universe, &mut f)
    }

    fn paths_internal<'a, F, R>(
        &'a self,
        name: &'a QualifiedItemName,
        path: &Path<(&'a QualifiedItemName, &'a Item)>,
        universe: &'a Universe,
        f: &mut F,
    ) -> Vec<R>
    where
        F: FnMut(&Path<(&'a QualifiedItemName, &'a Item)>) -> R,
    {
        (name == universe.root.key())
            .then(|| f(path))
            .into_iter()
            .chain(
                self.parents(universe)
                    .flat_map(move |(parent_name, parent)| {
                        parent.paths_internal(parent_name, &path.step((name, self)), universe, f)
                    }),
            )
            .collect()
    }

    pub fn show_tree(&self, universe: &Universe, name: &QualifiedItemName, indent: u64) {
        (0..indent).for_each(|_| print!("  "));
        println!("{name}");
        self.items.iter().for_each(|(name, item)| {
            universe
                .get_item(item)
                .show_tree(universe, name, indent + 1)
        });
        self.metrics.keys().for_each(|metric| {
            (0..indent + 1).for_each(|_| print!(" "));
            println!("{metric}");
        });
    }
}

impl Metric {
    pub fn to_serial(&self) -> serial::Metric {
        match &self.r#type {
            MetricType::Scalar(m) => serial::Metric::Scalar(serial::Scalar {
                query: self.query.clone(),
                labels: self.labels.clone(),
                unit: self.unit.clone(),
                r#type: m.r#type.clone(),
            }),
            MetricType::Histogram(_) => serial::Metric::Histogram(serial::Histogram {
                query: self.query.clone(),
                labels: self.labels.clone(),
                unit: self.unit.clone(),
            }),
            MetricType::Summary(_) => serial::Metric::Summary(serial::Summary {
                query: self.query.clone(),
                labels: self.labels.clone(),
                unit: self.unit.clone(),
            }),
        }
    }

    pub fn labels(&self) -> impl Iterator<Item = (&LabelName, &LabelSelector)> {
        self.labels.iter()
    }

    pub fn names(&self, name: &MetricName) -> BTreeSet<String> {
        match &self.r#type {
            MetricType::Scalar(metric) => metric.names(name),
            MetricType::Histogram(metric) => metric.names(name),
            MetricType::Summary(metric) => metric.names(name),
        }
    }

    /// Gives a list of paths where metrics with the same name are
    /// used. Different from items, that must have a unique name and
    /// can be reused, metrics definitions are not reused and can have
    /// overlapping names. Metrics at different paths can therefore
    /// have different properties.
    pub fn paths(&self, name: &MetricName, universe: &Universe) -> Vec<String> {
        universe
            .metrics
            .get(name)
            .iter()
            .flat_map(|items| items.iter())
            .flat_map(|(item_name, item_ref)| {
                universe.items.borrow(item_ref).paths_internal(
                    item_name,
                    &Path::Root,
                    universe,
                    &mut |path| {
                        let mut p = path.reversed().to_string();
                        write!(p, "/{name}").unwrap();
                        p
                    },
                )
            })
            .collect()
    }

    pub fn tree(&self, name: &MetricName) -> tree::Branch {
        match &self.r#type {
            MetricType::Scalar(metric) => metric.tree(name, self),
            MetricType::Histogram(metric) => metric.tree(name, self),
            MetricType::Summary(metric) => metric.tree(name, self),
        }
    }
}

impl Scalar {
    fn names(&self, name: &MetricName) -> BTreeSet<String> {
        BTreeSet::from_iter([name.to_string()])
    }

    fn tree(&self, name: &MetricName, metric: &Metric) -> tree::Branch {
        tree::Branch {
            query: std::iter::once((METRIC_LABEL.clone(), LabelSelector::In(self.names(name))))
                .chain(metric.query.clone())
                .collect(),
            assert: metric.labels.clone(),
            keys: BTreeSet::new(),
            item: None,
            tree: tree::Tree::Leaf(Leaf {}),
        }
    }
}

impl Histogram {
    fn names(&self, name: &MetricName) -> BTreeSet<String> {
        BTreeSet::from_iter([
            format!("{name}_count"),
            format!("{name}_sum"),
            format!("{name}_bucket"),
        ])
    }

    pub fn tree(&self, name: &MetricName, metric: &Metric) -> tree::Branch {
        tree::Branch {
            query: std::iter::once((METRIC_LABEL.clone(), LabelSelector::In(self.names(name))))
                .chain(metric.query.clone())
                .collect(),
            assert: metric.labels.clone(),
            keys: BTreeSet::new(),
            item: None,
            tree: tree::Tree::Branch(BTreeMap::from_iter([
                (
                    String::from("count"),
                    tree::Branch {
                        query: MetricSelector::from_iter([(
                            METRIC_LABEL.clone(),
                            LabelSelector::Eq(format!("{name}_count")),
                        )]),
                        assert: MetricSelector::new(),
                        keys: BTreeSet::new(),
                        item: None,
                        tree: tree::Tree::Leaf(Leaf {}),
                    },
                ),
                (
                    String::from("sum"),
                    tree::Branch {
                        query: MetricSelector::from_iter([(
                            METRIC_LABEL.clone(),
                            LabelSelector::Eq(format!("{name}_sum")),
                        )]),
                        assert: MetricSelector::new(),
                        keys: BTreeSet::new(),
                        item: None,
                        tree: tree::Tree::Leaf(Leaf {}),
                    },
                ),
                (
                    String::from("buckets"),
                    tree::Branch {
                        query: MetricSelector::from_iter([
                            (
                                METRIC_LABEL.clone(),
                                LabelSelector::Eq(format!("{name}_bucket")),
                            ),
                            (LE_LABEL.clone(), LabelSelector::Set),
                        ]),
                        assert: MetricSelector::new(),
                        keys: BTreeSet::from_iter([LE_LABEL.clone()]),
                        item: Some(format!("{name}_bucket")),
                        tree: tree::Tree::Leaf(Leaf {}),
                    },
                ),
            ])),
        }
    }
}

impl Summary {
    fn names(&self, name: &MetricName) -> BTreeSet<String> {
        BTreeSet::from_iter([
            name.to_string(),
            format!("{name}_count"),
            format!("{name}_sum"),
        ])
    }

    pub fn tree(&self, name: &MetricName, metric: &Metric) -> tree::Branch {
        tree::Branch {
            query: std::iter::once((METRIC_LABEL.clone(), LabelSelector::In(self.names(name))))
                .chain(metric.query.clone())
                .collect(),
            assert: metric.labels.clone(),
            keys: BTreeSet::new(),
            item: None,
            tree: tree::Tree::Branch(BTreeMap::from_iter([
                (
                    String::from("count"),
                    tree::Branch {
                        query: MetricSelector::from_iter([(
                            METRIC_LABEL.clone(),
                            LabelSelector::Eq(format!("{name}_count")),
                        )]),
                        keys: BTreeSet::new(),
                        assert: MetricSelector::new(),
                        item: None,
                        tree: tree::Tree::Leaf(Leaf {}),
                    },
                ),
                (
                    String::from("sum"),
                    tree::Branch {
                        query: MetricSelector::from_iter([(
                            METRIC_LABEL.clone(),
                            LabelSelector::Eq(format!("{name}_sum")),
                        )]),
                        keys: BTreeSet::new(),
                        assert: MetricSelector::new(),
                        item: None,
                        tree: tree::Tree::Leaf(Leaf {}),
                    },
                ),
                (
                    String::from("quantiles"),
                    tree::Branch {
                        query: MetricSelector::from_iter([
                            (METRIC_LABEL.clone(), LabelSelector::Eq(name.to_string())),
                            (QUANTILE_LABEL.clone(), LabelSelector::Set),
                        ]),
                        assert: MetricSelector::new(),
                        keys: BTreeSet::from_iter([QUANTILE_LABEL.clone()]),
                        item: Some(format!("{name}_quantile")),
                        tree: tree::Tree::Leaf(Leaf {}),
                    },
                ),
            ])),
        }
    }
}

impl<'a> Branch<'a> {
    pub fn walk(&self, universe: &'a Universe, name: &str) -> Option<Branch<'a>> {
        match self {
            Branch::Item(_, item) => item.walk(universe, name),
            Branch::Metric(_, _) => None,
        }
    }

    fn names(&self, universe: &Universe) -> BTreeSet<String> {
        match self {
            Branch::Item(_, item) => item.names(universe),
            Branch::Metric(name, metric) => metric.names(name),
        }
    }
}

impl<'a, Step> Path<'a, Step> {
    pub fn root() -> Self {
        Self::Root
    }

    pub fn step(&'a self, step: Step) -> Self {
        Self::Step(step, self)
    }

    pub fn reversed(&self) -> Reversed<&Self> {
        Reversed(self)
    }
}

impl<'a> Path<'a> {
    pub fn item(&'a self, name: &'a QualifiedItemName) -> Self {
        self.step(PathStep::Item(name))
    }

    pub fn metric(&'a self, name: &'a str) -> Self {
        self.step(PathStep::Metric(name))
    }
}

impl<'a, Step> Iterator for &'a Path<'a, Step> {
    type Item = &'a Step;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Path::Root => None,
            Path::Step(step, parent) => {
                *self = parent;
                Some(step)
            }
        }
    }
}

impl Display for Path<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Path::Root => write!(f, "/"),
            Path::Step(component, Path::Root) => {
                write!(f, "/{component}")
            }
            Path::Step(component, parent) => {
                write!(f, "{parent}/{component}")
            }
        }
    }
}

impl Display for Reversed<&Path<'_>> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            Path::Root => write!(f, "/"),
            Path::Step(component, Path::Root) => {
                write!(f, "/{component}")
            }
            Path::Step(component, parent) => {
                write!(f, "/{component}{}", Reversed(*parent))
            }
        }
    }
}

impl Display for Reversed<&Path<'_, (&QualifiedItemName, &Item)>> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            Path::Root => write!(f, "/"),
            Path::Step((component, _), Path::Root) => {
                write!(f, "/{component}")
            }
            Path::Step((component, _), parent) => {
                write!(f, "/{component}{}", Reversed(*parent))
            }
        }
    }
}

impl Display for PathStep<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PathStep::Item(name) => write!(f, "{name}"),
            PathStep::Metric(name) => write!(f, "{name}"),
        }
    }
}

#[derive(thiserror::Error, Debug)]
pub enum LoadSchemaError {
    #[error("failed to read file {}: {1}", .0.display())]
    ReadFile(PathBuf, std::io::Error),
    #[error("failed to deserialize file {}: {1}", .0.display())]
    DeserializeFile(PathBuf, serde_yaml::Error),
    #[error("failed to read mock file {}", .0.display())]
    MockFileNotFound(PathBuf),
    #[error("module {0} version {2} does not match requirement {1}")]
    IncompatibleModuleVersion(ModuleName, ModuleVersionReq, ModuleVersion),
    #[error("failed to resolve: {0}")]
    Resolve(ResolveError),
}

#[derive(thiserror::Error, Debug)]
pub enum ResolveError {
    #[error("missing module: {0}")]
    MissingModule(ModuleName),
    #[error("missing item: {0}")]
    MissingItem(QualifiedItemName),
}

// Note: must agree with Digestible impl for serial::Item!
impl Digestible for Item {
    type Data<'a> = (&'a Universe, &'a ModuleName);

    fn digest(&self, ctx: &mut ring::digest::Context, version: u64, data: Self::Data<'_>) {
        self.query.digest(ctx, version, ());
        self.assert.digest(ctx, version, ());
        self.keys.digest(ctx, version, ());

        self.items.len().digest(ctx, version, ());
        self.items
            .iter()
            .fold(Sha256::default(), |mut sum, (name, item_ref)| {
                if name.module() == data.1 {
                    sum ^= data.0.items.borrow(item_ref).digest_sha256(version, data);
                } else {
                    sum ^= name.digest_sha256(version, ());
                }
                sum
            })
            .digest(ctx, version, ());

        self.metrics.digest(ctx, version, ((), ()));
        self.fork.digest(ctx, version, ());
    }
}

// Note: must agree with Digestible impl for serial::Metric!
impl Digestible for Metric {
    type Data<'a> = ();

    fn digest(&self, ctx: &mut ring::digest::Context, version: u64, _data: Self::Data<'_>) {
        match &self.r#type {
            MetricType::Scalar(v) => {
                "scalar".digest(ctx, version, ());
                v.digest(ctx, version, self);
            }
            MetricType::Histogram(v) => {
                "histogram".digest(ctx, version, ());
                v.digest(ctx, version, self);
            }
            MetricType::Summary(v) => {
                "summary".digest(ctx, version, ());
                v.digest(ctx, version, self);
            }
        }
    }
}

impl Digestible for Scalar {
    type Data<'a> = &'a Metric;

    fn digest(&self, ctx: &mut ring::digest::Context, version: u64, data: Self::Data<'_>) {
        data.query.digest(ctx, version, ());
        data.labels.digest(ctx, version, ());
        data.unit.digest(ctx, version, ());
        self.r#type.digest(ctx, version, ());
    }
}

impl Digestible for Histogram {
    type Data<'a> = &'a Metric;

    fn digest(&self, ctx: &mut ring::digest::Context, version: u64, data: Self::Data<'_>) {
        data.query.digest(ctx, version, ());
        data.labels.digest(ctx, version, ());
        data.unit.digest(ctx, version, ());
    }
}

impl Digestible for Summary {
    type Data<'a> = &'a Metric;

    fn digest(&self, ctx: &mut ring::digest::Context, version: u64, data: Self::Data<'_>) {
        data.query.digest(ctx, version, ());
        data.labels.digest(ctx, version, ());
        data.unit.digest(ctx, version, ());
    }
}

#[cfg(test)]
mod test {
    use super::Path;

    #[test]
    fn display_path() {
        let item_a = "mod:a".parse().unwrap();
        let item_b = "mod:b".parse().unwrap();
        let item_c = "mod:c".parse().unwrap();
        let root = Path::root();
        let path = root.item(&item_a);
        let path = path.item(&item_b);
        let path = path.item(&item_c);
        assert_eq!(&path.to_string(), "/mod:a/mod:b/mod:c")
    }

    #[test]
    fn display_reversed_path() {
        let item_a = "mod:a".parse().unwrap();
        let item_b = "mod:b".parse().unwrap();
        let item_c = "mod:c".parse().unwrap();
        let root = Path::root();
        let path = root.item(&item_a);
        let path = path.item(&item_b);
        let path = path.item(&item_c);
        assert_eq!(&path.reversed().to_string(), "/mod:c/mod:b/mod:a")
    }
}
