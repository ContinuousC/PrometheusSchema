/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::{
    borrow::Cow,
    collections::{BTreeMap, BTreeSet},
    sync::atomic::{AtomicBool, Ordering},
};

use bimap::BiBTreeMap;
use itertools::Itertools;
use lazy_static::lazy_static;
use prometheus_api::GenericMetric;
use prometheus_core::MetricName;
use rayon::iter::{IndexedParallelIterator, IntoParallelIterator};
use serde::{Deserialize, Serialize};

use crate::{
    generate::label_map::{
        cumulative_union_of_unions, labelset_intersection, labelset_matches, labelset_union,
        union_of_unions,
    },
    query::LabelSelectorPrim,
    serial::{Histogram, Item, Metric, Module, Scalar, Summary},
    ItemName, ItemRef, MetricSelector, ModuleVersion,
};

use super::{
    label_map::{
        query_as_prim, LabelMap, LabelName, LabelSelector, LabelSet, LabelValue, Query, QueryPrim,
    },
    limited::{Limited, LimitedAlt},
    one_to_many::OneToMany,
    GenInfo,
};

#[derive(Clone, Default)]
struct Tree<'a> {
    branches: Vec<Branch<'a>>,
    choices: BTreeSet<BTreeSet<LabelName<'a>>>,
    choice: BTreeSet<LabelName<'a>>,
}

#[derive(Clone)]
struct Branch<'a> {
    query: Query<'a>,
    tree: Tree<'a>,
}

lazy_static! {
    static ref NAME: LabelName<'static> = LabelName::new("__name__");
    static ref LE: LabelName<'static> = LabelName::new("le");
    static ref QUANTILE: LabelName<'static> = LabelName::new("quantile");
    static ref SPECIALS: Vec<LabelSet<'static>> = Vec::from_iter([
        BTreeSet::from_iter([&*NAME]),
        BTreeSet::from_iter([&*LE, &*QUANTILE])
    ]);
}

#[serde_with::serde_as]
#[derive(Serialize, Deserialize)]
pub struct GeneratedSchema {
    pub module: Module,
    pub choices: Vec<Choice>,
    pub names: OneToMany<ItemName, MetricSelector>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
pub struct Choice {
    pub query: MetricSelector,
    pub choices: BTreeSet<BTreeSet<prometheus_core::LabelName>>,
    pub choice: BTreeSet<prometheus_core::LabelName>,
}

pub fn generate_schema<T>(
    metrics: &[GenericMetric<T>],
    geninfo: &GenInfo,
    term: &AtomicBool,
    debug_depth: usize,
    eval_depth: usize,
) -> Option<GeneratedSchema> {
    let metrics = metrics
        .iter()
        .filter(|m| geninfo.query.matches(&m.metric))
        .map(|m| {
            m.metric
                .iter()
                .map(|(label, value)| (LabelName::new(label.as_str()), LabelValue::new(value)))
                .collect::<BTreeMap<_, _>>()
        })
        .collect::<Vec<_>>();

    let split_by = geninfo
        .hints
        .split_by
        .iter()
        .map(|split_by| {
            let label = LabelName::new(split_by.label.as_str());
            let query = split_by
                .query
                .iter()
                .map(|(label, selector)| {
                    let label = LabelName::new(label.as_str());
                    (label, selector.as_prim())
                })
                .collect();
            (label, query)
        })
        .collect::<Vec<_>>();

    let choose = geninfo
        .hints
        .choose
        .iter()
        .map(|choose| {
            let query = choose
                .query
                .iter()
                .map(|(label, selector)| {
                    let label = LabelName::new(label.as_str());
                    (label, selector.as_prim())
                })
                .collect();
            let labels = choose
                .choice
                .iter()
                .map(|label| LabelName::new(label.as_str()))
                .collect();
            (query, labels)
        })
        .collect();

    let query = BTreeMap::new();
    let base_query = geninfo
        .query
        .iter()
        .map(|(label, selector)| (LabelName::new(label.as_str()), selector.as_prim()))
        .collect::<BTreeMap<_, _>>();
    let by_value = get_by_value(&query, &split_by);
    let mask = metrics
        .iter()
        .flat_map(|ls| ls.keys())
        .collect::<BTreeSet<_>>()
        .into_iter()
        .filter(|label| {
            !SPECIALS.iter().any(|s| s.contains(label))
                && prim_allows_refinement(label, &base_query, by_value.contains(label))
        })
        .collect::<BTreeSet<_>>();
    let lms = get_labelsets(metrics.iter(), &by_value);

    let (_, tree) = get_tree(TreeArgs {
        max: 10000,
        search: Search::Depth(eval_depth),
        depth: 0,
        debug_depth,
        term,
        specials: &SPECIALS,
        split_by: &split_by,
        choose: &choose,
        base_query: &base_query,
        query: &query,
        lms: &lms,
        mask: &mask,
        by_value: &by_value,
    })?;

    let mut choices = Vec::new();
    build_choices(&tree, &MetricSelector::new(), &mut choices);

    let mut items = BiBTreeMap::new();
    let children = build_items(tree, &mut items);
    let root_name = ItemName::new("root");
    items.insert(
        root_name.clone(),
        Item {
            items: children,
            ..Item::default()
        },
    );

    let (items, names) = rename_items(&root_name, items, &geninfo.hints.rename);

    Some(GeneratedSchema {
        module: Module {
            version: ModuleVersion::new("0.1.0".parse().unwrap()),
            requires: BTreeMap::new(),
            items,
        },
        choices,
        names,
    })
}

fn get_by_value<'a>(
    query: &Query,
    split_by: &'a [(LabelName<'a>, BTreeMap<LabelName, LabelSelectorPrim>)],
) -> LabelSet<'a> {
    std::iter::once(&*NAME)
        .chain(
            split_by
                .iter()
                .filter_map(|(label, crit)| query_implies_prim(query, crit).then_some(label)),
        )
        .collect()
}

fn get_labelsets<'a>(
    ms: impl IntoIterator<Item = &'a LabelMap<'a>>,
    by_value: &LabelSet<'a>,
) -> BTreeMap<Query<'a>, Vec<&'a LabelMap<'a>>> {
    ms.into_iter().fold(BTreeMap::new(), |mut m, lm| {
        let q = lm
            .iter()
            .map(|(label, value)| {
                if by_value.contains(label) {
                    (label, LabelSelector::ByValue(value))
                } else {
                    (label, LabelSelector::ByPresence(true))
                }
            })
            .collect();
        m.entry(q).or_default().push(lm);
        m
    })
}

fn query_implies_prim(a: &Query, b: &BTreeMap<LabelName, LabelSelectorPrim>) -> bool {
    // Check if `a` implies `b`.
    b.iter().all(|(label, b)| {
        a.get(label).map_or_else(
            || matches!(b, crate::query::LabelSelectorPrim::NotIn(vs) if vs.is_empty()),
            |a| match (a, b) {
                (LabelSelector::ByPresence(true), LabelSelectorPrim::NotIn(bs)) => bs.contains(""),
                (LabelSelector::ByPresence(false), LabelSelectorPrim::In(bs)) => bs.contains(""),
                (LabelSelector::ByPresence(_), _) => false,
                (LabelSelector::ByValue(a), LabelSelectorPrim::In(bs)) => bs.contains(a.as_str()),
                (LabelSelector::ByValue(a), LabelSelectorPrim::NotIn(bs)) => {
                    !bs.contains(a.as_str())
                }
            },
        )
    })
}

fn query_allows_refinement(label: &LabelName, query: &Query, by_value: bool) -> bool {
    query.get(label).map_or(true, |selector| match selector {
        LabelSelector::ByPresence(false) => false,
        LabelSelector::ByPresence(true) => by_value,
        LabelSelector::ByValue(_) => false,
    })
}

fn prim_allows_refinement(label: &LabelName, query: &QueryPrim, by_value: bool) -> bool {
    query.get(label).map_or(true, |selector| match selector {
        // None or only one possible value: no refinement possible.
        LabelSelectorPrim::In(vs) if vs.len() <= 1 => false,
        // Both unset and other values allowed: always refineable.
        LabelSelectorPrim::In(vs) if vs.contains("") => true,
        // Multiple non-unset values allowed: refineable if selecting by value.
        LabelSelectorPrim::In(_) => by_value,
        // Unset not allowed: refineable if selecting by value.
        LabelSelectorPrim::NotIn(vs) if vs.contains("") => by_value,
        // Both unset and set allowed: always refineable.
        LabelSelectorPrim::NotIn(_) => true,
    })
}

struct TreeArgs<'a: 'b, 'b> {
    max: usize,
    search: Search,
    depth: usize,
    debug_depth: usize,
    term: &'a AtomicBool,
    specials: &'b [LabelSet<'a>],
    split_by: &'a [(LabelName<'a>, QueryPrim<'a>)],
    choose: &'a BTreeMap<QueryPrim<'a>, BTreeSet<LabelName<'a>>>,
    base_query: &'a QueryPrim<'a>,
    query: &'b Query<'a>,
    lms: &'b BTreeMap<Query<'a>, Vec<&'a LabelMap<'a>>>,
    mask: &'b LabelSet<'a>,
    by_value: &'b LabelSet<'a>,
}

#[derive(Clone, Copy, Debug)]
enum Search {
    Depth(usize),
    Searching(usize),
}

trait TreeRes<'a>: Send + Default {
    fn tree(cs: &[(usize, Vec<Query<'a>>)], qs: &[Query<'a>]) -> Self;
    fn add_branch(&mut self, branch: Branch<'a>);
}

impl<'a> TreeRes<'a> for Tree<'a> {
    fn tree(cs: &[(usize, Vec<Query<'a>>)], qs: &[Query<'a>]) -> Self {
        Self {
            branches: Vec::with_capacity(qs.len()),
            choices: cs
                .iter()
                .map(|(_, c)| c.iter().flat_map(|q| q.keys()).copied().cloned().collect())
                .collect(),
            choice: qs.iter().flat_map(|q| q.keys()).copied().cloned().collect(),
        }
    }

    fn add_branch(&mut self, branch: Branch<'a>) {
        self.branches.push(branch);
    }
}

impl<'a> TreeRes<'a> for () {
    fn tree(_cs: &[(usize, Vec<Query<'a>>)], _qs: &[Query<'a>]) -> Self {}
    fn add_branch(&mut self, _branch: Branch<'a>) {}
}

fn get_tree<'a: 'b, 'b, T: TreeRes<'a>>(
    args @ TreeArgs {
        max,
        search,
        depth,
        debug_depth,
        term,
        specials,
        choose,
        query,
        lms,
        mask,
        ..
    }: TreeArgs<'a, 'b>,
) -> Limited<T> {
    if term.load(Ordering::Acquire) {
        log::debug!("terminating");
        return None;
    }

    if depth < debug_depth {
        log::debug!("{depth}: generating tree with max {max:?}");
    }

    //let lss = get_queries(lms, &by_value, &special, query);

    let r = if mask.is_empty() {
        if specials.is_empty() {
            Some((max, T::default()))
        } else {
            let mask = specials[0]
                .iter()
                .copied()
                .filter(|label| lms.keys().any(|ls| ls.contains_key(label)))
                .collect();
            get_tree(TreeArgs {
                specials: &specials[1..],
                mask: &mask,
                ..args
            })
        }
    } else {
        let cs = get_choices(lms, mask.clone());

        if let Some((_, qs)) = choose
            .get(&query_as_prim(query))
            .and_then(|ls| cs.iter().find(|(_, c)| choice_labels(c).is_superset(ls)))
        {
            get_tree2(args, qs.clone(), &cs)
        } else if cs.iter().any(|(_, qs)| qs.len() == 1) {
            let qs = cs
                .iter()
                .find_map(|(_, qs)| (qs.len() == 1).then_some(qs))
                .unwrap()
                .clone();
            get_tree2(args, qs, &cs)
        } else if let Search::Depth(0) | Search::Searching(0) = search {
            let (_, qs) = cs.iter().max_by_key(|(k, _)| *k).unwrap();
            get_tree2(args, qs.clone(), &cs)
        } else if let Search::Depth(search_depth) = search {
            let (_, qs) = cs
                .clone()
                .into_par_iter()
                .limited_alt(max, |max, (_, qs)| {
                    let (left, ()) = get_tree2(
                        TreeArgs {
                            max,
                            search: Search::Searching(search_depth),
                            ..args
                        },
                        qs.clone(),
                        &cs,
                    )?;
                    Some((left, qs))
                })?;
            get_tree2(args, qs, &cs)
        } else {
            let n = cs.len();

            cs.clone()
                .into_par_iter()
                .enumerate()
                .limited_alt(max, |max, (i, (_, qs))| {
                    if depth < debug_depth {
                        log::debug!(
                            "{depth}: testing tree with {} branches, max {max} (option {}/{n})",
                            qs.len(),
                            i + 1
                        );
                    }

                    let r = get_tree2(TreeArgs { max, ..args }, qs, &cs);

                    if depth < debug_depth {
                        if let Some((left, _)) = &r {
                            log::debug!("{depth}: found result with {left} left")
                        } else {
                            log::debug!("{depth}: pruned")
                        }
                    }

                    r
                })
        }
    };

    if depth < debug_depth {
        if let Some((left, _)) = &r {
            log::debug!("{depth}: returning tree with {left} left");
        } else {
            log::debug!("{depth}: failed to find tree");
        }
    }

    r
}

fn get_tree2<'a: 'b, 'b, T: TreeRes<'a>>(
    args @ TreeArgs {
        max,
        search,
        depth,
        term,
        base_query,
        query,
        split_by,
        specials,
        lms,
        mask,
        by_value,
        ..
    }: TreeArgs<'a, 'b>,
    qs: Vec<Query<'a>>,
    cs: &[(usize, Vec<Query<'a>>)],
) -> Limited<T> {
    if term.load(Ordering::Acquire) {
        log::debug!("terminating");
        return None;
    }

    let n = qs.len();
    let left = max.checked_sub(n)?;
    let tree = T::tree(cs, &qs);
    qs.into_iter()
        .try_fold((left, tree), |(left, mut tree), q| {
            let query = query
                .iter()
                .chain(&q)
                .map(|(label, selector)| (*label, *selector))
                .collect::<BTreeMap<_, _>>();

            let lms = lms
                .iter()
                .filter(|(ls, _)| labelset_matches(&q, ls))
                .map(|(ls, lm)| (ls.clone(), lm.clone())) /* Can this be avoided? :-( */
                .collect::<BTreeMap<_, _>>();
            // eprintln!("query {:?} --> {:?}", query.get("job"), lms[0].get("job"));

            let by_value_new = get_by_value(&query, split_by);
            let (by_value, lms) = if by_value == &by_value_new {
                (by_value, Cow::Borrowed(&lms))
            } else {
                (
                    &by_value_new,
                    Cow::Owned(get_labelsets(
                        lms.values().flatten().copied(),
                        &by_value_new,
                    )),
                )
            };

            let mask = mask
                .iter()
                .copied()
                .filter(|label| !q.contains_key(label))
                .chain(by_value.iter().copied().filter(|label| {
                    !specials.iter().any(|s| s.contains(label))
                        && query_allows_refinement(label, &query, true)
                        && prim_allows_refinement(label, base_query, true)
                }))
                .filter(|label| lms.keys().any(|lm| lm.contains_key(label)))
                .collect();

            if lms.is_empty() {
                panic!("empty branch: {q:?}",);
                //Some((left, tree))
            } else {
                let (left, branch) = get_tree(TreeArgs {
                    max: left,
                    search: match search {
                        Search::Depth(n) => Search::Depth(n),
                        Search::Searching(n) => Search::Searching(n.saturating_sub(1)),
                    },
                    depth: depth + 1,
                    query: &query,
                    lms: &lms,
                    mask: &mask,
                    by_value,
                    ..args
                })?;
                tree.add_branch(Branch {
                    query: q,
                    tree: branch,
                });
                Some((left, tree))
            }
        })
}

fn get_choices<'a>(
    lms: &BTreeMap<Query<'a>, Vec<&'a LabelMap<'a>>>,
    mut mask: LabelSet<'a>,
) -> Vec<(usize, Vec<Query<'a>>)> {
    let mut qss = Vec::new();

    while let Some(label) = mask.first() {
        let sets = lms
            .keys()
            .fold(BTreeMap::<_, Vec<_>>::new(), |mut map, ls| {
                map.entry(
                    ls.get(label)
                        .copied()
                        .unwrap_or(LabelSelector::ByPresence(false)),
                )
                .or_default()
                .push(ls);
                map
            })
            .into_values()
            .collect::<Vec<_>>();

        let unions = sets
            .iter()
            .map(|set| labelset_union(&mask, set.iter().copied()))
            .collect::<Vec<_>>();

        let union_left = cumulative_union_of_unions(unions.iter().take(unions.len() - 1));
        let union_right = cumulative_union_of_unions(unions.iter().skip(1).rev());

        let qs = sets
            .iter()
            .map(|set| labelset_intersection(&mask, set.iter().copied()))
            .enumerate()
            .map(|(i, mut intersection)| {
                let union = match (
                    i.checked_sub(1).map(|i| &union_left[i]),
                    (unions.len() - i).checked_sub(2).map(|i| &union_right[i]),
                ) {
                    (Some(a), Some(b)) => {
                        Some(Cow::Owned(union_of_unions(a.as_ref().clone(), b.as_ref())))
                    }
                    (Some(a), None) | (None, Some(a)) => Some(Cow::Borrowed(a.as_ref())),
                    (None, None) => None,
                };

                if let Some(union) = union {
                    intersection.retain(|label, value| {
                        union
                            .get(label)
                            .map_or(true, |values| !values.contains(value))
                    });
                }
                intersection
            })
            .collect::<Vec<_>>();

        debug_assert!(!qs.iter().any(|q| q.is_empty()));

        let ks = qs
            .iter()
            .fold::<Option<BTreeSet<&LabelName>>, _>(None, |a, b| {
                if let Some(mut a) = a {
                    a.retain(|k| b.contains_key(k));
                    Some(a)
                } else {
                    Some(b.keys().copied().collect())
                }
            })
            .unwrap_or_default();

        ks.iter().for_each(|k| {
            mask.remove(k);
        });

        qss.push((ks.len(), qs));
    }

    debug_assert!(mask.is_empty());
    debug_assert!(!qss.iter().any(|(_, qs)| qs.is_empty()));
    debug_assert!(!qss.is_empty());

    #[cfg(debug_assertions)]
    debug_assert!(
        qss.iter().all(|(_, qs)| {
            lms.keys()
                .all(|ls| qs.iter().filter(|q| labelset_matches(q, ls)).count() == 1)
        }),
        "generated invalid choice set for {lms:?}: {qss:?}",
    );

    qss
}

fn choice_labels<'a>(qs: &[Query<'a>]) -> BTreeSet<LabelName<'a>> {
    qs.iter().flat_map(|q| q.keys()).copied().cloned().collect()
}

fn build_choices(tree: &Tree, query: &MetricSelector, choices: &mut Vec<Choice>) {
    choices.push(Choice {
        query: query.clone(),
        choices: tree
            .choices
            .iter()
            .map(|ls| {
                ls.iter()
                    .map(|l| prometheus_core::LabelName::new(l.to_string()).unwrap())
                    .collect()
            })
            .collect(),
        choice: tree
            .choice
            .iter()
            .map(|l| prometheus_core::LabelName::new(l.to_string()).unwrap())
            .collect(),
    });
    tree.branches.iter().for_each(|branch| {
        let mut branch_query = query.clone();
        branch_query &= &branch.query.iter().map(to_metric_selector).collect();
        build_choices(&branch.tree, &branch_query, choices);
    });
}

fn build_items(tree: Tree, items: &mut BiBTreeMap<ItemName, Item>) -> BTreeSet<ItemRef> {
    tree.branches
        .into_iter()
        .map(|mut branch| {
            let metrics = get_metrics(&mut branch.tree);
            let children = build_items(branch.tree, items);

            let item = Item {
                query: branch.query.iter().map(to_metric_selector).collect(),
                keys: branch
                    .query
                    .iter()
                    .filter(|(_, selector)| matches!(selector, LabelSelector::ByPresence(true)))
                    .map(|(label, _)| prometheus_core::LabelName::new(label.to_string()).unwrap())
                    .collect(),
                items: children,
                metrics,
                ..Item::default()
            };

            if let Some(found) = items.get_by_right(&item) {
                ItemRef::new(None, found.clone())
            } else {
                let name = ItemName::new(
                    branch
                        .query
                        .iter()
                        .map(|(label, selector)| match selector {
                            LabelSelector::ByPresence(false) => format!("!{label}"),
                            LabelSelector::ByPresence(true) => format!("{label}"),
                            LabelSelector::ByValue(v) => {
                                if *label == &*NAME {
                                    format!("[{v}]")
                                } else {
                                    format!("{label}[{v}]")
                                }
                            }
                        })
                        .join("-"),
                );
                let name = std::iter::once(name.clone())
                    .chain((1..).map(|i| ItemName::new(format!("{name}{i}"))))
                    .find(|name| !items.contains_left(name))
                    .unwrap();

                items.insert(name.clone(), item);
                ItemRef::new(None, name)
            }
        })
        .collect()
}

fn rename_items(
    root: &ItemName,
    items: BiBTreeMap<ItemName, Item>,
    renames: &OneToMany<ItemName, MetricSelector>,
) -> (
    BTreeMap<ItemName, Item>,
    OneToMany<ItemName, MetricSelector>,
) {
    let mut paths = BTreeMap::new();
    build_paths(
        root,
        items.get_by_left(root).unwrap(),
        &items,
        &MetricSelector::new(),
        &mut paths,
    );

    let new_names = paths
        .iter()
        .fold(BiBTreeMap::new(), |mut new_names, (name, paths)| {
            let new_name = paths
                .iter()
                .find_map(|path| renames.get_right(path))
                .unwrap_or(name);
            let new_name = std::iter::once(new_name.clone())
                .chain((0..).map(|i| ItemName::new(format!("{new_name}{i}"))))
                .find(|name| !new_names.contains_right(name))
                .unwrap();
            new_names.insert(name.clone(), new_name);
            new_names
        });

    let items = items
        .into_iter()
        .map(|(name, item)| {
            (
                new_names.get_by_left(&name).cloned().unwrap_or(name),
                Item {
                    items: item
                        .items
                        .into_iter()
                        .map(|item_ref| match item_ref {
                            ItemRef { module: None, item } => ItemRef {
                                module: None,
                                item: new_names.get_by_left(&item).cloned().unwrap_or(item),
                            },
                            item_ref => item_ref,
                        })
                        .collect(),
                    ..item
                },
            )
        })
        .collect();

    let names = paths
        .into_iter()
        .filter_map(|(old_name, paths)| {
            Some((new_names.get_by_left(&old_name)?.clone(), paths.clone()))
        })
        .collect();

    (items, names)
}

fn build_paths(
    item_name: &ItemName,
    item: &Item,
    items: &BiBTreeMap<ItemName, Item>,
    query: &MetricSelector,
    paths: &mut BTreeMap<ItemName, BTreeSet<MetricSelector>>,
) {
    let mut path = query.clone();
    path &= &item.query;
    paths
        .entry(item_name.clone())
        .or_default()
        .insert(path.clone());
    item.items.iter().for_each(|item_ref| {
        if let ItemRef { module: None, item } = item_ref {
            build_paths(item, items.get_by_left(item).unwrap(), items, &path, paths)
        }
    });
}

fn get_metrics(tree: &mut Tree) -> BTreeMap<MetricName, Metric> {
    let mut i = 0;
    let mut metrics = BTreeMap::new();
    let mut metric_branches = BTreeMap::new();

    while i < tree.branches.len() {
        let branch = &tree.branches[i];

        if let Some(LabelSelector::ByValue(name)) = branch.query.get(&*NAME) {
            metric_branches.insert(name.as_str(), tree.branches.remove(i));
        } else {
            i += 1;
        }
    }

    while let Some((name, branch)) = metric_branches.pop_first() {
        if let Some((name, metric)) = parse_histogram(name, &branch, &mut metric_branches)
            .or_else(|| parse_summary(name, &branch, &mut metric_branches))
            .or_else(|| parse_scalar(name, &branch))
        {
            metrics.insert(name, metric);
        } else {
            tree.branches.push(branch);
        }
    }

    metrics
}

fn parse_histogram(
    name: &str,
    bucket: &Branch,
    metrics: &mut BTreeMap<&str, Branch>,
) -> Option<(MetricName, Metric)> {
    let base = name.strip_suffix("_bucket")?;
    let (&count_key, count) = metrics.get_key_value(format!("{base}_count").as_str())?;
    let (&sum_key, sum) = metrics.get_key_value(format!("{base}_sum").as_str())?;

    count.tree.branches.is_empty().then_some(())?;
    sum.tree.branches.is_empty().then_some(())?;
    (bucket.tree.branches.len() == 1
        && bucket.tree.branches[0].query.len() == 1
        && bucket.tree.branches[0]
            .query
            .iter()
            .all(|(label, selector)| {
                *label == &*LE && matches!(selector, LabelSelector::ByPresence(true))
            }))
    .then_some(())?;

    (bucket.query.len() == count.query.len()
        && bucket.query.len() == sum.query.len()
        && bucket
            .query
            .iter()
            .filter(|(label, _)| **label != &*NAME)
            .all(|(label, selector)| {
                (count.query.get(label) == Some(selector))
                    && (sum.query.get(label) == Some(selector))
            }))
    .then_some(())?;

    metrics.remove(count_key).unwrap();
    metrics.remove(sum_key).unwrap();

    Some((
        MetricName::new(base.to_string()).unwrap(),
        Metric::Histogram(Histogram {
            labels: bucket
                .query
                .iter()
                .filter(|(label, _)| **label != &*NAME)
                .map(to_metric_selector)
                .collect(),
            ..Histogram::default()
        }),
    ))
}

fn parse_summary<'a>(
    base: &'a str,
    quantile: &Branch<'a>,
    metrics: &mut BTreeMap<&str, Branch<'a>>,
) -> Option<(MetricName, Metric)> {
    let (&count_key, count) = metrics.get_key_value(format!("{base}_count").as_str())?;
    let (&sum_key, sum) = metrics.get_key_value(format!("{base}_sum").as_str())?;

    count.tree.branches.is_empty().then_some(())?;
    sum.tree.branches.is_empty().then_some(())?;
    (quantile.tree.branches.len() == 1
        && quantile.tree.branches[0].query.len() == 1
        && quantile.tree.branches[0]
            .query
            .iter()
            .all(|(label, selector)| {
                *label == &*QUANTILE && matches!(selector, LabelSelector::ByPresence(true))
            }))
    .then_some(())?;

    (quantile.query.len() == count.query.len()
        && quantile.query.len() == sum.query.len()
        && quantile
            .query
            .iter()
            .filter(|(label, _)| **label != &*NAME)
            .all(|(label, selector)| {
                (count.query.get(label) == Some(selector))
                    && (sum.query.get(label) == Some(selector))
            }))
    .then_some(())?;

    metrics.remove(count_key).unwrap();
    metrics.remove(sum_key).unwrap();

    Some((
        MetricName::new(base.to_string()).unwrap(),
        Metric::Summary(Summary {
            labels: quantile
                .query
                .iter()
                .filter(|(label, _)| **label != &*NAME)
                .map(to_metric_selector)
                .collect(),
            ..Summary::default()
        }),
    ))
}

fn parse_scalar<'a>(name: &'a str, branch: &Branch<'a>) -> Option<(MetricName, Metric)> {
    branch.tree.branches.is_empty().then_some(())?;
    Some((
        MetricName::new(name.to_string()).unwrap(),
        Metric::Scalar(Scalar {
            labels: branch
                .query
                .iter()
                .filter(|(label, _)| **label != &*NAME)
                .map(to_metric_selector)
                .collect(),
            ..Scalar::default()
        }),
    ))
}

fn to_metric_selector(
    (label, selector): (&&LabelName, &LabelSelector),
) -> (prometheus_core::LabelName, crate::LabelSelector) {
    (
        prometheus_core::LabelName::new(label.to_string()).unwrap(),
        match selector {
            LabelSelector::ByPresence(true) => crate::LabelSelector::Set,
            LabelSelector::ByPresence(false) => crate::LabelSelector::Unset,
            LabelSelector::ByValue(v) => crate::LabelSelector::Eq(v.to_string()),
        },
    )
}

#[cfg(test)]
mod test {
    use std::collections::{BTreeMap, BTreeSet};

    use crate::generate::{
        algo::get_labelsets,
        label_map::{LabelName, LabelSelector, LabelValue},
    };

    use super::{get_choices, NAME};

    #[test]
    fn choices_by_presence() {
        let label1 = LabelName::new("label1");
        let label2 = LabelName::new("label2");
        let lms = [
            BTreeMap::from_iter([
                (NAME.clone(), LabelValue::new("metric")),
                (label1.clone(), LabelValue::new("a")),
                (label2.clone(), LabelValue::new("a")),
            ]),
            BTreeMap::from_iter([
                (NAME.clone(), LabelValue::new("metric")),
                (label1.clone(), LabelValue::new("a")),
                (label2.clone(), LabelValue::new("b")),
            ]),
            BTreeMap::from_iter([
                (NAME.clone(), LabelValue::new("metric")),
                (label1.clone(), LabelValue::new("a")),
            ]),
        ];
        let mask = BTreeSet::from_iter([&label1, &label2]);
        let by_value = BTreeSet::from_iter([]);
        let lms = get_labelsets(lms.iter(), &by_value);
        let cs = get_choices(&lms, mask);

        assert_eq!(
            &cs,
            &[
                (
                    1,
                    Vec::from_iter([BTreeMap::from_iter([(
                        &label1,
                        LabelSelector::ByPresence(true),
                    )])]),
                ),
                (
                    1,
                    Vec::from_iter([
                        BTreeMap::from_iter([(&label2, LabelSelector::ByPresence(false),)]),
                        BTreeMap::from_iter([(&label2, LabelSelector::ByPresence(true),)])
                    ]),
                )
            ],
        );
    }

    #[test]
    fn choices_by_value() {
        let label1 = LabelName::new("label1");
        let label2 = LabelName::new("label2");
        let lms = [
            BTreeMap::from_iter([
                (NAME.clone(), LabelValue::new("metric")),
                (label1.clone(), LabelValue::new("a")),
                (label2.clone(), LabelValue::new("a")),
            ]),
            BTreeMap::from_iter([
                (NAME.clone(), LabelValue::new("metric")),
                (label1.clone(), LabelValue::new("a")),
                (label2.clone(), LabelValue::new("b")),
            ]),
            BTreeMap::from_iter([
                (NAME.clone(), LabelValue::new("metric")),
                (label1.clone(), LabelValue::new("a")),
            ]),
        ];
        let mask = BTreeSet::from_iter([&label1, &label2]);
        let by_value = BTreeSet::from_iter([&label2]);
        let lms = get_labelsets(lms.iter(), &by_value);
        let cs = get_choices(&lms, mask);

        assert_eq!(
            &cs,
            &[
                (
                    1,
                    Vec::from_iter([BTreeMap::from_iter([(
                        &label1,
                        LabelSelector::ByPresence(true),
                    )])]),
                ),
                (
                    1,
                    Vec::from_iter([
                        BTreeMap::from_iter([(&label2, LabelSelector::ByPresence(false),)]),
                        BTreeMap::from_iter([(
                            &label2,
                            LabelSelector::ByValue(&LabelValue::new("a")),
                        )]),
                        BTreeMap::from_iter([(
                            &label2,
                            LabelSelector::ByValue(&LabelValue::new("b")),
                        )])
                    ]),
                )
            ],
        );
    }
}
