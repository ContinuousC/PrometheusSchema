/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet};

use prometheus_api::GenericMetric;
use prometheus_core::{LabelName, MetricName};

use crate::ids::{ItemName, ItemRef, ModuleVersion};
use crate::query::{LabelSelector, MetricSelector};
use crate::serial::{self, Item, Metrics, Module, ScalarType};

use super::itemname::GenItemName;
use super::labelmap::LabelMap;
use super::labelset::{LabelSet, QUANTILE_LABELSET, SPECIAL_LABELSET};
use super::{GenInfo, Hints};

struct Metric<'a> {
    name: &'a str,
    labels: LabelMap<'a>,
}

pub fn generate_schema<T>(
    values: &[GenericMetric<T>],
    geninfo: &GenInfo,
) -> (Module, BTreeMap<ItemName, ItemName>) {
    let mut items = BTreeMap::new();
    let mut renames = BTreeMap::new();

    let root_name = GenItemName::Root;

    let metrics = values.iter().map(|m| Metric::new(m)).collect::<Vec<_>>();
    let common = metrics
        .iter()
        .map(|m| m.labels.labelset() - &SPECIAL_LABELSET)
        .reduce(|a, b| a & &b)
        .unwrap_or_default();

    // let (ms, rest) = get_metrics(&metrics, &common, &unset);
    let (metrics, subitems) = get_items_for(
        &root_name,
        &metrics,
        &common,
        &geninfo.hints,
        &mut items,
        &mut renames,
    );

    let root = Item {
        query: common
            .iter()
            .filter(|label| !geninfo.query.0.contains_key(*label))
            .map(|label| (label.clone(), LabelSelector::Set))
            .collect(),
        assert: MetricSelector::default(),
        keys: BTreeSet::new(),
        items: subitems,
        metrics,
        fork: false,
    };

    items.insert(
        geninfo.hints.item_name(&root_name, Some(&mut renames)),
        root,
    );

    let module = Module {
        version: ModuleVersion(semver::Version::new(0, 1, 0)),
        requires: BTreeMap::new(),
        items,
    };

    (module, renames)
}

impl<'a> Metric<'a> {
    fn new<T>(m: &'a GenericMetric<T>) -> Self {
        let mut labels = m
            .metric
            .iter()
            .map(|(k, v)| (k, v.as_str()))
            .collect::<LabelMap>();
        let name = labels.remove("__name__").unwrap();
        Self { name, labels }
    }
}

fn get_metrics<'a>(
    ms: impl IntoIterator<Item = &'a Metric<'a>>,
    common: &LabelSet<'a>,
) -> (BTreeMap<MetricName, serial::Metric>, Vec<&'a Metric<'a>>) {
    let labels = ms.into_iter().fold(
        BTreeMap::<LabelSet<'a>, BTreeMap<&'a str, Vec<&'a Metric<'a>>>>::new(),
        |mut ns, m| {
            let non_common_labels = m.labels.labelset_excluding(common);
            ns.entry(non_common_labels)
                .or_default()
                .entry(m.name)
                .or_default()
                .push(m);
            ns
        },
    );

    let mut metrics = BTreeMap::new();
    let mut rest = Vec::new();

    for (ls, ms) in &labels {
        if ls.is_empty() {
            for name in ms.keys() {
                if let Some(base) = name
                    .strip_suffix("_sum")
                    .filter(|base| ms.contains_key(format!("{base}_count").as_str()))
                {
                    if labels
                        .get(&QUANTILE_LABELSET)
                        .map_or(false, |ms| ms.contains_key(base))
                    {
                        metrics.insert(
                            MetricName::new(base.to_string()).unwrap(),
                            serial::Metric::Summary(serial::Summary {
                                labels: MetricSelector::new(),
                                unit: None,
                            }),
                        );
                    } else {
                        metrics.insert(
                            MetricName::new(base.to_string()).unwrap(),
                            serial::Metric::Histogram(serial::Histogram {
                                labels: MetricSelector::new(),
                                unit: None,
                            }),
                        );
                    }
                } else if !name.strip_suffix("_count").map_or(false, |base| {
                    ms.contains_key(format!("{base}_sum").as_str())
                }) {
                    metrics.insert(
                        MetricName::new(name.to_string()).unwrap(),
                        serial::Metric::Scalar(serial::Scalar {
                            labels: MetricSelector::new(),
                            r#type: ScalarType::default(),
                            unit: None,
                        }),
                    );
                }
            }
        } else if !SPECIAL_LABELSET.is_superset(ls) {
            rest.extend(ms.values().flatten());
        }
    }

    (metrics, rest)
}

fn get_items_for<'a>(
    parent: &GenItemName,
    metrics: impl IntoIterator<Item = &'a Metric<'a>>,
    common: &LabelSet<'a>,
    hints: &Hints,
    items: &mut BTreeMap<ItemName, Item>,
    renames: &mut BTreeMap<ItemName, ItemName>,
) -> (Option<Metrics>, BTreeSet<ItemRef>) {
    let orig_name = ItemName::new(parent.to_string());
    let split_by = hints
        .split_by
        .get(&orig_name)
        .map_or_else(|| Cow::Owned(BTreeSet::new()), Cow::Borrowed);

    if !split_by.is_empty() {
        let subitems = get_split_items(parent, &split_by, metrics, common, hints, items, renames);
        (None, subitems)
    } else {
        let (metrics, rest) = get_metrics(metrics, common);
        let subitems = get_excl_set_items(parent, rest, common, hints, items, renames);
        let metrics = (!metrics.is_empty()).then(|| {
            let mut set_labels = subitems
                .iter()
                .map(|item_ref| items.get(&item_ref.item).unwrap())
                .map(|item| {
                    item.query
                        .0
                        .iter()
                        .filter_map(|(label, selector)| {
                            (selector.excludes(&LabelSelector::Unset)).then_some(label)
                        })
                        .collect::<BTreeSet<_>>()
                })
                .collect::<Vec<_>>();
            let mut unset_labels = BTreeSet::new();
            while !set_labels.is_empty() {
                if let Some(most_used) = set_labels
                    .iter()
                    .flatten()
                    .copied()
                    .fold(BTreeMap::new(), |mut cs, label| {
                        *cs.entry(label).or_insert(0usize) += 1;
                        cs
                    })
                    .into_iter()
                    .max_by_key(|(_, c)| *c)
                    .map(|(label, _)| label)
                {
                    unset_labels.insert(most_used);
                    set_labels.retain(|labels| !labels.contains(most_used));
                } else {
                    break;
                }
            }

            let query = unset_labels
                .iter()
                .copied()
                .map(|label| (label.clone(), LabelSelector::Unset))
                .collect();

            Metrics {
                query,
                assert: MetricSelector::new(),
                metrics,
            }
        });

        (metrics, subitems)
    }
}

fn get_split_items<'a>(
    parent: &GenItemName,
    labels: &BTreeSet<LabelName>,
    ms: impl IntoIterator<Item = &'a Metric<'a>>,
    common: &LabelSet<'a>,
    hints: &Hints,
    items: &mut BTreeMap<ItemName, Item>,
    renames: &mut BTreeMap<ItemName, ItemName>,
) -> BTreeSet<ItemRef> {
    let by_value: BTreeMap<_, Vec<_>> = ms.into_iter().fold(BTreeMap::new(), |mut map, m| {
        let key = labels
            .iter()
            .map(|label| (label, m.labels.get(label).unwrap_or("")))
            .collect::<LabelMap>();
        map.entry(key).or_default().push(m);
        map
    });

    let mut subitems = BTreeSet::new();
    by_value.into_iter().for_each(|(key, metrics)| {
        //let common = LabelSet(common.0.iter().copied().chain(labels.as_str())).collect());

        let gen_name = GenItemName::ByValues(parent, &key);
        let item_name = hints.item_name(&gen_name, Some(renames));

        let (metrics, subitems2) = get_items_for(&gen_name, metrics, common, hints, items, renames);

        assert!(
            !items.contains_key(&item_name),
            "item name collision for {item_name} (orig = {gen_name})"
        );

        subitems.insert(ItemRef {
            module: None,
            item: item_name.clone(),
        });

        items.insert(
            item_name,
            Item {
                query: MetricSelector(
                    key.iter()
                        .map(|(label, value)| {
                            (
                                LabelName::new(label.to_string()).unwrap(),
                                LabelSelector::Eq(value.to_string()),
                            )
                        })
                        .collect(),
                ),
                assert: MetricSelector::new(),
                keys: BTreeSet::new(),
                items: subitems2,
                metrics,
                fork: false,
            },
        );
    });

    subitems
}

fn get_excl_set_items<'a>(
    parent: &GenItemName,
    ms: impl IntoIterator<Item = &'a Metric<'a>>,
    common: &LabelSet<'a>,
    hints: &Hints,
    items: &mut BTreeMap<ItemName, Item>,
    renames: &mut BTreeMap<ItemName, ItemName>,
) -> BTreeSet<ItemRef> {
    let labelsets = ms.into_iter().fold(
        BTreeMap::<LabelSet<'a>, Vec<&'a Metric<'a>>>::new(),
        |mut ns, m| {
            let non_common_labels = m.labels.labelset() - common;
            ns.entry(non_common_labels).or_default().push(m);
            ns
        },
    );

    let mut roots: BTreeMap<
        LabelSet<'a>,
        BTreeMap<
            LabelSet<'a>,
            (
                Vec<&'a Metric<'a>>,
                BTreeMap<LabelSet<'a>, Vec<&'a Metric<'a>>>,
            ),
        >,
    > = BTreeMap::new();

    labelsets.into_iter().for_each(|(labels, metrics)| {
        if let Some((root, children)) = roots
            .iter_mut()
            .find(|(root, _)| !root.is_disjoint(&labels))
        {
            if let Some((_, (_, children))) = children
                .iter_mut()
                .find(|(parent, _)| parent.is_subset(&labels))
            {
                children.insert(labels, metrics);
            } else {
            }
        } else {
            roots.insert(
                labels.clone(),
                BTreeMap::from_iter([(labels, (metrics, BTreeMap::new()))]),
            );
        }
    });

    // ms.into_iter().for_each(|m| {
    //     let non_common_labels = m.labels.labelset() - common;
    //     if let Some((parent, (metrics,))) = roots
    //         .iter_mut()
    //         .find(|(labels, _)| labels.is_strict_subset(non_common_labels))
    //     {
    //     } else {
    //         let children = roots
    //             .keys()
    //             .filter(|labels| labels.is_strict_superset(non_common_labels))
    //             .collect()
    //             .into_iter()
    //             .filter_map(|labels| roots.remove_entry(labels))
    //             .collect();
    //     }
    // });

    // let (roots, mut subs) = labels.iter().partition::<BTreeMap<_, _>, _>(|(ls, _)| {
    //     !labels.iter().any(|(other, _)| ls.is_strict_superset(other))
    // });

    let islands = roots.into_iter().fold(
        BTreeMap::<LabelSet, BTreeMap<&LabelSet, _>>::new(),
        |map, (ls, ms)| {
            let (mut disjoint, joint) = map
                .into_iter()
                .partition::<BTreeMap<_, _>, _>(|(ks, _)| ls.is_disjoint(ks));
            disjoint.insert(
                joint.keys().fold(ls.clone(), |ls, ks| ls | &ks),
                joint
                    .into_values()
                    .flatten()
                    .chain(std::iter::once((ls, ms)))
                    .collect(),
            );
            disjoint
        },
    );

    eprintln!("{}: found {} islands", parent, islands.len());

    let all_labels = islands.keys().fold(LabelSet::new(), |a, b| a | b);

    let mut subitems = BTreeSet::new();
    islands.iter().for_each(|(ils, roots)| {
        // let common = LabelSet(&common.0 | &ls.0);

        let gen_name = GenItemName::ByLabels(parent, ils);
        let item_name = hints.item_name(&gen_name, Some(renames));

        let common_labels = roots.keys().fold(LabelSet::new(), |ls, ks| ls & ks);

        let subitems2 = roots
            .iter()
            .fold(BTreeSet::new(), |mut subitems, (ls, ms)| {
                let non_common_labels = (*ls).clone() - &common_labels;

                let gen_name = GenItemName::ByLabels(&gen_name, &non_common_labels);
                let item_name = hints.item_name(&gen_name, Some(renames));

                let (ours, theirs) = subs
                    .iter()
                    .partition(|(other, _)| ls.is_strict_subset(other));
                subs = theirs;

                let sub_metrics = ms
                    .iter()
                    .copied()
                    .chain(ours.into_iter().flat_map(|(_, ms)| ms.iter().copied()))
                    .collect::<Vec<_>>();

                let common = common.clone() | &all_labels;
                let (metrics, new_subitems) =
                    get_items_for(&gen_name, sub_metrics, &common, hints, items, renames);

                subitems.insert(ItemRef::new(None, item_name.clone()));
                items.insert(
                    item_name,
                    Item {
                        query: (ils.clone() - *ls)
                            .iter()
                            .map(|label| (label.clone(), LabelSelector::Unset))
                            .chain(
                                non_common_labels
                                    .iter()
                                    .map(|label| (label.clone(), LabelSelector::Set)),
                            )
                            .collect(),
                        assert: MetricSelector::new(),
                        keys: BTreeSet::new(),
                        items: new_subitems,
                        metrics,
                        fork: false,
                    },
                );

                subitems
            });

        assert!(
            !items.contains_key(&item_name),
            "item name collision for {item_name} (orig = {gen_name})"
        );

        subitems.insert(ItemRef {
            module: None,
            item: item_name.clone(),
        });

        items.insert(
            item_name,
            Item {
                query: MetricSelector(
                    all_labels
                        .iter()
                        .filter(|label| !ils.contains(label))
                        .map(|label| (label.clone(), LabelSelector::Unset))
                        .chain(
                            common_labels
                                .iter()
                                .map(|label| (label.clone(), LabelSelector::Set)),
                        )
                        .collect(),
                ),
                assert: MetricSelector::default(),
                keys: ils
                    .iter()
                    .map(|label| LabelName::new(label.to_string()).unwrap())
                    .collect(),
                items: subitems2,
                metrics: None,
                fork: false,
            },
        );
    });

    subitems
}
