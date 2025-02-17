/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::collections::BTreeSet;
use std::path::Path;
use std::{collections::BTreeMap, fmt::Write};

use reqwest::Client;

use prometheus_api::{GenericQueryResponse, Vector};
use prometheus_core::{MetricName, METRIC_LABEL};
use prometheus_schema::schema::{self, MetricType, Universe};
use prometheus_schema::{LabelSelector, MetricSelector};

pub fn index(schema_path: &Path) -> Result<String, std::fmt::Error> {
    let mut html = String::new();
    write!(html, "<html><body>")?;
    write!(
        html,
        "<h1>Browsing metrics for {}</h1>",
        schema_path.display()
    )?;
    write!(html, "<ul>")?;
    write!(html, "<li><a href=\"tree\">Tree</a></li>")?;
    write!(html, "<li><a href=\"interface\">Interfaces</a></li>")?;
    // write!(html, "<li><a href=\"metrics\">Metrics</a></li>")?;
    write!(html, "</ul>")?;
    write!(html, "</body></html>")?;
    Ok(html)
}

pub async fn tree(
    schema: &Universe,
    client: &Client,
    url: &str,
    path: &[&str],
) -> Result<String, std::fmt::Error> {
    let root = schema.root_branch();

    let full_path = std::iter::once(match root {
        schema::Branch::Item(name, _) => name.to_string(),
        schema::Branch::Metric(name, _) => name.to_string(),
    })
    .chain(path.iter().map(|s| s.to_string()))
    .collect::<Vec<_>>();
    let full_path = full_path.iter().map(|s| s.as_str()).collect::<Vec<_>>();
    let full_path = full_path.as_slice();

    let item = path
        .iter()
        .copied()
        .try_fold(root, |item, name| item.walk(schema, name));

    let mut html = String::new();

    match item {
        Some(schema::Branch::Item(_name, item)) => {
            write!(html, "<h1>Path: /{}</h1>", path.join("/"))?;

            if !path.is_empty() {
                write!(html, "<h2>Parents</h2>",)?;
                write!(html, "<ul>",)?;
                path.iter().enumerate().try_for_each(|(i, parent)| {
                    write!(
                        html,
                        "<li><a href=\"/tree/{}\">{parent}</a></li>",
                        path[..=i].join("/")
                    )
                })?;
                write!(html, "</ul>",)?;
            }

            let all_items = item.items(schema).collect::<BTreeMap<_, _>>();

            if !all_items.is_empty() {
                write!(html, "<h2>Children</h2>",)?;
                write!(html, "<ul>",)?;
                all_items.keys().try_for_each(|name| {
                    write!(
                        html,
                        "<li><a href=\"/tree/{}/{}\">{name}</a></li>",
                        path.join("/"),
                        name
                    )
                })?;
                write!(html, "</ul>",)?;
            }

            let query = schema
                .gather(
                    full_path,
                    BTreeMap::from_iter([(
                        METRIC_LABEL.clone(),
                        LabelSelector::In(item.names(schema)),
                    )]),
                    |mut map, branch| {
                        if let schema::Branch::Item(_, item) = branch {
                            map.extend(
                                item.labels()
                                    .map(|(label, selector)| (label.clone(), selector.clone())),
                            );
                        };
                        map
                    },
                )
                .unwrap();

            if !query.is_empty() {
                write!(html, "<h2>Query</h2>",)?;
                write!(html, "<ul>",)?;
                query
                    .iter()
                    .try_for_each(|(name, selector)| write!(html, "<li>{name}{selector}</li>"))?;
                write!(html, "</ul>",)?;
            }

            if !item.assert.is_empty() {
                write!(html, "<h2>Asserts</h2>",)?;
                write!(html, "<ul>",)?;
                item.assert
                    .iter()
                    .try_for_each(|(name, selector)| write!(html, "<li>{name}{selector}</li>"))?;
                write!(html, "</ul>",)?;
            }

            let keys = schema
                .gather(full_path, BTreeSet::new(), |mut keys, branch| {
                    match branch {
                        schema::Branch::Item(_, item) => {
                            keys.extend(item.keys());
                            // keys.extend(item.all_labels(schema).map(|(label, _)| label))
                        }
                        schema::Branch::Metric(_, _) => {}
                    }
                    keys
                })
                .unwrap();

            if !keys.is_empty() {
                write!(html, "<h2>Keys</h2>",)?;
                write!(html, "<ul>",)?;
                keys.iter()
                    .try_for_each(|key| write!(html, "<li>{key}</li>"))?;
                write!(html, "</ul>",)?;
            }

            let meta = item
                .metrics
                .iter()
                .flat_map(|(name, metric)| metric.labels().map(move |(label, _)| (name, label)))
                .collect::<Vec<_>>();

            if !meta.is_empty() {
                write!(html, "<h2>Metadata</h2>",)?;
                write!(html, "<ul>",)?;
                meta.iter().try_for_each(|(metric, label)| {
                    write!(html, "<li>{label} (from {metric})</li>")
                })?;
                write!(html, "</ul>",)?;
            }

            let all_metrics = item.metrics().collect::<BTreeMap<_, _>>();

            if !all_metrics.is_empty() {
                write!(html, "<h2>Metrics</h2>",)?;
                write!(html, "<ul>",)?;
                all_metrics.iter().try_for_each(|(name, _)| {
                    write!(
                        html,
                        "<li><a href=\"/tree/{}/{}\">{name}</a></li>",
                        path.join("/"),
                        name
                    )
                })?;
                write!(html, "</ul>",)?;

                let metrics_query = query
                    .iter()
                    .map(|(label, selector)| (label.clone(), selector.clone()))
                    .chain([(
                        METRIC_LABEL.clone(),
                        LabelSelector::In(
                            all_metrics
                                .iter()
                                .flat_map(|(name, metric)| metric.names(name))
                                .collect(),
                        ),
                    )])
                    .collect::<MetricSelector>();

                let (data, _labels) = client
                    .post(url)
                    .form(&BTreeMap::from_iter([("query", metrics_query.to_string())]))
                    .send()
                    .await
                    .unwrap()
                    .json::<GenericQueryResponse>()
                    .await
                    .unwrap()
                    .into_result()
                    .unwrap()
                    .data
                    .unwrap_vector()
                    .into_iter()
                    .fold(
                        (BTreeMap::new(), BTreeMap::new()),
                        |(mut data, mut labels), mut row| {
                            let name: MetricName =
                                row.metric.remove(&METRIC_LABEL).unwrap().parse().unwrap();
                            let keys = keys
                                .iter()
                                .map(|key| {
                                    ((*key).clone(), row.metric.remove(*key).unwrap_or_default())
                                })
                                .collect::<BTreeMap<_, _>>();
                            data.entry(keys.clone())
                                .or_insert_with(BTreeMap::new)
                                .insert(name.clone(), row.value);
                            labels
                                .entry(name)
                                .or_insert_with(BTreeMap::new)
                                .entry(keys)
                                .or_insert_with(BTreeSet::new)
                                .insert(row.metric);
                            (data, labels)
                        },
                    );

                // let duplicates = labels
                //     .iter()
                //     .filter(|(_, keys)| {
                //         keys.values().any(|labels| {
                //             labels.len() > 1
                //                 && !labels
                //                     .iter()
                //                     .flat_map(|labels| labels.keys())
                //                     .all(|label| label == "quantile" || label == "le")
                //         })
                //     })
                //     .collect::<BTreeMap<_, _>>();

                // if !duplicates.is_empty() {
                //     write!(html, "<h2>Duplicate keysets (missing keys?)</h2>",)?;
                //     write!(html, "<ul>",)?;
                //     duplicates.iter().try_for_each(|(metric, keys)| {
                //         write!(html, "<li>{metric}: <ul>",)?;
                //         keys.iter()
                //             .filter(|(_, labels)| labels.len() > 1)
                //             .try_for_each(|(keys, labels)| {
                //                 write!(html, "<li>{keys:?}<ul>",)?;
                //                 labels.iter().try_for_each(|labels| {
                //                     write!(html, "<li>{labels:?}</li>")
                //                 })?;
                //                 write!(html, "</ul></li>")
                //             })?;
                //         write!(html, "</ul></li>",)
                //     })?;
                //     write!(html, "</ul>",)?;
                // }

                write!(html, "<h2>Data</h2>",)?;
                write!(html, "<table>",)?;
                write!(html, "<tr>",)?;
                keys.iter()
                    .try_for_each(|key| write!(html, "<th>{key}</th>",))?;
                all_metrics
                    .keys()
                    .try_for_each(|metric| write!(html, "<th>{metric}</th>",))?;
                write!(html, "</tr>",)?;
                data.iter().try_for_each(|(row_keys, row_metrics)| {
                    write!(html, "<tr>",)?;
                    keys.iter().try_for_each(|key| {
                        write!(html, "<td>{}</td>", row_keys.get(*key).unwrap())
                    })?;
                    all_metrics
                        .iter()
                        .try_for_each(|(name, metric)| match metric.r#type {
                            MetricType::Scalar(_) => match row_metrics.get(*name) {
                                Some(Vector::Value(v)) => {
                                    write!(html, "<td>{}</td>", v.value.0)
                                }
                                Some(Vector::Histogram(_)) => {
                                    write!(html, "<td>(histogram!?)</td>",)
                                }
                                None => write!(html, "<td></td>",),
                            },
                            MetricType::Histogram(_) | MetricType::Summary(_) => {
                                write!(html, "<td>sum: ")?;
                                match row_metrics.get(format!("{name}_sum").as_str()) {
                                    Some(Vector::Value(v)) => {
                                        write!(html, "{}", v.value.0)?;
                                    }
                                    Some(Vector::Histogram(_)) => {
                                        write!(html, "(histogram!?)>",)?;
                                    }
                                    None => {}
                                }
                                write!(html, ",<br />count: ")?;
                                match row_metrics.get(format!("{name}_count").as_str()) {
                                    Some(Vector::Value(v)) => {
                                        write!(html, "{}", v.value.0)?;
                                    }
                                    Some(Vector::Histogram(_)) => {
                                        write!(html, "(histogram!?)",)?;
                                    }
                                    None => {}
                                }
                                write!(html, "</td>")
                            }
                        })?;
                    write!(html, "</tr>",)
                })?;
                write!(html, "</table>",)?;

                let children = client
                    .post(url)
                    .form(&BTreeMap::from_iter([(
                        "query",
                        query
                            .iter()
                            .map(|(label, selector)| (label.clone(), selector.clone()))
                            .collect::<MetricSelector>()
                            .to_string(),
                    )]))
                    .send()
                    .await
                    .unwrap()
                    .json::<GenericQueryResponse>()
                    .await
                    .unwrap()
                    .into_result()
                    .unwrap()
                    .data
                    .unwrap_vector()
                    .into_iter()
                    .fold(BTreeMap::new(), |mut children, row| {
                        let keys = keys
                            .iter()
                            .map(|key| {
                                (
                                    key.to_string(),
                                    row.metric.get(*key).map_or("", |s| s.as_str()).to_string(),
                                )
                            })
                            .collect::<BTreeMap<_, _>>();
                        let item_children: &mut BTreeMap<
                            String,
                            BTreeSet<BTreeMap<String, String>>,
                        > = children.entry(keys).or_default();
                        all_items.iter().for_each(|(name, item)| {
                            if item.labels().all(|(label, selector)| {
                                selector.matches(row.metric.get(label).map_or("", |s| s.as_str()))
                            }) {
                                item_children.entry(name.to_string()).or_default().insert(
                                    item.labels()
                                        .map(|(label, _)| {
                                            (
                                                label.to_string(),
                                                row.metric
                                                    .get(label)
                                                    .map_or("", |s| s.as_str())
                                                    .to_string(),
                                            )
                                        })
                                        .collect::<BTreeMap<_, _>>(),
                                );
                            }
                        });
                        children
                    });

                write!(html, "<h2>Item tree</h2>")?;
                write!(html, "<ul>")?;
                children.iter().try_for_each(|(keys, item_children)| {
                    write!(html, "<li>{keys:?}:<ul>",)?;
                    item_children.iter().try_for_each(|(item, children)| {
                        write!(html, "<li>{item}: <ul>")?;
                        children
                            .iter()
                            .try_for_each(|child| write!(html, "<li>{child:?}</li>"))?;
                        write!(html, "</ul></li>")
                    })?;
                    write!(html, "</ul></li>")
                })?;
                write!(html, "</ul>")?;
            }
        }
        Some(schema::Branch::Metric(name, metric)) => match metric.r#type {
            MetricType::Scalar(_) => write!(html, "<h1>Metric: {name}</h1>")?,
            MetricType::Histogram(_) => write!(html, "<h1>Histogram: {name}</h1>",)?,
            MetricType::Summary(_) => write!(html, "<h1>Summary: {name}</h1>",)?,
        },
        None => {
            write!(html, "Not found").unwrap();
        }
    }
    write!(html, "</body></html>").unwrap();

    Ok(html)
}
