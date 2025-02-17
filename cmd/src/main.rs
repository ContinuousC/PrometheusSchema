/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

mod server;

use std::{
    borrow::Cow,
    collections::{BTreeMap, BTreeSet, HashMap},
    fmt::Display,
    path::{Path, PathBuf},
    sync::{atomic::AtomicBool, Arc},
};

use clap::Parser;
use futures::StreamExt;
use reqwest::Client;
use serde::{Deserialize, Serialize};
use warp::{path::Tail, Filter};

use prometheus_api::{
    DataPoint2, GenericMetric, GenericQueryResponse, QueryResult, SuccessResponse, Value, Vector,
};
use prometheus_core::{MetricName, METRIC_LABEL};
use prometheus_schema::tree::make_query;
use prometheus_schema::{generate, schema};

#[derive(Parser)]
struct Args {
    #[clap(subcommand)]
    command: Command,
}

#[derive(clap::Subcommand)]
enum Command {
    VerifySchema(VerifySchema),
    VerifyMetrics(VerifyMetrics),
    AnalyzeMetrics(AnalyzeMetrics),
    GenerateSchema(GenerateSchema),
    QueryMetrics(QueryMetrics),
    SaveMetrics(SaveMetrics),
    Server(Server),
    // ToSchema2(ToSchema2),
}

#[derive(clap::Args)]
struct VerifySchema {
    schema: PathBuf,
    #[clap(long)]
    show_tree: bool,
    #[clap(long)]
    show_items: bool,
}

#[derive(clap::Args)]
struct VerifyMetrics {
    #[clap(long)]
    metrics: Option<PathBuf>,
    #[clap(long, env = "PROMETHEUS_URL", required_unless_present = "metrics")]
    url: Option<String>,
    #[clap(long)]
    tree: bool,
    schema: PathBuf,
    #[clap(required_unless_present = "metrics")]
    query: Option<String>,
}

#[derive(clap::Args)]
struct AnalyzeMetrics {
    #[clap(long)]
    metrics: Option<PathBuf>,
    #[clap(long, env = "PROMETHEUS_URL", required_unless_present = "metrics")]
    url: Option<String>,
    #[clap(required_unless_present = "metrics")]
    query: Option<String>,
}

#[derive(clap::Args)]
struct GenerateSchema {
    #[clap(long)]
    metrics: Option<PathBuf>,
    #[clap(long, env = "PROMETHEUS_URL", required_unless_present = "metrics")]
    url: Option<String>,
    #[clap(long, short, default_value = "2")]
    debug_depth: usize,
    #[clap(long, short, default_value = "2")]
    eval_depth: usize,
    generate: PathBuf,
}

#[derive(clap::Args)]
struct QueryMetrics {
    #[clap(long, env = "PROMETHEUS_URL")]
    url: String,
    schema: PathBuf,
    item: Option<String>,
}

#[derive(clap::Args)]
struct SaveMetrics {
    #[clap(long, env = "PROMETHEUS_URL")]
    url: String,
    #[clap(long)]
    output: PathBuf,
    query: String,
}

#[derive(clap::Args)]
struct Server {
    #[clap(long, default_value = "0.0.0.0:9999")]
    listen: String,
    #[clap(long, env = "PROMETHEUS_URL")]
    url: String,
    schema: PathBuf,
    query: String,
}
// #[derive(clap::Args)]
// struct ToSchema2 {
//     schema: PathBuf,
// }

#[tokio::main]
async fn main() {
    env_logger::init();
    let args = Args::parse();

    match &args.command {
        Command::VerifySchema(args) => {
            let schema = schema::Universe::load_async(&args.schema)
                .await
                .unwrap()
                .tree();

            match schema.verify() {
                Ok(()) => println!("Schema successfully verified."),
                Err(e) => panic!("Schema error: {}", e[0]),
            }

            if args.show_tree {
                println!("Tree:\n{}", serde_yaml::to_string(&schema).unwrap());
            }

            if args.show_items {
                println!(
                    "Items:\n{}",
                    serde_yaml::to_string(&schema.items()).unwrap()
                );
            }
        }
        Command::VerifyMetrics(args) => {
            let schema = if args.tree {
                serde_yaml::from_str(&tokio::fs::read_to_string(&args.schema).await.unwrap())
                    .unwrap()
            } else {
                schema::Universe::load_async(&args.schema)
                    .await
                    .unwrap()
                    .tree()
            };

            match schema.verify() {
                Ok(()) => println!("Schema successfully verified."),
                Err(e) => panic!("Schema error: {}", e[0]),
            }

            let metrics = get_metrics(
                args.url.as_deref(),
                args.metrics.as_deref(),
                args.query.as_deref(),
            )
            .await;
            println!("Running sieve...");
            let (errors, unmatched, unmatched_labels) = schema.sieve(&metrics);

            if !errors.is_empty() {
                println!("Errors:");
                errors.iter().for_each(|error| println!("- {error}"))
            }

            println!();
            println!("Matched {} metrics", metrics.len() - unmatched);

            if unmatched > 0 {
                println!("Found {unmatched} unmatched metrics");
            }

            if !unmatched_labels.is_empty() {
                println!(
                    "Unmatched labels: {}",
                    unmatched_labels.into_iter().collect::<Vec<_>>().join(", ")
                );
            }
        }
        Command::AnalyzeMetrics(args) => {
            let metrics = get_metrics(
                args.url.as_deref(),
                args.metrics.as_deref(),
                args.query.as_deref(),
            )
            .await;
            let info = MetricsInfo::from_query_result(&metrics);
            info.show();
        }
        Command::GenerateSchema(args) => {
            let geninfo = generate::GenInfo::load(&args.generate).await.unwrap();
            let metrics = get_metrics(
                args.url.as_deref(),
                args.metrics.as_deref(),
                Some(&geninfo.query.to_string()),
            )
            .await;

            let schema = generate::generate_schema(
                &metrics,
                &geninfo,
                &AtomicBool::new(false),
                args.debug_depth,
                args.eval_depth,
            )
            .unwrap();
            println!("{}", serde_yaml::to_string(&schema).unwrap());
        }
        Command::QueryMetrics(args) => {
            let schema = schema::Universe::load_async(&args.schema)
                .await
                .unwrap()
                .tree();
            match schema.verify() {
                Ok(()) => println!("Schema successfully verified."),
                Err(e) => panic!("Schema error: {}", e[0]),
            }
            // let client = query_client();
            let items = futures::stream::iter(
                schema
                    .items()
                    .into_iter()
                    .filter(|(name, _)| args.item.as_ref().map_or(true, |item| name == item)),
            )
            .map(move |(item_type, item_info)| async move {
                let items = futures::stream::iter(item_info.queries.clone())
                    .map(move |(path, labels)| {
                        let item_info = item_info.clone();
                        async move {
                            let client = query_client();
                            let result = run_query(&client, &args.url, &make_query(&labels)).await;
                            let items = match result.data {
                                QueryResult::Vector(values) => {
                                    values
                                        .into_iter()
                                        .fold(BTreeMap::new(), |mut items, value| {
                                            let metric = value
                                                .metric
                                                .get(&METRIC_LABEL)
                                                .unwrap()
                                                .parse()
                                                .unwrap();
                                            let keys = labels
                                                .iter()
                                                .map(|(item, labels)| {
                                                    (
                                                        item,
                                                        labels
                                                            .iter()
                                                            .filter(|(label, selector)| {
                                                                *label != &METRIC_LABEL
                                                                    && selector.is_many()
                                                            })
                                                            .map(|(label, _)| {
                                                                (label, value.metric.get(label))
                                                            })
                                                            .collect::<BTreeMap<_, _>>(),
                                                    )
                                                })
                                                .collect::<BTreeMap<_, _>>();
                                            let item: &mut BTreeMap<MetricName, Value> = items
                                                .entry(serde_yaml::to_string(&keys).unwrap())
                                                .or_default();
                                            if item_info.metrics.contains(&metric) {
                                                item.insert(
                                                    metric,
                                                    match value.value {
                                                        Vector::Value(v) => v.value,
                                                        Vector::Histogram(_) => {
                                                            todo!()
                                                        }
                                                    },
                                                );
                                            }
                                            items
                                        })
                                }
                                QueryResult::Matrix(_) => todo!(),
                                QueryResult::Scalar(_) => todo!(),
                                QueryResult::String(_) => todo!(),
                            };
                            (path, items)
                        }
                    })
                    .buffer_unordered(100)
                    .collect::<BTreeMap<_, _>>()
                    .await;
                (item_type, items)
            })
            .buffer_unordered(100)
            .collect::<BTreeMap<_, _>>()
            .await;
            println!("{}", serde_yaml::to_string(&items).unwrap());
        }
        Command::SaveMetrics(args) => {
            let metrics: Metrics = run_query(&query_client(), &args.url, &args.query)
                .await
                .data
                .try_into()
                .unwrap();
            tokio::fs::write(&args.output, serde_cbor::to_vec(&metrics).unwrap())
                .await
                .unwrap();
        }
        Command::Server(args) => {
            let schema = Arc::new(schema::Universe::load_async(&args.schema).await.unwrap());

            warp::serve(
                warp::path::end()
                    .map({
                        let path = args.schema.clone();
                        move || server::index(&path)
                    })
                    .or(warp::path::path("tree").and(warp::path::tail()).then({
                        let schema = schema.clone();
                        let url = args.url.clone();
                        move |path: Tail| {
                            let schema = schema.clone();
                            let url = url.clone();
                            async move {
                                let path = path
                                    .as_str()
                                    .split('/')
                                    .filter(|c| !c.is_empty())
                                    .collect::<Vec<_>>();
                                let client = query_client();
                                server::tree(&schema, &client, &url, &path).await
                            }
                        }
                    }))
                    .unify()
                    // .or(warp::path("interface")
                    //     .and(warp::path::param().and(warp::path::tail()).map({
                    //         let schema = oldschema.clone();
                    //         move |name: String| server::interface(&schema, &name)
                    //     }))
                    //     .unify())
                    // .unify()
                    // .or(warp::path("metrics")
                    //     .and(warp::path::tail())
                    //     .map(|path| Ok(format!("Metrics for {path:?} :D"))))
                    // .unify()
                    .map(|res: Result<String, std::fmt::Error>| {
                        res.map_or_else(
                            |e| warp::reply::html(format!("Error: {e}")),
                            warp::reply::html,
                        )
                    }),
            )
            .run(
                tokio::net::lookup_host(&args.listen)
                    .await
                    .unwrap()
                    .next()
                    .unwrap(),
            )
            .await
        } // Command::ToSchema2(args) => {
          //     let schema = oldschema::Module::load(&args.schema).await.to_schema2();
          //     println!("{}", serde_yaml::to_string(&schema).unwrap());
          // }
    }
}

fn query_client() -> Client {
    Client::builder()
        .danger_accept_invalid_certs(true)
        .timeout(std::time::Duration::from_secs(120))
        .build()
        .unwrap()
}

type Metrics = Vec<GenericMetric<DataPoint2<Value>>>;

async fn get_metrics(url: Option<&str>, metrics: Option<&Path>, query: Option<&str>) -> Metrics {
    if let Some(path) = metrics {
        let ext = path.extension().unwrap();
        if ext == "cbor" {
            serde_cbor::from_slice(&tokio::fs::read(path).await.unwrap()).unwrap()
        } else if ext == "json" {
            serde_json::from_slice(&tokio::fs::read(path).await.unwrap()).unwrap()
        } else {
            panic!("unknown extension for metrics file")
        }
    } else {
        run_query(&query_client(), url.unwrap(), query.unwrap_or("{}"))
            .await
            .data
            .try_into()
            .unwrap()
    }
}

async fn run_query(client: &Client, url: &str, query: &str) -> SuccessResponse<QueryResult> {
    client
        .post(url)
        .form::<HashMap<&str, &str>>(&HashMap::from_iter([("query", query)]))
        .send()
        .await
        .unwrap()
        .json::<GenericQueryResponse>()
        .await
        .unwrap()
        .into_result()
        .unwrap()
}

#[derive(Serialize, Deserialize)]
struct MetricsInfo<'a> {
    metrics: BTreeMap<Cow<'a, str>, BTreeSet<BTreeSet<Cow<'a, str>>>>,
    labels: BTreeMap<Cow<'a, str>, BTreeSet<Cow<'a, str>>>,
    labelsets: BTreeMap<BTreeSet<Cow<'a, str>>, BTreeSet<Cow<'a, str>>>,
}

impl<'a> MetricsInfo<'a> {
    fn from_query_result<T>(values: &'a [GenericMetric<T>]) -> Self {
        let cleaned = || {
            values
                .iter()
                .map(|metric| {
                    let name = Cow::Borrowed(metric.metric.get("__name__").unwrap().as_str());
                    let labels: BTreeMap<Cow<'a, str>, Cow<'a, str>> = metric
                        .metric
                        .iter()
                        .map(|(label, value)| {
                            (Cow::Borrowed(label.as_str()), Cow::Borrowed(value.as_str()))
                        })
                        .filter(|(label, _)| *label != "__name__")
                        .collect();
                    (name, labels)
                })
                .filter(|(name, _)| !name.ends_with("_sum") && !name.ends_with("_bucket"))
        };

        let metrics: BTreeMap<Cow<'a, str>, BTreeSet<BTreeSet<Cow<'a, str>>>> =
            cleaned().fold(BTreeMap::new(), |mut map, (name, labels)| {
                map.entry(name)
                    .or_default()
                    .insert(labels.keys().cloned().collect());
                map
            });

        let labels: BTreeMap<Cow<'a, str>, BTreeSet<Cow<'a, str>>> = cleaned()
            .flat_map(|(_, labels)| labels)
            .fold(BTreeMap::new(), |mut map, (label, value)| {
                map.entry(label).or_default().insert(value);
                map
            });

        let labelsets: BTreeMap<BTreeSet<Cow<'a, str>>, BTreeSet<Cow<'a, str>>> =
            cleaned().fold(BTreeMap::new(), |mut labelsets, (name, labels)| {
                labelsets
                    .entry(labels.keys().cloned().collect())
                    .or_default()
                    .insert(name);
                labelsets
            });

        Self {
            metrics,
            labels,
            labelsets,
        }
    }

    fn show(&self /*_schema: Option<&MetricsSchema>*/) {
        let mut ls = self.metrics.values().flatten();
        let init = ls.next().unwrap().clone();
        let common = ls.fold(init, |common, labels| {
            common.intersection(labels).cloned().collect()
        });

        println!("Metrics (common labels: {}):", ShowLabels(&common));

        self.metrics.iter().for_each(|(metric, labelsets)| {
            if labelsets.len() == 1 {
                println!("- {}", metric);
                println!("   + {}", ShowLabels(labelsets.iter().next().unwrap()));
            } else {
                let mut ls = labelsets.iter();
                let init = ls.next().unwrap().clone();
                let common1 = ls
                    .fold(init, |common, labels| {
                        common.intersection(labels).cloned().collect()
                    })
                    .difference(&common)
                    .cloned()
                    .collect();
                println!("- {} (common labels: {})", metric, ShowLabels(&common1));
                labelsets.iter().for_each(|labels| {
                    let diff = labels
                        .iter()
                        .filter(|label| !common.contains(*label) && !common1.contains(*label))
                        .cloned()
                        .collect();
                    println!("   + {}", ShowLabels(&diff));
                });
            }
        });

        println!();
        println!("Labels:");
        self.labels.iter().for_each(|(label, values)| {
            println!("- {} ({} values)", label, values.len());
            if values.len() < 10 {
                values.iter().for_each(|value| {
                    println!("   {}", value);
                });
            }
        });

        println!();
        println!("Label sets:");

        self.labelsets.iter().for_each(|(labels, metrics)| {
            println!("+ {}", ShowLabels(labels));
            metrics
                .iter()
                .for_each(|metric| println!("   - {}", metric));
        });

        println!();
        println!("Tree:");

        let common = BTreeSet::new();

        self.labelsets
            .iter()
            .filter(|(labels, _)| {
                !self.labelsets.keys().any(|other| {
                    other.len() < labels.len() && other.is_subset(labels)
                } /* is_strict_subset */)
            })
            .for_each(|(root, metrics)| {
                let subtree = self
                    .labelsets
                    .iter()
                    .filter(|(other, _)| root.len() < other.len() && root.is_subset(other))
                    .collect();
                show_tree(&common, root, metrics, &subtree, 0);
            });
    }
}

fn show_tree<'a>(
    common: &'a BTreeSet<Cow<'a, str>>,
    root: &'a BTreeSet<Cow<'a, str>>,
    metrics: &'a BTreeSet<Cow<'a, str>>,
    subtree: &'a BTreeMap<&'a BTreeSet<Cow<'a, str>>, &'a BTreeSet<Cow<'a, str>>>,
    level: usize,
) {
    (0..level).for_each(|_| print!("   "));
    let mut rs = root
        .iter()
        .filter(|label| !common.contains(*label))
        .cloned();
    rs.next()
        .into_iter()
        .for_each(|label| print!("+ {}", label));
    rs.for_each(|label| print!(", {}", label));
    println!();

    metrics.iter().for_each(|metric| {
        (0..level + 1).for_each(|_| print!("   "));
        println!("- {}", metric);
    });

    subtree
        .iter()
        .filter(|(labels,_)| {
            !subtree
                .keys()
                .any(|other| other.len() < labels.len() && other.is_subset(labels) /* is_strict_subset */)
        })
        .for_each(|(subroot,metrics)| {
			let subtree = subtree.iter().map(|(ls,ms)|(*ls,*ms)).filter(|(other,_)|subroot.len() < other.len() && subroot.is_subset(other)).collect();
			show_tree(root,subroot,metrics, &subtree, level + 1);
		});
}

struct ShowLabels<'a>(&'a BTreeSet<Cow<'a, str>>);

impl Display for ShowLabels<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut ls = self.0.iter();
        ls.next()
            .iter()
            .try_for_each(|label| write!(f, "{}", label))?;
        ls.try_for_each(|label| write!(f, ", {}", label))
    }
}
