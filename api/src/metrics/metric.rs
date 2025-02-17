/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::collections::BTreeMap;

use serde::{Deserialize, Serialize};

use prometheus_core::LabelName;

use crate::{DataPoint, Histogram, Value};

/// A metric with a generic label map.
pub type GenericMetric<T> = Metric<GenericLabels, T>;

/// A generic label map.
pub type GenericLabels = BTreeMap<LabelName, String>;

/// A metric value or series as returned by prometheus.
#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
pub struct Metric<K, V> {
    /// The label map identifying the metric.
    pub metric: K,
    /// The value of the metric.
    #[serde(flatten)]
    pub value: V,
}

/// A metric series as returned by prometheus.
#[derive(Serialize, Deserialize, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[serde(rename_all = "camelCase")]
pub enum Matrix {
    Values(Vec<DataPoint<Value>>),
    Histograms(Vec<DataPoint<Histogram>>),
}

/// A metric value as returned by prometheus.
#[derive(Serialize, Deserialize, PartialEq, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[serde(rename_all = "camelCase")]
pub enum Vector {
    Value(DataPoint<Value>),
    Histogram(DataPoint<Histogram>),
}

// pub trait FromQueryResult: DeserializeOwned {
//     type Output;
//     fn decode<K>(data: QueryResult<K>)
//         -> std::result::Result<Vec<Metric<K, Self>>, QueryResult<K>>;
//     fn extract(self) -> Option<Self::Output>;
// }

// impl FromQueryResult for Vector {
//     type Output = DataPoint<Value>;
//     fn decode<K>(
//         data: QueryResult<K>,
//     ) -> std::result::Result<Vec<Metric<K, Self>>, QueryResult<K>> {
//         match data {
//             QueryResult::Vector(rows) => Ok(rows),
//             data => Err(data),
//         }
//     }
//     fn extract(self) -> Option<Self::Output> {
//         match self {
//             Vector::Value(v) => Some(v),
//             Vector::Histogram(_) => None,
//         }
//     }
// }

// impl FromQueryResult for Matrix {
//     type Output = Vec<DataPoint<Value>>;
//     fn decode<K>(
//         data: QueryResult<K>,
//     ) -> std::result::Result<Vec<Metric<K, Self>>, QueryResult<K>> {
//         match data {
//             QueryResult::Matrix(rows) => Ok(rows),
//             data => Err(data),
//         }
//     }
//     fn extract(self) -> Option<Self::Output> {
//         match self {
//             Matrix::Values(vs) => Some(vs),
//             Matrix::Histograms(_) => None,
//         }
//     }
// }
