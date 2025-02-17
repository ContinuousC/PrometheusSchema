/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::fmt::Display;

use serde::{Deserialize, Serialize};

use crate::{
    response::Response, DataPoint, DataPoint2, GenericLabels, Matrix, Metric, Value, Vector,
};

/// Response to the prometheus query api endpoint.
pub type GenericQueryResponse = QueryResponse<GenericLabels>;

/// Response to the prometheus query api endpoint.
pub type QueryResponse<K> = Response<QueryResult<K>>;

/// Query result data as returned by prometheus.
#[derive(Serialize, Deserialize, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[serde(tag = "resultType", content = "result", rename_all = "camelCase")]
pub enum QueryResult<K = GenericLabels> {
    Matrix(Vec<Metric<K, Matrix>>),
    Vector(Vec<Metric<K, Vector>>),
    Scalar(DataPoint<Value>),
    String(DataPoint<String>),
}

/// Query result type.
#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub enum QueryResultType {
    Matrix,
    Vector,
    Scalar,
    String,
}

impl<K> QueryResult<K> {
    pub fn get_type(&self) -> QueryResultType {
        match self {
            QueryResult::Matrix(_) => QueryResultType::Matrix,
            QueryResult::Vector(_) => QueryResultType::Vector,
            QueryResult::Scalar(_) => QueryResultType::Scalar,
            QueryResult::String(_) => QueryResultType::String,
        }
    }

    /// Get a list of label maps identifying the metric series returned.
    pub fn labels(&self) -> Vec<&K> {
        match self {
            QueryResult::Matrix(metrics) => metrics.iter().map(|metric| &metric.metric).collect(),
            QueryResult::Vector(metrics) => metrics.iter().map(|metric| &metric.metric).collect(),
            QueryResult::Scalar(_) => Vec::new(),
            QueryResult::String(_) => Vec::new(),
        }
    }

    /// Return the vector data or panic.
    pub fn unwrap_vector(self) -> Vec<Metric<K, Vector>> {
        match self {
            QueryResult::Vector(data) => data,
            _ => panic!("unwrap_vector called on a non-vector QueryResult"),
        }
    }

    /// Return the vector data or panic.
    pub fn unwrap_matrix(self) -> Vec<Metric<K, Matrix>> {
        match self {
            QueryResult::Matrix(data) => data,
            _ => panic!("unwrap_matrix called on a non-matrix QueryResult"),
        }
    }

    /// Return the scalar data or panic.
    pub fn unwrap_scalar(self) -> DataPoint<Value> {
        match self {
            QueryResult::Scalar(data) => data,
            _ => panic!("unwrap_scalar called on a non-scalar QueryResult"),
        }
    }

    /// Return the string data or panic.
    pub fn unwrap_string(self) -> DataPoint<String> {
        match self {
            QueryResult::String(data) => data,
            _ => panic!("unwrap_string called on a non-string QueryResult"),
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[serde(rename_all = "snake_case")]
pub enum ScalarOr<T> {
    Scalar(Value),
    Series(T),
}

impl<K, T> TryFrom<QueryResult<K>> for ScalarOr<T>
where
    T: TryFrom<QueryResult<K>>,
{
    type Error = T::Error;
    fn try_from(res: QueryResult<K>) -> Result<Self, Self::Error> {
        match res {
            QueryResult::Scalar(v) => Ok(ScalarOr::Scalar(v.value)),
            _ => res.try_into().map(ScalarOr::Series),
        }
    }
}

impl<K> TryFrom<QueryResult<K>> for Vec<Metric<K, DataPoint2<Value>>> {
    type Error = DecodeQueryError;
    fn try_from(res: QueryResult<K>) -> Result<Self, Self::Error> {
        match res {
            QueryResult::Vector(ms) => ms
                .into_iter()
                .map(|Metric { metric, value }| match value {
                    Vector::Value(value) => Ok(Metric {
                        metric,
                        value: value.into(),
                    }),
                    Vector::Histogram(_) => Err(DecodeQueryError::UnexpectedHistogram),
                })
                .collect(),
            r => Err(DecodeQueryError::UnexpectedResultType(
                QueryResultType::Vector,
                r.get_type(),
            )),
        }
    }
}

impl<K> TryFrom<QueryResult<K>> for Vec<Metric<K, Vec<DataPoint2<Value>>>> {
    type Error = DecodeQueryError;
    fn try_from(res: QueryResult<K>) -> Result<Self, Self::Error> {
        match res {
            QueryResult::Matrix(ms) => ms
                .into_iter()
                .map(|Metric { metric, value }| match value {
                    Matrix::Values(value) => Ok(Metric {
                        metric,
                        value: value.into_iter().map(|v| v.into()).collect(),
                    }),
                    Matrix::Histograms(_) => Err(DecodeQueryError::UnexpectedHistogram),
                })
                .collect(),
            r => Err(DecodeQueryError::UnexpectedResultType(
                QueryResultType::Matrix,
                r.get_type(),
            )),
        }
    }
}

#[derive(thiserror::Error, Debug)]
pub enum DecodeQueryError {
    #[error("unexpected result type: expected {0}, got {1}")]
    UnexpectedResultType(QueryResultType, QueryResultType),
    #[error("unexpected result: native histogram")]
    UnexpectedHistogram,
}

impl Display for QueryResultType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            QueryResultType::Matrix => write!(f, "matrix"),
            QueryResultType::Vector => write!(f, "vector"),
            QueryResultType::Scalar => write!(f, "scalar"),
            QueryResultType::String => write!(f, "string"),
        }
    }
}
