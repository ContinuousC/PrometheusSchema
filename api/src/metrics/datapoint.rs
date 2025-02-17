/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

/// A datapoint (timestamp and value) as returned by prometheus.
#[derive(Serialize, Deserialize, PartialEq, Eq, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(type = "[number, T]"))]
#[serde(
    try_from = "(f64, T)",
    into = "(f64, T)",
    bound(serialize = "T: Serialize + Clone")
)]
pub struct DataPoint<T> {
    pub timestamp: DateTime<Utc>,
    pub value: T,
}

/// A datapoint represented as an object with keys.
#[derive(Serialize, Deserialize, PartialEq, Eq, Clone, Debug)]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
pub struct DataPoint2<T> {
    pub timestamp: DateTime<Utc>,
    pub value: T,
}

impl<T> From<DataPoint<T>> for DataPoint2<T> {
    fn from(value: DataPoint<T>) -> Self {
        DataPoint2 {
            timestamp: value.timestamp,
            value: value.value,
        }
    }
}

impl<T> From<DataPoint2<T>> for DataPoint<T> {
    fn from(value: DataPoint2<T>) -> Self {
        DataPoint {
            timestamp: value.timestamp,
            value: value.value,
        }
    }
}

impl<T> TryFrom<(f64, T)> for DataPoint<T> {
    type Error = ParseDataPointError;
    fn try_from((ts, value): (f64, T)) -> Result<Self, Self::Error> {
        Ok(Self {
            timestamp: DateTime::from_timestamp(
                ts.floor() as i64,
                (ts.fract().abs() * 1000000000.0) as u32,
            )
            .ok_or(ParseDataPointError::InvalidTimestamp(ts))?,
            value,
        })
    }
}

impl<T> From<DataPoint<T>> for (f64, T) {
    fn from(val: DataPoint<T>) -> Self {
        let ts = val.timestamp.timestamp() as f64
            + val.timestamp.timestamp_subsec_nanos() as f64 / 1000000000.0;
        (ts, val.value)
    }
}

#[derive(thiserror::Error, Debug)]
pub enum ParseDataPointError {
    #[error("failed to decode timestamp")]
    InvalidTimestamp(f64),
}
