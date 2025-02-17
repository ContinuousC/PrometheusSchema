/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

use crate::sealed::Sealed;

pub trait PromQueryParams: Serialize + Sealed {
    const PATH: &'static str;
    fn get_step(&self) -> Option<f64>;
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[serde(untagged)]
pub enum QueryParams {
    Range(RangeQueryParams),
    Instant(InstantQueryParams),
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
pub struct InstantQueryParams {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub time: Option<DateTime<Utc>>,
}

#[derive(Serialize, Deserialize, PartialEq, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
pub struct RangeQueryParams {
    pub start: DateTime<Utc>,
    pub end: DateTime<Utc>,
    pub step: f64,
}

impl Sealed for InstantQueryParams {}
impl PromQueryParams for InstantQueryParams {
    const PATH: &'static str = "query";
    fn get_step(&self) -> Option<f64> {
        None
    }
}

impl Sealed for RangeQueryParams {}
impl PromQueryParams for RangeQueryParams {
    const PATH: &'static str = "query_range";
    fn get_step(&self) -> Option<f64> {
        Some(self.step)
    }
}
