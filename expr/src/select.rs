/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use serde::{Deserialize, Serialize};

use crate::Expr;

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
#[serde(rename_all = "snake_case")]
pub enum SelectItem {
    Top { n: u64 },
    Bottom { n: u64 },
}

#[cfg(feature = "api")]
pub trait PromSelect {
    fn select(&self, select: &SelectItem, query: Expr) -> Expr;
}

#[cfg(feature = "api")]
impl PromSelect for prometheus_api::RangeQueryParams {
    fn select(&self, select: &SelectItem, query: Expr) -> Expr {
        let range = crate::PromDuration::zero()
            .with_milliseconds((self.end - self.start).num_milliseconds() as u64)
            .unwrap_or(crate::PromDuration::MAX);
        let end = ordered_float::OrderedFloat(self.end.timestamp_micros() as f64 / 1_000_000.0);
        let filter = select.select(
            query
                .clone()
                .queried_over_range_at(range, None, end)
                .sum_over_time(),
        );
        query.and(filter)
    }
}

#[cfg(feature = "api")]
impl PromSelect for prometheus_api::InstantQueryParams {
    fn select(&self, select: &SelectItem, query: Expr) -> Expr {
        select.select(query)
    }
}

impl SelectItem {
    pub fn select(&self, query: Expr) -> Expr {
        match self {
            SelectItem::Top { n } => query.topk(*n),
            SelectItem::Bottom { n } => query.bottomk(*n),
        }
    }
}
