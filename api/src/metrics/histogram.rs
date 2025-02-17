/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use serde::{Deserialize, Serialize};

use crate::Value;

/// Prometheus histogram value. This requires (experimental) support
/// for native histograms to be enabled.
#[derive(Serialize, Deserialize, PartialEq, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
pub struct Histogram {
    pub count: Value,
    pub sum: f64,
    pub buckets: Vec<Bucket>,
}

#[derive(Serialize, Deserialize, PartialEq, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[serde(
    from = "(BoundaryRule,Value,Value,Value)",
    into = "(BoundaryRule,Value,Value,Value)"
)]
pub struct Bucket {
    pub boundary_rule: BoundaryRule,
    pub left_boundary: Value,
    pub right_boundary: Value,
    pub count_in_bucket: Value,
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Clone, Copy, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[serde(try_from = "u8", into = "u8")]
pub enum BoundaryRule {
    OpenLeft,
    OpenRight,
    OpenBoth,
    ClosedBoth,
}

impl From<(BoundaryRule, Value, Value, Value)> for Bucket {
    fn from(
        (boundary_rule, left_boundary, right_boundary, count_in_bucket): (
            BoundaryRule,
            Value,
            Value,
            Value,
        ),
    ) -> Self {
        Bucket {
            boundary_rule,
            left_boundary,
            right_boundary,
            count_in_bucket,
        }
    }
}

impl From<Bucket> for (BoundaryRule, Value, Value, Value) {
    fn from(
        Bucket {
            boundary_rule,
            left_boundary,
            right_boundary,
            count_in_bucket,
        }: Bucket,
    ) -> Self {
        (
            boundary_rule,
            left_boundary,
            right_boundary,
            count_in_bucket,
        )
    }
}

impl TryFrom<u8> for BoundaryRule {
    type Error = ParseBoundaryError;
    fn try_from(n: u8) -> Result<Self, Self::Error> {
        match n {
            0 => Ok(Self::OpenLeft),
            1 => Ok(Self::OpenRight),
            2 => Ok(Self::OpenBoth),
            3 => Ok(Self::ClosedBoth),
            _ => Err(ParseBoundaryError::UnknownBoundaryRule(n)),
        }
    }
}

impl From<BoundaryRule> for u8 {
    fn from(val: BoundaryRule) -> Self {
        match val {
            BoundaryRule::OpenLeft => 0,
            BoundaryRule::OpenRight => 1,
            BoundaryRule::OpenBoth => 2,
            BoundaryRule::ClosedBoth => 3,
        }
    }
}

#[derive(thiserror::Error, Debug)]
pub enum ParseBoundaryError {
    #[error("unknown value for boundary rule: {0}")]
    UnknownBoundaryRule(u8),
}
