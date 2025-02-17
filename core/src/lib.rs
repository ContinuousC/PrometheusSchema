/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

mod label;
mod metric;

pub use label::{LabelName, ParseLabelError, LE_LABEL, METRIC_LABEL, QUANTILE_LABEL};
pub use metric::{MetricName, ParseMetricError};
