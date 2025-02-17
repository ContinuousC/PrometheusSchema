/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::{collections::BTreeMap, fmt::Debug, hash::Hash};

use ordered_float::OrderedFloat;

use crate::{
    expr_impl::{ExprImpl, ExprParam},
    param::{Param, PromParam},
    ParamName, ParamType, ParamTypeError,
};
#[cfg(feature = "schema")]
use crate::{Expr, MetricResolveError, ParamResolveError, ParamValue};

/// A prometheus expression specification.
pub type ExprSpec = ExprImpl<Spec>;

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug)]
pub struct Spec;

impl ExprParam for Spec {
    type Param<T: PromParam> = Param<T>;
}

impl From<f64> for ExprSpec {
    fn from(value: f64) -> Self {
        Self::number(Param::Literal(OrderedFloat(value)))
    }
}

/*** ExprSpec -> Expr ***/

impl ExprSpec {
    pub fn get_param_types(&self) -> Result<BTreeMap<&ParamName, ParamType>, ParamTypeError> {
        let mut params = BTreeMap::new();
        self.add_param_types(&mut &mut params)?;
        Ok(params)
    }

    pub fn verify(
        &self,
        mut params: &BTreeMap<ParamName, ParamType>,
    ) -> Result<(), ParamTypeError> {
        self.add_param_types(&mut params)
    }

    #[cfg(feature = "schema")]
    pub fn with_params(
        &self,
        item_name: &prometheus_schema::QualifiedItemName,
        item: &prometheus_schema::Item,
        selectors: &prometheus_schema::MetricSelector,
        params: &BTreeMap<ParamName, ParamValue>,
    ) -> Result<Expr, SpecResolveError> {
        match self {
            ExprImpl::Number(param) => Ok(ExprImpl::Number(
                param.resolve(params).map_err(SpecResolveError::Param)?,
            )),
            ExprImpl::Metric(metric, range, at, offset) => Ok(ExprImpl::Metric(
                metric
                    .with_params(item_name, item, selectors)
                    .map_err(SpecResolveError::Metric)?,
                range
                    .as_ref()
                    .map(|duration| duration.resolve(params))
                    .transpose()
                    .map_err(SpecResolveError::Param)?,
                at.as_ref()
                    .map(|t| t.resolve(params))
                    .transpose()
                    .map_err(SpecResolveError::Param)?,
                offset
                    .as_ref()
                    .map(|offs| offs.resolve(params))
                    .transpose()
                    .map_err(SpecResolveError::Param)?,
            )),
            ExprImpl::SubQuery(expr, range, resolution, at, offset) => Ok(ExprImpl::SubQuery(
                Box::new(expr.with_params(item_name, item, selectors, params)?),
                range.resolve(params).map_err(SpecResolveError::Param)?,
                resolution
                    .as_ref()
                    .map(|duration| duration.resolve(params))
                    .transpose()
                    .map_err(SpecResolveError::Param)?,
                at.as_ref()
                    .map(|t| t.resolve(params))
                    .transpose()
                    .map_err(SpecResolveError::Param)?,
                offset
                    .as_ref()
                    .map(|offs| offs.resolve(params))
                    .transpose()
                    .map_err(SpecResolveError::Param)?,
            )),
            ExprImpl::Binary(op, m, a, b) => Ok(ExprImpl::Binary(
                *op,
                m.clone(),
                Box::new(a.with_params(item_name, item, selectors, params)?),
                Box::new(b.with_params(item_name, item, selectors, params)?),
            )),
            ExprImpl::Rate(expr) => Ok(ExprImpl::Rate(Box::new(
                expr.with_params(item_name, item, selectors, params)?,
            ))),
            ExprImpl::Aggr(op, m, expr) => Ok(ExprImpl::Aggr(
                op.resolve(params).map_err(SpecResolveError::Param)?,
                m.clone(),
                Box::new(expr.with_params(item_name, item, selectors, params)?),
            )),
            ExprImpl::AggrOverTime(op, expr) => Ok(ExprImpl::AggrOverTime(
                op.resolve(params).map_err(SpecResolveError::Param)?,
                Box::new(expr.with_params(item_name, item, selectors, params)?),
            )),
            ExprImpl::HistogramQuantile(param, expr) => Ok(ExprImpl::HistogramQuantile(
                param.resolve(params).map_err(SpecResolveError::Param)?,
                Box::new(expr.with_params(item_name, item, selectors, params)?),
            )),
            ExprImpl::Clamp(expr, min, max) => Ok(ExprImpl::Clamp(
                Box::new(expr.with_params(item_name, item, selectors, params)?),
                min.resolve(params).map_err(SpecResolveError::Param)?,
                max.resolve(params).map_err(SpecResolveError::Param)?,
            )),
            ExprImpl::ClampMin(expr, min) => Ok(ExprImpl::ClampMin(
                Box::new(expr.with_params(item_name, item, selectors, params)?),
                min.resolve(params).map_err(SpecResolveError::Param)?,
            )),
            ExprImpl::ClampMax(expr, max) => Ok(ExprImpl::ClampMax(
                Box::new(expr.with_params(item_name, item, selectors, params)?),
                max.resolve(params).map_err(SpecResolveError::Param)?,
            )),
        }
    }
}

#[cfg(feature = "schema")]
#[derive(thiserror::Error, Debug)]
pub enum SpecResolveError {
    #[error("failed to resolve parameter: {0}")]
    Param(ParamResolveError),
    #[error("failed to resolve metric: {0}")]
    Metric(MetricResolveError),
}

#[cfg(test)]
mod test {
    use std::{collections::BTreeMap, str::FromStr};

    use crate::{MetricSelector, Param, ParamName, ParamType};

    use super::ExprSpec;

    #[test]
    fn parse_expr_spec() {
        let s = r#"( sum without (service) (rate(container_cpu_usage_seconds_total {boot_id = "", container = "", cpu = "total", endpoint =~ ".+", id =~ ".+", image = "", instance =~ ".+", job = "kubelet", kernelVersion = "", machine_id = "", metrics_path = "/metrics/cadvisor", mode = "", name = "", namespace !~ "|cortex", node =~ ".+", osVersion = "", pod =~ ".+", service = "monitoring-kube-prometheus-kubelet", system_uuid = ""}[5m]) > $threshold) or sum without (service) (rate(container_cpu_usage_seconds_total {boot_id = "", container = "", cpu = "total", endpoint =~ ".+", id =~ ".+", image = "", instance =~ ".+", job = "kubelet", kernelVersion = "", machine_id = "", metrics_path = "/metrics/cadvisor", mode = "", name = "", namespace !~ "|cortex", node =~ ".+", osVersion = "", pod =~ ".+", service = "monitoring-tools-kube-prom-kubelet", system_uuid = ""}[5m]) > $threshold) ) unless ( sum without (service) (rate(container_cpu_usage_seconds_total {boot_id = "", container = "", cpu = "total", endpoint =~ ".+", id =~ ".+", image = "", instance =~ ".+", job = "kubelet", kernelVersion = "", machine_id = "", metrics_path = "/metrics/cadvisor", mode = "", name = "", namespace !~ "|cortex", node =~ ".+", osVersion = "", pod =~ ".+", service = "monitoring-kube-prometheus-kubelet", system_uuid = ""}[5m]) > 0.5) or sum without (service) (rate(container_cpu_usage_seconds_total {boot_id = "", container = "", cpu = "total", endpoint =~ ".+", id =~ ".+", image = "", instance =~ ".+", job = "kubelet", kernelVersion = "", machine_id = "", metrics_path = "/metrics/cadvisor", mode = "", name = "", namespace !~ "|cortex", node =~ ".+", osVersion = "", pod =~ ".+", service = "monitoring-tools-kube-prom-kubelet", system_uuid = ""}[5m]) > 0.5) )"#;
        let spec = ExprSpec::from_str(s).unwrap();
        assert_eq!(
            spec.get_param_types().unwrap(),
            BTreeMap::from_iter([(
                &ParamName(String::from("threshold")),
                ParamType::Quantity(unit::Dimension::Dimensionless)
            )])
        )
    }

    #[test]
    fn expr_spec_bin_ops() {
        let a = ExprSpec::number(Param::Param(ParamName::from_str("test").unwrap(), None));
        let b = ExprSpec::metric(MetricSelector::from_str("{ service = \"test\" }").unwrap());
        let c = ExprSpec::metric(
            MetricSelector::from_str("cpu_usage { service = \"another\" }").unwrap(),
        );
        assert_eq!((a.clone() + b.clone()).to_string(), format!("{a} + {b}"));
        assert_eq!((a.clone() - b.clone()).to_string(), format!("{a} - {b}"));
        assert_eq!((a.clone() * b.clone()).to_string(), format!("{a} * {b}"));
        assert_eq!((a.clone() / b.clone()).to_string(), format!("{a} / {b}"));
        assert_eq!((a.clone() ^ b.clone()).to_string(), format!("{a} ^ {b}"));
        assert_eq!((a.clone() % b.clone()).to_string(), format!("{a} % {b}"));

        assert_eq!(
            (a.clone() + b.clone() * c.clone()).to_string(),
            format!("{a} + {b} * {c}")
        );

        assert_eq!(
            // Associativity for BitXor in Rust differs from Pow in Prometheus.
            (a.clone() ^ (b.clone() ^ c.clone())).to_string(),
            format!("{a} ^ {b} ^ {c}")
        );
    }
}
