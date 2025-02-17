/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::{fmt::Debug, hash::Hash};

use crate::{
    expr_impl::{ExprImpl, ExprParam},
    param::PromParam,
};

/// A prometheus expression.
pub type Expr = ExprImpl<Vanilla>;

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug)]
pub struct Vanilla;

impl ExprParam for Vanilla {
    type Param<T: PromParam> = T;
}

impl From<f64> for Expr {
    fn from(n: f64) -> Self {
        Self::number(n)
    }
}

#[cfg(test)]
mod test {
    use std::str::FromStr;

    use crate::MetricSelector;

    use super::Expr;

    #[test]
    fn parse_expr() {
        let s = r#"( sum without (service) (rate(container_cpu_usage_seconds_total {boot_id = "", container = "", cpu = "total", endpoint =~ ".+", id =~ ".+", image = "", instance =~ ".+", job = "kubelet", kernelVersion = "", machine_id = "", metrics_path = "/metrics/cadvisor", mode = "", name = "", namespace !~ "|cortex", node =~ ".+", osVersion = "", pod =~ ".+", service = "monitoring-kube-prometheus-kubelet", system_uuid = ""}[5m]) > 0.2) or sum without (service) (rate(container_cpu_usage_seconds_total {boot_id = "", container = "", cpu = "total", endpoint =~ ".+", id =~ ".+", image = "", instance =~ ".+", job = "kubelet", kernelVersion = "", machine_id = "", metrics_path = "/metrics/cadvisor", mode = "", name = "", namespace !~ "|cortex", node =~ ".+", osVersion = "", pod =~ ".+", service = "monitoring-tools-kube-prom-kubelet", system_uuid = ""}[5m]) > 0.2) ) unless ( sum without (service) (rate(container_cpu_usage_seconds_total {boot_id = "", container = "", cpu = "total", endpoint =~ ".+", id =~ ".+", image = "", instance =~ ".+", job = "kubelet", kernelVersion = "", machine_id = "", metrics_path = "/metrics/cadvisor", mode = "", name = "", namespace !~ "|cortex", node =~ ".+", osVersion = "", pod =~ ".+", service = "monitoring-kube-prometheus-kubelet", system_uuid = ""}[5m]) > 0.5) or sum without (service) (rate(container_cpu_usage_seconds_total {boot_id = "", container = "", cpu = "total", endpoint =~ ".+", id =~ ".+", image = "", instance =~ ".+", job = "kubelet", kernelVersion = "", machine_id = "", metrics_path = "/metrics/cadvisor", mode = "", name = "", namespace !~ "|cortex", node =~ ".+", osVersion = "", pod =~ ".+", service = "monitoring-tools-kube-prom-kubelet", system_uuid = ""}[5m]) > 0.5) )"#;
        let _ = Expr::from_str(s).unwrap();
    }

    #[test]
    fn expr_bin_ops() {
        let a = Expr::number(1);
        let b = Expr::metric(MetricSelector::from_str("{ service = \"test\" }").unwrap());
        let c =
            Expr::metric(MetricSelector::from_str("cpu_usage { service = \"another\" }").unwrap());
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
            (a.clone() + b.clone() - c.clone()).to_string(),
            format!("{a} + {b} - {c}")
        );

        assert_eq!(
            // Associativity for BitXor in Rust differs from Pow in Prometheus.
            (a.clone() ^ (b.clone() ^ c.clone())).to_string(),
            format!("{a} ^ {b} ^ {c}")
        );
    }
}
