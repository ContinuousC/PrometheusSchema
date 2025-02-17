/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::{
    borrow::{Borrow, Cow},
    fmt::Display,
    str::FromStr,
};

use serde_with::{DeserializeFromStr, SerializeDisplay};

// No comment

/// A valid prometheus metric name.
/// Must match regex /[a-zA-Z_:][a-zA-Z0-9_:]*/.
#[derive(
    SerializeDisplay, DeserializeFromStr, Eq, PartialEq, Ord, PartialOrd, Hash, Clone, Debug,
)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct MetricName(Cow<'static, str>);

impl MetricName {
    /// Create a metric name from an owned string.
    pub fn new(s: String) -> Result<Self, ParseMetricError> {
        Self::check(&s)?;
        Ok(Self(Cow::Owned(s)))
    }

    /// Create a metric name from a static string. Panics if the
    /// metric name is not valid.
    pub const fn new_static(s: &'static str) -> Self {
        match Self::check(s) {
            Ok(()) => Self(Cow::Borrowed(s)),
            Err(_) => panic!("invalid static metric name"),
        }
    }

    /// Borrow the metric name as a string slice.
    pub fn as_str(&self) -> &str {
        self.0.as_ref()
    }

    /// Return the wrapped name as an owned string.
    pub fn into_string(self) -> String {
        self.0.into_owned()
    }

    const fn check(s: &str) -> Result<(), ParseMetricError> {
        let bs = s.as_bytes();

        if s.is_empty() {
            return Err(ParseMetricError {
                kind: MetricErrorKind::Empty,
            });
        };

        if !matches!(bs[0], b'A'..=b'Z' | b'a'..=b'z' | b'_' | b':') {
            return Err(ParseMetricError {
                kind: MetricErrorKind::InvalidChar(bs[0] as char),
            });
        }

        let mut i = 1;
        while i < bs.len() {
            if !matches!(bs[i], b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'_' | b':') {
                return Err(ParseMetricError {
                    kind: MetricErrorKind::InvalidChar(bs[i] as char),
                });
            }
            i += 1;
        }

        Ok(())
    }
}

impl Display for MetricName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl FromStr for MetricName {
    type Err = ParseMetricError;
    fn from_str(s: &str) -> Result<Self, ParseMetricError> {
        Self::check(s)?;
        Ok(Self(Cow::Owned(s.to_string())))
    }
}

impl Borrow<str> for MetricName {
    fn borrow(&self) -> &str {
        self.as_str()
    }
}

#[derive(thiserror::Error, Debug, Clone, PartialEq, Eq)]
#[error("{kind}")]
pub struct ParseMetricError {
    kind: MetricErrorKind,
}

#[derive(thiserror::Error, PartialEq, Eq, Clone, Debug)]
enum MetricErrorKind {
    #[error("cannot be empty")]
    Empty,
    #[error("invalid character: '{0}'")]
    InvalidChar(char),
}

#[cfg(test)]
mod test {
    use std::str::FromStr;

    use crate::MetricName;

    #[test]
    fn parse_valid_metric_names() {
        for name in &["metric_name", "metric:name", "m3tricN4me"] {
            assert!(MetricName::from_str(name).is_ok());
        }
    }

    #[test]
    fn reject_invalid_metric_names() {
        for name in &["metric name", "metric-name", ".metric"] {
            assert!(MetricName::from_str(name).is_err());
        }
    }
}
