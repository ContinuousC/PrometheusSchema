/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::{
    borrow::{Borrow, Cow},
    fmt::Display,
    str::FromStr,
};

use serde_with::{DeserializeFromStr, SerializeDisplay};

pub static METRIC_LABEL: LabelName = LabelName::new_static("__name__");
pub static LE_LABEL: LabelName = LabelName::new_static("le");
pub static QUANTILE_LABEL: LabelName = LabelName::new_static("quantile");

/// Prometheus label name.
/// Must match regex /[a-zA-Z_][a-zA-Z0-9_]*/.
#[derive(
    SerializeDisplay, DeserializeFromStr, Eq, PartialEq, Ord, PartialOrd, Hash, Clone, Debug,
)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "dbschema", derive(dbschema::HasSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct LabelName(Cow<'static, str>);

impl LabelName {
    /// Create a label name from an owned string.
    pub fn new<T: Into<String>>(s: T) -> Result<Self, ParseLabelError> {
        let s = s.into();
        Self::check(&s)?;
        Ok(Self(Cow::Owned(s)))
    }

    /// Create a label name from a static string. Panics if the
    /// label name is not valid.
    pub const fn new_static(s: &'static str) -> Self {
        match Self::check(s) {
            Ok(()) => Self(Cow::Borrowed(s)),
            Err(_) => panic!("invalid static metric name"),
        }
    }

    /// Return the wrapped name as an owned string.
    pub fn into_string(self) -> String {
        self.0.into_owned()
    }

    /// Borrow the metric name as a string slice.
    pub fn as_str(&self) -> &str {
        self.0.as_ref()
    }

    pub fn is_special(&self) -> bool {
        self.0.starts_with("__")
    }

    const fn check(s: &str) -> Result<(), ParseLabelError> {
        let bs = s.as_bytes();

        if s.is_empty() {
            return Err(ParseLabelError {
                kind: LabelErrorKind::Empty,
            });
        };

        if !matches!(bs[0], b'A'..=b'Z' | b'a'..=b'z' | b'_') {
            return Err(ParseLabelError {
                kind: LabelErrorKind::InvalidChar(bs[0] as char),
            });
        }

        let mut i = 1;
        while i < bs.len() {
            if !matches!(bs[i], b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'_' ) {
                return Err(ParseLabelError {
                    kind: LabelErrorKind::InvalidChar(bs[i] as char),
                });
            }
            i += 1;
        }

        Ok(())
    }
}

impl Display for LabelName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl FromStr for LabelName {
    type Err = ParseLabelError;
    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        Self::check(s)?;
        Ok(Self(Cow::Owned(s.to_string())))
    }
}

impl Borrow<str> for LabelName {
    fn borrow(&self) -> &str {
        self.as_str()
    }
}

impl Borrow<str> for &LabelName {
    fn borrow(&self) -> &str {
        self.as_str()
    }
}

#[derive(thiserror::Error, Debug, Clone, PartialEq, Eq)]
#[error("{kind}")]
pub struct ParseLabelError {
    kind: LabelErrorKind,
}

#[derive(thiserror::Error, PartialEq, Eq, Clone, Debug)]
enum LabelErrorKind {
    #[error("cannot be empty")]
    Empty,
    #[error("invalid character: '{0}'")]
    InvalidChar(char),
}
