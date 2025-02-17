/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::{fmt::Display, num::ParseFloatError, str::FromStr};

use serde_with::{DeserializeFromStr, SerializeDisplay};

/// A floating-point value as returned by prometheus. Prometheus uses
/// a string representation for floating-point numbers because JSON
/// does not allow any of the "special" floating-point values for its
/// number type. Special values are "NaN", "+Inf" and "-Inf".
#[derive(SerializeDisplay, DeserializeFromStr, PartialEq, PartialOrd, Clone, Copy, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(type = "string"))]
pub struct Value(pub f64);

/// Display value as returned by prometheus (string value).
impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.0.is_nan() {
            write!(f, "NaN")
        } else if self.0.is_infinite() {
            if self.0.is_sign_positive() {
                write!(f, "+Inf")
            } else {
                write!(f, "-Inf")
            }
        } else {
            write!(f, "{}", self.0)
        }
    }
}

/// Parse value as returned by prometheus (string value).
impl FromStr for Value {
    type Err = ParseFloatError;
    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s {
            "NaN" => Ok(Self(f64::NAN)),
            "-Inf" => Ok(Self(f64::NEG_INFINITY)),
            "Inf" => Ok(Self(f64::INFINITY)),
            n => Ok(Self(f64::from_str(n)?)),
        }
    }
}
