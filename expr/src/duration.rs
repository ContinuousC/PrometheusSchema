/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

#[cfg(feature = "schema")]
use std::collections::BTreeMap;
use std::{fmt::Display, str::FromStr};

use chrono::TimeDelta;
use nom::{
    bytes::complete::tag,
    character::complete::{alpha1, char},
    combinator::{map_res, not, opt},
    sequence::{terminated, tuple},
    IResult,
};
use serde_with::{DeserializeFromStr, SerializeDisplay};

use crate::{parse::Parse, Param, ParseError};
#[cfg(feature = "schema")]
use crate::{ParamName, ParamResolveError, ParamValue};

#[derive(
    SerializeDisplay,
    DeserializeFromStr,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Clone,
    Copy,
    Default,
    Debug,
)]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(
    feature = "tsify",
    tsify(from_wasm_abi, into_wasm_abi, type = "string")
)]
pub struct PromDuration {
    milliseconds: u64,
    seconds: u64,
    minutes: u32,
    hours: u32,
    days: u32,
    weeks: u16,
    years: u16,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy, Debug)]
pub enum Offset<PromDuration = crate::duration::PromDuration> {
    Negative(PromDuration),
    Positive(PromDuration),
}

impl PromDuration {
    pub const MAX_MILLISECONDS: u64 = (1u64 << 63) / 1_000_000;
    pub const MAX: Self = Self::zero()
        .with_milliseconds(Self::MAX_MILLISECONDS)
        .unwrap();

    pub const fn zero() -> Self {
        Self {
            milliseconds: 0,
            seconds: 0,
            minutes: 0,
            hours: 0,
            days: 0,
            weeks: 0,
            years: 0,
        }
    }

    pub const fn with_milliseconds(mut self, n: u64) -> Option<Self> {
        self.milliseconds = n;
        self.verified()
    }

    pub const fn with_seconds(mut self, n: u64) -> Option<Self> {
        self.seconds = n;
        self.verified()
    }

    pub const fn with_minutes(mut self, n: u32) -> Option<Self> {
        self.minutes = n;
        self.verified()
    }

    pub const fn with_hours(mut self, n: u32) -> Option<Self> {
        self.hours = n;
        self.verified()
    }

    pub const fn with_days(mut self, n: u32) -> Option<Self> {
        self.days = n;
        self.verified()
    }

    pub const fn with_weeks(mut self, n: u16) -> Option<Self> {
        self.weeks = n;
        self.verified()
    }

    pub const fn with_years(mut self, n: u16) -> Option<Self> {
        self.years = n;
        self.verified()
    }

    const fn verified(self) -> Option<Self> {
        let millis = self.milliseconds
            + self.seconds * 1000
            + self.minutes as u64 * 1000 * 60
            + self.hours as u64 * 60 * 60 * 1000
            + self.days as u64 * 24 * 60 * 60 * 1000
            + self.weeks as u64 * 7 * 24 * 60 * 60 * 1000
            + self.years as u64 * 365 * 24 * 60 * 60 * 1000;
        if millis <= Self::MAX_MILLISECONDS {
            Some(self)
        } else {
            None
        }
    }

    fn is_zero(&self) -> bool {
        self.milliseconds == 0
            && self.seconds == 0
            && self.minutes == 0
            && self.hours == 0
            && self.days == 0
            && self.weeks == 0
            && self.years == 0
    }

    pub fn to_time_delta(self) -> TimeDelta {
        self.try_to_time_delta().unwrap_or(TimeDelta::MAX)
    }

    fn try_to_time_delta(self) -> Option<TimeDelta> {
        TimeDelta::try_milliseconds(self.milliseconds as i64)?
            .checked_add(&TimeDelta::try_seconds(self.seconds as i64)?)?
            .checked_add(&TimeDelta::try_minutes(self.minutes as i64)?)?
            .checked_add(&TimeDelta::try_hours(self.hours as i64)?)?
            .checked_add(&TimeDelta::try_days(
                self.days as i64 + self.weeks as i64 * 7 + self.years as i64 * 365,
            )?)
    }
}

#[cfg(feature = "schemars")]
impl schemars::JsonSchema for PromDuration {
    fn schema_name() -> std::string::String {
        "PromDuration".to_owned()
    }
    fn schema_id() -> std::borrow::Cow<'static, str> {
        std::borrow::Cow::Borrowed(std::concat!(std::module_path!(), "::", "PromDuration"))
    }
    fn json_schema(gen: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
        String::json_schema(gen)
    }
}

impl Display for PromDuration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.years > 0 {
            write!(f, "{}y", self.years)?;
        }
        if self.weeks > 0 {
            write!(f, "{}w", self.weeks)?;
        }
        if self.days > 0 {
            write!(f, "{}d", self.days)?;
        }
        if self.hours > 0 {
            write!(f, "{}h", self.hours)?;
        }
        if self.minutes > 0 {
            write!(f, "{}m", self.minutes)?;
        }
        if self.seconds > 0 {
            write!(f, "{}s", self.seconds)?;
        }
        if self.milliseconds > 0 || self.is_zero() {
            write!(f, "{}ms", self.milliseconds)?;
        }
        Ok(())
    }
}

impl FromStr for PromDuration {
    type Err = ParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::parse_all(s)
    }
}

impl Parse for PromDuration {
    fn parse(s: &str) -> IResult<&str, Self> {
        let mut max_millis = PromDuration::MAX_MILLISECONDS;
        let mut elems = 0;

        macro_rules! parse_elem {
            ($s:ident, $name:ident, $type:ty, $unit:expr, $factor:expr) => {
                let ($s, $name) = map_res(
                    opt(terminated(<$type>::parse, tuple(($unit, not(alpha1))))),
                    |v| {
                        if let Some(v) = v {
                            elems += 1;
                            let millis = v as u64 * $factor;
                            if millis <= max_millis {
                                max_millis -= millis;
                                Ok(v)
                            } else {
                                Err("duration overflow")
                            }
                        } else {
                            Ok(0)
                        }
                    },
                )($s)?;
            };
        }

        parse_elem!(s, years, u16, char('y'), 1000 * 60 * 60 * 24 * 365);
        parse_elem!(s, weeks, u16, char('w'), 1000 * 60 * 60 * 24 * 7);
        parse_elem!(s, days, u32, char('d'), 1000 * 60 * 60 * 24);
        parse_elem!(s, hours, u32, char('h'), 1000 * 60 * 60);
        parse_elem!(s, minutes, u32, char('m'), 1000 * 60);
        parse_elem!(s, seconds, u64, char('s'), 1000);
        parse_elem!(s, milliseconds, u64, tag("ms"), 1);

        if elems > 0 {
            Ok((
                s,
                Self {
                    milliseconds,
                    seconds,
                    minutes,
                    hours,
                    days,
                    weeks,
                    years,
                },
            ))
        } else {
            Err(nom::Err::Failure(nom::error::Error::new(
                s,
                nom::error::ErrorKind::MapRes,
            )))
        }
    }
}

impl Offset<Param<PromDuration>> {
    #[cfg(feature = "schema")]
    pub(crate) fn resolve(
        &self,
        params: &BTreeMap<ParamName, ParamValue>,
    ) -> Result<Offset<PromDuration>, ParamResolveError> {
        match self {
            Offset::Negative(duration) => Ok(Offset::Negative(duration.resolve(params)?)),
            Offset::Positive(duration) => Ok(Offset::Positive(duration.resolve(params)?)),
        }
    }
}

impl<PromDuration: Display> Display for Offset<PromDuration> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Offset::Negative(duration) => write!(f, "-{duration}"),
            Offset::Positive(duration) => write!(f, "{duration}"),
        }
    }
}

#[cfg(test)]
mod test {
    use std::str::FromStr;

    use crate::PromDuration;

    #[test]
    fn parse_duration() {
        assert_eq!(
            PromDuration::from_str("1h5m").unwrap(),
            PromDuration::zero()
                .with_hours(1)
                .unwrap()
                .with_minutes(5)
                .unwrap()
        );
        assert_eq!(
            PromDuration::from_str("1y2w3d4h5m6s7ms").unwrap(),
            PromDuration {
                milliseconds: 7,
                seconds: 6,
                minutes: 5,
                hours: 4,
                days: 3,
                weeks: 2,
                years: 1,
            }
        );
    }

    #[test]
    fn roundtrip_duration() {
        for s in ["1y2w3d4h5m6s7ms", "1h32m3s", "5d123ms", "1y", "0ms"] {
            assert_eq!(PromDuration::from_str(s).unwrap().to_string(), s);
        }
    }

    #[test]
    fn parse_invalid_duration() {
        assert!(PromDuration::from_str("").is_err());
        assert!(PromDuration::from_str("1m2h").is_err());
        assert!(PromDuration::from_str("293y").is_err());
        assert!(PromDuration::from_str("15251w").is_err());
        assert!(PromDuration::from_str("106752d").is_err());
        assert!(PromDuration::from_str("2562048h").is_err());
        assert!(PromDuration::from_str("153722868m").is_err());
        assert!(PromDuration::from_str("9223372037s").is_err());
        assert!(PromDuration::from_str("9223372036855ms").is_err());
    }
}
