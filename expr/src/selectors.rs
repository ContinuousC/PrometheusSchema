/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::{
    collections::{btree_map::Entry, BTreeMap},
    fmt::Display,
    ops::BitAnd,
    str::FromStr,
};

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, space0},
    combinator::{cut, map, map_res, opt},
    sequence::{delimited, preceded, tuple},
    IResult,
};

use prometheus_core::METRIC_LABEL;
use prometheus_core::{LabelName, MetricName};
use serde::{Deserialize, Serialize};
use serde_with::{DeserializeFromStr, SerializeDisplay};

use crate::{
    parse::{our_separated_list0, parse_identifier, parse_string, Parse},
    regex::Regex,
    ParseError,
};

#[derive(
    SerializeDisplay,
    DeserializeFromStr,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Clone,
    Default,
    Debug,
)]
pub struct MetricSelector(BTreeMap<LabelName, LabelSelector>);

#[derive(Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug)]
#[serde(rename_all = "snake_case")]
pub enum LabelSelector {
    Eq(String),
    Ne(String),
    Match(Regex),
    NoMatch(Regex),
}

impl MetricSelector {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn metric(mut self, metric: MetricName) -> Self {
        self.0
            .insert(METRIC_LABEL.clone(), LabelSelector::Eq(metric.to_string()));
        self
    }

    pub fn label(mut self, label: LabelName, selector: LabelSelector) -> Self {
        self.0.insert(label, selector);
        self
    }

    pub fn labels<I, T>(mut self, labels: I) -> Self
    where
        I: IntoIterator<Item = (LabelName, T)>,
        T: Into<LabelSelector>,
    {
        labels.into_iter().for_each(|(label, selector)| {
            self.0.insert(label, selector.into());
        });
        self
    }

    pub fn label_eq<T: Into<String>>(self, label: LabelName, value: T) -> Self {
        self.label(label, LabelSelector::Eq(value.into()))
    }

    pub fn label_in<I, T>(self, label: LabelName, values: I) -> Self
    where
        I: IntoIterator<Item = T> + Copy,
        T: AsRef<str>,
    {
        self.label(
            label,
            LabelSelector::Match(MatchValues(values).to_string().parse().unwrap()),
        )
    }

    #[cfg(feature = "schema")]
    pub(crate) fn with_params(
        &self,
        _item_name: &prometheus_schema::QualifiedItemName,
        _item: &prometheus_schema::Item,
        selectors: &prometheus_schema::MetricSelector,
    ) -> Result<MetricSelector, MetricResolveError> {
        let m: MetricSelector = selectors.clone().into();
        self.clone() & m
    }
}

#[derive(thiserror::Error, Debug)]
pub enum MetricResolveError {
    #[error("unmatchable metric")]
    UnmatchabledMetric,
    #[error("unsupported regex combination")]
    RegexCombination,
}

#[cfg(feature = "schema")]
impl From<prometheus_schema::MetricSelector> for MetricSelector {
    fn from(selector: prometheus_schema::MetricSelector) -> Self {
        Self(
            selector
                .into_iter()
                .filter(|(_, selector)| !matches!(selector, prometheus_schema::LabelSelector::Opt))
                .map(|(label, selector)| (label, selector.into()))
                .collect(),
        )
    }
}

#[cfg(feature = "schema")]
impl From<prometheus_schema::LabelSelector> for LabelSelector {
    fn from(value: prometheus_schema::LabelSelector) -> Self {
        match value {
            prometheus_schema::LabelSelector::Opt => LabelSelector::Match(".*".parse().unwrap()),
            prometheus_schema::LabelSelector::Set => LabelSelector::Ne(String::new()),
            prometheus_schema::LabelSelector::Unset => LabelSelector::Eq(String::new()),
            prometheus_schema::LabelSelector::Eq(v) => LabelSelector::Eq(v),
            prometheus_schema::LabelSelector::Ne(v) => LabelSelector::Ne(v),
            prometheus_schema::LabelSelector::In(vs) => {
                LabelSelector::Match(MatchValues(&vs).to_string().parse().unwrap())
            }
            prometheus_schema::LabelSelector::NotIn(vs) => {
                LabelSelector::NoMatch(MatchValues(&vs).to_string().parse().unwrap())
            }
        }
    }
}

impl BitAnd for MetricSelector {
    type Output = Result<MetricSelector, MetricResolveError>;

    fn bitand(self, rhs: Self) -> Self::Output {
        rhs.0
            .into_iter()
            .try_fold(self, |mut m, (label, selector)| {
                match m.0.entry(label) {
                    Entry::Vacant(ent) => {
                        ent.insert(selector);
                    }
                    Entry::Occupied(mut ent) => {
                        ent.insert((ent.get().clone() & selector)?);
                    }
                }
                Ok(m)
            })
    }
}

impl BitAnd for LabelSelector {
    type Output = Result<LabelSelector, MetricResolveError>;

    fn bitand(self, rhs: LabelSelector) -> Self::Output {
        match (self, rhs) {
            (LabelSelector::Eq(a), LabelSelector::Eq(b)) => {
                if a == b {
                    Ok(LabelSelector::Eq(a))
                } else {
                    Err(MetricResolveError::UnmatchabledMetric)
                }
            }
            (LabelSelector::Ne(a), LabelSelector::Ne(b)) => {
                if a == b {
                    Ok(LabelSelector::Ne(a))
                } else {
                    Ok(LabelSelector::NoMatch(
                        MatchValues(&[a, b]).to_string().parse().unwrap(),
                    ))
                }
            }
            (LabelSelector::Eq(a), LabelSelector::Ne(b))
            | (LabelSelector::Ne(b), LabelSelector::Eq(a)) => {
                if a != b {
                    Ok(LabelSelector::Eq(a))
                } else {
                    Err(MetricResolveError::UnmatchabledMetric)
                }
            }
            (LabelSelector::Eq(a), LabelSelector::Match(b))
            | (LabelSelector::Match(b), LabelSelector::Eq(a)) => {
                if b.is_match(&a) {
                    Ok(LabelSelector::Eq(a))
                } else {
                    Err(MetricResolveError::UnmatchabledMetric)
                }
            }
            (LabelSelector::Eq(a), LabelSelector::NoMatch(b))
            | (LabelSelector::NoMatch(b), LabelSelector::Eq(a)) => {
                if !b.is_match(&a) {
                    Ok(LabelSelector::Eq(a))
                } else {
                    Err(MetricResolveError::UnmatchabledMetric)
                }
            }
            (LabelSelector::Ne(_), LabelSelector::Match(_) | LabelSelector::NoMatch(_))
            | (LabelSelector::Match(_) | LabelSelector::NoMatch(_), LabelSelector::Ne(_))
            | (
                LabelSelector::Match(_) | LabelSelector::NoMatch(_),
                LabelSelector::Match(_) | LabelSelector::NoMatch(_),
            ) => Err(MetricResolveError::RegexCombination),
        }
    }
}

impl Display for MetricSelector {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let metric_name = if let Some(LabelSelector::Eq(metric)) = self.0.get("__name__") {
            write!(f, "{metric} ")?;
            true
        } else {
            false
        };

        if !metric_name || self.0.len() > 1 {
            write!(f, "{{ ")?;
            self.0
                .iter()
                .filter(|(label, _)| !(metric_name && label.as_str() == "__name__"))
                .enumerate()
                .try_for_each(|(i, (label, selector))| {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{label} {selector}")
                })?;
            write!(f, " }}")?;
        }

        Ok(())
    }
}

impl Display for LabelSelector {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Eq(v) => write!(f, "= {}", ShowString(v)),
            Self::Ne(v) => write!(f, "!= {}", ShowString(v)),
            Self::Match(v) => write!(f, "=~ {}", ShowString(&v.to_string())),
            Self::NoMatch(v) => write!(f, "!~ {}", ShowString(&v.to_string())),
        }
    }
}

struct ShowString<'a>(&'a str);

impl Display for ShowString<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\"")?;
        self.0.chars().try_for_each(|c| match c {
            '"' => write!(f, "\\\""),
            '\\' => write!(f, "\\\\"),
            '\n' => write!(f, "\\n"),
            '\t' => write!(f, "\\t"),
            c => write!(f, "{c}"),
        })?;
        write!(f, "\"")
    }
}

struct RegexEscape<'a>(&'a str);

impl Display for RegexEscape<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.chars().try_for_each(|c| match c {
            '|' | '.' | '*' | '+' | '\\' | '(' | ')' | '{' | '}' | '[' | ']' => write!(f, "\\{c}"),
            '\n' => write!(f, "\\n"),
            '\r' => write!(f, "\\r"),
            '\t' => write!(f, "\\t"),
            _ => write!(f, "{c}"),
        })
    }
}

struct MatchValues<I>(I);

impl<I, T> Display for MatchValues<I>
where
    I: IntoIterator<Item = T> + Copy,
    T: AsRef<str>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut vs = (self.0).into_iter();
        vs.next()
            .map(|v| write!(f, "{}", RegexEscape(v.as_ref())))
            .transpose()?;
        vs.try_for_each(|v| write!(f, "|{}", RegexEscape(v.as_ref())))
    }
}

pub(crate) struct ShowLabels<'a>(pub(crate) &'a [LabelName]);

impl Display for ShowLabels<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut iter = self.0.iter();
        if let Some(first) = iter.next() {
            write!(f, "{first}")?;
            iter.try_for_each(|label| write!(f, ", {label}"))?;
        }
        Ok(())
    }
}

impl FromStr for MetricSelector {
    type Err = ParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::parse_all(s)
    }
}

impl Parse for MetricSelector {
    fn parse(s: &str) -> IResult<&str, MetricSelector> {
        let (s, metric) = opt(map_res(parse_identifier, |name| name.parse()))(s)?;
        let (s, labels): (_, Option<BTreeMap<_, _>>) = opt(delimited(
            tuple((char('{'), space0)),
            cut(our_separated_list0(
                tuple((char(','), space0)),
                <(LabelName, LabelSelector)>::parse,
            )),
            cut(tuple((space0, char('}')))),
        ))(s)?;
        if metric.is_some() || labels.is_some() {
            let a = labels.unwrap_or_default();
            let b = metric
                .into_iter()
                .map(|name| (METRIC_LABEL.clone(), LabelSelector::Eq(name)))
                .collect();
            if let Ok(m) = MetricSelector(a) & MetricSelector(b) {
                Ok((s, m))
            } else {
                Err(nom::Err::Error(nom::error::Error::new(
                    s,
                    nom::error::ErrorKind::MapRes,
                )))
            }
        } else {
            Err(nom::Err::Error(nom::error::Error::new(
                s,
                nom::error::ErrorKind::Alt,
            )))
        }
    }
}

impl Parse for (LabelName, LabelSelector) {
    fn parse(s: &str) -> IResult<&str, (LabelName, LabelSelector)> {
        let (s, label) = map_res(parse_identifier, |name| name.parse())(s)?;

        let (s, selector) = alt((
            map_res(preceded(tag("=~"), preceded(space0, parse_string)), |s| {
                s.parse().map(LabelSelector::Match)
            }),
            map_res(preceded(tag("!~"), preceded(space0, parse_string)), |s| {
                s.parse().map(LabelSelector::NoMatch)
            }),
            map(
                preceded(tag("!="), preceded(space0, parse_string)),
                LabelSelector::Ne,
            ),
            map(
                preceded(tag("="), preceded(space0, parse_string)),
                LabelSelector::Eq,
            ),
        ))(s)?;
        Ok((s, (label, selector)))
    }
}

#[cfg(test)]
mod test {

    #[test]
    fn and_metric_selectors() {
        use super::{MetricResolveError, MetricSelector};

        fn test_ok(a: &str, b: &str, c: &str) {
            let a: MetricSelector = a.parse().unwrap();
            let b: MetricSelector = b.parse().unwrap();
            let c: MetricSelector = c.parse().unwrap();
            assert_eq!((a & b).unwrap(), c);
        }

        macro_rules! test_err {
            ($a:expr, $b:expr, $e:pat) => {{
                let a: MetricSelector = $a.parse().unwrap();
                let b: MetricSelector = $b.parse().unwrap();
                matches!((a & b).unwrap_err(), $e)
            }};
        }

        test_ok(
            r#"cpu { pod = "myPod" }"#,
            r#"{ container = "myContainer"}"#,
            r#"cpu { container = "myContainer", pod = "myPod" }"#,
        );

        test_ok(
            r#"cpu { pod = "myPod", image = "myImage" }"#,
            r#"cpu { pod =~ ".+", container = "myContainer", image != "" }"#,
            r#"cpu { container = "myContainer", pod = "myPod", image = "myImage" }"#,
        );

        test_err!(
            r#"cpu { pod = "myPod" }"#,
            r#"memory { pod = "myPod" }"#,
            MetricResolveError::UnmatchabledMetric
        );
    }
}
