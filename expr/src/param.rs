/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::{collections::BTreeMap, fmt::Display, str::FromStr};

use crate::{
    duration::PromDuration,
    parse::{parse_identifier, Parse},
};
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, space0},
    combinator::map,
    error::FromExternalError,
    sequence::{delimited, preceded, terminated, tuple},
};
use ordered_float::OrderedFloat;
use serde::{Deserialize, Serialize};
use unit::{
    parser::valid_composite_unit, BaseUnit, Dimension, DimensionlessUnit, Unit, NEUTRAL_UNIT,
};

#[derive(Serialize, Deserialize, PartialEq, Eq, PartialOrd, Hash, Ord, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
pub struct ParamName(pub(crate) String);

#[derive(Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy, Debug)]
#[serde(rename_all = "snake_case")]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
pub enum ParamType {
    Int,
    // Float,
    Quantity(Dimension),
    PromDuration,
    // String,
    // Regex,
}

#[derive(Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[serde(rename_all = "snake_case")]
#[serde(untagged)]
pub enum ParamValue {
    Int(u64),
    Float(#[cfg_attr(feature = "schemars", schemars(with = "f64"))] OrderedFloat<f64>),
    Quantity(Quantity),
    PromDuration(PromDuration),
}

#[derive(Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[serde(rename_all = "snake_case")]
pub struct Quantity {
    #[cfg_attr(feature = "schemars", schemars(with = "f64"))]
    pub value: OrderedFloat<f64>,
    pub unit: Unit,
}

#[derive(Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Default, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(type = "{ [key: ParamName]: ParamValue }"))]
pub struct ParamValues(pub BTreeMap<ParamName, ParamValue>);

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug)]
pub enum Param<T: PromParam> {
    Literal(T),
    Param(ParamName, Option<T::Arg>),
}

impl ParamValue {
    //#[cfg(feature = "schema")]
    pub fn get_type(&self) -> ParamType {
        match self {
            ParamValue::Int(_) => ParamType::Int,
            ParamValue::Float(_) => ParamType::Quantity(Dimension::Dimensionless),
            ParamValue::Quantity(q) => ParamType::Quantity(q.unit.dimension()),
            ParamValue::PromDuration(_) => ParamType::PromDuration,
        }
    }

    pub fn into_type(self, typ: ParamType) -> Result<Self, ValueError> {
        match (typ, self) {
            (ParamType::Int, Self::Int(v)) => Ok(Self::Int(v)),
            (ParamType::Quantity(d), Self::Quantity(v)) if v.unit.dimension() == d => {
                Ok(Self::Quantity(v))
            }
            (ParamType::Quantity(Dimension::Dimensionless), Self::Float(v)) => {
                Ok(Self::Quantity(Quantity {
                    value: v,
                    unit: NEUTRAL_UNIT,
                }))
            }
            (ParamType::Quantity(Dimension::Dimensionless), Self::Int(v)) => {
                Ok(Self::Quantity(Quantity {
                    value: OrderedFloat(v as f64),
                    unit: NEUTRAL_UNIT,
                }))
            }
            (ParamType::PromDuration, Self::PromDuration(v)) => Ok(Self::PromDuration(v)),
            (typ, s) => Err(ValueError(typ, s.get_type())),
        }
    }
}

#[derive(thiserror::Error, Debug)]
#[error("invalid parameter type: expected {0}, got {1}")]
pub struct ValueError(ParamType, ParamType);

impl<T: PromParam> From<T> for Param<T> {
    fn from(value: T) -> Self {
        Self::Literal(value)
    }
}

impl From<f64> for Param<OrderedFloat<f64>> {
    fn from(value: f64) -> Self {
        Self::Literal(value.into())
    }
}

impl Display for ParamName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl FromStr for ParamName {
    type Err = ParamNameParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut cs = s.chars();
        cs.next().map_or(Err(Self::Err::EmptyName), |c| match c {
            'A'..='Z' | 'a'..='z' | '_' => Ok(()),
            _ => Err(Self::Err::InvalidFirstChar(c)),
        })?;
        s.chars().try_for_each(|c| match c {
            'A'..='Z' | 'a'..='z' | '0'..='9' | '_' => Ok(()),
            _ => Err(Self::Err::InvalidChar(c)),
        })?;
        Ok(Self(s.to_string()))
    }
}

#[derive(thiserror::Error, Debug)]
pub enum ParamNameParseError {
    #[error("cannot be empty")]
    EmptyName,
    #[error("invalid first character: {0}")]
    InvalidFirstChar(char),
    #[error("invalid character: {0}")]
    InvalidChar(char),
}

impl Display for ParamType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParamType::Int => write!(f, "int"),
            ParamType::Quantity(unit) => write!(f, "quantity ({unit})"),
            ParamType::PromDuration => write!(f, "duration"),
        }
    }
}

impl Parse for ParamName {
    fn parse(s: &str) -> nom::IResult<&str, Self> {
        map(parse_identifier, |name| ParamName(name.to_string()))(s)
    }
}

impl Parse for Unit {
    fn parse(s: &str) -> nom::IResult<&str, Self> {
        valid_composite_unit(s)
    }
}

pub trait PromParam: Parse + Display + Clone + Sized {
    type Arg: Parse + Display;
    fn param_type(arg: Option<&Self::Arg>) -> ParamType;
    fn resolve(
        name: &ParamName,
        value: &ParamValue,
        arg: Option<&Self::Arg>,
    ) -> Result<Self, ParamResolveError>;
}

pub(crate) trait PromParams<'a> {
    fn handle(&mut self, name: &'a ParamName, typ: ParamType) -> Result<(), ParamTypeError>;
}

impl<'a> PromParams<'a> for &mut BTreeMap<&'a ParamName, ParamType> {
    fn handle(&mut self, name: &'a ParamName, typ: ParamType) -> Result<(), ParamTypeError> {
        self.insert(name, typ)
            .filter(|old| *old != typ)
            .map_or(Ok(()), |old| {
                Err(ParamTypeError::Conflict(name.clone(), old, typ))
            })
    }
}

impl<'a> PromParams<'a> for &BTreeMap<ParamName, ParamType> {
    fn handle(&mut self, name: &'a ParamName, typ: ParamType) -> Result<(), ParamTypeError> {
        let actual = *self
            .get(name)
            .ok_or_else(|| ParamTypeError::Missing(name.clone()))?;
        (actual == typ)
            .then_some(())
            .ok_or_else(|| ParamTypeError::InvalidUse(name.clone(), actual, typ))
    }
}

impl<T: PromParam> Param<T> {
    pub(crate) fn param(&self) -> Option<(&ParamName, ParamType)> {
        match self {
            Param::Literal(_) => None,
            Param::Param(name, arg) => Some((name, T::param_type(arg.as_ref()))),
        }
    }

    pub(crate) fn add_param_type<'a, M: PromParams<'a>>(
        &'a self,
        params: &mut M,
    ) -> Result<(), ParamTypeError> {
        if let Some((name, typ)) = self.param() {
            params.handle(name, typ)
        } else {
            Ok(())
        }
    }

    #[cfg(feature = "schema")]
    pub(crate) fn resolve(
        &self,
        params: &BTreeMap<ParamName, ParamValue>,
    ) -> Result<T, ParamResolveError> {
        match self {
            Param::Literal(n) => Ok(n.clone()),
            Param::Param(name, arg) => {
                let value = params
                    .get(name)
                    .ok_or_else(|| ParamResolveError::Missing(name.clone()))?;
                T::resolve(name, value, arg.as_ref())
            }
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug)]
pub enum NoArg {}

impl Parse for NoArg {
    fn parse(s: &str) -> nom::IResult<&str, Self> {
        Err(nom::Err::Error(nom::error::Error::from_external_error(
            s,
            nom::error::ErrorKind::Alt,
            "this argument type takes no parameter",
        )))
    }
}

impl Display for NoArg {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        unreachable!("this argument type takes no parameter")
    }
}

impl<T: PromParam> Display for Param<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Param::Literal(lit) => write!(f, "{lit}"),
            Param::Param(name, None) => write!(f, "${name}"),
            Param::Param(name, Some(arg)) => write!(f, "${{{name} ({arg})}}"),
        }
    }
}

impl<T: PromParam> Parse for Param<T> {
    fn parse(s: &str) -> nom::IResult<&str, Self> {
        alt((
            map(T::parse, Self::Literal),
            map(
                delimited(
                    tag("${"),
                    tuple((
                        ParamName::parse,
                        terminated(
                            delimited(
                                tuple((char('('), space0)),
                                T::Arg::parse,
                                tuple((space0, char(')'))),
                            ),
                            space0,
                        ),
                    )),
                    tag("}"),
                ),
                |(name, arg)| Self::Param(name, Some(arg)),
            ),
            map(preceded(char('$'), ParamName::parse), |name| {
                Self::Param(name, None)
            }),
        ))(s)
    }
}

impl PromParam for OrderedFloat<f64> {
    type Arg = Unit;

    fn param_type(arg: Option<&Self::Arg>) -> ParamType {
        ParamType::Quantity(arg.map_or(Dimension::Dimensionless, |arg| arg.dimension()))
    }

    fn resolve(
        name: &ParamName,
        value: &ParamValue,
        arg: Option<&Self::Arg>,
    ) -> Result<Self, ParamResolveError> {
        let err = || {
            Err(ParamResolveError::UnexpectedType(
                name.clone(),
                ParamType::Quantity(arg.map_or(Dimension::Dimensionless, |unit| unit.dimension())),
                value.get_type(),
            ))
        };
        if let Some(n) = match value {
            ParamValue::Int(n) => Some(OrderedFloat(*n as f64)),
            ParamValue::Float(n) => Some(*n),
            _ => None,
        } {
            if let Some(unit) = arg {
                Unit::Dimensionless(DimensionlessUnit::REFERENCE)
                    .convert(unit, n.0)
                    .map_or_else(|_| err(), |v| Ok(OrderedFloat(v)))
            } else {
                Ok(n)
            }
        } else if let ParamValue::Quantity(q) = value {
            let target_unit = arg.copied().unwrap_or_else(|| q.unit.normalize());
            q.unit
                .convert(&target_unit, q.value.0)
                .map_or_else(|_| err(), |v| Ok(OrderedFloat(v)))
        } else {
            err()
        }
    }
}

impl PromParam for u64 {
    type Arg = NoArg;

    fn param_type(_arg: Option<&Self::Arg>) -> ParamType {
        ParamType::Int
    }

    fn resolve(
        name: &ParamName,
        value: &ParamValue,
        _arg: Option<&Self::Arg>,
    ) -> Result<Self, ParamResolveError> {
        match value {
            ParamValue::Int(n) => Ok(*n),
            _ => Err(ParamResolveError::UnexpectedType(
                name.clone(),
                ParamType::Int,
                value.get_type(),
            )),
        }
    }
}

impl PromParam for PromDuration {
    type Arg = NoArg;

    fn param_type(_arg: Option<&Self::Arg>) -> ParamType {
        ParamType::PromDuration
    }

    fn resolve(
        name: &ParamName,
        value: &ParamValue,
        _arg: Option<&Self::Arg>,
    ) -> Result<Self, ParamResolveError> {
        match value {
            ParamValue::PromDuration(v) => Ok(*v),
            _ => Err(ParamResolveError::UnexpectedType(
                name.clone(),
                ParamType::PromDuration,
                value.get_type(),
            )),
        }
    }
}

#[derive(thiserror::Error, Debug)]
pub enum ParamTypeError {
    #[error("missing parameter: {0}")]
    Missing(ParamName),
    #[error("invalid use of {0}: type {1}, used as {2}")]
    InvalidUse(ParamName, ParamType, ParamType),
    #[error("conflicting types for {0}: used as both {1} and {2}")]
    Conflict(ParamName, ParamType, ParamType),
}

#[derive(thiserror::Error, Debug)]
pub enum ParamResolveError {
    #[error("missing parameter: {0}")]
    Missing(ParamName),
    #[error("unexpected type for parameter {0}: expected {1}, got {2}")]
    UnexpectedType(ParamName, ParamType, ParamType),
}

// impl CustomParamType {
//     pub fn verify_param_type(&self, param_name: &ParamName, param_type: &ParamType) -> Result<()> {
//         match (self, param_type) {
//             (Self::PromDuration, ParamType::PromDuration) => Ok(()),
//             (Self::Unit(_), ParamType::Float) => Ok(()),
//             (Self::Unit(_), ParamType::Int) => Ok(()),
//             _ => Err(Error::CustomParamTypeMismatch(
//                 param_name.clone(),
//                 *self,
//                 *param_type,
//             )),
//         }
//     }
// }

// impl CustomParamValue {
//     pub fn to_type(&self, param_name: &ParamName, param_type: &CustomParamType) -> Result<Self> {
//         match (param_type, self) {
//             (CustomParamType::PromDuration, Self::PromDuration(v)) => {
//                 Ok(Self::PromDuration(v.clone()))
//             }
//             (CustomParamType::Unit(unit), Self::Quantity(v)) => {
//                 if unit.dimension() != v.unit.dimension() {
//                     Err(Error::CustomTypeUnitMisMatch(
//                         param_name.clone(),
//                         unit.dimension(),
//                         v.unit.dimension(),
//                     ))
//                 } else {
//                     Ok(Self::Quantity(v.clone()))
//                 }
//             }
//             (_, _) => Err(Error::CustomTypeMismatch(
//                 param_name.clone(),
//                 self.get_type(),
//                 param_type.clone(),
//             )),
//         }
//     }

//     pub fn to_param_value(
//         &self,
//         param_name: &ParamName,
//         param_type: &CustomParamType,
//     ) -> Result<ParamValue> {
//         match (param_type, self) {
//             (CustomParamType::PromDuration, Self::PromDuration(v)) => {
//                 Ok(ParamValue::PromDuration(v.clone()))
//             }
//             (CustomParamType::Unit(unit), Self::Quantity(v)) => {
//                 let value = v.unit.convert(unit, v.value.0).or_else(|_| {
//                     Err(Error::CustomTypeUnitMisMatch(
//                         param_name.clone(),
//                         unit.dimension(),
//                         v.unit.dimension(),
//                     ))
//                 })?;
//                 Ok(ParamValue::Float(OrderedFloat(value)))
//             }
//             (_, _) => Err(Error::CustomTypeMismatch(
//                 param_name.clone(),
//                 self.get_type(),
//                 param_type.clone(),
//             )),
//         }
//     }

//     fn get_type(&self) -> CustomParamType {
//         match self {
//             Self::PromDuration(_) => CustomParamType::PromDuration,
//             Self::Quantity(quantity) => CustomParamType::Unit(quantity.unit),
//         }
//     }
// }
