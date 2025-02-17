/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

#[cfg(feature = "schema")]
use std::collections::BTreeMap;
use std::fmt::Display;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, space0, space1},
    combinator::{cut, map, value},
    sequence::{preceded, terminated},
    IResult,
};

use ordered_float::OrderedFloat;
use prometheus_core::LabelName;

#[cfg(feature = "schema")]
use crate::{expr::Vanilla, ParamName};
use crate::{
    expr_impl::ExprType,
    expr_spec::Spec,
    param::{ParamTypeError, PromParams},
    parse::Parse,
    selectors::ShowLabels,
};
#[cfg(feature = "schema")]
use crate::{ParamResolveError, ParamValue};

#[derive(PartialEq, Clone, Copy, Debug)]
pub(crate) enum AggrFn {
    Sum,
    Min,
    Max,
    Avg,
    Group,
    StdDev,
    StdVar,
    Count,
    CountValues,
    BottomK,
    TopK,
    Quantile,
}

#[derive(PartialEq, Clone, Copy, Debug)]
pub(crate) enum AggrOverTimeFn {
    Avg,
    Min,
    Max,
    Sum,
    Count,
    Quantile,
    StdDev,
    StdVar,
    Last,
    Present,
    // Mad
}

pub(crate) enum AggrParam<'a, T: ExprType> {
    Int(&'a T::Param<u64>),
    Float(&'a T::Param<OrderedFloat<f64>>),
    LabelName(&'a LabelName),
}

pub(crate) enum AggrOverTimeParam<'a, T: ExprType> {
    Float(&'a T::Param<OrderedFloat<f64>>),
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug)]
pub enum AggrOp<T: ExprType> {
    Sum,
    Min,
    Max,
    Avg,
    Group,
    StdDev,
    StdVar,
    Count,
    CountValues(LabelName),
    BottomK(T::Param<u64>),
    TopK(T::Param<u64>),
    Quantile(T::Param<OrderedFloat<f64>>),
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug)]
pub enum AggrOverTimeOp<T: ExprType> {
    Avg,
    Min,
    Max,
    Sum,
    Count,
    Quantile(T::Param<OrderedFloat<f64>>),
    StdDev,
    StdVar,
    Last,
    Present,
    // Mad
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug)]
pub enum AggrModifier {
    By(Vec<LabelName>),
    Without(Vec<LabelName>),
}

impl AggrOp<Spec> {
    #[cfg(feature = "schema")]
    pub(crate) fn resolve(
        &self,
        params: &BTreeMap<ParamName, ParamValue>,
    ) -> Result<AggrOp<Vanilla>, ParamResolveError> {
        match self {
            AggrOp::Sum => Ok(AggrOp::Sum),
            AggrOp::Min => Ok(AggrOp::Min),
            AggrOp::Max => Ok(AggrOp::Max),
            AggrOp::Avg => Ok(AggrOp::Avg),
            AggrOp::Group => Ok(AggrOp::Group),
            AggrOp::StdDev => Ok(AggrOp::StdDev),
            AggrOp::StdVar => Ok(AggrOp::StdVar),
            AggrOp::Count => Ok(AggrOp::Count),
            AggrOp::CountValues(label) => Ok(AggrOp::CountValues(label.clone())),
            AggrOp::BottomK(param) => Ok(AggrOp::BottomK(param.resolve(params)?)),
            AggrOp::TopK(param) => Ok(AggrOp::TopK(param.resolve(params)?)),
            AggrOp::Quantile(param) => Ok(AggrOp::Quantile(param.resolve(params)?)),
        }
    }
}

impl AggrOverTimeOp<Spec> {
    #[cfg(feature = "schema")]
    pub(crate) fn resolve(
        &self,
        params: &BTreeMap<ParamName, ParamValue>,
    ) -> Result<AggrOverTimeOp<Vanilla>, ParamResolveError> {
        match self {
            AggrOverTimeOp::Avg => Ok(AggrOverTimeOp::Avg),
            AggrOverTimeOp::Min => Ok(AggrOverTimeOp::Min),
            AggrOverTimeOp::Max => Ok(AggrOverTimeOp::Max),
            AggrOverTimeOp::Sum => Ok(AggrOverTimeOp::Sum),
            AggrOverTimeOp::Count => Ok(AggrOverTimeOp::Count),
            AggrOverTimeOp::StdDev => Ok(AggrOverTimeOp::StdDev),
            AggrOverTimeOp::StdVar => Ok(AggrOverTimeOp::StdVar),
            AggrOverTimeOp::Quantile(param) => Ok(AggrOverTimeOp::Quantile(param.resolve(params)?)),
            AggrOverTimeOp::Last => Ok(AggrOverTimeOp::Last),
            AggrOverTimeOp::Present => Ok(AggrOverTimeOp::Present),
        }
    }
}

impl Display for AggrFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AggrFn::Sum => write!(f, "sum"),
            AggrFn::Min => write!(f, "min"),
            AggrFn::Max => write!(f, "max"),
            AggrFn::Avg => write!(f, "avg"),
            AggrFn::Group => write!(f, "group"),
            AggrFn::StdDev => write!(f, "stddev"),
            AggrFn::StdVar => write!(f, "stdvar"),
            AggrFn::Count => write!(f, "count"),
            AggrFn::CountValues => write!(f, "count_values"),
            AggrFn::BottomK => write!(f, "bottomk"),
            AggrFn::TopK => write!(f, "topk"),
            AggrFn::Quantile => write!(f, "quantile"),
        }
    }
}

impl Display for AggrOverTimeFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AggrOverTimeFn::Avg => write!(f, "avg"),
            AggrOverTimeFn::Min => write!(f, "min"),
            AggrOverTimeFn::Max => write!(f, "max"),
            AggrOverTimeFn::Sum => write!(f, "sum"),
            AggrOverTimeFn::Count => write!(f, "count"),
            AggrOverTimeFn::StdDev => write!(f, "stddev"),
            AggrOverTimeFn::StdVar => write!(f, "stdvar"),
            AggrOverTimeFn::Quantile => write!(f, "quantile"),
            AggrOverTimeFn::Last => write!(f, "last"),
            AggrOverTimeFn::Present => write!(f, "present"),
        }
    }
}

impl Display for AggrModifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AggrModifier::By(ls) => write!(f, "by ({})", ShowLabels(ls)),
            AggrModifier::Without(ls) => write!(f, "without ({})", ShowLabels(ls)),
        }
    }
}

impl<T: ExprType> AggrOp<T> {
    pub(crate) fn operator(&self) -> AggrFn {
        match self {
            AggrOp::Sum => AggrFn::Sum,
            AggrOp::Min => AggrFn::Min,
            AggrOp::Max => AggrFn::Max,
            AggrOp::Avg => AggrFn::Avg,
            AggrOp::Group => AggrFn::Group,
            AggrOp::StdDev => AggrFn::StdDev,
            AggrOp::StdVar => AggrFn::StdVar,
            AggrOp::Count => AggrFn::Count,
            AggrOp::CountValues(_) => AggrFn::CountValues,
            AggrOp::BottomK(_) => AggrFn::BottomK,
            AggrOp::TopK(_) => AggrFn::TopK,
            AggrOp::Quantile(_) => AggrFn::Quantile,
        }
    }

    pub(crate) fn param(&self) -> Option<AggrParam<'_, T>> {
        match self {
            AggrOp::Sum
            | AggrOp::Min
            | AggrOp::Max
            | AggrOp::Avg
            | AggrOp::Group
            | AggrOp::StdDev
            | AggrOp::StdVar
            | AggrOp::Count => None,
            AggrOp::CountValues(name) => Some(AggrParam::LabelName(name)),
            AggrOp::BottomK(p) | AggrOp::TopK(p) => Some(AggrParam::Int(p)),
            AggrOp::Quantile(p) => Some(AggrParam::Float(p)),
        }
    }
}

impl<T: ExprType> AggrOverTimeOp<T> {
    pub(crate) fn operator(&self) -> AggrOverTimeFn {
        match self {
            AggrOverTimeOp::Avg => AggrOverTimeFn::Avg,
            AggrOverTimeOp::Min => AggrOverTimeFn::Min,
            AggrOverTimeOp::Max => AggrOverTimeFn::Max,
            AggrOverTimeOp::Sum => AggrOverTimeFn::Sum,
            AggrOverTimeOp::Count => AggrOverTimeFn::Count,
            AggrOverTimeOp::StdDev => AggrOverTimeFn::StdDev,
            AggrOverTimeOp::StdVar => AggrOverTimeFn::StdVar,
            AggrOverTimeOp::Quantile(_) => AggrOverTimeFn::Quantile,
            AggrOverTimeOp::Last => AggrOverTimeFn::Last,
            AggrOverTimeOp::Present => AggrOverTimeFn::Present,
        }
    }

    pub(crate) fn param(&self) -> Option<AggrOverTimeParam<T>> {
        match self {
            AggrOverTimeOp::Quantile(p) => Some(AggrOverTimeParam::Float(p)),
            _ => None,
        }
    }
}

impl AggrOp<Spec> {
    pub(crate) fn add_param_types<'a, M: PromParams<'a>>(
        &'a self,
        params: &mut M,
    ) -> Result<(), ParamTypeError> {
        match self.param() {
            Some(p) => p.add_param_type(params),
            None => Ok(()),
        }
    }
}

impl<'a> AggrParam<'a, Spec> {
    pub(crate) fn add_param_type<M: PromParams<'a>>(
        &self,
        params: &mut M,
    ) -> Result<(), ParamTypeError> {
        match self {
            AggrParam::Int(p) => p.add_param_type(params),
            AggrParam::Float(p) => p.add_param_type(params),
            AggrParam::LabelName(_) => {
                /* LabelName param currently not implemented. */
                Ok(())
            }
        }
    }
}

impl AggrOverTimeOp<Spec> {
    pub(crate) fn add_param_types<'a, M: PromParams<'a>>(
        &'a self,
        params: &mut M,
    ) -> Result<(), ParamTypeError> {
        match self.param() {
            Some(p) => p.add_param_type(params),
            None => Ok(()),
        }
    }
}

impl<'a> AggrOverTimeParam<'a, Spec> {
    pub(crate) fn add_param_type<M: PromParams<'a>>(
        &self,
        params: &mut M,
    ) -> Result<(), ParamTypeError> {
        match self {
            AggrOverTimeParam::Float(p) => p.add_param_type(params),
        }
    }
}

impl<T: ExprType> Display for AggrParam<'_, T>
where
    T::Param<u64>: Display,
    T::Param<OrderedFloat<f64>>: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AggrParam::Float(param) => write!(f, "{param}"),
            AggrParam::Int(param) => write!(f, "{param}"),
            AggrParam::LabelName(param) => write!(f, "{param}"),
        }
    }
}

impl<T: ExprType> Display for AggrOverTimeParam<'_, T>
where
    T::Param<OrderedFloat<f64>>: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AggrOverTimeParam::Float(param) => write!(f, "{param}"),
        }
    }
}

impl Parse for AggrFn {
    fn parse(s: &str) -> IResult<&str, Self> {
        alt((
            value(AggrFn::Sum, tag("sum")),
            value(AggrFn::Min, tag("min")),
            value(AggrFn::Max, tag("max")),
            value(AggrFn::Avg, tag("avg")),
            value(AggrFn::Group, tag("group")),
            value(AggrFn::StdDev, tag("stddev")),
            value(AggrFn::StdVar, tag("stdvar")),
            value(AggrFn::Count, tag("count")),
            value(AggrFn::CountValues, tag("count_values")),
            value(AggrFn::BottomK, tag("bottomk")),
            value(AggrFn::TopK, tag("topk")),
            value(AggrFn::Quantile, tag("quantile")),
        ))(s)
    }
}

impl Parse for AggrOverTimeFn {
    fn parse(s: &str) -> IResult<&str, Self> {
        alt((
            value(AggrOverTimeFn::Avg, tag("avg")),
            value(AggrOverTimeFn::Min, tag("min")),
            value(AggrOverTimeFn::Max, tag("max")),
            value(AggrOverTimeFn::Sum, tag("sum")),
            value(AggrOverTimeFn::Count, tag("count")),
            value(AggrOverTimeFn::Quantile, tag("quantile")),
            value(AggrOverTimeFn::StdDev, tag("stddev")),
            value(AggrOverTimeFn::StdVar, tag("stdvar")),
            value(AggrOverTimeFn::Last, tag("last")),
            value(AggrOverTimeFn::Present, tag("present")),
        ))(s)
    }
}

impl Parse for AggrModifier {
    fn parse(s: &str) -> IResult<&str, Self> {
        preceded(
            space1,
            alt((
                map(
                    preceded(tag("by"), cut(preceded(space0, Vec::<LabelName>::parse))),
                    AggrModifier::By,
                ),
                map(
                    preceded(
                        tag("without"),
                        cut(preceded(space0, Vec::<LabelName>::parse)),
                    ),
                    AggrModifier::Without,
                ),
            )),
        )(s)
    }
}

impl<T: ExprType> AggrOp<T>
where
    T::Param<u64>: Parse,
    T::Param<OrderedFloat<f64>>: Parse,
{
    pub(crate) fn parser(op: AggrFn) -> impl Fn(&str) -> IResult<&str, Self> {
        move |s: &str| match op {
            AggrFn::Sum => Ok((s, Self::Sum)),
            AggrFn::Min => Ok((s, Self::Min)),
            AggrFn::Max => Ok((s, Self::Max)),
            AggrFn::Avg => Ok((s, Self::Avg)),
            AggrFn::Group => Ok((s, Self::Group)),
            AggrFn::StdDev => Ok((s, Self::StdDev)),
            AggrFn::StdVar => Ok((s, Self::StdVar)),
            AggrFn::Count => Ok((s, Self::Count)),
            AggrFn::CountValues => {
                let (s, param) = terminated(LabelName::parse, char(','))(s)?;
                Ok((s, Self::CountValues(param)))
            }
            AggrFn::BottomK => {
                let (s, param) = terminated(T::Param::<u64>::parse, char(','))(s)?;
                Ok((s, Self::BottomK(param)))
            }
            AggrFn::TopK => {
                let (s, param) = terminated(T::Param::<u64>::parse, char(','))(s)?;
                Ok((s, Self::TopK(param)))
            }
            AggrFn::Quantile => {
                let (s, param) = terminated(T::Param::<OrderedFloat<f64>>::parse, char(','))(s)?;
                Ok((s, Self::Quantile(param)))
            }
        }
    }
}

impl<T: ExprType> AggrOverTimeOp<T>
where
    T::Param<OrderedFloat<f64>>: Parse,
{
    pub(crate) fn parser(op: AggrOverTimeFn) -> impl Fn(&str) -> IResult<&str, Self> {
        move |s: &str| match op {
            AggrOverTimeFn::Avg => Ok((s, Self::Avg)),
            AggrOverTimeFn::Min => Ok((s, Self::Min)),
            AggrOverTimeFn::Max => Ok((s, Self::Max)),
            AggrOverTimeFn::Sum => Ok((s, Self::Sum)),
            AggrOverTimeFn::Count => Ok((s, Self::Count)),
            AggrOverTimeFn::Quantile => {
                let (s, param) = terminated(T::Param::<OrderedFloat<f64>>::parse, char(','))(s)?;
                Ok((s, Self::Quantile(param)))
            }
            AggrOverTimeFn::StdDev => Ok((s, Self::StdDev)),
            AggrOverTimeFn::StdVar => Ok((s, Self::StdVar)),
            AggrOverTimeFn::Last => Ok((s, Self::Last)),
            AggrOverTimeFn::Present => Ok((s, Self::Present)),
        }
    }
}
