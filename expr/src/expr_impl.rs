/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::{
    fmt::{Debug, Display},
    hash::Hash,
    ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Rem, Sub},
    str::FromStr,
};

use nom::{
    branch::{alt, permutation},
    bytes::complete::tag,
    character::complete::{char, space0, space1},
    combinator::{cut, map, opt},
    sequence::{delimited, preceded, terminated, tuple},
    IResult,
};
use ordered_float::OrderedFloat;
use prometheus_core::LabelName;
use serde_with::{DeserializeFromStr, SerializeDisplay};

use crate::{
    aggregation::{AggrFn, AggrModifier, AggrOp, AggrOverTimeFn, AggrOverTimeOp},
    binary::{Assoc, BinModifier, BinOp, Prec},
    duration::{Offset, PromDuration},
    expr_spec::Spec,
    param::{ParamTypeError, PromParam, PromParams},
    parse::Parse,
    selectors::MetricSelector,
    ParseError,
};

#[derive(
    SerializeDisplay, DeserializeFromStr, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug,
)]
#[cfg_attr(feature = "apistos", derive(apistos::ApiComponent))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(
    feature = "tsify",
    tsify(from_wasm_abi, into_wasm_abi, type = "string")
)]
pub enum ExprImpl<T: ExprType> {
    Number(T::Param<OrderedFloat<f64>>),
    Metric(
        MetricSelector,
        Option<T::Param<PromDuration>>,
        Option<T::Param<OrderedFloat<f64>>>,
        Option<Offset<T::Param<PromDuration>>>,
    ),
    SubQuery(
        Box<Self>,
        T::Param<PromDuration>,
        Option<T::Param<PromDuration>>,
        Option<T::Param<OrderedFloat<f64>>>,
        Option<Offset<T::Param<PromDuration>>>,
    ),
    Binary(BinOp, Option<BinModifier>, Box<Self>, Box<Self>),
    Rate(Box<Self>),
    Aggr(AggrOp<T>, Option<AggrModifier>, Box<Self>),
    AggrOverTime(AggrOverTimeOp<T>, Box<Self>),
    HistogramQuantile(T::Param<OrderedFloat<f64>>, Box<Self>),
    Clamp(
        Box<Self>,
        T::Param<OrderedFloat<f64>>,
        T::Param<OrderedFloat<f64>>,
    ),
    ClampMin(Box<Self>, T::Param<OrderedFloat<f64>>),
    ClampMax(Box<Self>, T::Param<OrderedFloat<f64>>),
}

#[cfg(feature = "schemars")]
impl<T: ExprType> schemars::JsonSchema for ExprImpl<T> {
    fn schema_name() -> String {
        String::from("ExprImpl")
    }

    fn json_schema(gen: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
        String::json_schema(gen)
    }
}

macro_rules! define_for {
    ($expr:expr, [
		$($name:ident ( $($fnarg:ident $(: $fnargty:ty)?),* |
						$($arg:ident = $val:expr),* )
		),*
	]) => {
        $(
			#[allow(clippy::wrong_self_convention)]
			pub fn $name($($fnarg$(: $fnargty)?),*) -> Self {
				$(let $arg = $val;)*
					$expr
			}
		)*
    };
}

// Trait used as bound on the type parameter to `ExprImpl` that
// controls the parametrization of the expression. Implementations
// define a ZST that explicitly implements `ExprParam` instead, and
// implements this trait via the blanket implementation below. This
// supertrait / subtrait setup is a workaround to allow us to
// constrain the GAT (without repeating the bounds at every
// use-site). See:
// https://users.rust-lang.org/t/confused-about-bounds-in-conjunction-with-gats/117357/9
pub trait ExprType
where
    Self: ExprParam<
        Param<u64>: PartialEq + Eq + PartialOrd + Ord + Hash + Clone + Parse + Display + Debug,
    >,
    Self: ExprParam<
        Param<OrderedFloat<f64>>: PartialEq
                                      + Eq
                                      + PartialOrd
                                      + Ord
                                      + Hash
                                      + Clone
                                      + Parse
                                      + Display
                                      + Debug,
    >,
    Self: ExprParam<
        Param<PromDuration>: PartialEq
                                 + Eq
                                 + PartialOrd
                                 + Ord
                                 + Hash
                                 + Clone
                                 + Parse
                                 + Display
                                 + Debug,
    >,
{
}

// The trait explicitly implemented by `ExprType` implementers.
pub trait ExprParam {
    type Param<T: PromParam>;
}

impl<T> ExprType for T
where
    Self: ExprParam<
        Param<u64>: PartialEq + Eq + PartialOrd + Ord + Hash + Clone + Parse + Display + Debug,
    >,
    Self: ExprParam<
        Param<OrderedFloat<f64>>: PartialEq
                                      + Eq
                                      + PartialOrd
                                      + Ord
                                      + Hash
                                      + Clone
                                      + Parse
                                      + Display
                                      + Debug,
    >,
    Self: ExprParam<
        Param<PromDuration>: PartialEq
                                 + Eq
                                 + PartialOrd
                                 + Ord
                                 + Hash
                                 + Clone
                                 + Parse
                                 + Display
                                 + Debug,
    >,
{
}

impl<T: ExprType> ExprImpl<T> {
    pub fn number(value: impl Into<T::Param<OrderedFloat<f64>>>) -> Self {
        ExprImpl::Number(value.into())
    }

    pub fn metric(metric: MetricSelector) -> Self {
        ExprImpl::Metric(metric, None, None, None)
    }

    pub fn metric_at(metric: MetricSelector, at: impl Into<T::Param<OrderedFloat<f64>>>) -> Self {
        ExprImpl::Metric(metric, None, Some(at.into()), None)
    }

    pub fn metric_offset(metric: MetricSelector, offs: Offset<T::Param<PromDuration>>) -> Self {
        ExprImpl::Metric(metric, None, None, Some(offs))
    }

    pub fn metric_at_offset(
        metric: MetricSelector,
        at: impl Into<T::Param<OrderedFloat<f64>>>,
        offs: Offset<T::Param<PromDuration>>,
    ) -> Self {
        ExprImpl::Metric(metric, None, Some(at.into()), Some(offs))
    }

    pub fn range(metric: MetricSelector, duration: T::Param<PromDuration>) -> Self {
        ExprImpl::Metric(metric, Some(duration), None, None)
    }

    pub fn range_at(
        metric: MetricSelector,
        duration: T::Param<PromDuration>,
        at: impl Into<T::Param<OrderedFloat<f64>>>,
    ) -> Self {
        ExprImpl::Metric(metric, Some(duration), Some(at.into()), None)
    }

    pub fn range_offset(
        metric: MetricSelector,
        duration: T::Param<PromDuration>,
        offs: Offset<T::Param<PromDuration>>,
    ) -> Self {
        ExprImpl::Metric(metric, Some(duration), None, Some(offs))
    }

    pub fn range_at_offset(
        metric: MetricSelector,
        duration: T::Param<PromDuration>,
        at: T::Param<OrderedFloat<f64>>,
        offs: Offset<T::Param<PromDuration>>,
    ) -> Self {
        ExprImpl::Metric(metric, Some(duration), Some(at), Some(offs))
    }

    pub fn queried_over_range(
        self,
        range: T::Param<PromDuration>,
        resolution: Option<T::Param<PromDuration>>,
    ) -> Self {
        ExprImpl::SubQuery(Box::new(self), range, resolution, None, None)
    }

    pub fn queried_over_range_at(
        self,
        range: T::Param<PromDuration>,
        resolution: Option<T::Param<PromDuration>>,
        at: impl Into<T::Param<OrderedFloat<f64>>>,
    ) -> Self {
        ExprImpl::SubQuery(Box::new(self), range, resolution, Some(at.into()), None)
    }

    pub fn queried_over_range_offset(
        self,
        range: T::Param<PromDuration>,
        resolution: Option<T::Param<PromDuration>>,
        offset: Offset<T::Param<PromDuration>>,
    ) -> Self {
        ExprImpl::SubQuery(Box::new(self), range, resolution, None, Some(offset))
    }

    pub fn queried_over_range_at_offset(
        self,
        range: T::Param<PromDuration>,
        resolution: Option<T::Param<PromDuration>>,
        at: impl Into<T::Param<OrderedFloat<f64>>>,
        offset: Offset<T::Param<PromDuration>>,
    ) -> Self {
        ExprImpl::SubQuery(
            Box::new(self),
            range,
            resolution,
            Some(at.into()),
            Some(offset),
        )
    }

    pub fn rate(self) -> Self {
        ExprImpl::Rate(Box::new(self))
    }

    pub fn bin_op(self, op: BinOp, other: Self) -> Self {
        ExprImpl::Binary(op, None, Box::new(self), Box::new(other))
    }

    pub fn bin_op_on(self, op: BinOp, labels: Vec<LabelName>, other: Self) -> Self {
        ExprImpl::Binary(
            op,
            Some(BinModifier::On(labels)),
            Box::new(self),
            Box::new(other),
        )
    }

    pub fn bin_op_ignoring(self, op: BinOp, labels: Vec<LabelName>, other: Self) -> Self {
        ExprImpl::Binary(
            op,
            Some(BinModifier::Ignoring(labels)),
            Box::new(self),
            Box::new(other),
        )
    }

    define_for!(
        self.bin_op(op, other.into()), [
            is_eq(self, other: impl Into<Self> | op = BinOp::Eq),
            is_ne(self, other: impl Into<Self> | op = BinOp::Ne),
            is_gt(self, other: impl Into<Self> | op = BinOp::Gt),
            is_ge(self, other: impl Into<Self> | op = BinOp::Ge),
            is_le(self, other: impl Into<Self> | op = BinOp::Le),
            is_lt(self, other: impl Into<Self> | op = BinOp::Lt),
            // add(self, other: impl IntoSelf> | op = BinOp::Add),
            // sub(self, other: impl IntoSelf> | op = BinOp::Sub),
            // mul(self, other: impl Into<Self> | op = BinOp::Mul),
            // div(self, other: impl Into<Self> | op = BinOp::Div),
            modulo(self, other: impl Into<Self> | op = BinOp::Mod),
            atan2(self, other: impl Into<Self> | op = BinOp::Atan2),
            pow(self, other: impl Into<Self> | op = BinOp::Pow),
            and(self, other: impl Into<Self> | op = BinOp::And),
            or(self, other: impl Into<Self> | op = BinOp::Or),
            unless(self, other: impl Into<Self> | op = BinOp::Unless)
        ]
    );

    define_for!(
        self.bin_op_on(op, labels, other.into()), [
            is_eq_on(self, labels: Vec<LabelName>, other: impl Into<Self> | op = BinOp::Eq),
            is_ne_on(self, labels: Vec<LabelName>, other: impl Into<Self> | op = BinOp::Ne),
            is_gt_on(self, labels: Vec<LabelName>, other: impl Into<Self> | op = BinOp::Gt),
            is_ge_on(self, labels: Vec<LabelName>, other: impl Into<Self> | op = BinOp::Ge),
            is_le_on(self, labels: Vec<LabelName>, other: impl Into<Self> | op = BinOp::Le),
            is_lt_on(self, labels: Vec<LabelName>, other: impl Into<Self> | op = BinOp::Lt),
            add_on(self, labels: Vec<LabelName>, other: impl Into<Self> | op = BinOp::Add),
            sub_on(self, labels: Vec<LabelName>, other: impl Into<Self> | op = BinOp::Sub),
            mul_on(self, labels: Vec<LabelName>, other: impl Into<Self> | op = BinOp::Mul),
            div_on(self, labels: Vec<LabelName>, other: impl Into<Self> | op = BinOp::Div),
            modulo_on(self, labels: Vec<LabelName>, other: impl Into<Self> | op = BinOp::Mod),
            atan2_on(self, labels: Vec<LabelName>, other: impl Into<Self> | op = BinOp::Atan2),
            pow_on(self, labels: Vec<LabelName>, other: impl Into<Self> | op = BinOp::Pow),
            and_on(self, labels: Vec<LabelName>, other: impl Into<Self> | op = BinOp::And),
            or_on(self, labels: Vec<LabelName>, other: impl Into<Self> | op = BinOp::Or),
            unless_on(self, labels: Vec<LabelName>, other: impl Into<Self> | op = BinOp::Unless)
        ]
    );

    define_for!(
        self.bin_op_ignoring(op, labels, other.into()), [
            is_eq_ignoring(self, labels: Vec<LabelName>, other: impl Into<Self> | op = BinOp::Eq),
            is_ne_ignoring(self, labels: Vec<LabelName>, other: impl Into<Self> | op = BinOp::Ne),
            is_gt_ignoring(self, labels: Vec<LabelName>, other: impl Into<Self> | op = BinOp::Gt),
            is_ge_ignoring(self, labels: Vec<LabelName>, other: impl Into<Self> | op = BinOp::Ge),
            is_le_ignoring(self, labels: Vec<LabelName>, other: impl Into<Self> | op = BinOp::Le),
            is_lt_ignoring(self, labels: Vec<LabelName>, other: impl Into<Self> | op = BinOp::Lt),
            add_ignoring(self, labels: Vec<LabelName>, other: impl Into<Self> | op = BinOp::Add),
            sub_ignoring(self, labels: Vec<LabelName>, other: impl Into<Self> | op = BinOp::Sub),
            mul_ignoring(self, labels: Vec<LabelName>, other: impl Into<Self> | op = BinOp::Mul),
            div_ignoring(self, labels: Vec<LabelName>, other: impl Into<Self> | op = BinOp::Div),
            modulo_ignoring(self, labels: Vec<LabelName>, other: impl Into<Self> | op = BinOp::Mod),
            atan2_ignoring(self, labels: Vec<LabelName>, other: impl Into<Self> | op = BinOp::Atan2),
            pow_ignoring(self, labels: Vec<LabelName>, other: impl Into<Self> | op = BinOp::Pow),
            and_ignoring(self, labels: Vec<LabelName>, other: impl Into<Self> | op = BinOp::And),
            or_ignoring(self, labels: Vec<LabelName>, other: impl Into<Self> | op = BinOp::Or),
            unless_ignoring(self, labels: Vec<LabelName>, other: impl Into<Self> | op = BinOp::Unless)
        ]
    );

    pub fn aggr_over_time(self, op: AggrOverTimeOp<T>) -> Self {
        ExprImpl::AggrOverTime(op, Box::new(self))
    }

    define_for!(
        self.aggr_over_time(op),
        [
            avg_over_time(self | op = AggrOverTimeOp::Avg),
            min_over_time(self | op = AggrOverTimeOp::Min),
            max_over_time(self | op = AggrOverTimeOp::Max),
            sum_over_time(self | op = AggrOverTimeOp::Sum),
            count_time(self | op = AggrOverTimeOp::Count),
            stddev_over_time(self | op = AggrOverTimeOp::StdDev),
            stdvar_over_time(self | op = AggrOverTimeOp::StdVar),
            last_over_time(self | op = AggrOverTimeOp::Last),
            present_over_time(self | op = AggrOverTimeOp::Present),
            quantile_over_time(self, p: impl Into<T::Param<OrderedFloat<f64>>>
                               | op = AggrOverTimeOp::Quantile(p.into()))
        ]
    );

    pub fn aggr(self, op: AggrOp<T>) -> Self {
        ExprImpl::Aggr(op, None, Box::new(self))
    }

    pub fn aggr_by(self, op: AggrOp<T>, labels: Vec<LabelName>) -> Self {
        ExprImpl::Aggr(op, Some(AggrModifier::By(labels)), Box::new(self))
    }

    pub fn aggr_without(self, op: AggrOp<T>, labels: Vec<LabelName>) -> Self {
        ExprImpl::Aggr(op, Some(AggrModifier::Without(labels)), Box::new(self))
    }

    define_for!(
        self.aggr(op),
        [
            sum(self | op = AggrOp::Sum),
            min(self | op = AggrOp::Min),
            max(self | op = AggrOp::Max),
            avg(self | op = AggrOp::Avg),
            group(self | op = AggrOp::Group),
            stddev(self | op = AggrOp::StdDev),
            stdvar(self | op = AggrOp::StdVar),
            count(self | op = AggrOp::Count),
            count_values(self, label: LabelName | op = AggrOp::CountValues(label)),
            bottomk(self, n: impl Into<T::Param<u64>> | op = AggrOp::BottomK(n.into())),
            topk(self, n: impl Into<T::Param<u64>> | op = AggrOp::TopK(n.into())),
            quantile(self, p: impl Into<T::Param<OrderedFloat<f64>>> | op = AggrOp::Quantile(p.into()))
        ]
    );

    define_for!(
        self.aggr_by(op, labels),
        [
            sum_by(self, labels: Vec<LabelName> | op = AggrOp::Sum),
            min_by(self, labels: Vec<LabelName> | op = AggrOp::Min),
            max_by(self, labels: Vec<LabelName> | op = AggrOp::Max),
            avg_by(self, labels: Vec<LabelName> | op = AggrOp::Avg),
            group_by(self, labels: Vec<LabelName> | op = AggrOp::Group),
            stddev_by(self, labels: Vec<LabelName> | op = AggrOp::StdDev),
            stdvar_by(self, labels: Vec<LabelName> | op = AggrOp::StdVar),
            count_by(self, labels: Vec<LabelName> | op = AggrOp::Count),
            count_values_by(self, labels: Vec<LabelName>, label: LabelName
                            | op = AggrOp::CountValues(label)),
            bottomk_by(self, n: impl Into<T::Param<u64>>, labels: Vec<LabelName>
                       | op = AggrOp::BottomK(n.into())),
            topk_by(self, labels: Vec<LabelName>, n: impl Into<T::Param<u64>>
                    | op = AggrOp::TopK(n.into())),
            quantile_by(self, labels: Vec<LabelName>, p: impl Into<T::Param<OrderedFloat<f64>>>
                        | op = AggrOp::Quantile(p.into()))
        ]
    );

    define_for!(
        self.aggr_without(op, labels),
        [
            sum_without(self, labels: Vec<LabelName> | op = AggrOp::Sum),
            min_without(self, labels: Vec<LabelName> | op = AggrOp::Min),
            max_without(self, labels: Vec<LabelName> | op = AggrOp::Max),
            avg_without(self, labels: Vec<LabelName> | op = AggrOp::Avg),
            group_without(self, labels: Vec<LabelName> | op = AggrOp::Group),
            stddev_without(self, labels: Vec<LabelName> | op = AggrOp::StdDev),
            stdvar_without(self, labels: Vec<LabelName> | op = AggrOp::StdVar),
            count_without(self, labels: Vec<LabelName> | op = AggrOp::Count),
            count_values_without(self, labels: Vec<LabelName>, label: LabelName
                                 | op = AggrOp::CountValues(label)),
            bottomk_without(self, n: impl Into<T::Param<u64>>, labels: Vec<LabelName>
                            | op = AggrOp::BottomK(n.into())),
            topk_without(self, labels: Vec<LabelName>, n: impl Into<T::Param<u64>>
                         | op = AggrOp::TopK(n.into())),
            quantile_without(self, labels: Vec<LabelName>, p: impl Into<T::Param<OrderedFloat<f64>>>
                             | op = AggrOp::Quantile(p.into()))
        ]
    );

    define_for!(
        expr, [
            clamp(self, min: impl Into<T::Param<OrderedFloat<f64>>>, max: impl Into<T::Param<OrderedFloat<f64>>>
                  | expr = ExprImpl::Clamp(Box::new(self), min.into(), max.into())),
            clamp_min(self, min: impl Into<T::Param<OrderedFloat<f64>>>
                      | expr = ExprImpl::ClampMin(Box::new(self), min.into())),
            clamp_max(self, max: impl Into<T::Param<OrderedFloat<f64>>>
                      | expr = ExprImpl::ClampMax(Box::new(self), max.into()))
        ]
    );
}

impl<T: ExprType> From<MetricSelector> for ExprImpl<T> {
    fn from(metric: MetricSelector) -> Self {
        Self::metric(metric)
    }
}

impl<T: ExprType, U: Into<Self>> Add<U> for ExprImpl<T> {
    type Output = Self;
    fn add(self, rhs: U) -> Self::Output {
        self.bin_op(BinOp::Add, rhs.into())
    }
}

impl<T: ExprType, U: Into<Self>> Sub<U> for ExprImpl<T> {
    type Output = Self;
    fn sub(self, rhs: U) -> Self::Output {
        self.bin_op(BinOp::Sub, rhs.into())
    }
}

impl<T: ExprType, U: Into<Self>> Mul<U> for ExprImpl<T> {
    type Output = Self;
    fn mul(self, rhs: U) -> Self::Output {
        self.bin_op(BinOp::Mul, rhs.into())
    }
}

impl<T: ExprType, U: Into<Self>> Div<U> for ExprImpl<T> {
    type Output = Self;
    fn div(self, rhs: U) -> Self::Output {
        self.bin_op(BinOp::Div, rhs.into())
    }
}

impl<T: ExprType, U: Into<Self>> Rem<U> for ExprImpl<T> {
    type Output = Self;
    fn rem(self, rhs: U) -> Self::Output {
        self.bin_op(BinOp::Mod, rhs.into())
    }
}

impl<T: ExprType, U: Into<Self>> BitXor<U> for ExprImpl<T> {
    type Output = Self;
    fn bitxor(self, rhs: U) -> Self::Output {
        self.bin_op(BinOp::Pow, rhs.into())
    }
}

impl<T: ExprType, U: Into<Self>> BitAnd<U> for ExprImpl<T> {
    type Output = Self;
    fn bitand(self, rhs: U) -> Self::Output {
        self.bin_op(BinOp::And, rhs.into())
    }
}

impl<T: ExprType, U: Into<Self>> BitOr<U> for ExprImpl<T> {
    type Output = Self;
    fn bitor(self, rhs: U) -> Self::Output {
        self.bin_op(BinOp::Or, rhs.into())
    }
}

/*** Display implementation ***/

impl<T: ExprType> Display for ExprImpl<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", ShowExpr(self, Prec::Or, Assoc::Left))
    }
}

struct ShowExpr<'a, T: ExprType>(&'a ExprImpl<T>, Prec, Assoc);

impl<T: ExprType> Display for ShowExpr<'_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.0 {
            ExprImpl::Number(n) => {
                write!(f, "{n}")
            }
            ExprImpl::Metric(m, range, at, offset) => {
                write!(f, "{m}")?;
                if let Some(duration) = range {
                    write!(f, " [{duration}]")?;
                }
                if let Some(t) = at {
                    write!(f, " @ {t}")?;
                }
                if let Some(offs) = offset {
                    write!(f, " offset {offs}")?;
                }
                Ok(())
            }
            ExprImpl::SubQuery(expr, range, resolution, at, offset) => {
                write!(f, "({expr}) [{range}:")?;
                if let Some(duration) = resolution {
                    write!(f, "{duration}")?;
                }
                write!(f, "]")?;
                if let Some(t) = at {
                    write!(f, " @ {t}")?;
                }
                if let Some(offs) = offset {
                    write!(f, " offset {offs}")?;
                }
                Ok(())
            }
            ExprImpl::Binary(op, m, a, b) => {
                let prec = op.prec();
                let paren = self.1 > prec || self.1 == prec && self.2 != op.assoc();
                if paren {
                    write!(f, "( ")?;
                }
                write!(f, "{} {} ", ShowExpr(a, prec, Assoc::Left), op)?;
                if let Some(m) = m {
                    write!(f, "{m} ")?;
                }
                write!(f, "{}", ShowExpr(b, prec, Assoc::Right))?;
                if paren {
                    write!(f, " )")?;
                }
                Ok(())
            }
            ExprImpl::AggrOverTime(op, e) => {
                write!(f, "{}_over_time", op.operator())?;
                if let Some(p) = op.param() {
                    write!(f, "({p}, {e})")
                } else {
                    write!(f, "({e})")
                }
            }
            ExprImpl::Aggr(op, m, e) => {
                write!(f, "{}", op.operator())?;
                if let Some(m) = m {
                    write!(f, " {m} ")?;
                }
                if let Some(p) = op.param() {
                    write!(f, "({p}, {e})")
                } else {
                    write!(f, "({e})")
                }
            }
            ExprImpl::Rate(e) => write!(f, "rate({e})"),
            ExprImpl::HistogramQuantile(n, e) => write!(f, "histogram_quantile({n}, {e})"),
            ExprImpl::Clamp(e, min, max) => write!(f, "clamp({e}, {min}, {max})"),
            ExprImpl::ClampMin(e, min) => write!(f, "clamp_min({e}, {min})"),
            ExprImpl::ClampMax(e, max) => write!(f, "clamp_max({e}, {max})"),
        }
    }
}

/*** Parse implementation ***/

impl<T: ExprType> FromStr for ExprImpl<T> {
    type Err = ParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::parse_all(s)
    }
}

impl<T: ExprType> Parse for ExprImpl<T>
where
    T::Param<u64>: Parse,
    T::Param<OrderedFloat<f64>>: Parse,
    T::Param<PromDuration>: Parse,
{
    fn parse(s: &str) -> IResult<&str, Self> {
        Self::parse_binop(s)
    }
}

impl<T: ExprType> ExprImpl<T>
where
    T::Param<u64>: Parse,
    T::Param<OrderedFloat<f64>>: Parse,
    T::Param<PromDuration>: Parse,
{
    pub(crate) fn parse_term(s: &str) -> IResult<&str, Self> {
        alt((
            Self::parse_lit,
            Self::parse_histogram_quantile,
            Self::parse_aggr_over_time,
            Self::parse_clamp,
            Self::parse_clamp_min,
            Self::parse_clamp_max,
            Self::parse_aggr,
            Self::parse_rate,
            Self::parse_metric_selector_or_range,
            delimited(
                tuple((tag("("), space0)),
                Self::parse_binop,
                tuple((space0, tag(")"))),
            ),
        ))(s)
    }

    fn parse_lit(s: &str) -> IResult<&str, Self> {
        let (s, p) = T::Param::<OrderedFloat<f64>>::parse(s)?;
        Ok((s, Self::Number(p)))
    }

    fn parse_rate(s: &str) -> IResult<&str, Self> {
        let (s, _) = delimited(space0, tag("rate"), space0)(s)?;
        let (s, expr) = cut(delimited(
            char('('),
            Self::parse_binop,
            tuple((char(')'), space0)),
        ))(s)?;
        Ok((s, Self::Rate(Box::new(expr))))
    }

    fn parse_metric_selector_or_range(s: &str) -> IResult<&str, Self> {
        let (s, ms) = MetricSelector::parse(s)?;
        let (s, range) = opt(delimited(
            tuple((space0, char('['), space0)),
            T::Param::<PromDuration>::parse,
            tuple((space0, char(']'), space0)),
        ))(s)?;
        let (s, (at, offset)) = permutation((
            opt(preceded(
                tuple((space0, char('@'), space0)),
                T::Param::<OrderedFloat<f64>>::parse,
            )),
            opt(preceded(
                tuple((space0, tag("offset"), space1)),
                alt((
                    map(
                        preceded(char('-'), T::Param::<PromDuration>::parse),
                        Offset::Negative,
                    ),
                    map(T::Param::<PromDuration>::parse, Offset::Positive),
                )),
            )),
        ))(s)?;
        Ok((s, ExprImpl::Metric(ms, range, at, offset)))
    }

    fn parse_histogram_quantile(s: &str) -> IResult<&str, Self> {
        preceded(
            tuple((space0, tag("histogram_quantile"), space0)),
            cut(delimited(
                char('('),
                map(
                    tuple((
                        T::Param::<OrderedFloat<f64>>::parse,
                        preceded(char(','), Self::parse_binop),
                    )),
                    |(n, e)| ExprImpl::HistogramQuantile(n, Box::new(e)),
                ),
                char(')'),
            )),
        )(s)
    }

    fn parse_clamp(s: &str) -> IResult<&str, Self> {
        preceded(
            tuple((space0, tag("clamp"), space0)),
            delimited(
                tuple((char('('), space0)),
                cut(map(
                    tuple((
                        Self::parse_binop,
                        preceded(char(','), T::Param::<OrderedFloat<f64>>::parse),
                        preceded(char(','), T::Param::<OrderedFloat<f64>>::parse),
                    )),
                    |(e, min, max)| ExprImpl::Clamp(Box::new(e), min, max),
                )),
                tuple((space0, char(')'))),
            ),
        )(s)
    }

    fn parse_clamp_min(s: &str) -> IResult<&str, Self> {
        preceded(
            tuple((space0, tag("clamp_min"), space0)),
            delimited(
                tuple((char('('), space0)),
                cut(map(
                    tuple((
                        Self::parse_binop,
                        preceded(char(','), T::Param::<OrderedFloat<f64>>::parse),
                    )),
                    |(e, min)| ExprImpl::ClampMin(Box::new(e), min),
                )),
                tuple((space0, char(')'))),
            ),
        )(s)
    }

    fn parse_clamp_max(s: &str) -> IResult<&str, Self> {
        preceded(
            tuple((space0, tag("clamp_max"), space0)),
            delimited(
                tuple((char('('), space0)),
                cut(map(
                    tuple((
                        Self::parse_binop,
                        preceded(char(','), T::Param::<OrderedFloat<f64>>::parse),
                    )),
                    |(e, max)| ExprImpl::ClampMax(Box::new(e), max),
                )),
                tuple((space0, char(')'))),
            ),
        )(s)
    }

    fn parse_aggr_over_time(s: &str) -> IResult<&str, Self> {
        let (s, op) = terminated(preceded(space0, AggrOverTimeFn::parse), tag("_over_time"))(s)?;
        let (s, (op, expr)) = cut(delimited(
            char('('),
            tuple((AggrOverTimeOp::parser(op), Self::parse_binop)),
            char(')'),
        ))(s)?;
        Ok((s, Self::AggrOverTime(op, Box::new(expr))))
    }

    fn parse_aggr(s: &str) -> IResult<&str, Self> {
        let (s, op) = preceded(space0, AggrFn::parse)(s)?;
        let (s, modifier) = opt(AggrModifier::parse)(s)?;
        let (s, (op, expr)) = cut(delimited(
            char('('),
            tuple((AggrOp::parser(op), Self::parse_binop)),
            char(')'),
        ))(s)?;
        let (s, _) = space0(s)?;
        Ok((s, Self::Aggr(op, modifier, Box::new(expr))))
    }
}

/*** Other impls. ***/

impl ExprImpl<Spec> {
    pub(crate) fn add_param_types<'a, M: PromParams<'a>>(
        &'a self,
        params: &mut M,
    ) -> Result<(), ParamTypeError> {
        match &self {
            ExprImpl::Number(p) => p.add_param_type(params),
            ExprImpl::Metric(_, range, at, offset) => {
                if let Some(duration) = range {
                    duration.add_param_type(params)?;
                }
                if let Some(t) = at {
                    t.add_param_type(params)?;
                }
                if let Some(offs) = offset {
                    match offs {
                        Offset::Negative(duration) | Offset::Positive(duration) => {
                            duration.add_param_type(params)?;
                        }
                    }
                }
                Ok(())
            }
            ExprImpl::SubQuery(expr, range, resolution, at, offset) => {
                expr.add_param_types(params)?;
                range.add_param_type(params)?;
                if let Some(duration) = resolution {
                    duration.add_param_type(params)?;
                }
                if let Some(t) = at {
                    t.add_param_type(params)?;
                }
                if let Some(offs) = offset {
                    match offs {
                        Offset::Negative(duration) | Offset::Positive(duration) => {
                            duration.add_param_type(params)?;
                        }
                    }
                }
                Ok(())
            }
            ExprImpl::Binary(_, _, a, b) => {
                a.add_param_types(params)?;
                b.add_param_types(params)
            }
            ExprImpl::Rate(e) => e.add_param_types(params),
            ExprImpl::AggrOverTime(op, e) => {
                op.add_param_types(params)?;
                e.add_param_types(params)
            }
            ExprImpl::Aggr(op, _, e) => {
                op.add_param_types(params)?;
                e.add_param_types(params)
            }
            ExprImpl::HistogramQuantile(p, e) => {
                p.add_param_type(params)?;
                e.add_param_types(params)
            }
            ExprImpl::Clamp(e, min, max) => {
                e.add_param_types(params)?;
                min.add_param_type(params)?;
                max.add_param_type(params)
            }
            ExprImpl::ClampMin(e, min) => {
                e.add_param_types(params)?;
                min.add_param_type(params)
            }
            ExprImpl::ClampMax(e, max) => {
                e.add_param_types(params)?;
                max.add_param_type(params)
            }
        }
    }
}
