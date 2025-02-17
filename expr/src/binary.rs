/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::fmt::Display;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::space0,
    combinator::{cut, map, opt},
    sequence::{delimited, preceded, tuple},
    IResult,
};
use ordered_float::OrderedFloat;
use prometheus_core::LabelName;

use crate::{
    expr_impl::{ExprImpl, ExprType},
    parse::Parse,
    selectors::ShowLabels,
    PromDuration,
};

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy, Debug)]
pub enum BinOp {
    Eq,
    Ne,
    Gt,
    Ge,
    Le,
    Lt,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Atan2,
    Pow,
    And,
    Or,
    Unless,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug)]
pub enum BinModifier {
    On(Vec<LabelName>),
    Ignoring(Vec<LabelName>),
}

#[derive(Eq, PartialEq, Ord, PartialOrd, Clone, Copy)]
pub(crate) enum Prec {
    Or,     // or
    And,    // and, unless
    Eq,     // ==, !=, <=, <, >=, >
    AddSub, // +, -
    MulDiv, // *, /, %, atan2
    Pow,    // ^
}

#[derive(Eq, PartialEq, Clone, Copy)]
pub(crate) enum Assoc {
    Left,
    Right,
}

impl BinOp {
    pub(crate) fn prec(&self) -> Prec {
        match self {
            BinOp::Pow => Prec::Pow,
            BinOp::Mul | BinOp::Div | BinOp::Mod | BinOp::Atan2 => Prec::MulDiv,
            BinOp::Add | BinOp::Sub => Prec::AddSub,
            BinOp::Eq | BinOp::Ne | BinOp::Gt | BinOp::Ge | BinOp::Le | BinOp::Lt => Prec::Eq,
            BinOp::And | BinOp::Unless => Prec::And,
            BinOp::Or => Prec::Or,
        }
    }

    pub(crate) fn assoc(&self) -> Assoc {
        match self {
            BinOp::Pow => Assoc::Right,
            _ => Assoc::Left,
        }
    }
}

impl Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinOp::Eq => write!(f, "=="),
            BinOp::Ne => write!(f, "!="),
            BinOp::Gt => write!(f, ">"),
            BinOp::Ge => write!(f, ">="),
            BinOp::Le => write!(f, "<="),
            BinOp::Lt => write!(f, "<"),
            BinOp::Add => write!(f, "+"),
            BinOp::Sub => write!(f, "-"),
            BinOp::Mul => write!(f, "*"),
            BinOp::Div => write!(f, "/"),
            BinOp::Mod => write!(f, "%"),
            BinOp::Atan2 => write!(f, "atan2"),
            BinOp::Pow => write!(f, "^"),
            BinOp::And => write!(f, "and"),
            BinOp::Or => write!(f, "or"),
            BinOp::Unless => write!(f, "unless"),
        }
    }
}

impl Display for BinModifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::On(ls) => write!(f, "on ({})", ShowLabels(ls)),
            Self::Ignoring(ls) => write!(f, "ignoring ({})", ShowLabels(ls)),
        }
    }
}

impl Parse for BinModifier {
    fn parse(s: &str) -> IResult<&str, Self> {
        delimited(
            space0,
            alt((
                preceded(tag("on"), cut(map(Vec::<LabelName>::parse, Self::On))),
                preceded(
                    tag("ignoring"),
                    cut(map(Vec::<LabelName>::parse, Self::Ignoring)),
                ),
            )),
            space0,
        )(s)
    }
}

macro_rules! binary_ops {
	($name:ident) => {};
    ($name:ident lassoc [$(($parser:expr, $op:expr)),+],
	 $next:ident $($assoc:ident [$(($next_parser:expr, $next_op:expr)),+],
				   $next_next:ident)*) => {
        fn $name(s: &str) -> IResult<&str, Self> {
			let (mut s, mut res) = Self::$next(s)?;
			loop {
				$(if let Ok((t, (modifier, right))) = preceded(delimited(space0, $parser, space0), cut(tuple((opt(BinModifier::parse), Self::$next))))(s) {
					s = t;
					res = Self::Binary($op, modifier, Box::new(res), Box::new(right));
				} else)+ {
					return Ok((s, res))
				}
			}
        }
        binary_ops!($next $($assoc [$(($next_parser, $next_op)),+], $next_next)*);
    };
    ($name:ident rassoc [$(($parser:expr, $op:expr)),+],
	 $next:ident $($assoc:ident [$(($next_parser:expr, $next_op:expr)),+],
				   $next_next:ident)*) => {
        fn $name(s: &str) -> IResult<&str, Self> {
			let (s, left) = Self::$next(s)?;
			$(if let Ok((s, (modifier, right))) = preceded(delimited(space0, $parser,space0), cut(tuple((opt(BinModifier::parse), Self::$name))))(s) {
				Ok((s, Self::Binary($op, modifier, Box::new(left), Box::new(right))))
			} else)+ {
				Ok((s, left))
			}
        }
        binary_ops!($next $($assoc [$(($next_parser, $next_op)),+], $next_next)*);
    };
}

impl<T: ExprType> ExprImpl<T>
where
    T::Param<u64>: Parse,
    T::Param<OrderedFloat<f64>>: Parse,
    T::Param<PromDuration>: Parse,
{
    pub(crate) fn parse_binop(s: &str) -> IResult<&str, Self> {
        Self::parse_or(s)
    }

    binary_ops! {
        parse_or lassoc [
            (tag("or"), BinOp::Or)],
        parse_and_unless lassoc [
            (tag("and"), BinOp::And),
            (tag("unless"), BinOp::Unless)
        ],
        parse_comparison lassoc [
            (tag("=="), BinOp::Eq),
            (tag("!="), BinOp::Ne),
            (tag(">="), BinOp::Ge),
            (tag("<="), BinOp::Le),
            (tag(">"), BinOp::Gt),
            (tag("<"), BinOp::Lt)
        ],
        parse_add_sub lassoc [
            (tag("+"), BinOp::Add), (tag("-"), BinOp::Sub)
        ],
        parse_mul_div lassoc [
            (tag("*"), BinOp::Mul),
            (tag("/"), BinOp::Div),
            (tag("%"), BinOp::Mod),
            (tag("atan2"), BinOp::Atan2)
        ],
        parse_pow rassoc [
            (tag("^"), BinOp::Pow)
        ],
        parse_term
    }
}
