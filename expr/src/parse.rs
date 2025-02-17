/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use nom::{
    branch::alt,
    bytes::complete::take_while1,
    character::complete::{anychar, char, digit1, none_of, space0},
    combinator::{eof, map, map_res, value},
    multi::fold_many0,
    number::complete::double,
    sequence::{delimited, preceded, terminated, tuple},
    Finish, IResult, Parser,
};

use ordered_float::OrderedFloat;
use prometheus_core::LabelName;

// Do not expose.
pub trait Parse: Sized {
    fn parse(s: &str) -> IResult<&str, Self>;
    fn parse_all(s: &str) -> Result<Self, ParseError> {
        Ok(terminated(Self::parse, eof)(s)
            .finish()
            .map_err(|e| ParseError(e.to_string()))?
            .1)
    }
}

pub(crate) fn our_separated_list0<F, G, I, O1, O2, E, R>(
    mut sep: F,
    mut elem: G,
) -> impl FnMut(I) -> IResult<I, R, E>
where
    I: Clone + nom::InputLength,
    F: Parser<I, O1, E>,
    G: Parser<I, O2, E>,
    R: Default + Extend<O2>,
{
    move |s| {
        let mut r = R::default();
        if let Ok((mut s, first)) = elem.parse(s.clone()) {
            r.extend(std::iter::once(first));
            while let Ok((t, _)) = sep.parse(s.clone()) {
                s = t;
                let Ok((t, el)) = elem.parse(s.clone()) else {
                    break;
                };
                s = t;
                r.extend(std::iter::once(el));
            }
            Ok((s, r))
        } else {
            Ok((s, r))
        }
    }
}

impl Parse for Vec<LabelName> {
    fn parse(s: &str) -> IResult<&str, Self> {
        delimited(
            tuple((space0, char('('))),
            our_separated_list0(char(','), LabelName::parse),
            tuple((char(')'), space0)),
        )(s)
    }
}

impl Parse for LabelName {
    fn parse(s: &str) -> IResult<&str, Self> {
        map_res(parse_identifier, |name| name.parse())(s)
    }
}

pub(crate) fn parse_identifier(s: &str) -> IResult<&str, &str> {
    delimited(
        space0,
        take_while1(|c: char| c.is_ascii_alphanumeric() || c == '_'),
        space0,
    )(s)
}

impl Parse for f64 {
    fn parse(s: &str) -> IResult<&str, f64> {
        delimited(space0, double, space0)(s)
    }
}

impl Parse for OrderedFloat<f64> {
    fn parse(s: &str) -> IResult<&str, Self> {
        map(f64::parse, OrderedFloat)(s)
    }
}

impl Parse for u64 {
    fn parse(s: &str) -> IResult<&str, u64> {
        map_res(delimited(space0, digit1, space0), |s: &str| s.parse())(s)
    }
}

impl Parse for u32 {
    fn parse(s: &str) -> IResult<&str, u32> {
        map_res(delimited(space0, digit1, space0), |s: &str| s.parse())(s)
    }
}

impl Parse for u16 {
    fn parse(s: &str) -> IResult<&str, u16> {
        map_res(delimited(space0, digit1, space0), |s: &str| s.parse())(s)
    }
}

pub(crate) fn parse_string(s: &str) -> IResult<&str, String> {
    let (s, _) = char('"')(s)?;
    let (s, r) = fold_many0(
        alt((
            none_of("\"\\"),
            preceded(
                char('\\'),
                alt((value('\n', char('n')), value('\t', char('t')), anychar)),
            ),
        )),
        String::new,
        |mut r, c| {
            r.push(c);
            r
        },
    )(s)?;
    let (s, _) = char('"')(s)?;
    Ok((s, r))
}

#[derive(thiserror::Error, Debug)]
#[error("{0}")]
pub struct ParseError(String);
