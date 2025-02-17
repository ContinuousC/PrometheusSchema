/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::{
    collections::{BTreeMap, BTreeSet},
    fmt::Display,
    str::FromStr,
};

use prometheus_core::{LabelName, MetricName};
use serde_with::{DeserializeFromStr, SerializeDisplay};

use crate::{ItemName, ItemRef, ModuleName, QualifiedItemName};

pub trait Digestible {
    type Data<'a>: Copy;

    fn digest(&self, ctx: &mut ring::digest::Context, version: u64, data: Self::Data<'_>);

    fn digest_sha256(&self, version: u64, data: Self::Data<'_>) -> Sha256 {
        let mut ctx = ring::digest::Context::new(&ring::digest::SHA256);
        self.digest(&mut ctx, version, data);
        Sha256(ctx.finish().as_ref().try_into().unwrap())
    }
}

#[derive(
    SerializeDisplay, DeserializeFromStr, PartialEq, Eq, PartialOrd, Ord, Clone, Default, Debug,
)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "schemars", schemars(schema_with = "String::json_schema"))]
pub struct Sha256([u8; ring::digest::SHA256_OUTPUT_LEN]);

impl Display for Sha256 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.iter().try_for_each(|b| write!(f, "{b:02x}"))
    }
}

impl FromStr for Sha256 {
    type Err = Sha256ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        (s.len() == 64)
            .then_some(())
            .ok_or(Sha256ParseError::WrongLength)?;
        let mut buf = [0; 32];
        for i in (0..32).step_by(2) {
            buf[i] =
                u8::from_str_radix(s.get(i..i + 2).ok_or(Sha256ParseError::InvalidNumber)?, 16)
                    .map_err(|_| Sha256ParseError::InvalidNumber)?;
        }
        Ok(Sha256(buf))
    }
}

#[derive(thiserror::Error, Debug)]
pub enum Sha256ParseError {
    #[error("wrong length")]
    WrongLength,
    #[error("invalid hex number")]
    InvalidNumber,
}

impl Digestible for LabelName {
    type Data<'a> = ();

    fn digest(&self, ctx: &mut ring::digest::Context, version: u64, _data: Self::Data<'_>) {
        self.as_str().digest(ctx, version, ());
    }
}

impl Digestible for MetricName {
    type Data<'a> = ();

    fn digest(&self, ctx: &mut ring::digest::Context, version: u64, _data: Self::Data<'_>) {
        self.as_str().digest(ctx, version, ());
    }
}

impl Digestible for ModuleName {
    type Data<'a> = ();

    fn digest(&self, ctx: &mut ring::digest::Context, version: u64, _data: Self::Data<'_>) {
        self.0.as_str().digest(ctx, version, ());
    }
}

impl Digestible for ItemName {
    type Data<'a> = ();

    fn digest(&self, ctx: &mut ring::digest::Context, version: u64, _data: Self::Data<'_>) {
        self.0.as_str().digest(ctx, version, ());
    }
}

// Note: must agree with QualifiedItemName!
impl Digestible for ItemRef {
    type Data<'a> = &'a ModuleName;

    fn digest(&self, ctx: &mut ring::digest::Context, version: u64, data: Self::Data<'_>) {
        self.module
            .as_ref()
            .unwrap_or(data)
            .digest(ctx, version, ());
        self.item.digest(ctx, version, ())
    }
}

impl Digestible for QualifiedItemName {
    type Data<'a> = ();

    fn digest(&self, ctx: &mut ring::digest::Context, version: u64, _data: Self::Data<'_>) {
        self.0.digest(ctx, version, ());
        self.1.digest(ctx, version, ());
    }
}

impl Digestible for str {
    type Data<'a> = ();

    fn digest(&self, ctx: &mut ring::digest::Context, version: u64, _data: Self::Data<'_>) {
        let bs = self.as_bytes();
        bs.len().digest(ctx, version, ());
        ctx.update(bs);
    }
}

impl Digestible for String {
    type Data<'a> = ();

    fn digest(&self, ctx: &mut ring::digest::Context, version: u64, _data: Self::Data<'_>) {
        self.as_str().digest(ctx, version, ())
    }
}

impl Digestible for usize {
    type Data<'a> = ();

    fn digest(&self, ctx: &mut ring::digest::Context, _version: u64, _data: Self::Data<'_>) {
        ctx.update(&self.to_le_bytes());
    }
}

impl Digestible for bool {
    type Data<'a> = ();

    fn digest(&self, ctx: &mut ring::digest::Context, _version: u64, _data: Self::Data<'_>) {
        ctx.update(if *self { &[1] } else { &[0] });
    }
}

impl<T: Digestible> Digestible for Option<T> {
    type Data<'a> = T::Data<'a>;

    fn digest(&self, ctx: &mut ring::digest::Context, version: u64, data: Self::Data<'_>) {
        self.is_some().digest(ctx, version, ());
        if let Some(v) = self {
            v.digest(ctx, version, data);
        }
    }
}

impl<T: Digestible> Digestible for BTreeSet<T> {
    type Data<'a> = T::Data<'a>;

    fn digest(&self, ctx: &mut ring::digest::Context, version: u64, data: Self::Data<'_>) {
        self.len().digest(ctx, version, ());
        self.iter().for_each(|elem| elem.digest(ctx, version, data));
    }
}

impl<K: Digestible, V: Digestible> Digestible for BTreeMap<K, V> {
    type Data<'a> = (K::Data<'a>, V::Data<'a>);

    fn digest(&self, ctx: &mut ring::digest::Context, version: u64, data: Self::Data<'_>) {
        self.len().digest(ctx, version, ());
        self.iter().for_each(|(k, v)| {
            k.digest(ctx, version, data.0);
            v.digest(ctx, version, data.1);
        });
    }
}

impl Digestible for Sha256 {
    type Data<'a> = ();

    fn digest(&self, ctx: &mut ring::digest::Context, _version: u64, _data: Self::Data<'_>) {
        ctx.update(self.0.as_ref());
    }
}

impl std::ops::BitXorAssign for Sha256 {
    fn bitxor_assign(&mut self, rhs: Self) {
        self.0.iter_mut().zip(rhs.0).for_each(|(a, b)| *a ^= b);
    }
}
