/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::{fmt::Display, str::FromStr};

use graph::{BTreeGraph, Ref};
use serde::{Deserialize, Serialize};

use crate::schema::{self, ResolveError};

#[derive(Serialize, Deserialize, Eq, PartialEq, Ord, PartialOrd, Hash, Clone, Debug)]
#[cfg_attr(feature = "apistos", derive(apistos::ApiComponent))]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
pub struct ModuleName(pub(crate) String);

#[derive(Serialize, Deserialize, Eq, PartialEq, Ord, PartialOrd, Hash, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
pub struct ItemName(pub(crate) String);

#[derive(Serialize, Deserialize, Eq, PartialEq, Ord, PartialOrd, Hash, Clone, Debug)]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(
    feature = "tsify",
    tsify(from_wasm_abi, into_wasm_abi, type = "string")
)]
#[serde(try_from = "String", into = "String")]
pub struct QualifiedItemName(pub(crate) ModuleName, pub(crate) ItemName);

impl QualifiedItemName {
    pub fn new(module: ModuleName, item: ItemName) -> Self {
        Self(module, item)
    }

    pub fn module(&self) -> &ModuleName {
        &self.0
    }

    pub fn item(&self) -> &ItemName {
        &self.1
    }
}

#[cfg(feature = "schemars")]
impl schemars::JsonSchema for QualifiedItemName {
    fn schema_name() -> std::string::String {
        "QualifiedItemName".to_owned()
    }
    fn schema_id() -> std::borrow::Cow<'static, str> {
        std::borrow::Cow::Borrowed(std::concat!(std::module_path!(), "::", "QualifiedItemName"))
    }
    fn json_schema(gen: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
        String::json_schema(gen)
    }
}

#[derive(Serialize, Deserialize, Eq, PartialEq, Ord, PartialOrd, Hash, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "tsify", tsify(from_wasm_abi, into_wasm_abi))]
#[serde(try_from = "String", into = "String")]
pub struct ItemRef {
    pub module: Option<ModuleName>,
    pub item: ItemName,
}

#[derive(Serialize, Deserialize, Eq, PartialEq, Ord, PartialOrd, Hash, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(
    feature = "tsify",
    tsify(from_wasm_abi, into_wasm_abi, type = "string")
)]
pub struct ModuleVersion(
    #[cfg_attr(feature = "schemars", schemars(with = "String"))] pub(crate) semver::Version,
);

impl ModuleVersion {
    pub fn new(version: semver::Version) -> Self {
        Self(version)
    }
}

#[derive(Serialize, Deserialize, Eq, PartialEq, Hash, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
pub struct ModuleVersionReq(
    #[cfg_attr(feature = "schemars", schemars(with = "String"))] pub(crate) semver::VersionReq,
);

impl ModuleVersionReq {
    pub fn new(req: semver::VersionReq) -> Self {
        Self(req)
    }
}

impl ModuleName {
    pub fn new<T: Into<String>>(name: T) -> Self {
        Self(name.into())
    }
}

impl Display for ModuleName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl FromStr for ModuleName {
    type Err = ParseIdError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self(s.to_string()))
    }
}

// impl ModuleVersion {
//     pub fn new(version: semver::Version) -> Self {
//         Self(version)
//     }
// }

impl Display for ModuleVersion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Display for ModuleVersionReq {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl ItemName {
    pub fn new<T: Into<String>>(name: T) -> Self {
        Self(name.into())
    }
}

impl Display for ItemName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl FromStr for ItemName {
    type Err = ParseIdError;
    fn from_str(s: &str) -> Result<Self, ParseIdError> {
        Ok(Self(s.to_string()))
    }
}

impl QualifiedItemName {
    pub(crate) fn resolve(
        self,
        items: &BTreeGraph<QualifiedItemName, schema::Item>,
    ) -> Result<(Self, Ref<schema::Item>), ResolveError> {
        let item = items
            .get_ref(&self)
            .ok_or_else(|| ResolveError::MissingItem(self.clone()))?
            .clone();
        Ok((self, item))
    }
}

impl Display for QualifiedItemName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.0, self.1)
    }
}

impl FromStr for QualifiedItemName {
    type Err = ParseIdError;
    fn from_str(s: &str) -> Result<Self, ParseIdError> {
        let (module, item) = s.split_once(':').ok_or(ParseIdError::UnqualifiedItemName)?;
        Ok(Self(
            ModuleName(module.to_string()),
            ItemName(item.to_string()),
        ))
    }
}

impl TryFrom<String> for QualifiedItemName {
    type Error = ParseIdError;
    fn try_from(value: String) -> Result<Self, ParseIdError> {
        QualifiedItemName::from_str(&value)
    }
}

impl From<QualifiedItemName> for String {
    fn from(value: QualifiedItemName) -> Self {
        value.to_string()
    }
}

impl ItemRef {
    pub fn new(module: Option<ModuleName>, item: ItemName) -> Self {
        Self { module, item }
    }

    pub(crate) fn resolve(
        self,
        module_name: &ModuleName,
        items: &BTreeGraph<QualifiedItemName, schema::Item>,
    ) -> Result<(QualifiedItemName, Ref<schema::Item>), ResolveError> {
        QualifiedItemName(
            self.module.unwrap_or_else(|| module_name.clone()),
            self.item,
        )
        .resolve(items)
    }
}

impl Display for ItemRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(module) = &self.module {
            write!(f, "{module}:")?;
        }
        write!(f, "{}", self.item)
    }
}

impl FromStr for ItemRef {
    type Err = ParseIdError;
    fn from_str(s: &str) -> Result<Self, ParseIdError> {
        let (module, item) = s
            .split_once(':')
            .map_or((None, s), |(module, item)| (Some(module), item));
        Ok(ItemRef::new(
            module.map(ModuleName::from_str).transpose()?,
            ItemName::from_str(item)?,
        ))
    }
}

impl TryFrom<String> for ItemRef {
    type Error = ParseIdError;
    fn try_from(value: String) -> Result<Self, ParseIdError> {
        Self::from_str(&value)
    }
}

impl From<ItemRef> for String {
    fn from(value: ItemRef) -> Self {
        value.to_string()
    }
}

#[derive(thiserror::Error, Debug)]
pub enum ParseIdError {
    #[error("missing module name; please use {{module}}:{{item}}")]
    UnqualifiedItemName,
    // #[error("invalid character: '{0}'")]
    // InvalidChar(char),
}

impl ModuleVersionReq {
    pub fn verify(&self, version: &ModuleVersion) -> bool {
        self.0.matches(&version.0)
    }
}
