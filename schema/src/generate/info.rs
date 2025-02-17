/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::collections::BTreeSet;
#[cfg(all(not(target_family = "wasm"), feature = "tokio"))]
use std::path::PathBuf;

use prometheus_core::LabelName;
use serde::{Deserialize, Serialize};

use crate::{ItemName, MetricSelector};

use super::one_to_many::OneToMany;

#[derive(Serialize, Deserialize, Clone, Default, Debug)]
#[cfg_attr(feature = "apistos", derive(apistos::ApiComponent))]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
pub struct GenInfo {
    pub query: MetricSelector,
    pub hints: Hints,
}

#[derive(Serialize, Deserialize, Clone, Default, Debug)]
#[cfg_attr(feature = "apistos", derive(apistos::ApiComponent))]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
pub struct Hints {
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub choose: Vec<Choose>,
    #[serde(default, skip_serializing_if = "OneToMany::is_empty")]
    pub rename: OneToMany<ItemName, MetricSelector>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub split_by: Vec<SplitBy>,
    // pub assert: BTreeMap<ItemName, BTreeSet<LabelName>>,
    // pub optional: BTreeMap<ItemName, BTreeSet<LabelName>>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
pub struct Choose {
    pub query: MetricSelector,
    pub choice: BTreeSet<LabelName>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
pub struct SplitBy {
    #[serde(default)]
    pub query: MetricSelector,
    pub label: LabelName,
}

impl GenInfo {
    #[cfg(all(not(target_family = "wasm"), feature = "tokio"))]
    pub async fn load(path: &std::path::Path) -> Result<Self, LoadGenInfoError> {
        let data = tokio::fs::read(path)
            .await
            .map_err(|e| LoadGenInfoError::ReadFile(path.to_path_buf(), e))?;
        serde_yaml::from_slice(&data)
            .map_err(|e| LoadGenInfoError::DeserializeFile(path.to_path_buf(), e))
    }
}

#[cfg(all(not(target_family = "wasm"), feature = "tokio"))]
#[derive(thiserror::Error, Debug)]
pub enum LoadGenInfoError {
    #[error("failed to read file {}: {1}", .0.display())]
    ReadFile(PathBuf, std::io::Error),
    #[error("failed to deserialize file {}: {1}", .0.display())]
    DeserializeFile(PathBuf, serde_yaml::Error),
}

impl Hints {
    pub fn root_name(&self) -> ItemName {
        ItemName::new("root")
    }

    //     pub(crate) fn item_name(
    //         &self,
    //         gen: &GenItemName,
    //         renames: Option<&mut BTreeMap<ItemName, ItemName>>,
    //     ) -> ItemName {
    //         let name = ItemName(CustomItemName(self, gen).to_string());
    //         if let Some(renames) = renames {
    //             let orig_name = ItemName(gen.to_string());
    //             if orig_name != name {
    //                 renames.insert(name.clone(), orig_name);
    //             }
    //         }
    //         name
    //     }
}

// struct CustomItemName<'a>(&'a Hints, &'a GenItemName<'a>);

// impl Display for CustomItemName<'_> {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         if let Some(custom) = self.0.rename.get_by_left(&ItemName(self.1.to_string())) {
//             write!(f, "{custom}")
//         } else {
//             match &self.1 {
//                 GenItemName::Root => write!(f, "{}", self.1),
//                 GenItemName::ByLabels(parent, labels) => {
//                     write!(f, "{}-", Self(self.0, parent))?;
//                     labels.iter().try_for_each(|label| write!(f, "-{label}"))
//                 }
//                 GenItemName::ByValues(parent, labels) => {
//                     write!(f, "{}-", Self(self.0, parent))?;
//                     labels
//                         .iter()
//                         .try_for_each(|(label, value)| write!(f, "-{label}:{value}"))
//                 }
//             }
//         }
//     }
// }
