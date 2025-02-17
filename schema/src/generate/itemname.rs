/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::fmt::Display;

use super::{labelmap::LabelMap, labelset::LabelSet};

pub(crate) enum GenItemName<'a> {
    Root,
    ByLabels(&'a GenItemName<'a>, &'a LabelSet<'a>),
    ByValues(&'a GenItemName<'a>, &'a LabelMap<'a>),
}

impl Display for GenItemName<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GenItemName::Root => write!(f, "root"),
            GenItemName::ByLabels(parent, labels) => {
                write!(f, "{parent}-")?;
                labels.iter().try_for_each(|label| write!(f, "-{label}"))
            }
            GenItemName::ByValues(parent, labels) => {
                write!(f, "{parent}-")?;
                labels
                    .iter()
                    .try_for_each(|(label, value)| write!(f, "-{label}:{value}"))
            }
        }
    }
}
