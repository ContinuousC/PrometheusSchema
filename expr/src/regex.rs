/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::{fmt::Display, hash::Hash, str::FromStr};

use serde_with::{DeserializeFromStr, SerializeDisplay};

/// Regex wrapper representing regex in prometheus label
/// matchers. Prometheus docs say it uses re2 syntax. We use the regex
/// crate here, so there might be slight differences in syntax.
#[derive(SerializeDisplay, DeserializeFromStr, Clone, Debug)]
pub struct Regex(regex::Regex);

impl FromStr for Regex {
    type Err = regex::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self(format!("^(?:{s})$").parse()?))
    }
}

impl Display for Regex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = self.0.as_str();
        write!(f, "{}", &s[4..s.len() - 2])
    }
}

impl PartialEq for Regex {
    fn eq(&self, other: &Self) -> bool {
        self.0.as_str() == other.0.as_str()
    }
}

impl Eq for Regex {}

impl PartialOrd for Regex {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Regex {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.as_str().cmp(other.0.as_str())
    }
}

impl Hash for Regex {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.as_str().hash(state);
    }
}

impl Regex {
    pub fn is_match(&self, s: &str) -> bool {
        self.0.is_match(s)
    }
}
