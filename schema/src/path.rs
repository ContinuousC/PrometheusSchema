/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::fmt::Display;

pub enum Path<'a> {
    Root,
    Step(&'a str, &'a Path<'a>),
}

impl<'a> Path<'a> {
    pub fn root() -> Self {
        Self::Root
    }
    pub fn step(&'a self, component: &'a str) -> Self {
        Self::Step(component, self)
    }
}

impl Display for Path<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Path::Root => write!(f, "/"),
            Path::Step(component, Path::Root) => {
                write!(f, "/{component}")
            }
            Path::Step(component, parent) => {
                write!(f, "{parent}/{component}")
            }
        }
    }
}
