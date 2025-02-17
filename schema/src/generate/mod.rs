/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

mod algo;
mod info;
mod label_map;
mod limited;
mod one_to_many;

pub use algo::{generate_schema, Choice, GeneratedSchema};
pub use info::{Choose, GenInfo, Hints, SplitBy};
