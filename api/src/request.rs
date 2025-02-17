/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use serde::{de::DeserializeOwned, Serialize};

use crate::sealed::Sealed;

pub trait PromRequest: Serialize + Sealed {
    const PATH: &'static str;
    type Response: DeserializeOwned;
}
