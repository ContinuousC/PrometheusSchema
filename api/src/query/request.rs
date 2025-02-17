/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use serde::{Deserialize, Serialize};

use crate::{sealed::Sealed, PromRequest};

use super::{
    params::{InstantQueryParams, PromQueryParams, RangeQueryParams},
    response::GenericQueryResponse,
};

/// A prometheus instant query.
pub type InstantQuery<Q> = Query<Q, InstantQueryParams>;

/// A prometheus range query.
pub type RangeQuery<Q> = Query<Q, RangeQueryParams>;

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
pub struct Query<Q, P: PromQueryParams> {
    pub query: Q,
    #[serde(flatten)]
    pub params: P,
}

impl<Q: Serialize, P: PromQueryParams> Sealed for Query<Q, P> {}
impl<Q: Serialize, P: PromQueryParams> PromRequest for Query<Q, P> {
    const PATH: &'static str = P::PATH;
    type Response = GenericQueryResponse;
}
