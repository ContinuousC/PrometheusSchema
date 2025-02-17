/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::marker::PhantomData;

use serde::{de::IgnoredAny, Deserialize, Deserializer, Serialize};

#[derive(Serialize, Deserialize, Debug)]
#[serde(tag = "status", rename_all = "camelCase")]
pub enum Response<T> {
    Success(SuccessResponse<T>),
    Error(ErrorResponse),
}

#[derive(Serialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct SuccessResponse<T> {
    pub data: T,
    #[serde(default)]
    pub warnings: Vec<String>,
}

#[derive(thiserror::Error, Serialize, Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
#[error("{error_type}: {error}")]
pub struct ErrorResponse {
    pub error_type: String,
    pub error: String,
}

impl<T> Response<T> {
    pub fn into_result(self) -> Result<SuccessResponse<T>, ErrorResponse> {
        match self {
            Self::Success(res) => Ok(res),
            Self::Error(res) => Err(res),
        }
    }
}

impl<'de, T> Deserialize<'de> for SuccessResponse<T>
where
    T: Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        #[derive(Deserialize)]
        #[serde(rename_all = "snake_case")]
        enum Field {
            Data,
            Warnings,
            #[serde(other)]
            Ignore,
        }

        struct Visitor<T>(PhantomData<T>);

        impl<'de, T> serde::de::Visitor<'de> for Visitor<T>
        where
            T: Deserialize<'de>,
        {
            type Value = SuccessResponse<T>;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(formatter, "SuccessResponse")
            }

            fn visit_map<A>(self, mut map: A) -> std::result::Result<Self::Value, A::Error>
            where
                A: serde::de::MapAccess<'de>,
            {
                let mut data = None;
                let mut warnings = None;

                while let Some(field) = map.next_key::<Field>()? {
                    match field {
                        Field::Data => {
                            data = Some(map.next_value()?);
                        }
                        Field::Warnings => {
                            warnings = Some(map.next_value()?);
                        }
                        Field::Ignore => {
                            map.next_value::<IgnoredAny>()?;
                        }
                    }
                }

                Ok(SuccessResponse {
                    data: data
                        .or_else(|| {
                            T::deserialize(serde::de::value::UnitDeserializer::<A::Error>::new())
                                .ok()
                        })
                        .ok_or_else(|| serde::de::Error::missing_field("data"))?,
                    warnings: warnings.unwrap_or_else(Vec::new),
                })
            }
        }

        deserializer.deserialize_map(Visitor(PhantomData))
    }
}
