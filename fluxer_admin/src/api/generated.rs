// SPDX-License-Identifier: AGPL-3.0-or-later

use self::types as generated_types;
use crate::api::client::{ApiError, ApiResult};

use progenitor_client::{ClientHooks, ClientInfo, Error, OperationInfo};
use reqwest::header::HeaderMap;

#[allow(
    clippy::all,
    unused_imports,
    unreachable_code,
    unused_variables,
    dead_code,
    renamed_and_removed_lints
)]
mod inner {
    include!(concat!(env!("OUT_DIR"), "/admin_api_generated.rs"));
}

pub use inner::types;

pub use inner::Client as GeneratedClient;

impl ClientHooks<HeaderMap> for GeneratedClient {
    async fn pre<E>(
        &self,
        request: &mut reqwest::Request,
        _info: &OperationInfo,
    ) -> Result<(), Error<E>> {
        let headers = request.headers_mut();
        for (name, value) in self.inner() {
            headers.entry(name.clone()).or_insert_with(|| value.clone());
        }
        Ok(())
    }
}

pub(crate) fn snowflake(value: &str) -> types::SnowflakeType {
    types::SnowflakeType::Variant0(value.to_owned())
}

pub(crate) fn number_to_u64(value: f64, field: &str) -> Result<u64, String> {
    const MAX_SAFE_INTEGER: f64 = 9_007_199_254_740_991.0;
    if !value.is_finite() || value < 0.0 || value.fract() != 0.0 || value > MAX_SAFE_INTEGER {
        return Err(format!("{field} is not a safe unsigned integer: {value}"));
    }
    Ok(value as u64)
}

pub(crate) fn i64_to_u64(value: i64, field: &str) -> Result<u64, String> {
    u64::try_from(value).map_err(|_| format!("{field} is negative: {value}"))
}

pub(crate) fn nonzero_u32(value: u32, field: &str) -> Result<std::num::NonZeroU32, String> {
    std::num::NonZeroU32::new(value).ok_or_else(|| format!("{field} must be greater than zero"))
}

pub(crate) fn user_flags(values: &[String]) -> ApiResult<Vec<generated_types::UserFlags>> {
    values
        .iter()
        .map(|value| {
            generated_types::UserFlags::try_from(value.as_str())
                .map_err(|error| ApiError::Parse(error.to_string()))
        })
        .collect()
}

pub(crate) fn guild_features(values: &[String]) -> Vec<generated_types::GuildFeatureSchema> {
    values
        .iter()
        .cloned()
        .map(generated_types::GuildFeatureSchema::from)
        .collect()
}

pub(crate) fn bool_param(value: bool) -> &'static str {
    if value { "true" } else { "false" }
}

pub(crate) fn float_to_u64(value: f64, field: &str) -> ApiResult<u64> {
    crate::api::generated::number_to_u64(value, field).map_err(ApiError::Parse)
}
