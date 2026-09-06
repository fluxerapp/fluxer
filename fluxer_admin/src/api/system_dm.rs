// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::api::generated::types as generated_types;

use super::client::{AdminApiClient, ApiError, ApiResult};
use super::types::SendSystemDmResponse;

impl AdminApiClient {
    pub async fn send_system_dm(
        &self,
        user_ids: &[String],
        content: &str,
    ) -> ApiResult<SendSystemDmResponse> {
        let body = generated_types::SendSystemDmRequest {
            content: generated_types::SendSystemDmRequestContent::try_from(content)
                .map_err(|e| ApiError::Parse(e.to_string()))?,
            user_ids: user_ids
                .iter()
                .cloned()
                .map(generated_types::SnowflakeType::from)
                .collect(),
        };
        let response = self
            .generated()
            .create_admin_system_dm(&body)
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }
}
