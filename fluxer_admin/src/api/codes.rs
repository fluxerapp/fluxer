// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::api::generated::types as generated_types;

use super::client::{AdminApiClient, ApiError, ApiResult};
use super::types::CodesResponse;

impl AdminApiClient {
    pub async fn generate_gift_codes(
        &self,
        count: u32,
        duration_type: &str,
        duration_quantity: u32,
    ) -> ApiResult<CodesResponse> {
        let body = generated_types::GenerateGiftCodesRequest {
            count: crate::api::generated::nonzero_u32(count, "count").map_err(ApiError::Parse)?,
            duration_quantity: crate::api::generated::nonzero_u32(
                duration_quantity,
                "duration_quantity",
            )
            .map_err(ApiError::Parse)?,
            duration_type: generated_types::GenerateGiftCodesRequestDurationType::try_from(
                duration_type,
            )
            .map_err(|e| ApiError::Parse(e.to_string()))?,
        };
        let response = self
            .generated()
            .create_admin_gift_codes(&body)
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }
}
