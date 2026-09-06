// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::api::generated::types as generated_types;

use super::client::{AdminApiClient, ApiResult};
use super::types::{IndexRefreshStatusResponse, RefreshSearchIndexResponse};

impl AdminApiClient {
    pub async fn refresh_search_index(
        &self,
        index_type: &str,
        guild_id: Option<&str>,
    ) -> ApiResult<RefreshSearchIndexResponse> {
        let body = generated_types::RefreshSearchIndexRequest {
            guild_id: guild_id.map(|id| generated_types::SnowflakeType::from(id.to_owned())),
            user_id: None,
        };
        let response = self
            .generated()
            .create_admin_search_index_refresh(index_type, &body)
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn get_index_refresh_status(
        &self,
        job_id: &str,
    ) -> ApiResult<IndexRefreshStatusResponse> {
        let response = self
            .generated()
            .get_admin_search_index_refresh(job_id)
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }
}
