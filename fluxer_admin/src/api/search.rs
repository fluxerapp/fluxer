// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::api::generated::float_to_u64;

use crate::api::generated::{snowflake, types as generated_types};

use super::client::{AdminApiClient, ApiError, ApiResult};
use super::types::{IndexRefreshStatusResponse, RefreshSearchIndexResponse};

impl AdminApiClient {
    pub async fn refresh_search_index(
        &self,
        index_type: &str,
        guild_id: Option<&str>,
    ) -> ApiResult<RefreshSearchIndexResponse> {
        let index_type =
            generated_types::CreateAdminSearchIndexRefreshIndexName::try_from(index_type)
                .map_err(|e| ApiError::Parse(e.to_string()))?;
        let body = generated_types::RefreshSearchIndexRequest {
            guild_id: guild_id.map(snowflake),
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
        index_refresh_status(response.into_inner())
    }
}

fn index_refresh_status(
    response: generated_types::IndexRefreshStatusResponse,
) -> ApiResult<IndexRefreshStatusResponse> {
    match response {
        generated_types::IndexRefreshStatusResponse::Variant0 { status } => {
            Ok(IndexRefreshStatusResponse::NotFound {
                status: status.to_string(),
            })
        }
        generated_types::IndexRefreshStatusResponse::Variant1 {
            status,
            index_type,
            phase,
            total,
            indexed,
            started_at,
            completed_at,
            failed_at,
            error,
        } => Ok(IndexRefreshStatusResponse::Progress {
            status: status.to_string(),
            index_type: Some(index_type),
            phase,
            total: total
                .map(|value| float_to_u64(value, "total"))
                .transpose()?,
            indexed: indexed
                .map(|value| float_to_u64(value, "indexed"))
                .transpose()?,
            started_at,
            completed_at,
            failed_at,
            error,
        }),
    }
}
