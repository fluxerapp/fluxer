// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::api::generated::types as generated_types;

use super::client::{AdminApiClient, ApiError, ApiResult};
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
            total,
            indexed,
            started_at,
            completed_at,
            failed_at,
            error,
        } => Ok(IndexRefreshStatusResponse::Progress {
            status: status.to_string(),
            index_type: Some(index_type),
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

fn float_to_u64(value: f64, field: &str) -> ApiResult<u64> {
    crate::api::generated::number_to_u64(value, field).map_err(ApiError::Parse)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn maps_a_running_refresh_to_progress() {
        let json = r#"{"status":"in_progress","index_type":"users","total":50000,"indexed":1200,"started_at":"2026-09-06T00:00:00Z"}"#;
        let response: generated_types::IndexRefreshStatusResponse =
            serde_json::from_str(json).unwrap();
        match index_refresh_status(response).unwrap() {
            IndexRefreshStatusResponse::Progress {
                status,
                index_type,
                total,
                indexed,
                started_at,
                ..
            } => {
                assert_eq!(status, "in_progress");
                assert_eq!(index_type.as_deref(), Some("users"));
                assert_eq!(total, Some(50_000));
                assert_eq!(indexed, Some(1_200));
                assert_eq!(started_at.as_deref(), Some("2026-09-06T00:00:00Z"));
            }
            other => panic!("expected a progress status, got {other:?}"),
        }
    }

    #[test]
    fn maps_a_missing_refresh_to_not_found() {
        let json = r#"{"status":"not_found"}"#;
        let response: generated_types::IndexRefreshStatusResponse =
            serde_json::from_str(json).unwrap();
        match index_refresh_status(response).unwrap() {
            IndexRefreshStatusResponse::NotFound { status } => assert_eq!(status, "not_found"),
            other => panic!("expected a not found status, got {other:?}"),
        }
    }
}
