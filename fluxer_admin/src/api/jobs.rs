// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::api::generated::types as generated_types;

use super::client::{AdminApiClient, ApiError, ApiResult};
use super::types::{
    ActiveJobsRequest, ActiveJobsResponse, CancelJobResponse, GetJobResponse, ListJobsResponse,
};

pub struct ListJobsParams {
    pub limit: u32,
    pub cursor: Option<serde_json::Value>,
    pub max_lookback_days: u32,
    pub status: Option<String>,
    pub task_type: Option<String>,
    pub requested_by_user_id: Option<String>,
}

impl AdminApiClient {
    pub async fn list_jobs(&self, params: &ListJobsParams) -> ApiResult<ListJobsResponse> {
        let cursor = params
            .cursor
            .clone()
            .map(serde_json::from_value::<generated_types::ListJobsRequestCursor>)
            .transpose()
            .map_err(|e| ApiError::Parse(e.to_string()))?;
        let status = params
            .status
            .as_deref()
            .map(generated_types::ListJobsRequestStatus::try_from)
            .transpose()
            .map_err(|e| ApiError::Parse(e.to_string()))?;
        let body = generated_types::ListJobsRequest {
            cursor,
            limit: Some(
                crate::api::generated::nonzero_u32(params.limit, "limit")
                    .map_err(ApiError::Parse)?,
            ),
            max_lookback_days: Some(
                crate::api::generated::nonzero_u32(params.max_lookback_days, "max_lookback_days")
                    .map_err(ApiError::Parse)?,
            ),
            requested_by_user_id: params
                .requested_by_user_id
                .as_ref()
                .cloned()
                .map(generated_types::SnowflakeType::from),
            status,
            task_type: params.task_type.clone(),
        };
        let response = self
            .generated()
            .list_jobs(&body)
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn get_job(&self, job_id: &str) -> ApiResult<GetJobResponse> {
        let body = generated_types::GetJobRequest {
            job_id: generated_types::SnowflakeType::from(job_id.to_owned()),
        };
        let response = self
            .generated()
            .get_job(&body)
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn cancel_job(
        &self,
        job_id: &str,
        audit_log_reason: Option<&str>,
    ) -> ApiResult<CancelJobResponse> {
        let body = generated_types::CancelJobRequest {
            job_id: generated_types::SnowflakeType::from(job_id.to_owned()),
        };
        self.post_typed_with_reason("/admin/jobs/cancel", &body, audit_log_reason)
            .await
    }

    pub async fn list_active_jobs_page(
        &self,
        page_state: Option<String>,
    ) -> ApiResult<ActiveJobsResponse> {
        let body = ActiveJobsRequest {
            limit: 200,
            page_state,
            task_type: None,
        };
        self.post_typed_with_reason("/admin/jobs/active", &body, None)
            .await
    }
}
