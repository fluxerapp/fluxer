// SPDX-License-Identifier: AGPL-3.0-or-later

use super::client::{AdminApiClient, ApiResult};
use super::types::{ActiveJobsResponse, CancelJobResponse, GetJobResponse, ListJobsResponse};

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
        let cursor = params.cursor.as_ref();
        let cursor_bucket_day = cursor_field(cursor, "bucket_day");
        let cursor_created_at = cursor_field(cursor, "created_at");
        let cursor_job_id = cursor_field(cursor, "job_id");
        let limit = params.limit.to_string();
        let max_lookback_days = params.max_lookback_days.to_string();
        let query_params = [
            ("limit", limit.as_str()),
            ("cursor_bucket_day", cursor_bucket_day.as_str()),
            ("cursor_created_at", cursor_created_at.as_str()),
            ("cursor_job_id", cursor_job_id.as_str()),
            ("max_lookback_days", max_lookback_days.as_str()),
            ("status", params.status.as_deref().unwrap_or_default()),
            ("task_type", params.task_type.as_deref().unwrap_or_default()),
            (
                "requested_by_user_id",
                params.requested_by_user_id.as_deref().unwrap_or_default(),
            ),
        ];
        self.get("/admin/jobs", Some(&query_params)).await
    }

    pub async fn get_job(&self, job_id: &str) -> ApiResult<GetJobResponse> {
        let response = self
            .generated()
            .get_admin_job(job_id)
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn cancel_job(
        &self,
        job_id: &str,
        audit_log_reason: Option<&str>,
    ) -> ApiResult<CancelJobResponse> {
        self.put_with_reason(
            &format!("/admin/jobs/{}/cancellation", urlencoding::encode(job_id)),
            None,
            audit_log_reason,
        )
        .await
    }

    pub async fn list_active_jobs(&self) -> ApiResult<ActiveJobsResponse> {
        let response = self
            .generated()
            .list_admin_active_jobs()
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }
}

fn cursor_field(cursor: Option<&serde_json::Value>, field: &str) -> String {
    cursor
        .and_then(|cursor| cursor.get(field))
        .and_then(serde_json::Value::as_str)
        .unwrap_or_default()
        .to_owned()
}
