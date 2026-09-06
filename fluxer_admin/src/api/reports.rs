// SPDX-License-Identifier: AGPL-3.0-or-later

use super::client::{AdminApiClient, ApiError, ApiResult};
use super::types::{
    ListReportsResponse, ReportEntry, ResolveReportResponse, SearchReportsResponse,
};

impl AdminApiClient {
    pub async fn list_reports(
        &self,
        status: Option<i32>,
        limit: u32,
        offset: Option<u32>,
    ) -> ApiResult<ListReportsResponse> {
        let status = status.map(report_status).transpose()?.unwrap_or_default();
        let limit = limit.to_string();
        let offset = offset.map(|value| value.to_string()).unwrap_or_default();
        let query_params = [
            ("status", status),
            ("limit", limit.as_str()),
            ("offset", offset.as_str()),
        ];
        self.get("/admin/reports", Some(&query_params)).await
    }

    pub async fn get_report(&self, report_id: &str) -> ApiResult<ReportEntry> {
        let response = self
            .generated()
            .get_admin_report(report_id)
            .await
            .map_err(|e| self.generated_error(e))?;
        self.generated_value(response.into_inner())
    }

    pub async fn resolve_report(
        &self,
        report_id: &str,
        public_comment: Option<&str>,
        audit_log_reason: Option<&str>,
    ) -> ApiResult<ResolveReportResponse> {
        let mut body = serde_json::json!({"status": "resolved"});
        if let Some(public_comment) = public_comment {
            body["public_comment"] = serde_json::Value::from(public_comment);
        }
        self.patch_with_reason(
            &format!("/admin/reports/{}", urlencoding::encode(report_id)),
            Some(&body),
            audit_log_reason,
        )
        .await
    }

    #[allow(clippy::too_many_arguments)]
    pub async fn search_reports(
        &self,
        query: Option<&str>,
        status: Option<i32>,
        report_type: Option<i32>,
        category: Option<&str>,
        reporter_id: Option<&str>,
        reported_user_id: Option<&str>,
        reported_guild_id: Option<&str>,
        reported_channel_id: Option<&str>,
        guild_context_id: Option<&str>,
        resolved_by_admin_id: Option<&str>,
        sort_by: Option<&str>,
        sort_order: Option<&str>,
        limit: u32,
        offset: u32,
    ) -> ApiResult<SearchReportsResponse> {
        let status = status.map(report_status).transpose()?.unwrap_or_default();
        let report_type = report_type
            .map(report_type_name)
            .transpose()?
            .unwrap_or_default();
        let sort_by = sort_by.map(report_sort_by).transpose()?.unwrap_or_default();
        let limit = limit.to_string();
        let offset = offset.to_string();
        let query_params = [
            ("q", query.unwrap_or_default()),
            ("status", status),
            ("report_type", report_type),
            ("category", category.unwrap_or_default()),
            ("reporter_id", reporter_id.unwrap_or_default()),
            ("reported_user_id", reported_user_id.unwrap_or_default()),
            ("reported_guild_id", reported_guild_id.unwrap_or_default()),
            (
                "reported_channel_id",
                reported_channel_id.unwrap_or_default(),
            ),
            ("guild_context_id", guild_context_id.unwrap_or_default()),
            (
                "resolved_by_admin_id",
                resolved_by_admin_id.unwrap_or_default(),
            ),
            ("sort_by", sort_by),
            ("sort_order", sort_order.unwrap_or_default()),
            ("limit", limit.as_str()),
            ("offset", offset.as_str()),
        ];
        self.get("/admin/reports", Some(&query_params)).await
    }

    pub async fn search_reports_by_reporter(
        &self,
        reporter_id: &str,
        limit: u32,
        offset: u32,
    ) -> ApiResult<SearchReportsResponse> {
        self.search_reports(
            None,
            None,
            None,
            None,
            Some(reporter_id),
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            limit,
            offset,
        )
        .await
    }

    pub async fn search_reports_by_reported_user(
        &self,
        reported_user_id: &str,
        limit: u32,
        offset: u32,
    ) -> ApiResult<SearchReportsResponse> {
        self.search_reports(
            None,
            None,
            None,
            None,
            None,
            Some(reported_user_id),
            None,
            None,
            None,
            None,
            None,
            None,
            limit,
            offset,
        )
        .await
    }
}

fn report_status(value: i32) -> ApiResult<&'static str> {
    match value {
        0 => Ok("pending"),
        1 => Ok("resolved"),
        other => Err(ApiError::Parse(format!("unknown report status: {other}"))),
    }
}

fn report_type_name(value: i32) -> ApiResult<&'static str> {
    match value {
        0 => Ok("message"),
        1 => Ok("user"),
        2 => Ok("guild"),
        other => Err(ApiError::Parse(format!("unknown report type: {other}"))),
    }
}

fn report_sort_by(value: &str) -> ApiResult<&'static str> {
    match value {
        "created_at" | "createdAt" => Ok("created_at"),
        "reported_at" | "reportedAt" => Ok("reported_at"),
        "resolved_at" | "resolvedAt" => Ok("resolved_at"),
        other => Err(ApiError::Parse(format!(
            "unknown report sort field: {other}"
        ))),
    }
}
