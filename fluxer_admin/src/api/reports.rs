// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::api::generated::snowflake;

use super::client::{AdminApiClient, ApiError, ApiResult};
use super::types::{
    ListReportsResponse, ReportEntry, ResolveReportResponse, SearchReportsResponse,
};

#[derive(Default)]
pub struct SearchReportsParams<'a> {
    pub query: Option<&'a str>,
    pub status: Option<i32>,
    pub report_type: Option<i32>,
    pub category: Option<&'a str>,
    pub reporter_id: Option<&'a str>,
    pub reported_user_id: Option<&'a str>,
    pub reported_guild_id: Option<&'a str>,
    pub reported_channel_id: Option<&'a str>,
    pub guild_context_id: Option<&'a str>,
    pub resolved_by_admin_id: Option<&'a str>,
    pub sort_by: Option<&'a str>,
    pub sort_order: Option<&'a str>,
    pub limit: u32,
    pub offset: u32,
}

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
            .get_admin_report(&snowflake(report_id))
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

    pub async fn search_reports(
        &self,
        params: &SearchReportsParams<'_>,
    ) -> ApiResult<SearchReportsResponse> {
        let status = params
            .status
            .map(report_status)
            .transpose()?
            .unwrap_or_default();
        let report_type = params
            .report_type
            .map(report_type_name)
            .transpose()?
            .unwrap_or_default();
        let sort_by = params
            .sort_by
            .map(report_sort_by)
            .transpose()?
            .unwrap_or_default();
        let limit = params.limit.to_string();
        let offset = params.offset.to_string();
        let query_params = [
            ("q", params.query.unwrap_or_default()),
            ("status", status),
            ("report_type", report_type),
            ("category", params.category.unwrap_or_default()),
            ("reporter_id", params.reporter_id.unwrap_or_default()),
            (
                "reported_user_id",
                params.reported_user_id.unwrap_or_default(),
            ),
            (
                "reported_guild_id",
                params.reported_guild_id.unwrap_or_default(),
            ),
            (
                "reported_channel_id",
                params.reported_channel_id.unwrap_or_default(),
            ),
            (
                "guild_context_id",
                params.guild_context_id.unwrap_or_default(),
            ),
            (
                "resolved_by_admin_id",
                params.resolved_by_admin_id.unwrap_or_default(),
            ),
            ("sort_by", sort_by),
            ("sort_order", params.sort_order.unwrap_or_default()),
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
        self.search_reports(&SearchReportsParams {
            reporter_id: Some(reporter_id),
            limit,
            offset,
            ..Default::default()
        })
        .await
    }

    pub async fn search_reports_by_reported_user(
        &self,
        reported_user_id: &str,
        limit: u32,
        offset: u32,
    ) -> ApiResult<SearchReportsResponse> {
        self.search_reports(&SearchReportsParams {
            reported_user_id: Some(reported_user_id),
            limit,
            offset,
            ..Default::default()
        })
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn report_statuses_preserve_the_closed_wire_mapping() {
        for (value, expected) in [(0, "pending"), (1, "resolved")] {
            assert_eq!(report_status(value).expect("supported status"), expected);
        }
        for value in [-1, 2] {
            assert_eq!(
                report_status(value)
                    .expect_err("unknown status")
                    .to_string(),
                format!("parse error: unknown report status: {value}")
            );
        }
    }

    #[test]
    fn report_types_preserve_the_closed_wire_mapping() {
        for (value, expected) in [(0, "message"), (1, "user"), (2, "guild")] {
            assert_eq!(report_type_name(value).expect("supported type"), expected);
        }
        for value in [-1, 3] {
            assert_eq!(
                report_type_name(value)
                    .expect_err("unknown type")
                    .to_string(),
                format!("parse error: unknown report type: {value}")
            );
        }
    }

    #[test]
    fn report_sort_fields_accept_only_the_existing_aliases() {
        for (field, expected) in [
            ("createdAt", "created_at"),
            ("created_at", "created_at"),
            ("reportedAt", "reported_at"),
            ("reported_at", "reported_at"),
            ("resolvedAt", "resolved_at"),
            ("resolved_at", "resolved_at"),
        ] {
            assert_eq!(
                report_sort_by(field).expect("supported sort field"),
                expected
            );
        }
        assert_eq!(
            report_sort_by("unknown")
                .expect_err("unknown sort field")
                .to_string(),
            "parse error: unknown report sort field: unknown"
        );
    }
}
