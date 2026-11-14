// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::api::generated::types as generated_types;

use super::client::{AdminApiClient, ApiError, ApiResult};
use super::types::AuditLogsListResponse;

pub struct SearchAuditLogsParams {
    pub query: Option<String>,
    pub admin_user_id: Option<String>,
    pub target_id: Option<String>,
    pub target_type: Option<String>,
    pub access: Option<String>,
    pub sort_by: Option<String>,
    pub sort_order: Option<String>,
    pub limit: u32,
    pub offset: u64,
}

impl AdminApiClient {
    pub async fn search_audit_logs(
        &self,
        params: &SearchAuditLogsParams,
    ) -> ApiResult<AuditLogsListResponse> {
        let access = params
            .access
            .as_deref()
            .map(audit_access)
            .transpose()?
            .map(|value| value.to_string());
        let sort_by = params
            .sort_by
            .as_deref()
            .map(audit_sort_by)
            .transpose()?
            .map(|value| value.to_string());
        let sort_order = params
            .sort_order
            .as_deref()
            .map(audit_sort_order)
            .transpose()?
            .map(|value| value.to_string());
        let limit = params.limit.to_string();
        let offset = params.offset.to_string();
        let query_params = [
            ("q", params.query.as_deref().unwrap_or_default()),
            (
                "admin_user_id",
                params.admin_user_id.as_deref().unwrap_or_default(),
            ),
            (
                "target_type",
                params.target_type.as_deref().unwrap_or_default(),
            ),
            ("target_id", params.target_id.as_deref().unwrap_or_default()),
            ("access", access.as_deref().unwrap_or_default()),
            ("sort_by", sort_by.as_deref().unwrap_or_default()),
            ("sort_order", sort_order.as_deref().unwrap_or_default()),
            ("limit", limit.as_str()),
            ("offset", offset.as_str()),
        ];
        self.get("/admin/audit-logs", Some(&query_params)).await
    }
}

fn audit_access(value: &str) -> ApiResult<generated_types::ListAdminAuditLogsAccess> {
    generated_types::ListAdminAuditLogsAccess::try_from(value)
        .map_err(|e| ApiError::Parse(e.to_string()))
}

fn audit_sort_by(value: &str) -> ApiResult<generated_types::ListAdminAuditLogsSortBy> {
    let value = match value {
        "created_at" => "createdAt",
        value => value,
    };
    generated_types::ListAdminAuditLogsSortBy::try_from(value)
        .map_err(|e| ApiError::Parse(e.to_string()))
}

fn audit_sort_order(value: &str) -> ApiResult<generated_types::ListAdminAuditLogsSortOrder> {
    generated_types::ListAdminAuditLogsSortOrder::try_from(value)
        .map_err(|e| ApiError::Parse(e.to_string()))
}
