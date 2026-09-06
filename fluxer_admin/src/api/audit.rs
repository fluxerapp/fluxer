// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::api::generated::types as generated_types;

use super::client::{AdminApiClient, ApiError, ApiResult};
#[cfg(test)]
use super::types::AuditLogEntry;
use super::types::AuditLogsListResponse;

pub struct SearchAuditLogsParams {
    pub query: Option<String>,
    pub admin_user_id: Option<String>,
    pub target_id: Option<String>,
    pub target_type: Option<String>,
    pub sort_by: Option<String>,
    pub sort_order: Option<String>,
    pub limit: u32,
    pub offset: u32,
}

impl AdminApiClient {
    pub async fn search_audit_logs(
        &self,
        params: &SearchAuditLogsParams,
    ) -> ApiResult<AuditLogsListResponse> {
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
            (
                "q",
                nonempty_string(params.query.as_deref()).unwrap_or_default(),
            ),
            (
                "admin_user_id",
                nonempty_string(params.admin_user_id.as_deref()).unwrap_or_default(),
            ),
            (
                "target_type",
                nonempty_string(params.target_type.as_deref()).unwrap_or_default(),
            ),
            (
                "target_id",
                nonempty_string(params.target_id.as_deref()).unwrap_or_default(),
            ),
            ("sort_by", sort_by.unwrap_or_default()),
            ("sort_order", sort_order.unwrap_or_default()),
            ("limit", limit),
            ("offset", offset),
        ];
        let query_params: Vec<(&str, &str)> = query_params
            .iter()
            .map(|(key, value)| (*key, value.as_str()))
            .collect();
        self.get("/admin/audit-logs", Some(&query_params)).await
    }
}

#[cfg(test)]
fn audit_logs_response(
    response: generated_types::AuditLogsListResponseSchema,
) -> ApiResult<AuditLogsListResponse> {
    Ok(AuditLogsListResponse {
        logs: response.logs.into_iter().map(audit_log_entry).collect(),
        total: crate::api::generated::number_to_u64(response.total, "total")
            .map_err(ApiError::Parse)?,
    })
}

#[cfg(test)]
fn audit_log_entry(entry: generated_types::AdminAuditLogResponseSchema) -> AuditLogEntry {
    AuditLogEntry {
        log_id: String::from(entry.log_id),
        admin_user_id: String::from(entry.admin_user_id),
        admin_user: None,
        action: entry.action,
        target_id: entry.target_id,
        target_type: entry.target_type,
        target_user: None,
        target_guild: None,
        target_channel: None,
        related_users: Default::default(),
        related_guilds: Default::default(),
        related_channels: Default::default(),
        audit_log_reason: entry.audit_log_reason,
        metadata: entry.metadata,
        created_at: entry.created_at,
    }
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

fn nonempty_string(value: Option<&str>) -> Option<String> {
    value
        .filter(|value| !value.is_empty())
        .map(std::borrow::ToOwned::to_owned)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn accepts_route_sort_aliases() {
        assert_eq!(
            audit_sort_by("created_at").unwrap().to_string(),
            "createdAt"
        );
        assert_eq!(audit_sort_by("createdAt").unwrap().to_string(), "createdAt");
        assert_eq!(audit_sort_order("desc").unwrap().to_string(), "desc");
    }

    #[test]
    fn rejects_lossy_audit_totals() {
        let response = generated_types::AuditLogsListResponseSchema {
            logs: Vec::new(),
            total: 1.5,
        };
        assert!(audit_logs_response(response).is_err());
    }
}
