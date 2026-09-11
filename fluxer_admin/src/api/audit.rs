// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::api::generated::types as generated_types;

use super::client::{AdminApiClient, ApiError, ApiResult};
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
            ("sort_by", sort_by.as_deref().unwrap_or_default()),
            ("sort_order", sort_order.as_deref().unwrap_or_default()),
            ("limit", limit.as_str()),
            ("offset", offset.as_str()),
        ];
        self.get("/admin/audit-logs", Some(&query_params)).await
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
        for total in [serde_json::json!(1.5), serde_json::json!(-1)] {
            let response = serde_json::json!({"logs": [], "total": total});
            assert!(serde_json::from_value::<AuditLogsListResponse>(response).is_err());
        }
    }

    #[test]
    fn deserializes_audit_fields_without_losing_generated_string_values() {
        let json = serde_json::json!({
            "logs": [{
                "log_id": "123456789012345678",
                "admin_user_id": "234567890123456789",
                "admin_user": null,
                "action": "USER_UPDATE",
                "target_id": "345678901234567890",
                "target_type": "user",
                "target_user": null,
                "target_guild": null,
                "target_channel": null,
                "related_users": {},
                "related_guilds": {},
                "related_channels": {},
                "audit_log_reason": "Account review",
                "metadata": {"field": "username"},
                "created_at": "2026-09-11T12:00:00.000Z"
            }],
            "total": 1
        });
        let generated: generated_types::AuditLogsListResponseSchema =
            serde_json::from_value(json.clone()).unwrap();
        assert_eq!(generated.logs[0].action.to_string(), "USER_UPDATE");

        let response: AuditLogsListResponse = serde_json::from_value(json.clone()).unwrap();
        assert_eq!(serde_json::to_value(response).unwrap(), json);
    }
}
