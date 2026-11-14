// SPDX-License-Identifier: AGPL-3.0-or-later

use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct AuditLogEntry {
    pub log_id: String,
    pub admin_user_id: String,
    #[serde(default)]
    pub admin_user: Option<AuditLogUserSummary>,
    pub action: String,
    #[serde(default)]
    pub access: Option<String>,
    pub target_id: String,
    pub target_type: String,
    #[serde(default)]
    pub target_user: Option<AuditLogUserSummary>,
    #[serde(default)]
    pub target_guild: Option<AuditLogGuildSummary>,
    #[serde(default)]
    pub target_channel: Option<AuditLogChannelSummary>,
    #[serde(default)]
    pub related_users: std::collections::HashMap<String, AuditLogUserSummary>,
    #[serde(default)]
    pub related_guilds: std::collections::HashMap<String, AuditLogGuildSummary>,
    #[serde(default)]
    pub related_channels: std::collections::HashMap<String, AuditLogChannelSummary>,
    pub audit_log_reason: Option<String>,
    #[serde(default)]
    pub metadata: std::collections::HashMap<String, String>,
    pub created_at: String,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct AuditLogUserSummary {
    pub id: String,
    pub username: String,
    pub discriminator: String,
    pub global_name: Option<String>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct AuditLogGuildSummary {
    pub id: String,
    pub name: String,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct AuditLogChannelSummary {
    pub id: String,
    pub name: Option<String>,
    #[serde(rename = "type")]
    pub channel_type: i32,
    pub guild_id: Option<String>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct AuditLogsListResponse {
    pub logs: Vec<AuditLogEntry>,
    pub total: u64,
}
