// SPDX-License-Identifier: AGPL-3.0-or-later

use super::common::deserialize_discriminator;
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct UserSession {
    pub session_id_hash: String,
    pub created_at: String,
    pub approx_last_used_at: String,
    pub client_ip: String,
    pub client_ip_reverse: Option<String>,
    pub client_os: Option<String>,
    pub client_platform: Option<String>,
    pub client_location: Option<String>,
    pub deleted_at: Option<String>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ListUserSessionsResponse {
    pub sessions: Vec<UserSession>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct AdminResolvedUser {
    pub id: String,
    pub username: String,
    #[serde(deserialize_with = "deserialize_discriminator")]
    pub discriminator: String,
    pub global_name: Option<String>,
    pub avatar: Option<String>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct RelationshipEntry {
    pub target_user_id: String,
    pub category: String,
    pub nickname: Option<String>,
    pub since: Option<String>,
    pub target: Option<AdminResolvedUser>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ListUserRelationshipsResponse {
    pub friends: Vec<RelationshipEntry>,
    pub incoming_requests: Vec<RelationshipEntry>,
    pub outgoing_requests: Vec<RelationshipEntry>,
    pub blocked: Vec<RelationshipEntry>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct DmChannel {
    pub channel_id: String,
    pub channel_type: Option<i32>,
    pub channel_nsfw: Option<bool>,
    pub guild_nsfw_level: Option<i32>,
    #[serde(default)]
    pub recipient_ids: Vec<String>,
    #[serde(default)]
    pub recipients: Vec<AdminResolvedUser>,
    pub last_message_id: Option<String>,
    #[serde(default)]
    pub is_open: bool,
    pub name: Option<String>,
    pub icon: Option<String>,
    pub owner_id: Option<String>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ListUserDmChannelsResponse {
    pub channels: Vec<DmChannel>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ListUserGroupDmChannelsResponse {
    pub channels: Vec<DmChannel>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ReportEntry {
    pub report_id: String,
    pub reporter_id: Option<String>,
    pub reporter_tag: Option<String>,
    pub reporter_username: Option<String>,
    pub reporter_global_name: Option<String>,
    pub reporter_discriminator: Option<String>,
    pub reporter_email: Option<String>,
    pub reporter_full_legal_name: Option<String>,
    pub reporter_country_of_residence: Option<String>,
    pub reported_at: String,
    pub status: i32,
    pub report_type: i32,
    pub category: Option<String>,
    pub additional_info: Option<String>,
    pub reported_user_id: Option<String>,
    pub reported_user_tag: Option<String>,
    pub reported_user_username: Option<String>,
    pub reported_user_global_name: Option<String>,
    pub reported_user_discriminator: Option<String>,
    pub reported_user_avatar_hash: Option<String>,
    pub reported_guild_id: Option<String>,
    pub reported_guild_name: Option<String>,
    pub reported_guild_icon_hash: Option<String>,
    pub reported_message_id: Option<String>,
    pub reported_channel_id: Option<String>,
    pub reported_channel_name: Option<String>,
    pub reported_channel_nsfw: Option<bool>,
    pub reported_guild_invite_code: Option<String>,
    pub reported_guild_nsfw_level: Option<i32>,
    pub reported_guild_nsfw: Option<bool>,
    pub reported_guild_content_warning_level: Option<i32>,
    pub reported_guild_content_warning_text: Option<String>,
    pub reported_channel_nsfw_override: Option<bool>,
    pub reported_channel_content_warning_level: Option<i32>,
    pub reported_channel_content_warning_text: Option<String>,
    pub reported_channel_effective_nsfw: Option<bool>,
    pub reported_channel_effective_content_warning_level: Option<i32>,
    pub reported_channel_effective_content_warning_text: Option<String>,
    pub resolved_at: Option<String>,
    pub resolved_by_admin_id: Option<String>,
    pub public_comment: Option<String>,
    pub mutual_dm_channel_id: Option<String>,
    pub message_context: Option<Vec<serde_json::Value>>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct SearchReportsResponse {
    pub reports: Vec<ReportEntry>,
    pub total: u64,
    pub offset: u64,
    pub limit: u64,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ListReportsResponse {
    pub reports: Vec<ReportEntry>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ResolveReportResponse {
    pub report_id: String,
    pub status: i32,
    pub resolved_at: Option<String>,
    pub public_comment: Option<String>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct GuildAuditLogEntry {
    pub id: String,
    pub action_type: i32,
    pub user_id: Option<String>,
    pub target_id: Option<String>,
    pub reason: Option<String>,
    pub options: Option<serde_json::Value>,
    pub changes: Option<Vec<GuildAuditLogChange>>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct GuildAuditLogChange {
    pub key: String,
    pub old_value: Option<serde_json::Value>,
    pub new_value: Option<serde_json::Value>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct GuildAuditLogUser {
    pub id: String,
    pub username: String,
    pub global_name: Option<String>,
    pub discriminator: Option<String>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct GuildAuditLogResponse {
    pub audit_log_entries: Vec<GuildAuditLogEntry>,
    #[serde(default)]
    pub users: Vec<GuildAuditLogUser>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct GuildMemberUser {
    pub id: String,
    pub username: String,
    #[serde(deserialize_with = "deserialize_discriminator")]
    pub discriminator: String,
    pub global_name: Option<String>,
    pub avatar: Option<String>,
    #[serde(default)]
    pub bot: bool,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct GuildMember {
    pub user: GuildMemberUser,
    pub nick: Option<String>,
    pub joined_at: String,
    #[serde(default)]
    pub roles: Vec<String>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ListGuildMembersResponse {
    pub members: Vec<GuildMember>,
    pub total: u64,
    pub limit: u64,
    pub offset: u64,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct UserContactChangeLogEntry {
    pub event_id: String,
    pub field: String,
    pub old_value: Option<String>,
    pub new_value: Option<String>,
    pub reason: Option<String>,
    pub actor_user_id: Option<String>,
    pub event_at: String,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ListUserChangeLogResponse {
    pub entries: Vec<UserContactChangeLogEntry>,
    pub next_page_token: Option<String>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct WebAuthnCredential {
    pub id: String,
    pub name: String,
    pub created_at: String,
    pub last_used_at: Option<String>,
}

pub type WebAuthnCredentialListResponse = Vec<WebAuthnCredential>;

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct PasswordResetLinkResponse {
    pub url: String,
    pub expires_at: String,
}
