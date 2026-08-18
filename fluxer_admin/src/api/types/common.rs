// SPDX-License-Identifier: AGPL-3.0-or-later

use serde::{Deserialize, Deserializer, Serialize};

pub fn deserialize_discriminator<'de, D: Deserializer<'de>>(d: D) -> Result<String, D::Error> {
    let v: serde_json::Value = Deserialize::deserialize(d)?;
    match v {
        serde_json::Value::String(s) => Ok(format!("{:0>4}", s)),
        serde_json::Value::Number(n) => Ok(format!("{:04}", n.as_u64().unwrap_or(0))),
        _ => Ok("0000".to_owned()),
    }
}

pub fn deserialize_string_or_u64<'de, D: Deserializer<'de>>(d: D) -> Result<u64, D::Error> {
    let v: serde_json::Value = Deserialize::deserialize(d)?;
    match v {
        serde_json::Value::String(s) => s.parse::<u64>().map_err(serde::de::Error::custom),
        serde_json::Value::Number(n) => Ok(n.as_u64().unwrap_or(0)),
        serde_json::Value::Null => Ok(0),
        _ => Err(serde::de::Error::custom("expected string or number")),
    }
}

#[derive(Clone, Debug, Deserialize)]
pub struct AdminUserMeResponse {
    pub user: AdminUser,
}

#[derive(Clone, Debug, Deserialize)]
pub struct UserMutationResponse {
    pub user: AdminUser,
}

#[derive(Clone, Debug, Deserialize)]
pub struct LookupUserResponse {
    pub users: Vec<AdminUser>,
}

#[derive(Clone, Debug, Deserialize)]
pub struct SearchUsersResponse {
    pub users: Vec<AdminUser>,
    pub total: u64,
}

#[derive(Clone, Debug, Deserialize)]
pub struct SearchGuildsResponse {
    pub guilds: Vec<GuildInfo>,
    pub total: u64,
}

#[derive(Clone, Debug, Deserialize)]
pub struct LookupGuildResponse {
    pub guild: Option<GuildDetailInfo>,
}

#[derive(Clone, Debug, Deserialize)]
pub struct GuildUpdateResponse {
    pub guild: GuildInfo,
}

#[derive(Clone, Debug, Deserialize)]
pub struct ListUserGuildsResponse {
    pub guilds: Vec<GuildInfo>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct AdminUser {
    pub id: String,
    pub username: String,
    #[serde(deserialize_with = "deserialize_discriminator")]
    pub discriminator: String,
    pub avatar: Option<String>,
    pub banner: Option<String>,
    pub email: Option<String>,
    #[serde(default)]
    pub email_verified: bool,
    #[serde(default)]
    pub email_bounced: bool,
    pub global_name: Option<String>,
    pub bio: Option<String>,
    pub pronouns: Option<String>,
    pub accent_color: Option<i32>,
    pub date_of_birth: Option<String>,
    pub locale: Option<String>,
    #[serde(default)]
    pub acls: Vec<String>,
    #[serde(default)]
    pub traits: Vec<String>,
    #[serde(default, deserialize_with = "deserialize_string_or_u64")]
    pub flags: u64,
    #[serde(default)]
    pub premium_flags: i32,
    #[serde(default)]
    pub bot: bool,
    #[serde(default)]
    pub system: bool,
    pub premium_type: Option<i32>,
    pub premium_since: Option<String>,
    pub premium_until: Option<String>,
    pub premium_grace_ends_at: Option<String>,
    pub premium_lifetime_sequence: Option<i32>,
    #[serde(default)]
    pub suspicious_activity_flags: i32,
    #[serde(default)]
    pub phone_verification_deferred: bool,
    #[serde(default)]
    pub has_totp: bool,
    #[serde(default)]
    pub authenticator_types: Vec<i32>,
    #[serde(default)]
    pub has_verified_phone: bool,
    pub temp_banned_until: Option<String>,
    pub pending_deletion_at: Option<String>,
    pub pending_bulk_message_deletion_at: Option<String>,
    pub deletion_reason_code: Option<i32>,
    pub deletion_public_reason: Option<String>,
    pub last_active_at: Option<String>,
    pub last_active_ip: Option<String>,
    pub last_active_ip_reverse: Option<String>,
    pub last_active_location: Option<String>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct GuildInfo {
    pub id: String,
    pub name: String,
    pub icon: Option<String>,
    pub banner: Option<String>,
    pub owner_id: String,
    pub owner_username: Option<String>,
    pub owner_global_name: Option<String>,
    pub owner_discriminator: Option<String>,
    #[serde(default)]
    pub member_count: u64,
    #[serde(default)]
    pub features: Vec<String>,
    pub nsfw_level: Option<i32>,
    pub nsfw: Option<bool>,
    pub content_warning_level: Option<i32>,
    pub content_warning_text: Option<String>,
    pub description: Option<String>,
    pub vanity_url_code: Option<String>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct GuildDetailInfo {
    pub id: String,
    pub owner_id: String,
    pub owner_username: Option<String>,
    pub owner_global_name: Option<String>,
    pub owner_discriminator: Option<String>,
    pub name: String,
    pub vanity_url_code: Option<String>,
    pub icon: Option<String>,
    pub banner: Option<String>,
    pub splash: Option<String>,
    pub embed_splash: Option<String>,
    #[serde(default)]
    pub features: Vec<String>,
    pub verification_level: Option<i32>,
    pub mfa_level: Option<i32>,
    pub nsfw_level: Option<i32>,
    pub nsfw: Option<bool>,
    pub content_warning_level: Option<i32>,
    pub content_warning_text: Option<String>,
    pub explicit_content_filter: Option<i32>,
    pub default_message_notifications: Option<i32>,
    pub afk_channel_id: Option<String>,
    pub afk_timeout: Option<i32>,
    pub system_channel_id: Option<String>,
    pub system_channel_flags: Option<i32>,
    pub rules_channel_id: Option<String>,
    pub disabled_operations: Option<i32>,
    #[serde(default)]
    pub member_count: u64,
    #[serde(default)]
    pub channels: Vec<GuildChannelSummary>,
    #[serde(default)]
    pub roles: Vec<GuildRoleSummary>,
    pub description: Option<String>,
}

impl From<GuildDetailInfo> for GuildInfo {
    fn from(d: GuildDetailInfo) -> Self {
        Self {
            id: d.id,
            name: d.name,
            icon: d.icon,
            banner: d.banner,
            owner_id: d.owner_id,
            owner_username: d.owner_username,
            owner_global_name: d.owner_global_name,
            owner_discriminator: d.owner_discriminator,
            member_count: d.member_count,
            features: d.features,
            nsfw_level: d.nsfw_level,
            nsfw: d.nsfw,
            content_warning_level: d.content_warning_level,
            content_warning_text: d.content_warning_text,
            description: d.description,
            vanity_url_code: d.vanity_url_code,
        }
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct GuildChannelSummary {
    pub id: String,
    pub name: Option<String>,
    #[serde(rename = "type")]
    pub channel_type: i32,
    pub position: i32,
    pub parent_id: Option<String>,
    pub nsfw: Option<bool>,
    pub nsfw_override: Option<bool>,
    pub content_warning_level: Option<i32>,
    pub content_warning_text: Option<String>,
    pub url: Option<String>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct GuildRoleSummary {
    pub id: String,
    pub name: String,
    pub color: i32,
    pub position: i32,
    pub permissions: String,
    pub hoist: bool,
    pub mentionable: bool,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct FlashMessage {
    pub level: FlashLevel,
    pub message: String,
}

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq)]
#[serde(rename_all = "lowercase")]
pub enum FlashLevel {
    Success,
    Error,
    Info,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct BanCheckResult {
    pub banned: bool,
    #[serde(default)]
    pub entries: Vec<serde_json::Value>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct BulkBanResult {
    pub job_id: String,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct BanAvatarResult {
    pub hash_short: String,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct SuccessResponse {
    pub success: bool,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct TerminateSessionsResponse {
    pub terminated_count: i32,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct RemoveRelationshipsResponse {
    pub removed_count: i32,
}
