// SPDX-License-Identifier: AGPL-3.0-or-later

use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct VoiceRegion {
    pub id: String,
    pub name: Option<String>,
    pub emoji: Option<String>,
    pub latitude: Option<f64>,
    pub longitude: Option<f64>,
    pub is_default: Option<bool>,
    pub vip_only: Option<bool>,
    #[serde(default)]
    pub required_guild_features: Vec<String>,
    #[serde(default)]
    pub allowed_guild_ids: Vec<String>,
    #[serde(default)]
    pub allowed_user_ids: Vec<String>,
    pub servers: Option<Vec<VoiceServer>>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct VoiceServer {
    pub server_id: String,
    pub region_id: String,
    pub endpoint: Option<String>,
    pub latitude: Option<f64>,
    pub longitude: Option<f64>,
    pub is_active: Option<bool>,
    pub soft_connection_limit: Option<i64>,
    pub notes: Option<String>,
    pub vip_only: Option<bool>,
    #[serde(default)]
    pub required_guild_features: Vec<String>,
    #[serde(default)]
    pub allowed_guild_ids: Vec<String>,
    #[serde(default)]
    pub allowed_user_ids: Vec<String>,
    pub internal_endpoint: Option<String>,
    #[serde(default)]
    pub draining: bool,
    #[serde(default)]
    pub public_ipv4: Option<String>,
    #[serde(default)]
    pub public_ipv6: Option<String>,
    pub health: Option<VoiceServerHealth>,
    #[serde(default)]
    pub duplicate_api_key: bool,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct VoiceServerHealth {
    pub status: String,
    pub checked_at: Option<String>,
    pub error: Option<String>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct VoiceJoinToken {
    pub token_id: String,
    pub status: String,
    pub created_at: String,
    pub expires_at: String,
    pub used_at: Option<String>,
    pub used_server_id: Option<String>,
    pub revoked_at: Option<String>,
    pub region_id: Option<String>,
    pub region_name: Option<String>,
    pub server_id: Option<String>,
    pub hostname: Option<String>,
    pub notes: Option<String>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ListVoiceJoinTokensResponse {
    pub tokens: Vec<VoiceJoinToken>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct CreateVoiceJoinTokenResponse {
    pub token_id: String,
    pub token: String,
    pub expires_at: String,
    pub command: String,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ListVoiceRegionsResponse {
    pub regions: Vec<VoiceRegion>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct GetVoiceRegionResponse {
    pub region: Option<VoiceRegion>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct CreateVoiceRegionResponse {
    pub region: VoiceRegion,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct UpdateVoiceRegionResponse {
    pub region: VoiceRegion,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct DeleteVoiceResponse {
    #[serde(flatten)]
    pub data: serde_json::Value,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ListVoiceServersResponse {
    pub servers: Vec<VoiceServer>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct CreateVoiceServerResponse {
    pub server: VoiceServer,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct UpdateVoiceServerResponse {
    pub server: VoiceServer,
}
