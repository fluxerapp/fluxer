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
    pub vip_only: Option<bool>,
    #[serde(default)]
    pub required_guild_features: Vec<String>,
    #[serde(default)]
    pub allowed_guild_ids: Vec<String>,
    #[serde(default)]
    pub allowed_user_ids: Vec<String>,
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

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct GetVoiceServerResponse {
    pub server: Option<VoiceServer>,
}
