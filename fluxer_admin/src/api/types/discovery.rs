// SPDX-License-Identifier: AGPL-3.0-or-later

use serde::{Deserialize, Serialize};

pub struct DiscoveryGuild {
    pub guild_id: String,
    pub guild_name: String,
    pub guild_icon: Option<String>,
    pub guild_owner_id: String,
    pub guild_owner_username: Option<String>,
    pub guild_owner_global_name: Option<String>,
    pub guild_owner_discriminator: Option<String>,
    pub guild_member_count: u64,
    pub guild_nsfw_level: Option<i32>,
    pub description: String,
    pub category_type: i32,
    pub primary_language: Option<String>,
    pub custom_tags: Vec<String>,
    pub applied_at: String,
    pub approved_at: Option<String>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct DiscoveryApplicationResponse {
    #[serde(flatten)]
    pub data: serde_json::Value,
}
