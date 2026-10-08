// SPDX-License-Identifier: AGPL-3.0-or-later

use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct GuildThreadMetadata {
    pub archived: bool,
    pub locked: bool,
    pub auto_archive_duration: i32,
    pub archive_timestamp: String,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct GuildThreadItem {
    pub id: String,
    #[serde(rename = "type")]
    pub channel_type: i32,
    #[serde(default)]
    pub name: Option<String>,
    #[serde(default)]
    pub parent_id: Option<String>,
    #[serde(default)]
    pub owner_id: Option<String>,
    #[serde(default)]
    pub member_count: Option<i32>,
    #[serde(default)]
    pub message_count: Option<i32>,
    #[serde(default)]
    pub thread_metadata: Option<GuildThreadMetadata>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ListGuildThreadsResponse {
    pub threads: Vec<GuildThreadItem>,
}
