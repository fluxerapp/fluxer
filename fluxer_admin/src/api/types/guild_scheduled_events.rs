// SPDX-License-Identifier: AGPL-3.0-or-later

use serde::Deserialize;

#[derive(Debug, Clone, Deserialize)]
pub struct GuildScheduledEventItem {
    pub id: String,
    pub guild_id: String,
    pub creator_id: String,
    pub name: String,
    pub description: Option<String>,
    pub image_url: Option<String>,
    pub scheduled_start_time: String,
    pub scheduled_end_time: Option<String>,
    pub status: String,
    pub entity_type: String,
    pub user_count: i64,
}
