// SPDX-License-Identifier: AGPL-3.0-or-later

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashSet;

#[cfg_attr(
    feature = "scylla",
    derive(scylla::DeserializeValue, scylla::SerializeValue)
)]
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AttachmentUdt {
    pub attachment_id: Option<i64>,
    pub filename: Option<String>,
    pub size: Option<i64>,
    pub title: Option<String>,
    pub description: Option<String>,
    pub width: Option<i32>,
    pub height: Option<i32>,
    pub duration: Option<i32>,
    pub content_type: Option<String>,
    pub content_hash: Option<String>,
    pub placeholder: Option<String>,
    pub flags: Option<i32>,
    pub nsfw: Option<bool>,
    pub duration_secs: Option<i32>,
    pub waveform: Option<String>,
}

#[cfg_attr(
    feature = "scylla",
    derive(scylla::DeserializeValue, scylla::SerializeValue)
)]
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EmbedAuthorUdt {
    pub name: Option<String>,
    pub url: Option<String>,
    pub icon_url: Option<String>,
}

#[cfg_attr(
    feature = "scylla",
    derive(scylla::DeserializeValue, scylla::SerializeValue)
)]
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EmbedFieldUdt {
    pub name: Option<String>,
    pub value: Option<String>,
    #[cfg_attr(feature = "scylla", scylla(rename = "inline"))]
    pub is_inline: Option<bool>,
}

#[cfg_attr(
    feature = "scylla",
    derive(scylla::DeserializeValue, scylla::SerializeValue)
)]
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EmbedFooterUdt {
    pub text: Option<String>,
    pub icon_url: Option<String>,
}

#[cfg_attr(
    feature = "scylla",
    derive(scylla::DeserializeValue, scylla::SerializeValue)
)]
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EmbedMediaUdt {
    pub url: Option<String>,
    pub width: Option<i32>,
    pub height: Option<i32>,
    pub duration: Option<i32>,
    pub description: Option<String>,
    pub content_type: Option<String>,
    pub content_hash: Option<String>,
    pub placeholder: Option<String>,
    pub flags: Option<i32>,
}

#[cfg_attr(
    feature = "scylla",
    derive(scylla::DeserializeValue, scylla::SerializeValue)
)]
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EmbedProviderUdt {
    pub name: Option<String>,
    pub url: Option<String>,
}

#[cfg_attr(
    feature = "scylla",
    derive(scylla::DeserializeValue, scylla::SerializeValue)
)]
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EmbedChildUdt {
    #[cfg_attr(feature = "scylla", scylla(rename = "type"))]
    pub embed_type: Option<String>,
    pub title: Option<String>,
    pub description: Option<String>,
    pub url: Option<String>,
    pub timestamp: Option<DateTime<Utc>>,
    pub color: Option<i32>,
    pub author: Option<EmbedAuthorUdt>,
    pub provider: Option<EmbedProviderUdt>,
    pub thumbnail: Option<EmbedMediaUdt>,
    pub image: Option<EmbedMediaUdt>,
    pub video: Option<EmbedMediaUdt>,
    pub footer: Option<EmbedFooterUdt>,
    pub fields: Option<Vec<EmbedFieldUdt>>,
    pub nsfw: Option<bool>,
    pub audio: Option<EmbedMediaUdt>,
    pub html: Option<String>,
    pub html_width: Option<i32>,
    pub html_height: Option<i32>,
}

#[cfg_attr(
    feature = "scylla",
    derive(scylla::DeserializeValue, scylla::SerializeValue)
)]
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EmbedUdt {
    #[cfg_attr(feature = "scylla", scylla(rename = "type"))]
    pub embed_type: Option<String>,
    pub title: Option<String>,
    pub description: Option<String>,
    pub url: Option<String>,
    pub timestamp: Option<DateTime<Utc>>,
    pub color: Option<i32>,
    pub author: Option<EmbedAuthorUdt>,
    pub provider: Option<EmbedProviderUdt>,
    pub thumbnail: Option<EmbedMediaUdt>,
    pub image: Option<EmbedMediaUdt>,
    pub video: Option<EmbedMediaUdt>,
    pub footer: Option<EmbedFooterUdt>,
    pub fields: Option<Vec<EmbedFieldUdt>>,
    pub nsfw: Option<bool>,
    pub children: Option<Vec<EmbedChildUdt>>,
    pub audio: Option<EmbedMediaUdt>,
    pub html: Option<String>,
    pub html_width: Option<i32>,
    pub html_height: Option<i32>,
}

#[cfg_attr(
    feature = "scylla",
    derive(scylla::DeserializeValue, scylla::SerializeValue)
)]
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StickerItemUdt {
    pub sticker_id: Option<i64>,
    pub name: Option<String>,
    pub format_type: Option<i32>,
    pub animated: Option<bool>,
}

#[cfg_attr(
    feature = "scylla",
    derive(scylla::DeserializeValue, scylla::SerializeValue)
)]
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MessageReferenceUdt {
    pub channel_id: Option<i64>,
    pub message_id: Option<i64>,
    pub guild_id: Option<i64>,
    #[cfg_attr(feature = "scylla", scylla(rename = "type"))]
    pub reference_type: Option<i32>,
}

#[cfg_attr(
    feature = "scylla",
    derive(scylla::DeserializeValue, scylla::SerializeValue)
)]
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MessageCallUdt {
    pub participant_ids: Option<HashSet<i64>>,
    pub ended_timestamp: Option<DateTime<Utc>>,
}

#[cfg_attr(
    feature = "scylla",
    derive(scylla::DeserializeValue, scylla::SerializeValue)
)]
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MessageSnapshotUdt {
    pub content: Option<String>,
    pub timestamp: Option<DateTime<Utc>>,
    pub edited_timestmap: Option<DateTime<Utc>>,
    pub mention_users: Option<HashSet<i64>>,
    pub mention_roles: Option<HashSet<i64>>,
    pub mention_channels: Option<HashSet<i64>>,
    pub attachments: Option<Vec<AttachmentUdt>>,
    pub embeds: Option<Vec<EmbedUdt>>,
    pub sticker_items: Option<Vec<StickerItemUdt>>,
    #[cfg_attr(feature = "scylla", scylla(rename = "type"))]
    pub snapshot_type: Option<i32>,
    pub flags: Option<i32>,
    pub edited_timestamp: Option<DateTime<Utc>>,
}
