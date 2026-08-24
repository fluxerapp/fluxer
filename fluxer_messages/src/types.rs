// SPDX-License-Identifier: AGPL-3.0-or-later

use serde::{Deserialize, Serialize};

fn deserialize_double_option<'de, D, T>(deserializer: D) -> Result<Option<Option<T>>, D::Error>
where
    D: serde::Deserializer<'de>,
    T: Deserialize<'de>,
{
    Option::<T>::deserialize(deserializer).map(Some)
}

mod serde_id {
    use serde::Deserialize;
    use serde::de::{self, Deserializer};

    #[derive(Deserialize)]
    #[serde(untagged)]
    enum StringOrI64 {
        String(String),
        I64(i64),
        U64(u64),
    }

    impl StringOrI64 {
        fn into_i64<E: de::Error>(self) -> Result<i64, E> {
            match self {
                Self::String(value) => value.parse::<i64>().map_err(E::custom),
                Self::I64(value) => Ok(value),
                Self::U64(value) => i64::try_from(value).map_err(E::custom),
            }
        }
    }

    pub fn i64_from_string_or_number<'de, D>(deserializer: D) -> Result<i64, D::Error>
    where
        D: Deserializer<'de>,
    {
        StringOrI64::deserialize(deserializer)?.into_i64()
    }

    pub fn opt_i64_from_string_or_number<'de, D>(deserializer: D) -> Result<Option<i64>, D::Error>
    where
        D: Deserializer<'de>,
    {
        Option::<StringOrI64>::deserialize(deserializer)?
            .map(StringOrI64::into_i64)
            .transpose()
    }

    pub fn vec_i64_from_strings_or_numbers<'de, D>(deserializer: D) -> Result<Vec<i64>, D::Error>
    where
        D: Deserializer<'de>,
    {
        Option::<Vec<StringOrI64>>::deserialize(deserializer)?
            .unwrap_or_default()
            .into_iter()
            .map(StringOrI64::into_i64)
            .collect()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "op")]
#[allow(clippy::enum_variant_names, clippy::large_enum_variant)]
pub enum MessageRequest {
    GetById {
        channel_id: i64,
        message_id: i64,
    },
    GetLatest {
        channel_id: i64,
        limit: u32,
    },
    GetBefore {
        channel_id: i64,
        before_id: i64,
        limit: u32,
    },
    GetAfter {
        channel_id: i64,
        after_id: i64,
        limit: u32,
    },
    GetResponseById {
        channel_id: String,
        message_id: String,
        viewer_user_id: String,
        source_guild_id: Option<String>,
        message_history_cutoff_ms: Option<i64>,
        can_read_message_history: bool,
        media_endpoint: String,
        media_proxy_secret_key: String,
        include_reactions: Option<bool>,
        nonce: Option<String>,
        tts: Option<bool>,
    },
    BuildResponse {
        message: Message,
        viewer_user_id: String,
        source_guild_id: Option<String>,
        message_history_cutoff_ms: Option<i64>,
        can_read_message_history: bool,
        media_endpoint: String,
        media_proxy_secret_key: String,
        include_reactions: Option<bool>,
        nonce: Option<String>,
        tts: Option<bool>,
    },
    BuildResponses {
        messages: Vec<Message>,
        viewer_user_id: String,
        source_guild_id: Option<String>,
        message_history_cutoff_ms: Option<i64>,
        can_read_message_history: bool,
        media_endpoint: String,
        media_proxy_secret_key: String,
        include_reactions: Option<bool>,
    },
    ListResponses {
        channel_id: String,
        viewer_user_id: String,
        limit: u32,
        before_id: Option<String>,
        after_id: Option<String>,
        around_id: Option<String>,
        source_guild_id: Option<String>,
        message_history_cutoff_ms: Option<i64>,
        can_read_message_history: bool,
        media_endpoint: String,
        media_proxy_secret_key: String,
        include_reactions: Option<bool>,
    },
    ExtractMentions {
        contents: Vec<String>,
    },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[allow(clippy::large_enum_variant)]
pub enum MessageResponse {
    Found(Message),
    FoundMany(Vec<Message>),
    FoundApi(ApiMessageResponse),
    FoundApiMany(Vec<ApiMessageResponse>),
    FoundMentions(Vec<ExtractedMentionsResponse>),
    NotFound,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Message {
    #[serde(deserialize_with = "serde_id::i64_from_string_or_number")]
    pub message_id: i64,
    #[serde(deserialize_with = "serde_id::i64_from_string_or_number")]
    pub channel_id: i64,
    pub bucket: i32,
    #[serde(default, deserialize_with = "serde_id::opt_i64_from_string_or_number")]
    pub author_id: Option<i64>,
    #[serde(rename = "type")]
    pub message_type: i32,
    #[serde(default, deserialize_with = "serde_id::opt_i64_from_string_or_number")]
    pub webhook_id: Option<i64>,
    pub webhook_name: Option<String>,
    pub webhook_avatar_hash: Option<String>,
    pub content: Option<String>,
    pub edited_timestamp: Option<i64>,
    pub pinned: Option<bool>,
    pub flags: Option<i64>,
    pub mention_everyone: Option<bool>,
    #[serde(
        default,
        deserialize_with = "serde_id::vec_i64_from_strings_or_numbers"
    )]
    pub mention_users: Vec<i64>,
    #[serde(
        default,
        deserialize_with = "serde_id::vec_i64_from_strings_or_numbers"
    )]
    pub mention_roles: Vec<i64>,
    #[serde(
        default,
        deserialize_with = "serde_id::vec_i64_from_strings_or_numbers"
    )]
    pub mention_channels: Vec<i64>,
    pub has_reaction: Option<bool>,
    pub version: i32,
    #[serde(
        default,
        deserialize_with = "serde_id::vec_i64_from_strings_or_numbers"
    )]
    pub nsfw_emojis: Vec<i64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub attachments: Option<Vec<MessageAttachment>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub embeds: Option<Vec<MessageEmbed>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub sticker_items: Option<Vec<MessageStickerItem>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub message_reference: Option<MessageReference>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub call: Option<MessageCall>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub message_snapshots: Option<Vec<MessageSnapshot>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ApiUserPartialResponse {
    pub id: String,
    pub username: String,
    pub discriminator: String,
    pub global_name: Option<String>,
    pub avatar: Option<String>,
    pub avatar_color: Option<i32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub bot: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub system: Option<bool>,
    pub flags: i64,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub mention_flags: Option<i32>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ApiMessageAttachmentResponse {
    pub id: String,
    pub filename: String,
    pub title: Option<String>,
    pub description: Option<String>,
    pub content_type: Option<String>,
    pub content_hash: Option<String>,
    pub size: i64,
    pub url: Option<String>,
    pub proxy_url: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub width: Option<i32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub height: Option<i32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub placeholder: Option<String>,
    pub flags: i32,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub nsfw: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub duration: Option<i32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub waveform: Option<String>,
    pub expires_at: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub expired: Option<bool>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ApiEmbedAuthorResponse {
    pub name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub url: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub icon_url: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub proxy_icon_url: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ApiEmbedProviderResponse {
    pub name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub url: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub icon_url: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub proxy_icon_url: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ApiEmbedFooterResponse {
    pub text: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub icon_url: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub proxy_icon_url: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ApiEmbedMediaResponse {
    pub url: String,
    pub proxy_url: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub width: Option<i32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub height: Option<i32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub duration: Option<i32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub content_type: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub content_hash: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub placeholder: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub flags: Option<i32>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ApiEmbedFieldResponse {
    pub name: String,
    pub value: String,
    #[serde(rename = "inline")]
    pub is_inline: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ApiMessageEmbedChildResponse {
    #[serde(rename = "type")]
    pub embed_type: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub title: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub url: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub timestamp: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub color: Option<i32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub author: Option<ApiEmbedAuthorResponse>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub provider: Option<ApiEmbedProviderResponse>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub thumbnail: Option<ApiEmbedMediaResponse>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub image: Option<ApiEmbedMediaResponse>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub video: Option<ApiEmbedMediaResponse>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub audio: Option<ApiEmbedMediaResponse>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub footer: Option<ApiEmbedFooterResponse>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub fields: Option<Vec<ApiEmbedFieldResponse>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub nsfw: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub html: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub html_width: Option<i32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub html_height: Option<i32>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ApiMessageEmbedResponse {
    #[serde(flatten)]
    pub base: ApiMessageEmbedChildResponse,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub children: Option<Vec<ApiMessageEmbedChildResponse>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ApiMessageStickerResponse {
    pub id: String,
    pub name: String,
    pub animated: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub nsfw: Option<bool>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ApiMessageReferenceResponse {
    pub channel_id: String,
    pub message_id: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub guild_id: Option<String>,
    #[serde(rename = "type")]
    pub reference_type: i32,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ApiChannelMentionResponse {
    pub id: String,
    pub name: String,
    #[serde(rename = "type")]
    pub channel_type: i32,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ApiReactionEmojiResponse {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub id: Option<String>,
    pub name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub animated: Option<bool>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ApiMessageReactionResponse {
    pub emoji: ApiReactionEmojiResponse,
    pub count: i32,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub me: Option<bool>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ApiMessageSnapshotResponse {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub content: Option<String>,
    pub timestamp: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub edited_timestamp: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub mentions: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub mention_roles: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub mention_channels: Option<Vec<ApiChannelMentionResponse>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub embeds: Option<Vec<ApiMessageEmbedResponse>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub attachments: Option<Vec<ApiMessageAttachmentResponse>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub stickers: Option<Vec<ApiMessageStickerResponse>>,
    #[serde(rename = "type")]
    pub snapshot_type: i32,
    pub flags: i32,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ApiMessageCallResponse {
    pub participants: Vec<String>,
    pub ended_timestamp: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ApiMessageResponse {
    pub id: String,
    pub channel_id: String,
    pub author: ApiUserPartialResponse,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub webhook_id: Option<String>,
    #[serde(rename = "type")]
    pub message_type: i32,
    pub flags: i64,
    pub content: String,
    pub timestamp: String,
    pub edited_timestamp: Option<String>,
    pub pinned: bool,
    pub mention_everyone: bool,
    pub tts: bool,
    pub mentions: Vec<ApiUserPartialResponse>,
    pub mention_roles: Vec<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub mention_channels: Option<Vec<ApiChannelMentionResponse>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub users: Option<Vec<ApiUserPartialResponse>>,
    pub embeds: Vec<ApiMessageEmbedResponse>,
    pub attachments: Vec<ApiMessageAttachmentResponse>,
    pub stickers: Vec<ApiMessageStickerResponse>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub nsfw_emojis: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub reactions: Option<Vec<ApiMessageReactionResponse>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub message_reference: Option<ApiMessageReferenceResponse>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub message_snapshots: Option<Vec<ApiMessageSnapshotResponse>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub nonce: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub call: Option<ApiMessageCallResponse>,
    #[serde(
        default,
        skip_serializing_if = "Option::is_none",
        deserialize_with = "deserialize_double_option"
    )]
    pub referenced_message: Option<Option<Box<ApiMessageResponse>>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExtractedMentionsResponse {
    pub users: Vec<String>,
    pub roles: Vec<String>,
    pub channels: Vec<String>,
    pub everyone: bool,
    pub here: bool,
}

impl From<crate::mention_extractor::MessageMentions> for ExtractedMentionsResponse {
    fn from(mentions: crate::mention_extractor::MessageMentions) -> Self {
        Self {
            users: mentions
                .users
                .into_iter()
                .map(|id| id.to_string())
                .collect(),
            roles: mentions
                .roles
                .into_iter()
                .map(|id| id.to_string())
                .collect(),
            channels: mentions
                .channels
                .into_iter()
                .map(|id| id.to_string())
                .collect(),
            everyone: mentions.everyone,
            here: mentions.here,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MessageAttachment {
    #[serde(default, deserialize_with = "serde_id::opt_i64_from_string_or_number")]
    pub attachment_id: Option<i64>,
    pub filename: Option<String>,
    #[serde(default, deserialize_with = "serde_id::opt_i64_from_string_or_number")]
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

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MessageEmbedAuthor {
    pub name: Option<String>,
    pub url: Option<String>,
    pub icon_url: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MessageEmbedField {
    pub name: Option<String>,
    pub value: Option<String>,
    #[serde(rename = "inline")]
    pub is_inline: Option<bool>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MessageEmbedFooter {
    pub text: Option<String>,
    pub icon_url: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MessageEmbedMedia {
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

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MessageEmbedProvider {
    pub name: Option<String>,
    pub url: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MessageEmbedChild {
    #[serde(rename = "type")]
    pub embed_type: Option<String>,
    pub title: Option<String>,
    pub description: Option<String>,
    pub url: Option<String>,
    pub timestamp: Option<i64>,
    pub color: Option<i32>,
    pub author: Option<MessageEmbedAuthor>,
    pub provider: Option<MessageEmbedProvider>,
    pub thumbnail: Option<MessageEmbedMedia>,
    pub image: Option<MessageEmbedMedia>,
    pub video: Option<MessageEmbedMedia>,
    pub footer: Option<MessageEmbedFooter>,
    pub fields: Option<Vec<MessageEmbedField>>,
    pub nsfw: Option<bool>,
    pub audio: Option<MessageEmbedMedia>,
    pub html: Option<String>,
    pub html_width: Option<i32>,
    pub html_height: Option<i32>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MessageEmbed {
    #[serde(rename = "type")]
    pub embed_type: Option<String>,
    pub title: Option<String>,
    pub description: Option<String>,
    pub url: Option<String>,
    pub timestamp: Option<i64>,
    pub color: Option<i32>,
    pub author: Option<MessageEmbedAuthor>,
    pub provider: Option<MessageEmbedProvider>,
    pub thumbnail: Option<MessageEmbedMedia>,
    pub image: Option<MessageEmbedMedia>,
    pub video: Option<MessageEmbedMedia>,
    pub footer: Option<MessageEmbedFooter>,
    pub fields: Option<Vec<MessageEmbedField>>,
    pub nsfw: Option<bool>,
    pub children: Option<Vec<MessageEmbedChild>>,
    pub audio: Option<MessageEmbedMedia>,
    pub html: Option<String>,
    pub html_width: Option<i32>,
    pub html_height: Option<i32>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MessageStickerItem {
    #[serde(default, deserialize_with = "serde_id::opt_i64_from_string_or_number")]
    pub sticker_id: Option<i64>,
    pub name: Option<String>,
    pub format_type: Option<i32>,
    pub animated: Option<bool>,
    pub nsfw: Option<bool>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MessageReference {
    #[serde(default, deserialize_with = "serde_id::opt_i64_from_string_or_number")]
    pub channel_id: Option<i64>,
    #[serde(default, deserialize_with = "serde_id::opt_i64_from_string_or_number")]
    pub message_id: Option<i64>,
    #[serde(default, deserialize_with = "serde_id::opt_i64_from_string_or_number")]
    pub guild_id: Option<i64>,
    #[serde(rename = "type")]
    pub reference_type: Option<i32>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MessageCall {
    #[serde(
        default,
        deserialize_with = "serde_id::vec_i64_from_strings_or_numbers"
    )]
    pub participant_ids: Vec<i64>,
    pub ended_timestamp: Option<i64>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MessageSnapshot {
    pub content: Option<String>,
    pub timestamp: Option<i64>,
    pub edited_timestamp: Option<i64>,
    #[serde(
        default,
        deserialize_with = "serde_id::vec_i64_from_strings_or_numbers"
    )]
    pub mention_users: Vec<i64>,
    #[serde(
        default,
        deserialize_with = "serde_id::vec_i64_from_strings_or_numbers"
    )]
    pub mention_roles: Vec<i64>,
    #[serde(
        default,
        deserialize_with = "serde_id::vec_i64_from_strings_or_numbers"
    )]
    pub mention_channels: Vec<i64>,
    pub attachments: Option<Vec<MessageAttachment>>,
    pub embeds: Option<Vec<MessageEmbed>>,
    pub sticker_items: Option<Vec<MessageStickerItem>>,
    #[serde(rename = "type")]
    pub snapshot_type: Option<i32>,
    pub flags: Option<i32>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn message_deserializes_service_payload_ids_from_strings() {
        let message: Message = serde_json::from_value(json!({
            "message_id": "1509197195776110592",
            "channel_id": "1497639278555484216",
            "bucket": 416,
            "author_id": "1472426752046002208",
            "type": 0,
            "webhook_id": null,
            "content": "hello <@1472426752046002208>",
            "edited_timestamp": null,
            "pinned": false,
            "flags": 0,
            "mention_everyone": false,
            "mention_users": ["1472426752046002208"],
            "mention_roles": [],
            "mention_channels": [],
            "has_reaction": false,
            "version": 1,
            "nsfw_emojis": [],
            "attachments": [{
                "attachment_id": "1509197195776110593",
                "filename": "a.png",
                "size": "12345",
                "content_type": "image/png",
                "flags": 0
            }],
            "sticker_items": [{
                "sticker_id": "1509197195776110594",
                "name": "sticker"
            }],
            "message_reference": {
                "channel_id": "1497639278555484216",
                "message_id": "1509197195776110590",
                "guild_id": "1497639278555484210",
                "type": 0
            },
            "call": {
                "participant_ids": ["1472426752046002208"],
                "ended_timestamp": null
            },
            "message_snapshots": [{
                "content": "snapshot",
                "timestamp": 1779891051001i64,
                "mention_users": ["1472426752046002208"],
                "mention_roles": [],
                "mention_channels": [],
                "type": 0,
                "flags": 0
            }]
        }))
        .expect("message payload should deserialize");

        assert_eq!(message.message_id, 1_509_197_195_776_110_592);
        assert_eq!(message.author_id, Some(1_472_426_752_046_002_208));
        assert_eq!(message.mention_users, vec![1_472_426_752_046_002_208]);
        assert_eq!(
            message.attachments.unwrap()[0].attachment_id,
            Some(1_509_197_195_776_110_593)
        );
        assert_eq!(
            message.message_reference.unwrap().message_id,
            Some(1_509_197_195_776_110_590)
        );
        assert_eq!(
            message.call.unwrap().participant_ids,
            vec![1_472_426_752_046_002_208]
        );
    }

    fn minimal_api_message(
        referenced: Option<Option<Box<ApiMessageResponse>>>,
    ) -> ApiMessageResponse {
        let author = ApiUserPartialResponse {
            id: "1".to_string(),
            username: "user".to_string(),
            discriminator: "0001".to_string(),
            global_name: None,
            avatar: None,
            avatar_color: None,
            bot: None,
            system: None,
            flags: 0,
            mention_flags: None,
        };
        ApiMessageResponse {
            id: "2".to_string(),
            channel_id: "3".to_string(),
            author,
            webhook_id: None,
            message_type: 0,
            flags: 0,
            content: String::new(),
            timestamp: "2026-01-01T00:00:00Z".to_string(),
            edited_timestamp: None,
            pinned: false,
            mention_everyone: false,
            tts: false,
            mentions: Vec::new(),
            mention_roles: Vec::new(),
            mention_channels: None,
            users: None,
            embeds: Vec::new(),
            attachments: Vec::new(),
            stickers: Vec::new(),
            nsfw_emojis: None,
            reactions: None,
            message_reference: None,
            message_snapshots: None,
            nonce: None,
            call: None,
            referenced_message: referenced,
        }
    }

    fn msgpack_round_trip(
        referenced: Option<Option<Box<ApiMessageResponse>>>,
    ) -> Option<Option<Box<ApiMessageResponse>>> {
        let encoded =
            rmp_serde::to_vec_named(&minimal_api_message(referenced)).expect("encodes to msgpack");
        rmp_serde::from_slice::<ApiMessageResponse>(&encoded)
            .expect("decodes from msgpack")
            .referenced_message
    }

    #[test]
    fn referenced_message_null_survives_the_msgpack_round_trip() {
        assert!(msgpack_round_trip(None).is_none());
        assert!(matches!(msgpack_round_trip(Some(None)), Some(None)));
        assert!(matches!(
            msgpack_round_trip(Some(Some(Box::new(minimal_api_message(None))))),
            Some(Some(_))
        ));
    }

    #[test]
    fn referenced_message_null_serialises_as_json_null_not_an_absent_key() {
        let deleted = serde_json::to_value(minimal_api_message(Some(None))).expect("serialises");
        assert_eq!(
            deleted.get("referenced_message"),
            Some(&serde_json::Value::Null)
        );

        let absent = serde_json::to_value(minimal_api_message(None)).expect("serialises");
        assert!(absent.get("referenced_message").is_none());
    }
}
