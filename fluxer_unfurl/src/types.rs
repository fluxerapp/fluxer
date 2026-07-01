// SPDX-License-Identifier: AGPL-3.0-or-later

use serde::{Deserialize, Serialize};
use std::sync::Arc;

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "op")]
pub enum UnfurlRequest {
    Unfurl {
        url: String,
        nsfw_mode: Option<NsfwMode>,
        #[serde(default)]
        bypass_cache: bool,
        #[serde(default)]
        cache_only: bool,
        #[serde(default)]
        youtube_api_key: Option<String>,
        #[serde(default)]
        klipy_api_key: Option<String>,
    },
    Invalidate {
        url: String,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Default)]
#[serde(rename_all = "lowercase")]
pub enum NsfwMode {
    #[default]
    Block,
    Flag,
    Allow,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum UnfurlResponse {
    Resolved(Arc<UnfurlResult>),
    Invalidated(InvalidatedResponse),
    Failed { message: String },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InvalidatedResponse {
    pub invalidated: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UnfurlResult {
    pub embeds: Vec<MessageEmbed>,
    pub cache_ttl_seconds: Option<u64>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MessageEmbed {
    #[serde(rename = "type")]
    pub embed_type: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub url: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub title: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub color: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub timestamp: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub thumbnail: Option<EmbedMedia>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub image: Option<EmbedMedia>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub video: Option<EmbedMedia>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub audio: Option<EmbedMedia>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub author: Option<EmbedAuthor>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub provider: Option<EmbedProvider>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub footer: Option<EmbedFooter>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub fields: Option<Vec<EmbedField>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub html: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub html_width: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub html_height: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub children: Option<Vec<MessageEmbed>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub nsfw: Option<bool>,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct EmbedMedia {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub url: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub proxy_url: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub content_type: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub content_hash: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub width: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub height: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub placeholder: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub duration: Option<u32>,
    #[serde(default)]
    pub flags: u32,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct EmbedAuthor {
    pub name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub url: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub icon_url: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub proxy_icon_url: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EmbedProvider {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub url: Option<String>,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct EmbedFooter {
    pub text: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub icon_url: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub proxy_icon_url: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EmbedField {
    pub name: String,
    pub value: String,
    #[serde(default)]
    pub inline: bool,
}

impl MessageEmbed {
    pub fn new(embed_type: impl Into<String>) -> Self {
        Self {
            embed_type: embed_type.into(),
            url: None,
            title: None,
            description: None,
            color: None,
            timestamp: None,
            thumbnail: None,
            image: None,
            video: None,
            audio: None,
            author: None,
            provider: None,
            footer: None,
            fields: None,
            html: None,
            html_width: None,
            html_height: None,
            children: None,
            nsfw: None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn unfurl_request_ignores_caller_supplied_media_proxy_fields() {
        let request = serde_json::json!({
            "op": "Unfurl",
            "url": "https://fxtwitter.com/example/status/1",
            "nsfw_mode": "block",
            "bypass_cache": false,
            "cache_only": false,
            "media_endpoint": "https://caller.example/media",
            "media_proxy_endpoint": "https://caller.example/media",
            "media_proxy_secret_key": "caller-secret"
        });

        let parsed: UnfurlRequest =
            serde_json::from_value(request).expect("unfurl request should deserialize");

        match parsed {
            UnfurlRequest::Unfurl {
                url,
                nsfw_mode,
                bypass_cache,
                cache_only,
                youtube_api_key,
                klipy_api_key,
            } => {
                assert_eq!(url, "https://fxtwitter.com/example/status/1");
                assert_eq!(nsfw_mode, Some(NsfwMode::Block));
                assert!(!bypass_cache);
                assert!(!cache_only);
                assert!(youtube_api_key.is_none());
                assert!(klipy_api_key.is_none());
            }
            UnfurlRequest::Invalidate { .. } => panic!("expected unfurl request"),
        }
    }
}
