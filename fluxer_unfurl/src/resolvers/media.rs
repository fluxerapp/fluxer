// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::direct_media::{MediaKind, detect_media_kind};
use crate::media_proxy::{MediaMetadata, MediaProxyClient, embed_media_flags};
use crate::types::{EmbedMedia, MessageEmbed, NsfwMode};
use url::Url;

pub fn media_kind_from_content_type(content_type: &str) -> Option<MediaKind> {
    let content_type = content_type
        .split(';')
        .next()
        .unwrap_or(content_type)
        .trim()
        .to_ascii_lowercase();
    if content_type.starts_with("image/") {
        Some(MediaKind::Image)
    } else if content_type.starts_with("video/") {
        Some(MediaKind::Video)
    } else if content_type.starts_with("audio/") {
        Some(MediaKind::Audio)
    } else if matches!(
        content_type.as_str(),
        "application/mp4" | "application/vnd.apple.mpegurl" | "application/x-mpegurl"
    ) {
        Some(MediaKind::Video)
    } else {
        None
    }
}

pub fn media_kind_from_response(
    content_type: &str,
    final_url: &Url,
    bytes: &[u8],
) -> Option<MediaKind> {
    if let Some(kind) = media_kind_from_content_type(content_type) {
        return Some(kind);
    }
    let normalized = content_type
        .split(';')
        .next()
        .unwrap_or(content_type)
        .trim()
        .to_ascii_lowercase();
    if normalized == "application/ogg" {
        return detect_media_kind(final_url).or(Some(MediaKind::Audio));
    }
    if matches!(
        normalized.as_str(),
        "application/mp4" | "application/vnd.apple.mpegurl" | "application/x-mpegurl"
    ) {
        return detect_media_kind(final_url).or(Some(MediaKind::Video));
    }
    if normalized.is_empty()
        || normalized == "application/octet-stream"
        || normalized == "binary/octet-stream"
    {
        return detect_media_kind(final_url).or_else(|| media_kind_from_magic_bytes(bytes));
    }
    None
}

pub fn media_kind_from_magic_bytes(bytes: &[u8]) -> Option<MediaKind> {
    let mime_type = infer::get(bytes)?.mime_type();
    media_kind_from_content_type(mime_type)
}

pub async fn build_direct_media_embed(
    media_proxy: &MediaProxyClient,
    url: &Url,
    nsfw_mode: NsfwMode,
    kind: MediaKind,
) -> anyhow::Result<MessageEmbed> {
    let nsfw_str = MediaProxyClient::nsfw_mode_str(nsfw_mode);
    let meta = media_proxy.get_metadata(url.as_str(), nsfw_str).await?;
    Ok(media_embed(url, &meta, kind))
}

pub async fn build_own_media_embed(
    media_proxy: &MediaProxyClient,
    url: &Url,
    nsfw_mode: NsfwMode,
) -> anyhow::Result<Option<MessageEmbed>> {
    let nsfw_str = MediaProxyClient::nsfw_mode_str(nsfw_mode);
    let meta = media_proxy.get_metadata(url.as_str(), nsfw_str).await?;
    let Some(kind) =
        media_kind_from_content_type(&meta.content_type).or_else(|| detect_media_kind(url))
    else {
        return Ok(None);
    };
    Ok(Some(media_embed(url, &meta, kind)))
}

fn media_embed(url: &Url, meta: &MediaMetadata, kind: MediaKind) -> MessageEmbed {
    let url_str = url.to_string();
    let media = EmbedMedia {
        url: Some(url_str.clone()),
        content_type: Some(meta.content_type.clone()),
        content_hash: Some(meta.content_hash.clone()),
        width: meta.width,
        height: meta.height,
        placeholder: meta.placeholder.clone(),
        duration: meta.duration.map(|duration| duration as u32),
        flags: embed_media_flags(meta),
        ..Default::default()
    };

    let mut embed = match kind {
        MediaKind::Image => {
            let mut embed = MessageEmbed::new("image");
            embed.thumbnail = Some(media);
            embed
        }
        MediaKind::Video => {
            let mut embed = MessageEmbed::new("video");
            embed.video = Some(media);
            embed
        }
        MediaKind::Audio => {
            let mut embed = MessageEmbed::new("audio");
            embed.audio = Some(media);
            embed
        }
    };
    embed.url = Some(url_str);
    embed
}
