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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn detects_media_content_types() {
        assert_eq!(
            media_kind_from_content_type("image/png"),
            Some(MediaKind::Image)
        );
        assert_eq!(
            media_kind_from_content_type("video/mp4; charset=binary"),
            Some(MediaKind::Video)
        );
        assert_eq!(
            media_kind_from_content_type("audio/mpeg"),
            Some(MediaKind::Audio)
        );
        assert_eq!(
            media_kind_from_content_type("application/vnd.apple.mpegurl"),
            Some(MediaKind::Video)
        );
        assert_eq!(media_kind_from_content_type("application/ogg"), None);
        assert_eq!(media_kind_from_content_type("text/html"), None);
    }

    #[test]
    fn falls_back_to_extension_only_for_unknown_binary_types() {
        let media_url = Url::parse("https://example.com/file.mp4").unwrap();
        let htmlish_url = Url::parse("https://example.com/file.mp4").unwrap();
        assert_eq!(
            media_kind_from_response("application/octet-stream", &media_url, &[]),
            Some(MediaKind::Video)
        );
        assert_eq!(
            media_kind_from_response("text/html", &htmlish_url, &[]),
            None
        );
        assert_eq!(
            media_kind_from_response(
                "application/ogg",
                &Url::parse("https://example.com/file.ogv").unwrap(),
                &[]
            ),
            Some(MediaKind::Video)
        );
        assert_eq!(
            media_kind_from_response(
                "application/ogg",
                &Url::parse("https://example.com/file.ogg").unwrap(),
                &[]
            ),
            Some(MediaKind::Audio)
        );
    }

    #[test]
    fn detects_extensionless_media_from_magic_bytes() {
        let png = [
            0x89, b'P', b'N', b'G', b'\r', b'\n', 0x1a, b'\n', 0, 0, 0, 0,
        ];
        assert_eq!(media_kind_from_magic_bytes(&png), Some(MediaKind::Image));
        assert_eq!(
            media_kind_from_response("", &Url::parse("https://example.com/opaque").unwrap(), &png),
            Some(MediaKind::Image)
        );
    }

    #[test]
    fn content_type_with_charset_is_normalised() {
        assert_eq!(
            media_kind_from_content_type("image/jpeg; charset=utf-8"),
            Some(MediaKind::Image)
        );
        assert_eq!(
            media_kind_from_content_type("VIDEO/MP4; codecs=\"avc1\""),
            Some(MediaKind::Video)
        );
    }

    #[test]
    fn application_mp4_detected_as_video() {
        assert_eq!(
            media_kind_from_content_type("application/mp4"),
            Some(MediaKind::Video)
        );
    }

    #[test]
    fn hls_types_detected_as_video() {
        assert_eq!(
            media_kind_from_content_type("application/vnd.apple.mpegurl"),
            Some(MediaKind::Video)
        );
        assert_eq!(
            media_kind_from_content_type("application/x-mpegurl"),
            Some(MediaKind::Video)
        );
    }

    #[test]
    fn application_ogg_not_in_content_type_alone() {
        assert_eq!(media_kind_from_content_type("application/ogg"), None);
    }

    #[test]
    fn text_html_never_treated_as_media() {
        assert_eq!(
            media_kind_from_response(
                "text/html",
                &Url::parse("https://e.com/page.mp4").unwrap(),
                &[]
            ),
            None
        );
        assert_eq!(
            media_kind_from_response(
                "text/plain",
                &Url::parse("https://e.com/file.mp3").unwrap(),
                &[]
            ),
            None
        );
    }

    #[test]
    fn binary_octet_stream_falls_back_to_extension() {
        assert_eq!(
            media_kind_from_response(
                "binary/octet-stream",
                &Url::parse("https://e.com/f.jpg").unwrap(),
                &[]
            ),
            Some(MediaKind::Image)
        );
    }

    #[test]
    fn empty_content_type_falls_back_to_extension_then_magic() {
        assert_eq!(
            media_kind_from_response("", &Url::parse("https://e.com/f.wav").unwrap(), &[]),
            Some(MediaKind::Audio)
        );
        let mp4_ftyp = b"\x00\x00\x00\x1cftypisom\x00\x00\x02\x00";
        assert_eq!(
            media_kind_from_response("", &Url::parse("https://e.com/noext").unwrap(), mp4_ftyp),
            Some(MediaKind::Video)
        );
    }

    #[test]
    fn application_ogg_with_ogv_extension_is_video() {
        assert_eq!(
            media_kind_from_response(
                "application/ogg",
                &Url::parse("https://e.com/clip.ogv").unwrap(),
                &[]
            ),
            Some(MediaKind::Video)
        );
    }

    #[test]
    fn application_ogg_with_oga_extension_is_audio() {
        assert_eq!(
            media_kind_from_response(
                "application/ogg",
                &Url::parse("https://e.com/song.oga").unwrap(),
                &[]
            ),
            Some(MediaKind::Audio)
        );
    }

    #[test]
    fn application_ogg_without_extension_defaults_audio() {
        assert_eq!(
            media_kind_from_response(
                "application/ogg",
                &Url::parse("https://e.com/stream").unwrap(),
                &[]
            ),
            Some(MediaKind::Audio)
        );
    }

    #[test]
    fn application_mp4_with_video_extension() {
        assert_eq!(
            media_kind_from_response(
                "application/mp4",
                &Url::parse("https://e.com/clip.m4v").unwrap(),
                &[]
            ),
            Some(MediaKind::Video)
        );
    }

    #[test]
    fn unknown_content_type_no_extension_no_magic_returns_none() {
        assert_eq!(
            media_kind_from_response(
                "application/json",
                &Url::parse("https://e.com/api").unwrap(),
                &[]
            ),
            None
        );
    }
}
