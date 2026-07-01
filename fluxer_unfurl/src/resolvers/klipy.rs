// SPDX-License-Identifier: AGPL-3.0-or-later

use super::{ResolveContext, Resolver, ResolverResult};
use crate::http_fetch;
use crate::media_proxy::{MediaMetadata, MediaProxyClient, embed_media_flags};
use crate::types::{EmbedMedia, EmbedProvider, MessageEmbed};
use std::future::Future;
use std::pin::Pin;
use std::time::Duration;
use url::Url;

const KLIPY_API_V1_BASE_URL: &str = "https://api.klipy.com/api/v1";
const KLIPY_API_MAX_BYTES: usize = 512 * 1024;
const KLIPY_API_TIMEOUT: Duration = Duration::from_secs(10);
const KLIPY_SIZE_PREFERENCE: &[&str] = &["hd", "md", "sm", "xs"];
const KLIPY_THUMBNAIL_FORMATS: &[&str] = &["webp", "gif"];
const KLIPY_VIDEO_FORMATS: &[&str] = &["webm", "mp4"];

pub struct KlipyResolver;

#[derive(Debug, Clone, Default, PartialEq)]
struct KlipyMediaFormat {
    url: Option<String>,
    width: Option<u32>,
    height: Option<u32>,
}

#[derive(Debug, Clone, Default, PartialEq)]
struct KlipyMediaFormats {
    thumbnail: Option<KlipyMediaFormat>,
    video: Option<KlipyMediaFormat>,
}

impl Resolver for KlipyResolver {
    fn matches(&self, url: &Url) -> bool {
        is_klipy_host(url)
    }

    fn transform_url(&self, url: &Url) -> Option<Url> {
        let (kind, slug) = klipy_path(url)?;
        let resource = klipy_resource(&kind);
        Url::parse(&format!("https://klipy.com/{resource}/{slug}/player")).ok()
    }

    fn resolve<'a>(
        &'a self,
        ctx: &'a ResolveContext<'_>,
    ) -> Pin<Box<dyn Future<Output = anyhow::Result<ResolverResult>> + Send + 'a>> {
        Box::pin(async move {
            let Some(api_key) = ctx.klipy_api_key.clone().or_else(klipy_api_key) else {
                return Ok(ResolverResult { embeds: vec![] });
            };
            let formats = match resolve_media_via_api(ctx, &api_key).await {
                Ok(formats) => formats,
                Err(err) => {
                    tracing::warn!(error = %err, "KLIPY API resolution failed");
                    None
                }
            };

            let Some(formats) = formats else {
                return Ok(ResolverResult { embeds: vec![] });
            };

            let mut embed = MessageEmbed::new("gifv");
            embed.url = Some(ctx.original_url.to_string());
            embed.provider = Some(EmbedProvider {
                name: Some("KLIPY".to_owned()),
                url: Some("https://klipy.com".to_owned()),
            });
            let nsfw_str = MediaProxyClient::nsfw_mode_str(ctx.nsfw_mode);

            if let Some(ref thumbnail) = formats.thumbnail {
                embed.thumbnail = resolve_klipy_media(ctx, thumbnail, nsfw_str).await;
            }

            if let Some(ref video) = formats.video {
                embed.video = resolve_klipy_media(ctx, video, nsfw_str).await;
            }

            Ok(ResolverResult {
                embeds: vec![embed],
            })
        })
    }
}

fn is_klipy_host(url: &Url) -> bool {
    url.host_str().is_some_and(|h| {
        h.eq_ignore_ascii_case("klipy.com") || h.eq_ignore_ascii_case("www.klipy.com")
    })
}

async fn resolve_klipy_media(
    ctx: &ResolveContext<'_>,
    format: &KlipyMediaFormat,
    nsfw_mode: &str,
) -> Option<EmbedMedia> {
    let url = format.url.as_deref()?;
    let resolved_url = resolve_relative_url(&ctx.original_url, url)?;
    let meta = match ctx.media_proxy.get_metadata(&resolved_url, nsfw_mode).await {
        Ok(meta) => meta,
        Err(err) => {
            tracing::warn!(error = %err, url = resolved_url, "failed to enrich KLIPY media metadata");
            return None;
        }
    };
    Some(build_embed_media_payload(
        &resolved_url,
        &meta,
        format.width,
        format.height,
    ))
}

fn klipy_path(url: &Url) -> Option<(String, String)> {
    if !is_klipy_host(url) {
        return None;
    }
    static PATH_RE: std::sync::LazyLock<regex::Regex> = std::sync::LazyLock::new(|| {
        regex::Regex::new(r"^/(gif|gifs|clip|clips)/([^/]+)").expect("valid regex")
    });
    let caps = PATH_RE.captures(url.path())?;
    Some((
        caps.get(1)?.as_str().to_owned(),
        caps.get(2)?.as_str().to_owned(),
    ))
}

fn klipy_resource(kind: &str) -> &'static str {
    if kind.starts_with("clip") {
        "clips"
    } else {
        "gifs"
    }
}

fn klipy_api_key() -> Option<String> {
    std::env::var("FLUXER_KLIPY_API_KEY")
        .ok()
        .filter(|key| !key.is_empty())
        .or_else(|| {
            std::env::var("KLIPY_API_KEY")
                .ok()
                .filter(|key| !key.is_empty())
        })
}

async fn resolve_media_via_api(
    ctx: &ResolveContext<'_>,
    api_key: &str,
) -> anyhow::Result<Option<KlipyMediaFormats>> {
    let Some((kind, slug)) = klipy_path(&ctx.original_url) else {
        return Ok(None);
    };
    let resource = klipy_resource(&kind);
    let url = klipy_direct_url(api_key, resource, &slug)?;
    let response = http_fetch::fetch_url(
        &ctx.http_client,
        url.as_str(),
        KLIPY_API_MAX_BYTES,
        KLIPY_API_TIMEOUT,
    )
    .await?;

    if response.status != 200 {
        tracing::warn!(
            status = response.status,
            "KLIPY direct API lookup returned non-200 status"
        );
        return Ok(None);
    }

    let payload: serde_json::Value = serde_json::from_slice(&response.bytes)?;
    Ok(payload.get("data").and_then(extract_klipy_api_media))
}

fn klipy_direct_url(api_key: &str, resource: &str, slug: &str) -> anyhow::Result<Url> {
    Ok(Url::parse(&format!(
        "{KLIPY_API_V1_BASE_URL}/{api_key}/{resource}/{slug}"
    ))?)
}

fn extract_klipy_api_media(item: &serde_json::Value) -> Option<KlipyMediaFormats> {
    let file = item.get("file");
    let thumbnail = file.and_then(|file| pick_klipy_file_format(file, KLIPY_THUMBNAIL_FORMATS));
    let video = file
        .and_then(|file| pick_klipy_file_format(file, KLIPY_VIDEO_FORMATS))
        .or_else(|| extract_klipy_fallback_webm(item.pointer("/media_formats/webm")));

    if thumbnail.is_none() && video.is_none() {
        return None;
    }
    Some(KlipyMediaFormats { thumbnail, video })
}

fn pick_klipy_file_format(file: &serde_json::Value, formats: &[&str]) -> Option<KlipyMediaFormat> {
    for size in KLIPY_SIZE_PREFERENCE {
        for media_format in formats {
            if let Some(media) =
                extract_media_format(file.pointer(&format!("/{size}/{media_format}")))
            {
                return Some(media);
            }
        }
    }
    for media_format in formats {
        if let Some(media) = extract_media_format(file.get(*media_format)) {
            return Some(media);
        }
    }
    None
}

fn extract_klipy_fallback_webm(value: Option<&serde_json::Value>) -> Option<KlipyMediaFormat> {
    let value = value?;
    let url = value
        .get("url")
        .and_then(|v| v.as_str())
        .filter(|url| !url.is_empty())?;
    let dims = value.get("dims")?.as_array()?;
    let width = dims
        .first()
        .and_then(|value| value.as_i64())
        .filter(|value| *value > 0)
        .and_then(|value| u32::try_from(value).ok());
    let height = dims
        .get(1)
        .and_then(|value| value.as_i64())
        .filter(|value| *value > 0)
        .and_then(|value| u32::try_from(value).ok());
    Some(KlipyMediaFormat {
        url: Some(url.to_owned()),
        width,
        height,
    })
}

fn resolve_relative_url(base_url: &Url, media_url: &str) -> Option<String> {
    let url = base_url.join(media_url).ok()?;
    if matches!(url.scheme(), "http" | "https") {
        Some(url.to_string())
    } else {
        None
    }
}

fn build_embed_media_payload(
    url: &str,
    metadata: &MediaMetadata,
    width: Option<u32>,
    height: Option<u32>,
) -> EmbedMedia {
    EmbedMedia {
        url: Some(url.to_owned()),
        width: width.or(metadata.width),
        height: height.or(metadata.height),
        placeholder: metadata.placeholder.clone(),
        flags: embed_media_flags(metadata),
        content_hash: Some(metadata.content_hash.clone()),
        content_type: Some(metadata.content_type.clone()),
        duration: metadata.duration.map(|duration| duration as u32),
        ..Default::default()
    }
}

fn extract_media_format(value: Option<&serde_json::Value>) -> Option<KlipyMediaFormat> {
    let value = value?;
    if let Some(url) = value.as_str().filter(|url| !url.is_empty()) {
        return Some(KlipyMediaFormat {
            url: Some(url.to_owned()),
            width: None,
            height: None,
        });
    }
    let url = value
        .get("url")
        .and_then(|v| v.as_str())
        .filter(|url| !url.is_empty())?;
    Some(KlipyMediaFormat {
        url: Some(url.to_owned()),
        width: value
            .get("width")
            .and_then(|v| v.as_u64())
            .and_then(|width| u32::try_from(width).ok()),
        height: value
            .get("height")
            .and_then(|v| v.as_u64())
            .and_then(|height| u32::try_from(height).ok()),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn resolve_relative_url_uses_original_url_like_ts() {
        let base = Url::parse("https://klipy.com/gifs/funny").unwrap();
        assert_eq!(
            resolve_relative_url(&base, "/media/a.webp").as_deref(),
            Some("https://klipy.com/media/a.webp")
        );
    }

    #[test]
    fn build_embed_media_payload_prefers_format_dimensions() {
        let meta = MediaMetadata {
            format: "webp".to_owned(),
            content_type: "image/webp".to_owned(),
            content_hash: "hash".to_owned(),
            size: 123,
            width: Some(640),
            height: Some(360),
            duration: Some(2.9),
            placeholder: Some("placeholder".to_owned()),
            animated: Some(true),
            nsfw: false,
            nsfw_probability: None,
        };
        let media = build_embed_media_payload("https://img.example/a.webp", &meta, Some(320), None);
        assert_eq!(media.width, Some(320));
        assert_eq!(media.height, Some(360));
        assert_eq!(media.duration, Some(2));
        assert_eq!(media.content_hash.as_deref(), Some("hash"));
        assert_eq!(media.content_type.as_deref(), Some("image/webp"));
        assert_eq!(media.placeholder.as_deref(), Some("placeholder"));
        assert_eq!(media.flags, 1 << 5);
    }

    #[test]
    fn matches_klipy_com_only() {
        let r = KlipyResolver;
        assert!(r.matches(&Url::parse("https://klipy.com/gifs/funny").unwrap()));
        assert!(!r.matches(&Url::parse("https://notklipy.com/gifs/funny").unwrap()));
    }

    #[test]
    fn transform_url_normalises_gif_to_gifs_player() {
        let r = KlipyResolver;
        let transformed = r.transform_url(&Url::parse("https://klipy.com/gif/abc-123").unwrap());
        assert_eq!(
            transformed.as_ref().map(|u| u.as_str()),
            Some("https://klipy.com/gifs/abc-123/player")
        );
    }

    #[test]
    fn transform_url_normalises_clip_to_clips_player() {
        let r = KlipyResolver;
        let transformed = r.transform_url(&Url::parse("https://klipy.com/clip/xyz").unwrap());
        assert_eq!(
            transformed.as_ref().map(|u| u.as_str()),
            Some("https://klipy.com/clips/xyz/player")
        );
    }

    #[test]
    fn transform_url_already_pluralised() {
        let r = KlipyResolver;
        let transformed = r.transform_url(&Url::parse("https://klipy.com/gifs/abc").unwrap());
        assert_eq!(
            transformed.as_ref().map(|u| u.as_str()),
            Some("https://klipy.com/gifs/abc/player")
        );
    }

    #[test]
    fn transform_url_non_matching_path_returns_none() {
        let r = KlipyResolver;
        assert!(
            r.transform_url(&Url::parse("https://klipy.com/about").unwrap())
                .is_none()
        );
    }

    #[test]
    fn klipy_path_extracts_kind_and_slug() {
        let (kind, slug) =
            klipy_path(&Url::parse("https://www.klipy.com/gifs/funny-cat-123").unwrap()).unwrap();
        assert_eq!(kind, "gifs");
        assert_eq!(slug, "funny-cat-123");
        assert!(klipy_path(&Url::parse("https://klipy.com/about").unwrap()).is_none());
        assert!(klipy_path(&Url::parse("https://notklipy.com/gifs/x").unwrap()).is_none());
    }

    #[test]
    fn klipy_resource_maps_kind_to_api_segment() {
        assert_eq!(klipy_resource("gif"), "gifs");
        assert_eq!(klipy_resource("gifs"), "gifs");
        assert_eq!(klipy_resource("clip"), "clips");
        assert_eq!(klipy_resource("clips"), "clips");
    }

    #[test]
    fn pick_klipy_file_format_prefers_hd_and_format_order() {
        let file = serde_json::json!({
            "hd": {
                "webp": {"url": "https://img.klipy.com/hd.webp", "width": 254, "height": 450},
                "webm": {"url": "https://img.klipy.com/hd.webm", "width": 254, "height": 450},
                "mp4": {"url": "https://img.klipy.com/hd.mp4", "width": 254, "height": 450}
            },
            "sm": {
                "webp": {"url": "https://img.klipy.com/sm.webp", "width": 165, "height": 294}
            }
        });
        let thumbnail = pick_klipy_file_format(&file, KLIPY_THUMBNAIL_FORMATS).unwrap();
        assert_eq!(
            thumbnail.url.as_deref(),
            Some("https://img.klipy.com/hd.webp")
        );
        assert_eq!(thumbnail.width, Some(254));
        assert_eq!(thumbnail.height, Some(450));
        assert_eq!(
            pick_klipy_file_format(&file, KLIPY_VIDEO_FORMATS)
                .unwrap()
                .url
                .as_deref(),
            Some("https://img.klipy.com/hd.webm")
        );
    }

    #[test]
    fn pick_klipy_file_format_handles_string_shape() {
        let file = serde_json::json!({
            "mp4": "https://img.klipy.com/c.mp4",
            "gif": "https://img.klipy.com/c.gif",
            "webp": "https://img.klipy.com/c.webp"
        });
        let thumbnail = pick_klipy_file_format(&file, KLIPY_THUMBNAIL_FORMATS).unwrap();
        assert_eq!(
            thumbnail.url.as_deref(),
            Some("https://img.klipy.com/c.webp")
        );
        assert_eq!(thumbnail.width, None);
        assert_eq!(
            pick_klipy_file_format(&file, KLIPY_VIDEO_FORMATS)
                .unwrap()
                .url
                .as_deref(),
            Some("https://img.klipy.com/c.mp4")
        );
    }

    #[test]
    fn extract_klipy_api_media_uses_fallback_webm_shape() {
        let item = serde_json::json!({
            "media_formats": {
                "webm": {
                    "url": "https://img.klipy.com/fallback.webm",
                    "dims": [320, 180]
                }
            }
        });
        let media = extract_klipy_api_media(&item).unwrap();
        let video = media.video.unwrap();
        assert_eq!(
            video.url.as_deref(),
            Some("https://img.klipy.com/fallback.webm")
        );
        assert_eq!(video.width, Some(320));
        assert_eq!(video.height, Some(180));
        assert!(media.thumbnail.is_none());
    }

    #[tokio::test]
    #[ignore = "hits the live KLIPY API and local media proxy"]
    async fn live_klipy_embed_resolves_real_media() {
        let api_key = std::env::var("FLUXER_KLIPY_API_KEY").expect("FLUXER_KLIPY_API_KEY set");
        let media_proxy_endpoint =
            std::env::var("FLUXER_MEDIA_PROXY_ENDPOINT").expect("FLUXER_MEDIA_PROXY_ENDPOINT set");
        let media_proxy_secret = std::env::var("FLUXER_MEDIA_PROXY_SECRET_KEY")
            .expect("FLUXER_MEDIA_PROXY_SECRET_KEY set");
        let media_proxy_public_endpoint = std::env::var("FLUXER_MEDIA_PROXY_PUBLIC_ENDPOINT").ok();
        let raw_url = std::env::var("FLUXER_KLIPY_LIVE_URL")
            .unwrap_or_else(|_| "https://klipy.com/gifs/goatplaybanjo-chat-4".to_owned());

        let resolver = KlipyResolver;
        let original_url = Url::parse(&raw_url).expect("valid live KLIPY URL");
        let url = resolver
            .transform_url(&original_url)
            .unwrap_or_else(|| original_url.clone());
        let media_proxy = crate::media_proxy::MediaProxyClient::new_with_public_endpoint(
            &media_proxy_endpoint,
            &media_proxy_secret,
            media_proxy_public_endpoint.as_deref(),
            reqwest::Client::new(),
        );
        let ctx = ResolveContext {
            url,
            original_url: original_url.clone(),
            http_client: reqwest::Client::new(),
            nsfw_mode: crate::types::NsfwMode::Allow,
            media_proxy: &media_proxy,
            static_cdn_endpoint: "",
            youtube_api_key: None,
            klipy_api_key: Some(api_key),
        };

        let result = resolver.resolve(&ctx).await.expect("resolve KLIPY embed");
        assert_eq!(result.embeds.len(), 1);
        let embed = &result.embeds[0];
        assert_eq!(embed.embed_type, "gifv");
        assert_eq!(embed.url.as_deref(), Some(original_url.as_str()));
        assert_eq!(
            embed
                .provider
                .as_ref()
                .and_then(|provider| provider.name.as_deref()),
            Some("KLIPY")
        );
        let video = embed.video.as_ref().expect("video media resolved");
        assert!(
            video
                .url
                .as_deref()
                .is_some_and(|url| url.starts_with("https://"))
        );
        assert!(video.width.is_some_and(|width| width > 0));
        assert!(video.height.is_some_and(|height| height > 0));
        assert!(video.content_type.as_deref().is_some_and(|content_type| {
            content_type.starts_with("video/") || content_type == "image/gif"
        }));
    }

    #[test]
    fn resolve_relative_url_rejects_non_http() {
        let base = Url::parse("https://klipy.com/gifs/test").unwrap();
        assert!(resolve_relative_url(&base, "ftp://evil.com/file").is_none());
    }
}
