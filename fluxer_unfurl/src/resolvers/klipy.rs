// SPDX-License-Identifier: AGPL-3.0-or-later

use super::{ResolveContext, Resolver, ResolverResult};
use crate::http_fetch;
use crate::media_proxy::{MediaMetadata, MediaProxyClient, embed_media_flags};
use crate::types::{EmbedMedia, EmbedProvider, MessageEmbed};
use std::future::Future;
use std::pin::Pin;
use std::time::Duration;
use url::Url;

const KLIPY_FLIGHT_CHUNK_MAX_BYTES: usize = 512 * 1024;
const KLIPY_API_BASE_URL: &str = "https://api.klipy.com/api/v1";
const KLIPY_API_MAX_BYTES: usize = 512 * 1024;
const KLIPY_API_TIMEOUT: Duration = Duration::from_secs(10);
const KLIPY_SIZE_PREFERENCE: &[&str] = &["hd", "md", "sm", "xs"];
const KLIPY_THUMBNAIL_FORMATS: &[&str] = &["webp", "gif"];
const KLIPY_VIDEO_FORMATS: &[&str] = &["mp4", "webm"];

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
        url.host_str()
            .is_some_and(|h| h.eq_ignore_ascii_case("klipy.com"))
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
            let api_key = ctx.klipy_api_key.clone().or_else(klipy_api_key);
            let formats = match api_key {
                Some(key) => match resolve_media_via_api(ctx, &key).await {
                    Ok(Some(formats)) => Some(formats),
                    Ok(None) => resolve_media_via_scrape(ctx).await?,
                    Err(err) => {
                        tracing::warn!(
                            error = %err,
                            "KLIPY API resolution failed; falling back to page scrape"
                        );
                        resolve_media_via_scrape(ctx).await?
                    }
                },
                None => resolve_media_via_scrape(ctx).await?,
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
    if !url
        .host_str()
        .is_some_and(|h| h.eq_ignore_ascii_case("klipy.com"))
    {
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

async fn resolve_media_via_scrape(
    ctx: &ResolveContext<'_>,
) -> anyhow::Result<Option<KlipyMediaFormats>> {
    let result = http_fetch::fetch_url(
        &ctx.http_client,
        ctx.url.as_str(),
        http_fetch::DEFAULT_HTML_MAX_BYTES,
        Duration::from_secs(10),
    )
    .await?;

    if result.status != 200 {
        return Ok(None);
    }

    let html = String::from_utf8_lossy(&result.bytes);
    Ok(extract_klipy_media(&html))
}

async fn resolve_media_via_api(
    ctx: &ResolveContext<'_>,
    api_key: &str,
) -> anyhow::Result<Option<KlipyMediaFormats>> {
    let Some((kind, slug)) = klipy_path(&ctx.original_url) else {
        return Ok(None);
    };
    let resource = klipy_resource(&kind);

    let url = Url::parse(&format!("{KLIPY_API_BASE_URL}/{api_key}/{resource}/{slug}"))?;

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
            "KLIPY API lookup returned non-200"
        );
        return Ok(None);
    }

    let payload: serde_json::Value = serde_json::from_slice(&response.bytes)?;
    let Some(file) = payload.pointer("/data/file") else {
        return Ok(None);
    };

    let thumbnail = pick_klipy_file_format(file, KLIPY_THUMBNAIL_FORMATS);
    let video = pick_klipy_file_format(file, KLIPY_VIDEO_FORMATS);
    if thumbnail.is_none() && video.is_none() {
        return Ok(None);
    }
    Ok(Some(KlipyMediaFormats { thumbnail, video }))
}

fn pick_klipy_file_format(file: &serde_json::Value, formats: &[&str]) -> Option<KlipyMediaFormat> {
    for size in KLIPY_SIZE_PREFERENCE {
        for format in formats {
            if let Some(media) = extract_media_format(file.pointer(&format!("/{size}/{format}")))
                && media.url.is_some()
            {
                return Some(media);
            }
        }
    }
    for format in formats {
        if let Some(url) = file.get(*format).and_then(|v| v.as_str())
            && !url.is_empty()
        {
            return Some(KlipyMediaFormat {
                url: Some(url.to_owned()),
                width: None,
                height: None,
            });
        }
    }
    None
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

fn extract_klipy_media(html: &str) -> Option<KlipyMediaFormats> {
    static FLIGHT_RE: std::sync::LazyLock<regex::Regex> = std::sync::LazyLock::new(|| {
        regex::Regex::new(r#"(?s)self\.__next_f\.push\(\[1,"(.*?)"\]\)"#).expect("valid regex")
    });

    for cap in FLIGHT_RE.captures_iter(html) {
        let encoded = cap.get(1)?.as_str();
        if encoded.len() > KLIPY_FLIGHT_CHUNK_MAX_BYTES {
            continue;
        }
        if let Some(media) = parse_next_flight_data(encoded) {
            return Some(media);
        }
    }

    None
}

fn parse_next_flight_data(encoded: &str) -> Option<KlipyMediaFormats> {
    let unescaped = serde_json::from_str::<String>(&format!("\"{encoded}\"")).ok()?;
    let colon_idx = unescaped.find(':')?;
    let json_str = &unescaped[colon_idx + 1..];
    if json_str.len() > KLIPY_FLIGHT_CHUNK_MAX_BYTES {
        return None;
    }

    let arr: Vec<serde_json::Value> = serde_json::from_str(json_str).ok()?;

    for item in &arr {
        if let Some(media) = item.get("media")
            && media.get("file").is_some()
        {
            return Some(KlipyMediaFormats {
                thumbnail: extract_media_format(media.pointer("/file/hd/webp")),
                video: extract_media_format(media.pointer("/file/hd/mp4")),
            });
        }
    }

    None
}

fn extract_media_format(value: Option<&serde_json::Value>) -> Option<KlipyMediaFormat> {
    let value = value?;
    Some(KlipyMediaFormat {
        url: value
            .get("url")
            .and_then(|v| v.as_str())
            .map(|url| url.to_owned()),
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
    fn parse_next_flight_data_extracts_media_formats() {
        let encoded = r#"0:[{\"media\":{\"file\":{\"hd\":{\"webp\":{\"url\":\"https://img.example/a.webp\",\"width\":320,\"height\":180},\"mp4\":{\"url\":\"https://img.example/a.mp4\",\"width\":640,\"height\":360}}}}}]"#;
        assert_eq!(
            parse_next_flight_data(encoded),
            Some(KlipyMediaFormats {
                thumbnail: Some(KlipyMediaFormat {
                    url: Some("https://img.example/a.webp".to_owned()),
                    width: Some(320),
                    height: Some(180),
                }),
                video: Some(KlipyMediaFormat {
                    url: Some("https://img.example/a.mp4".to_owned()),
                    width: Some(640),
                    height: Some(360),
                }),
            })
        );
    }

    #[test]
    fn extract_klipy_media_skips_oversized_flight_chunks() {
        let html = format!(
            r#"self.__next_f.push([1,"{}"])"#,
            "x".repeat(KLIPY_FLIGHT_CHUNK_MAX_BYTES + 1)
        );
        assert_eq!(extract_klipy_media(&html), None);
    }

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
    fn extract_klipy_media_from_realistic_html() {
        let html = r#"
        <script>self.__next_f.push([1,"0:[{\"media\":{\"file\":{\"hd\":{\"webp\":{\"url\":\"https://img.klipy.com/hd.webp\",\"width\":640,\"height\":360},\"mp4\":{\"url\":\"https://img.klipy.com/hd.mp4\",\"width\":1280,\"height\":720}}}}}]"])</script>
        "#;
        let result = extract_klipy_media(html);
        assert!(result.is_some());
        let formats = result.unwrap();
        assert_eq!(
            formats.thumbnail.as_ref().unwrap().url.as_deref(),
            Some("https://img.klipy.com/hd.webp")
        );
        assert_eq!(
            formats.video.as_ref().unwrap().url.as_deref(),
            Some("https://img.klipy.com/hd.mp4")
        );
    }

    #[test]
    fn extract_klipy_media_returns_none_for_non_media_chunks() {
        let html = r#"<script>self.__next_f.push([1,"0:[{\"status\":\"ok\"}]"])</script>"#;
        assert!(extract_klipy_media(html).is_none());
    }

    #[test]
    fn resolve_relative_url_rejects_non_http() {
        let base = Url::parse("https://klipy.com/gifs/test").unwrap();
        assert!(resolve_relative_url(&base, "ftp://evil.com/file").is_none());
    }

    #[test]
    fn klipy_path_extracts_kind_and_slug() {
        let (kind, slug) =
            klipy_path(&Url::parse("https://klipy.com/gifs/funny-cat-123").unwrap()).unwrap();
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
            Some("https://img.klipy.com/hd.mp4")
        );
    }

    #[test]
    fn pick_klipy_file_format_handles_clip_string_shape() {
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
    fn pick_klipy_file_format_falls_back_to_smaller_size() {
        let file = serde_json::json!({
            "sm": {"webp": {"url": "https://img.klipy.com/sm.webp", "width": 165, "height": 294}}
        });
        assert_eq!(
            pick_klipy_file_format(&file, KLIPY_THUMBNAIL_FORMATS)
                .unwrap()
                .url
                .as_deref(),
            Some("https://img.klipy.com/sm.webp")
        );
        assert!(pick_klipy_file_format(&file, KLIPY_VIDEO_FORMATS).is_none());
    }

    #[test]
    fn pick_klipy_file_format_none_when_empty() {
        let file = serde_json::json!({});
        assert!(pick_klipy_file_format(&file, KLIPY_THUMBNAIL_FORMATS).is_none());
    }
}
