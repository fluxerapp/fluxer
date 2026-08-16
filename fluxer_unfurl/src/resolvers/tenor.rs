// SPDX-License-Identifier: AGPL-3.0-or-later

use super::{ResolveContext, Resolver, ResolverResult};
use crate::charset;
use crate::html_parser;
use crate::http_fetch;
use crate::media_proxy::{MediaMetadata, MediaProxyClient, embed_media_flags};
use crate::types::{EmbedMedia, EmbedProvider, MessageEmbed};
use scraper::{Html, Selector};
use std::future::Future;
use std::pin::Pin;
use std::time::Duration;
use url::Url;

const TENOR_JSON_LD_MAX_BYTES: usize = 256 * 1024;
const TENOR_STATIC_PNG_THUMBNAIL_SUFFIX: &str = "AAAAe";
const TENOR_ANIMATED_WEBP_THUMBNAIL_SUFFIX: &str = "AAAA1";

pub struct TenorResolver;

impl Resolver for TenorResolver {
    fn matches(&self, url: &Url) -> bool {
        url.host_str()
            .is_some_and(|h| h.eq_ignore_ascii_case("tenor.com"))
    }

    fn resolve<'a>(
        &'a self,
        ctx: &'a ResolveContext<'_>,
    ) -> Pin<Box<dyn Future<Output = anyhow::Result<ResolverResult>> + Send + 'a>> {
        Box::pin(async move {
            let result = http_fetch::fetch_url(
                &ctx.http_client,
                ctx.url.as_str(),
                http_fetch::DEFAULT_HTML_MAX_BYTES,
                Duration::from_secs(10),
            )
            .await?;

            if result.status != 200 {
                return Ok(ResolverResult { embeds: vec![] });
            }

            let html = charset::decode_body(
                &result.bytes,
                result
                    .headers
                    .get(reqwest::header::CONTENT_TYPE)
                    .and_then(|value| value.to_str().ok()),
            );
            let og = html_parser::parse_opengraph(&html);
            let nsfw_str = MediaProxyClient::nsfw_mode_str(ctx.nsfw_mode);
            let json_ld = extract_json_ld_urls(&html);
            let thumbnail_url = json_ld
                .as_ref()
                .and_then(|urls| urls.thumbnail_url.clone())
                .or_else(|| {
                    og.image
                        .as_deref()
                        .filter(|url| is_gif_url(url))
                        .map(ToOwned::to_owned)
                })
                .map(|url| tenor_webp_thumbnail_url(&url).unwrap_or(url));
            let video_url = json_ld.and_then(|urls| urls.video_url);

            if thumbnail_url.is_none() && video_url.is_none() {
                return Ok(ResolverResult { embeds: vec![] });
            }

            let thumbnail_future = async {
                match thumbnail_url.as_deref() {
                    Some(url) => resolve_media_url(ctx, url, nsfw_str).await,
                    None => None,
                }
            };
            let video_future = async {
                match video_url.as_deref() {
                    Some(url) => resolve_media_url(ctx, url, nsfw_str).await,
                    None => None,
                }
            };
            let (thumbnail, video) = tokio::join!(thumbnail_future, video_future);
            let Some(embed) = tenor_embed(&ctx.url, thumbnail, video) else {
                return Ok(ResolverResult { embeds: vec![] });
            };

            Ok(ResolverResult {
                embeds: vec![embed],
            })
        })
    }
}

fn tenor_embed(
    source_url: &Url,
    thumbnail: Option<EmbedMedia>,
    video: Option<EmbedMedia>,
) -> Option<MessageEmbed> {
    if thumbnail.is_none() && video.is_none() {
        return None;
    }

    let mut embed = MessageEmbed::new("gifv");
    embed.url = Some(source_url.to_string());
    embed.provider = Some(EmbedProvider {
        name: Some("Tenor".to_owned()),
        url: Some("https://tenor.com".to_owned()),
    });
    embed.thumbnail = thumbnail;
    embed.video = video;
    Some(embed)
}

async fn resolve_media_url(
    ctx: &ResolveContext<'_>,
    media_url: &str,
    nsfw_mode: &str,
) -> Option<EmbedMedia> {
    let resolved_url = match ctx.url.join(media_url) {
        Ok(url) if matches!(url.scheme(), "http" | "https") => url.to_string(),
        Ok(url) => {
            tracing::warn!(url = %url, "rejected unsafe Tenor media URL");
            return None;
        }
        Err(err) => {
            tracing::warn!(error = %err, url = media_url, "failed to resolve Tenor media URL");
            return None;
        }
    };
    let meta = match ctx.media_proxy.get_metadata(&resolved_url, nsfw_mode).await {
        Ok(meta) => meta,
        Err(err) => {
            tracing::warn!(error = %err, url = resolved_url, "failed to enrich Tenor media metadata");
            return None;
        }
    };
    Some(build_embed_media_payload(&resolved_url, &meta))
}

fn build_embed_media_payload(url: &str, metadata: &MediaMetadata) -> EmbedMedia {
    EmbedMedia {
        url: Some(url.to_owned()),
        width: metadata.width,
        height: metadata.height,
        placeholder: metadata.placeholder.clone(),
        flags: embed_media_flags(metadata),
        content_hash: Some(metadata.content_hash.clone()),
        content_type: Some(metadata.content_type.clone()),
        duration: metadata.duration.map(|duration| duration as u32),
        ..Default::default()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct TenorJsonLdUrls {
    thumbnail_url: Option<String>,
    video_url: Option<String>,
}

fn extract_json_ld_urls(html: &str) -> Option<TenorJsonLdUrls> {
    let doc = Html::parse_document(html);
    let selector = Selector::parse(r#"script.dynamic[type="application/ld+json"]"#).ok()?;

    for script in doc.select(&selector) {
        let json_str = script.text().collect::<String>();
        if json_str.len() > TENOR_JSON_LD_MAX_BYTES {
            continue;
        }
        if let Ok(value) = serde_json::from_str::<serde_json::Value>(&json_str) {
            let image = value
                .get("image")
                .and_then(|i| i.get("thumbnailUrl"))
                .and_then(|v| v.as_str())
                .and_then(valid_absolute_url)
                .or_else(|| {
                    value
                        .get("image")
                        .and_then(|i| i.get("contentUrl"))
                        .and_then(|v| v.as_str())
                        .and_then(valid_absolute_url)
                });
            let video = value
                .get("video")
                .and_then(|v| v.get("contentUrl"))
                .and_then(|v| v.as_str())
                .and_then(valid_absolute_url);

            return Some(TenorJsonLdUrls {
                thumbnail_url: image,
                video_url: video,
            });
        }
    }

    None
}

fn is_gif_url(value: &str) -> bool {
    Url::parse(value)
        .map(|url| url.path().to_ascii_lowercase().ends_with(".gif"))
        .unwrap_or_else(|_| value.to_ascii_lowercase().ends_with(".gif"))
}

fn tenor_webp_thumbnail_url(value: &str) -> Option<String> {
    let mut url = Url::parse(value).ok()?;
    if !url
        .host_str()
        .is_some_and(|host| host.eq_ignore_ascii_case("media.tenor.com"))
    {
        return None;
    }
    if !url.path().to_ascii_lowercase().ends_with(".png") {
        return None;
    }

    let mut path_segments = url
        .path_segments()?
        .map(ToOwned::to_owned)
        .collect::<Vec<_>>();
    if path_segments.len() != 2 {
        return None;
    }

    let media_key = path_segments.first_mut()?;
    let media_id = media_key.strip_suffix(TENOR_STATIC_PNG_THUMBNAIL_SUFFIX)?;
    *media_key = format!("{media_id}{TENOR_ANIMATED_WEBP_THUMBNAIL_SUFFIX}");

    let filename = path_segments.last_mut()?;
    filename.truncate(filename.len() - ".png".len());
    filename.push_str(".webp");

    url.set_path(&format!("/{}", path_segments.join("/")));
    Some(url.to_string())
}

fn valid_absolute_url(value: &str) -> Option<String> {
    let url = Url::parse(value).ok()?;
    if matches!(url.scheme(), "http" | "https") {
        Some(value.to_owned())
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn extract_json_ld_urls_ignores_oversized_script_blocks() {
        let html = format!(
            r#"<script class="dynamic" type="application/ld+json">{}</script>"#,
            "x".repeat(TENOR_JSON_LD_MAX_BYTES + 1)
        );
        assert_eq!(extract_json_ld_urls(&html), None);
    }

    #[test]
    fn extract_json_ld_urls_extracts_media() {
        let html = r#"<script class="dynamic" type="application/ld+json">{"image":{"contentUrl":"https://tenor.example/a.gif","thumbnailUrl":"https://tenor.example/a.png"},"video":{"contentUrl":"https://tenor.example/a.mp4"}}</script>"#;
        assert_eq!(
            extract_json_ld_urls(html),
            Some(TenorJsonLdUrls {
                thumbnail_url: Some("https://tenor.example/a.png".to_owned()),
                video_url: Some("https://tenor.example/a.mp4".to_owned())
            })
        );
    }

    #[test]
    fn extract_json_ld_urls_requires_dynamic_script_like_ts() {
        let html = r#"<script type="application/ld+json">{"image":{"thumbnailUrl":"https://tenor.example/a.png"}}</script>"#;
        assert_eq!(extract_json_ld_urls(html), None);
    }

    #[test]
    fn extract_json_ld_urls_keeps_valid_json_ld_without_media_urls() {
        let html =
            r#"<script class="dynamic" type="application/ld+json">{"name":"empty"}</script>"#;
        assert_eq!(
            extract_json_ld_urls(html),
            Some(TenorJsonLdUrls {
                thumbnail_url: None,
                video_url: None
            })
        );
    }

    #[test]
    fn extract_json_ld_urls_falls_back_to_thumbnail_url() {
        let html = r#"<script class="dynamic" type="application/ld+json">{"image":{"thumbnailUrl":"https://tenor.example/a.png"}}</script>"#;
        assert_eq!(
            extract_json_ld_urls(html),
            Some(TenorJsonLdUrls {
                thumbnail_url: Some("https://tenor.example/a.png".to_owned()),
                video_url: None,
            })
        );
    }

    #[test]
    fn gif_url_detection_uses_pathname() {
        assert!(is_gif_url("https://media.tenor.com/a/b/c.gif?cache=1"));
        assert!(!is_gif_url("https://media.tenor.com/a/b/c.gifv"));
    }

    #[test]
    fn tenor_webp_thumbnail_url_rewrites_static_png_preview() {
        assert_eq!(
            tenor_webp_thumbnail_url("https://media.tenor.com/pdX9YTI4_eoAAAAe/cat.png").as_deref(),
            Some("https://media.tenor.com/pdX9YTI4_eoAAAA1/cat.webp")
        );
        assert_eq!(
            tenor_webp_thumbnail_url("https://media.tenor.com/pdX9YTI4_eoAAAAe/cat.PNG").as_deref(),
            Some("https://media.tenor.com/pdX9YTI4_eoAAAA1/cat.webp")
        );
    }

    #[test]
    fn tenor_webp_thumbnail_url_preserves_query_string() {
        assert_eq!(
            tenor_webp_thumbnail_url(
                "https://media.tenor.com/pdX9YTI4_eoAAAAe/cat.png?hh=320&ww=320"
            )
            .as_deref(),
            Some("https://media.tenor.com/pdX9YTI4_eoAAAA1/cat.webp?hh=320&ww=320")
        );
    }

    #[test]
    fn tenor_webp_thumbnail_url_rejects_unknown_shapes() {
        assert!(tenor_webp_thumbnail_url("https://example.com/pdX9YTI4_eoAAAAe/cat.png").is_none());
        assert!(
            tenor_webp_thumbnail_url("https://media.tenor.com/pdX9YTI4_eoAAAAM/cat.gif").is_none()
        );
        assert!(
            tenor_webp_thumbnail_url("https://media.tenor.com/pdX9YTI4_eoAAAAe/cat.jpg").is_none()
        );
        assert!(
            tenor_webp_thumbnail_url("https://media.tenor.com/nested/pdX9YTI4_eoAAAAe/cat.png")
                .is_none()
        );
    }

    #[test]
    fn build_embed_media_payload_keeps_metadata_fields() {
        let meta = MediaMetadata {
            format: "mp4".to_owned(),
            content_type: "video/mp4".to_owned(),
            content_hash: "hash".to_owned(),
            size: 123,
            width: Some(320),
            height: Some(240),
            duration: Some(1.5),
            placeholder: Some("placeholder".to_owned()),
            animated: Some(true),
            nsfw: true,
            nsfw_probability: None,
        };
        let media = build_embed_media_payload("https://tenor.example/a.mp4", &meta);
        assert_eq!(media.content_type.as_deref(), Some("video/mp4"));
        assert_eq!(media.content_hash.as_deref(), Some("hash"));
        assert_eq!(media.width, Some(320));
        assert_eq!(media.height, Some(240));
        assert_eq!(media.duration, Some(1));
        assert_eq!(media.placeholder.as_deref(), Some("placeholder"));
        assert_eq!(media.flags, (1 << 4) | (1 << 5));
    }

    #[test]
    fn tenor_embed_returns_none_without_enriched_media() {
        let source_url = Url::parse("https://tenor.com/view/cat-gif-12345").unwrap();

        assert!(tenor_embed(&source_url, None, None).is_none());
    }

    #[test]
    fn tenor_embed_keeps_enriched_media() {
        let source_url = Url::parse("https://tenor.com/view/cat-gif-12345").unwrap();
        let video = EmbedMedia {
            url: Some("https://media.tenor.com/cat.mp4".to_owned()),
            content_type: Some("video/mp4".to_owned()),
            ..Default::default()
        };

        let embed = tenor_embed(&source_url, None, Some(video)).expect("embed");

        assert_eq!(embed.embed_type, "gifv");
        assert_eq!(embed.url.as_deref(), Some(source_url.as_str()));
        assert!(embed.thumbnail.is_none());
        assert_eq!(
            embed.video.as_ref().and_then(|media| media.url.as_deref()),
            Some("https://media.tenor.com/cat.mp4")
        );
        assert_eq!(
            embed
                .provider
                .as_ref()
                .and_then(|provider| provider.name.as_deref()),
            Some("Tenor")
        );
    }

    #[test]
    fn matches_tenor_com_only() {
        let r = TenorResolver;
        assert!(r.matches(&Url::parse("https://tenor.com/view/cat-gif-12345").unwrap()));
        assert!(!r.matches(&Url::parse("https://media.tenor.com/images/abc.gif").unwrap()));
        assert!(!r.matches(&Url::parse("https://nottenor.com/view/test").unwrap()));
    }

    #[test]
    fn is_gif_url_with_query_params() {
        assert!(is_gif_url("https://media.tenor.com/a/b.gif?hh=320&ww=320"));
    }

    #[test]
    fn is_gif_url_rejects_non_gif() {
        assert!(!is_gif_url("https://media.tenor.com/a/b.mp4"));
        assert!(!is_gif_url("https://media.tenor.com/a/b.png"));
        assert!(!is_gif_url("https://media.tenor.com/a/b.webp"));
    }

    #[test]
    fn is_gif_url_case_insensitive() {
        assert!(is_gif_url("https://media.tenor.com/a/b.GIF"));
    }

    #[test]
    fn valid_absolute_url_rejects_non_http() {
        assert!(valid_absolute_url("ftp://example.com/file").is_none());
        assert!(valid_absolute_url("javascript:alert(1)").is_none());
    }

    #[test]
    fn valid_absolute_url_accepts_http_and_https() {
        assert!(valid_absolute_url("http://example.com/file").is_some());
        assert!(valid_absolute_url("https://example.com/file").is_some());
    }

    #[test]
    fn valid_absolute_url_rejects_relative() {
        assert!(valid_absolute_url("/relative/path").is_none());
    }

    #[test]
    fn extract_json_ld_urls_with_content_url_and_thumbnail() {
        let html = r#"<script class="dynamic" type="application/ld+json">{"image":{"contentUrl":"https://t.example/content.gif","thumbnailUrl":"https://t.example/thumb.png"}}</script>"#;
        let urls = extract_json_ld_urls(html).unwrap();
        assert_eq!(
            urls.thumbnail_url.as_deref(),
            Some("https://t.example/thumb.png")
        );
    }

    #[test]
    fn extract_json_ld_urls_prefers_thumbnail_url_over_content_url() {
        let html = r#"<script class="dynamic" type="application/ld+json">{"image":{"contentUrl":"https://t.example/a.gif","thumbnailUrl":"https://t.example/thumb.png"}}</script>"#;
        let urls = extract_json_ld_urls(html).unwrap();
        assert_eq!(
            urls.thumbnail_url.as_deref(),
            Some("https://t.example/thumb.png")
        );
    }
}
