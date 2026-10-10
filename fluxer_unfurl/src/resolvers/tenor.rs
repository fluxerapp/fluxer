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
        name: "Tenor".to_owned(),
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
    fn valid_absolute_url_rejects_non_http() {
        assert!(valid_absolute_url("ftp://example.com/file").is_none());
        assert!(valid_absolute_url("javascript:alert(1)").is_none());
    }

    #[test]
    fn valid_absolute_url_rejects_relative() {
        assert!(valid_absolute_url("/relative/path").is_none());
    }
}
