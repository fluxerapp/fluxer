// SPDX-License-Identifier: AGPL-3.0-or-later

use super::{ResolveContext, Resolver, ResolverResult};
use crate::http_fetch;
use crate::types::{EmbedMedia, EmbedProvider, MessageEmbed};
use std::future::Future;
use std::pin::Pin;
use std::time::Duration;
use url::Url;

const CURATED_PROVIDER_NSFW_MODE: &str = "allow";
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
        let path = fluxer_common::klipy::parse_share_path(url)?;
        let resource = path.kind.resource();
        let slug = path.slug;
        Url::parse(&format!("https://klipy.com/{resource}/{slug}/player")).ok()
    }

    fn resolve<'a>(
        &'a self,
        ctx: &'a ResolveContext<'_>,
    ) -> Pin<Box<dyn Future<Output = anyhow::Result<ResolverResult>> + Send + 'a>> {
        Box::pin(async move {
            let Some(api_key) = ctx.klipy_api_key.clone() else {
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
                name: "KLIPY".to_owned(),
                url: Some("https://klipy.com".to_owned()),
            });
            if let Some(ref thumbnail) = formats.thumbnail {
                embed.thumbnail =
                    resolve_klipy_media(ctx, thumbnail, CURATED_PROVIDER_NSFW_MODE).await;
            }

            if let Some(ref video) = formats.video {
                embed.video = resolve_klipy_media(ctx, video, CURATED_PROVIDER_NSFW_MODE).await;
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
    let resolved_url = resolve_http_url(&ctx.original_url, url)?;
    let meta = match ctx.media_proxy.get_metadata(&resolved_url, nsfw_mode).await {
        Ok(meta) => meta,
        Err(err) => {
            tracing::warn!(error = %err, url = resolved_url, "failed to enrich KLIPY media metadata");
            return None;
        }
    };
    Some(meta.to_embed_media_sized(&resolved_url, format.width, format.height))
}

async fn resolve_media_via_api(
    ctx: &ResolveContext<'_>,
    api_key: &str,
) -> anyhow::Result<Option<KlipyMediaFormats>> {
    let Some(path) = fluxer_common::klipy::parse_share_path(&ctx.original_url) else {
        return Ok(None);
    };
    let url = fluxer_common::klipy::direct_items_url(api_key, path.kind, &path.slug)?;
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
    Ok(payload
        .pointer("/data/data/0")
        .and_then(extract_klipy_api_media))
}

fn extract_klipy_api_media(item: &serde_json::Value) -> Option<KlipyMediaFormats> {
    let file = item.get("file")?;
    let file_meta = item.get("file_meta");
    let thumbnail = pick_klipy_file_format(file, file_meta, KLIPY_THUMBNAIL_FORMATS);
    let video = pick_klipy_file_format(file, file_meta, KLIPY_VIDEO_FORMATS);

    if thumbnail.is_none() && video.is_none() {
        return None;
    }
    Some(KlipyMediaFormats { thumbnail, video })
}

fn pick_klipy_file_format(
    file: &serde_json::Value,
    file_meta: Option<&serde_json::Value>,
    formats: &[&str],
) -> Option<KlipyMediaFormat> {
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
        let Some(mut media) = extract_media_format(file.get(*media_format)) else {
            continue;
        };
        if media.width.is_none() {
            media.width = klipy_meta_dimension(file_meta, media_format, "width");
        }
        if media.height.is_none() {
            media.height = klipy_meta_dimension(file_meta, media_format, "height");
        }
        return Some(media);
    }
    None
}

fn klipy_meta_dimension(
    file_meta: Option<&serde_json::Value>,
    media_format: &str,
    key: &str,
) -> Option<u32> {
    file_meta?
        .pointer(&format!("/{media_format}/{key}"))
        .and_then(serde_json::Value::as_u64)
        .filter(|value| *value > 0)
        .and_then(|value| u32::try_from(value).ok())
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

use super::default_helpers::resolve_http_url;
