// SPDX-License-Identifier: AGPL-3.0-or-later

use super::{ResolveContext, Resolver, ResolverResult};
use crate::http_fetch;
use crate::media_proxy::{MediaMetadata, MediaProxyClient, embed_media_flags};
use crate::text_limits;
use crate::types::{EmbedMedia, MessageEmbed};
use serde::Deserialize;
use std::future::Future;
use std::pin::Pin;
use std::time::Duration;
use url::Url;

const SUPPORTED_LANGS: &[&str] = &["en", "de", "fr", "es", "it", "ja", "ru", "zh"];

pub struct WikipediaResolver;

#[derive(Debug, Deserialize)]
struct WikiSummary {
    title: Option<String>,
    extract: Option<String>,
    thumbnail: Option<WikiImage>,
    originalimage: Option<WikiImage>,
}

#[derive(Debug, Deserialize)]
struct WikiImage {
    source: String,
    width: Option<u32>,
    height: Option<u32>,
}

impl Resolver for WikipediaResolver {
    fn matches(&self, url: &Url) -> bool {
        let host = match url.host_str() {
            Some(h) => h.to_ascii_lowercase(),
            None => return false,
        };

        url.path().starts_with("/wiki/") && is_wikipedia_host(&host)
    }

    fn resolve<'a>(
        &'a self,
        ctx: &'a ResolveContext<'_>,
    ) -> Pin<Box<dyn Future<Output = anyhow::Result<ResolverResult>> + Send + 'a>> {
        Box::pin(async move {
            let host = ctx
                .url
                .host_str()
                .ok_or_else(|| anyhow::anyhow!("no host"))?
                .to_ascii_lowercase();

            let lang = extract_language(&host);
            let raw_title = ctx.url.path().strip_prefix("/wiki/").unwrap_or_default();

            if raw_title.is_empty() {
                return Ok(ResolverResult { embeds: vec![] });
            }
            let title = urlencoding::decode(raw_title)
                .map(|title| title.into_owned())
                .unwrap_or_else(|_| raw_title.to_owned());
            let encoded_title = urlencoding::encode(&title);

            let api_url = format!(
                "https://{lang}.wikipedia.org/api/rest_v1/page/summary/{}",
                encoded_title
            );

            let result = http_fetch::fetch_url(
                &ctx.http_client,
                &api_url,
                256 * 1024,
                Duration::from_secs(5),
            )
            .await?;

            if result.status != 200 {
                return Ok(ResolverResult { embeds: vec![] });
            }

            let summary: WikiSummary = serde_json::from_slice(&result.bytes)?;
            let nsfw_str = MediaProxyClient::nsfw_mode_str(ctx.nsfw_mode);
            let thumbnail = match process_thumbnail(ctx, summary.thumbnail.as_ref(), nsfw_str).await
            {
                Ok(thumbnail) => thumbnail,
                Err(err) => {
                    tracing::warn!(error = %err, "failed to enrich Wikipedia thumbnail metadata");
                    return Ok(ResolverResult { embeds: vec![] });
                }
            };
            let original_image = match process_thumbnail(
                ctx,
                summary.originalimage.as_ref(),
                nsfw_str,
            )
            .await
            {
                Ok(original_image) => original_image,
                Err(err) => {
                    tracing::warn!(error = %err, "failed to enrich Wikipedia original image metadata");
                    return Ok(ResolverResult { embeds: vec![] });
                }
            };
            let unique_images = deduplicate_thumbnails(vec![thumbnail, original_image]);

            let mut embed = MessageEmbed::new("article");
            embed.url = Some(ctx.url.to_string());

            if let Some(ref t) = summary.title {
                embed.title = Some(parse_text(t, text_limits::TITLE_MAX));
            }
            if let Some(ref e) = summary.extract {
                embed.description = Some(parse_text(e, 350));
            }

            embed.thumbnail = unique_images.first().cloned();

            let mut embeds = vec![embed];
            for image in unique_images.into_iter().skip(1) {
                let mut extra = MessageEmbed::new("rich");
                extra.url = Some(ctx.url.to_string());
                extra.image = Some(image);
                embeds.push(extra);
            }

            Ok(ResolverResult { embeds })
        })
    }
}

async fn process_thumbnail(
    ctx: &ResolveContext<'_>,
    thumbnail_data: Option<&WikiImage>,
    nsfw_mode: &str,
) -> anyhow::Result<Option<EmbedMedia>> {
    let Some(thumbnail) = thumbnail_data else {
        return Ok(None);
    };
    let meta = ctx
        .media_proxy
        .get_metadata(&thumbnail.source, nsfw_mode)
        .await?;
    Ok(Some(build_embed_media_payload(
        &thumbnail.source,
        &meta,
        thumbnail.width,
        thumbnail.height,
    )))
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

fn parse_text(value: &str, max_len: usize) -> String {
    text_limits::truncate(decode_html_entities(value).trim(), max_len)
}

fn decode_html_entities(input: &str) -> String {
    scraper::Html::parse_fragment(input)
        .root_element()
        .text()
        .collect()
}

fn deduplicate_thumbnails(images: Vec<Option<EmbedMedia>>) -> Vec<EmbedMedia> {
    let mut seen = std::collections::HashSet::new();
    let mut unique = Vec::new();
    for image in images.into_iter().flatten() {
        let Some(normalized) = image.url.as_deref().map(normalize_wiki_url) else {
            unique.push(image);
            continue;
        };
        if seen.insert(normalized) {
            unique.push(image);
        }
    }
    unique
}

fn is_wikipedia_host(host: &str) -> bool {
    host == "wikipedia.org"
        || host == "www.wikipedia.org"
        || SUPPORTED_LANGS
            .iter()
            .any(|lang| host == format!("{lang}.wikipedia.org"))
}

fn extract_language(host: &str) -> &str {
    let subdomain = host.split('.').next().unwrap_or("en");
    if SUPPORTED_LANGS.contains(&subdomain) {
        subdomain
    } else {
        "en"
    }
}

fn normalize_wiki_url(url_str: &str) -> String {
    if let Ok(mut parsed) = Url::parse(url_str) {
        if parsed.host_str() == Some("upload.wikimedia.org")
            && parsed.path().contains("/wikipedia/commons/thumb/")
        {
            let segments: Vec<&str> = parsed.path().split('/').collect();
            if let Some(thumb_idx) = segments.iter().position(|s| *s == "thumb")
                && segments.len() > thumb_idx + 2
            {
                let mut normalized: Vec<&str> = Vec::new();
                normalized.extend_from_slice(&segments[..thumb_idx]);
                normalized.extend_from_slice(&segments[thumb_idx + 1..segments.len() - 1]);
                let path = normalized.join("/");
                let path = if path.starts_with('/') {
                    path
                } else {
                    format!("/{path}")
                };
                parsed.set_path(&path);
            }
        }
        parsed.as_str().trim_end_matches('/').to_owned()
    } else {
        url_str.trim_end_matches('/').to_owned()
    }
}
