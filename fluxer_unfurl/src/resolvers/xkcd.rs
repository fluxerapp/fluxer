// SPDX-License-Identifier: AGPL-3.0-or-later

use super::{ResolveContext, Resolver, ResolverResult};
use crate::charset;
use crate::html_parser;
use crate::http_fetch;
use crate::media_proxy::{MediaMetadata, MediaProxyClient, embed_media_flags};
use crate::text_limits;
use crate::types::{EmbedFooter, EmbedMedia, MessageEmbed};
use scraper::{Html, Selector};
use std::future::Future;
use std::pin::Pin;
use std::time::Duration;
use url::Url;

const XKCD_COLOR: u32 = 0x000000;

pub struct XkcdResolver;

impl Resolver for XkcdResolver {
    fn matches(&self, url: &Url) -> bool {
        url.host_str()
            .is_some_and(|h| h.eq_ignore_ascii_case("xkcd.com"))
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

            let title = og.title.as_deref().map(|t| parse_text(t, 70));

            let image_alt = {
                let doc = Html::parse_document(&html);
                extract_comic_alt(&doc)
            };
            let footer_text = image_alt.clone();
            let image_media = match og.image.as_deref() {
                Some(image_url) => {
                    resolve_media_url(ctx, image_url, image_alt.clone())
                        .await
                        .unwrap_or_else(|err| {
                            tracing::warn!(error = %err, url = image_url, "failed to enrich xkcd image metadata");
                            None
                        })
                }
                None => None,
            };

            let mut embed = MessageEmbed::new("rich");
            embed.url = Some(ctx.url.to_string());
            embed.title = title;
            embed.color = Some(XKCD_COLOR);
            embed.image = image_media;

            if let Some(text) = footer_text {
                embed.footer = Some(EmbedFooter {
                    text,
                    ..Default::default()
                });
            }

            Ok(ResolverResult {
                embeds: vec![embed],
            })
        })
    }
}

async fn resolve_media_url(
    ctx: &ResolveContext<'_>,
    media_url: &str,
    description: Option<String>,
) -> anyhow::Result<Option<EmbedMedia>> {
    let Some(resolved_url) = resolve_relative_url(&ctx.url, media_url) else {
        return Ok(None);
    };
    let nsfw_str = MediaProxyClient::nsfw_mode_str(ctx.nsfw_mode);
    let meta = ctx
        .media_proxy
        .get_metadata(&resolved_url, nsfw_str)
        .await?;
    Ok(Some(build_embed_media_payload(
        &resolved_url,
        &meta,
        None,
        None,
        description,
    )))
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
    description: Option<String>,
) -> EmbedMedia {
    EmbedMedia {
        url: Some(url.to_owned()),
        width: width.or(metadata.width),
        height: height.or(metadata.height),
        description,
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

fn extract_comic_alt(doc: &Html) -> Option<String> {
    let sel = Selector::parse("#comic img").ok()?;
    doc.select(&sel)
        .next()
        .and_then(|el| el.value().attr("title"))
        .map(|s| s.to_owned())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn resolve_relative_url_rejects_non_http() {
        let base = Url::parse("https://xkcd.com/1/").unwrap();
        assert!(resolve_relative_url(&base, "ftp://evil.com/img.png").is_none());
    }
}
