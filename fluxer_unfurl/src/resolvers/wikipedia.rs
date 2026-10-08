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

#[cfg(test)]
mod tests {
    use super::*;

    fn u(s: &str) -> Url {
        Url::parse(s).unwrap()
    }

    #[test]
    fn matches_wikipedia_hosts() {
        let w = WikipediaResolver;
        assert!(w.matches(&u(
            "https://en.wikipedia.org/wiki/Rust_(programming_language)"
        )));
        assert!(w.matches(&u("https://de.wikipedia.org/wiki/Hund")));
        assert!(w.matches(&u("https://ja.wikipedia.org/wiki/Dog")));
    }

    #[test]
    fn no_match_non_wiki_path() {
        assert!(!WikipediaResolver.matches(&u("https://en.wikipedia.org/w/index.php")));
    }

    #[test]
    fn no_match_other_host() {
        assert!(!WikipediaResolver.matches(&u("https://example.com/wiki/Test")));
    }

    #[test]
    fn extract_language_from_host() {
        assert_eq!(extract_language("en.wikipedia.org"), "en");
        assert_eq!(extract_language("de.wikipedia.org"), "de");
        assert_eq!(extract_language("www.wikipedia.org"), "en");
    }

    #[test]
    fn is_wikipedia_host_recognizes_supported_langs() {
        assert!(is_wikipedia_host("en.wikipedia.org"));
        assert!(is_wikipedia_host("fr.wikipedia.org"));
        assert!(is_wikipedia_host("wikipedia.org"));
        assert!(!is_wikipedia_host("xx.wikipedia.org"));
    }

    #[test]
    fn normalizes_wikimedia_thumb_urls_for_deduplication() {
        assert_eq!(
            normalize_wiki_url(
                "https://upload.wikimedia.org/wikipedia/commons/thumb/a/ab/Foo.png/220px-Foo.png"
            ),
            "https://upload.wikimedia.org/wikipedia/commons/a/ab/Foo.png"
        );
    }

    #[test]
    fn deduplicates_thumbnail_and_original_after_metadata_enrichment() {
        let thumb = EmbedMedia {
            url: Some(
                "https://upload.wikimedia.org/wikipedia/commons/thumb/a/ab/Foo.png/220px-Foo.png"
                    .to_owned(),
            ),
            width: Some(220),
            ..Default::default()
        };
        let original = EmbedMedia {
            url: Some("https://upload.wikimedia.org/wikipedia/commons/a/ab/Foo.png".to_owned()),
            width: Some(1024),
            ..Default::default()
        };
        let unique = deduplicate_thumbnails(vec![Some(thumb.clone()), Some(original)]);
        assert_eq!(unique.len(), 1);
        assert_eq!(unique[0].url, thumb.url);
        assert_eq!(unique[0].width, thumb.width);
    }

    #[test]
    fn build_embed_media_payload_prefers_api_dimensions_and_keeps_metadata() {
        let meta = MediaMetadata {
            content_type: "image/jpeg".to_owned(),
            content_hash: "hash".to_owned(),
            width: Some(640),
            height: Some(480),
            duration: None,
            placeholder: Some("placeholder".to_owned()),
            animated: Some(false),
            nsfw: true,
        };
        let media = build_embed_media_payload("https://example.com/a.jpg", &meta, Some(320), None);
        assert_eq!(media.url.as_deref(), Some("https://example.com/a.jpg"));
        assert_eq!(media.width, Some(320));
        assert_eq!(media.height, Some(480));
        assert_eq!(media.content_hash.as_deref(), Some("hash"));
        assert_eq!(media.content_type.as_deref(), Some("image/jpeg"));
        assert_eq!(media.placeholder.as_deref(), Some("placeholder"));
        assert_eq!(media.flags, 1 << 4);
    }

    #[test]
    fn matches_all_supported_languages() {
        let w = WikipediaResolver;
        for lang in SUPPORTED_LANGS {
            let url_str = format!("https://{lang}.wikipedia.org/wiki/Test");
            assert!(w.matches(&u(&url_str)), "should match {lang}.wikipedia.org");
        }
    }

    #[test]
    fn does_not_match_unsupported_language() {
        let w = WikipediaResolver;
        assert!(!w.matches(&u("https://xx.wikipedia.org/wiki/Test")));
        assert!(!w.matches(&u("https://sv.wikipedia.org/wiki/Test")));
    }

    #[test]
    fn matches_bare_wikipedia_org() {
        assert!(WikipediaResolver.matches(&u("https://wikipedia.org/wiki/Test")));
        assert!(WikipediaResolver.matches(&u("https://www.wikipedia.org/wiki/Test")));
    }

    #[test]
    fn requires_wiki_path() {
        assert!(!WikipediaResolver.matches(&u("https://en.wikipedia.org/w/index.php?title=Test")));
        assert!(!WikipediaResolver.matches(&u("https://en.wikipedia.org/")));
    }

    #[test]
    fn extract_language_defaults_to_en() {
        assert_eq!(extract_language("www.wikipedia.org"), "en");
        assert_eq!(extract_language("unknown.wikipedia.org"), "en");
    }

    #[test]
    fn normalise_wiki_url_non_thumb_passthrough() {
        let url = "https://upload.wikimedia.org/wikipedia/commons/a/ab/Foo.png";
        assert_eq!(normalize_wiki_url(url), url);
    }

    #[test]
    fn normalise_wiki_url_non_wikimedia_passthrough() {
        let url = "https://other.example.com/wikipedia/commons/thumb/a/ab/Foo.png/220px-Foo.png";
        assert_eq!(normalize_wiki_url(url), url);
    }

    #[test]
    fn decode_html_entities_in_title() {
        let result = parse_text("Schr&ouml;dinger&#39;s cat", 256);
        assert!(result.starts_with("Schr\u{00f6}dinger"));
        assert!(result.contains("s cat"));
    }

    #[test]
    fn deduplication_keeps_first_occurrence() {
        let a = EmbedMedia {
            url: Some(
                "https://upload.wikimedia.org/wikipedia/commons/thumb/a/ab/Foo.png/220px-Foo.png"
                    .to_owned(),
            ),
            width: Some(220),
            ..Default::default()
        };
        let b = EmbedMedia {
            url: Some("https://upload.wikimedia.org/wikipedia/commons/a/ab/Foo.png".to_owned()),
            width: Some(1024),
            ..Default::default()
        };
        let unique = deduplicate_thumbnails(vec![Some(a.clone()), Some(b)]);
        assert_eq!(unique.len(), 1);
        assert_eq!(unique[0].width, Some(220));
    }

    #[test]
    fn deduplication_keeps_distinct_images() {
        let a = EmbedMedia {
            url: Some("https://upload.wikimedia.org/wikipedia/commons/a/ab/Foo.png".to_owned()),
            ..Default::default()
        };
        let b = EmbedMedia {
            url: Some("https://upload.wikimedia.org/wikipedia/commons/b/bc/Bar.png".to_owned()),
            ..Default::default()
        };
        let unique = deduplicate_thumbnails(vec![Some(a), Some(b)]);
        assert_eq!(unique.len(), 2);
    }

    #[test]
    fn deduplication_handles_none() {
        let a = EmbedMedia {
            url: Some("https://e.com/a.png".to_owned()),
            ..Default::default()
        };
        let unique = deduplicate_thumbnails(vec![None, Some(a), None]);
        assert_eq!(unique.len(), 1);
    }
}
