// SPDX-License-Identifier: AGPL-3.0-or-later

use super::ResolveContext;
use crate::http_fetch;
use crate::oembed;
use std::borrow::Cow;
use std::time::Duration;
use url::Url;

const MEDIAWIKI_QUERY: &str =
    "action=query&prop=extracts&exintro=&explaintext=&format=json&titles=";
const MEDIAWIKI_MAX_BYTES: usize = 256 * 1024;
const MEDIAWIKI_TIMEOUT: Duration = Duration::from_secs(5);

pub struct ImageCandidate {
    pub url: String,
    pub width: Option<u32>,
    pub height: Option<u32>,
}

pub fn build_image_candidates(
    base_url: &Url,
    og: &crate::html_parser::OgMetadata,
    oembed: Option<&oembed::OEmbedResponse>,
) -> Vec<ImageCandidate> {
    let mut candidates = Vec::new();
    let mut seen = std::collections::HashSet::new();
    for url in &og.images {
        if let Some(resolved) = resolve_media_url(base_url, url)
            && let Some(n) = normalize_url(&resolved)
            && seen.insert(n)
        {
            candidates.push(ImageCandidate {
                url: resolved,
                width: None,
                height: None,
            });
        }
    }
    if let Some(o) = oembed {
        let is_photo = o
            .oembed_type
            .as_deref()
            .map(|t| t.eq_ignore_ascii_case("photo"))
            .unwrap_or(false);
        if is_photo {
            add_candidate(
                &mut candidates,
                &mut seen,
                base_url,
                o.url.as_deref(),
                o.width.as_ref().and_then(oembed::parse_dimension),
                o.height.as_ref().and_then(oembed::parse_dimension),
            );
        }
        add_candidate(
            &mut candidates,
            &mut seen,
            base_url,
            o.thumbnail_url.as_deref(),
            o.thumbnail_width.as_ref().and_then(oembed::parse_dimension),
            o.thumbnail_height
                .as_ref()
                .and_then(oembed::parse_dimension),
        );
    }
    candidates
}

fn add_candidate(
    cs: &mut Vec<ImageCandidate>,
    seen: &mut std::collections::HashSet<String>,
    base_url: &Url,
    url: Option<&str>,
    w: Option<u32>,
    h: Option<u32>,
) {
    if let Some(u) = url
        && let Some(resolved) = resolve_media_url(base_url, u)
        && let Some(n) = normalize_url(&resolved)
        && seen.insert(n)
    {
        cs.push(ImageCandidate {
            url: resolved,
            width: w,
            height: h,
        });
    }
}

pub fn resolve_media_url(base_url: &Url, value: &str) -> Option<String> {
    let value = value.trim();
    if value.is_empty()
        || value
            .chars()
            .any(|c| c.is_ascii_whitespace() || c.is_control())
    {
        return None;
    }
    let url = base_url.join(value).ok()?;
    matches!(url.scheme(), "http" | "https").then(|| url.to_string())
}

fn normalize_url(url: &str) -> Option<String> {
    Url::parse(url)
        .ok()
        .map(|u| u.as_str().trim_end_matches('/').to_owned())
}

pub async fn fetch_oembed_data(
    ctx: &ResolveContext<'_>,
    html: &str,
) -> Option<oembed::OEmbedResponse> {
    for ep in &oembed::discover_oembed_url(html) {
        let endpoint = ctx
            .url
            .join(&ep.url)
            .map(|url| url.to_string())
            .unwrap_or_else(|_| ep.url.clone());
        if let Ok(data) = oembed::fetch_oembed(&ctx.http_client, &endpoint, ep.format).await {
            return Some(data);
        }
    }
    let hostname = ctx.url.host_str()?;
    let known = oembed::known_oembed_endpoint(hostname, ctx.url.as_str())?;
    oembed::fetch_oembed(&ctx.http_client, &known.url, known.format)
        .await
        .ok()
}

#[derive(Debug, Default, PartialEq, Eq)]
pub struct MediaWikiArticle {
    pub title: Option<String>,
    pub description: Option<String>,
}

pub async fn fetch_mediawiki_article(
    ctx: &ResolveContext<'_>,
    html: &str,
) -> Option<MediaWikiArticle> {
    let query_url = mediawiki_query_url(&ctx.url, html)?;
    let result = http_fetch::fetch_url(
        &ctx.http_client,
        &query_url,
        MEDIAWIKI_MAX_BYTES,
        MEDIAWIKI_TIMEOUT,
    )
    .await
    .ok()?;
    if result.status != 200 {
        return None;
    }
    let json = serde_json::from_slice::<serde_json::Value>(&result.bytes).ok()?;
    let pages = json.get("query").and_then(|query| query.get("pages"))?;
    if !pages.is_object() && !pages.is_array() {
        return None;
    }
    Some(mediawiki_article(pages))
}

fn mediawiki_article(pages: &serde_json::Value) -> MediaWikiArticle {
    let page = match pages {
        serde_json::Value::Object(pages) => pages.values().next(),
        serde_json::Value::Array(pages) => pages.first(),
        _ => None,
    };
    MediaWikiArticle {
        title: page.and_then(|page| mediawiki_text(page, "title")),
        description: page.and_then(|page| mediawiki_text(page, "extract")),
    }
}

fn mediawiki_text(page: &serde_json::Value, key: &str) -> Option<String> {
    let value = page.get(key)?.as_str()?.trim();
    (!value.is_empty()).then(|| value.to_owned())
}

fn mediawiki_query_url(page_url: &Url, html: &str) -> Option<String> {
    let rsd = crate::html_parser::find_rsd_url(html)?;
    let endpoint = page_url.join(&rsd).ok()?;
    if !matches!(endpoint.scheme(), "http" | "https") {
        return None;
    }
    let title = page_url
        .path_segments()?
        .rfind(|segment| !segment.is_empty())?;
    let decoded = urlencoding::decode(title).unwrap_or(Cow::Borrowed(title));
    let title = urlencoding::encode(&decoded);
    let separator = if endpoint.query().is_some() { '&' } else { '?' };
    Some(format!("{endpoint}{separator}{MEDIAWIKI_QUERY}{title}"))
}

pub fn parse_hex_color(s: &str) -> Option<u32> {
    let hex = s.trim().strip_prefix('#')?;
    let ok = (hex.len() == 6 || hex.len() == 3) && hex.chars().all(|c| c.is_ascii_hexdigit());
    if !ok {
        return None;
    }
    match hex.len() {
        6 => u32::from_str_radix(hex, 16).ok(),
        3 => {
            let e: String = hex.chars().flat_map(|c| [c, c]).collect();
            u32::from_str_radix(&e, 16).ok()
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::html_parser::OgMetadata;

    fn url(value: &str) -> Url {
        Url::parse(value).unwrap()
    }

    #[test]
    fn resolve_media_url_resolves_relative_references() {
        let base = url("https://forgetful.vercel.app/posts/page");

        assert_eq!(
            resolve_media_url(&base, "/api/og").as_deref(),
            Some("https://forgetful.vercel.app/api/og")
        );
        assert_eq!(
            resolve_media_url(&base, "images/card.png").as_deref(),
            Some("https://forgetful.vercel.app/posts/images/card.png")
        );
        assert_eq!(
            resolve_media_url(&base, "//cdn.example.com/card.png").as_deref(),
            Some("https://cdn.example.com/card.png")
        );
    }

    #[test]
    fn resolve_media_url_rejects_non_http_and_invalid_references() {
        let base = url("https://example.com/page");

        assert!(resolve_media_url(&base, "javascript:alert(1)").is_none());
        assert!(resolve_media_url(&base, "data:image/png;base64,abcd").is_none());
        assert!(resolve_media_url(&base, "bad url.png").is_none());
    }

    #[test]
    fn build_image_candidates_resolves_relative_og_images() {
        let base = url("https://forgetful.vercel.app/posts/page");
        let og = OgMetadata {
            images: vec![
                "/api/og".to_owned(),
                "images/card.png".to_owned(),
                "//cdn.example.com/card.png".to_owned(),
            ],
            ..Default::default()
        };

        let candidates = build_image_candidates(&base, &og, None);
        let urls = candidates
            .into_iter()
            .map(|candidate| candidate.url)
            .collect::<Vec<_>>();

        assert_eq!(
            urls,
            vec![
                "https://forgetful.vercel.app/api/og".to_owned(),
                "https://forgetful.vercel.app/posts/images/card.png".to_owned(),
                "https://cdn.example.com/card.png".to_owned()
            ]
        );
    }

    #[test]
    fn mediawiki_query_url_appends_params_to_the_rsd_href() {
        let page = url("https://w.example/wiki/Rust");
        let html = r#"<head><link rel="EditURI" type="application/rsd+xml" href="//w.example/w/api.php?action=rsd"></head>"#;
        assert_eq!(
            mediawiki_query_url(&page, html).as_deref(),
            Some(
                "https://w.example/w/api.php?action=rsd&action=query&prop=extracts&exintro=&explaintext=&format=json&titles=Rust"
            )
        );
    }

    #[test]
    fn mediawiki_query_url_uses_question_mark_when_rsd_has_no_query() {
        let page = url("https://w.example/wiki/Rust/");
        let html =
            r#"<head><link rel="EditURI" type="application/rsd+xml" href="/w/api.php"></head>"#;
        assert_eq!(
            mediawiki_query_url(&page, html).as_deref(),
            Some(
                "https://w.example/w/api.php?action=query&prop=extracts&exintro=&explaintext=&format=json&titles=Rust"
            )
        );
    }

    #[test]
    fn mediawiki_query_url_reencodes_the_last_path_segment() {
        let page = url("https://w.example/wiki/Rust_%28programming_language%29");
        let html =
            r#"<head><link rel="EditURI" type="application/rsd+xml" href="/w/api.php"></head>"#;
        assert!(
            mediawiki_query_url(&page, html)
                .unwrap()
                .ends_with("titles=Rust_%28programming_language%29")
        );
    }

    #[test]
    fn mediawiki_query_url_is_none_without_rsd_link() {
        let page = url("https://w.example/wiki/Rust");
        assert!(mediawiki_query_url(&page, "<head></head>").is_none());
    }

    #[test]
    fn build_image_candidates_deduplicates_after_resolution() {
        let base = url("https://forgetful.vercel.app/posts/page");
        let og = OgMetadata {
            images: vec![
                "/api/og".to_owned(),
                "https://forgetful.vercel.app/api/og/".to_owned(),
            ],
            ..Default::default()
        };

        let candidates = build_image_candidates(&base, &og, None);

        assert_eq!(candidates.len(), 1);
        assert_eq!(
            candidates[0].url.as_str(),
            "https://forgetful.vercel.app/api/og"
        );
    }
}
