// SPDX-License-Identifier: AGPL-3.0-or-later

use scraper::{Html, Selector};

const TEXTUAL_KEYS: [&str; 5] = [
    "og:title",
    "og:description",
    "og:site_name",
    "twitter:title",
    "twitter:description",
];

const CLASSIFYING_CARDS: [&str; 3] = ["summary_large_image", "photo", "player"];

const INERT_ELEMENTS: [&str; 4] = ["script", "style", "noscript", "template"];

#[derive(Debug, Default, Clone)]
pub struct OgMetadata {
    pub title: Option<String>,
    pub description: Option<String>,
    pub image: Option<String>,
    pub images: Vec<String>,
    pub image_alt: Option<String>,
    pub video_primary: Option<String>,
    pub audio: Option<String>,
    pub site_name: Option<String>,
    pub og_type: Option<String>,
    pub theme_color: Option<String>,
    pub textual_keys_present: bool,
}

#[derive(Debug, Default, Clone)]
pub struct TwitterCardMetadata {
    pub classifying_card: Option<String>,
    pub player: Option<String>,
}

struct MetaTags {
    entries: Vec<(String, String)>,
}

impl MetaTags {
    fn parse(doc: &Html) -> Self {
        let mut entries = Vec::new();
        let mut stack: Vec<_> = doc.tree.root().children().rev().collect();
        while let Some(node) = stack.pop() {
            let Some(element) = node.value().as_element() else {
                continue;
            };
            if INERT_ELEMENTS.contains(&element.name()) {
                continue;
            }
            stack.extend(node.children().rev());
            if element.name() != "meta" {
                continue;
            }
            let Some(key) = element.attr("property").or_else(|| element.attr("name")) else {
                continue;
            };
            let Some(value) = element
                .attr("content")
                .filter(|value| !value.is_empty())
                .or_else(|| element.attr("value").filter(|value| !value.is_empty()))
            else {
                continue;
            };
            entries.push((key.to_owned(), value.to_owned()));
        }
        Self { entries }
    }

    fn all<'a>(&'a self, key: &'a str) -> impl Iterator<Item = &'a str> {
        self.entries
            .iter()
            .filter(move |(entry_key, _)| entry_key == key)
            .map(|(_, value)| value.as_str())
    }

    fn first(&self, key: &str) -> Option<String> {
        self.all(key).next().map(ToOwned::to_owned)
    }
}

pub fn parse_opengraph(html: &str) -> OgMetadata {
    let doc = Html::parse_document(html);
    let meta = MetaTags::parse(&doc);

    let description = meta
        .first("og:description")
        .or_else(|| meta.first("description"));
    let image = meta
        .first("og:image")
        .or_else(|| meta.first("og:image:secure_url"));
    let video_primary = meta
        .first("og:video")
        .or_else(|| meta.first("og:video:url"));

    let mut og = OgMetadata {
        title: meta.first("og:title"),
        description,
        image,
        images: extract_image_urls(&meta),
        image_alt: meta
            .first("og:image:alt")
            .or_else(|| meta.first("twitter:image:alt"))
            .or_else(|| meta.first("og:image:description")),
        video_primary,
        audio: meta
            .first("og:audio")
            .or_else(|| meta.first("og:audio:url")),
        site_name: meta
            .first("og:site_name")
            .or_else(|| meta.first("twitter:site:name"))
            .or_else(|| meta.first("application-name")),
        og_type: meta.first("og:type"),
        theme_color: meta.first("theme-color"),
        textual_keys_present: TEXTUAL_KEYS
            .iter()
            .any(|key| meta.all(key).next().is_some()),
    };

    if og.title.is_none() {
        og.title = meta
            .first("twitter:title")
            .or_else(|| document_title(&doc))
            .or_else(|| meta.first("title"));
    }

    if og.description.is_none() {
        og.description = meta.first("twitter:description");
    }

    og
}

fn document_title(doc: &Html) -> Option<String> {
    let selector = Selector::parse("title").ok()?;
    let text = doc.select(&selector).next()?.text().collect::<String>();
    let trimmed = text.trim();
    if trimmed.is_empty() {
        return None;
    }
    Some(trimmed.to_owned())
}

pub fn parse_twitter_card(html: &str) -> TwitterCardMetadata {
    let doc = Html::parse_document(html);
    let meta = MetaTags::parse(&doc);

    TwitterCardMetadata {
        classifying_card: meta
            .all("twitter:card")
            .find(|value| CLASSIFYING_CARDS.contains(value))
            .map(ToOwned::to_owned),
        player: meta.first("twitter:player"),
    }
}

pub fn find_rsd_url(html: &str) -> Option<String> {
    let doc = Html::parse_document(html);
    let sel = Selector::parse("link").ok()?;

    for el in doc.select(&sel) {
        let Some(link_type) = el.value().attr("type") else {
            continue;
        };
        if !link_type.eq_ignore_ascii_case("application/rsd+xml") {
            continue;
        }
        let Some(rel) = el.value().attr("rel") else {
            continue;
        };
        if !rel
            .split_whitespace()
            .any(|token| token.eq_ignore_ascii_case("EditURI"))
        {
            continue;
        }
        if let Some(href) = el.value().attr("href").filter(|href| !href.is_empty()) {
            return Some(href.to_owned());
        }
    }

    None
}

pub fn find_activity_pub_link(html: &str) -> Option<String> {
    let doc = Html::parse_document(html);
    let sel = Selector::parse("link").ok()?;

    for el in doc.select(&sel) {
        let Some(rel) = el.value().attr("rel") else {
            continue;
        };
        let rel = rel.to_ascii_lowercase();
        let rel_tokens: Vec<&str> = rel.split_whitespace().collect();
        if !rel_tokens.contains(&"alternate") {
            continue;
        }

        let Some(link_type) = el.value().attr("type") else {
            continue;
        };
        let link_type = link_type.to_ascii_lowercase();
        let is_ap = link_type == "application/activity+json"
            || (link_type == "application/ld+json" && el.value().attr("href").is_some())
            || link_type.contains("application/activity+json")
            || link_type.contains("profile=\"https://www.w3.org/ns/activitystreams\"")
            || link_type.contains("profile='https://www.w3.org/ns/activitystreams'");

        if is_ap {
            return el.value().attr("href").map(|s| s.to_owned());
        }
    }

    None
}

pub fn find_canonical_url(html: &str, base_url: &url::Url) -> Option<String> {
    let doc = Html::parse_document(html);
    let sel = Selector::parse("link[rel=\"canonical\"]").ok()?;
    let el = doc.select(&sel).next()?;
    let href = el.value().attr("href")?;
    if href.is_empty() {
        return None;
    }
    base_url.join(href).ok().map(|u| u.to_string())
}

pub fn find_apple_touch_icon(html: &str, base_url: &url::Url) -> Option<String> {
    let doc = Html::parse_document(html);
    for selector in [
        r#"link[rel="apple-touch-icon"][sizes="180x180"]"#,
        r#"link[rel="apple-touch-icon"]"#,
    ] {
        let sel = Selector::parse(selector).ok()?;
        if let Some(el) = doc.select(&sel).next()
            && let Some(href) = el.value().attr("href")
            && !href.is_empty()
            && let Ok(resolved) = base_url.join(href)
        {
            return Some(resolved.to_string());
        }
    }
    None
}

fn extract_image_urls(meta: &MetaTags) -> Vec<String> {
    let properties = [
        "og:image",
        "og:image:secure_url",
        "twitter:image",
        "twitter:image:src",
        "image",
    ];
    let mut seen = std::collections::HashSet::new();
    let mut values = Vec::new();

    for prop in properties {
        for val in meta.all(prop) {
            let Some(normalized) = normalize_image_reference_key(val) else {
                continue;
            };
            if seen.insert(normalized) {
                values.push(val.trim().to_owned());
            }
        }
    }
    values
}

fn normalize_image_reference_key(value: &str) -> Option<String> {
    let value = value.trim();
    if value.is_empty()
        || value
            .chars()
            .any(|c| c.is_ascii_whitespace() || c.is_control())
    {
        return None;
    }

    if let Ok(parsed) = url::Url::parse(value) {
        return is_http_url(&parsed).then(|| parsed.as_str().trim_end_matches('/').to_owned());
    }

    let base = url::Url::parse("https://example.invalid/").ok()?;
    let resolved = base.join(value).ok()?;
    is_http_url(&resolved).then(|| format!("relative:{}", value.trim_end_matches('/')))
}

fn is_http_url(url: &url::Url) -> bool {
    matches!(url.scheme(), "http" | "https")
}

#[cfg(test)]
mod tests {
    use super::*;

    fn og(html: &str) -> OgMetadata {
        parse_opengraph(html)
    }

    #[test]
    fn images_reject_bad_url_references() {
        let h = r#"<head>
            <meta property="og:image" content="javascript:alert(1)">
            <meta property="og:image" content="data:image/png;base64,abcd">
            <meta property="og:image" content="bad url.png">
            <meta property="og:image" content="https://a.com/ok.png">
        </head>"#;
        assert_eq!(og(h).images, vec!["https://a.com/ok.png".to_owned()]);
    }
}
