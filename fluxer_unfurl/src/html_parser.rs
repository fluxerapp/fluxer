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
#[allow(dead_code)]
pub struct OgMetadata {
    pub title: Option<String>,
    pub description: Option<String>,
    pub url: Option<String>,
    pub image: Option<String>,
    pub images: Vec<String>,
    pub image_alt: Option<String>,
    pub image_width: Option<u32>,
    pub image_height: Option<u32>,
    pub video_primary: Option<String>,
    pub audio: Option<String>,
    pub site_name: Option<String>,
    pub og_type: Option<String>,
    pub theme_color: Option<String>,
    pub textual_keys_present: bool,
}

#[derive(Debug, Default, Clone)]
#[allow(dead_code)]
pub struct TwitterCardMetadata {
    pub card: Option<String>,
    pub classifying_card: Option<String>,
    pub title: Option<String>,
    pub description: Option<String>,
    pub image: Option<String>,
    pub image_alt: Option<String>,
    pub player: Option<String>,
    pub player_width: Option<u32>,
    pub player_height: Option<u32>,
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
        url: meta.first("og:url"),
        image,
        images: extract_image_urls(&meta),
        image_alt: meta
            .first("og:image:alt")
            .or_else(|| meta.first("twitter:image:alt"))
            .or_else(|| meta.first("og:image:description")),
        image_width: meta.first("og:image:width").and_then(|v| v.parse().ok()),
        image_height: meta.first("og:image:height").and_then(|v| v.parse().ok()),
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

#[allow(dead_code)]
pub fn parse_twitter_card(html: &str) -> TwitterCardMetadata {
    let doc = Html::parse_document(html);
    let meta = MetaTags::parse(&doc);

    TwitterCardMetadata {
        card: meta.first("twitter:card"),
        classifying_card: meta
            .all("twitter:card")
            .find(|value| CLASSIFYING_CARDS.contains(value))
            .map(ToOwned::to_owned),
        title: meta.first("twitter:title"),
        description: meta.first("twitter:description"),
        image: meta
            .first("twitter:image")
            .or_else(|| meta.first("twitter:image:src")),
        image_alt: meta.first("twitter:image:alt"),
        player: meta.first("twitter:player"),
        player_width: meta
            .first("twitter:player:width")
            .and_then(|v| v.parse().ok()),
        player_height: meta
            .first("twitter:player:height")
            .and_then(|v| v.parse().ok()),
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

#[allow(dead_code)]
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
    fn extracts_og_title_desc_image_url() {
        let h = r#"<html><head>
            <meta property="og:title" content="T">
            <meta property="og:description" content="D">
            <meta property="og:image" content="https://i.example.com/a.png">
            <meta property="og:url" content="https://example.com/p">
        </head></html>"#;
        let m = og(h);
        assert_eq!(m.title.as_deref(), Some("T"));
        assert_eq!(m.description.as_deref(), Some("D"));
        assert_eq!(m.image.as_deref(), Some("https://i.example.com/a.png"));
        assert_eq!(m.url.as_deref(), Some("https://example.com/p"));
    }

    #[test]
    fn handles_missing_tags() {
        let m = og("<html><head><title>X</title></head></html>");
        assert_eq!(m.title.as_deref(), Some("X"));
        assert!(m.description.is_none() && m.image.is_none() && m.url.is_none());
    }

    #[test]
    fn extracts_multiple_images() {
        let h = r#"<head><meta property="og:image" content="https://a.com/1.png">
            <meta property="og:image" content="https://a.com/2.png"></head>"#;
        assert!(og(h).images.len() >= 2);
    }

    #[test]
    fn falls_back_to_meta_description() {
        let m = og(r#"<head><meta name="description" content="MD"></head>"#);
        assert_eq!(m.description.as_deref(), Some("MD"));
    }

    #[test]
    fn meta_extraction_matches_twitter_aliases_for_og_fields() {
        let m = og(r#"<head><meta name="twitter:description" content="TD"></head>"#);
        assert_eq!(m.description.as_deref(), Some("TD"));
    }

    #[test]
    fn extracts_site_name_from_application_name() {
        let m = og(r#"<head><meta name="application-name" content="App"></head>"#);
        assert_eq!(m.site_name.as_deref(), Some("App"));
    }

    #[test]
    fn find_ap_link() {
        let h = r#"<head><link rel="alternate" type="application/activity+json" href="https://e.com/ap"></head>"#;
        assert_eq!(find_activity_pub_link(h), Some("https://e.com/ap".into()));
        assert!(find_activity_pub_link("<head></head>").is_none());
    }

    #[test]
    fn find_canonical() {
        let h = r#"<head><link rel="canonical" href="/p"></head>"#;
        let base = url::Url::parse("https://e.com/old").unwrap();
        assert_eq!(find_canonical_url(h, &base), Some("https://e.com/p".into()));
        assert!(find_canonical_url("<head></head>", &base).is_none());
    }

    #[test]
    fn title_fallback_chain_matches_ts() {
        let m = og(r#"<head><meta property="og:title" content="OG"></head>"#);
        assert_eq!(m.title.as_deref(), Some("OG"));
        let m = og(r#"<head><meta name="twitter:title" content="TW"></head>"#);
        assert_eq!(m.title.as_deref(), Some("TW"));
        let m = og(r#"<html><head><title>HTML Title</title></head></html>"#);
        assert_eq!(m.title.as_deref(), Some("HTML Title"));
        let m = og(r#"<head><meta name="title" content="Meta"></head>"#);
        assert_eq!(m.title.as_deref(), Some("Meta"));
    }

    #[test]
    fn title_prefers_open_graph_over_the_document_title() {
        let m = og(
            r#"<html><head><meta property="og:title" content="OG"><title>HTML Title</title></head></html>"#,
        );
        assert_eq!(m.title.as_deref(), Some("OG"));
        let m = og(
            r#"<html><head><meta name="twitter:title" content="TW"><title>HTML Title</title></head></html>"#,
        );
        assert_eq!(m.title.as_deref(), Some("TW"));
    }

    #[test]
    fn document_title_is_trimmed_and_blank_is_ignored() {
        let m = og(r#"<html><head><title>  Hello World  </title></head></html>"#);
        assert_eq!(m.title.as_deref(), Some("Hello World"));
        let m = og(r#"<html><head><title>   </title></head></html>"#);
        assert!(m.title.is_none());
    }

    #[test]
    fn description_fallback_chain_matches_ts() {
        let m = og(r#"<head><meta property="og:description" content="OD"></head>"#);
        assert_eq!(m.description.as_deref(), Some("OD"));
        let m = og(r#"<head><meta name="description" content="MD"></head>"#);
        assert_eq!(m.description.as_deref(), Some("MD"));
        let m = og(r#"<head><meta name="twitter:description" content="TD"></head>"#);
        assert_eq!(m.description.as_deref(), Some("TD"));
    }

    #[test]
    fn site_name_fallback_chain_matches_ts() {
        let m = og(r#"<head><meta property="og:site_name" content="OG Site"></head>"#);
        assert_eq!(m.site_name.as_deref(), Some("OG Site"));
        let m = og(r#"<head><meta name="twitter:site:name" content="TW Site"></head>"#);
        assert_eq!(m.site_name.as_deref(), Some("TW Site"));
        let m = og(r#"<head><meta name="application-name" content="App"></head>"#);
        assert_eq!(m.site_name.as_deref(), Some("App"));
    }

    #[test]
    fn video_primary_only_accepts_og_video_and_og_video_url() {
        let m = og(r#"<head><meta property="og:video" content="https://v.com/a.mp4"></head>"#);
        assert_eq!(m.video_primary.as_deref(), Some("https://v.com/a.mp4"));
        let m = og(r#"<head><meta property="og:video:url" content="https://v.com/b.mp4"></head>"#);
        assert_eq!(m.video_primary.as_deref(), Some("https://v.com/b.mp4"));
        let m = og(
            r#"<head><meta property="og:video:secure_url" content="https://v.com/c.mp4"></head>"#,
        );
        assert!(m.video_primary.is_none());
        let m = og(r#"<head><meta name="twitter:player" content="https://p.com/embed"></head>"#);
        assert!(m.video_primary.is_none());
    }

    #[test]
    fn audio_url_fallback_matches_ts() {
        let m = og(r#"<head><meta property="og:audio" content="https://a.com/song.mp3"></head>"#);
        assert_eq!(m.audio.as_deref(), Some("https://a.com/song.mp3"));
        let m =
            og(r#"<head><meta property="og:audio:url" content="https://a.com/song2.mp3"></head>"#);
        assert_eq!(m.audio.as_deref(), Some("https://a.com/song2.mp3"));
    }

    #[test]
    fn image_url_fallback_matches_ts() {
        let m = og(r#"<head><meta property="og:image" content="https://i.com/a.png"></head>"#);
        assert_eq!(m.image.as_deref(), Some("https://i.com/a.png"));
        let m = og(
            r#"<head><meta property="og:image:secure_url" content="https://i.com/b.png"></head>"#,
        );
        assert_eq!(m.image.as_deref(), Some("https://i.com/b.png"));
    }

    #[test]
    fn extracts_image_dimensions() {
        let m = og(r#"<head>
            <meta property="og:image:width" content="1200">
            <meta property="og:image:height" content="630">
        </head>"#);
        assert_eq!(m.image_width, Some(1200));
        assert_eq!(m.image_height, Some(630));
    }

    #[test]
    fn extracts_theme_color() {
        let m = og(r##"<head><meta name="theme-color" content="#FF0000"></head>"##);
        assert_eq!(m.theme_color.as_deref(), Some("#FF0000"));
    }

    #[test]
    fn extracts_og_type() {
        let m = og(r#"<head><meta property="og:type" content="article"></head>"#);
        assert_eq!(m.og_type.as_deref(), Some("article"));
    }

    #[test]
    fn images_deduplicated_by_normalized_url() {
        let h = r#"<head>
            <meta property="og:image" content="https://a.com/1.png">
            <meta property="og:image" content="https://a.com/1.png/">
        </head>"#;
        assert_eq!(og(h).images.len(), 1);
    }

    #[test]
    fn images_collected_from_multiple_sources() {
        let h = r#"<head>
            <meta property="og:image" content="https://a.com/1.png">
            <meta name="twitter:image" content="https://a.com/2.png">
            <meta name="twitter:image:src" content="https://a.com/3.png">
        </head>"#;
        assert_eq!(og(h).images.len(), 3);
    }

    #[test]
    fn images_keep_relative_url_references() {
        let h = r#"<head>
            <meta property="og:image" content="/api/og">
            <meta property="og:image" content="hero.png">
            <meta name="twitter:image" content="//cdn.example.com/card.png">
        </head>"#;
        assert_eq!(
            og(h).images,
            vec![
                "/api/og".to_owned(),
                "hero.png".to_owned(),
                "//cdn.example.com/card.png".to_owned()
            ]
        );
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

    #[test]
    fn twitter_card_parsing() {
        let h = r#"<head>
            <meta name="twitter:card" content="summary_large_image">
            <meta name="twitter:title" content="Title">
            <meta name="twitter:description" content="Desc">
            <meta name="twitter:image" content="https://i.com/a.png">
            <meta name="twitter:player" content="https://p.com/embed">
            <meta name="twitter:player:width" content="640">
            <meta name="twitter:player:height" content="360">
        </head>"#;
        let tc = parse_twitter_card(h);
        assert_eq!(tc.card.as_deref(), Some("summary_large_image"));
        assert_eq!(tc.title.as_deref(), Some("Title"));
        assert_eq!(tc.description.as_deref(), Some("Desc"));
        assert_eq!(tc.image.as_deref(), Some("https://i.com/a.png"));
        assert_eq!(tc.player.as_deref(), Some("https://p.com/embed"));
        assert_eq!(tc.player_width, Some(640));
        assert_eq!(tc.player_height, Some(360));
    }

    #[test]
    fn twitter_card_image_falls_back_to_src() {
        let h = r#"<head><meta name="twitter:image:src" content="https://i.com/a.png"></head>"#;
        let tc = parse_twitter_card(h);
        assert_eq!(tc.image.as_deref(), Some("https://i.com/a.png"));
    }

    #[test]
    fn activity_pub_link_ld_json_variant() {
        let h = r#"<head><link rel="alternate" type="application/ld+json" href="https://e.com/ld"></head>"#;
        assert_eq!(find_activity_pub_link(h), Some("https://e.com/ld".into()));
    }

    #[test]
    fn activity_pub_link_profile_annotation() {
        let h = r#"<head><link rel="alternate" type="application/ld+json; profile='https://www.w3.org/ns/activitystreams'" href="https://e.com/ap"></head>"#;
        assert_eq!(find_activity_pub_link(h), Some("https://e.com/ap".into()));
    }

    #[test]
    fn activity_pub_link_requires_alternate_rel() {
        let h = r#"<head><link rel="preload" type="application/activity+json" href="https://e.com/ap"></head>"#;
        assert!(find_activity_pub_link(h).is_none());
    }

    #[test]
    fn find_apple_touch_icon_prefers_180x180() {
        let h = r#"<head>
            <link rel="apple-touch-icon" href="/icon-default.png">
            <link rel="apple-touch-icon" sizes="180x180" href="/icon-180.png">
        </head>"#;
        let base = url::Url::parse("https://e.com/page").unwrap();
        assert_eq!(
            find_apple_touch_icon(h, &base),
            Some("https://e.com/icon-180.png".into())
        );
    }

    #[test]
    fn find_apple_touch_icon_resolves_relative_urls() {
        let h = r#"<head><link rel="apple-touch-icon" href="/icons/touch.png"></head>"#;
        let base = url::Url::parse("https://e.com/page").unwrap();
        assert_eq!(
            find_apple_touch_icon(h, &base),
            Some("https://e.com/icons/touch.png".into())
        );
    }

    #[test]
    fn canonical_url_resolves_relative() {
        let h = r#"<head><link rel="canonical" href="../other"></head>"#;
        let base = url::Url::parse("https://e.com/a/b/c").unwrap();
        assert_eq!(
            find_canonical_url(h, &base),
            Some("https://e.com/a/other".into())
        );
    }

    #[test]
    fn canonical_url_empty_href_returns_none() {
        let h = r#"<head><link rel="canonical" href=""></head>"#;
        let base = url::Url::parse("https://e.com/page").unwrap();
        assert!(find_canonical_url(h, &base).is_none());
    }

    #[test]
    fn empty_meta_content_is_skipped() {
        let m = og(r#"<head><meta property="og:title" content=""></head>"#);
        assert!(m.title.is_none());
    }

    #[test]
    fn first_meta_occurrence_wins() {
        let h = r#"<head>
            <meta property="og:title" content="First">
            <meta property="og:title" content="Second">
        </head>"#;
        assert_eq!(og(h).title.as_deref(), Some("First"));
    }

    #[test]
    fn property_outranks_name_on_the_same_element() {
        let h = r#"<head><meta property="og:title" name="og:description" content="P"></head>"#;
        let m = og(h);
        assert_eq!(m.title.as_deref(), Some("P"));
        assert!(m.description.is_none());
    }

    #[test]
    fn value_attribute_is_used_when_content_is_absent() {
        let m = og(r#"<head><meta property="og:title" value="V"></head>"#);
        assert_eq!(m.title.as_deref(), Some("V"));
    }

    #[test]
    fn content_attribute_wins_over_value_attribute() {
        let m = og(r#"<head><meta property="og:title" content="C" value="V"></head>"#);
        assert_eq!(m.title.as_deref(), Some("C"));
    }

    #[test]
    fn meta_keys_are_case_sensitive() {
        let m = og(r#"<head><meta property="OG:TITLE" content="T"></head>"#);
        assert!(m.title.is_none());
        let m = og(r#"<head><meta PROPERTY="og:title" CONTENT="T"></head>"#);
        assert_eq!(m.title.as_deref(), Some("T"));
    }

    #[test]
    fn meta_values_are_not_trimmed() {
        let m = og(r#"<head><meta property="og:title" content=" T "></head>"#);
        assert_eq!(m.title.as_deref(), Some(" T "));
    }

    #[test]
    fn og_keys_do_not_alias_to_twitter_keys() {
        let m = og(r#"<head><meta name="twitter:url" content="https://e.com/p"></head>"#);
        assert!(m.url.is_none());
    }

    #[test]
    fn metas_inside_inert_elements_are_skipped() {
        for tag in ["noscript", "template"] {
            let h = format!(
                r#"<head><{tag}><meta property="og:title" content="Hidden"></{tag}></head>"#
            );
            assert!(og(&h).title.is_none(), "{tag} content must be skipped");
        }
    }

    #[test]
    fn textual_keys_present_requires_a_discord_textual_key() {
        for key in [
            "og:title",
            "og:description",
            "og:site_name",
            "twitter:title",
            "twitter:description",
        ] {
            let h = format!(r#"<head><meta property="{key}" content="X"></head>"#);
            assert!(og(&h).textual_keys_present, "{key} must satisfy the gate");
        }
        for key in [
            "og:image",
            "og:url",
            "twitter:image",
            "twitter:card",
            "twitter:site",
            "description",
        ] {
            let h = format!(r#"<head><meta property="{key}" content="X"></head>"#);
            assert!(
                !og(&h).textual_keys_present,
                "{key} must not satisfy the gate"
            );
        }
    }

    #[test]
    fn classifying_card_skips_non_assigning_values() {
        let h = r#"<head>
            <meta name="twitter:card" content="summary">
            <meta name="twitter:card" content="photo">
        </head>"#;
        let tc = parse_twitter_card(h);
        assert_eq!(tc.card.as_deref(), Some("summary"));
        assert_eq!(tc.classifying_card.as_deref(), Some("photo"));
    }

    #[test]
    fn classifying_card_is_case_sensitive_and_untrimmed() {
        for value in ["SUMMARY_LARGE_IMAGE", "Summary_Large_Image", "photo "] {
            let h = format!(r#"<head><meta name="twitter:card" content="{value}"></head>"#);
            assert!(
                parse_twitter_card(&h).classifying_card.is_none(),
                "{value} must not classify"
            );
        }
    }

    #[test]
    fn classifying_card_ignores_unknown_values() {
        let h = r#"<head><meta name="twitter:card" content="summary"></head>"#;
        assert!(parse_twitter_card(h).classifying_card.is_none());
    }

    #[test]
    fn activity_pub_link_scan_continues_past_unrelated_links() {
        let h = r#"<head>
            <link rel="stylesheet" href="/a.css">
            <link rel="alternate" type="application/activity+json" href="https://e.com/ap">
        </head>"#;
        assert_eq!(find_activity_pub_link(h), Some("https://e.com/ap".into()));
    }

    #[test]
    fn finds_rsd_edit_uri() {
        let h = r#"<head><link rel="EditURI" type="application/rsd+xml" href="//w.example/w/api.php?action=rsd"></head>"#;
        assert_eq!(
            find_rsd_url(h),
            Some("//w.example/w/api.php?action=rsd".into())
        );
    }

    #[test]
    fn rsd_requires_edit_uri_rel_and_rsd_type() {
        let h = r#"<head><link rel="alternate" type="application/rsd+xml" href="/rsd"></head>"#;
        assert!(find_rsd_url(h).is_none());
        let h = r#"<head><link rel="EditURI" type="application/xml" href="/rsd"></head>"#;
        assert!(find_rsd_url(h).is_none());
        assert!(find_rsd_url("<head></head>").is_none());
    }
}
