// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::http_fetch;
use scraper::{Html, Selector};
use serde::Deserialize;
use std::time::Duration;

#[derive(Debug, Clone)]
pub struct OEmbedEndpoint {
    pub url: String,
    pub format: OEmbedFormat,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OEmbedFormat {
    Json,
    Xml,
}

#[derive(Debug, Clone, Deserialize, Default)]
pub struct OEmbedResponse {
    #[serde(rename = "type")]
    pub oembed_type: Option<String>,
    pub title: Option<String>,
    pub provider_name: Option<String>,
    pub provider_url: Option<String>,
    pub author_name: Option<String>,
    pub author_url: Option<String>,
    pub thumbnail_url: Option<String>,
    pub thumbnail_width: Option<serde_json::Value>,
    pub thumbnail_height: Option<serde_json::Value>,
    pub html: Option<String>,
    pub width: Option<serde_json::Value>,
    pub height: Option<serde_json::Value>,
    pub url: Option<String>,
}

pub fn discover_oembed_url(html: &str) -> Vec<OEmbedEndpoint> {
    let doc = Html::parse_document(html);
    let sel = match Selector::parse("link") {
        Ok(s) => s,
        Err(_) => return Vec::new(),
    };

    let mut endpoints = Vec::new();

    for el in doc.select(&sel) {
        let href = match el.value().attr("href") {
            Some(h) if !h.is_empty() => h,
            _ => continue,
        };
        let link_type = match el.value().attr("type") {
            Some(t) => t.to_ascii_lowercase(),
            None => continue,
        };

        if !link_type.contains("+oembed") {
            continue;
        }

        let href_lower = href.to_ascii_lowercase();
        let format = if link_type.contains("xml")
            || href_lower.contains("format=xml")
            || href_lower.ends_with(".xml")
        {
            OEmbedFormat::Xml
        } else {
            OEmbedFormat::Json
        };

        endpoints.push(OEmbedEndpoint {
            url: href.to_owned(),
            format,
        });
    }

    endpoints
}

pub async fn fetch_oembed(
    client: &reqwest::Client,
    url: &str,
    format: OEmbedFormat,
) -> anyhow::Result<OEmbedResponse> {
    let result = http_fetch::fetch_url(client, url, 256 * 1024, Duration::from_secs(5)).await?;

    if result.status != 200 {
        anyhow::bail!("oEmbed endpoint returned status {}", result.status);
    }

    match format {
        OEmbedFormat::Json => {
            let response: OEmbedResponse = serde_json::from_slice(&result.bytes)?;
            Ok(response)
        }
        OEmbedFormat::Xml => {
            let text = crate::charset::decode_body(
                &result.bytes,
                result
                    .headers
                    .get(reqwest::header::CONTENT_TYPE)
                    .and_then(|value| value.to_str().ok()),
            );
            parse_oembed_xml(&text)
        }
    }
}

fn parse_oembed_xml(xml: &str) -> anyhow::Result<OEmbedResponse> {
    fn extract_xml_field(xml: &str, tag: &str) -> Option<String> {
        let open = format!("<{tag}>");
        let close = format!("</{tag}>");
        let start = xml.find(&open)? + open.len();
        let end = xml[start..].find(&close)? + start;
        let val = xml[start..end].trim();
        if val.is_empty() {
            None
        } else {
            Some(val.to_owned())
        }
    }

    fn to_json_value(s: Option<String>) -> Option<serde_json::Value> {
        s.map(serde_json::Value::String)
    }

    Ok(OEmbedResponse {
        oembed_type: extract_xml_field(xml, "type"),
        title: extract_xml_field(xml, "title"),
        provider_name: extract_xml_field(xml, "provider_name"),
        provider_url: extract_xml_field(xml, "provider_url"),
        author_name: extract_xml_field(xml, "author_name"),
        author_url: extract_xml_field(xml, "author_url"),
        thumbnail_url: extract_xml_field(xml, "thumbnail_url"),
        thumbnail_width: to_json_value(extract_xml_field(xml, "thumbnail_width")),
        thumbnail_height: to_json_value(extract_xml_field(xml, "thumbnail_height")),
        html: extract_xml_field(xml, "html"),
        width: to_json_value(extract_xml_field(xml, "width")),
        height: to_json_value(extract_xml_field(xml, "height")),
        url: extract_xml_field(xml, "url"),
    })
}

pub fn parse_dimension(value: &serde_json::Value) -> Option<u32> {
    match value {
        serde_json::Value::Number(n) => n.as_u64().filter(|&v| v > 0).map(|v| v.min(4096) as u32),
        serde_json::Value::String(s) => {
            let trimmed = s.trim();
            let digits: String = trimmed.chars().take_while(|c| c.is_ascii_digit()).collect();
            digits
                .parse::<u32>()
                .ok()
                .filter(|&v| v > 0)
                .map(|v| v.min(4096))
        }
        _ => None,
    }
}

pub fn known_oembed_endpoint(hostname: &str, source_url: &str) -> Option<OEmbedEndpoint> {
    let host = hostname.to_ascii_lowercase();
    let host = host.strip_suffix('.').unwrap_or(&host);

    let (base, extra_params) =
        if host_matches(host, &["youtube.com", "youtu.be", "youtube-nocookie.com"]) {
            ("https://www.youtube.com/oembed", "&format=json")
        } else if host_matches(host, &["vimeo.com"]) {
            ("https://vimeo.com/api/oembed.json", "")
        } else if host_matches(host, &["soundcloud.com"]) {
            ("https://soundcloud.com/oembed", "&format=json")
        } else if host_matches(host, &["spotify.com"]) {
            ("https://open.spotify.com/oembed", "")
        } else if host_matches(host, &["twitter.com", "x.com"]) {
            (
                "https://publish.twitter.com/oembed",
                "&omit_script=true&dnt=true",
            )
        } else if host_matches(host, &["tiktok.com"]) {
            ("https://www.tiktok.com/oembed", "")
        } else if host_matches(host, &["reddit.com"]) {
            ("https://www.reddit.com/oembed", "")
        } else if host_matches(host, &["codepen.io"]) {
            ("https://codepen.io/api/oembed", "&format=json")
        } else {
            return None;
        };

    let encoded = urlencoding::encode(source_url);
    let url = format!("{base}?url={encoded}{extra_params}");

    Some(OEmbedEndpoint {
        url,
        format: OEmbedFormat::Json,
    })
}

fn host_matches(hostname: &str, allowed: &[&str]) -> bool {
    allowed
        .iter()
        .any(|&a| hostname == a || hostname.ends_with(&format!(".{a}")))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn discover_json_endpoint() {
        let html = r#"<html><head>
            <link rel="alternate" type="application/json+oembed" href="https://e.com/oembed?url=x">
        </head></html>"#;
        let eps = discover_oembed_url(html);
        assert_eq!(eps.len(), 1);
        assert_eq!(eps[0].format, OEmbedFormat::Json);
        assert_eq!(eps[0].url, "https://e.com/oembed?url=x");
    }

    #[test]
    fn discover_xml_endpoint() {
        let html = r#"<head><link rel="alternate" type="text/xml+oembed" href="https://e.com/o.xml"></head>"#;
        let eps = discover_oembed_url(html);
        assert_eq!(eps.len(), 1);
        assert_eq!(eps[0].format, OEmbedFormat::Xml);
    }

    #[test]
    fn discover_no_oembed() {
        assert!(discover_oembed_url("<head></head>").is_empty());
    }

    #[test]
    fn parse_oembed_json_photo() {
        let j = r#"{"type":"photo","title":"Sunset","url":"https://e.com/img.jpg","width":800,"height":600}"#;
        let r: OEmbedResponse = serde_json::from_str(j).unwrap();
        assert_eq!(r.oembed_type.as_deref(), Some("photo"));
        assert_eq!(r.title.as_deref(), Some("Sunset"));
    }

    #[test]
    fn parse_oembed_json_video() {
        let j = r#"{"type":"video","title":"Clip","html":"<iframe></iframe>","width":1280,"height":720}"#;
        let r: OEmbedResponse = serde_json::from_str(j).unwrap();
        assert_eq!(r.oembed_type.as_deref(), Some("video"));
        assert!(r.html.is_some());
    }

    #[test]
    fn parse_oembed_json_rich_and_link() {
        let r: OEmbedResponse =
            serde_json::from_str(r#"{"type":"rich","provider_name":"P"}"#).unwrap();
        assert_eq!(r.oembed_type.as_deref(), Some("rich"));
        let l: OEmbedResponse = serde_json::from_str(r#"{"type":"link","title":"P"}"#).unwrap();
        assert_eq!(l.oembed_type.as_deref(), Some("link"));
    }

    #[test]
    fn parse_oembed_xml_basic() {
        let r = parse_oembed_xml("<oembed><type>photo</type><title>S</title></oembed>").unwrap();
        assert_eq!(r.oembed_type.as_deref(), Some("photo"));
    }

    #[test]
    fn parse_dimension_values() {
        assert_eq!(parse_dimension(&serde_json::json!(480)), Some(480));
        assert_eq!(parse_dimension(&serde_json::json!("720px")), Some(720));
        assert_eq!(parse_dimension(&serde_json::json!(0)), None);
        assert_eq!(parse_dimension(&serde_json::json!(5000)), Some(4096));
        assert_eq!(parse_dimension(&serde_json::json!("5000px")), Some(4096));
    }

    #[test]
    fn known_oembed_youtube() {
        let ep = known_oembed_endpoint("youtube.com", "https://youtube.com/watch?v=abc");
        assert!(ep.is_some());
        assert!(ep.unwrap().url.contains("youtube.com/oembed"));
    }

    #[test]
    fn known_oembed_unknown_host() {
        assert!(known_oembed_endpoint("example.com", "https://example.com").is_none());
    }

    #[test]
    fn known_oembed_all_providers_match_ts() {
        let cases = [
            ("youtube.com", "youtube.com/oembed"),
            ("www.youtube.com", "youtube.com/oembed"),
            ("youtu.be", "youtube.com/oembed"),
            ("youtube-nocookie.com", "youtube.com/oembed"),
            ("vimeo.com", "vimeo.com/api/oembed.json"),
            ("soundcloud.com", "soundcloud.com/oembed"),
            ("spotify.com", "open.spotify.com/oembed"),
            ("open.spotify.com", "open.spotify.com/oembed"),
            ("twitter.com", "publish.twitter.com/oembed"),
            ("x.com", "publish.twitter.com/oembed"),
            ("tiktok.com", "tiktok.com/oembed"),
            ("www.tiktok.com", "tiktok.com/oembed"),
            ("reddit.com", "reddit.com/oembed"),
            ("www.reddit.com", "reddit.com/oembed"),
            ("codepen.io", "codepen.io/api/oembed"),
        ];
        for (host, expected_substr) in cases {
            let ep = known_oembed_endpoint(host, &format!("https://{host}/page"));
            assert!(ep.is_some(), "expected known endpoint for {host}");
            assert!(
                ep.as_ref().unwrap().url.contains(expected_substr),
                "endpoint for {host} should contain {expected_substr}, got {}",
                ep.unwrap().url
            );
        }
    }

    #[test]
    fn known_oembed_twitter_params_match_ts() {
        let ep =
            known_oembed_endpoint("twitter.com", "https://twitter.com/user/status/123").unwrap();
        assert!(ep.url.contains("omit_script=true"));
        assert!(ep.url.contains("dnt=true"));
    }

    #[test]
    fn known_oembed_youtube_format_param() {
        let ep = known_oembed_endpoint("youtube.com", "https://youtube.com/watch?v=abc").unwrap();
        assert!(ep.url.contains("format=json"));
    }

    #[test]
    fn known_oembed_subdomain_matching() {
        assert!(
            known_oembed_endpoint("music.youtube.com", "https://music.youtube.com/watch?v=abc")
                .is_some()
        );
        assert!(
            known_oembed_endpoint("m.soundcloud.com", "https://m.soundcloud.com/track").is_some()
        );
        assert!(known_oembed_endpoint("old.reddit.com", "https://old.reddit.com/r/test").is_some());
    }

    #[test]
    fn known_oembed_url_encodes_source() {
        let ep =
            known_oembed_endpoint("youtube.com", "https://youtube.com/watch?v=abc&t=10").unwrap();
        assert!(ep.url.contains("url="));
        assert!(!ep.url.contains("url=https://youtube.com/watch?v=abc&t=10"));
    }

    #[test]
    fn discover_both_json_and_xml() {
        let html = r#"<head>
            <link rel="alternate" type="application/json+oembed" href="https://e.com/j">
            <link rel="alternate" type="text/xml+oembed" href="https://e.com/x">
        </head>"#;
        let eps = discover_oembed_url(html);
        assert_eq!(eps.len(), 2);
        assert_eq!(eps[0].format, OEmbedFormat::Json);
        assert_eq!(eps[1].format, OEmbedFormat::Xml);
    }

    #[test]
    fn discover_infers_xml_from_format_param() {
        let html = r#"<head>
            <link rel="alternate" type="application/json+oembed" href="https://e.com/oembed?format=xml">
        </head>"#;
        let eps = discover_oembed_url(html);
        assert_eq!(eps[0].format, OEmbedFormat::Xml);
    }

    #[test]
    fn discover_skips_link_without_oembed_type() {
        let html = r#"<head>
            <link rel="alternate" type="application/json" href="https://e.com/api">
        </head>"#;
        assert!(discover_oembed_url(html).is_empty());
    }

    #[test]
    fn discover_skips_empty_href() {
        let html = r#"<head>
            <link rel="alternate" type="application/json+oembed" href="">
        </head>"#;
        assert!(discover_oembed_url(html).is_empty());
    }

    #[test]
    fn parse_dimension_integer() {
        assert_eq!(parse_dimension(&serde_json::json!(1280)), Some(1280));
    }

    #[test]
    fn parse_dimension_string_with_px_suffix() {
        assert_eq!(parse_dimension(&serde_json::json!("480px")), Some(480));
    }

    #[test]
    fn parse_dimension_string_plain() {
        assert_eq!(parse_dimension(&serde_json::json!("720")), Some(720));
    }

    #[test]
    fn parse_dimension_zero_is_none() {
        assert_eq!(parse_dimension(&serde_json::json!(0)), None);
        assert_eq!(parse_dimension(&serde_json::json!("0")), None);
    }

    #[test]
    fn parse_dimension_clamps_at_4096() {
        assert_eq!(parse_dimension(&serde_json::json!(9999)), Some(4096));
        assert_eq!(parse_dimension(&serde_json::json!("10000")), Some(4096));
    }

    #[test]
    fn parse_dimension_negative_returns_none() {
        assert_eq!(parse_dimension(&serde_json::json!(-1)), None);
    }

    #[test]
    fn parse_dimension_non_numeric_string_returns_none() {
        assert_eq!(parse_dimension(&serde_json::json!("abc")), None);
    }

    #[test]
    fn parse_dimension_null_returns_none() {
        assert_eq!(parse_dimension(&serde_json::json!(null)), None);
    }

    #[test]
    fn parse_dimension_bool_returns_none() {
        assert_eq!(parse_dimension(&serde_json::json!(true)), None);
    }

    #[test]
    fn parse_oembed_xml_with_nested_tags() {
        let xml = r#"<oembed>
            <type>video</type>
            <title>My Video</title>
            <html><![CDATA[<iframe></iframe>]]></html>
            <width>640</width>
            <height>360</height>
            <provider_name>Provider</provider_name>
            <provider_url>https://provider.com</provider_url>
        </oembed>"#;
        let r = parse_oembed_xml(xml).unwrap();
        assert_eq!(r.oembed_type.as_deref(), Some("video"));
        assert_eq!(r.title.as_deref(), Some("My Video"));
        assert_eq!(r.provider_name.as_deref(), Some("Provider"));
    }

    #[test]
    fn parse_oembed_xml_empty_field_returns_none() {
        let xml = "<oembed><title></title><type>link</type></oembed>";
        let r = parse_oembed_xml(xml).unwrap();
        assert!(r.title.is_none());
        assert_eq!(r.oembed_type.as_deref(), Some("link"));
    }

    #[test]
    fn parse_oembed_json_with_author() {
        let j = r#"{
            "type": "video",
            "author_name": "Author",
            "author_url": "https://author.com",
            "provider_name": "Provider",
            "provider_url": "https://provider.com",
            "thumbnail_url": "https://e.com/thumb.jpg",
            "thumbnail_width": 120,
            "thumbnail_height": 90
        }"#;
        let r: OEmbedResponse = serde_json::from_str(j).unwrap();
        assert_eq!(r.author_name.as_deref(), Some("Author"));
        assert_eq!(r.author_url.as_deref(), Some("https://author.com"));
        assert_eq!(r.thumbnail_url.as_deref(), Some("https://e.com/thumb.jpg"));
        assert_eq!(
            parse_dimension(r.thumbnail_width.as_ref().unwrap()),
            Some(120)
        );
    }

    #[test]
    fn parse_oembed_json_missing_all_optional() {
        let r: OEmbedResponse = serde_json::from_str("{}").unwrap();
        assert!(r.oembed_type.is_none());
        assert!(r.title.is_none());
        assert!(r.html.is_none());
    }

    #[test]
    fn parse_oembed_json_dimension_as_string() {
        let j = r#"{"type": "video", "width": "640px", "height": "360"}"#;
        let r: OEmbedResponse = serde_json::from_str(j).unwrap();
        assert_eq!(parse_dimension(r.width.as_ref().unwrap()), Some(640));
        assert_eq!(parse_dimension(r.height.as_ref().unwrap()), Some(360));
    }
}
