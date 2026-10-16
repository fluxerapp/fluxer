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
