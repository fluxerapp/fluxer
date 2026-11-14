// SPDX-License-Identifier: AGPL-3.0-or-later

use ammonia::{Builder, UrlRelative};
use std::borrow::Cow;
use std::collections::HashSet;

const TRUSTED_HOSTS: &[&str] = &[
    "youtube.com",
    "youtu.be",
    "youtube-nocookie.com",
    "vimeo.com",
    "soundcloud.com",
    "spotify.com",
    "twitter.com",
    "x.com",
    "reddit.com",
    "tiktok.com",
    "codepen.io",
];

pub fn sanitize_oembed_html_with_context(
    html: &str,
    source_url: &url::Url,
    provider_url: Option<&str>,
) -> Option<String> {
    let source_trusted = source_url.host_str().is_some_and(is_trusted_oembed_host);

    let provider_trusted = provider_url
        .and_then(|u| url::Url::parse(u).ok())
        .and_then(|u| u.host_str().map(is_trusted_oembed_host))
        .unwrap_or(false);

    if !source_trusted && !provider_trusted {
        return None;
    }

    sanitize_oembed_html(html)
}

pub fn sanitize_oembed_html(html: &str) -> Option<String> {
    let trimmed = html.trim();
    if trimmed.is_empty() {
        return None;
    }

    let mut allowed_tags: HashSet<&str> = HashSet::new();
    allowed_tags.extend([
        "blockquote",
        "div",
        "span",
        "p",
        "strong",
        "em",
        "b",
        "i",
        "br",
        "a",
        "iframe",
    ]);

    let mut tag_attributes: std::collections::HashMap<&str, HashSet<&str>> =
        std::collections::HashMap::new();

    let mut iframe_attrs: HashSet<&str> = HashSet::new();
    iframe_attrs.extend([
        "src",
        "width",
        "height",
        "allow",
        "allowfullscreen",
        "frameborder",
        "title",
        "loading",
        "referrerpolicy",
        "sandbox",
    ]);
    tag_attributes.insert("iframe", iframe_attrs);

    let mut a_attrs: HashSet<&str> = HashSet::new();
    a_attrs.extend(["href", "target"]);
    tag_attributes.insert("a", a_attrs);

    let mut bq_attrs: HashSet<&str> = HashSet::new();
    bq_attrs.insert("cite");
    tag_attributes.insert("blockquote", bq_attrs);

    let sanitized = Builder::new()
        .tags(allowed_tags)
        .tag_attributes(tag_attributes)
        .url_schemes(HashSet::from(["http", "https"]))
        .url_relative(UrlRelative::Deny)
        .attribute_filter(|element, attribute, value| {
            if element == "iframe" && attribute == "src" && !is_trusted_oembed_src(value) {
                return None;
            }
            Some(Cow::Borrowed(value))
        })
        .link_rel(Some("noopener noreferrer"))
        .clean(trimmed)
        .to_string();

    let result = sanitized.trim().to_owned();
    if result.is_empty() {
        return None;
    }

    Some(result)
}

pub fn is_trusted_oembed_host(hostname: &str) -> bool {
    let lower = hostname.to_ascii_lowercase();
    TRUSTED_HOSTS
        .iter()
        .any(|&trusted| lower == trusted || lower.ends_with(&format!(".{trusted}")))
}

fn is_trusted_oembed_src(src: &str) -> bool {
    url::Url::parse(src).ok().is_some_and(|url| {
        matches!(url.scheme(), "http" | "https")
            && url.host_str().is_some_and(is_trusted_oembed_host)
    })
}
