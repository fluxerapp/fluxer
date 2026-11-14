// SPDX-License-Identifier: AGPL-3.0-or-later

use std::borrow::Cow;
use std::cell::Cell;
use std::rc::Rc;

use html5ever::driver::{self, ParseOpts, Parser};
use html5ever::tendril::{StrTendril, TendrilSink};
use html5ever::tree_builder::{ElementFlags, NodeOrText, QuirksMode, TreeSink};
use html5ever::{Attribute, QualName, local_name, ns};
use scraper::{Html, HtmlTreeSink, Selector};

const MAX_NESTING_DEPTH: usize = 512;

const PARSE_CHUNK_BYTES: usize = 4096;

const TEXTUAL_KEYS: [&str; 5] = [
    "og:title",
    "og:description",
    "og:site_name",
    "twitter:title",
    "twitter:description",
];

const CLASSIFYING_CARDS: [&str; 3] = ["summary_large_image", "photo", "player"];

const INERT_ELEMENTS: [&str; 4] = ["script", "style", "noscript", "template"];

type Handle = <HtmlTreeSink as TreeSink>::Handle;

struct DepthLimitedSink {
    inner: HtmlTreeSink,
    too_deep: Rc<Cell<bool>>,
}

impl DepthLimitedSink {
    fn note_depth_below(&self, node: &Handle) {
        if self.too_deep.get() {
            return;
        }
        let html = self.inner.0.borrow();
        let depth = html
            .tree
            .get(*node)
            .map_or(0, |node| node.ancestors().take(MAX_NESTING_DEPTH).count());
        if depth >= MAX_NESTING_DEPTH {
            self.too_deep.set(true);
        }
    }
}

impl TreeSink for DepthLimitedSink {
    type Output = Html;
    type Handle = Handle;
    type ElemName<'a> = <HtmlTreeSink as TreeSink>::ElemName<'a>;

    fn finish(self) -> Html {
        self.inner.finish()
    }

    fn parse_error(&self, msg: Cow<'static, str>) {
        self.inner.parse_error(msg);
    }

    fn get_document(&self) -> Handle {
        self.inner.get_document()
    }

    fn elem_name<'a>(&'a self, target: &'a Handle) -> Self::ElemName<'a> {
        self.inner.elem_name(target)
    }

    fn create_element(&self, name: QualName, attrs: Vec<Attribute>, flags: ElementFlags) -> Handle {
        self.inner.create_element(name, attrs, flags)
    }

    fn create_comment(&self, text: StrTendril) -> Handle {
        self.inner.create_comment(text)
    }

    fn create_pi(&self, target: StrTendril, data: StrTendril) -> Handle {
        self.inner.create_pi(target, data)
    }

    fn append(&self, parent: &Handle, child: NodeOrText<Handle>) {
        if matches!(child, NodeOrText::AppendNode(_)) {
            self.note_depth_below(parent);
        }
        self.inner.append(parent, child);
    }

    fn append_based_on_parent_node(
        &self,
        element: &Handle,
        prev_element: &Handle,
        child: NodeOrText<Handle>,
    ) {
        if matches!(child, NodeOrText::AppendNode(_)) {
            self.note_depth_below(element);
            self.note_depth_below(prev_element);
        }
        self.inner
            .append_based_on_parent_node(element, prev_element, child);
    }

    fn append_doctype_to_document(
        &self,
        name: StrTendril,
        public_id: StrTendril,
        system_id: StrTendril,
    ) {
        self.inner
            .append_doctype_to_document(name, public_id, system_id);
    }

    fn mark_script_already_started(&self, node: &Handle) {
        self.inner.mark_script_already_started(node);
    }

    fn pop(&self, node: &Handle) {
        self.inner.pop(node);
    }

    fn get_template_contents(&self, target: &Handle) -> Handle {
        self.inner.get_template_contents(target)
    }

    fn same_node(&self, x: &Handle, y: &Handle) -> bool {
        self.inner.same_node(x, y)
    }

    fn set_quirks_mode(&self, mode: QuirksMode) {
        self.inner.set_quirks_mode(mode);
    }

    fn append_before_sibling(&self, sibling: &Handle, new_node: NodeOrText<Handle>) {
        if matches!(new_node, NodeOrText::AppendNode(_)) {
            self.note_depth_below(sibling);
        }
        self.inner.append_before_sibling(sibling, new_node);
    }

    fn add_attrs_if_missing(&self, target: &Handle, attrs: Vec<Attribute>) {
        self.inner.add_attrs_if_missing(target, attrs);
    }

    fn remove_from_parent(&self, target: &Handle) {
        self.inner.remove_from_parent(target);
    }

    fn reparent_children(&self, node: &Handle, new_parent: &Handle) {
        self.note_depth_below(new_parent);
        self.inner.reparent_children(node, new_parent);
    }
}

fn parse_with_depth_limit(
    input: &str,
    document: Html,
    start: impl FnOnce(DepthLimitedSink) -> Parser<DepthLimitedSink>,
) -> Html {
    let too_deep = Rc::new(Cell::new(false));
    let mut parser = start(DepthLimitedSink {
        inner: HtmlTreeSink::new(document),
        too_deep: Rc::clone(&too_deep),
    });
    let mut rest = input;
    while !rest.is_empty() && !too_deep.get() {
        let (chunk, tail) = rest.split_at(rest.floor_char_boundary(PARSE_CHUNK_BYTES));
        parser.process(StrTendril::from_slice(chunk));
        rest = tail;
    }
    parser.finish()
}

pub(crate) fn with_document<T>(html: &str, read: impl FnOnce(&Html) -> T) -> T {
    off_worker(|| {
        read(&parse_with_depth_limit(
            html,
            Html::new_document(),
            |sink| driver::parse_document(sink, ParseOpts::default()),
        ))
    })
}

fn off_worker<T>(work: impl FnOnce() -> T) -> T {
    match tokio::runtime::Handle::try_current() {
        Ok(handle) if handle.runtime_flavor() == tokio::runtime::RuntimeFlavor::MultiThread => {
            tokio::task::block_in_place(work)
        }
        _ => work(),
    }
}

pub(crate) fn parse_fragment(fragment: &str) -> Html {
    parse_with_depth_limit(fragment, Html::new_fragment(), |sink| {
        driver::parse_fragment(
            sink,
            ParseOpts::default(),
            QualName::new(None, ns!(html), local_name!("body")),
            Vec::new(),
            false,
        )
    })
}

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

#[derive(Debug, Default, Clone)]
pub struct PageMetadata {
    pub og: OgMetadata,
    pub twitter_card: TwitterCardMetadata,
    pub rsd_url: Option<String>,
    pub activity_pub_link: Option<String>,
    pub has_fediverse_profile_username: bool,
    pub canonical_url: Option<String>,
    pub apple_touch_icon: Option<String>,
    pub oembed_endpoints: Vec<crate::oembed::OEmbedEndpoint>,
}

pub fn parse_page(html: &str, base_url: &url::Url) -> PageMetadata {
    with_document(html, |doc| {
        let meta = MetaTags::parse(doc);
        PageMetadata {
            og: opengraph(doc, &meta),
            twitter_card: twitter_card(&meta),
            rsd_url: rsd_url(doc),
            activity_pub_link: activity_pub_link(doc),
            has_fediverse_profile_username: has_fediverse_profile_username(&meta),
            canonical_url: canonical_url(doc, base_url),
            apple_touch_icon: apple_touch_icon(doc, base_url),
            oembed_endpoints: crate::oembed::discover_oembed_endpoints(doc),
        }
    })
}

pub(crate) fn opengraph_of(doc: &Html) -> OgMetadata {
    opengraph(doc, &MetaTags::parse(doc))
}

fn opengraph(doc: &Html, meta: &MetaTags) -> OgMetadata {
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
        images: extract_image_urls(meta),
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
            .or_else(|| document_title(doc))
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

fn twitter_card(meta: &MetaTags) -> TwitterCardMetadata {
    TwitterCardMetadata {
        classifying_card: meta
            .all("twitter:card")
            .find(|value| CLASSIFYING_CARDS.contains(value))
            .map(ToOwned::to_owned),
        player: meta.first("twitter:player"),
    }
}

fn rsd_url(doc: &Html) -> Option<String> {
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

fn activity_pub_link(doc: &Html) -> Option<String> {
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

fn has_fediverse_profile_username(meta: &MetaTags) -> bool {
    meta.all("og:profile:username")
        .chain(meta.all("profile:username"))
        .any(is_acct_shaped)
}

fn is_acct_shaped(value: &str) -> bool {
    let value = value.trim();
    let value = value.strip_prefix('@').unwrap_or(value);
    let Some((local, host)) = value.split_once('@') else {
        return false;
    };
    !local.is_empty()
        && local
            .chars()
            .all(|c| c.is_alphanumeric() || matches!(c, '_' | '.' | '-'))
        && host.contains('.')
        && host.split('.').all(|label| {
            !label.is_empty() && label.chars().all(|c| c.is_alphanumeric() || c == '-')
        })
}

fn canonical_url(doc: &Html, base_url: &url::Url) -> Option<String> {
    let sel = Selector::parse("link[rel=\"canonical\"]").ok()?;
    let el = doc.select(&sel).next()?;
    let href = el.value().attr("href")?;
    if href.is_empty() {
        return None;
    }
    base_url.join(href).ok().map(|u| u.to_string())
}

fn apple_touch_icon(doc: &Html, base_url: &url::Url) -> Option<String> {
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

    #[test]
    fn deeply_nested_page_keeps_head_metadata() {
        let mut html =
            String::from(r#"<html><head><meta property="og:title" content="Kept"></head><body>"#);
        html.push_str(&"<div>".repeat(200_000));
        let base = url::Url::parse("https://example.com/").unwrap();
        assert_eq!(parse_page(&html, &base).og.title.as_deref(), Some("Kept"));
    }

    #[test]
    fn parse_page_reads_every_field_from_one_parse() {
        let html = r#"<html><head>
            <meta property="og:title" content="Post">
            <meta name="twitter:card" content="player">
            <meta name="twitter:player" content="https://player.example/embed">
            <meta property="profile:username" content="@alice@social.example">
            <link rel="EditURI" type="application/rsd+xml" href="/w/api.php?action=rsd">
            <link rel="alternate" type="application/activity+json" href="/notes/1">
            <link rel="canonical" href="/canonical">
            <link rel="apple-touch-icon" href="/icon.png">
            <link rel="alternate" type="application/json+oembed" href="/oembed?format=json">
        </head></html>"#;
        let base = url::Url::parse("https://social.example/post/1").unwrap();
        let page = parse_page(html, &base);
        assert_eq!(page.og.title.as_deref(), Some("Post"));
        assert_eq!(
            page.twitter_card.classifying_card.as_deref(),
            Some("player")
        );
        assert_eq!(
            page.twitter_card.player.as_deref(),
            Some("https://player.example/embed")
        );
        assert!(page.has_fediverse_profile_username);
        assert_eq!(page.rsd_url.as_deref(), Some("/w/api.php?action=rsd"));
        assert_eq!(page.activity_pub_link.as_deref(), Some("/notes/1"));
        assert_eq!(
            page.canonical_url.as_deref(),
            Some("https://social.example/canonical")
        );
        assert_eq!(
            page.apple_touch_icon.as_deref(),
            Some("https://social.example/icon.png")
        );
        assert_eq!(page.oembed_endpoints.len(), 1);
        assert_eq!(page.oembed_endpoints[0].url, "/oembed?format=json");
    }
}
