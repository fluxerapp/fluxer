// SPDX-License-Identifier: AGPL-3.0-or-later

use super::{ResolveContext, Resolver, ResolverResult};
use crate::http_fetch;
use crate::media_proxy::{MediaProxyClient, embed_media_flags};
use crate::network_policy;
use crate::text_limits;
use crate::types::{EmbedAuthor, EmbedField, EmbedFooter, EmbedMedia, MessageEmbed};
use serde::Deserialize;
use serde_json::Value;
use std::collections::HashSet;
use std::future::Future;
use std::pin::Pin;
use std::sync::OnceLock;
use std::time::Duration;
use url::Url;

const BLUESKY_COLOR: u32 = 0x1185FE;
const BLUESKY_ICON: &str = "https://bsky.app/static/apple-touch-icon.png";
const BLUESKY_API_BASE: &str = "https://api.bsky.app/xrpc";
const MAX_GALLERY_IMAGES: usize = 10;
const SERVICE_ENDPOINT_CACHE_TTL: Duration = Duration::from_secs(60 * 60);

pub struct BlueskyResolver;

impl Resolver for BlueskyResolver {
    fn matches(&self, url: &Url) -> bool {
        url.host_str()
            .is_some_and(|h| h.eq_ignore_ascii_case("bsky.app"))
    }

    fn resolve<'a>(
        &'a self,
        ctx: &'a ResolveContext<'_>,
    ) -> Pin<Box<dyn Future<Output = anyhow::Result<ResolverResult>> + Send + 'a>> {
        Box::pin(async move {
            let parts = parse_path_parts(&ctx.url);

            if is_post_url(&parts) {
                return resolve_post(ctx, &parts).await;
            }

            if is_profile_url(&parts) {
                return resolve_profile(ctx, &parts).await;
            }

            Ok(ResolverResult { embeds: vec![] })
        })
    }
}

fn parse_path_parts(url: &Url) -> Vec<String> {
    url.path()
        .trim_matches('/')
        .split('/')
        .map(|s| s.to_owned())
        .collect()
}

fn is_profile_url(parts: &[String]) -> bool {
    parts.len() == 2 && parts[0] == "profile"
}

fn is_post_url(parts: &[String]) -> bool {
    parts.len() == 4 && parts[0] == "profile" && parts[2] == "post" && !parts[3].is_empty()
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct BskyProfile {
    handle: Option<String>,
    display_name: Option<String>,
    description: Option<String>,
}

async fn resolve_profile(
    ctx: &ResolveContext<'_>,
    parts: &[String],
) -> anyhow::Result<ResolverResult> {
    let handle = &parts[1];
    let api_url = format!(
        "{BLUESKY_API_BASE}/app.bsky.actor.getProfile?actor={}",
        urlencoding::encode(handle)
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

    let profile: BskyProfile = serde_json::from_slice(&result.bytes)?;

    let handle_str = profile.handle.as_deref().unwrap_or(handle);
    let title = match profile.display_name.as_deref().filter(|s| !s.is_empty()) {
        Some(display) => format!("{display} (@{handle_str})"),
        None => format!("@{handle_str}"),
    };

    let mut embed = MessageEmbed::new("rich");
    embed.url = Some(ctx.url.to_string());
    embed.title = Some(text_limits::truncate(&title, text_limits::TITLE_MAX));
    embed.description = profile.description.clone();
    embed.color = Some(BLUESKY_COLOR);
    embed.footer = Some(EmbedFooter {
        text: "Bluesky".to_owned(),
        icon_url: Some(BLUESKY_ICON.to_owned()),
        ..Default::default()
    });

    Ok(ResolverResult {
        embeds: vec![embed],
    })
}

#[derive(Debug, Deserialize)]
struct BskyThreadResponse {
    thread: Option<BskyThread>,
}

#[derive(Debug, Deserialize)]
struct BskyThread {
    post: Option<BskyPost>,
    parent: Option<BskyThreadParent>,
}

#[derive(Debug, Deserialize)]
struct BskyThreadParent {
    post: Option<BskyPost>,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct BskyPost {
    uri: Option<String>,
    author: Option<BskyPostAuthor>,
    record: Option<BskyRecord>,
    embed: Option<Value>,
    like_count: Option<u64>,
    repost_count: Option<u64>,
    quote_count: Option<u64>,
    bookmark_count: Option<u64>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
struct BskyPostAuthor {
    did: Option<String>,
    handle: Option<String>,
    display_name: Option<String>,
    avatar: Option<String>,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct BskyRecord {
    text: Option<String>,
    created_at: Option<String>,
    facets: Option<Vec<BskyFacet>>,
    reply: Option<Value>,
}

#[derive(Debug, Clone, Deserialize)]
struct BskyFacet {
    index: BskyFacetIndex,
    #[serde(default)]
    features: Vec<BskyFacetFeature>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
struct BskyFacetIndex {
    byte_start: usize,
    byte_end: usize,
}

#[derive(Debug, Clone, Deserialize)]
struct BskyFacetFeature {
    #[serde(rename = "$type")]
    facet_type: Option<String>,
    uri: Option<String>,
    did: Option<String>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
struct BskyAspectRatio {
    width: Option<u32>,
    height: Option<u32>,
}

#[derive(Debug, Clone)]
struct ImageEntry {
    url: String,
    aspect_ratio: Option<BskyAspectRatio>,
    alt: Option<String>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
struct BskyImageView {
    thumb: Option<String>,
    fullsize: Option<String>,
    alt: Option<String>,
    aspect_ratio: Option<BskyAspectRatio>,
}

#[derive(Debug, Deserialize)]
struct BskyImagesView {
    images: Option<Vec<BskyImageView>>,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct BskyVideoView {
    cid: Option<String>,
    thumbnail: Option<String>,
    aspect_ratio: Option<BskyAspectRatio>,
}

#[derive(Debug, Clone, Deserialize)]
struct BskyExternalView {
    external: Option<BskyExternal>,
}

#[derive(Debug, Clone, Deserialize)]
struct BskyExternal {
    uri: Option<String>,
    title: Option<String>,
    description: Option<String>,
    thumb: Option<String>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
struct BskyEmbeddedRecord {
    uri: Option<String>,
    author: Option<BskyPostAuthor>,
    value: Option<BskyEmbeddedRecordValue>,
    embeds: Option<Vec<Value>>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
struct BskyEmbeddedRecordValue {
    text: Option<String>,
    created_at: Option<String>,
    facets: Option<Vec<BskyFacet>>,
}

#[derive(Debug, Clone)]
struct ProcessedExternal {
    uri: String,
    title: String,
    description: Option<String>,
}

#[derive(Debug, Default, Clone)]
struct ProcessedPostEmbed {
    image: Option<EmbedMedia>,
    thumbnail: Option<EmbedMedia>,
    video: Option<EmbedMedia>,
    gallery_images: Vec<EmbedMedia>,
    external: Option<ProcessedExternal>,
}

#[derive(Debug, Clone)]
struct ProcessedEmbeddedPost {
    uri: String,
    author: BskyPostAuthor,
    text: String,
    created_at: String,
    facets: Option<Vec<BskyFacet>>,
    embed: Option<ProcessedPostEmbed>,
}

async fn resolve_post(
    ctx: &ResolveContext<'_>,
    parts: &[String],
) -> anyhow::Result<ResolverResult> {
    let handle = &parts[1];
    let post_id = &parts[3];

    let did = resolve_did(&ctx.http_client, handle).await;
    let did = match did {
        Some(d) => d,
        None => {
            return Ok(ResolverResult { embeds: vec![] });
        }
    };

    let at_uri = format!("at://{did}/app.bsky.feed.post/{post_id}");
    let api_url = format!(
        "{BLUESKY_API_BASE}/app.bsky.feed.getPostThread?uri={}&depth=0",
        urlencoding::encode(&at_uri)
    );

    let result = http_fetch::fetch_url(
        &ctx.http_client,
        &api_url,
        512 * 1024,
        Duration::from_secs(5),
    )
    .await?;

    if result.status != 200 {
        return Ok(ResolverResult { embeds: vec![] });
    }

    let thread_response: BskyThreadResponse = serde_json::from_slice(&result.bytes)?;
    let mut thread = match thread_response.thread {
        Some(thread) => thread,
        None => {
            return Ok(ResolverResult { embeds: vec![] });
        }
    };
    let post = match thread.post.take() {
        Some(p) => p,
        None => {
            return Ok(ResolverResult { embeds: vec![] });
        }
    };

    let processed_embed = process_post_embed(ctx, &post).await;
    let processed_embedded_post = process_embedded_post(ctx, &post).await;
    let post_description = format_post_content(&post, &thread);
    let child_embed = processed_embedded_post.map(build_embedded_post_embed);

    let root_embed = build_bluesky_embed(BlueskyEmbedBuildInput {
        post_url: ctx.url.to_string(),
        post_description,
        created_at: post
            .record
            .as_ref()
            .and_then(|record| record.created_at.clone()),
        author: post.author.as_ref(),
        fallback_handle: handle,
        processed_embed: Some(&processed_embed),
        fields: build_engagement_fields(&post),
        children: child_embed.into_iter().collect(),
        is_nested: false,
    });

    let mut embeds = vec![root_embed];
    for gallery_image in processed_embed.gallery_images {
        let mut embed = MessageEmbed::new("rich");
        embed.url = Some(ctx.url.to_string());
        embed.image = Some(gallery_image);
        embeds.push(embed);
    }

    Ok(ResolverResult { embeds })
}

struct BlueskyEmbedBuildInput<'a> {
    post_url: String,
    post_description: String,
    created_at: Option<String>,
    author: Option<&'a BskyPostAuthor>,
    fallback_handle: &'a str,
    processed_embed: Option<&'a ProcessedPostEmbed>,
    fields: Vec<EmbedField>,
    children: Vec<MessageEmbed>,
    is_nested: bool,
}

fn build_bluesky_embed(input: BlueskyEmbedBuildInput<'_>) -> MessageEmbed {
    let description = match input
        .processed_embed
        .and_then(|processed_embed| processed_embed.external.as_ref())
        .map(build_external_summary)
    {
        Some(external_summary) => append_section(&input.post_description, &external_summary),
        None => input.post_description,
    };

    let author_handle = input
        .author
        .and_then(|a| a.handle.as_deref())
        .unwrap_or(input.fallback_handle);
    let author_display = input
        .author
        .and_then(|a| a.display_name.as_deref())
        .filter(|display| !display.is_empty())
        .unwrap_or(author_handle);

    let mut embed = MessageEmbed::new("bluesky");
    embed.url = Some(input.post_url.clone());
    embed.description = Some(text_limits::clamp_lines(
        &text_limits::truncate(&description, text_limits::DESCRIPTION_MAX),
        text_limits::DESCRIPTION_LINES_MAX,
    ));
    embed.color = Some(BLUESKY_COLOR);
    embed.author = Some(EmbedAuthor {
        name: text_limits::truncate(
            &format!("{author_display} (@{author_handle})"),
            text_limits::AUTHOR_NAME_MAX,
        ),
        url: Some(input.post_url.clone()),
        icon_url: input
            .author
            .and_then(|a| sanitize_optional_absolute_url(a.avatar.as_deref())),
        ..Default::default()
    });

    if let Some(processed_embed) = input.processed_embed {
        if let Some(image) = processed_embed.image.clone() {
            embed.image = Some(image);
        }
        if let Some(video) = processed_embed.video.clone() {
            embed.thumbnail = processed_embed.thumbnail.clone();
            embed.video = Some(video);
        } else if embed.image.is_none() {
            embed.thumbnail = processed_embed.thumbnail.clone();
        }
    }

    if !input.children.is_empty() {
        embed.children = Some(input.children);
    }

    if input.is_nested {
        return embed;
    }

    if let Some(external) = input
        .processed_embed
        .and_then(|processed_embed| processed_embed.external.as_ref())
    {
        embed.title = Some(text_limits::truncate(
            &external.title,
            text_limits::TITLE_MAX,
        ));
    }
    embed.timestamp = input.created_at.as_deref().map(normalize_timestamp);
    if !input.fields.is_empty() {
        embed.fields = Some(input.fields);
    }
    embed.footer = Some(EmbedFooter {
        text: "Bluesky".to_owned(),
        icon_url: Some(BLUESKY_ICON.to_owned()),
        ..Default::default()
    });

    embed
}

fn build_embedded_post_embed(processed: ProcessedEmbeddedPost) -> MessageEmbed {
    let fallback_handle = processed
        .author
        .handle
        .as_deref()
        .unwrap_or("bsky.app")
        .to_owned();
    let post_url = at_uri_to_post_url(&processed.uri, &fallback_handle);
    let formatted_text = embed_links_in_text(&processed.text, processed.facets.as_deref());

    build_bluesky_embed(BlueskyEmbedBuildInput {
        post_url,
        post_description: formatted_text,
        created_at: Some(processed.created_at),
        author: Some(&processed.author),
        fallback_handle: &fallback_handle,
        processed_embed: processed.embed.as_ref(),
        fields: vec![],
        children: vec![],
        is_nested: true,
    })
}

fn build_engagement_fields(post: &BskyPost) -> Vec<EmbedField> {
    let pairs = [
        ("repostCount", post.repost_count),
        ("quoteCount", post.quote_count),
        ("likeCount", post.like_count),
        ("bookmarkCount", post.bookmark_count),
    ];
    pairs
        .iter()
        .filter_map(|(name, count)| {
            count.filter(|&c| c > 0).map(|c| EmbedField {
                name: (*name).to_owned(),
                value: format_count(c),
                inline: true,
            })
        })
        .collect()
}

async fn process_post_embed(ctx: &ResolveContext<'_>, post: &BskyPost) -> ProcessedPostEmbed {
    let Some(embed) = post.embed.as_ref() else {
        return ProcessedPostEmbed::default();
    };
    let author_did = post
        .author
        .as_ref()
        .and_then(|author| author.did.as_deref())
        .unwrap_or_default();
    process_embed(ctx, embed, author_did).await
}

async fn process_embedded_post(
    ctx: &ResolveContext<'_>,
    post: &BskyPost,
) -> Option<ProcessedEmbeddedPost> {
    let embedded_record = extract_embedded_record(post.embed.as_ref()?)?;
    let nested_embed_view = extract_embedded_record_media(&embedded_record);
    let author_did = embedded_record
        .author
        .as_ref()
        .and_then(|author| author.did.as_deref())
        .unwrap_or_default();
    let embed = match nested_embed_view {
        Some(embed) => Some(process_embed(ctx, embed, author_did).await),
        None => None,
    };
    let value = embedded_record.value?;
    Some(ProcessedEmbeddedPost {
        uri: embedded_record.uri?,
        author: embedded_record.author?,
        text: value.text.unwrap_or_default(),
        created_at: value.created_at.unwrap_or_default(),
        facets: value.facets,
        embed,
    })
}

async fn process_embed(
    ctx: &ResolveContext<'_>,
    embed: &Value,
    author_did: &str,
) -> ProcessedPostEmbed {
    let mut processed = ProcessedPostEmbed::default();
    let processed_images = process_embed_images(ctx, embed).await;
    processed.image = processed_images.first().cloned();
    processed.gallery_images = processed_images.into_iter().skip(1).collect();

    match embed_type(embed) {
        Some("app.bsky.embed.video#view") => {
            let (thumbnail, video) = process_video_embed(ctx, embed, author_did).await;
            processed.thumbnail = thumbnail;
            processed.video = video;
        }
        Some("app.bsky.embed.external#view") => {
            let (external, thumbnail) = process_external_embed(ctx, embed).await;
            processed.external = external;
            processed.thumbnail = thumbnail;
        }
        Some("app.bsky.embed.recordWithMedia#view") => {
            if let Some(media) = embed.get("media") {
                match embed_type(media) {
                    Some("app.bsky.embed.video#view") => {
                        let (thumbnail, video) = process_video_embed(ctx, media, author_did).await;
                        processed.thumbnail = thumbnail;
                        processed.video = video;
                    }
                    Some("app.bsky.embed.external#view") => {
                        let (external, thumbnail) = process_external_embed(ctx, media).await;
                        processed.external = external;
                        if processed.thumbnail.is_none() {
                            processed.thumbnail = thumbnail;
                        }
                    }
                    _ => {}
                }
            }
        }
        _ => {}
    }

    processed
}

async fn process_embed_images(ctx: &ResolveContext<'_>, embed: &Value) -> Vec<EmbedMedia> {
    let image_entries = collect_image_entries(embed);
    let nsfw_mode = MediaProxyClient::nsfw_mode_str(ctx.nsfw_mode);
    let mut processed = Vec::new();
    let mut seen_urls = HashSet::new();

    for entry in image_entries {
        let Some(normalized_url) = normalize_url(&entry.url) else {
            continue;
        };
        if !seen_urls.insert(normalized_url) {
            continue;
        }
        if let Some(image) = process_image(
            ctx,
            &entry.url,
            entry.aspect_ratio.as_ref(),
            entry.alt.as_deref(),
            nsfw_mode,
        )
        .await
        {
            processed.push(image);
            if processed.len() >= MAX_GALLERY_IMAGES {
                break;
            }
        }
    }

    processed
}

fn collect_image_entries(embed: &Value) -> Vec<ImageEntry> {
    let mut entries = Vec::new();
    if embed_type(embed) == Some("app.bsky.embed.images#view") {
        add_image_entries(embed, &mut entries);
    }
    if embed_type(embed) == Some("app.bsky.embed.recordWithMedia#view")
        && embed_type(&embed["media"]) == Some("app.bsky.embed.images#view")
    {
        add_image_entries(&embed["media"], &mut entries);
    }
    entries
}

fn add_image_entries(embed: &Value, entries: &mut Vec<ImageEntry>) {
    let Ok(images_view) = serde_json::from_value::<BskyImagesView>(embed.clone()) else {
        return;
    };
    let Some(images) = images_view.images else {
        return;
    };
    for image in images {
        let Some(url) = image.fullsize.or(image.thumb) else {
            continue;
        };
        entries.push(ImageEntry {
            url,
            aspect_ratio: image.aspect_ratio,
            alt: image.alt,
        });
    }
}

async fn process_image(
    ctx: &ResolveContext<'_>,
    image_url: &str,
    aspect_ratio: Option<&BskyAspectRatio>,
    alt_text: Option<&str>,
    nsfw_mode: &str,
) -> Option<EmbedMedia> {
    let meta = ctx
        .media_proxy
        .get_metadata(image_url, nsfw_mode)
        .await
        .ok()?;
    Some(EmbedMedia {
        url: Some(image_url.to_owned()),
        content_type: Some(meta.content_type.clone()),
        content_hash: Some(meta.content_hash.clone()),
        width: aspect_ratio
            .and_then(|aspect_ratio| aspect_ratio.width)
            .or(meta.width),
        height: aspect_ratio
            .and_then(|aspect_ratio| aspect_ratio.height)
            .or(meta.height),
        description: alt_text
            .filter(|alt| !alt.is_empty())
            .map(|alt| text_limits::truncate(alt, text_limits::MEDIA_DESCRIPTION_MAX)),
        placeholder: meta.placeholder.clone(),
        duration: meta.duration.map(|duration| duration as u32),
        flags: embed_media_flags(&meta),
        ..Default::default()
    })
}

async fn process_video_embed(
    ctx: &ResolveContext<'_>,
    embed: &Value,
    did: &str,
) -> (Option<EmbedMedia>, Option<EmbedMedia>) {
    if did.is_empty() {
        return (None, None);
    }
    let Ok(video_view) = serde_json::from_value::<BskyVideoView>(embed.clone()) else {
        return (None, None);
    };
    let nsfw_mode = MediaProxyClient::nsfw_mode_str(ctx.nsfw_mode);
    let thumbnail = match video_view.thumbnail.as_deref() {
        Some(thumbnail_url) => {
            process_image(
                ctx,
                thumbnail_url,
                video_view.aspect_ratio.as_ref(),
                None,
                nsfw_mode,
            )
            .await
        }
        None => None,
    };
    let Some(thumbnail) = thumbnail else {
        return (None, None);
    };
    let Some(cid) = video_view.cid else {
        return (None, None);
    };

    let service_endpoint = resolve_service_endpoint(&ctx.http_client, did).await;
    let direct_url =
        format!("{service_endpoint}/xrpc/com.atproto.sync.getBlob?did={did}&cid={cid}");
    let Ok(meta) = ctx.media_proxy.get_metadata(&direct_url, nsfw_mode).await else {
        return (Some(thumbnail), None);
    };

    let video = EmbedMedia {
        url: Some(direct_url),
        content_type: Some(meta.content_type.clone()),
        content_hash: Some(meta.content_hash.clone()),
        width: meta.width.or(thumbnail.width),
        height: meta.height.or(thumbnail.height),
        placeholder: meta.placeholder.clone(),
        duration: meta.duration.map(|duration| duration as u32),
        flags: embed_media_flags(&meta),
        ..Default::default()
    };

    (Some(thumbnail), Some(video))
}

async fn process_external_embed(
    ctx: &ResolveContext<'_>,
    embed: &Value,
) -> (Option<ProcessedExternal>, Option<EmbedMedia>) {
    let Ok(external_view) = serde_json::from_value::<BskyExternalView>(embed.clone()) else {
        return (None, None);
    };
    let Some(external) = external_view.external else {
        return (None, None);
    };
    let Some(uri) = external.uri else {
        return (None, None);
    };
    let nsfw_mode = MediaProxyClient::nsfw_mode_str(ctx.nsfw_mode);
    let thumbnail = match external.thumb.as_deref() {
        Some(thumb) => process_image(ctx, thumb, None, None, nsfw_mode).await,
        None => None,
    };
    let title = external
        .title
        .filter(|title| !title.is_empty())
        .unwrap_or_else(|| uri.clone());
    (
        Some(ProcessedExternal {
            uri,
            title,
            description: external
                .description
                .filter(|description| !description.is_empty()),
        }),
        thumbnail,
    )
}

fn extract_embedded_record(embed: &Value) -> Option<BskyEmbeddedRecord> {
    match embed_type(embed) {
        Some("app.bsky.embed.record#view") => {
            serde_json::from_value::<BskyEmbeddedRecord>(embed.get("record")?.clone()).ok()
        }
        Some("app.bsky.embed.recordWithMedia#view") => {
            serde_json::from_value::<BskyEmbeddedRecord>(
                embed.get("record")?.get("record")?.clone(),
            )
            .ok()
        }
        _ => None,
    }
}

fn extract_embedded_record_media(record: &BskyEmbeddedRecord) -> Option<&Value> {
    record.embeds.as_ref()?.iter().find(|candidate| {
        matches!(
            embed_type(candidate),
            Some("app.bsky.embed.images#view")
                | Some("app.bsky.embed.video#view")
                | Some("app.bsky.embed.external#view")
        )
    })
}

fn embed_type(embed: &Value) -> Option<&str> {
    embed.get("$type").and_then(Value::as_str)
}

fn format_post_content(post: &BskyPost, thread: &BskyThread) -> String {
    let Some(record) = post.record.as_ref() else {
        return String::new();
    };
    let mut processed_text = embed_links_in_text(
        record.text.as_deref().unwrap_or_default(),
        record.facets.as_deref(),
    );
    if record.reply.is_some()
        && let Some(reply_context) = extract_reply_context(thread)
    {
        processed_text = format!(
            "-# \u{21a9} [{} (@{})]({})\n{}",
            reply_context.author_name,
            reply_context.author_handle,
            reply_context.post_url,
            processed_text
        );
    }
    processed_text
}

struct ReplyContext {
    author_name: String,
    author_handle: String,
    post_url: String,
}

fn extract_reply_context(thread: &BskyThread) -> Option<ReplyContext> {
    let parent_post = thread.parent.as_ref()?.post.as_ref()?;
    let parent_author = parent_post.author.as_ref()?;
    let author_handle = parent_author.handle.clone()?;
    let author_name = parent_author
        .display_name
        .clone()
        .filter(|name| !name.is_empty())
        .unwrap_or_else(|| author_handle.clone());
    let post_id = parent_post.uri.as_deref()?.rsplit('/').next()?;
    Some(ReplyContext {
        author_name,
        author_handle: author_handle.clone(),
        post_url: format!("https://bsky.app/profile/{author_handle}/post/{post_id}"),
    })
}

fn embed_links_in_text(text: &str, facets: Option<&[BskyFacet]>) -> String {
    let Some(facets) = facets else {
        return text.to_owned();
    };
    let mut sorted_facets = facets.to_vec();
    sorted_facets.sort_by_key(|facet| facet.index.byte_start);

    let mut result = String::new();
    let mut last_index = 0;
    for facet in sorted_facets {
        let start = facet.index.byte_start;
        let end = facet.index.byte_end;
        if start < last_index
            || start > end
            || end > text.len()
            || !text.is_char_boundary(start)
            || !text.is_char_boundary(end)
        {
            continue;
        }

        result.push_str(&text[last_index..start]);
        let facet_text = &text[start..end];
        let Some(feature) = facet.features.first() else {
            result.push_str(facet_text);
            last_index = end;
            continue;
        };

        match feature.facet_type.as_deref() {
            Some("app.bsky.richtext.facet#link") => {
                if let Some(uri) = feature.uri.as_deref() {
                    result.push_str(&format!("[{}]({})", get_link_display_text(uri), uri));
                } else {
                    result.push_str(facet_text);
                }
            }
            Some("app.bsky.richtext.facet#mention") => {
                if let Some(did) = feature.did.as_deref() {
                    result.push_str(&format!("[{facet_text}](https://bsky.app/profile/{did})"));
                } else {
                    result.push_str(facet_text);
                }
            }
            Some("app.bsky.richtext.facet#tag") => {
                let tag_text = facet_text.strip_prefix('#').unwrap_or(facet_text);
                result.push_str(&format!(
                    "[{facet_text}](https://bsky.app/search?q=%23{})",
                    urlencoding::encode(tag_text)
                ));
            }
            _ => result.push_str(facet_text),
        }
        last_index = end;
    }
    result.push_str(&text[last_index..]);
    result
}

fn get_link_display_text(uri: &str) -> String {
    let Ok(url) = Url::parse(uri) else {
        return uri.to_owned();
    };
    let hostname = url.host_str().unwrap_or_default();
    let trimmed_path = url.path().trim_matches('/');
    if trimmed_path.is_empty() {
        return format!("{hostname}/");
    }
    let path = if trimmed_path.chars().count() > 12 {
        let truncated: String = trimmed_path.chars().take(12).collect();
        format!("{truncated}...")
    } else {
        trimmed_path.to_owned()
    };
    format!("{hostname}/{path}")
}

fn build_external_summary(external: &ProcessedExternal) -> String {
    let title_line = format!("[{}]({})", external.title, external.uri);
    match external.description.as_deref() {
        Some(description) => format!("{title_line}\n{description}"),
        None => title_line,
    }
}

fn append_section(base: &str, section: &str) -> String {
    if base.is_empty() {
        section.to_owned()
    } else {
        format!("{base}\n\n{section}")
    }
}

fn at_uri_to_post_url(at_uri: &str, fallback_handle: &str) -> String {
    let post_id = at_uri.trim_start_matches("at://").split('/').nth(2);
    match post_id {
        Some(post_id) if !post_id.is_empty() => {
            format!("https://bsky.app/profile/{fallback_handle}/post/{post_id}")
        }
        _ => format!("https://bsky.app/profile/{fallback_handle}"),
    }
}

fn normalize_url(url: &str) -> Option<String> {
    Url::parse(url)
        .ok()
        .map(|url| url.to_string().trim_end_matches('/').to_owned())
}

fn sanitize_optional_absolute_url(url: Option<&str>) -> Option<String> {
    Url::parse(url?.trim()).ok().map(|url| url.to_string())
}

#[derive(Debug, Deserialize)]
struct DidResponse {
    did: Option<String>,
}

async fn resolve_did(client: &reqwest::Client, handle: &str) -> Option<String> {
    if handle.starts_with("did:") {
        return Some(handle.to_owned());
    }
    let url = format!(
        "{BLUESKY_API_BASE}/com.atproto.identity.resolveHandle?handle={}",
        urlencoding::encode(handle)
    );
    let r = http_fetch::fetch_url(client, &url, 128 * 1024, Duration::from_secs(5))
        .await
        .ok()?;
    if r.status != 200 {
        return None;
    }
    serde_json::from_slice::<DidResponse>(&r.bytes).ok()?.did
}

#[derive(Debug, Deserialize)]
struct DidDocument {
    service: Option<Vec<DidService>>,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct DidService {
    #[serde(rename = "type")]
    service_type: Option<String>,
    service_endpoint: Option<String>,
}

async fn resolve_service_endpoint(client: &reqwest::Client, did: &str) -> String {
    if let Some(cached) = service_endpoint_cache().get(did).await {
        return cached;
    }
    let did_doc_url = if did.starts_with("did:web:") {
        format!(
            "https://{}/.well-known/did.json",
            did.split(':').nth(2).unwrap_or_default()
        )
    } else {
        format!("https://plc.directory/{did}")
    };
    let fallback = "https://bsky.social".to_owned();
    let Ok(result) =
        http_fetch::fetch_url(client, &did_doc_url, 256 * 1024, Duration::from_secs(5)).await
    else {
        return fallback;
    };
    if result.status != 200 {
        return fallback;
    }
    let Ok(did_doc) = serde_json::from_slice::<DidDocument>(&result.bytes) else {
        return fallback;
    };
    let candidate = did_doc
        .service
        .unwrap_or_default()
        .into_iter()
        .find(|service| service.service_type.as_deref() == Some("AtprotoPersonalDataServer"))
        .and_then(|service| service.service_endpoint);
    let service_endpoint = match candidate {
        Some(endpoint) if is_safe_service_endpoint(&endpoint).await => endpoint,
        _ => fallback,
    };
    service_endpoint_cache()
        .insert(did.to_owned(), service_endpoint.clone())
        .await;
    service_endpoint
}

async fn is_safe_service_endpoint(endpoint: &str) -> bool {
    match network_policy::parse_url(endpoint) {
        Ok(url) => network_policy::validate_url(&url).await.is_ok(),
        Err(_) => false,
    }
}

fn service_endpoint_cache() -> &'static moka::future::Cache<String, String> {
    static CACHE: OnceLock<moka::future::Cache<String, String>> = OnceLock::new();
    CACHE.get_or_init(|| {
        moka::future::Cache::builder()
            .max_capacity(10_000)
            .time_to_live(SERVICE_ENDPOINT_CACHE_TTL)
            .build()
    })
}

fn normalize_timestamp(value: &str) -> String {
    chrono::DateTime::parse_from_rfc3339(value)
        .map(|dt| {
            dt.with_timezone(&chrono::Utc)
                .to_rfc3339_opts(chrono::SecondsFormat::Millis, true)
        })
        .unwrap_or_else(|_| value.to_owned())
}

fn format_count(count: u64) -> String {
    if count < 1000 {
        return count.to_string();
    }
    if count < 10_000 {
        let thousands = count as f64 / 1000.0;
        return format!("{thousands:.1}K");
    }
    let thousands = count / 1000;
    format!("{thousands}K")
}

#[cfg(test)]
mod tests {
    use super::*;
    fn u(s: &str) -> Url {
        Url::parse(s).unwrap()
    }

    #[test]
    fn matches_and_classifies_bsky_urls() {
        assert!(BlueskyResolver.matches(&u("https://bsky.app/profile/a.bsky.social")));
        assert!(BlueskyResolver.matches(&u("https://bsky.app/profile/a.bsky.social/post/x")));
        assert!(!BlueskyResolver.matches(&u("https://example.com/profile/x")));
        let prof = parse_path_parts(&u("https://bsky.app/profile/a.bsky.social"));
        assert!(is_profile_url(&prof) && !is_post_url(&prof));
        let post = parse_path_parts(&u("https://bsky.app/profile/a.bsky.social/post/x"));
        assert!(is_post_url(&post) && !is_profile_url(&post));
    }

    #[test]
    fn formats_counts_like_typescript_resolver() {
        assert_eq!(format_count(999), "999");
        assert_eq!(format_count(1_234), "1.2K");
        assert_eq!(format_count(9_999), "10.0K");
        assert_eq!(format_count(10_000), "10K");
        assert_eq!(format_count(10_999), "10K");
    }

    #[test]
    fn embeds_links_mentions_and_tags_from_byte_facets() {
        let text = "snow \u{2603} @alice read https://example.com/very/long/path #rust";
        let mention_start = text.find("@alice").unwrap();
        let link_start = text.find("https://").unwrap();
        let tag_start = text.find("#rust").unwrap();
        let facets = vec![
            BskyFacet {
                index: BskyFacetIndex {
                    byte_start: mention_start,
                    byte_end: mention_start + "@alice".len(),
                },
                features: vec![BskyFacetFeature {
                    facet_type: Some("app.bsky.richtext.facet#mention".to_owned()),
                    uri: None,
                    did: Some("did:plc:alice".to_owned()),
                }],
            },
            BskyFacet {
                index: BskyFacetIndex {
                    byte_start: link_start,
                    byte_end: link_start + "https://example.com/very/long/path".len(),
                },
                features: vec![BskyFacetFeature {
                    facet_type: Some("app.bsky.richtext.facet#link".to_owned()),
                    uri: Some("https://example.com/very/long/path".to_owned()),
                    did: None,
                }],
            },
            BskyFacet {
                index: BskyFacetIndex {
                    byte_start: tag_start,
                    byte_end: tag_start + "#rust".len(),
                },
                features: vec![BskyFacetFeature {
                    facet_type: Some("app.bsky.richtext.facet#tag".to_owned()),
                    uri: None,
                    did: None,
                }],
            },
        ];

        assert_eq!(
            embed_links_in_text(text, Some(&facets)),
            "snow \u{2603} [@alice](https://bsky.app/profile/did:plc:alice) read [example.com/very/long/pa...](https://example.com/very/long/path) [#rust](https://bsky.app/search?q=%23rust)"
        );
    }

    #[test]
    fn tag_facets_use_display_text_not_feature_tag_for_search_url() {
        let text = "#visible";
        let facets = vec![BskyFacet {
            index: BskyFacetIndex {
                byte_start: 0,
                byte_end: text.len(),
            },
            features: vec![BskyFacetFeature {
                facet_type: Some("app.bsky.richtext.facet#tag".to_owned()),
                uri: None,
                did: None,
            }],
        }];

        assert_eq!(
            embed_links_in_text(text, Some(&facets)),
            "[#visible](https://bsky.app/search?q=%23visible)"
        );
    }

    #[test]
    fn normalizes_bluesky_timestamps_like_typescript() {
        assert_eq!(
            normalize_timestamp("2026-05-27T14:10:51.001+00:00"),
            "2026-05-27T14:10:51.001Z"
        );
    }

    #[test]
    fn collects_record_with_media_images_and_prefers_fullsize() {
        let embed = serde_json::json!({
            "$type": "app.bsky.embed.recordWithMedia#view",
            "media": {
                "$type": "app.bsky.embed.images#view",
                "images": [
                    {
                        "thumb": "https://cdn.example/thumb.jpg",
                        "fullsize": "https://cdn.example/full.jpg",
                        "alt": "alt text",
                        "aspectRatio": {"width": 4, "height": 3}
                    },
                    {
                        "thumb": "https://cdn.example/second.jpg"
                    }
                ]
            }
        });

        let entries = collect_image_entries(&embed);

        assert_eq!(entries.len(), 2);
        assert_eq!(entries[0].url, "https://cdn.example/full.jpg");
        assert_eq!(entries[0].alt.as_deref(), Some("alt text"));
        assert_eq!(entries[0].aspect_ratio.as_ref().unwrap().width, Some(4));
        assert_eq!(entries[1].url, "https://cdn.example/second.jpg");
    }

    #[test]
    fn builds_external_summary_and_post_urls() {
        let external = ProcessedExternal {
            uri: "https://example.com/story".to_owned(),
            title: "Story".to_owned(),
            description: Some("Summary".to_owned()),
        };

        assert_eq!(
            build_external_summary(&external),
            "[Story](https://example.com/story)\nSummary"
        );
        assert_eq!(
            at_uri_to_post_url(
                "at://did:plc:abc/app.bsky.feed.post/3k",
                "alice.bsky.social"
            ),
            "https://bsky.app/profile/alice.bsky.social/post/3k"
        );
        assert_eq!(
            at_uri_to_post_url("not-an-at-uri", "alice.bsky.social"),
            "https://bsky.app/profile/alice.bsky.social"
        );
    }

    #[test]
    fn bluesky_color_matches_brand() {
        assert_eq!(BLUESKY_COLOR, 0x1185FE);
    }

    #[test]
    fn format_count_matches_ts() {
        assert_eq!(format_count(0), "0");
        assert_eq!(format_count(1), "1");
        assert_eq!(format_count(999), "999");
        assert_eq!(format_count(1_000), "1.0K");
        assert_eq!(format_count(1_500), "1.5K");
        assert_eq!(format_count(9_999), "10.0K");
        assert_eq!(format_count(10_000), "10K");
        assert_eq!(format_count(999_999), "999K");
    }

    #[test]
    fn is_not_post_with_missing_post_id() {
        let parts = parse_path_parts(&u("https://bsky.app/profile/a.bsky.social/post/"));
        assert!(!is_post_url(&parts));
    }

    #[test]
    fn is_not_post_for_feed_path() {
        let parts = parse_path_parts(&u("https://bsky.app/profile/a.bsky.social/feed/abc"));
        assert!(!is_post_url(&parts));
    }

    #[test]
    fn embed_links_empty_facets() {
        let text = "no links here";
        assert_eq!(embed_links_in_text(text, None), "no links here");
        assert_eq!(embed_links_in_text(text, Some(&[])), "no links here");
    }

    #[test]
    fn embed_links_skips_overlapping_facets() {
        let text = "overlap";
        let facets = vec![
            BskyFacet {
                index: BskyFacetIndex {
                    byte_start: 0,
                    byte_end: 4,
                },
                features: vec![BskyFacetFeature {
                    facet_type: Some("app.bsky.richtext.facet#link".to_owned()),
                    uri: Some("https://a.com".to_owned()),
                    did: None,
                }],
            },
            BskyFacet {
                index: BskyFacetIndex {
                    byte_start: 2,
                    byte_end: 7,
                },
                features: vec![BskyFacetFeature {
                    facet_type: Some("app.bsky.richtext.facet#link".to_owned()),
                    uri: Some("https://b.com".to_owned()),
                    did: None,
                }],
            },
        ];
        let result = embed_links_in_text(text, Some(&facets));
        assert!(result.contains("[a.com/](https://a.com)"));
        assert!(!result.contains("https://b.com"));
        assert!(result.ends_with("lap"));
    }

    #[test]
    fn embed_links_skips_out_of_bounds_facets() {
        let text = "hi";
        let facets = vec![BskyFacet {
            index: BskyFacetIndex {
                byte_start: 0,
                byte_end: 10,
            },
            features: vec![BskyFacetFeature {
                facet_type: Some("app.bsky.richtext.facet#link".to_owned()),
                uri: Some("https://e.com".to_owned()),
                did: None,
            }],
        }];
        assert_eq!(embed_links_in_text(text, Some(&facets)), "hi");
    }

    #[test]
    fn get_link_display_text_short_path() {
        assert_eq!(
            get_link_display_text("https://example.com/short"),
            "example.com/short"
        );
    }

    #[test]
    fn get_link_display_text_long_path_truncated() {
        assert_eq!(
            get_link_display_text("https://example.com/very/long/path/that/exceeds"),
            "example.com/very/long/pa..."
        );
    }

    #[test]
    fn get_link_display_text_root_url() {
        assert_eq!(get_link_display_text("https://example.com"), "example.com/");
        assert_eq!(
            get_link_display_text("https://example.com/"),
            "example.com/"
        );
    }

    #[test]
    fn normalize_timestamp_rfc3339() {
        assert_eq!(
            normalize_timestamp("2026-05-27T14:10:51.000Z"),
            "2026-05-27T14:10:51.000Z"
        );
    }

    #[test]
    fn normalize_timestamp_with_offset() {
        assert_eq!(
            normalize_timestamp("2026-05-27T14:10:51+02:00"),
            "2026-05-27T12:10:51.000Z"
        );
    }

    #[test]
    fn normalize_timestamp_invalid_passthrough() {
        assert_eq!(normalize_timestamp("not-a-date"), "not-a-date");
    }

    #[test]
    fn sanitize_optional_url_accepts_valid() {
        assert_eq!(
            sanitize_optional_absolute_url(Some("https://e.com/img.png")),
            Some("https://e.com/img.png".to_owned())
        );
    }

    #[test]
    fn sanitize_optional_url_rejects_relative() {
        assert!(sanitize_optional_absolute_url(Some("/relative/path")).is_none());
    }

    #[test]
    fn sanitize_optional_url_trims_whitespace() {
        assert!(sanitize_optional_absolute_url(Some("  https://e.com  ")).is_some());
    }

    #[test]
    fn sanitize_optional_url_none_input() {
        assert!(sanitize_optional_absolute_url(None).is_none());
    }

    #[test]
    fn external_summary_without_description() {
        let ext = ProcessedExternal {
            uri: "https://e.com".to_owned(),
            title: "Title".to_owned(),
            description: None,
        };
        assert_eq!(build_external_summary(&ext), "[Title](https://e.com)");
    }

    #[test]
    fn append_section_empty_base() {
        assert_eq!(append_section("", "content"), "content");
    }

    #[test]
    fn append_section_non_empty_base() {
        assert_eq!(append_section("base", "section"), "base\n\nsection");
    }
}
