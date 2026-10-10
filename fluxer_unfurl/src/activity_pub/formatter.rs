// SPDX-License-Identifier: AGPL-3.0-or-later

use super::types::{
    ActivityPubActor, ActivityPubCollectionCount, ActivityPubContext, ActivityPubPost,
    MastodonMediaAttachment, MastodonPost,
};
use crate::media_proxy::{MediaMetadata, MediaProxyClient, embed_media_flags};
use crate::text_limits;
use crate::types::{EmbedAuthor, EmbedField, EmbedFooter, EmbedMedia, MessageEmbed, NsfwMode};
use chrono::{DateTime, SecondsFormat, Utc};
use url::Url;

const AP_COLOR: u32 = 0x6364FF;
const ENGAGEMENT_THRESHOLD: u64 = 100;

pub struct ActivityPubFormatOptions<'a> {
    pub author_actor: Option<&'a ActivityPubActor>,
    pub quote_child: Option<MessageEmbed>,
    pub is_nested: bool,
}

pub async fn format_mastodon_post(
    post: &MastodonPost,
    url: &Url,
    context: &ActivityPubContext,
    media_proxy: &MediaProxyClient,
    nsfw_mode: NsfwMode,
) -> Vec<MessageEmbed> {
    let account = match &post.account {
        Some(a) => a,
        None => return Vec::new(),
    };

    let display_name = account
        .display_name
        .as_deref()
        .filter(|s| !s.is_empty())
        .or(account.username.as_deref())
        .unwrap_or("unknown");
    let username = account.username.as_deref().unwrap_or("unknown");

    let author_label = text_limits::truncate(
        &format!("{display_name} (@{username}@{})", context.server_domain),
        text_limits::AUTHOR_NAME_MAX,
    );

    let content_source = post.reblog.as_deref().unwrap_or(post);
    let mut content = content_source
        .content
        .as_deref()
        .map(html_to_markdown)
        .unwrap_or_default();

    if let Some(reblog) = post.reblog.as_deref()
        && let Some(reblog_account) = reblog.account.as_ref()
    {
        let reblog_author = reblog_account
            .display_name
            .as_deref()
            .filter(|s| !s.is_empty())
            .or(reblog_account.username.as_deref())
            .unwrap_or("unknown");
        let reblog_text = reblog
            .content
            .as_deref()
            .map(html_to_markdown)
            .unwrap_or_default();
        content = format!("**Boosted from {reblog_author}**\n\n{reblog_text}");
    }

    let description = if let Some(ref spoiler) = post.spoiler_text {
        if !spoiler.is_empty() {
            format!("**{spoiler}**\n\n{content}")
        } else {
            content
        }
    } else {
        content
    };
    let description = add_reply_context(description, context);

    let mut embed = MessageEmbed::new("rich");
    embed.url = Some(url.to_string());
    embed.description = Some(text_limits::truncate(
        &description,
        text_limits::DESCRIPTION_MAX,
    ));
    embed.color = Some(AP_COLOR);

    if let Some(ref ts) = post.created_at {
        embed.timestamp = Some(normalize_timestamp(ts));
    }

    embed.author = Some(EmbedAuthor {
        name: author_label,
        url: account
            .url
            .as_deref()
            .and_then(sanitize_optional_absolute_url),
        icon_url: account
            .avatar
            .as_deref()
            .and_then(sanitize_optional_absolute_url),
        ..Default::default()
    });

    embed.footer = Some(EmbedFooter {
        text: text_limits::truncate(&context.server_title, text_limits::FOOTER_TEXT_MAX),
        icon_url: context.server_icon.clone(),
        ..Default::default()
    });

    let fields = mastodon_fields(post);
    if !fields.is_empty() {
        embed.fields = Some(fields);
    }

    let resolved = resolve_mastodon_media_embeds(post, url, media_proxy, nsfw_mode).await;
    if let Some(image) = resolved.image {
        embed.image = Some(image);
    }
    if let Some(video) = resolved.video {
        embed.video = Some(video);
        embed.thumbnail = resolved.thumbnail;
    }

    let mut embeds = vec![embed];
    embeds.extend(resolved.gallery_embeds);
    embeds
}

pub async fn format_activity_pub_post(
    post: &ActivityPubPost,
    url: &Url,
    context: &ActivityPubContext,
    media_proxy: &MediaProxyClient,
    nsfw_mode: NsfwMode,
    options: ActivityPubFormatOptions<'_>,
) -> Vec<MessageEmbed> {
    let content = post
        .content
        .as_deref()
        .map(html_to_markdown)
        .unwrap_or_default();

    let description = if let Some(ref summary) = post.summary {
        if !summary.is_empty() {
            format!("**{summary}**\n\n{content}")
        } else {
            content
        }
    } else {
        content
    };
    let description = add_reply_context(description, context);

    let (author_name, author_url, author_icon) =
        resolve_author(post, context, options.author_actor);

    let mut embed = MessageEmbed::new("rich");
    embed.url = Some(post.url.as_deref().unwrap_or(url.as_str()).to_owned());
    embed.description = Some(text_limits::truncate(
        &description,
        text_limits::DESCRIPTION_MAX,
    ));
    embed.color = Some(AP_COLOR);

    embed.author = Some(EmbedAuthor {
        name: text_limits::truncate(&author_name, text_limits::AUTHOR_NAME_MAX),
        url: author_url,
        icon_url: author_icon,
        ..Default::default()
    });

    if !options.is_nested {
        if let Some(ref ts) = post.published {
            embed.timestamp = Some(normalize_timestamp(ts));
        }
        embed.footer = Some(EmbedFooter {
            text: text_limits::truncate(&context.server_title, text_limits::FOOTER_TEXT_MAX),
            icon_url: context
                .server_icon
                .as_deref()
                .and_then(sanitize_optional_absolute_url),
            ..Default::default()
        });

        let fields = activity_pub_fields(post);
        if !fields.is_empty() {
            embed.fields = Some(fields);
        }
    }

    let resolved = resolve_activity_pub_media_embeds(post, url, media_proxy, nsfw_mode).await;
    if let Some(image) = resolved.image {
        embed.image = Some(image);
    }
    if let Some(video) = resolved.video {
        embed.video = Some(video);
        embed.thumbnail = resolved.thumbnail;
    }
    if let Some(child) = options.quote_child {
        embed.children = Some(vec![child]);
    }

    let mut embeds = vec![embed];
    embeds.extend(resolved.gallery_embeds);
    embeds
}

struct ResolvedMediaEmbeds {
    image: Option<EmbedMedia>,
    video: Option<EmbedMedia>,
    thumbnail: Option<EmbedMedia>,
    gallery_embeds: Vec<MessageEmbed>,
}

impl ResolvedMediaEmbeds {
    fn empty() -> Self {
        Self {
            image: None,
            video: None,
            thumbnail: None,
            gallery_embeds: Vec::new(),
        }
    }
}

async fn resolve_mastodon_media_embeds(
    post: &MastodonPost,
    url: &Url,
    media_proxy: &MediaProxyClient,
    nsfw_mode: NsfwMode,
) -> ResolvedMediaEmbeds {
    let mut resolved = ResolvedMediaEmbeds::empty();
    let mut primary_media_claimed = false;
    let nsfw_str = MediaProxyClient::nsfw_mode_str(nsfw_mode);

    for attachment in post.media_attachments.as_deref().unwrap_or_default() {
        match attachment.attachment_type.as_deref() {
            Some("image" | "gifv") => {
                let Some(image) = process_mastodon_media(attachment, media_proxy, nsfw_str).await
                else {
                    continue;
                };
                if !primary_media_claimed {
                    resolved.image = Some(image);
                    primary_media_claimed = true;
                } else {
                    resolved
                        .gallery_embeds
                        .push(create_rich_image_embed(url, image));
                }
            }
            Some("video") => {
                let Some(video) = process_mastodon_media(attachment, media_proxy, nsfw_str).await
                else {
                    continue;
                };
                let thumbnail = match attachment.preview_url.as_deref() {
                    Some(preview_url) => {
                        process_mastodon_media_url(attachment, preview_url, media_proxy, nsfw_str)
                            .await
                    }
                    None => None,
                };
                if !primary_media_claimed {
                    resolved.video = Some(video);
                    resolved.thumbnail = thumbnail;
                    primary_media_claimed = true;
                } else {
                    resolved
                        .gallery_embeds
                        .push(create_rich_video_embed(url, video, thumbnail));
                }
            }
            _ => {}
        }
    }

    resolved
}

async fn resolve_activity_pub_media_embeds(
    post: &ActivityPubPost,
    url: &Url,
    media_proxy: &MediaProxyClient,
    nsfw_mode: NsfwMode,
) -> ResolvedMediaEmbeds {
    let mut resolved = ResolvedMediaEmbeds::empty();
    let mut primary_media_claimed = false;
    let nsfw_str = MediaProxyClient::nsfw_mode_str(nsfw_mode);

    for attachment in post.attachment.as_deref().unwrap_or_default() {
        if attachment_is_image(attachment) {
            let Some(image) = process_activity_pub_media(attachment, media_proxy, nsfw_str).await
            else {
                continue;
            };
            if !primary_media_claimed {
                resolved.image = Some(image);
                primary_media_claimed = true;
            } else {
                resolved
                    .gallery_embeds
                    .push(create_rich_image_embed(url, image));
            }
            continue;
        }

        if attachment_is_video(attachment) {
            let Some(video) = process_activity_pub_media(attachment, media_proxy, nsfw_str).await
            else {
                continue;
            };
            let thumbnail =
                post.attachment
                    .as_deref()
                    .unwrap_or_default()
                    .iter()
                    .find(|candidate| {
                        attachment_is_image(candidate)
                            && candidate.url.as_deref() != attachment.url.as_deref()
                    });
            let thumbnail = match thumbnail {
                Some(thumbnail) => {
                    process_activity_pub_media(thumbnail, media_proxy, nsfw_str).await
                }
                None => None,
            };
            if !primary_media_claimed {
                resolved.video = Some(video);
                resolved.thumbnail = thumbnail;
                primary_media_claimed = true;
            } else {
                resolved
                    .gallery_embeds
                    .push(create_rich_video_embed(url, video, thumbnail));
            }
        }
    }

    resolved
}

async fn process_mastodon_media(
    attachment: &MastodonMediaAttachment,
    media_proxy: &MediaProxyClient,
    nsfw_str: &str,
) -> Option<EmbedMedia> {
    let url = attachment.url.as_deref()?;
    process_mastodon_media_url(attachment, url, media_proxy, nsfw_str).await
}

async fn process_mastodon_media_url(
    attachment: &MastodonMediaAttachment,
    media_url: &str,
    media_proxy: &MediaProxyClient,
    nsfw_str: &str,
) -> Option<EmbedMedia> {
    match media_proxy.get_metadata(media_url, nsfw_str).await {
        Ok(metadata) => Some(build_media_from_metadata(
            media_url,
            &metadata,
            mastodon_attachment_width(attachment).or(metadata.width),
            mastodon_attachment_height(attachment).or(metadata.height),
            attachment.description.as_deref(),
        )),
        Err(err) => {
            tracing::warn!(error = %err, url = media_url, "failed to process ActivityPub media");
            None
        }
    }
}

async fn process_activity_pub_media(
    attachment: &super::types::ActivityPubAttachment,
    media_proxy: &MediaProxyClient,
    nsfw_str: &str,
) -> Option<EmbedMedia> {
    let url = attachment.url.as_deref()?;
    match media_proxy.get_metadata(url, nsfw_str).await {
        Ok(metadata) => Some(build_media_from_metadata(
            url,
            &metadata,
            attachment.width.or(metadata.width),
            attachment.height.or(metadata.height),
            attachment.name.as_deref(),
        )),
        Err(err) => {
            tracing::warn!(error = %err, url, "failed to process ActivityPub media");
            None
        }
    }
}

fn build_media_from_metadata(
    url: &str,
    metadata: &MediaMetadata,
    width: Option<u32>,
    height: Option<u32>,
    description: Option<&str>,
) -> EmbedMedia {
    EmbedMedia {
        url: Some(url.to_owned()),
        content_type: Some(metadata.content_type.clone()),
        content_hash: Some(metadata.content_hash.clone()),
        width,
        height,
        duration: metadata.duration.map(|duration| duration as u32),
        placeholder: metadata.placeholder.clone(),
        flags: embed_media_flags(metadata),
        description: description
            .filter(|description| !description.is_empty())
            .map(|description| {
                text_limits::truncate(description, text_limits::MEDIA_DESCRIPTION_MAX)
            }),
        ..Default::default()
    }
}

fn create_rich_image_embed(url: &Url, image: EmbedMedia) -> MessageEmbed {
    let mut embed = MessageEmbed::new("rich");
    embed.url = Some(url.to_string());
    embed.image = Some(image);
    embed
}

fn create_rich_video_embed(
    url: &Url,
    video: EmbedMedia,
    thumbnail: Option<EmbedMedia>,
) -> MessageEmbed {
    let mut embed = MessageEmbed::new("rich");
    embed.url = Some(url.to_string());
    embed.video = Some(video);
    embed.thumbnail = thumbnail;
    embed
}

fn mastodon_attachment_width(attachment: &MastodonMediaAttachment) -> Option<u32> {
    attachment
        .meta
        .as_ref()
        .and_then(|meta| meta.original.as_ref().or(meta.small.as_ref()))
        .and_then(|size| size.width)
}

fn mastodon_attachment_height(attachment: &MastodonMediaAttachment) -> Option<u32> {
    attachment
        .meta
        .as_ref()
        .and_then(|meta| meta.original.as_ref().or(meta.small.as_ref()))
        .and_then(|size| size.height)
}

fn attachment_is_image(attachment: &super::types::ActivityPubAttachment) -> bool {
    attachment
        .media_type
        .as_deref()
        .is_some_and(|media_type| media_type.starts_with("image/"))
}

fn attachment_is_video(attachment: &super::types::ActivityPubAttachment) -> bool {
    attachment
        .media_type
        .as_deref()
        .is_some_and(|media_type| media_type.starts_with("video/"))
}

fn mastodon_fields(post: &MastodonPost) -> Vec<EmbedField> {
    let mut fields = Vec::new();
    push_threshold_field(
        &mut fields,
        "Favorites",
        post.favourites_count.unwrap_or_default(),
    );
    push_threshold_field(
        &mut fields,
        "Boosts",
        post.reblogs_count.unwrap_or_default(),
    );
    push_threshold_field(
        &mut fields,
        "Replies",
        post.replies_count.unwrap_or_default(),
    );

    if let Some(poll) = post.poll.as_ref() {
        let poll_options = poll
            .options
            .iter()
            .map(|option| match option.votes_count {
                Some(votes) => format!("• {}: {votes}", option.title),
                None => format!("• {}", option.title),
            })
            .collect::<Vec<_>>()
            .join("\n");
        fields.push(EmbedField {
            name: text_limits::truncate(
                &format!("Poll ({} votes)", poll.votes_count.unwrap_or_default()),
                text_limits::FIELD_NAME_MAX,
            ),
            value: text_limits::truncate(&poll_options, text_limits::FIELD_VALUE_MAX),
            inline: false,
        });
    }

    fields
}

fn activity_pub_fields(post: &ActivityPubPost) -> Vec<EmbedField> {
    let mut fields = Vec::new();
    push_threshold_field(&mut fields, "Likes", collection_count(post.likes.as_ref()));
    push_threshold_field(
        &mut fields,
        "Shares",
        collection_count(post.shares.as_ref()),
    );
    push_threshold_field(
        &mut fields,
        "Replies",
        post.replies
            .as_ref()
            .and_then(|replies| replies.total_items)
            .unwrap_or_default(),
    );
    fields
}

fn push_threshold_field(fields: &mut Vec<EmbedField>, name: &str, value: u64) {
    if value >= ENGAGEMENT_THRESHOLD {
        fields.push(EmbedField {
            name: name.to_owned(),
            value: value.to_string(),
            inline: true,
        });
    }
}

fn collection_count(collection: Option<&ActivityPubCollectionCount>) -> u64 {
    match collection {
        Some(ActivityPubCollectionCount::Count(count)) => *count,
        Some(ActivityPubCollectionCount::Collection(collection)) => {
            collection.total_items.unwrap_or_default()
        }
        None => 0,
    }
}

fn resolve_author(
    post: &ActivityPubPost,
    context: &ActivityPubContext,
    actor_override: Option<&ActivityPubActor>,
) -> (String, Option<String>, Option<String>) {
    if let Some(actor) = actor_override {
        return resolve_actor_author(actor, context);
    }

    let attributed = match &post.attributed_to {
        Some(v) => v,
        None => return ("unknown".to_owned(), None, None),
    };

    if let Some(url_str) = attributed.as_str() {
        let username = extract_username_from_url(url_str).unwrap_or_default();
        let label = if username.is_empty() {
            url_str.to_owned()
        } else {
            format!("{username} (@{username}@{})", context.server_domain)
        };
        return (label, sanitize_optional_absolute_url(url_str), None);
    }

    if let Ok(actor) = serde_json::from_value::<ActivityPubActor>(attributed.clone()) {
        return resolve_actor_author(&actor, context);
    }

    ("unknown".to_owned(), None, None)
}

fn resolve_actor_author(
    actor: &ActivityPubActor,
    context: &ActivityPubContext,
) -> (String, Option<String>, Option<String>) {
    let name = actor
        .name
        .as_deref()
        .or(actor.preferred_username.as_deref())
        .unwrap_or("unknown");
    let username = actor.preferred_username.as_deref().unwrap_or(name);
    let label = format!("{name} (@{username}@{})", context.server_domain);
    let author_url = actor
        .url
        .as_deref()
        .or(actor.id.as_deref())
        .and_then(sanitize_optional_absolute_url);
    let icon = actor
        .icon
        .as_ref()
        .and_then(|i| i.url.as_deref())
        .and_then(sanitize_optional_absolute_url);
    (label, author_url, icon)
}

fn extract_username_from_url(url: &str) -> Option<String> {
    let parsed = Url::parse(url).ok()?;
    let segs: Vec<&str> = parsed.path_segments()?.filter(|s| !s.is_empty()).collect();
    if segs.is_empty() {
        return None;
    }
    if let Some(stripped) = segs[0].strip_prefix('@') {
        return Some(stripped.to_owned());
    }
    if let Some(users_index) = segs.iter().position(|segment| *segment == "users")
        && let Some(username) = segs.get(users_index + 1)
    {
        return Some((*username).to_owned());
    }
    segs.last().map(|s| s.to_string())
}

fn add_reply_context(description: String, context: &ActivityPubContext) -> String {
    match context.in_reply_to.as_ref() {
        Some(reply) => format!("-# ↩ [{}]({})\n{description}", reply.author, reply.url),
        None => description,
    }
}

fn html_to_markdown(html: &str) -> String {
    crate::html_markdown::to_markdown(html, decode_html_entities)
}

fn decode_html_entities(input: &str) -> String {
    static ENTITY_RE: std::sync::LazyLock<regex::Regex> = std::sync::LazyLock::new(|| {
        regex::Regex::new(r"&(#(?:x[0-9A-Fa-f]+|\d+)|[A-Za-z][A-Za-z0-9]+);?").expect("valid regex")
    });
    ENTITY_RE
        .replace_all(input, |caps: &regex::Captures<'_>| {
            let entity = caps.get(0).map(|m| m.as_str()).unwrap_or_default();
            if let Some(numeric) = entity.strip_prefix("&#") {
                let numeric = numeric.strip_suffix(';').unwrap_or(numeric);
                let codepoint = if let Some(hex) = numeric
                    .strip_prefix('x')
                    .or_else(|| numeric.strip_prefix('X'))
                {
                    u32::from_str_radix(hex, 16).ok()
                } else {
                    numeric.parse::<u32>().ok()
                };
                return codepoint
                    .and_then(char::from_u32)
                    .map(|ch| ch.to_string())
                    .unwrap_or_else(|| entity.to_owned());
            }
            entities::ENTITIES
                .iter()
                .find(|entry| entry.entity == entity)
                .map(|entry| entry.characters.to_owned())
                .unwrap_or_else(|| entity.to_owned())
        })
        .into_owned()
}

fn sanitize_optional_absolute_url(value: &str) -> Option<String> {
    Url::parse(value.trim()).ok().map(|url| url.to_string())
}

fn normalize_timestamp(value: &str) -> String {
    DateTime::parse_from_rfc3339(value)
        .map(|dt| {
            dt.with_timezone(&Utc)
                .to_rfc3339_opts(SecondsFormat::Millis, true)
        })
        .unwrap_or_else(|_| value.to_owned())
}
