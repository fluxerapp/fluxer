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

#[cfg(test)]
mod tests {
    use super::super::types::{
        ActivityPubAttachment, ActivityPubCollection, MastodonMediaMeta, MastodonMediaMetaSize,
        MastodonPoll, MastodonPollOption,
    };
    use super::*;

    #[test]
    fn username_from_activity_pub_url_shapes() {
        assert_eq!(
            extract_username_from_url("https://example.test/@alice").as_deref(),
            Some("alice")
        );
        assert_eq!(
            extract_username_from_url("https://example.test/users/bob").as_deref(),
            Some("bob")
        );
        assert_eq!(
            extract_username_from_url("https://example.test/actors/users/carol").as_deref(),
            Some("carol")
        );
    }

    #[test]
    fn activity_pub_engagement_fields_use_threshold() {
        let post = ActivityPubPost {
            url: None,
            published: None,
            attributed_to: None,
            content: None,
            summary: None,
            attachment: None,
            in_reply_to: None,
            likes: Some(ActivityPubCollectionCount::Count(100)),
            shares: Some(ActivityPubCollectionCount::Collection(
                ActivityPubCollection {
                    total_items: Some(99),
                },
            )),
            replies: Some(ActivityPubCollection {
                total_items: Some(101),
            }),
            quote: None,
            quote_uri: None,
            misskey_quote: None,
        };

        let fields = activity_pub_fields(&post);
        assert_eq!(fields.len(), 2);
        assert_eq!(fields[0].name, "Likes");
        assert_eq!(fields[0].value, "100");
        assert_eq!(fields[1].name, "Replies");
        assert_eq!(fields[1].value, "101");
    }

    #[test]
    fn mastodon_poll_field_formats_votes() {
        let post = MastodonPost {
            created_at: None,
            in_reply_to_id: None,
            in_reply_to_account_id: None,
            content: None,
            spoiler_text: None,
            url: None,
            account: None,
            media_attachments: None,
            favourites_count: Some(99),
            reblogs_count: Some(100),
            replies_count: None,
            reblog: None,
            poll: Some(MastodonPoll {
                votes_count: Some(321),
                options: vec![
                    MastodonPollOption {
                        title: "First".to_owned(),
                        votes_count: Some(10),
                    },
                    MastodonPollOption {
                        title: "Second".to_owned(),
                        votes_count: None,
                    },
                ],
            }),
        };

        let fields = mastodon_fields(&post);
        assert_eq!(fields.len(), 2);
        assert_eq!(fields[0].name, "Boosts");
        assert_eq!(fields[0].value, "100");
        assert_eq!(fields[1].name, "Poll (321 votes)");
        assert_eq!(fields[1].value, "• First: 10\n• Second");
        assert!(!fields[1].inline);
    }

    #[test]
    fn mastodon_dimensions_prefer_original_then_small() {
        let attachment = MastodonMediaAttachment {
            attachment_type: Some("image".to_owned()),
            url: Some("https://cdn.example.test/a.jpg".to_owned()),
            preview_url: None,
            description: None,
            meta: Some(MastodonMediaMeta {
                original: None,
                small: Some(MastodonMediaMetaSize {
                    width: Some(640),
                    height: Some(480),
                }),
            }),
        };

        assert_eq!(mastodon_attachment_width(&attachment), Some(640));
        assert_eq!(mastodon_attachment_height(&attachment), Some(480));
    }

    #[test]
    fn attachment_kind_uses_media_type() {
        let image = ActivityPubAttachment {
            media_type: Some("image/png".to_owned()),
            url: Some("https://cdn.example.test/a.png".to_owned()),
            width: None,
            height: None,
            name: None,
        };
        let video = ActivityPubAttachment {
            media_type: Some("video/mp4".to_owned()),
            ..image.clone()
        };

        assert!(attachment_is_image(&image));
        assert!(!attachment_is_video(&image));
        assert!(attachment_is_video(&video));
        assert!(!attachment_is_image(&video));
    }

    #[test]
    fn metadata_builds_embed_media_payload() {
        let metadata = MediaMetadata {
            content_type: "image/jpeg".to_owned(),
            content_hash: "abc123".to_owned(),
            width: Some(800),
            height: Some(600),
            duration: None,
            placeholder: Some("blur".to_owned()),
            animated: Some(true),
            nsfw: true,
        };

        let media = build_media_from_metadata(
            "https://cdn.example.test/a.jpg",
            &metadata,
            Some(1024),
            Some(768),
            Some("alt text"),
        );

        assert_eq!(media.url.as_deref(), Some("https://cdn.example.test/a.jpg"));
        assert_eq!(media.width, Some(1024));
        assert_eq!(media.height, Some(768));
        assert_eq!(media.content_type.as_deref(), Some("image/jpeg"));
        assert_eq!(media.content_hash.as_deref(), Some("abc123"));
        assert_eq!(media.placeholder.as_deref(), Some("blur"));
        assert_eq!(media.description.as_deref(), Some("alt text"));
        assert_eq!(media.flags, (1 << 4) | (1 << 5));
    }

    #[test]
    fn html_to_markdown_preserves_links() {
        assert_eq!(
            html_to_markdown(r#"<a href="https://e.com">text</a>"#),
            "[text](https://e.com)"
        );
    }

    #[test]
    fn html_to_markdown_converts_bold() {
        assert_eq!(html_to_markdown("<strong>bold</strong>"), "**bold**");
        assert_eq!(html_to_markdown("<b>bold</b>"), "**bold**");
    }

    #[test]
    fn html_to_markdown_converts_italic() {
        assert_eq!(html_to_markdown("<em>italic</em>"), "_italic_");
        assert_eq!(html_to_markdown("<i>italic</i>"), "_italic_");
    }

    #[test]
    fn html_to_markdown_converts_pre_code_blocks() {
        let result = html_to_markdown("<pre><code>fn main() {}</code></pre>");
        assert!(result.contains("fn main() {}"));
    }

    #[test]
    fn html_to_markdown_converts_paragraphs() {
        let result = html_to_markdown("<p>first</p><p>second</p>");
        assert!(result.contains("first"));
        assert!(result.contains("second"));
        assert!(!result.contains("<p>"));
    }

    #[test]
    fn html_to_markdown_converts_br() {
        assert_eq!(html_to_markdown("line1<br>line2"), "line1\nline2");
        assert_eq!(html_to_markdown("line1<br/>line2"), "line1\nline2");
        assert_eq!(html_to_markdown("line1<br />line2"), "line1\nline2");
    }

    #[test]
    fn html_to_markdown_converts_headers() {
        let result = html_to_markdown("<h1>Title</h1>");
        assert!(result.contains("**Title**"));
    }

    #[test]
    fn html_to_markdown_strips_unknown_tags() {
        assert_eq!(html_to_markdown("<div>hello</div>"), "hello");
    }

    #[test]
    fn html_to_markdown_decodes_entities() {
        assert_eq!(html_to_markdown("&amp; &lt; &gt;"), "& < >");
    }

    #[test]
    fn html_to_markdown_collapses_excess_newlines() {
        let result = html_to_markdown("<p>a</p><p></p><p></p><p>b</p>");
        assert!(!result.contains("\n\n\n"));
    }

    #[test]
    fn mastodon_engagement_below_threshold_hidden() {
        let post = MastodonPost {
            favourites_count: Some(50),
            reblogs_count: Some(99),
            replies_count: Some(1),
            ..MastodonPost {
                created_at: None,
                in_reply_to_id: None,
                in_reply_to_account_id: None,
                content: None,
                spoiler_text: None,
                url: None,
                account: None,
                media_attachments: None,
                favourites_count: None,
                reblogs_count: None,
                replies_count: None,
                reblog: None,
                poll: None,
            }
        };
        let fields = mastodon_fields(&post);
        assert!(fields.is_empty());
    }

    #[test]
    fn mastodon_engagement_at_threshold_shown() {
        let post = MastodonPost {
            favourites_count: Some(100),
            reblogs_count: Some(200),
            replies_count: Some(50),
            created_at: None,
            in_reply_to_id: None,
            in_reply_to_account_id: None,
            content: None,
            spoiler_text: None,
            url: None,
            account: None,
            media_attachments: None,
            reblog: None,
            poll: None,
        };
        let fields = mastodon_fields(&post);
        assert_eq!(fields.len(), 2);
        assert_eq!(fields[0].name, "Favorites");
        assert_eq!(fields[1].name, "Boosts");
    }

    #[test]
    fn resolve_author_from_actor() {
        let actor = ActivityPubActor {
            id: None,
            name: Some("Alice".to_owned()),
            preferred_username: Some("alice".to_owned()),
            url: Some("https://masto.test/@alice".to_owned()),
            icon: Some(super::super::types::ActivityPubIcon {
                url: Some("https://masto.test/avatar.png".to_owned()),
            }),
        };
        let context = ActivityPubContext {
            server_domain: "masto.test".to_owned(),
            server_title: "Mastodon".to_owned(),
            server_icon: None,
            in_reply_to: None,
        };
        let (name, url, icon) = resolve_actor_author(&actor, &context);
        assert!(name.contains("Alice"));
        assert!(name.contains("@alice@masto.test"));
        assert_eq!(url, Some("https://masto.test/@alice".to_owned()));
        assert!(icon.is_some());
    }

    #[test]
    fn resolve_author_from_url_string() {
        let post = ActivityPubPost {
            attributed_to: Some(serde_json::json!("https://example.test/@bob")),
            ..ActivityPubPost {
                url: None,
                published: None,
                attributed_to: None,
                content: None,
                summary: None,
                attachment: None,
                in_reply_to: None,
                likes: None,
                shares: None,
                replies: None,
                quote: None,
                quote_uri: None,
                misskey_quote: None,
            }
        };
        let context = ActivityPubContext {
            server_domain: "example.test".to_owned(),
            server_title: "Example".to_owned(),
            server_icon: None,
            in_reply_to: None,
        };
        let (name, url, _) = resolve_author(&post, &context, None);
        assert!(name.contains("bob"));
        assert!(url.is_some());
    }

    #[test]
    fn resolve_author_missing_returns_unknown() {
        let post = ActivityPubPost {
            url: None,
            published: None,
            attributed_to: None,
            content: None,
            summary: None,
            attachment: None,
            in_reply_to: None,
            likes: None,
            shares: None,
            replies: None,
            quote: None,
            quote_uri: None,
            misskey_quote: None,
        };
        let context = ActivityPubContext {
            server_domain: "e.test".to_owned(),
            server_title: "E".to_owned(),
            server_icon: None,
            in_reply_to: None,
        };
        let (name, _, _) = resolve_author(&post, &context, None);
        assert_eq!(name, "unknown");
    }

    #[test]
    fn add_reply_context_with_parent() {
        let ctx = ActivityPubContext {
            server_domain: "e.test".to_owned(),
            server_title: "E".to_owned(),
            server_icon: None,
            in_reply_to: Some(super::super::types::ActivityPubReplyContext {
                author: "@alice@masto.test".to_owned(),
                url: "https://masto.test/@alice/123".to_owned(),
            }),
        };
        let result = add_reply_context("My reply".to_owned(), &ctx);
        assert!(result.starts_with("-# \u{21a9}"));
        assert!(result.contains("@alice@masto.test"));
        assert!(result.contains("My reply"));
    }

    #[test]
    fn add_reply_context_without_parent() {
        let ctx = ActivityPubContext {
            server_domain: "e.test".to_owned(),
            server_title: "E".to_owned(),
            server_icon: None,
            in_reply_to: None,
        };
        assert_eq!(add_reply_context("text".to_owned(), &ctx), "text");
    }

    #[test]
    fn embed_media_flags_nsfw_only() {
        let meta = MediaMetadata {
            content_type: "image/jpeg".to_owned(),
            content_hash: "h".to_owned(),
            width: None,
            height: None,
            duration: None,
            placeholder: None,
            animated: None,
            nsfw: true,
        };
        assert_eq!(embed_media_flags(&meta), 1 << 4);
    }

    #[test]
    fn embed_media_flags_animated_only() {
        let meta = MediaMetadata {
            content_type: "image/gif".to_owned(),
            content_hash: "h".to_owned(),
            width: None,
            height: None,
            duration: None,
            placeholder: None,
            animated: Some(true),
            nsfw: false,
        };
        assert_eq!(embed_media_flags(&meta), 1 << 5);
    }

    #[test]
    fn embed_media_flags_both() {
        let meta = MediaMetadata {
            content_type: "image/gif".to_owned(),
            content_hash: "h".to_owned(),
            width: None,
            height: None,
            duration: None,
            placeholder: None,
            animated: Some(true),
            nsfw: true,
        };
        assert_eq!(embed_media_flags(&meta), (1 << 4) | (1 << 5));
    }

    #[test]
    fn embed_media_flags_neither() {
        let meta = MediaMetadata {
            content_type: "image/jpeg".to_owned(),
            content_hash: "h".to_owned(),
            width: None,
            height: None,
            duration: None,
            placeholder: None,
            animated: Some(false),
            nsfw: false,
        };
        assert_eq!(embed_media_flags(&meta), 0);
    }

    #[test]
    fn empty_description_not_included_in_media() {
        let meta = MediaMetadata {
            content_type: "image/jpeg".to_owned(),
            content_hash: "h".to_owned(),
            width: None,
            height: None,
            duration: None,
            placeholder: None,
            animated: None,
            nsfw: false,
        };
        let media = build_media_from_metadata("https://e.com/a.jpg", &meta, None, None, Some(""));
        assert!(media.description.is_none());
    }
}
