// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::media_proxy::MediaProxyClient;
use crate::text_limits::*;
use crate::types::{EmbedAuthor, EmbedFooter, EmbedMedia, EmbedProvider, MessageEmbed};

pub fn normalize_embeds(
    embeds: Vec<MessageEmbed>,
    media_proxy: &MediaProxyClient,
) -> Vec<MessageEmbed> {
    embeds
        .into_iter()
        .map(|e| normalize_embed(e, media_proxy))
        .collect()
}

fn normalize_embed(mut embed: MessageEmbed, media_proxy: &MediaProxyClient) -> MessageEmbed {
    embed.title = embed.title.map(|t| truncate(&t, TITLE_MAX));
    embed.description = embed.description.map(|d| {
        let truncated = truncate(&d, DESCRIPTION_MAX);
        clamp_lines(&truncated, DESCRIPTION_LINES_MAX)
    });
    embed.author = embed.author.and_then(|a| normalize_author(a, media_proxy));
    embed.provider = embed.provider.and_then(normalize_provider);
    embed.footer = embed.footer.and_then(|f| normalize_footer(f, media_proxy));
    embed.thumbnail = embed.thumbnail.map(|m| normalize_media(m, media_proxy));
    embed.image = embed.image.map(|m| normalize_media(m, media_proxy));
    embed.video = embed.video.map(|m| normalize_media(m, media_proxy));
    embed.audio = embed.audio.map(|m| normalize_media(m, media_proxy));
    embed.html = embed.html.map(|h| truncate(&h, HTML_MAX));

    embed.fields = embed.fields.map(|fields| {
        fields
            .into_iter()
            .take(MAX_FIELDS)
            .filter_map(|mut field| {
                let name = truncate(&field.name, FIELD_NAME_MAX);
                if name.is_empty() {
                    return None;
                }
                field.name = name;
                field.value = truncate(&field.value, FIELD_VALUE_MAX);
                Some(field)
            })
            .collect()
    });

    embed.children = embed.children.map(|children| {
        children
            .into_iter()
            .take(MAX_CHILDREN)
            .map(|e| normalize_embed(e, media_proxy))
            .collect()
    });

    embed
}

fn normalize_author(
    mut author: EmbedAuthor,
    media_proxy: &MediaProxyClient,
) -> Option<EmbedAuthor> {
    author.name = truncate(&author.name, AUTHOR_NAME_MAX);
    if author.name.is_empty() {
        return None;
    }
    if author.proxy_icon_url.is_none()
        && let Some(icon) = author.icon_url.as_deref().filter(|u| !u.is_empty())
    {
        author.proxy_icon_url = media_proxy.external_proxy_url(icon);
    }
    Some(author)
}

fn normalize_provider(mut provider: EmbedProvider) -> Option<EmbedProvider> {
    provider.name = truncate(&provider.name, PROVIDER_NAME_MAX);
    if provider.name.is_empty() {
        return None;
    }
    Some(provider)
}

fn normalize_footer(
    mut footer: EmbedFooter,
    media_proxy: &MediaProxyClient,
) -> Option<EmbedFooter> {
    footer.text = truncate(&footer.text, FOOTER_TEXT_MAX);
    if footer.text.is_empty() {
        return None;
    }
    if footer.proxy_icon_url.is_none()
        && let Some(icon) = footer.icon_url.as_deref().filter(|u| !u.is_empty())
    {
        footer.proxy_icon_url = media_proxy.external_proxy_url(icon);
    }
    Some(footer)
}

fn normalize_media(mut media: EmbedMedia, media_proxy: &MediaProxyClient) -> EmbedMedia {
    media.description = media
        .description
        .map(|d| truncate(&d, MEDIA_DESCRIPTION_MAX));
    if media.proxy_url.is_none()
        && let Some(url) = media.url.as_deref().filter(|u| !u.is_empty())
    {
        media.proxy_url = media_proxy.external_proxy_url(url);
    }
    media
}
