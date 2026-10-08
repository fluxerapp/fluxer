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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::EmbedField;

    fn test_proxy() -> MediaProxyClient {
        MediaProxyClient::new_with_public_endpoint(
            "http://media-proxy:8080/",
            "test-secret",
            Some("https://media.example.test/"),
            reqwest::Client::new(),
        )
    }

    fn embed_with_title(t: &str) -> MessageEmbed {
        let mut e = MessageEmbed::new("rich");
        e.title = Some(t.to_owned());
        e
    }

    #[test]
    fn long_titles_are_truncated() {
        let long = "a".repeat(500);
        let result = normalize_embeds(vec![embed_with_title(&long)], &test_proxy());
        assert!(result[0].title.as_ref().unwrap().chars().count() <= TITLE_MAX);
    }

    #[test]
    fn short_titles_are_unchanged() {
        let result = normalize_embeds(vec![embed_with_title("Short")], &test_proxy());
        assert_eq!(result[0].title.as_deref(), Some("Short"));
    }

    #[test]
    fn empty_author_name_strips_author() {
        let mut e = MessageEmbed::new("rich");
        e.author = Some(EmbedAuthor {
            name: "".into(),
            ..Default::default()
        });
        let result = normalize_embeds(vec![e], &test_proxy());
        assert!(result[0].author.is_none());
    }

    #[test]
    fn empty_footer_text_strips_footer() {
        let mut e = MessageEmbed::new("rich");
        e.footer = Some(EmbedFooter {
            text: "".into(),
            ..Default::default()
        });
        let result = normalize_embeds(vec![e], &test_proxy());
        assert!(result[0].footer.is_none());
    }

    #[test]
    fn fields_limited_to_max() {
        let mut e = MessageEmbed::new("rich");
        let fields: Vec<EmbedField> = (0..50)
            .map(|i| EmbedField {
                name: format!("f{i}"),
                value: "v".into(),
                inline: false,
            })
            .collect();
        e.fields = Some(fields);
        let result = normalize_embeds(vec![e], &test_proxy());
        assert!(result[0].fields.as_ref().unwrap().len() <= MAX_FIELDS);
    }

    #[test]
    fn empty_field_names_are_removed() {
        let mut e = MessageEmbed::new("rich");
        e.fields = Some(vec![
            EmbedField {
                name: "".into(),
                value: "v".into(),
                inline: false,
            },
            EmbedField {
                name: "ok".into(),
                value: "v".into(),
                inline: false,
            },
        ]);
        let result = normalize_embeds(vec![e], &test_proxy());
        assert_eq!(result[0].fields.as_ref().unwrap().len(), 1);
    }

    #[test]
    fn provider_with_empty_name_string_is_removed() {
        let mut e = MessageEmbed::new("rich");
        e.provider = Some(EmbedProvider {
            name: String::new(),
            url: Some("https://example.com".to_owned()),
        });
        let result = normalize_embeds(vec![e], &test_proxy());
        assert!(result[0].provider.is_none());
    }

    #[test]
    fn provider_with_long_name_is_truncated() {
        let mut e = MessageEmbed::new("rich");
        let long = "X".repeat(500);
        e.provider = Some(EmbedProvider {
            name: long,
            url: None,
        });
        let result = normalize_embeds(vec![e], &test_proxy());
        let p = result[0].provider.as_ref().unwrap();
        assert!(p.name.chars().count() <= PROVIDER_NAME_MAX);
    }

    #[test]
    fn description_clamped_to_max_lines() {
        let mut e = MessageEmbed::new("rich");
        let many_lines = (0..50)
            .map(|i| format!("line {i}"))
            .collect::<Vec<_>>()
            .join("\n");
        e.description = Some(many_lines);
        let result = normalize_embeds(vec![e], &test_proxy());
        let lines: Vec<_> = result[0].description.as_ref().unwrap().lines().collect();
        assert!(lines.len() <= DESCRIPTION_LINES_MAX + 1);
    }

    #[test]
    fn children_limited_to_max() {
        let mut e = MessageEmbed::new("rich");
        e.children = Some(vec![
            MessageEmbed::new("rich"),
            MessageEmbed::new("rich"),
            MessageEmbed::new("rich"),
        ]);
        let result = normalize_embeds(vec![e], &test_proxy());
        assert_eq!(result[0].children.as_ref().unwrap().len(), MAX_CHILDREN);
    }

    #[test]
    fn nested_children_are_normalized() {
        let mut child = MessageEmbed::new("rich");
        child.title = Some("a".repeat(500));
        let mut e = MessageEmbed::new("rich");
        e.children = Some(vec![child]);
        let result = normalize_embeds(vec![e], &test_proxy());
        let child_title = result[0].children.as_ref().unwrap()[0]
            .title
            .as_ref()
            .unwrap();
        assert!(child_title.chars().count() <= TITLE_MAX);
    }

    #[test]
    fn media_description_is_truncated() {
        let mut e = MessageEmbed::new("rich");
        e.thumbnail = Some(EmbedMedia {
            url: Some("https://example.com/img.png".into()),
            description: Some("d".repeat(5000)),
            ..Default::default()
        });
        let result = normalize_embeds(vec![e], &test_proxy());
        let desc = result[0]
            .thumbnail
            .as_ref()
            .unwrap()
            .description
            .as_ref()
            .unwrap();
        assert!(desc.chars().count() <= MEDIA_DESCRIPTION_MAX);
    }

    #[test]
    fn html_field_is_truncated() {
        let mut e = MessageEmbed::new("rich");
        e.html = Some("x".repeat(15000));
        let result = normalize_embeds(vec![e], &test_proxy());
        assert!(result[0].html.as_ref().unwrap().chars().count() <= HTML_MAX);
    }

    #[test]
    fn field_value_is_truncated() {
        let mut e = MessageEmbed::new("rich");
        e.fields = Some(vec![EmbedField {
            name: "ok".into(),
            value: "v".repeat(2000),
            inline: false,
        }]);
        let result = normalize_embeds(vec![e], &test_proxy());
        let val = &result[0].fields.as_ref().unwrap()[0].value;
        assert!(val.chars().count() <= FIELD_VALUE_MAX);
    }

    #[test]
    fn none_fields_remain_none() {
        let e = MessageEmbed::new("rich");
        let result = normalize_embeds(vec![e], &test_proxy());
        assert!(result[0].title.is_none());
        assert!(result[0].description.is_none());
        assert!(result[0].author.is_none());
        assert!(result[0].provider.is_none());
        assert!(result[0].footer.is_none());
        assert!(result[0].fields.is_none());
        assert!(result[0].children.is_none());
    }

    #[test]
    fn missing_proxy_url_on_media_is_populated() {
        let mut e = MessageEmbed::new("rich");
        e.image = Some(EmbedMedia {
            url: Some("https://pbs.twimg.com/media/a.jpg".into()),
            ..Default::default()
        });
        e.thumbnail = Some(EmbedMedia {
            url: Some("https://goulartnogueira.github.io/badui.gif".into()),
            ..Default::default()
        });
        e.video = Some(EmbedMedia {
            url: Some("https://cdn.bsky.app/x/video.mp4".into()),
            ..Default::default()
        });
        let result = normalize_embeds(vec![e], &test_proxy());
        let r = &result[0];
        let image_proxy = r.image.as_ref().unwrap().proxy_url.as_deref().unwrap();
        assert!(image_proxy.starts_with("https://media.example.test/external/"));
        let thumb_proxy = r.thumbnail.as_ref().unwrap().proxy_url.as_deref().unwrap();
        assert!(thumb_proxy.starts_with("https://media.example.test/external/"));
        let video_proxy = r.video.as_ref().unwrap().proxy_url.as_deref().unwrap();
        assert!(video_proxy.starts_with("https://media.example.test/external/"));
    }

    #[test]
    fn existing_proxy_url_is_preserved() {
        let mut e = MessageEmbed::new("rich");
        let existing = "https://media.example.test/external/already/v2/abc".to_owned();
        e.image = Some(EmbedMedia {
            url: Some("https://pbs.twimg.com/media/a.jpg".into()),
            proxy_url: Some(existing.clone()),
            ..Default::default()
        });
        let result = normalize_embeds(vec![e], &test_proxy());
        assert_eq!(
            result[0].image.as_ref().unwrap().proxy_url.as_deref(),
            Some(existing.as_str())
        );
    }

    #[test]
    fn author_and_footer_icon_urls_are_proxied() {
        let mut e = MessageEmbed::new("rich");
        e.author = Some(EmbedAuthor {
            name: "Someone".into(),
            url: None,
            icon_url: Some("https://bsky.social/avatar.png".into()),
            proxy_icon_url: None,
        });
        e.footer = Some(EmbedFooter {
            text: "Footer".into(),
            icon_url: Some("https://bsky.social/footer.png".into()),
            proxy_icon_url: None,
        });
        let result = normalize_embeds(vec![e], &test_proxy());
        let author_proxy = result[0]
            .author
            .as_ref()
            .unwrap()
            .proxy_icon_url
            .as_deref()
            .unwrap();
        assert!(author_proxy.starts_with("https://media.example.test/external/"));
        let footer_proxy = result[0]
            .footer
            .as_ref()
            .unwrap()
            .proxy_icon_url
            .as_deref()
            .unwrap();
        assert!(footer_proxy.starts_with("https://media.example.test/external/"));
    }

    #[test]
    fn already_proxied_url_is_preserved_in_proxy_field() {
        let mut e = MessageEmbed::new("rich");
        e.image = Some(EmbedMedia {
            url: Some("https://media.example.test/avatars/abc.webp".into()),
            ..Default::default()
        });
        let result = normalize_embeds(vec![e], &test_proxy());
        assert_eq!(
            result[0].image.as_ref().unwrap().proxy_url.as_deref(),
            Some("https://media.example.test/avatars/abc.webp")
        );
    }

    #[test]
    fn nested_children_media_is_proxied() {
        let mut child = MessageEmbed::new("rich");
        child.image = Some(EmbedMedia {
            url: Some("https://raw.githubusercontent.com/fluxer/logo.png".into()),
            ..Default::default()
        });
        let mut e = MessageEmbed::new("rich");
        e.children = Some(vec![child]);
        let result = normalize_embeds(vec![e], &test_proxy());
        let proxy = result[0].children.as_ref().unwrap()[0]
            .image
            .as_ref()
            .unwrap()
            .proxy_url
            .as_deref()
            .unwrap();
        assert!(proxy.starts_with("https://media.example.test/external/"));
    }
}
