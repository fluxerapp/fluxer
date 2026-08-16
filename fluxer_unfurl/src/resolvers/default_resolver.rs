// SPDX-License-Identifier: AGPL-3.0-or-later

use super::default_helpers::{
    MediaWikiArticle, build_image_candidates, fetch_mediawiki_article, fetch_oembed_data,
    parse_hex_color, resolve_media_url,
};
use super::media::{
    build_direct_media_embed, media_kind_from_content_type, media_kind_from_response,
};
use super::{ResolveContext, Resolver, ResolverResult};
use crate::activity_pub;
use crate::charset;
use crate::direct_media::MediaKind;
use crate::html_parser::{self, OgMetadata, TwitterCardMetadata};
use crate::http_fetch;
use crate::media_proxy::embed_media_flags;
use crate::oembed;
use crate::sanitizer;
use crate::text_limits;
use crate::types::{EmbedAuthor, EmbedMedia, EmbedProvider, MessageEmbed};
use std::future::Future;
use std::pin::Pin;
use url::Url;

const MAX_GALLERY_IMAGES: usize = 10;

const PLAYER_HOSTS: [&str; 12] = [
    "youtube.com",
    "youtube-nocookie.com",
    "youtu.be",
    "vimeo.com",
    "soundcloud.com",
    "twitch.tv",
    "streamable.com",
    "steampowered.com",
    "imgur.com",
    "sketchfab.com",
    "twitter.com",
    "x.com",
];

pub struct DefaultResolver;

impl Resolver for DefaultResolver {
    fn matches(&self, _url: &Url) -> bool {
        true
    }

    fn resolve<'a>(
        &'a self,
        ctx: &'a ResolveContext<'_>,
    ) -> Pin<Box<dyn Future<Output = anyhow::Result<ResolverResult>> + Send + 'a>> {
        Box::pin(resolve_html(ctx))
    }
}

async fn resolve_html(ctx: &ResolveContext<'_>) -> anyhow::Result<ResolverResult> {
    let result = http_fetch::fetch_url_maybe_body(
        &ctx.http_client,
        ctx.url.as_str(),
        http_fetch::DEFAULT_HTML_MAX_BYTES,
        http_fetch::DEFAULT_TIMEOUT,
        |head| {
            if head.status != 200 {
                return false;
            }
            let Some(final_url) = Url::parse(&head.final_url).ok() else {
                return true;
            };
            media_kind_from_response(head.content_type.as_deref().unwrap_or(""), &final_url, &[])
                .is_none()
        },
    )
    .await?;

    if result.status != 200 {
        return Ok(ResolverResult { embeds: vec![] });
    }
    let final_url = Url::parse(&result.final_url).unwrap_or_else(|_| ctx.url.clone());
    let content_type = result.content_type.as_deref().unwrap_or("");
    let bytes = result.bytes.as_deref().unwrap_or_default();
    let normalized_content_type = content_type.to_ascii_lowercase();
    if !normalized_content_type.starts_with("text/html") {
        if let Some(kind) = media_kind_from_response(content_type, &final_url, bytes) {
            let embed =
                build_direct_media_embed(ctx.media_proxy, &final_url, ctx.nsfw_mode, kind).await?;
            return Ok(ResolverResult {
                embeds: vec![embed],
            });
        }
        return Ok(ResolverResult { embeds: vec![] });
    }

    let html = charset::decode_body(
        bytes,
        result
            .headers
            .get(reqwest::header::CONTENT_TYPE)
            .and_then(|value| value.to_str().ok()),
    );

    let final_ctx = ResolveContext {
        url: final_url.clone(),
        original_url: ctx.original_url.clone(),
        http_client: ctx.http_client.clone(),
        nsfw_mode: ctx.nsfw_mode,
        media_proxy: ctx.media_proxy,
        static_cdn_endpoint: ctx.static_cdn_endpoint,
        youtube_api_key: ctx.youtube_api_key.clone(),
        klipy_api_key: ctx.klipy_api_key.clone(),
    };

    if let Some(embeds) = activity_pub::try_resolve(
        &ctx.http_client,
        ctx.media_proxy,
        ctx.nsfw_mode,
        &final_url,
        &html,
    )
    .await
        && !embeds.is_empty()
    {
        return Ok(ResolverResult { embeds });
    }

    if urls_differ(&ctx.url, &final_url)
        && let Some(embeds) = try_canonical_activity_pub(ctx, &final_url, &html).await
        && !embeds.is_empty()
    {
        return Ok(ResolverResult { embeds });
    }

    let og = html_parser::parse_opengraph(&html);
    let twitter_card = html_parser::parse_twitter_card(&html);

    let mediawiki_article = fetch_mediawiki_article(&final_ctx, &html).await;
    let oembed_data = fetch_oembed_data(&final_ctx, &html).await;

    let mut image_media = resolve_image_media(&final_ctx, &og, oembed_data.as_ref()).await;
    if let Some(first) = image_media.first_mut()
        && let Some(ref alt) = og.image_alt
    {
        first.description = Some(text_limits::truncate(alt, 4096));
    }
    let player_url = twitter_card
        .classifying_card
        .as_deref()
        .is_some_and(|card| card == "player")
        .then(|| {
            twitter_card
                .player
                .as_deref()
                .and_then(|url| resolve_media_url(&final_ctx.url, url))
        })
        .flatten()
        .filter(|url| is_allowlisted_player_url(url));
    let open_graph_video_url = og
        .og_type
        .as_deref()
        .is_some_and(|og_type| og_type.starts_with("video."))
        .then(|| {
            og.video_primary
                .as_deref()
                .and_then(|url| resolve_media_url(&final_ctx.url, url))
        })
        .flatten();
    let open_graph_video_allowlisted = open_graph_video_url
        .as_deref()
        .is_some_and(is_allowlisted_player_url);
    let open_graph_video_media = if open_graph_video_allowlisted {
        open_graph_video_url.clone().map(player_embed_media)
    } else {
        resolve_media_with_metadata(
            &final_ctx,
            open_graph_video_url.as_deref(),
            MediaKind::Video,
        )
        .await
    };
    let open_graph_video_accepted = open_graph_video_media.is_some();
    let video_media = player_url
        .clone()
        .map(player_embed_media)
        .or(open_graph_video_media);
    let audio_url = og
        .audio
        .as_deref()
        .and_then(|url| resolve_media_url(&final_ctx.url, url));
    let audio_media =
        resolve_media_with_metadata(&final_ctx, audio_url.as_deref(), MediaKind::Audio).await;

    let embed_type = classify_embed_type(
        &twitter_card,
        mediawiki_article.is_some(),
        oembed_data.as_ref(),
        player_url.is_some(),
        open_graph_video_accepted,
    );

    if embed_type == "link"
        && !og.textual_keys_present
        && og.title.is_none()
        && og.description.is_none()
    {
        return Ok(ResolverResult { embeds: vec![] });
    }

    let embeds = build_embeds(ResolvedPageContent {
        page_url: &final_ctx.url,
        embed_type,
        og: &og,
        mediawiki_article: mediawiki_article.as_ref(),
        oembed: oembed_data.as_ref(),
        image_media,
        video_media,
        audio_media,
    });

    Ok(ResolverResult { embeds })
}

fn classify_embed_type(
    twitter_card: &TwitterCardMetadata,
    mediawiki_article: bool,
    oembed: Option<&oembed::OEmbedResponse>,
    twitter_player_allowlisted: bool,
    open_graph_video: bool,
) -> &'static str {
    if mediawiki_article {
        return "article";
    }
    match twitter_card.classifying_card.as_deref() {
        Some("summary_large_image") => return "article",
        Some("photo") => return "image",
        Some("player") if twitter_player_allowlisted => {
            return "video";
        }
        _ => {}
    }
    if open_graph_video {
        return "video";
    }
    if oembed
        .and_then(|o| optional_oembed_string(o.oembed_type.as_deref()))
        .is_some_and(|value| value.eq_ignore_ascii_case("photo"))
    {
        return "image";
    }
    "link"
}

fn is_allowlisted_player_url(value: &str) -> bool {
    let Ok(url) = Url::parse(value) else {
        return false;
    };
    let Some(host) = url.host_str() else {
        return false;
    };
    let host = host.trim_end_matches('.').to_ascii_lowercase();
    PLAYER_HOSTS
        .iter()
        .any(|allowed| host == *allowed || host.ends_with(&format!(".{allowed}")))
}

struct ResolvedPageContent<'a> {
    page_url: &'a Url,
    embed_type: &'a str,
    og: &'a OgMetadata,
    mediawiki_article: Option<&'a MediaWikiArticle>,
    oembed: Option<&'a oembed::OEmbedResponse>,
    image_media: Vec<EmbedMedia>,
    video_media: Option<EmbedMedia>,
    audio_media: Option<EmbedMedia>,
}

fn build_embeds(content: ResolvedPageContent<'_>) -> Vec<MessageEmbed> {
    let ResolvedPageContent {
        page_url,
        embed_type,
        og,
        mediawiki_article,
        oembed,
        image_media,
        video_media,
        audio_media,
    } = content;
    let title = mediawiki_article
        .and_then(|article| article.title.as_deref())
        .or(og.title.as_deref())
        .or(oembed.and_then(|o| optional_oembed_string(o.title.as_deref())));
    let description = mediawiki_article
        .and_then(|article| article.description.as_deref())
        .or(og.description.as_deref());
    let site_name = oembed
        .and_then(|o| optional_oembed_string(o.provider_name.as_deref()))
        .or(og.site_name.as_deref());
    let color = og.theme_color.as_deref().and_then(parse_hex_color);
    let provider_url = oembed
        .and_then(|o| optional_absolute_url(o.provider_url.as_deref()))
        .unwrap_or_else(|| page_url.origin().ascii_serialization());
    let oembed_html = oembed.and_then(|o| o.html.as_deref()).and_then(|html| {
        sanitizer::sanitize_oembed_html_with_context(html, page_url, Some(&provider_url))
    });
    let embed_url = page_url.to_string();

    let mut embed = MessageEmbed::new(embed_type);
    embed.url = Some(embed_url.clone());
    if let Some(t) = title {
        embed.title = Some(text_limits::truncate(t, 70));
    }
    if let Some(d) = description {
        embed.description = Some(text_limits::truncate(d, 350));
    }
    embed.color = color;
    set_author_and_provider(&mut embed, oembed, site_name, &provider_url);
    set_media(
        &mut embed,
        oembed,
        &oembed_html,
        image_media.as_slice(),
        video_media,
        audio_media,
    );
    if !carries_content(&embed) {
        return Vec::new();
    }
    let mut embeds = vec![embed];
    append_gallery_images(&mut embeds, image_media.as_slice(), &embed_url);
    embeds
}

fn carries_content(embed: &MessageEmbed) -> bool {
    embed.title.is_some()
        || embed.description.is_some()
        || embed.author.is_some()
        || embed.provider.is_some()
        || embed.thumbnail.is_some()
        || embed.image.is_some()
        || embed.video.is_some()
        || embed.audio.is_some()
        || embed.html.is_some()
}

fn set_author_and_provider(
    embed: &mut MessageEmbed,
    oembed: Option<&oembed::OEmbedResponse>,
    site_name: Option<&str>,
    provider_url: &str,
) {
    if let Some(name) = oembed
        .and_then(|o| optional_oembed_string(o.author_name.as_deref()))
        .filter(|n| !n.trim().is_empty())
    {
        embed.author = Some(EmbedAuthor {
            name: text_limits::truncate(name.trim(), text_limits::AUTHOR_NAME_MAX),
            url: oembed.and_then(|o| optional_absolute_url(o.author_url.as_deref())),
            ..Default::default()
        });
    }
    if let Some(name) = site_name {
        embed.provider = Some(EmbedProvider {
            name: Some(text_limits::truncate(name, text_limits::PROVIDER_NAME_MAX)),
            url: Some(provider_url.to_owned()),
        });
    }
}

fn set_media(
    embed: &mut MessageEmbed,
    oembed: Option<&oembed::OEmbedResponse>,
    oembed_html: &Option<String>,
    image_media: &[EmbedMedia],
    video_media: Option<EmbedMedia>,
    audio_media: Option<EmbedMedia>,
) {
    if let Some(primary) = image_media.first().cloned() {
        embed.thumbnail = Some(primary);
    }
    embed.video = video_media;
    embed.audio = audio_media;
    if let Some(html) = oembed_html {
        embed.html = Some(html.clone());
        embed.html_width = oembed
            .and_then(|o| o.width.as_ref())
            .and_then(oembed::parse_dimension);
        embed.html_height = oembed
            .and_then(|o| o.height.as_ref())
            .and_then(oembed::parse_dimension);
    }
}

async fn resolve_image_media(
    ctx: &ResolveContext<'_>,
    og: &OgMetadata,
    oembed: Option<&oembed::OEmbedResponse>,
) -> Vec<EmbedMedia> {
    let mut resolved = Vec::new();
    for candidate in build_image_candidates(&ctx.url, og, oembed)
        .into_iter()
        .take(MAX_GALLERY_IMAGES)
    {
        if let Some(media) = resolve_media_with_metadata_and_size(
            ctx,
            &candidate.url,
            candidate.width,
            candidate.height,
            MediaKind::Image,
        )
        .await
        {
            resolved.push(media);
        }
    }
    resolved
}

fn player_embed_media(url: String) -> EmbedMedia {
    EmbedMedia {
        url: Some(url),
        ..Default::default()
    }
}

async fn resolve_media_with_metadata(
    ctx: &ResolveContext<'_>,
    url: Option<&str>,
    expected_kind: MediaKind,
) -> Option<EmbedMedia> {
    resolve_media_with_metadata_and_size(ctx, url?, None, None, expected_kind).await
}

async fn resolve_media_with_metadata_and_size(
    ctx: &ResolveContext<'_>,
    url: &str,
    width: Option<u32>,
    height: Option<u32>,
    expected_kind: MediaKind,
) -> Option<EmbedMedia> {
    let nsfw_str = crate::media_proxy::MediaProxyClient::nsfw_mode_str(ctx.nsfw_mode);
    match ctx.media_proxy.get_metadata(url, nsfw_str).await {
        Ok(m) if media_kind_from_content_type(&m.content_type) != Some(expected_kind) => {
            tracing::warn!(
                url,
                content_type = %m.content_type,
                "default embed media metadata returned an unexpected media kind"
            );
            None
        }
        Ok(m) => Some(EmbedMedia {
            url: Some(url.to_owned()),
            width: width.or(m.width),
            height: height.or(m.height),
            content_type: Some(m.content_type.clone()),
            content_hash: Some(m.content_hash.clone()),
            placeholder: m.placeholder.clone(),
            duration: m.duration.map(|duration| duration as u32),
            flags: embed_media_flags(&m),
            ..Default::default()
        }),
        Err(err) => {
            tracing::warn!(error = %err, url, "failed to resolve default embed media metadata");
            None
        }
    }
}

fn append_gallery_images(
    embeds: &mut Vec<MessageEmbed>,
    image_media: &[EmbedMedia],
    embed_url: &str,
) {
    for image in image_media.iter().skip(1).take(MAX_GALLERY_IMAGES - 1) {
        let mut extra = MessageEmbed::new("rich");
        extra.url = Some(embed_url.to_owned());
        extra.image = Some(image.clone());
        embeds.push(extra);
    }
}

fn optional_oembed_string(value: Option<&str>) -> Option<&str> {
    value.map(str::trim).filter(|value| !value.is_empty())
}

fn optional_absolute_url(value: Option<&str>) -> Option<String> {
    Url::parse(optional_oembed_string(value)?)
        .ok()
        .map(|url| url.to_string())
}

fn urls_differ(a: &Url, b: &Url) -> bool {
    let norm = |u: &Url| u.as_str().trim_end_matches('/').to_owned();
    norm(a) != norm(b)
}

async fn try_canonical_activity_pub(
    ctx: &ResolveContext<'_>,
    final_url: &Url,
    html: &str,
) -> Option<Vec<MessageEmbed>> {
    let canonical = html_parser::find_canonical_url(html, final_url)?;
    let canonical_url = Url::parse(&canonical).ok()?;
    if !urls_differ(&canonical_url, final_url) {
        return None;
    }
    let ap_post =
        crate::activity_pub::fetcher::fetch_activity_pub(&ctx.http_client, &canonical).await?;
    let post_url = ap_post
        .url
        .as_deref()
        .and_then(|u| Url::parse(u).ok())
        .unwrap_or_else(|| canonical_url.clone());
    let context = build_canonical_ap_context(&ctx.http_client, &post_url).await;
    let author_actor = fetch_canonical_author(&ctx.http_client, &ap_post).await;
    let embeds = crate::activity_pub::formatter::format_activity_pub_post(
        &ap_post,
        &post_url,
        &context,
        ctx.media_proxy,
        ctx.nsfw_mode,
        crate::activity_pub::formatter::ActivityPubFormatOptions {
            author_actor: author_actor.as_ref(),
            quote_child: None,
            is_nested: false,
        },
    )
    .await;
    if embeds.is_empty() {
        None
    } else {
        Some(embeds)
    }
}

async fn build_canonical_ap_context(
    client: &reqwest::Client,
    url: &Url,
) -> crate::activity_pub::types::ActivityPubContext {
    let instance = crate::activity_pub::fetcher::fetch_instance_info(client, url).await;
    let clean_host = url
        .host_str()
        .unwrap_or_default()
        .trim_start_matches("www.")
        .trim_start_matches("social.")
        .trim_start_matches("mstdn.")
        .to_owned();
    crate::activity_pub::types::ActivityPubContext {
        server_domain: instance
            .as_ref()
            .and_then(|i| i.domain.clone())
            .unwrap_or_else(|| clean_host.clone()),
        server_title: instance
            .as_ref()
            .and_then(|i| i.title.clone())
            .unwrap_or_else(|| format!("{clean_host} Mastodon")),
        server_icon: instance.and_then(|i| i.thumbnail_url),
        in_reply_to: None,
    }
}

async fn fetch_canonical_author(
    client: &reqwest::Client,
    post: &crate::activity_pub::types::ActivityPubPost,
) -> Option<crate::activity_pub::types::ActivityPubActor> {
    let actor_url = post.attributed_to.as_ref()?.as_str()?;
    crate::activity_pub::fetcher::fetch_activity_pub_actor(client, actor_url).await
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::direct_media::MediaKind;

    fn url(value: &str) -> Url {
        Url::parse(value).unwrap()
    }

    #[test]
    fn direct_media_uses_header_mime_before_extension() {
        assert_eq!(
            media_kind_from_response("image/png", &url("https://e.com/file.mp4"), &[]),
            Some(MediaKind::Image)
        );
    }

    #[test]
    fn direct_media_uses_extension_for_binary_headers() {
        assert_eq!(
            media_kind_from_response("binary/octet-stream", &url("https://e.com/file.weba"), &[]),
            Some(MediaKind::Audio)
        );
    }

    #[test]
    fn direct_media_rejects_extension_when_header_is_non_binary() {
        assert_eq!(
            media_kind_from_response("text/html", &url("https://e.com/file.mp4"), &[]),
            None
        );
    }

    #[test]
    fn application_ogg_uses_extension_or_audio_default() {
        assert_eq!(
            media_kind_from_response("application/ogg", &url("https://e.com/file.ogv"), &[]),
            Some(MediaKind::Video)
        );
        assert_eq!(
            media_kind_from_response("application/ogg", &url("https://e.com/file"), &[]),
            Some(MediaKind::Audio)
        );
    }

    #[test]
    fn direct_media_falls_back_to_magic_bytes_without_header_or_extension() {
        let png = [
            0x89, b'P', b'N', b'G', 0x0d, 0x0a, 0x1a, 0x0a, 0, 0, 0, 0x0d, b'I', b'H', b'D', b'R',
        ];
        assert_eq!(
            media_kind_from_response("", &url("https://e.com/file"), &png),
            Some(MediaKind::Image)
        );
    }

    #[test]
    fn optional_oembed_string_trims_and_filters_empty() {
        assert_eq!(optional_oembed_string(Some("  hello  ")), Some("hello"));
        assert_eq!(optional_oembed_string(Some("  ")), None);
        assert_eq!(optional_oembed_string(Some("")), None);
        assert_eq!(optional_oembed_string(None), None);
    }

    #[test]
    fn optional_absolute_url_accepts_valid() {
        assert_eq!(
            optional_absolute_url(Some("https://e.com/page")),
            Some("https://e.com/page".to_owned())
        );
    }

    #[test]
    fn optional_absolute_url_rejects_relative() {
        assert!(optional_absolute_url(Some("/relative")).is_none());
    }

    #[test]
    fn optional_absolute_url_rejects_empty() {
        assert!(optional_absolute_url(Some("")).is_none());
        assert!(optional_absolute_url(None).is_none());
    }

    use crate::resolvers::default_helpers::parse_hex_color;

    #[test]
    fn parse_hex_color_six_digit() {
        assert_eq!(parse_hex_color("#FF0000"), Some(0xFF0000));
        assert_eq!(parse_hex_color("#ffffff"), Some(0xFFFFFF));
        assert_eq!(parse_hex_color("#000000"), Some(0x000000));
    }

    #[test]
    fn parse_hex_color_three_digit() {
        assert_eq!(parse_hex_color("#F00"), Some(0xFF0000));
        assert_eq!(parse_hex_color("#fff"), Some(0xFFFFFF));
    }

    #[test]
    fn parse_hex_color_invalid() {
        assert!(parse_hex_color("").is_none());
        assert!(parse_hex_color("FF0000").is_none());
        assert!(parse_hex_color("#GG0000").is_none());
        assert!(parse_hex_color("#12345").is_none());
        assert!(parse_hex_color("#1234567").is_none());
    }

    #[test]
    fn parse_hex_color_with_whitespace() {
        assert_eq!(parse_hex_color("  #FF0000  "), Some(0xFF0000));
    }

    #[test]
    fn urls_differ_ignores_trailing_slash() {
        assert!(!urls_differ(
            &url("https://e.com/page"),
            &url("https://e.com/page/")
        ));
        assert!(!urls_differ(
            &url("https://e.com/page/"),
            &url("https://e.com/page")
        ));
    }

    #[test]
    fn urls_differ_detects_different_paths() {
        assert!(urls_differ(
            &url("https://e.com/a"),
            &url("https://e.com/b")
        ));
    }

    #[test]
    fn urls_differ_detects_different_hosts() {
        assert!(urls_differ(
            &url("https://a.com/page"),
            &url("https://b.com/page")
        ));
    }

    #[test]
    fn urls_differ_same_url() {
        assert!(!urls_differ(
            &url("https://e.com/page"),
            &url("https://e.com/page")
        ));
    }

    fn classify(html: &str) -> &'static str {
        classify_with(html, false, None, false)
    }

    fn classify_with(
        html: &str,
        mediawiki_article: bool,
        oembed_type: Option<&str>,
        video_validated: bool,
    ) -> &'static str {
        let oembed = oembed_type.map(|value| oembed::OEmbedResponse {
            oembed_type: Some(value.to_owned()),
            ..Default::default()
        });
        let page = url("https://example.com/page");
        let og = html_parser::parse_opengraph(html);
        let twitter_card = html_parser::parse_twitter_card(html);
        let player_url = twitter_card
            .classifying_card
            .as_deref()
            .is_some_and(|card| card == "player")
            .then(|| {
                twitter_card
                    .player
                    .as_deref()
                    .and_then(|value| resolve_media_url(&page, value))
            })
            .flatten()
            .filter(|value| is_allowlisted_player_url(value));
        let open_graph_video_url = og
            .og_type
            .as_deref()
            .is_some_and(|og_type| og_type.starts_with("video."))
            .then(|| {
                og.video_primary
                    .as_deref()
                    .and_then(|value| resolve_media_url(&page, value))
            })
            .flatten();
        let open_graph_video = open_graph_video_url.is_some()
            && (open_graph_video_url
                .as_deref()
                .is_some_and(is_allowlisted_player_url)
                || video_validated);
        classify_embed_type(
            &twitter_card,
            mediawiki_article,
            oembed.as_ref(),
            player_url.is_some(),
            open_graph_video,
        )
    }

    #[test]
    fn classify_defaults_to_link() {
        assert_eq!(
            classify(r#"<head><meta property="og:title" content="T"></head>"#),
            "link"
        );
    }

    #[test]
    fn classify_oembed_rich_html_no_longer_forces_rich() {
        assert_eq!(
            classify_with(
                r#"<head><meta property="og:title" content="T"></head>"#,
                false,
                Some("rich"),
                false
            ),
            "link"
        );
        for oembed_type in ["video", "link", "audio"] {
            assert_eq!(
                classify_with(
                    r#"<head><meta property="og:title" content="T"></head>"#,
                    false,
                    Some(oembed_type),
                    false
                ),
                "link",
                "oembed {oembed_type} must not change the type"
            );
        }
    }

    #[test]
    fn classify_oembed_photo_is_image() {
        assert_eq!(
            classify_with(
                r#"<head><meta property="og:title" content="T"></head>"#,
                false,
                Some("photo"),
                false
            ),
            "image"
        );
    }

    #[test]
    fn classify_twitter_card_summary_large_image_is_article() {
        assert_eq!(
            classify(r#"<head><meta name="twitter:card" content="summary_large_image"></head>"#),
            "article"
        );
    }

    #[test]
    fn classify_twitter_card_photo_is_image() {
        assert_eq!(
            classify(r#"<head><meta name="twitter:card" content="photo"></head>"#),
            "image"
        );
    }

    #[test]
    fn classify_twitter_card_summary_is_a_no_op() {
        assert_eq!(
            classify(r#"<head><meta name="twitter:card" content="summary"></head>"#),
            "link"
        );
    }

    #[test]
    fn classify_twitter_player_requires_allowlisted_host() {
        let allowed = r#"<head>
            <meta name="twitter:card" content="player">
            <meta name="twitter:player" content="https://player.vimeo.com/video/1">
        </head>"#;
        assert_eq!(classify(allowed), "video");

        let denied = r#"<head>
            <meta name="twitter:card" content="player">
            <meta name="twitter:player" content="https://embed.ted.com/talks/1">
        </head>"#;
        assert_eq!(classify(denied), "link");
    }

    #[test]
    fn classify_open_graph_video_requires_video_og_type() {
        let html = r#"<head>
            <meta property="og:type" content="video.other">
            <meta property="og:video" content="https://www.youtube.com/embed/x">
        </head>"#;
        assert_eq!(classify(html), "video");

        let without_type = r#"<head>
            <meta property="og:video" content="https://www.youtube.com/embed/x">
        </head>"#;
        assert_eq!(classify(without_type), "link");

        for og_type in ["music.song", "article", "website"] {
            let html = format!(
                r#"<head>
                    <meta property="og:type" content="{og_type}">
                    <meta property="og:video" content="https://www.youtube.com/embed/x">
                </head>"#
            );
            assert_eq!(classify(&html), "link", "{og_type} must not be video");
        }
    }

    #[test]
    fn classify_open_graph_video_secure_url_alone_does_not_qualify() {
        let html = r#"<head>
            <meta property="og:type" content="video.other">
            <meta property="og:video:secure_url" content="https://www.youtube.com/embed/x">
        </head>"#;
        assert_eq!(classify(html), "link");
    }

    #[test]
    fn classify_open_graph_direct_video_requires_validation() {
        let html = r#"<head>
            <meta property="og:type" content="video.other">
            <meta property="og:video" content="https://e.com/clip.mp4">
        </head>"#;
        assert_eq!(classify_with(html, false, None, false), "link");
        assert_eq!(classify_with(html, false, None, true), "video");
    }

    #[test]
    fn classify_mediawiki_beats_every_twitter_card() {
        for card in ["summary_large_image", "photo", "player"] {
            let html = format!(
                r#"<head>
                    <meta name="twitter:card" content="{card}">
                    <meta name="twitter:player" content="https://player.twitch.tv/?video=1">
                </head>"#
            );
            assert_eq!(classify_with(&html, true, None, false), "article");
        }
    }

    #[test]
    fn classify_twitter_card_beats_open_graph_video() {
        let html = r#"<head>
            <meta name="twitter:card" content="photo">
            <meta property="og:type" content="video.other">
            <meta property="og:video" content="https://www.youtube.com/embed/x">
        </head>"#;
        assert_eq!(classify(html), "image");
    }

    #[test]
    fn classify_open_graph_video_beats_oembed_photo() {
        let html = r#"<head>
            <meta property="og:type" content="video.other">
            <meta property="og:video" content="https://www.youtube.com/embed/x">
        </head>"#;
        assert_eq!(classify_with(html, false, Some("photo"), false), "video");
    }

    #[test]
    fn player_allowlist_accepts_verified_hosts() {
        for value in [
            "https://www.youtube.com/embed/x",
            "https://www.youtube-nocookie.com/embed/x",
            "https://youtu.be/x",
            "https://player.vimeo.com/video/1",
            "https://w.soundcloud.com/player/?url=x",
            "https://player.twitch.tv/?video=1",
        ] {
            assert!(is_allowlisted_player_url(value), "{value} must be allowed");
        }
    }

    #[test]
    fn classify_twitter_player_resolves_relative_references_first() {
        let html = r#"<head>
            <meta name="twitter:card" content="player">
            <meta name="twitter:player" content="//player.vimeo.com/video/1">
        </head>"#;
        assert_eq!(classify(html), "video");
    }

    fn build(embed_type: &str, og: &OgMetadata) -> Vec<MessageEmbed> {
        let page_url = url("https://example.com/page");
        build_embeds(ResolvedPageContent {
            page_url: &page_url,
            embed_type,
            og,
            mediawiki_article: None,
            oembed: None,
            image_media: Vec::new(),
            video_media: None,
            audio_media: None,
        })
    }

    #[test]
    fn drops_a_classified_embed_that_carries_no_content() {
        for embed_type in ["article", "image", "link"] {
            assert!(
                build(embed_type, &OgMetadata::default()).is_empty(),
                "{embed_type}"
            );
        }
    }

    #[test]
    fn keeps_a_classified_embed_whose_only_content_is_a_site_name() {
        let og = OgMetadata {
            site_name: Some("Example".to_owned()),
            ..Default::default()
        };
        let embeds = build("link", &og);
        assert_eq!(embeds.len(), 1);
        assert!(embeds[0].provider.is_some());
    }

    #[test]
    fn prefers_mediawiki_text_over_open_graph_text() {
        let page_url = url("https://w.example/wiki/Rust");
        let og = OgMetadata {
            title: Some("T".to_owned()),
            description: Some("D".to_owned()),
            ..Default::default()
        };
        let article = MediaWikiArticle {
            title: Some("Probe Page".to_owned()),
            description: Some("Extract text.".to_owned()),
        };
        let embeds = build_embeds(ResolvedPageContent {
            page_url: &page_url,
            embed_type: "article",
            og: &og,
            mediawiki_article: Some(&article),
            oembed: None,
            image_media: Vec::new(),
            video_media: None,
            audio_media: None,
        });
        assert_eq!(embeds.len(), 1);
        assert_eq!(embeds[0].title.as_deref(), Some("Probe Page"));
        assert_eq!(embeds[0].description.as_deref(), Some("Extract text."));
    }

    #[test]
    fn player_allowlist_rejects_other_hosts() {
        for value in [
            "https://embed.ted.com/talks/1",
            "https://example.com/player",
            "https://notyoutube.com/embed/x",
            "//www.youtube.com/embed/x",
            "not a url",
        ] {
            assert!(!is_allowlisted_player_url(value), "{value} must be denied");
        }
    }
}
