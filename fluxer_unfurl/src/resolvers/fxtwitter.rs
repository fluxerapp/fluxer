// SPDX-License-Identifier: AGPL-3.0-or-later

use super::{ResolveContext, Resolver, ResolverResult};
use crate::http_fetch;
use crate::media_proxy::embed_media_flags;
use crate::text_limits;
use crate::types::{EmbedAuthor, EmbedFooter, EmbedMedia, MessageEmbed};
use serde::{Deserialize, Deserializer};
use serde_json::Value;
use std::future::Future;
use std::pin::Pin;
use std::time::Duration;
use url::Url;

const FXTWITTER_COLOR: u32 = 0x6364FF;
const FXTWITTER_FOOTER_TEXT: &str = "FxTwitter";
const FXTWITTER_FOOTER_ICON: &str = "https://assets.fxembed.com/logos/fxtwitter64.png";
const FXTWITTER_API_BASE: &str = "https://api.fxtwitter.com";
const API_MAX_BYTES: usize = 512 * 1024;
const API_TIMEOUT: Duration = Duration::from_secs(8);
const MAX_GALLERY_IMAGES: usize = 10;

const FXTWITTER_HOSTS: &[&str] = &["fxtwitter.com", "fixupx.com", "twittpr.com", "xfixup.com"];
const DIRECT_MEDIA_EXTENSIONS: &[&str] = &["mp4", "png", "jpg", "jpeg", "gif", "gifv"];

const EMOJI_REPLY: &str = "\u{1F4AC}";
const EMOJI_RETWEET: &str = "\u{1F501}";
const EMOJI_LIKE: &str = "\u{2764}\u{FE0F}";
const EMOJI_VIEWS: &str = "\u{1F441}\u{FE0F}";
const STAT_SEP: &str = "\u{2002}";
const QUOTE_EMPTY_LINE: &str = "> \u{FE00}";

pub struct FxTwitterResolver;

impl Resolver for FxTwitterResolver {
    fn matches(&self, url: &Url) -> bool {
        is_fxtwitter_host(url.host_str())
    }

    fn resolve<'a>(
        &'a self,
        ctx: &'a ResolveContext<'_>,
    ) -> Pin<Box<dyn Future<Output = anyhow::Result<ResolverResult>> + Send + 'a>> {
        Box::pin(async move {
            let Some(request) = parse_status_request(&ctx.original_url) else {
                return Ok(ResolverResult { embeds: vec![] });
            };
            resolve_status(ctx, &request).await
        })
    }
}

fn is_fxtwitter_host(host: Option<&str>) -> bool {
    fxtwitter_host_prefixes(host).is_some()
}

fn fxtwitter_host_prefixes(host: Option<&str>) -> Option<Vec<String>> {
    let host = host?.to_ascii_lowercase();
    let domain = FXTWITTER_HOSTS
        .iter()
        .find(|domain| host == **domain || host.ends_with(&format!(".{}", **domain)))?;
    if host == **domain {
        return Some(Vec::new());
    }
    let prefix = host.strip_suffix(*domain)?.trim_end_matches('.');
    Some(
        prefix
            .split('.')
            .filter(|label| !label.is_empty())
            .map(str::to_owned)
            .collect(),
    )
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
struct FxUrlFlags {
    direct: bool,
    text_only: bool,
    gallery: bool,
    force_mosaic: bool,
    force_instant_view: bool,
    old_embed: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum MediaSelectorKind {
    Photo,
    Video,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct FxStatusRequest {
    screen_name: String,
    status_id: String,
    language: Option<String>,
    media_index: Option<usize>,
    media_kind: Option<MediaSelectorKind>,
    direct_media_name: Option<String>,
    flags: FxUrlFlags,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ParsedStatusId {
    id: String,
    direct_media_suffix: bool,
    direct_media_name: Option<String>,
}

fn parse_status_request(url: &Url) -> Option<FxStatusRequest> {
    let mut flags = flags_from_host(url);
    let mut segments: Vec<&str> = url
        .path()
        .split('/')
        .filter(|segment| !segment.is_empty())
        .collect();

    if matches!(segments.first().copied(), Some("dir" | "dl")) {
        flags.direct = true;
        segments.remove(0);
    }

    let status_idx = segments
        .iter()
        .position(|segment| *segment == "status" || *segment == "statuses")?;
    let parsed_id = parse_status_id_segment(segments.get(status_idx + 1)?)?;
    if parsed_id.direct_media_suffix {
        flags.direct = true;
    }

    let screen_name = if status_idx == 0 {
        "i"
    } else {
        segments.first().copied().unwrap_or("i")
    };
    let mut suffix = &segments[(status_idx + 2)..];
    let mut media_kind = None;
    let mut media_index = None;
    if suffix.len() >= 2
        && let Some(kind) = parse_media_selector_kind(suffix[0])
        && let Some(index) = parse_media_selector_index(suffix[1])
    {
        media_kind = Some(kind);
        media_index = Some(index);
        suffix = &suffix[2..];
    }
    let language = suffix
        .first()
        .filter(|segment| is_language_suffix(segment))
        .map(|segment| (*segment).to_owned());

    let query_name = url
        .query_pairs()
        .find(|(key, _)| key == "name")
        .map(|(_, value)| value.into_owned());

    Some(FxStatusRequest {
        screen_name: screen_name.to_owned(),
        status_id: parsed_id.id,
        language,
        media_index,
        media_kind,
        direct_media_name: query_name.or(parsed_id.direct_media_name),
        flags,
    })
}

#[cfg(test)]
fn parse_status(url: &Url) -> Option<(String, String)> {
    parse_status_request(url).map(|request| (request.screen_name, request.status_id))
}

fn flags_from_host(url: &Url) -> FxUrlFlags {
    let mut flags = FxUrlFlags::default();
    let Some(prefixes) = fxtwitter_host_prefixes(url.host_str()) else {
        return flags;
    };
    for prefix in prefixes {
        match prefix.as_str() {
            "d" | "dl" => flags.direct = true,
            "t" => flags.text_only = true,
            "g" => flags.gallery = true,
            "m" => flags.force_mosaic = true,
            "i" => flags.force_instant_view = true,
            "o" => flags.old_embed = true,
            "www" => {}
            _ => {}
        }
    }
    flags
}

fn parse_status_id_segment(segment: &str) -> Option<ParsedStatusId> {
    let digit_end = segment
        .bytes()
        .take_while(|byte| byte.is_ascii_digit())
        .count();
    if digit_end == 0 {
        return None;
    }

    let id = segment[..digit_end].to_owned();
    let rest = &segment[digit_end..];
    if rest.is_empty() {
        return Some(ParsedStatusId {
            id,
            direct_media_suffix: false,
            direct_media_name: None,
        });
    }

    if let Some(name) = rest.strip_prefix(':') {
        return Some(ParsedStatusId {
            id,
            direct_media_suffix: false,
            direct_media_name: (!name.is_empty()).then(|| name.to_owned()),
        });
    }

    let ext_and_name = rest.strip_prefix('.')?;
    let (extension, name) = ext_and_name
        .split_once(':')
        .map_or((ext_and_name, None), |(extension, name)| {
            (extension, (!name.is_empty()).then_some(name))
        });
    if !DIRECT_MEDIA_EXTENSIONS
        .iter()
        .any(|known| extension.eq_ignore_ascii_case(known))
    {
        return None;
    }

    Some(ParsedStatusId {
        id,
        direct_media_suffix: true,
        direct_media_name: name.map(str::to_owned),
    })
}

fn parse_media_selector_kind(segment: &str) -> Option<MediaSelectorKind> {
    match segment {
        "photo" | "photos" => Some(MediaSelectorKind::Photo),
        "video" | "videos" => Some(MediaSelectorKind::Video),
        _ => None,
    }
}

fn parse_media_selector_index(segment: &str) -> Option<usize> {
    let index = segment.parse::<usize>().ok()?;
    (1..=4).contains(&index).then_some(index - 1)
}

fn is_language_suffix(segment: &str) -> bool {
    let mut parts = segment.split('-');
    let Some(language) = parts.next() else {
        return false;
    };
    if !(2..=3).contains(&language.len())
        || !language.bytes().all(|byte| byte.is_ascii_alphabetic())
    {
        return false;
    }
    parts.all(|part| {
        (2..=8).contains(&part.len())
            && part
                .bytes()
                .all(|byte| byte.is_ascii_alphanumeric() || byte == b'_')
    })
}

#[derive(Debug, Deserialize)]
struct FxResponse {
    tweet: Option<FxTweet>,
}

#[derive(Debug, Deserialize)]
struct FxTweet {
    id: Option<String>,
    text: Option<String>,
    created_timestamp: Option<i64>,
    author: Option<FxAuthor>,
    replies: Option<u64>,
    #[serde(alias = "reposts")]
    retweets: Option<u64>,
    likes: Option<u64>,
    views: Option<u64>,
    quote: Option<Box<FxQuote>>,
    media: Option<FxMedia>,
    translation: Option<FxTranslation>,
}

#[derive(Debug, Deserialize)]
struct FxAuthor {
    screen_name: Option<String>,
    name: Option<String>,
    avatar_url: Option<String>,
}

#[derive(Debug, Deserialize)]
struct FxQuote {
    url: Option<String>,
    text: Option<String>,
    author: Option<FxAuthor>,
    media: Option<FxMedia>,
}

#[derive(Debug, Deserialize)]
struct FxTranslation {
    text: Option<String>,
    source_lang: Option<String>,
    target_lang: Option<String>,
}

#[derive(Debug, Default, Deserialize)]
struct FxMedia {
    all: Option<Vec<FxMediaItem>>,
    photos: Option<Vec<FxPhoto>>,
    videos: Option<Vec<FxVideo>>,
    mosaic: Option<FxMosaicPhoto>,
}

impl FxMedia {
    fn has_any(&self) -> bool {
        self.all.as_ref().is_some_and(|all| !all.is_empty())
            || self.photos.as_ref().is_some_and(|p| !p.is_empty())
            || self.videos.as_ref().is_some_and(|v| !v.is_empty())
            || self
                .mosaic
                .as_ref()
                .and_then(FxMosaicPhoto::image_url)
                .is_some()
    }
}

#[derive(Debug, Deserialize)]
struct FxMediaItem {
    #[serde(rename = "type")]
    item_type: Option<String>,
    url: Option<String>,
    thumbnail_url: Option<String>,
    width: Option<u32>,
    height: Option<u32>,
    duration: Option<f64>,
    transcode_url: Option<String>,
    #[serde(default, deserialize_with = "deserialize_optional_mosaic_formats")]
    formats: Option<FxMosaicFormats>,
}

#[derive(Debug, Deserialize)]
struct FxPhoto {
    url: Option<String>,
    width: Option<u32>,
    height: Option<u32>,
}

#[derive(Debug, Deserialize)]
struct FxMosaicPhoto {
    url: Option<String>,
    width: Option<u32>,
    height: Option<u32>,
    formats: Option<FxMosaicFormats>,
}

#[derive(Debug, Deserialize)]
struct FxMosaicFormats {
    jpeg: Option<String>,
    webp: Option<String>,
}

fn deserialize_optional_mosaic_formats<'de, D>(
    deserializer: D,
) -> Result<Option<FxMosaicFormats>, D::Error>
where
    D: Deserializer<'de>,
{
    match Option::<Value>::deserialize(deserializer)? {
        Some(Value::Object(map)) => serde_json::from_value(Value::Object(map))
            .map(Some)
            .map_err(serde::de::Error::custom),
        _ => Ok(None),
    }
}

impl FxMosaicPhoto {
    fn image_url(&self) -> Option<&str> {
        self.formats
            .as_ref()
            .and_then(|formats| formats.jpeg.as_deref().or(formats.webp.as_deref()))
            .or(self.url.as_deref())
    }
}

#[derive(Debug, Deserialize)]
struct FxVideo {
    url: Option<String>,
    thumbnail_url: Option<String>,
    width: Option<u32>,
    height: Option<u32>,
    duration: Option<f64>,
}

#[derive(Debug, Clone, Copy)]
enum MediaChoice<'a> {
    Image {
        url: &'a str,
        width: Option<u32>,
        height: Option<u32>,
    },
    Video {
        url: &'a str,
        thumbnail_url: Option<&'a str>,
        width: Option<u32>,
        height: Option<u32>,
        duration: Option<f64>,
    },
}

async fn resolve_status(
    ctx: &ResolveContext<'_>,
    request: &FxStatusRequest,
) -> anyhow::Result<ResolverResult> {
    let api_url = fxtwitter_api_url(request);
    let result =
        http_fetch::fetch_url(&ctx.http_client, &api_url, API_MAX_BYTES, API_TIMEOUT).await?;
    if result.status != 200 {
        return Ok(ResolverResult { embeds: vec![] });
    }
    let Ok(response) = serde_json::from_slice::<FxResponse>(&result.bytes) else {
        return Ok(ResolverResult { embeds: vec![] });
    };
    let Some(tweet) = response.tweet else {
        return Ok(ResolverResult { embeds: vec![] });
    };

    if request.flags.direct
        && let Some(embed) = build_direct_media_embed(ctx, &tweet, request).await
    {
        return Ok(ResolverResult {
            embeds: vec![embed],
        });
    }

    let posted_url = ctx.original_url.to_string();
    let mut embed = MessageEmbed::new("rich");
    embed.url = Some(posted_url.clone());
    if !request.flags.gallery {
        embed.color = Some(FXTWITTER_COLOR);
        embed.description = Some(build_description(&tweet));
        embed.timestamp = tweet.created_timestamp.and_then(format_timestamp);
        embed.footer = Some(build_fxtwitter_footer(ctx));
    }

    if let Some(author) = tweet.author.as_ref() {
        embed.author = Some(build_fxtwitter_author(
            ctx,
            author,
            &posted_url,
            &request.screen_name,
        ));
    }

    let mut gallery: Vec<EmbedMedia> = Vec::new();
    if !request.flags.text_only {
        apply_media_to_embed(ctx, &tweet, request, &mut embed, &mut gallery).await;
    }

    let mut embeds = vec![embed];
    for image in gallery {
        let mut extra = MessageEmbed::new("rich");
        extra.url = Some(posted_url.clone());
        extra.image = Some(image);
        embeds.push(extra);
    }

    Ok(ResolverResult { embeds })
}

fn build_fxtwitter_footer(ctx: &ResolveContext<'_>) -> EmbedFooter {
    EmbedFooter {
        text: FXTWITTER_FOOTER_TEXT.to_owned(),
        icon_url: Some(FXTWITTER_FOOTER_ICON.to_owned()),
        proxy_icon_url: ctx.media_proxy.external_proxy_url(FXTWITTER_FOOTER_ICON),
    }
}

fn build_fxtwitter_author(
    ctx: &ResolveContext<'_>,
    author: &FxAuthor,
    posted_url: &str,
    fallback_screen_name: &str,
) -> EmbedAuthor {
    let screen = author
        .screen_name
        .as_deref()
        .unwrap_or(fallback_screen_name);
    let name = author.name.as_deref().unwrap_or(screen);
    EmbedAuthor {
        name: text_limits::truncate(&format!("{name} (@{screen})"), text_limits::AUTHOR_NAME_MAX),
        url: Some(posted_url.to_owned()),
        icon_url: author.avatar_url.clone(),
        proxy_icon_url: author
            .avatar_url
            .as_deref()
            .and_then(|url| ctx.media_proxy.external_proxy_url(url)),
    }
}

fn fxtwitter_api_url(request: &FxStatusRequest) -> String {
    let mut api_url = format!(
        "{FXTWITTER_API_BASE}/{}/status/{}",
        request.screen_name, request.status_id
    );
    if let Some(language) = request.language.as_deref() {
        api_url.push('/');
        api_url.push_str(language);
    }
    api_url
}

async fn build_direct_media_embed(
    ctx: &ResolveContext<'_>,
    tweet: &FxTweet,
    request: &FxStatusRequest,
) -> Option<MessageEmbed> {
    let media = tweet.media.as_ref()?;
    let choice = if let Some(index) = request.media_index {
        selected_choice_from_media(media, index, request.media_kind)
    } else if request.flags.force_mosaic {
        mosaic_choice(media).or_else(|| first_direct_choice_from_media(media))
    } else {
        first_direct_choice_from_media(media)
    }?;

    build_direct_media_embed_for_choice(ctx, choice, request.direct_media_name.as_deref()).await
}

async fn build_direct_media_embed_for_choice(
    ctx: &ResolveContext<'_>,
    choice: MediaChoice<'_>,
    image_name: Option<&str>,
) -> Option<MessageEmbed> {
    match choice {
        MediaChoice::Image { url, width, height } => {
            let media_url = apply_image_name(url, image_name);
            let media = build_image_media(ctx, &media_url, width, height).await?;
            let mut embed = MessageEmbed::new("image");
            embed.url = Some(media_url);
            embed.thumbnail = Some(media);
            Some(embed)
        }
        MediaChoice::Video {
            url,
            thumbnail_url,
            width,
            height,
            duration,
        } => {
            let (thumbnail, video) =
                build_video_pair_from_parts(ctx, url, thumbnail_url, width, height, duration).await;
            let video = video?;
            let mut embed = MessageEmbed::new("video");
            embed.url = video.url.clone();
            embed.thumbnail = thumbnail;
            embed.video = Some(video);
            Some(embed)
        }
    }
}

async fn apply_media_to_embed(
    ctx: &ResolveContext<'_>,
    tweet: &FxTweet,
    request: &FxStatusRequest,
    embed: &mut MessageEmbed,
    gallery: &mut Vec<EmbedMedia>,
) {
    let Some(media) = select_media_source(tweet) else {
        return;
    };

    if let Some(index) = request.media_index {
        if let Some(choice) = selected_choice_from_media(media, index, request.media_kind) {
            apply_media_choice(ctx, embed, choice).await;
        }
        return;
    }

    if request.flags.force_mosaic
        && let Some(choice) = mosaic_choice(media)
        && apply_media_choice(ctx, embed, choice).await
    {
        return;
    }

    if let Some(video) = media.videos.as_ref().and_then(|videos| videos.first()) {
        let (thumbnail, video_media) = build_video_pair(ctx, video).await;
        embed.thumbnail = thumbnail;
        embed.video = video_media;
    } else if let Some(photos) = media.photos.as_ref() {
        let mut photos = photos.iter();
        if let Some(first) = photos.next()
            && let Some(image) = build_photo_media(ctx, first).await
        {
            embed.image = Some(image);
        }
        for photo in photos.take(MAX_GALLERY_IMAGES.saturating_sub(1)) {
            if let Some(image) = build_photo_media(ctx, photo).await {
                gallery.push(image);
            }
        }
    } else if let Some(choice) = first_choice_from_all(media) {
        apply_media_choice(ctx, embed, choice).await;
    }
}

async fn apply_media_choice(
    ctx: &ResolveContext<'_>,
    embed: &mut MessageEmbed,
    choice: MediaChoice<'_>,
) -> bool {
    match choice {
        MediaChoice::Image { url, width, height } => {
            let Some(image) = build_image_media(ctx, url, width, height).await else {
                return false;
            };
            embed.image = Some(image);
            true
        }
        MediaChoice::Video {
            url,
            thumbnail_url,
            width,
            height,
            duration,
        } => {
            let (thumbnail, video_media) =
                build_video_pair_from_parts(ctx, url, thumbnail_url, width, height, duration).await;
            embed.thumbnail = thumbnail;
            embed.video = video_media;
            embed.video.is_some()
        }
    }
}

fn select_media_source(tweet: &FxTweet) -> Option<&FxMedia> {
    if let Some(media) = tweet.media.as_ref().filter(|media| media.has_any()) {
        return Some(media);
    }
    tweet
        .quote
        .as_deref()
        .and_then(|quote| quote.media.as_ref())
        .filter(|media| media.has_any())
}

fn selected_choice_from_media<'a>(
    media: &'a FxMedia,
    index: usize,
    media_kind: Option<MediaSelectorKind>,
) -> Option<MediaChoice<'a>> {
    media
        .all
        .as_ref()
        .and_then(|all| all.get(index))
        .and_then(media_item_choice)
        .or_else(|| match media_kind {
            Some(MediaSelectorKind::Photo) => media
                .photos
                .as_ref()
                .and_then(|photos| photos.get(index))
                .and_then(photo_choice),
            Some(MediaSelectorKind::Video) => media
                .videos
                .as_ref()
                .and_then(|videos| videos.get(index))
                .and_then(video_choice),
            None => first_direct_choice_from_media(media),
        })
}

fn first_direct_choice_from_media(media: &FxMedia) -> Option<MediaChoice<'_>> {
    first_choice_from_all(media)
        .or_else(|| media.photos.as_ref()?.first().and_then(photo_choice))
        .or_else(|| media.videos.as_ref()?.first().and_then(video_choice))
        .or_else(|| mosaic_choice(media))
}

fn first_choice_from_all(media: &FxMedia) -> Option<MediaChoice<'_>> {
    media
        .all
        .as_ref()
        .and_then(|all| all.first())
        .and_then(media_item_choice)
}

fn mosaic_choice(media: &FxMedia) -> Option<MediaChoice<'_>> {
    let mosaic = media.mosaic.as_ref()?;
    Some(MediaChoice::Image {
        url: mosaic.image_url()?,
        width: mosaic.width,
        height: mosaic.height,
    })
}

fn photo_choice(photo: &FxPhoto) -> Option<MediaChoice<'_>> {
    Some(MediaChoice::Image {
        url: photo.url.as_deref()?,
        width: photo.width,
        height: photo.height,
    })
}

fn video_choice(video: &FxVideo) -> Option<MediaChoice<'_>> {
    Some(MediaChoice::Video {
        url: video.url.as_deref()?,
        thumbnail_url: video.thumbnail_url.as_deref(),
        width: video.width,
        height: video.height,
        duration: video.duration,
    })
}

fn media_item_choice(item: &FxMediaItem) -> Option<MediaChoice<'_>> {
    let item_type = item.item_type.as_deref().unwrap_or_default();
    let is_video = item_type == "video"
        || (item_type == "gif" && (item.thumbnail_url.is_some() || item.duration.is_some()))
        || item.thumbnail_url.is_some()
        || item.duration.is_some();

    if is_video {
        return Some(MediaChoice::Video {
            url: item.transcode_url.as_deref().or(item.url.as_deref())?,
            thumbnail_url: item.thumbnail_url.as_deref(),
            width: item.width,
            height: item.height,
            duration: item.duration,
        });
    }

    let image_url = item
        .formats
        .as_ref()
        .and_then(|formats| formats.jpeg.as_deref().or(formats.webp.as_deref()))
        .or(item.url.as_deref())?;
    Some(MediaChoice::Image {
        url: image_url,
        width: item.width,
        height: item.height,
    })
}

async fn build_photo_media(ctx: &ResolveContext<'_>, photo: &FxPhoto) -> Option<EmbedMedia> {
    let url = photo.url.as_deref()?;
    build_image_media(ctx, url, photo.width, photo.height).await
}

async fn build_image_media(
    ctx: &ResolveContext<'_>,
    url: &str,
    width: Option<u32>,
    height: Option<u32>,
) -> Option<EmbedMedia> {
    let nsfw = crate::media_proxy::MediaProxyClient::nsfw_mode_str(ctx.nsfw_mode);
    let meta = ctx.media_proxy.get_metadata(url, nsfw).await.ok()?;
    Some(EmbedMedia {
        url: Some(url.to_owned()),
        proxy_url: ctx.media_proxy.external_proxy_url(url),
        content_type: Some(meta.content_type.clone()),
        content_hash: Some(meta.content_hash.clone()),
        width: width.or(meta.width),
        height: height.or(meta.height),
        placeholder: meta.placeholder.clone(),
        duration: meta.duration.map(|duration| duration as u32),
        flags: embed_media_flags(&meta),
        ..Default::default()
    })
}

async fn build_video_pair(
    ctx: &ResolveContext<'_>,
    video: &FxVideo,
) -> (Option<EmbedMedia>, Option<EmbedMedia>) {
    build_video_pair_from_parts(
        ctx,
        video.url.as_deref().unwrap_or_default(),
        video.thumbnail_url.as_deref(),
        video.width,
        video.height,
        video.duration,
    )
    .await
}

async fn build_video_pair_from_parts(
    ctx: &ResolveContext<'_>,
    video_url: &str,
    thumbnail_url: Option<&str>,
    width: Option<u32>,
    height: Option<u32>,
    duration: Option<f64>,
) -> (Option<EmbedMedia>, Option<EmbedMedia>) {
    let nsfw = crate::media_proxy::MediaProxyClient::nsfw_mode_str(ctx.nsfw_mode);

    let thumbnail = match thumbnail_url {
        Some(thumb_url) => match ctx.media_proxy.get_metadata(thumb_url, nsfw).await {
            Ok(meta) => Some(EmbedMedia {
                url: Some(thumb_url.to_owned()),
                proxy_url: ctx.media_proxy.external_proxy_url(thumb_url),
                content_type: Some(meta.content_type.clone()),
                content_hash: Some(meta.content_hash.clone()),
                width: meta.width,
                height: meta.height,
                placeholder: meta.placeholder.clone(),
                flags: embed_media_flags(&meta),
                ..Default::default()
            }),
            Err(_) => None,
        },
        None => None,
    };

    if video_url.is_empty() {
        return (thumbnail, None);
    }
    let Ok(meta) = ctx.media_proxy.get_metadata(video_url, nsfw).await else {
        return (thumbnail, None);
    };
    let video_media = EmbedMedia {
        url: Some(video_url.to_owned()),
        proxy_url: ctx.media_proxy.external_proxy_url(video_url),
        content_type: Some(meta.content_type.clone()),
        content_hash: Some(meta.content_hash.clone()),
        width: width.or(meta.width),
        height: height.or(meta.height),
        placeholder: meta.placeholder.clone(),
        duration: duration.or(meta.duration).map(|duration| duration as u32),
        flags: embed_media_flags(&meta),
        ..Default::default()
    };
    (thumbnail, Some(video_media))
}

fn apply_image_name(url: &str, name: Option<&str>) -> String {
    let Some(name) = name else {
        return url.to_owned();
    };
    let Ok(mut parsed) = Url::parse(url) else {
        return url.to_owned();
    };

    let path = parsed.path().to_owned();
    if let Some(colon_idx) = path.rfind(':') {
        let slash_idx = path.rfind('/').unwrap_or(0);
        if colon_idx > slash_idx {
            parsed.set_path(&path[..colon_idx]);
        }
    }
    let query_pairs: Vec<(String, String)> = parsed
        .query_pairs()
        .filter(|(key, _)| key != "name")
        .map(|(key, value)| (key.into_owned(), value.into_owned()))
        .collect();
    {
        let mut query = parsed.query_pairs_mut();
        query.clear();
        for (key, value) in query_pairs {
            query.append_pair(&key, &value);
        }
        if !name.is_empty() {
            query.append_pair("name", name);
        }
    }
    parsed.to_string()
}

fn build_description(tweet: &FxTweet) -> String {
    let mut sections: Vec<String> = Vec::new();
    let text = tweet.text.as_deref().unwrap_or("");
    if !text.is_empty() {
        sections.push(format_body(text));
    }
    if let Some(translation) = tweet.translation.as_ref().and_then(build_translation_block) {
        sections.push(translation);
    }
    if let Some(quote) = tweet.quote.as_deref() {
        sections.push(build_quote_block(quote));
    }
    sections.push(build_stats(tweet));
    sections.join("\n\n")
}

fn build_translation_block(translation: &FxTranslation) -> Option<String> {
    let text = translation
        .text
        .as_deref()
        .filter(|text| !text.is_empty())?;
    let label = match (
        translation.source_lang.as_deref(),
        translation.target_lang.as_deref(),
    ) {
        (Some(source), Some(target)) => format!("**Translation ({source} -> {target})**"),
        (_, Some(target)) => format!("**Translation ({target})**"),
        _ => "**Translation**".to_owned(),
    };
    Some(format!("{label}\n{}", format_body(text)))
}

fn build_quote_block(quote: &FxQuote) -> String {
    let screen = quote
        .author
        .as_ref()
        .and_then(|author| author.screen_name.as_deref())
        .unwrap_or_default();
    let name = quote
        .author
        .as_ref()
        .and_then(|author| author.name.as_deref())
        .unwrap_or(screen);
    let quote_url = quote
        .url
        .clone()
        .unwrap_or_else(|| format!("https://x.com/{screen}/status/"));
    let author_url = format!("https://x.com/{screen}");
    let header = format!(
        "> **[Quoting]({quote_url}) {name} \\([@{screen}]({author_url})\\)**",
        name = escape_markdown(name),
    );
    let body = format_body(quote.text.as_deref().unwrap_or(""))
        .split('\n')
        .map(|line| format!("> {line}"))
        .collect::<Vec<_>>()
        .join("\n");
    format!("{header}\n{QUOTE_EMPTY_LINE}\n{body}")
}

fn build_stats(tweet: &FxTweet) -> String {
    let id = tweet.id.as_deref().unwrap_or_default();
    let mut out = String::from("**");
    out.push_str(&stat_segment(
        EMOJI_REPLY,
        &format!("https://x.com/intent/tweet?in_reply_to={id}"),
        tweet.replies.unwrap_or(0),
    ));
    out.push_str(&stat_segment(
        EMOJI_RETWEET,
        &format!("https://x.com/intent/retweet?tweet_id={id}"),
        tweet.retweets.unwrap_or(0),
    ));
    out.push_str(&stat_segment(
        EMOJI_LIKE,
        &format!("https://x.com/intent/like?tweet_id={id}"),
        tweet.likes.unwrap_or(0),
    ));
    if let Some(views) = tweet.views {
        out.push_str(EMOJI_VIEWS);
        out.push(' ');
        out.push_str(&escape_markdown(&format_number(views)));
        out.push_str(STAT_SEP);
    }
    out.push_str("**");
    out
}

fn stat_segment(emoji: &str, intent_url: &str, count: u64) -> String {
    format!(
        "[{emoji}]({intent_url}) {count}{STAT_SEP}",
        count = escape_markdown(&format_number(count)),
    )
}

fn format_number(count: u64) -> String {
    if count >= 1_000_000 {
        format!("{:.2}M", count as f64 / 1_000_000.0)
    } else if count >= 1_000 {
        format!("{:.1}K", count as f64 / 1_000.0)
    } else {
        count.to_string()
    }
}

fn format_body(text: &str) -> String {
    let mut out = String::with_capacity(text.len());
    let mut rest = text;
    while let Some((start, end)) = next_url(rest) {
        out.push_str(&escape_markdown(&rest[..start]));
        out.push_str(&linkify_url(&rest[start..end]));
        rest = &rest[end..];
    }
    out.push_str(&escape_markdown(rest));
    out
}

fn next_url(text: &str) -> Option<(usize, usize)> {
    const SCHEMES: [&str; 2] = ["https://", "http://"];
    for (start, _) in text.char_indices() {
        let rest = &text[start..];
        if SCHEMES.iter().any(|scheme| rest.starts_with(scheme)) {
            let end = rest
                .find(char::is_whitespace)
                .map_or(text.len(), |offset| start + offset);
            return Some((start, end));
        }
    }
    None
}

fn linkify_url(url: &str) -> String {
    let label = url
        .strip_prefix("https://")
        .or_else(|| url.strip_prefix("http://"))
        .unwrap_or(url);
    format!("[{label}]({url})")
}

fn escape_markdown(text: &str) -> String {
    let mut out = String::with_capacity(text.len());
    for ch in text.chars() {
        if matches!(
            ch,
            '\\' | '*' | '_' | '~' | '`' | '|' | '>' | '[' | ']' | '(' | ')' | '.'
        ) {
            out.push('\\');
        }
        out.push(ch);
    }
    out
}

fn format_timestamp(unix_seconds: i64) -> Option<String> {
    chrono::DateTime::from_timestamp(unix_seconds, 0)
        .map(|dt| dt.to_rfc3339_opts(chrono::SecondsFormat::Secs, true))
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEXT_JACK: &str = include_str!("testdata/fxtwitter/text_jack.json");
    const TRANSLATED_JACK_ES: &str = include_str!("testdata/fxtwitter/translated_jack_es.json");
    const SINGLE_PHOTO_INTERIOR: &str =
        include_str!("testdata/fxtwitter/single_photo_interior.json");
    const VIDEO_EMINEM: &str = include_str!("testdata/fxtwitter/video_eminem.json");
    const GIF_ROROFLI: &str = include_str!("testdata/fxtwitter/gif_rorofli.json");
    const GALLERY_ATTIC: &str = include_str!("testdata/fxtwitter/gallery_attic.json");
    const QUOTE_SCANDINAVIAN: &str = include_str!("testdata/fxtwitter/quote_scandinavian.json");
    const MIXED_BLACKPRINTS: &str = include_str!("testdata/fxtwitter/mixed_blackprints.json");

    fn u(s: &str) -> Url {
        Url::parse(s).unwrap()
    }

    fn fixture(json: &str) -> FxTweet {
        serde_json::from_str::<FxResponse>(json)
            .expect("fixture parses")
            .tweet
            .expect("fixture contains tweet")
    }

    fn test_media_proxy() -> crate::media_proxy::MediaProxyClient {
        crate::media_proxy::MediaProxyClient::new_with_public_endpoint(
            "http://media-proxy:8080",
            "secret",
            Some("https://media.example.test"),
            reqwest::Client::new(),
        )
    }

    fn test_context<'a>(
        media_proxy: &'a crate::media_proxy::MediaProxyClient,
    ) -> ResolveContext<'a> {
        ResolveContext {
            url: u("https://fxtwitter.com/AdolphsonFalkk/status/2050662857763688642"),
            original_url: u("https://fxtwitter.com/AdolphsonFalkk/status/2050662857763688642"),
            http_client: reqwest::Client::new(),
            nsfw_mode: crate::types::NsfwMode::Block,
            media_proxy,
            static_cdn_endpoint: "https://static.example.test",
            youtube_api_key: None,
            klipy_api_key: None,
        }
    }

    fn assert_image_choice(
        choice: MediaChoice<'_>,
        expected_url: &str,
        expected_width: Option<u32>,
        expected_height: Option<u32>,
    ) {
        match choice {
            MediaChoice::Image { url, width, height } => {
                assert_eq!(url, expected_url);
                assert_eq!(width, expected_width);
                assert_eq!(height, expected_height);
            }
            MediaChoice::Video { .. } => panic!("expected image media choice"),
        }
    }

    fn assert_video_choice(
        choice: MediaChoice<'_>,
        expected_url: &str,
        expected_thumbnail_url: Option<&str>,
        expected_width: Option<u32>,
        expected_height: Option<u32>,
        expected_duration: Option<f64>,
    ) {
        match choice {
            MediaChoice::Video {
                url,
                thumbnail_url,
                width,
                height,
                duration,
            } => {
                assert_eq!(url, expected_url);
                assert_eq!(thumbnail_url, expected_thumbnail_url);
                assert_eq!(width, expected_width);
                assert_eq!(height, expected_height);
                assert_eq!(duration, expected_duration);
            }
            MediaChoice::Image { .. } => panic!("expected video media choice"),
        }
    }

    #[test]
    fn fxtwitter_author_and_footer_icons_include_proxy_urls() {
        let media_proxy = test_media_proxy();
        let ctx = test_context(&media_proxy);
        let author = FxAuthor {
            screen_name: Some("AdolphsonFalkk".to_owned()),
            name: Some("Elise".to_owned()),
            avatar_url: Some(
                "https://pbs.twimg.com/profile_images/2011041762391842816/cMiGvRb-_200x200.jpg"
                    .to_owned(),
            ),
        };

        let embed_author = build_fxtwitter_author(
            &ctx,
            &author,
            "https://fxtwitter.com/AdolphsonFalkk/status/2050662857763688642",
            "fallback",
        );
        assert_eq!(
            embed_author.icon_url.as_deref(),
            author.avatar_url.as_deref()
        );
        assert!(
            embed_author
                .proxy_icon_url
                .as_deref()
                .is_some_and(|url| url.starts_with("https://media.example.test/external/"))
        );

        let footer = build_fxtwitter_footer(&ctx);
        assert_eq!(footer.icon_url.as_deref(), Some(FXTWITTER_FOOTER_ICON));
        assert!(
            footer
                .proxy_icon_url
                .as_deref()
                .is_some_and(|url| url.starts_with("https://media.example.test/external/"))
        );
    }

    #[test]
    fn matches_only_fxtwitter_domains() {
        for host in [
            "https://fxtwitter.com/a/status/1",
            "https://fixupx.com/a/status/1",
            "https://twittpr.com/a/status/1",
            "https://xfixup.com/a/status/1",
            "https://www.fxtwitter.com/a/status/1",
            "https://d.fixupx.com/a/status/1",
            "https://dl.xfixup.com/a/status/1",
            "https://g.twittpr.com/a/status/1",
            "https://t.fxtwitter.com/a/status/1",
            "https://m.fixupx.com/a/status/1",
        ] {
            assert!(FxTwitterResolver.matches(&u(host)), "should match {host}");
        }
        for host in [
            "https://x.com/a/status/1",
            "https://twitter.com/a/status/1",
            "https://mobile.twitter.com/a/status/1",
            "https://vxtwitter.com/a/status/1",
            "https://fixvx.com/a/status/1",
            "https://example.com/a/status/1",
            "https://notfxtwitter.com/a/status/1",
        ] {
            assert!(
                !FxTwitterResolver.matches(&u(host)),
                "should NOT match {host}"
            );
        }
    }

    #[test]
    fn parses_status_ids_from_path_shapes() {
        assert_eq!(
            parse_status(&u("https://x.com/jack/status/20")),
            Some(("jack".to_owned(), "20".to_owned()))
        );
        assert_eq!(
            parse_status(&u("https://twitter.com/jack/statuses/20?s=21")),
            Some(("jack".to_owned(), "20".to_owned()))
        );
        assert_eq!(
            parse_status(&u("https://x.com/i/web/status/12345")),
            Some(("i".to_owned(), "12345".to_owned()))
        );
        assert_eq!(
            parse_status(&u("https://fxtwitter.com/status/12345/es")),
            Some(("i".to_owned(), "12345".to_owned()))
        );
        assert_eq!(parse_status(&u("https://x.com/jack")), None);
        assert_eq!(parse_status(&u("https://x.com/jack/status/notanid")), None);
        assert_eq!(parse_status(&u("https://x.com/home")), None);
    }

    #[test]
    fn parses_fxembed_url_modifiers() {
        let direct = parse_status_request(&u("https://d.fxtwitter.com/user/status/123")).unwrap();
        assert_eq!(direct.screen_name, "user");
        assert_eq!(direct.status_id, "123");
        assert!(direct.flags.direct);
        assert_eq!(direct.language, None);

        let stacked = parse_status_request(&u(
            "https://dl.xfixup.com/user/status/123/photo/3/es?name=small",
        ))
        .unwrap();
        assert!(stacked.flags.direct);
        assert_eq!(stacked.media_kind, Some(MediaSelectorKind::Photo));
        assert_eq!(stacked.media_index, Some(2));
        assert_eq!(stacked.language.as_deref(), Some("es"));
        assert_eq!(stacked.direct_media_name.as_deref(), Some("small"));
        assert_eq!(
            fxtwitter_api_url(&stacked),
            "https://api.fxtwitter.com/user/status/123/es"
        );

        let suffix =
            parse_status_request(&u("https://fxtwitter.com/user/status/123.jpg:orig")).unwrap();
        assert!(suffix.flags.direct);
        assert_eq!(suffix.status_id, "123");
        assert_eq!(suffix.direct_media_name.as_deref(), Some("orig"));

        let path_prefix =
            parse_status_request(&u("https://fxtwitter.com/dl/user/status/123/video/2/en"))
                .unwrap();
        assert!(path_prefix.flags.direct);
        assert_eq!(path_prefix.screen_name, "user");
        assert_eq!(path_prefix.media_kind, Some(MediaSelectorKind::Video));
        assert_eq!(path_prefix.media_index, Some(1));
        assert_eq!(path_prefix.language.as_deref(), Some("en"));
    }

    #[test]
    fn parses_presentation_modifier_flags() {
        let gallery = parse_status_request(&u("https://g.fixupx.com/a/status/1")).unwrap();
        assert!(gallery.flags.gallery);
        assert!(!gallery.flags.direct);

        let text = parse_status_request(&u("https://t.fxtwitter.com/a/status/1/fr")).unwrap();
        assert!(text.flags.text_only);
        assert_eq!(text.language.as_deref(), Some("fr"));

        let mosaic = parse_status_request(&u("https://m.twittpr.com/a/status/1")).unwrap();
        assert!(mosaic.flags.force_mosaic);

        let instant = parse_status_request(&u("https://i.fxtwitter.com/a/status/1")).unwrap();
        assert!(instant.flags.force_instant_view);

        let old = parse_status_request(&u("https://o.fixupx.com/a/status/1")).unwrap();
        assert!(old.flags.old_embed);
    }

    #[test]
    fn applies_direct_image_name_like_fxembed() {
        assert_eq!(
            apply_image_name(
                "https://pbs.twimg.com/media/a.jpg?name=orig&format=jpg",
                Some("small")
            ),
            "https://pbs.twimg.com/media/a.jpg?format=jpg&name=small"
        );
        assert_eq!(
            apply_image_name("https://pbs.twimg.com/media/a.jpg:orig", Some("large")),
            "https://pbs.twimg.com/media/a.jpg?name=large"
        );
    }

    #[test]
    fn parses_video_media_item_with_formats_array() {
        let tweet = fixture(VIDEO_EMINEM);
        let media = tweet.media.as_ref().unwrap();
        let choice = first_choice_from_all(media).unwrap();
        assert_video_choice(
            choice,
            "https://video.twimg.com/amplify_video/943561926340218880/vid/1280x720/2RNJO2a6D60ZDTTi.mp4",
            Some(
                "https://pbs.twimg.com/amplify_video_thumb/943561926340218880/img/s0HDjV8X4Qhvpg3J.jpg",
            ),
            Some(1280),
            Some(720),
            Some(32.366),
        );
    }

    #[test]
    fn parses_live_fxtwitter_fixture_shapes() {
        for (name, json) in [
            ("text", TEXT_JACK),
            ("translation", TRANSLATED_JACK_ES),
            ("photo", SINGLE_PHOTO_INTERIOR),
            ("video", VIDEO_EMINEM),
            ("gif", GIF_ROROFLI),
            ("gallery", GALLERY_ATTIC),
            ("quote", QUOTE_SCANDINAVIAN),
            ("mixed", MIXED_BLACKPRINTS),
        ] {
            let response = serde_json::from_str::<FxResponse>(json)
                .unwrap_or_else(|err| panic!("{name} fixture failed to parse: {err}"));
            assert!(response.tweet.is_some(), "{name} fixture has no tweet");
        }
    }

    #[test]
    fn text_fixture_formats_description_and_timestamp() {
        let tweet = fixture(TEXT_JACK);
        assert_eq!(
            tweet
                .author
                .as_ref()
                .and_then(|author| author.screen_name.as_deref()),
            Some("jack")
        );
        assert_eq!(
            tweet
                .created_timestamp
                .and_then(format_timestamp)
                .as_deref(),
            Some("2006-03-21T20:50:14Z")
        );
        assert!(select_media_source(&tweet).is_none());
        assert_eq!(
            build_description(&tweet),
            "just setting up my twttr\n\n**[\u{1F4AC}](https://x.com/intent/tweet?in_reply_to=20) 17\\.9K\u{2002}[\u{1F501}](https://x.com/intent/retweet?tweet_id=20) 125\\.8K\u{2002}[\u{2764}\u{FE0F}](https://x.com/intent/like?tweet_id=20) 308\\.7K\u{2002}**"
        );
    }

    #[test]
    fn translation_fixture_adds_translation_block() {
        let tweet = fixture(TRANSLATED_JACK_ES);
        let description = build_description(&tweet);
        assert!(description.starts_with(
            "just setting up my twttr\n\n**Translation (en -> es)**\nsolo configurando mi twttr"
        ));
        assert!(description.ends_with("308\\.7K\u{2002}**"));
    }

    #[test]
    fn single_photo_fixture_selects_image_media() {
        let tweet = fixture(SINGLE_PHOTO_INTERIOR);
        let media = select_media_source(&tweet).expect("media source");
        assert_eq!(media.all.as_ref().map(Vec::len), Some(1));
        assert_eq!(media.photos.as_ref().map(Vec::len), Some(1));
        assert_image_choice(
            first_choice_from_all(media).unwrap(),
            "https://pbs.twimg.com/media/Bwni7DgCQAABVQh.jpg?name=orig",
            Some(1024),
            Some(682),
        );
    }

    #[test]
    fn video_fixture_matches_reported_eminem_embed_data() {
        let tweet = fixture(VIDEO_EMINEM);
        assert_eq!(
            tweet
                .author
                .as_ref()
                .and_then(|author| author.name.as_deref()),
            Some("Marshall Mathers")
        );
        assert_eq!(
            tweet
                .created_timestamp
                .and_then(format_timestamp)
                .as_deref(),
            Some("2017-12-20T21:15:03Z")
        );
        let description = build_description(&tweet);
        assert!(description.starts_with("Can\u{2019}t wait to share this\n\n**"));
        assert!(description.contains("tweet_id=943590594491772928) 8\\.4K"));
        assert!(description.contains("31\\.6K"));
    }

    #[test]
    fn gif_fixture_is_treated_as_video_media() {
        let tweet = fixture(GIF_ROROFLI);
        let media = select_media_source(&tweet).expect("media source");
        assert_eq!(media.videos.as_ref().map(Vec::len), Some(1));
        assert_video_choice(
            first_choice_from_all(media).unwrap(),
            "https://video.twimg.com/tweet_video/HDCxjM_XYAAG6nY.mp4",
            Some("https://pbs.twimg.com/tweet_video_thumb/HDCxjM_XYAAG6nY.jpg"),
            Some(164),
            Some(206),
            Some(0.0),
        );
        assert!(build_description(&tweet).contains("\u{1F441}\u{FE0F} 4\\.80M"));
    }

    #[test]
    fn gallery_fixture_parses_photo_list_and_mosaic_object_formats() {
        let tweet = fixture(GALLERY_ATTIC);
        let media = select_media_source(&tweet).expect("media source");
        assert_eq!(media.all.as_ref().map(Vec::len), Some(4));
        assert_eq!(media.photos.as_ref().map(Vec::len), Some(4));
        assert_image_choice(
            first_choice_from_all(media).unwrap(),
            "https://pbs.twimg.com/media/EbNvNcpWAAAKY6x.jpg?name=orig",
            Some(2048),
            Some(1365),
        );
        assert_image_choice(
            mosaic_choice(media).unwrap(),
            "https://mosaic.fxtwitter.com/jpeg/1275485415903019009/EbNvNcpWAAAKY6x/EbNvNcrXkAAr940/EbNvNcqWkAAxPzx/EbNvNcqWoAI4OCc",
            None,
            None,
        );
    }

    #[test]
    fn quote_fixture_promotes_quote_media_when_status_media_is_empty() {
        let tweet = fixture(QUOTE_SCANDINAVIAN);
        let media = select_media_source(&tweet).expect("quote media source");
        assert_image_choice(
            first_choice_from_all(media).unwrap(),
            "https://pbs.twimg.com/media/HHVbs9nW0AAaeVg.jpg?name=orig",
            Some(1170),
            Some(1428),
        );
        let description = build_description(&tweet);
        assert!(description.contains(
            "> **[Quoting](https://x.com/ScandinavianAE/status/2050646038420045877) Scandinavian Aesthetics"
        ));
        assert!(description.contains("\u{1F441}\u{FE0F} 7\\.40M"));
    }

    #[test]
    fn mixed_media_fixture_keeps_all_order_for_numbered_selection() {
        let tweet = fixture(MIXED_BLACKPRINTS);
        let media = select_media_source(&tweet).expect("media source");
        assert_eq!(media.all.as_ref().map(Vec::len), Some(2));
        assert_eq!(media.photos.as_ref().map(Vec::len), Some(1));
        assert_eq!(media.videos.as_ref().map(Vec::len), Some(1));
        assert_image_choice(
            selected_choice_from_media(media, 0, None).unwrap(),
            "https://pbs.twimg.com/media/FeLsnyxUoAE9IQQ.jpg?name=orig",
            Some(1364),
            Some(2048),
        );
        assert_video_choice(
            selected_choice_from_media(media, 1, None).unwrap(),
            "https://video.twimg.com/ext_tw_video/1577082987920834561/pu/vid/720x1280/nheMJUJTacHgkP-5.mp4?tag=12",
            Some(
                "https://pbs.twimg.com/ext_tw_video_thumb/1577082987920834561/pu/img/lpjkeE_5PK1x0Dyp.jpg",
            ),
            Some(720),
            Some(1280),
            Some(12.866),
        );
    }

    #[test]
    fn format_number_matches_fxtwitter() {
        assert_eq!(format_number(538), "538");
        assert_eq!(format_number(723), "723");
        assert_eq!(format_number(9_057), "9.1K");
        assert_eq!(format_number(17_876), "17.9K");
        assert_eq!(format_number(125_897), "125.9K");
        assert_eq!(format_number(308_997), "309.0K");
        assert_eq!(format_number(296_573), "296.6K");
        assert_eq!(format_number(7_401_595), "7.40M");
        assert_eq!(format_number(1_230_000), "1.23M");
        assert_eq!(format_number(3_440_000), "3.44M");
        assert_eq!(format_number(0), "0");
        assert_eq!(format_number(999), "999");
        assert_eq!(format_number(1_000), "1.0K");
    }

    #[test]
    fn escape_markdown_escapes_observed_set() {
        assert_eq!(escape_markdown("religion..."), "religion\\.\\.\\.");
        assert_eq!(escape_markdown("9.1K"), "9\\.1K");
        assert_eq!(escape_markdown("a*b_c~d"), "a\\*b\\_c\\~d");
        assert_eq!(escape_markdown("(x)[y]"), "\\(x\\)\\[y\\]");
        assert_eq!(escape_markdown("@handle #tag it's"), "@handle #tag it's");
    }

    #[test]
    fn format_timestamp_matches_reference() {
        assert_eq!(
            format_timestamp(1_777_751_099).as_deref(),
            Some("2026-05-02T19:44:59Z")
        );
        assert_eq!(
            format_timestamp(1_142_974_214).as_deref(),
            Some("2006-03-21T20:50:14Z")
        );
    }

    fn quote_example() -> FxQuote {
        FxQuote {
            url: Some("https://x.com/ScandinavianAE/status/2050646038420045877".to_owned()),
            text: Some("Sweden \u{1F1F8}\u{1F1EA}".to_owned()),
            author: Some(FxAuthor {
                screen_name: Some("ScandinavianAE".to_owned()),
                name: Some("Scandinavian Aesthetics".to_owned()),
                avatar_url: None,
            }),
            media: None,
        }
    }

    fn tweet_example() -> FxTweet {
        FxTweet {
            id: Some("2050662857763688642".to_owned()),
            text: Some("No that's fine I just imagined it would be a bit bigger".to_owned()),
            created_timestamp: Some(1_777_751_099),
            author: Some(FxAuthor {
                screen_name: Some("AdolphsonFalkk".to_owned()),
                name: Some("Elise \u{1F352}\u{1F352}\u{2671}".to_owned()),
                avatar_url: Some("https://example.com/a.jpg".to_owned()),
            }),
            replies: Some(538),
            retweets: Some(9_057),
            likes: Some(296_573),
            views: Some(7_401_595),
            quote: Some(Box::new(quote_example())),
            media: None,
            translation: None,
        }
    }

    #[test]
    fn build_quote_block_matches_reference() {
        assert_eq!(
            build_quote_block(&quote_example()),
            "> **[Quoting](https://x.com/ScandinavianAE/status/2050646038420045877) Scandinavian Aesthetics \\([@ScandinavianAE](https://x.com/ScandinavianAE)\\)**\n> \u{FE00}\n> Sweden \u{1F1F8}\u{1F1EA}"
        );
    }

    #[test]
    fn build_stats_matches_reference_with_views() {
        assert_eq!(
            build_stats(&tweet_example()),
            "**[\u{1F4AC}](https://x.com/intent/tweet?in_reply_to=2050662857763688642) 538\u{2002}[\u{1F501}](https://x.com/intent/retweet?tweet_id=2050662857763688642) 9\\.1K\u{2002}[\u{2764}\u{FE0F}](https://x.com/intent/like?tweet_id=2050662857763688642) 296\\.6K\u{2002}\u{1F441}\u{FE0F} 7\\.40M\u{2002}**"
        );
    }

    #[test]
    fn build_stats_omits_views_when_absent() {
        let mut tweet = tweet_example();
        tweet.id = Some("20".to_owned());
        tweet.replies = Some(17_876);
        tweet.retweets = Some(125_897);
        tweet.likes = Some(308_997);
        tweet.views = None;
        tweet.quote = None;
        assert_eq!(
            build_stats(&tweet),
            "**[\u{1F4AC}](https://x.com/intent/tweet?in_reply_to=20) 17\\.9K\u{2002}[\u{1F501}](https://x.com/intent/retweet?tweet_id=20) 125\\.9K\u{2002}[\u{2764}\u{FE0F}](https://x.com/intent/like?tweet_id=20) 309\\.0K\u{2002}**"
        );
    }

    #[test]
    fn build_description_quote_tweet_matches_reference() {
        assert_eq!(
            build_description(&tweet_example()),
            "No that's fine I just imagined it would be a bit bigger\n\n> **[Quoting](https://x.com/ScandinavianAE/status/2050646038420045877) Scandinavian Aesthetics \\([@ScandinavianAE](https://x.com/ScandinavianAE)\\)**\n> \u{FE00}\n> Sweden \u{1F1F8}\u{1F1EA}\n\n**[\u{1F4AC}](https://x.com/intent/tweet?in_reply_to=2050662857763688642) 538\u{2002}[\u{1F501}](https://x.com/intent/retweet?tweet_id=2050662857763688642) 9\\.1K\u{2002}[\u{2764}\u{FE0F}](https://x.com/intent/like?tweet_id=2050662857763688642) 296\\.6K\u{2002}\u{1F441}\u{FE0F} 7\\.40M\u{2002}**"
        );
    }

    #[test]
    fn build_description_single_photo_matches_reference() {
        let tweet = FxTweet {
            id: Some("896523232098078720".to_owned()),
            text: Some(
                "\"No one is born hating another person because of the color of his skin or his background or his religion...\""
                    .to_owned(),
            ),
            created_timestamp: Some(1_502_582_769),
            author: None,
            replies: Some(58_500),
            retweets: Some(1_230_000),
            likes: Some(3_440_000),
            views: None,
            quote: None,
            media: None,
            translation: None,
        };
        assert_eq!(
            build_description(&tweet),
            "\"No one is born hating another person because of the color of his skin or his background or his religion\\.\\.\\.\"\n\n**[\u{1F4AC}](https://x.com/intent/tweet?in_reply_to=896523232098078720) 58\\.5K\u{2002}[\u{1F501}](https://x.com/intent/retweet?tweet_id=896523232098078720) 1\\.23M\u{2002}[\u{2764}\u{FE0F}](https://x.com/intent/like?tweet_id=896523232098078720) 3\\.44M\u{2002}**"
        );
    }

    #[test]
    fn format_body_linkifies_urls_and_escapes_the_rest() {
        assert_eq!(
            format_body("Four more years. http://t.co/bAJE6Vom"),
            "Four more years\\. [t.co/bAJE6Vom](http://t.co/bAJE6Vom)"
        );
        assert_eq!(
            format_body("see https://t.co/aB9 ...thanks."),
            "see [t.co/aB9](https://t.co/aB9) \\.\\.\\.thanks\\."
        );
        assert_eq!(
            format_body("http://t.co/a http://t.co/b"),
            "[t.co/a](http://t.co/a) [t.co/b](http://t.co/b)"
        );
        assert_eq!(format_body("just text."), "just text\\.");
    }

    #[test]
    fn build_description_linkifies_body_url_matches_reference() {
        let tweet = FxTweet {
            id: Some("266031293945503744".to_owned()),
            text: Some("Four more years. http://t.co/bAJE6Vom".to_owned()),
            created_timestamp: Some(1_352_261_778),
            author: None,
            replies: Some(46_995),
            retweets: Some(693_048),
            likes: Some(461_344),
            views: None,
            quote: None,
            media: None,
            translation: None,
        };
        assert_eq!(
            build_description(&tweet),
            "Four more years\\. [t.co/bAJE6Vom](http://t.co/bAJE6Vom)\n\n**[\u{1F4AC}](https://x.com/intent/tweet?in_reply_to=266031293945503744) 47\\.0K\u{2002}[\u{1F501}](https://x.com/intent/retweet?tweet_id=266031293945503744) 693\\.0K\u{2002}[\u{2764}\u{FE0F}](https://x.com/intent/like?tweet_id=266031293945503744) 461\\.3K\u{2002}**"
        );
    }

    #[test]
    fn build_description_includes_translation_suffix_result() {
        let mut tweet = tweet_example();
        tweet.quote = None;
        tweet.translation = Some(FxTranslation {
            text: Some("Hello world.".to_owned()),
            source_lang: Some("ja".to_owned()),
            target_lang: Some("en".to_owned()),
        });
        let description = build_description(&tweet);
        assert!(description.starts_with(
            "No that's fine I just imagined it would be a bit bigger\n\n**Translation (ja -> en)**\nHello world\\."
        ));
        assert!(description.contains(
            "\n\n**[\u{1F4AC}](https://x.com/intent/tweet?in_reply_to=2050662857763688642) "
        ));
    }

    fn media_with(photos: usize, videos: usize) -> FxMedia {
        FxMedia {
            all: None,
            photos: (photos > 0).then(|| {
                (0..photos)
                    .map(|i| FxPhoto {
                        url: Some(format!("https://pbs.twimg.com/media/p{i}.jpg")),
                        width: Some(100),
                        height: Some(100),
                    })
                    .collect()
            }),
            videos: (videos > 0).then(|| {
                (0..videos)
                    .map(|_| FxVideo {
                        url: Some("https://video.twimg.com/v.mp4".to_owned()),
                        thumbnail_url: Some("https://pbs.twimg.com/t.jpg".to_owned()),
                        width: Some(1280),
                        height: Some(720),
                        duration: None,
                    })
                    .collect()
            }),
            mosaic: None,
        }
    }

    #[test]
    fn select_media_source_prefers_tweet_then_quote() {
        let mut tweet = tweet_example();
        tweet.media = Some(media_with(1, 0));
        let selected = select_media_source(&tweet).unwrap();
        assert_eq!(
            selected.photos.as_ref().unwrap()[0].url.as_deref(),
            Some("https://pbs.twimg.com/media/p0.jpg")
        );

        let mut quote = quote_example();
        quote.media = Some(media_with(1, 0));
        let promoting = FxTweet {
            media: None,
            quote: Some(Box::new(quote)),
            ..tweet_example()
        };
        assert!(select_media_source(&promoting).is_some());

        let bare = FxTweet {
            media: None,
            quote: None,
            ..tweet_example()
        };
        assert!(select_media_source(&bare).is_none());
        let empty = FxTweet {
            media: Some(FxMedia::default()),
            quote: None,
            ..tweet_example()
        };
        assert!(select_media_source(&empty).is_none());
    }

    #[test]
    fn build_description_empty_text_is_stats_only() {
        let tweet = FxTweet {
            id: Some("1299530165463199747".to_owned()),
            text: Some(String::new()),
            created_timestamp: None,
            author: None,
            replies: Some(135_100),
            retweets: Some(1_760_000),
            likes: Some(6_590_000),
            views: None,
            quote: None,
            media: None,
            translation: None,
        };
        assert_eq!(
            build_description(&tweet),
            "**[\u{1F4AC}](https://x.com/intent/tweet?in_reply_to=1299530165463199747) 135\\.1K\u{2002}[\u{1F501}](https://x.com/intent/retweet?tweet_id=1299530165463199747) 1\\.76M\u{2002}[\u{2764}\u{FE0F}](https://x.com/intent/like?tweet_id=1299530165463199747) 6\\.59M\u{2002}**"
        );
    }

    #[tokio::test]
    #[ignore = "hits the live FxTwitter API"]
    async fn live_example_pipeline_matches_reference() {
        let client = reqwest::Client::builder()
            .user_agent("Mozilla/5.0 (compatible; Fluxerbot/1.0; +https://fluxer.app)")
            .redirect(reqwest::redirect::Policy::none())
            .build()
            .unwrap();
        let res = http_fetch::fetch_url(
            &client,
            &format!("{FXTWITTER_API_BASE}/AdolphsonFalkk/status/2050662857763688642"),
            API_MAX_BYTES,
            API_TIMEOUT,
        )
        .await
        .expect("fetch fxtwitter api");
        assert_eq!(res.status, 200);
        let tweet = serde_json::from_slice::<FxResponse>(&res.bytes)
            .expect("parse api json")
            .tweet
            .expect("tweet present");

        assert_eq!(
            tweet.author.as_ref().and_then(|a| a.screen_name.as_deref()),
            Some("AdolphsonFalkk")
        );
        assert_eq!(
            tweet
                .created_timestamp
                .and_then(format_timestamp)
                .as_deref(),
            Some("2026-05-02T19:44:59Z")
        );

        let media = select_media_source(&tweet).expect("media source");
        assert!(
            media
                .photos
                .as_ref()
                .is_some_and(|photos| !photos.is_empty())
        );

        let description = build_description(&tweet);
        assert!(
            description.starts_with(
                "No that's fine I just imagined it would be a bit bigger\n\n> **[Quoting](https://x.com/ScandinavianAE/status/2050646038420045877) Scandinavian Aesthetics \\([@ScandinavianAE](https://x.com/ScandinavianAE)\\)**\n> \u{FE00}\n> Sweden \u{1F1F8}\u{1F1EA}\n\n**[\u{1F4AC}](https://x.com/intent/tweet?in_reply_to=2050662857763688642) "
            ),
            "description was: {description:?}"
        );
        assert!(
            description.contains(
                "[\u{1F501}](https://x.com/intent/retweet?tweet_id=2050662857763688642) "
            )
        );
        assert!(description.contains(
            "[\u{2764}\u{FE0F}](https://x.com/intent/like?tweet_id=2050662857763688642) "
        ));
        assert!(description.contains("\u{1F441}\u{FE0F} "));
        assert!(description.ends_with("\u{2002}**"));
    }
}
