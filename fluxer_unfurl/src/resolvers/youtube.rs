// SPDX-License-Identifier: AGPL-3.0-or-later

use super::{ResolveContext, Resolver, ResolverResult};
use crate::http_fetch;
use crate::text_limits;
use crate::types::{EmbedAuthor, EmbedMedia, EmbedProvider, MessageEmbed};
use serde::Deserialize;
use std::future::Future;
use std::pin::Pin;
use std::time::Duration;
use url::Url;

const YOUTUBE_COLOR: u32 = 0xFF0000;
const YOUTUBE_API_BASE: &str = "https://www.googleapis.com/youtube/v3/videos";
const YOUTUBE_API_MAX_BYTES: usize = 256 * 1024;
const YOUTUBE_API_TIMEOUT: Duration = Duration::from_secs(10);
const YOUTUBE_DESCRIPTION_MAX: usize = 350;
const YOUTUBE_PLAYER_SIZE: (u32, u32) = (1280, 720);

const YOUTUBE_HOSTS: &[&str] = &[
    "www.youtube.com",
    "youtube.com",
    "m.youtube.com",
    "music.youtube.com",
    "www.youtube-nocookie.com",
    "youtube-nocookie.com",
    "youtu.be",
];

pub struct YouTubeResolver;

impl Resolver for YouTubeResolver {
    fn matches(&self, url: &Url) -> bool {
        parse_youtube_url(url).is_some()
    }

    fn transform_url(&self, url: &Url) -> Option<Url> {
        let parsed = parse_youtube_url(url)?;
        let mut canonical = Url::parse("https://www.youtube.com/watch").ok()?;
        canonical
            .query_pairs_mut()
            .append_pair("v", &parsed.video_id);
        if let Some(t) = parsed.timestamp {
            canonical
                .query_pairs_mut()
                .append_pair("t", &format!("{t}s"));
        }
        Some(canonical)
    }

    fn resolve<'a>(
        &'a self,
        ctx: &'a ResolveContext<'_>,
    ) -> Pin<Box<dyn Future<Output = anyhow::Result<ResolverResult>> + Send + 'a>> {
        Box::pin(resolve_video(ctx, YOUTUBE_API_BASE))
    }
}

async fn resolve_video(ctx: &ResolveContext<'_>, api_base: &str) -> anyhow::Result<ResolverResult> {
    let Some(parsed) = parse_youtube_url(&ctx.original_url) else {
        return Ok(ResolverResult { embeds: vec![] });
    };

    let api_video = match ctx.youtube_api_key.as_deref() {
        Some(api_key) => fetch_api_video(ctx, api_base, &parsed.video_id, api_key).await,
        None => {
            tracing::debug!("No YouTube API key configured");
            None
        }
    };

    let Some(video) = api_video else {
        return Ok(ResolverResult { embeds: vec![] });
    };
    let embed = build_api_embed(ctx, &parsed, video).await?;

    Ok(ResolverResult {
        embeds: vec![embed],
    })
}

async fn fetch_api_video(
    ctx: &ResolveContext<'_>,
    api_base: &str,
    video_id: &str,
    api_key: &str,
) -> Option<YouTubeVideo> {
    let api_url = build_youtube_api_url(api_base, video_id, api_key).ok()?;
    let api_response = match http_fetch::fetch_url(
        &ctx.http_client,
        api_url.as_ref(),
        YOUTUBE_API_MAX_BYTES,
        YOUTUBE_API_TIMEOUT,
    )
    .await
    {
        Ok(response) => response,
        Err(err) => {
            tracing::error!(video_id, error = %err, "Failed to fetch YouTube API data");
            return None;
        }
    };
    if api_response.status != 200 {
        let error = api_error(&api_response.bytes);
        tracing::error!(
            video_id,
            status = api_response.status,
            reason = error.reason.as_deref().unwrap_or("unknown"),
            message = error.message.as_deref().unwrap_or(""),
            "YouTube Data API rejected the request"
        );
        return None;
    }
    let data: YouTubeApiResponse = match serde_json::from_slice(&api_response.bytes) {
        Ok(data) => data,
        Err(err) => {
            tracing::error!(video_id, error = %err, "Failed to parse YouTube API data");
            return None;
        }
    };
    let video = data.items.and_then(|mut items| items.pop());
    if video.is_none() {
        tracing::error!(video_id, "No YouTube video data found");
    }
    video
}

async fn build_api_embed(
    ctx: &ResolveContext<'_>,
    parsed: &ParsedYouTube,
    video: YouTubeVideo,
) -> anyhow::Result<MessageEmbed> {
    let mut embed = base_video_embed(parsed)?;
    embed.title = Some(video.snippet.title);
    embed.description = Some(parse_youtube_string(
        &video.snippet.description,
        YOUTUBE_DESCRIPTION_MAX,
    ));

    if !video.snippet.channel_title.is_empty() {
        embed.author = Some(EmbedAuthor {
            name: text_limits::truncate(&video.snippet.channel_title, text_limits::AUTHOR_NAME_MAX),
            url: Some(format!(
                "https://www.youtube.com/channel/{}",
                video.snippet.channel_id
            )),
            ..Default::default()
        });
    }

    if let Some(thumbnail) = video.snippet.thumbnails.best() {
        embed.thumbnail =
            Some(thumbnail_media(ctx, &thumbnail.url, thumbnail.width, thumbnail.height).await);
    }

    let (embed_width, embed_height) =
        parse_player_embed_dimensions(&video.player.embed_html).unwrap_or(YOUTUBE_PLAYER_SIZE);
    if let Some(media) = embed.video.as_mut() {
        media.width = Some(embed_width);
        media.height = Some(embed_height);
    }

    Ok(embed)
}

fn base_video_embed(parsed: &ParsedYouTube) -> anyhow::Result<MessageEmbed> {
    let mut embed_url = Url::parse(&format!(
        "https://www.youtube.com/embed/{}",
        parsed.video_id
    ))?;
    let mut main_url = Url::parse("https://www.youtube.com/watch")?;
    main_url
        .query_pairs_mut()
        .append_pair("v", &parsed.video_id);
    if let Some(t) = parsed.timestamp {
        embed_url
            .query_pairs_mut()
            .append_pair("start", &t.to_string());
        main_url
            .query_pairs_mut()
            .append_pair("start", &t.to_string());
    }

    let (width, height) = YOUTUBE_PLAYER_SIZE;
    let mut embed = MessageEmbed::new("video");
    embed.url = Some(main_url.to_string());
    embed.color = Some(YOUTUBE_COLOR);
    embed.provider = Some(EmbedProvider {
        name: "YouTube".to_owned(),
        url: Some("https://www.youtube.com".to_owned()),
    });
    embed.video = Some(EmbedMedia {
        url: Some(embed_url.to_string()),
        width: Some(width),
        height: Some(height),
        ..Default::default()
    });
    Ok(embed)
}

async fn thumbnail_media(
    ctx: &ResolveContext<'_>,
    url: &str,
    width: Option<u32>,
    height: Option<u32>,
) -> EmbedMedia {
    let nsfw_str = crate::media_proxy::MediaProxyClient::nsfw_mode_str(ctx.nsfw_mode);
    let media = match ctx.media_proxy.get_metadata(url, nsfw_str).await {
        Ok(meta) => meta.to_embed_media(url),
        Err(err) => {
            tracing::warn!(
                error = %err,
                url,
                "failed to enrich YouTube thumbnail metadata"
            );
            EmbedMedia {
                url: Some(url.to_owned()),
                content_type: Some("image/jpeg".to_owned()),
                ..Default::default()
            }
        }
    };
    EmbedMedia {
        width: width.or(media.width),
        height: height.or(media.height),
        ..media
    }
}

#[derive(Debug, Default, PartialEq, Eq)]
struct YouTubeApiError {
    reason: Option<String>,
    message: Option<String>,
}

fn api_error(body: &[u8]) -> YouTubeApiError {
    let Ok(value) = serde_json::from_slice::<serde_json::Value>(body) else {
        return YouTubeApiError::default();
    };
    let error = &value["error"];
    let first_reason = |key: &str| {
        error[key].as_array().and_then(|entries| {
            entries
                .iter()
                .find_map(|entry| entry["reason"].as_str().map(ToOwned::to_owned))
        })
    };
    YouTubeApiError {
        reason: first_reason("details").or_else(|| first_reason("errors")),
        message: error["message"].as_str().map(ToOwned::to_owned),
    }
}

#[derive(Debug, Deserialize)]
struct YouTubeApiResponse {
    items: Option<Vec<YouTubeVideo>>,
}

#[derive(Debug, Deserialize)]
struct YouTubeVideo {
    snippet: YouTubeSnippet,
    player: YouTubePlayer,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct YouTubeSnippet {
    title: String,
    description: String,
    channel_title: String,
    channel_id: String,
    thumbnails: YouTubeThumbnails,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct YouTubePlayer {
    embed_html: String,
}

#[derive(Debug, Deserialize)]
struct YouTubeThumbnails {
    default: Option<YouTubeThumbnail>,
    medium: Option<YouTubeThumbnail>,
    high: Option<YouTubeThumbnail>,
    standard: Option<YouTubeThumbnail>,
    maxres: Option<YouTubeThumbnail>,
}

impl YouTubeThumbnails {
    fn best(&self) -> Option<&YouTubeThumbnail> {
        self.maxres
            .as_ref()
            .or(self.high.as_ref())
            .or(self.standard.as_ref())
            .or(self.medium.as_ref())
            .or(self.default.as_ref())
    }
}

#[derive(Debug, Deserialize)]
struct YouTubeThumbnail {
    url: String,
    width: Option<u32>,
    height: Option<u32>,
}

fn build_youtube_api_url(base: &str, video_id: &str, api_key: &str) -> anyhow::Result<Url> {
    let mut url = Url::parse(base)?;
    url.query_pairs_mut()
        .append_pair("key", api_key)
        .append_pair("id", video_id)
        .append_pair("part", "snippet,player,status");
    Ok(url)
}

fn parse_player_embed_dimensions(html: &str) -> Option<(u32, u32)> {
    let width = extract_html_dimension(html, "width")?;
    let height = extract_html_dimension(html, "height")?;
    Some((width, height))
}

fn extract_html_dimension(html: &str, name: &str) -> Option<u32> {
    let pattern = format!("{name}=\"");
    let start = html.find(&pattern)? + pattern.len();
    let rest = &html[start..];
    let end = rest.find('"')?;
    rest[..end].parse().ok()
}

fn parse_youtube_string(value: &str, max_len: usize) -> String {
    let trimmed = value.trim();
    if trimmed.chars().count() <= max_len {
        return trimmed.to_owned();
    }
    let keep = max_len.saturating_sub(3);
    format!("{}...", trimmed.chars().take(keep).collect::<String>())
}

struct ParsedYouTube {
    video_id: String,
    timestamp: Option<u64>,
}

fn parse_youtube_url(url: &Url) -> Option<ParsedYouTube> {
    let host = url.host_str()?;
    if !YOUTUBE_HOSTS.iter().any(|h| host.eq_ignore_ascii_case(h)) {
        return None;
    }

    let video_id = extract_video_id(url)?;

    static ID_RE: std::sync::LazyLock<regex::Regex> = std::sync::LazyLock::new(|| {
        regex::Regex::new(r"^[A-Za-z0-9_-]{6,15}$").expect("valid regex")
    });
    if !ID_RE.is_match(&video_id) {
        return None;
    }

    let timestamp = extract_timestamp(url);

    Some(ParsedYouTube {
        video_id,
        timestamp,
    })
}

fn extract_video_id(url: &Url) -> Option<String> {
    let host = url.host_str()?;
    let path = url.path();

    if host.eq_ignore_ascii_case("youtu.be") {
        let id = path.trim_start_matches('/').split('/').next()?;
        return if id.is_empty() {
            None
        } else {
            Some(id.to_owned())
        };
    }

    if path.starts_with("/shorts/") {
        return path
            .strip_prefix("/shorts/")?
            .split('/')
            .next()
            .map(|s| s.to_owned());
    }
    if path.starts_with("/v/") {
        return path
            .strip_prefix("/v/")?
            .split('/')
            .next()
            .map(|s| s.to_owned());
    }
    if path.starts_with("/embed/") {
        let id = path.strip_prefix("/embed/")?.split('/').next()?;
        if id == "videoseries" || id == "live_stream" {
            return None;
        }
        return Some(id.to_owned());
    }
    if path == "/watch" || path.starts_with("/watch/") {
        return url
            .query_pairs()
            .find(|(k, _)| k == "v")
            .map(|(_, v)| v.into_owned());
    }

    None
}

fn extract_timestamp(url: &Url) -> Option<u64> {
    let t = url
        .query_pairs()
        .find(|(k, _)| k == "t" || k == "start")
        .map(|(_, v)| v.into_owned())?;

    if let Some(secs) = t.strip_suffix('s') {
        return secs.parse().ok();
    }
    t.parse().ok()
}
