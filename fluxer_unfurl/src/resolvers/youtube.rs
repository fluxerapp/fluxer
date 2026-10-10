// SPDX-License-Identifier: AGPL-3.0-or-later

use super::{ResolveContext, Resolver, ResolverResult};
use crate::http_fetch;
use crate::media_proxy::embed_media_flags;
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
        Box::pin(async move {
            let parsed = match parse_youtube_url(&ctx.original_url) {
                Some(p) => p,
                None => {
                    return Ok(ResolverResult { embeds: vec![] });
                }
            };

            let Some(api_key) = ctx.youtube_api_key.clone() else {
                tracing::debug!("No YouTube API key configured");
                return Ok(ResolverResult { embeds: vec![] });
            };

            let api_url = build_youtube_api_url(&parsed.video_id, &api_key)?;
            let api_response = http_fetch::fetch_url(
                &ctx.http_client,
                api_url.as_ref(),
                YOUTUBE_API_MAX_BYTES,
                YOUTUBE_API_TIMEOUT,
            )
            .await?;
            if api_response.status != 200 {
                tracing::error!(
                    video_id = %parsed.video_id,
                    status = api_response.status,
                    "Failed to fetch YouTube API data"
                );
                return Ok(ResolverResult { embeds: vec![] });
            }
            let data: YouTubeApiResponse = serde_json::from_slice(&api_response.bytes)?;
            let Some(video) = data.items.and_then(|mut items| items.pop()) else {
                tracing::error!(video_id = %parsed.video_id, "No YouTube video data found");
                return Ok(ResolverResult { embeds: vec![] });
            };

            let mut embed_url_parsed = Url::parse(&format!(
                "https://www.youtube.com/embed/{}",
                parsed.video_id
            ))
            .unwrap_or_else(|_| ctx.url.clone());
            if let Some(t) = parsed.timestamp {
                embed_url_parsed
                    .query_pairs_mut()
                    .append_pair("start", &t.to_string());
            }

            let mut main_url = Url::parse("https://www.youtube.com/watch")?;
            main_url
                .query_pairs_mut()
                .append_pair("v", &parsed.video_id);
            if let Some(t) = parsed.timestamp {
                main_url
                    .query_pairs_mut()
                    .append_pair("start", &t.to_string());
            }

            let mut embed = MessageEmbed::new("video");
            embed.url = Some(main_url.to_string());
            embed.title = Some(video.snippet.title);
            embed.description = Some(parse_youtube_string(
                &video.snippet.description,
                YOUTUBE_DESCRIPTION_MAX,
            ));
            embed.color = Some(YOUTUBE_COLOR);

            if !video.snippet.channel_title.is_empty() {
                embed.author = Some(EmbedAuthor {
                    name: text_limits::truncate(
                        &video.snippet.channel_title,
                        text_limits::AUTHOR_NAME_MAX,
                    ),
                    url: Some(format!(
                        "https://www.youtube.com/channel/{}",
                        video.snippet.channel_id
                    )),
                    ..Default::default()
                });
            }

            embed.provider = Some(EmbedProvider {
                name: "YouTube".to_owned(),
                url: Some("https://www.youtube.com".to_owned()),
            });

            if let Some(thumbnail) = video.snippet.thumbnails.best() {
                let nsfw_str = crate::media_proxy::MediaProxyClient::nsfw_mode_str(ctx.nsfw_mode);
                let thumbnail_meta =
                    match ctx.media_proxy.get_metadata(&thumbnail.url, nsfw_str).await {
                        Ok(meta) => Some(meta),
                        Err(err) => {
                            tracing::warn!(
                                error = %err,
                                url = thumbnail.url,
                                "failed to enrich YouTube thumbnail metadata"
                            );
                            None
                        }
                    };
                embed.thumbnail = Some(EmbedMedia {
                    url: Some(thumbnail.url.clone()),
                    content_type: thumbnail_meta
                        .as_ref()
                        .map(|meta| meta.content_type.clone())
                        .or_else(|| Some("image/jpeg".to_owned())),
                    content_hash: thumbnail_meta
                        .as_ref()
                        .map(|meta| meta.content_hash.clone()),
                    width: thumbnail
                        .width
                        .or_else(|| thumbnail_meta.as_ref().and_then(|meta| meta.width)),
                    height: thumbnail
                        .height
                        .or_else(|| thumbnail_meta.as_ref().and_then(|meta| meta.height)),
                    placeholder: thumbnail_meta
                        .as_ref()
                        .and_then(|meta| meta.placeholder.clone()),
                    duration: thumbnail_meta
                        .as_ref()
                        .and_then(|meta| meta.duration.map(|duration| duration as u32)),
                    flags: thumbnail_meta
                        .as_ref()
                        .map(embed_media_flags)
                        .unwrap_or_default(),
                    ..Default::default()
                });
            }

            let (embed_width, embed_height) =
                parse_player_embed_dimensions(&video.player.embed_html).unwrap_or((1280, 720));
            embed.video = Some(EmbedMedia {
                url: Some(embed_url_parsed.to_string()),
                width: Some(embed_width),
                height: Some(embed_height),
                ..Default::default()
            });

            Ok(ResolverResult {
                embeds: vec![embed],
            })
        })
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

fn build_youtube_api_url(video_id: &str, api_key: &str) -> anyhow::Result<Url> {
    let mut url = Url::parse(YOUTUBE_API_BASE)?;
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
