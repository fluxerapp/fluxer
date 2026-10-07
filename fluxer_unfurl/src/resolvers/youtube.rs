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
    #[allow(dead_code)]
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

#[cfg(test)]
mod tests {
    use super::*;

    fn u(s: &str) -> Url {
        Url::parse(s).unwrap()
    }

    #[test]
    fn matches_youtube_variants() {
        let yt = YouTubeResolver;
        assert!(yt.matches(&u("https://www.youtube.com/watch?v=dQw4w9WgXcQ")));
        assert!(yt.matches(&u("https://youtube.com/watch?v=dQw4w9WgXcQ")));
        assert!(yt.matches(&u("https://youtu.be/dQw4w9WgXcQ")));
        assert!(yt.matches(&u("https://music.youtube.com/watch?v=dQw4w9WgXcQ")));
    }

    #[test]
    fn no_match_unrelated() {
        let yt = YouTubeResolver;
        assert!(!yt.matches(&u("https://example.com/watch?v=dQw4w9WgXcQ")));
    }

    #[test]
    fn parses_url_variants() {
        assert_eq!(
            parse_youtube_url(&u("https://www.youtube.com/watch?v=dQw4w9WgXcQ"))
                .unwrap()
                .video_id,
            "dQw4w9WgXcQ"
        );
        assert_eq!(
            parse_youtube_url(&u("https://youtu.be/dQw4w9WgXcQ"))
                .unwrap()
                .video_id,
            "dQw4w9WgXcQ"
        );
        assert_eq!(
            parse_youtube_url(&u("https://www.youtube.com/shorts/dQw4w9WgXcQ"))
                .unwrap()
                .video_id,
            "dQw4w9WgXcQ"
        );
        assert_eq!(
            parse_youtube_url(&u("https://www.youtube.com/embed/dQw4w9WgXcQ"))
                .unwrap()
                .video_id,
            "dQw4w9WgXcQ"
        );
    }

    #[test]
    fn extracts_timestamp() {
        assert_eq!(
            parse_youtube_url(&u("https://youtu.be/dQw4w9WgXcQ?t=120"))
                .unwrap()
                .timestamp,
            Some(120)
        );
        assert_eq!(
            parse_youtube_url(&u("https://youtu.be/dQw4w9WgXcQ?t=90s"))
                .unwrap()
                .timestamp,
            Some(90)
        );
    }

    #[test]
    fn rejects_invalid_id() {
        assert!(parse_youtube_url(&u("https://youtube.com/watch?v=ab")).is_none());
    }

    #[test]
    fn parses_player_embed_dimensions_like_youtube_api() {
        assert_eq!(
            parse_player_embed_dimensions(
                r#"<iframe width="480" height="270" src="//www.youtube.com/embed/dQw4w9WgXcQ"></iframe>"#
            ),
            Some((480, 270))
        );
        assert_eq!(
            parse_player_embed_dimensions(
                r#"<iframe height="270" width="480" src="//www.youtube.com/embed/dQw4w9WgXcQ"></iframe>"#
            ),
            Some((480, 270))
        );
        assert_eq!(parse_player_embed_dimensions("<iframe></iframe>"), None);
    }

    #[test]
    fn prefers_same_thumbnail_order_as_old_resolver() {
        let thumbnails = YouTubeThumbnails {
            default: Some(YouTubeThumbnail {
                url: "default".to_owned(),
                width: Some(120),
                height: Some(90),
            }),
            medium: Some(YouTubeThumbnail {
                url: "medium".to_owned(),
                width: Some(320),
                height: Some(180),
            }),
            high: Some(YouTubeThumbnail {
                url: "high".to_owned(),
                width: Some(480),
                height: Some(360),
            }),
            standard: Some(YouTubeThumbnail {
                url: "standard".to_owned(),
                width: Some(640),
                height: Some(480),
            }),
            maxres: Some(YouTubeThumbnail {
                url: "maxres".to_owned(),
                width: Some(1280),
                height: Some(720),
            }),
        };
        assert_eq!(thumbnails.best().unwrap().url, "maxres");

        let thumbnails = YouTubeThumbnails {
            maxres: None,
            ..thumbnails
        };
        assert_eq!(thumbnails.best().unwrap().url, "high");
    }

    #[test]
    fn builds_data_api_url() {
        let url = build_youtube_api_url("dQw4w9WgXcQ", "secret").unwrap();
        let pairs: std::collections::HashMap<_, _> = url.query_pairs().into_owned().collect();
        assert_eq!(url.as_str().split('?').next().unwrap(), YOUTUBE_API_BASE);
        assert_eq!(pairs.get("key").unwrap(), "secret");
        assert_eq!(pairs.get("id").unwrap(), "dQw4w9WgXcQ");
        assert_eq!(pairs.get("part").unwrap(), "snippet,player,status");
    }

    #[test]
    fn parses_v_path_variant() {
        assert_eq!(
            parse_youtube_url(&u("https://www.youtube.com/v/dQw4w9WgXcQ"))
                .unwrap()
                .video_id,
            "dQw4w9WgXcQ"
        );
    }

    #[test]
    fn rejects_embed_videoseries_and_live_stream() {
        assert!(parse_youtube_url(&u("https://www.youtube.com/embed/videoseries")).is_none());
        assert!(parse_youtube_url(&u("https://www.youtube.com/embed/live_stream")).is_none());
    }

    #[test]
    fn parses_nocookie_domain() {
        assert!(YouTubeResolver.matches(&u("https://www.youtube-nocookie.com/embed/dQw4w9WgXcQ")));
        assert_eq!(
            parse_youtube_url(&u("https://www.youtube-nocookie.com/embed/dQw4w9WgXcQ"))
                .unwrap()
                .video_id,
            "dQw4w9WgXcQ"
        );
    }

    #[test]
    fn timestamp_with_start_param() {
        assert_eq!(
            parse_youtube_url(&u("https://www.youtube.com/watch?v=dQw4w9WgXcQ&start=60"))
                .unwrap()
                .timestamp,
            Some(60)
        );
    }

    #[test]
    fn no_timestamp_returns_none() {
        assert!(
            parse_youtube_url(&u("https://www.youtube.com/watch?v=dQw4w9WgXcQ"))
                .unwrap()
                .timestamp
                .is_none()
        );
    }

    #[test]
    fn rejects_too_short_id() {
        assert!(parse_youtube_url(&u("https://youtube.com/watch?v=abc")).is_none());
    }

    #[test]
    fn rejects_too_long_id() {
        let long_id = "a".repeat(20);
        assert!(parse_youtube_url(&u(&format!("https://youtube.com/watch?v={long_id}"))).is_none());
    }

    #[test]
    fn rejects_id_with_special_chars() {
        assert!(parse_youtube_url(&u("https://youtube.com/watch?v=abc!@#def")).is_none());
    }

    #[test]
    fn transform_url_produces_canonical() {
        let yt = YouTubeResolver;
        let canonical = yt
            .transform_url(&u("https://youtu.be/dQw4w9WgXcQ?t=42"))
            .unwrap();
        assert_eq!(canonical.host_str(), Some("www.youtube.com"));
        assert_eq!(canonical.path(), "/watch");
        let pairs: std::collections::HashMap<_, _> = canonical.query_pairs().into_owned().collect();
        assert_eq!(pairs.get("v").unwrap(), "dQw4w9WgXcQ");
        assert_eq!(pairs.get("t").unwrap(), "42s");
    }

    #[test]
    fn transform_url_without_timestamp() {
        let yt = YouTubeResolver;
        let canonical = yt
            .transform_url(&u("https://youtube.com/watch?v=dQw4w9WgXcQ"))
            .unwrap();
        let pairs: std::collections::HashMap<_, _> = canonical.query_pairs().into_owned().collect();
        assert!(!pairs.contains_key("t"));
    }

    #[test]
    fn youtu_be_with_extra_path_segment() {
        let parsed = parse_youtube_url(&u("https://youtu.be/dQw4w9WgXcQ/extra"));
        assert!(parsed.is_some());
        assert_eq!(parsed.unwrap().video_id, "dQw4w9WgXcQ");
    }

    #[test]
    fn shorts_url_parsing() {
        let parsed = parse_youtube_url(&u(
            "https://www.youtube.com/shorts/dQw4w9WgXcQ?feature=share",
        ));
        assert!(parsed.is_some());
        assert_eq!(parsed.unwrap().video_id, "dQw4w9WgXcQ");
    }

    #[test]
    fn parse_youtube_string_truncates_with_ellipsis() {
        let long = "a".repeat(400);
        let result = parse_youtube_string(&long, YOUTUBE_DESCRIPTION_MAX);
        assert!(result.ends_with("..."));
        assert!(result.chars().count() <= YOUTUBE_DESCRIPTION_MAX);
    }

    #[test]
    fn parse_youtube_string_short_unchanged() {
        assert_eq!(parse_youtube_string("Hello", 350), "Hello");
    }

    #[test]
    fn parse_youtube_string_trims_whitespace() {
        assert_eq!(parse_youtube_string("  hello  ", 350), "hello");
    }

    #[test]
    fn parse_player_embed_dimensions_missing_width() {
        assert_eq!(
            parse_player_embed_dimensions(r#"<iframe height="270"></iframe>"#),
            None
        );
    }

    #[test]
    fn youtube_color_matches_brand() {
        assert_eq!(YOUTUBE_COLOR, 0xFF0000);
    }
}
