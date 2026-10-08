// SPDX-License-Identifier: AGPL-3.0-or-later

use super::{ResolveContext, Resolver, ResolverResult};
use crate::http_fetch;
use crate::text_limits;
use crate::types::{EmbedAuthor, EmbedFooter, MessageEmbed};
use serde::Deserialize;
use std::future::Future;
use std::pin::Pin;
use std::time::Duration;
use url::Url;

const HN_API_BASE: &str = "https://hacker-news.firebaseio.com/v0";
const HN_SITE_BASE: &str = "https://news.ycombinator.com";
const HN_COLOR: u32 = 0xFF6600;
const HN_ICON_URL: &str = "https://fluxer.app/static/img/app-hn-icon.4237980ebe2ba783.webp";
const MAX_DESCRIPTION: usize = 400;

pub struct HackerNewsResolver;

#[derive(Debug, Deserialize)]
struct HnItem {
    id: u64,
    #[serde(rename = "type")]
    item_type: Option<String>,
    by: Option<String>,
    time: Option<u64>,
    text: Option<String>,
    dead: Option<bool>,
    deleted: Option<bool>,
    title: Option<String>,
    #[serde(default)]
    #[allow(dead_code)]
    score: Option<u64>,
}

impl Resolver for HackerNewsResolver {
    fn matches(&self, url: &Url) -> bool {
        url.host_str()
            .is_some_and(|h| h.eq_ignore_ascii_case("news.ycombinator.com"))
            && url.path().starts_with("/item")
    }

    fn resolve<'a>(
        &'a self,
        ctx: &'a ResolveContext<'_>,
    ) -> Pin<Box<dyn Future<Output = anyhow::Result<ResolverResult>> + Send + 'a>> {
        Box::pin(async move {
            let item_id = ctx
                .url
                .query_pairs()
                .find(|(k, _)| k == "id")
                .map(|(_, v)| v.into_owned());

            let item_id = match item_id {
                Some(id) if !id.is_empty() => id,
                _ => {
                    return Ok(ResolverResult { embeds: vec![] });
                }
            };

            let api_url = format!("{HN_API_BASE}/item/{item_id}.json");

            let result = http_fetch::fetch_url(
                &ctx.http_client,
                &api_url,
                128 * 1024,
                Duration::from_secs(5),
            )
            .await?;

            if result.status != 200 {
                return Ok(ResolverResult { embeds: vec![] });
            }

            let item: HnItem = serde_json::from_slice(&result.bytes)?;

            if item.deleted.unwrap_or(false) || item.dead.unwrap_or(false) {
                return Ok(ResolverResult { embeds: vec![] });
            }

            let item_url = format!("{HN_SITE_BASE}/item?id={}", item.id);

            let mut embed = MessageEmbed::new("rich");
            embed.url = Some(item_url);
            embed.color = Some(HN_COLOR);

            if let Some(time) = item.time {
                embed.timestamp = Some(format_unix_timestamp(time));
            }

            embed.footer = Some(hn_footer(ctx.self_hosted));

            let has_title = item
                .item_type
                .as_deref()
                .is_some_and(|t| matches!(t, "story" | "job" | "poll"));
            if has_title && let Some(ref title) = item.title {
                embed.title = Some(parse_text(title, text_limits::TITLE_MAX));
            }

            if let Some(ref by) = item.by {
                embed.author = Some(EmbedAuthor {
                    name: parse_text(by, text_limits::AUTHOR_NAME_MAX),
                    ..Default::default()
                });
            }

            if let Some(ref text) = item.text {
                let markdown = html_to_markdown(text);
                let single_line = markdown.split_whitespace().collect::<Vec<_>>().join(" ");
                if !single_line.is_empty() {
                    embed.description = Some(text_limits::truncate(&single_line, MAX_DESCRIPTION));
                }
            }

            Ok(ResolverResult {
                embeds: vec![embed],
            })
        })
    }
}

fn hn_footer(self_hosted: bool) -> EmbedFooter {
    EmbedFooter {
        text: "Hacker News".to_owned(),
        icon_url: (!self_hosted).then(|| HN_ICON_URL.to_owned()),
        ..Default::default()
    }
}

fn format_unix_timestamp(secs: u64) -> String {
    chrono_lite(secs)
}

fn chrono_lite(unix_secs: u64) -> String {
    let secs_per_day: u64 = 86400;
    let days = unix_secs / secs_per_day;
    let remaining = unix_secs % secs_per_day;
    let hours = remaining / 3600;
    let minutes = (remaining % 3600) / 60;
    let seconds = remaining % 60;

    let (year, month, day) = days_to_ymd(days);

    format!("{year:04}-{month:02}-{day:02}T{hours:02}:{minutes:02}:{seconds:02}Z")
}

fn days_to_ymd(days: u64) -> (u64, u64, u64) {
    let z = days + 719468;
    let era = z / 146097;
    let doe = z - era * 146097;
    let yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365;
    let y = yoe + era * 400;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let d = doy - (153 * mp + 2) / 5 + 1;
    let m = if mp < 10 { mp + 3 } else { mp - 9 };
    let y = if m <= 2 { y + 1 } else { y };
    (y, m, d)
}

fn parse_text(value: &str, max_len: usize) -> String {
    text_limits::truncate(decode_html_entities(value).trim(), max_len)
}

fn html_to_markdown(html: &str) -> String {
    crate::html_markdown::to_markdown(html, decode_html_entities)
}

fn decode_html_entities(input: &str) -> String {
    scraper::Html::parse_fragment(input)
        .root_element()
        .text()
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn u(s: &str) -> Url {
        Url::parse(s).unwrap()
    }

    #[test]
    fn matches_hn_item_urls() {
        let r = HackerNewsResolver;
        assert!(r.matches(&u("https://news.ycombinator.com/item?id=12345")));
        assert!(!r.matches(&u("https://news.ycombinator.com/newest")));
        assert!(!r.matches(&u("https://example.com/item?id=1")));
    }

    #[test]
    fn html_to_markdown_preserves_ts_markup_and_decodes_entities() {
        assert_eq!(
            html_to_markdown(
                r#"<p>Hello <b>world</b><br><a href="https://example.com?a=1&amp;b=2">link</a></p>"#
            ),
            "Hello **world**\n[link](https://example.com?a=1&b=2)"
        );
        assert_eq!(html_to_markdown("no &amp; tags &#x1F44D;"), "no & tags 👍");
    }

    #[test]
    fn parse_text_decodes_trims_and_truncates() {
        assert_eq!(parse_text("  Tom &amp; Jerry  ", 32), "Tom & Jerry");
        assert_eq!(parse_text("abcdef", 4), "abc\u{2026}");
    }

    #[test]
    fn chrono_lite_formats_correctly() {
        assert_eq!(chrono_lite(0), "1970-01-01T00:00:00Z");
        assert_eq!(chrono_lite(1700000000), "2023-11-14T22:13:20Z");
    }

    #[test]
    fn html_to_markdown_converts_pre_code_blocks() {
        let result = html_to_markdown("<pre><code>fn main() {}</code></pre>");
        assert!(result.contains("fn main() {}"));
    }

    #[test]
    fn html_to_markdown_converts_inline_code() {
        assert_eq!(
            html_to_markdown("Use <code>cargo test</code> to run"),
            "Use `cargo test` to run"
        );
    }

    #[test]
    fn html_to_markdown_converts_links() {
        assert_eq!(
            html_to_markdown(r#"<a href="https://e.com">text</a>"#),
            "[text](https://e.com)"
        );
    }

    #[test]
    fn html_to_markdown_converts_lists() {
        assert_eq!(
            html_to_markdown("<ul><li>one</li><li>two</li></ul>").trim(),
            "• one\n• two"
        );
    }

    #[test]
    fn html_to_markdown_collapses_newlines() {
        assert_eq!(
            html_to_markdown("<p>a</p><p></p><p></p><p>b</p>").trim(),
            "a\n\nb"
        );
    }

    #[test]
    fn html_to_markdown_strips_unknown_tags() {
        assert_eq!(
            html_to_markdown("<div>hello <span>world</span></div>"),
            "hello world"
        );
    }

    #[test]
    fn parse_text_decodes_html_entities_comprehensively() {
        assert_eq!(parse_text("a &lt; b &gt; c", 256), "a < b > c");
        assert_eq!(parse_text("&quot;quoted&quot;", 256), "\"quoted\"");
        assert_eq!(parse_text("5 &#x2B; 3", 256), "5 + 3");
    }

    #[test]
    fn hn_color_matches_brand() {
        assert_eq!(HN_COLOR, 0xFF6600);
    }

    #[test]
    fn matches_only_item_urls() {
        let r = HackerNewsResolver;
        assert!(r.matches(&u("https://news.ycombinator.com/item?id=12345")));
        assert!(!r.matches(&u("https://news.ycombinator.com/newest")));
        assert!(!r.matches(&u("https://news.ycombinator.com/user?id=dang")));
        assert!(!r.matches(&u("https://news.ycombinator.com/")));
    }

    #[test]
    fn hn_footer_icon_is_hosted_only() {
        assert_eq!(hn_footer(false).icon_url.as_deref(), Some(HN_ICON_URL));
        assert_eq!(hn_footer(true).icon_url, None);
        assert_eq!(hn_footer(true).text, "Hacker News");
    }

    #[test]
    fn chrono_lite_leap_year() {
        assert_eq!(chrono_lite(1582934400), "2020-02-29T00:00:00Z");
    }
}
