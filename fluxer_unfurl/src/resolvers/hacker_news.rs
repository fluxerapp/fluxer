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

            embed.timestamp = item
                .time
                .and_then(|time| i64::try_from(time).ok())
                .and_then(format_timestamp);

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

fn html_to_markdown(html: &str) -> String {
    crate::html_markdown::to_markdown(html, decode_html_text)
}

use super::default_helpers::{decode_html_text, format_timestamp, parse_text};
