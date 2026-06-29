// SPDX-License-Identifier: AGPL-3.0-or-later

pub mod bluesky;
mod default_helpers;
pub mod default_resolver;
pub mod fxtwitter;
pub mod hacker_news;
pub mod klipy;
pub mod media;
pub mod tenor;
pub mod wikipedia;
pub mod xkcd;
pub mod youtube;

use crate::media_proxy::MediaProxyClient;
use crate::types::{MessageEmbed, NsfwMode};
use std::future::Future;
use std::pin::Pin;
use url::Url;

#[allow(dead_code)]
pub struct ResolveContext<'mp> {
    pub url: Url,
    pub original_url: Url,
    pub http_client: reqwest::Client,
    pub nsfw_mode: NsfwMode,
    pub media_proxy: &'mp MediaProxyClient,
    pub static_cdn_endpoint: &'mp str,
    pub youtube_api_key: Option<String>,
    pub klipy_api_key: Option<String>,
}

impl ResolveContext<'_> {
    pub fn static_asset_url(&self, path: &str) -> Option<String> {
        let endpoint = self.static_cdn_endpoint.trim().trim_end_matches('/');
        if endpoint.is_empty() {
            return None;
        }
        Some(format!("{}/{}", endpoint, path.trim_start_matches('/')))
    }
}

pub struct ResolverResult {
    pub embeds: Vec<MessageEmbed>,
}

pub trait Resolver: Send + Sync {
    fn matches(&self, url: &Url) -> bool;

    fn transform_url(&self, _url: &Url) -> Option<Url> {
        None
    }

    fn resolve<'a>(
        &'a self,
        ctx: &'a ResolveContext<'_>,
    ) -> Pin<Box<dyn Future<Output = anyhow::Result<ResolverResult>> + Send + 'a>>;
}

pub fn build_resolver_chain() -> Vec<Box<dyn Resolver>> {
    vec![
        Box::new(hacker_news::HackerNewsResolver),
        Box::new(klipy::KlipyResolver),
        Box::new(tenor::TenorResolver),
        Box::new(xkcd::XkcdResolver),
        Box::new(youtube::YouTubeResolver),
        Box::new(wikipedia::WikipediaResolver),
        Box::new(bluesky::BlueskyResolver),
        Box::new(fxtwitter::FxTwitterResolver),
        Box::new(default_resolver::DefaultResolver),
    ]
}
