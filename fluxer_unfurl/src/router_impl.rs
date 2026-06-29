// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::cache_policy::FIXED_UNFURL_CACHE_TTL_SECS;
use crate::types::{NsfwMode, UnfurlRequest, UnfurlResponse};
use fluxer_svc::router::RouterService;
use moka::sync::Cache;
use std::time::Duration;

pub struct UnfurlRouter {
    l1: Cache<String, UnfurlResponse>,
}

impl UnfurlRouter {
    pub fn new(max_entries: u64) -> Self {
        Self {
            l1: Cache::builder()
                .max_capacity(max_entries)
                .time_to_live(Duration::from_secs(FIXED_UNFURL_CACHE_TTL_SECS))
                .build(),
        }
    }
}

impl RouterService for UnfurlRouter {
    type Request = UnfurlRequest;
    type Response = UnfurlResponse;

    fn service_name(&self) -> &str {
        "unfurl"
    }

    fn route_key(req: &UnfurlRequest) -> String {
        match req {
            UnfurlRequest::Unfurl { url, .. } => url.clone(),
            UnfurlRequest::Invalidate { url } => url.clone(),
        }
    }

    fn coalesce_key(req: &UnfurlRequest) -> Option<String> {
        match req {
            UnfurlRequest::Unfurl {
                url,
                nsfw_mode,
                bypass_cache,
                cache_only,
                ..
            } => {
                if *bypass_cache {
                    return None;
                }
                let mode = if *cache_only { "cache-only" } else { "full" };
                Some(format!(
                    "{mode}:{}",
                    unfurl_cache_key(url, nsfw_mode.unwrap_or_default())
                ))
            }
            UnfurlRequest::Invalidate { .. } => None,
        }
    }

    fn l1_lookup(&self, req: &UnfurlRequest) -> Option<UnfurlResponse> {
        match req {
            UnfurlRequest::Unfurl {
                url,
                nsfw_mode,
                bypass_cache,
                cache_only: _,
                ..
            } => {
                if *bypass_cache {
                    None
                } else {
                    let key = unfurl_cache_key(url, nsfw_mode.unwrap_or_default());
                    self.l1.get(&key)
                }
            }
            UnfurlRequest::Invalidate { .. } => None,
        }
    }

    fn l1_insert(&self, req: &UnfurlRequest, resp: &UnfurlResponse) {
        match req {
            UnfurlRequest::Unfurl { url, nsfw_mode, .. } => {
                if let UnfurlResponse::Resolved(result) = resp
                    && !result.embeds.is_empty()
                {
                    self.l1.insert(
                        unfurl_cache_key(url, nsfw_mode.unwrap_or_default()),
                        resp.clone(),
                    );
                }
            }
            UnfurlRequest::Invalidate { url } => {
                self.l1.invalidate(&unfurl_cache_key(url, NsfwMode::Block));
                self.l1.invalidate(&unfurl_cache_key(url, NsfwMode::Flag));
                self.l1.invalidate(&unfurl_cache_key(url, NsfwMode::Allow));
            }
        }
    }

    fn l1_invalidate(&self, key: &str) {
        self.l1.invalidate(&unfurl_cache_key(key, NsfwMode::Block));
        self.l1.invalidate(&unfurl_cache_key(key, NsfwMode::Flag));
        self.l1.invalidate(&unfurl_cache_key(key, NsfwMode::Allow));
    }
}

fn unfurl_cache_key(url: &str, nsfw_mode: NsfwMode) -> String {
    let mode = match nsfw_mode {
        NsfwMode::Block => "block",
        NsfwMode::Flag => "flag",
        NsfwMode::Allow => "allow",
    };
    format!("{mode}:{url}")
}

#[cfg(test)]
mod tests {
    use super::*;

    fn request(cache_only: bool) -> UnfurlRequest {
        UnfurlRequest::Unfurl {
            url: "https://example.com/article".to_owned(),
            nsfw_mode: Some(NsfwMode::Block),
            bypass_cache: false,
            cache_only,
            youtube_api_key: None,
            klipy_api_key: None,
        }
    }

    #[test]
    fn coalesce_key_separates_cache_only_from_full_unfurls() {
        assert_ne!(
            UnfurlRouter::coalesce_key(&request(true)),
            UnfurlRouter::coalesce_key(&request(false))
        );
    }
}
