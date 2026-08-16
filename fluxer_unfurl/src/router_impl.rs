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

    fn invalidate_all_variants(&self, url: &str) {
        for mode in [NsfwMode::Block, NsfwMode::Flag, NsfwMode::Allow] {
            for capability in PROVIDER_CAPABILITIES {
                self.l1.invalidate(&unfurl_cache_key(url, mode, capability));
            }
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
                youtube_api_key,
                klipy_api_key,
            } => {
                if *bypass_cache {
                    return None;
                }
                let mode = if *cache_only { "cache-only" } else { "full" };
                Some(format!(
                    "{mode}:{}",
                    unfurl_cache_key(
                        url,
                        nsfw_mode.unwrap_or_default(),
                        provider_capability(youtube_api_key, klipy_api_key)
                    )
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
                youtube_api_key,
                klipy_api_key,
                ..
            } => {
                if *bypass_cache {
                    None
                } else {
                    let key = unfurl_cache_key(
                        url,
                        nsfw_mode.unwrap_or_default(),
                        provider_capability(youtube_api_key, klipy_api_key),
                    );
                    self.l1.get(&key)
                }
            }
            UnfurlRequest::Invalidate { .. } => None,
        }
    }

    fn l1_insert(&self, req: &UnfurlRequest, resp: &UnfurlResponse) {
        match req {
            UnfurlRequest::Unfurl {
                url,
                nsfw_mode,
                youtube_api_key,
                klipy_api_key,
                ..
            } => {
                if let UnfurlResponse::Resolved(result) = resp
                    && !result.embeds.is_empty()
                {
                    self.l1.insert(
                        unfurl_cache_key(
                            url,
                            nsfw_mode.unwrap_or_default(),
                            provider_capability(youtube_api_key, klipy_api_key),
                        ),
                        resp.clone(),
                    );
                }
            }
            UnfurlRequest::Invalidate { url } => {
                self.invalidate_all_variants(url);
            }
        }
    }

    fn l1_invalidate(&self, key: &str) {
        self.invalidate_all_variants(key);
    }
}

const PROVIDER_CAPABILITIES: &[&str] = &["none", "youtube", "klipy", "youtube+klipy"];

fn provider_capability(
    youtube_api_key: &Option<String>,
    klipy_api_key: &Option<String>,
) -> &'static str {
    match (youtube_api_key.is_some(), klipy_api_key.is_some()) {
        (true, true) => "youtube+klipy",
        (true, false) => "youtube",
        (false, true) => "klipy",
        (false, false) => "none",
    }
}

fn unfurl_cache_key(url: &str, nsfw_mode: NsfwMode, capability: &str) -> String {
    let mode = match nsfw_mode {
        NsfwMode::Block => "block",
        NsfwMode::Flag => "flag",
        NsfwMode::Allow => "allow",
    };
    format!("{mode}:{capability}:{url}")
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

    fn resolved_response() -> UnfurlResponse {
        UnfurlResponse::Resolved(std::sync::Arc::new(crate::types::UnfurlResult {
            embeds: vec![crate::types::MessageEmbed::new("link")],
            cache_ttl_seconds: None,
        }))
    }

    fn unfurl(url: &str, youtube_api_key: Option<&str>) -> UnfurlRequest {
        UnfurlRequest::Unfurl {
            url: url.to_owned(),
            nsfw_mode: Some(NsfwMode::Block),
            bypass_cache: false,
            cache_only: false,
            youtube_api_key: youtube_api_key.map(str::to_owned),
            klipy_api_key: None,
        }
    }

    #[test]
    fn configuring_a_youtube_key_does_not_reuse_the_keyless_cache_entry() {
        let router = UnfurlRouter::new(16);
        let url = "https://www.youtube.com/watch?v=dQw4w9WgXcQ";
        router.l1_insert(&unfurl(url, None), &resolved_response());
        assert!(
            router.l1_lookup(&unfurl(url, Some("configured"))).is_none(),
            "a result resolved without a YouTube key must not be served once a key is configured"
        );
        assert!(
            router.l1_lookup(&unfurl(url, None)).is_some(),
            "the keyless entry must still serve keyless requests"
        );
    }

    #[test]
    fn invalidate_clears_every_provider_capability_variant() {
        let router = UnfurlRouter::new(16);
        let url = "https://www.youtube.com/watch?v=dQw4w9WgXcQ";
        router.l1_insert(&unfurl(url, None), &resolved_response());
        router.l1_insert(&unfurl(url, Some("configured")), &resolved_response());
        router.l1_insert(
            &UnfurlRequest::Invalidate {
                url: url.to_owned(),
            },
            &resolved_response(),
        );
        assert!(router.l1_lookup(&unfurl(url, None)).is_none());
        assert!(router.l1_lookup(&unfurl(url, Some("configured"))).is_none());
    }

    #[test]
    fn coalesce_key_separates_cache_only_from_full_unfurls() {
        assert_ne!(
            UnfurlRouter::coalesce_key(&request(true)),
            UnfurlRouter::coalesce_key(&request(false))
        );
    }
}
