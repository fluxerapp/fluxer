// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::cache_policy::{
    UnfurlEntryExpiry, default_empty_entry_lifetime, every_unfurl_cache_key, provider_capability,
    unfurl_cache_key,
};
use crate::types::{UnfurlRequest, UnfurlResponse};
use fluxer_svc::router::RouterService;
use moka::sync::Cache;
use std::time::Duration;

pub struct UnfurlRouter {
    l1: Cache<String, UnfurlResponse>,
}

impl UnfurlRouter {
    pub fn new(max_entries: u64, lifetime: Duration) -> Self {
        Self::with_empty_entry_lifetime(max_entries, lifetime, default_empty_entry_lifetime())
    }

    fn with_empty_entry_lifetime(
        max_entries: u64,
        lifetime: Duration,
        empty_entry_lifetime: Duration,
    ) -> Self {
        Self {
            l1: Cache::builder()
                .max_capacity(max_entries)
                .time_to_live(lifetime)
                .expire_after(UnfurlEntryExpiry::new(empty_entry_lifetime))
                .build(),
        }
    }

    fn invalidate_all_variants(&self, url: &str) {
        for key in every_unfurl_cache_key(url) {
            self.l1.invalidate(&key);
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
                        provider_capability(youtube_api_key.is_some(), klipy_api_key.is_some())
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
                        provider_capability(youtube_api_key.is_some(), klipy_api_key.is_some()),
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
                cache_only,
                youtube_api_key,
                klipy_api_key,
                ..
            } => {
                if let UnfurlResponse::Resolved(result) = resp
                    && (!cache_only || !result.embeds.is_empty())
                {
                    self.l1.insert(
                        unfurl_cache_key(
                            url,
                            nsfw_mode.unwrap_or_default(),
                            provider_capability(youtube_api_key.is_some(), klipy_api_key.is_some()),
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::NsfwMode;

    fn test_lifetime() -> Duration {
        Duration::from_secs(30 * 60)
    }

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

    fn empty_response() -> UnfurlResponse {
        UnfurlResponse::Resolved(std::sync::Arc::new(crate::types::UnfurlResult {
            embeds: Vec::new(),
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

    fn unfurl_with_klipy(url: &str, klipy_api_key: Option<&str>) -> UnfurlRequest {
        UnfurlRequest::Unfurl {
            url: url.to_owned(),
            nsfw_mode: Some(NsfwMode::Block),
            bypass_cache: false,
            cache_only: false,
            youtube_api_key: None,
            klipy_api_key: klipy_api_key.map(str::to_owned),
        }
    }

    fn probe(url: &str) -> UnfurlRequest {
        UnfurlRequest::Unfurl {
            url: url.to_owned(),
            nsfw_mode: Some(NsfwMode::Block),
            bypass_cache: false,
            cache_only: true,
            youtube_api_key: None,
            klipy_api_key: None,
        }
    }

    fn unfurl_allowing_nsfw(url: &str) -> UnfurlRequest {
        UnfurlRequest::Unfurl {
            url: url.to_owned(),
            nsfw_mode: Some(NsfwMode::Allow),
            bypass_cache: false,
            cache_only: false,
            youtube_api_key: None,
            klipy_api_key: None,
        }
    }

    #[test]
    fn configuring_a_youtube_key_does_not_reuse_the_keyless_cache_entry() {
        let router = UnfurlRouter::new(16, test_lifetime());
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
        let router = UnfurlRouter::new(16, test_lifetime());
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
    #[test]
    fn a_url_that_produced_no_embed_is_remembered_instead_of_being_resolved_again() {
        let router = UnfurlRouter::new(16, test_lifetime());
        let url = "https://example.com/no-metadata";
        router.l1_insert(&unfurl(url, None), &empty_response());
        assert!(
            router.l1_lookup(&unfurl(url, None)).is_some(),
            "an empty resolution must be served from the upper cache, not re-resolved"
        );
    }

    #[test]
    fn an_empty_cache_only_probe_is_never_written_to_the_upper_cache() {
        let router = UnfurlRouter::new(16, test_lifetime());
        let url = "https://example.com/never-probed-into-cache";
        router.l1_insert(&probe(url), &empty_response());
        assert!(
            router.l1_lookup(&unfurl(url, None)).is_none(),
            "a cache-only miss means unknown, not resolved-to-nothing"
        );
    }

    #[test]
    fn a_cache_only_probe_that_found_embeds_still_warms_the_upper_cache() {
        let router = UnfurlRouter::new(16, test_lifetime());
        let url = "https://example.com/warm";
        router.l1_insert(&probe(url), &resolved_response());
        assert!(router.l1_lookup(&unfurl(url, None)).is_some());
    }

    #[test]
    fn configuring_a_klipy_key_does_not_reuse_the_keyless_cache_entry() {
        let router = UnfurlRouter::new(16, test_lifetime());
        let url = "https://klipy.com/gifs/example";
        router.l1_insert(&unfurl_with_klipy(url, None), &resolved_response());
        assert!(
            router
                .l1_lookup(&unfurl_with_klipy(url, Some("configured")))
                .is_none(),
            "a result resolved without a KLIPY key must not be served once a key is configured"
        );
        assert!(
            router.l1_lookup(&unfurl_with_klipy(url, None)).is_some(),
            "the keyless entry must still serve keyless requests"
        );
    }

    #[test]
    fn an_unscanned_entry_is_not_served_to_a_scanning_request() {
        let router = UnfurlRouter::new(16, test_lifetime());
        let url = "https://example.com/media";
        router.l1_insert(&unfurl_allowing_nsfw(url), &resolved_response());
        assert!(
            router.l1_lookup(&unfurl(url, None)).is_none(),
            "a result resolved with nsfw scanning off carries no nsfw flags and must not be reused"
        );
        assert!(router.l1_lookup(&unfurl_allowing_nsfw(url)).is_some());
    }

    #[test]
    fn invalidate_clears_both_scanning_and_unscanned_entries() {
        let router = UnfurlRouter::new(16, test_lifetime());
        let url = "https://example.com/both";
        router.l1_insert(&unfurl(url, None), &resolved_response());
        router.l1_insert(&unfurl_allowing_nsfw(url), &resolved_response());
        router.l1_insert(
            &UnfurlRequest::Invalidate {
                url: url.to_owned(),
            },
            &resolved_response(),
        );
        assert!(router.l1_lookup(&unfurl(url, None)).is_none());
        assert!(router.l1_lookup(&unfurl_allowing_nsfw(url)).is_none());
    }
    #[tokio::test]
    async fn an_empty_upper_cache_entry_lapses_long_before_a_populated_one() {
        let router =
            UnfurlRouter::with_empty_entry_lifetime(16, test_lifetime(), Duration::from_millis(50));
        let empty_url = "https://example.com/transient";
        let populated_url = "https://example.com/stable";
        router.l1_insert(&unfurl(empty_url, None), &empty_response());
        router.l1_insert(&unfurl(populated_url, None), &resolved_response());

        tokio::time::sleep(Duration::from_millis(200)).await;

        assert!(
            router.l1_lookup(&unfurl(empty_url, None)).is_none(),
            "a link that failed to unfurl must become retryable again quickly"
        );
        assert!(
            router.l1_lookup(&unfurl(populated_url, None)).is_some(),
            "a real embed must keep the full lifetime"
        );
    }
    #[tokio::test]
    async fn the_configured_lifetime_governs_the_upper_cache() {
        let router = UnfurlRouter::new(16, Duration::from_millis(80));
        let url = "https://example.com/configured";
        router.l1_insert(&unfurl(url, None), &resolved_response());
        tokio::time::sleep(Duration::from_millis(220)).await;
        assert!(
            router.l1_lookup(&unfurl(url, None)).is_none(),
            "the configured lifetime must govern the upper cache"
        );
    }
}
