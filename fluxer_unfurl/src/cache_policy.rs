// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::types::{NsfwMode, UnfurlResponse, UnfurlResult};
use moka::Expiry;
use std::sync::Arc;
use std::time::{Duration, Instant};

pub const EMPTY_UNFURL_CACHE_TTL_SECS: u64 = 60;

const SCAN_TOKENS: [&str; 2] = ["scanned", "unscanned"];

const PROVIDER_CAPABILITIES: [&str; 4] = ["none", "youtube", "klipy", "youtube+klipy"];

pub fn nsfw_scan_token(nsfw_mode: NsfwMode) -> &'static str {
    match nsfw_mode {
        NsfwMode::Block | NsfwMode::Flag => SCAN_TOKENS[0],
        NsfwMode::Allow => SCAN_TOKENS[1],
    }
}

pub fn provider_capability(has_youtube_key: bool, has_klipy_key: bool) -> &'static str {
    match (has_youtube_key, has_klipy_key) {
        (true, true) => "youtube+klipy",
        (true, false) => "youtube",
        (false, true) => "klipy",
        (false, false) => "none",
    }
}

pub fn unfurl_cache_key(url: &str, nsfw_mode: NsfwMode, capability: &str) -> String {
    let scan = nsfw_scan_token(nsfw_mode);
    format!("{scan}:{capability}:{url}")
}

pub fn every_unfurl_cache_key(url: &str) -> Vec<String> {
    let mut keys = Vec::with_capacity(SCAN_TOKENS.len() * PROVIDER_CAPABILITIES.len());
    for scan in SCAN_TOKENS {
        for capability in PROVIDER_CAPABILITIES {
            keys.push(format!("{scan}:{capability}:{url}"));
        }
    }
    keys
}

pub fn default_empty_entry_lifetime() -> Duration {
    Duration::from_secs(EMPTY_UNFURL_CACHE_TTL_SECS)
}

pub struct UnfurlEntryExpiry {
    empty_entry_lifetime: Duration,
}

impl UnfurlEntryExpiry {
    pub fn new(empty_entry_lifetime: Duration) -> Self {
        Self {
            empty_entry_lifetime,
        }
    }

    fn lifetime(&self, has_embeds: bool) -> Option<Duration> {
        if has_embeds {
            None
        } else {
            Some(self.empty_entry_lifetime)
        }
    }
}

impl Expiry<String, Arc<UnfurlResult>> for UnfurlEntryExpiry {
    fn expire_after_create(
        &self,
        _key: &String,
        value: &Arc<UnfurlResult>,
        _created_at: Instant,
    ) -> Option<Duration> {
        self.lifetime(!value.embeds.is_empty())
    }

    fn expire_after_update(
        &self,
        _key: &String,
        value: &Arc<UnfurlResult>,
        _updated_at: Instant,
        _duration_until_expiry: Option<Duration>,
    ) -> Option<Duration> {
        self.lifetime(!value.embeds.is_empty())
    }
}

impl Expiry<String, UnfurlResponse> for UnfurlEntryExpiry {
    fn expire_after_create(
        &self,
        _key: &String,
        value: &UnfurlResponse,
        _created_at: Instant,
    ) -> Option<Duration> {
        self.lifetime(response_has_embeds(value))
    }

    fn expire_after_update(
        &self,
        _key: &String,
        value: &UnfurlResponse,
        _updated_at: Instant,
        _duration_until_expiry: Option<Duration>,
    ) -> Option<Duration> {
        self.lifetime(response_has_embeds(value))
    }
}

fn response_has_embeds(response: &UnfurlResponse) -> bool {
    match response {
        UnfurlResponse::Resolved(result) => !result.embeds.is_empty(),
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::MessageEmbed;

    const URL: &str = "https://example.com/a";

    const EMPTY_UNFURL_CACHE_TTL_CEILING_SECS: u64 = 5 * 60;

    fn resolved(embed_count: usize) -> UnfurlResponse {
        UnfurlResponse::Resolved(Arc::new(UnfurlResult {
            embeds: (0..embed_count)
                .map(|_| MessageEmbed::new("link"))
                .collect(),
            cache_ttl_seconds: None,
        }))
    }

    #[test]
    fn every_key_covers_the_full_scan_and_capability_matrix() {
        let keys = every_unfurl_cache_key(URL);
        assert_eq!(keys.len(), 8);
        let unique: std::collections::HashSet<&String> = keys.iter().collect();
        assert_eq!(unique.len(), keys.len(), "invalidation keys must be unique");
        for nsfw_mode in [NsfwMode::Block, NsfwMode::Flag, NsfwMode::Allow] {
            for capability in PROVIDER_CAPABILITIES {
                assert!(
                    keys.contains(&unfurl_cache_key(URL, nsfw_mode, capability)),
                    "invalidation must cover {nsfw_mode:?}/{capability}"
                );
            }
        }
    }

    #[test]
    fn scanning_and_unscanned_requests_do_not_share_an_entry() {
        assert_ne!(
            unfurl_cache_key(URL, NsfwMode::Block, "none"),
            unfurl_cache_key(URL, NsfwMode::Allow, "none"),
            "an unscanned result carries no nsfw media flag and must not serve a scanning request"
        );
    }

    #[test]
    fn the_two_scanning_modes_share_a_single_entry() {
        assert_eq!(
            unfurl_cache_key(URL, NsfwMode::Block, "none"),
            unfurl_cache_key(URL, NsfwMode::Flag, "none")
        );
    }

    #[test]
    fn capability_distinguishes_every_key_combination() {
        let all = [
            provider_capability(false, false),
            provider_capability(true, false),
            provider_capability(false, true),
            provider_capability(true, true),
        ];
        let unique: std::collections::HashSet<&str> = all.iter().copied().collect();
        assert_eq!(unique.len(), all.len());
        for capability in all {
            assert_ne!(
                unfurl_cache_key(URL, NsfwMode::Block, capability),
                unfurl_cache_key(URL, NsfwMode::Block, "unrelated")
            );
        }
    }

    #[test]
    fn the_url_distinguishes_entries() {
        assert_ne!(
            unfurl_cache_key(URL, NsfwMode::Block, "none"),
            unfurl_cache_key("https://example.com/b", NsfwMode::Block, "none")
        );
    }

    #[test]
    fn the_empty_entry_lifetime_stays_a_short_retry_window() {
        assert!(
            default_empty_entry_lifetime()
                <= Duration::from_secs(EMPTY_UNFURL_CACHE_TTL_CEILING_SECS),
            "a site that was merely down must become retryable again quickly"
        );
    }

    #[test]
    fn the_expiry_reads_the_short_lifetime_off_an_empty_response() {
        let expiry = UnfurlEntryExpiry::new(default_empty_entry_lifetime());
        let now = Instant::now();
        let key = unfurl_cache_key(URL, NsfwMode::Block, "none");
        assert_eq!(
            Some(default_empty_entry_lifetime()),
            Expiry::<String, UnfurlResponse>::expire_after_create(&expiry, &key, &resolved(0), now)
        );
        assert_eq!(
            None,
            Expiry::<String, UnfurlResponse>::expire_after_create(&expiry, &key, &resolved(1), now)
        );
        assert_eq!(
            Some(default_empty_entry_lifetime()),
            Expiry::<String, Arc<UnfurlResult>>::expire_after_create(
                &expiry,
                &key,
                &Arc::new(UnfurlResult {
                    embeds: Vec::new(),
                    cache_ttl_seconds: None,
                }),
                now
            )
        );
    }
}
