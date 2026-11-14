// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::types::{NsfwMode, UnfurlResponse, UnfurlResult};
use moka::Expiry;
use std::sync::Arc;
use std::time::{Duration, Instant};

pub const EMPTY_UNFURL_CACHE_TTL_SECS: u64 = 60;

const SCAN_TOKENS: [&str; 2] = ["scanned", "unscanned"];

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
