// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::cache_policy::{
    UnfurlEntryExpiry, default_empty_entry_lifetime, provider_capability, unfurl_cache_key,
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
            UnfurlRequest::VerifyDomain { domain, .. } => domain.clone(),
            UnfurlRequest::Fetch { url, .. } => url.clone(),
            UnfurlRequest::ResolveTxt { name } => name.clone(),
            UnfurlRequest::ActivityPubKey => "activity_pub_key".to_owned(),
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
                ..
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
            UnfurlRequest::VerifyDomain { .. }
            | UnfurlRequest::Fetch { .. }
            | UnfurlRequest::ResolveTxt { .. }
            | UnfurlRequest::ActivityPubKey => None,
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
            UnfurlRequest::VerifyDomain { .. }
            | UnfurlRequest::Fetch { .. }
            | UnfurlRequest::ResolveTxt { .. }
            | UnfurlRequest::ActivityPubKey => None,
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
            UnfurlRequest::VerifyDomain { .. }
            | UnfurlRequest::Fetch { .. }
            | UnfurlRequest::ResolveTxt { .. }
            | UnfurlRequest::ActivityPubKey => {}
        }
    }
}
