// SPDX-License-Identifier: AGPL-3.0-or-later

use serde::{Deserialize, Serialize};
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::sync::{Mutex, RwLock};
use tokio::task::JoinHandle;

const COLD_START_RETRY_COOLDOWN: Duration = Duration::from_secs(10);

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct DiscoveryResponse {
    #[serde(flatten)]
    pub data: serde_json::Value,
}

pub struct DiscoveryCache {
    cached: RwLock<Option<DiscoveryResponse>>,
    cold_start_attempt: Mutex<Option<Instant>>,
}

impl DiscoveryCache {
    pub fn new() -> Self {
        Self {
            cached: RwLock::new(None),
            cold_start_attempt: Mutex::new(None),
        }
    }

    /// Discovery for a request that has no cached value yet.
    ///
    /// Steady-state freshness is owned by `start_background_refresh`, which backs off when the
    /// upstream is unhealthy. Request handlers must never block on the network while a cached
    /// value exists, and must not retry a failing upstream once per request: the upstream is
    /// reached through this proxy's own public hostname, so a stall here feeds back into the
    /// front door that serves it.
    pub async fn get_or_cold_start(
        &self,
        client: &reqwest::Client,
        upstream_url: &str,
    ) -> Option<DiscoveryResponse> {
        if let Some(cached) = self.get().await {
            return Some(cached);
        }
        {
            let mut last_attempt = self.cold_start_attempt.lock().await;
            if let Some(at) = *last_attempt
                && at.elapsed() < COLD_START_RETRY_COOLDOWN
            {
                return None;
            }
            *last_attempt = Some(Instant::now());
        }
        if let Err(err) = self.refresh(client, upstream_url).await {
            tracing::warn!(%err, url = upstream_url, "cold-start discovery fetch failed");
        }
        self.get().await
    }

    pub async fn refresh(
        &self,
        client: &reqwest::Client,
        upstream_url: &str,
    ) -> anyhow::Result<()> {
        let response = client
            .get(upstream_url)
            .header("Accept", "application/json")
            .timeout(Duration::from_secs(5))
            .send()
            .await?;

        if !response.status().is_success() {
            anyhow::bail!("upstream discovery returned {}", response.status());
        }

        let body: DiscoveryResponse = response.json().await?;
        let api_code_version = body
            .data
            .get("api_code_version")
            .and_then(|v| v.as_str())
            .unwrap_or("unknown");
        tracing::info!(url = upstream_url, %api_code_version, "discovery cache updated");

        *self.cached.write().await = Some(body);
        Ok(())
    }

    pub async fn get(&self) -> Option<DiscoveryResponse> {
        self.cached.read().await.clone()
    }

    pub fn start_background_refresh(
        self: &Arc<Self>,
        client: reqwest::Client,
        upstream_url: String,
        interval_ms: u64,
    ) -> JoinHandle<()> {
        let cache = Arc::clone(self);
        tokio::spawn(async move {
            let base = Duration::from_millis(interval_ms);
            let max_backoff = Duration::from_secs(300);
            let mut consecutive_failures: u32 = 0;
            tokio::time::sleep(base).await;
            loop {
                match cache.refresh(&client, &upstream_url).await {
                    Ok(()) => {
                        consecutive_failures = 0;
                        tokio::time::sleep(base).await;
                    }
                    Err(err) => {
                        consecutive_failures = consecutive_failures.saturating_add(1);
                        let multiplier = 1u32 << consecutive_failures.min(5);
                        let backoff = base.saturating_mul(multiplier).min(max_backoff);
                        tracing::warn!(
                            %err,
                            url = %upstream_url,
                            consecutive_failures,
                            backoff_ms = backoff.as_millis() as u64,
                            "discovery refresh failed; backing off"
                        );
                        tokio::time::sleep(backoff).await;
                    }
                }
            }
        })
    }
}

impl Default for DiscoveryCache {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn new_cache_starts_with_none() {
        let cache = DiscoveryCache::new();
        assert!(cache.get().await.is_none());
    }

    #[tokio::test]
    async fn default_cache_starts_with_none() {
        let cache = DiscoveryCache::default();
        assert!(cache.get().await.is_none());
    }

    #[tokio::test]
    async fn cached_discovery_never_touches_the_network() {
        let cache = DiscoveryCache::new();
        let json = r#"{"api_code_version":"v1"}"#;
        *cache.cached.write().await = Some(serde_json::from_str(json).unwrap());
        // An unroutable upstream: if this were contacted the call would stall or error.
        let client = reqwest::Client::new();
        let got = cache
            .get_or_cold_start(&client, "http://127.0.0.1:1/discovery")
            .await;
        assert!(got.is_some());
        assert!(cache.cold_start_attempt.lock().await.is_none());
    }

    #[tokio::test]
    async fn cold_start_failure_is_not_retried_on_every_request() {
        let cache = DiscoveryCache::new();
        let client = reqwest::Client::builder()
            .timeout(Duration::from_millis(50))
            .build()
            .unwrap();
        assert!(
            cache
                .get_or_cold_start(&client, "http://127.0.0.1:1/discovery")
                .await
                .is_none()
        );
        let first = *cache.cold_start_attempt.lock().await;
        assert!(first.is_some());
        assert!(
            cache
                .get_or_cold_start(&client, "http://127.0.0.1:1/discovery")
                .await
                .is_none()
        );
        assert_eq!(*cache.cold_start_attempt.lock().await, first);
    }

    #[test]
    fn discovery_response_deserializes_from_json() {
        let json = r#"{"api_code_version":"v1","name":"test"}"#;
        let resp: DiscoveryResponse = serde_json::from_str(json).unwrap();
        assert_eq!(
            resp.data.get("api_code_version").and_then(|v| v.as_str()),
            Some("v1")
        );
        assert_eq!(resp.data.get("name").and_then(|v| v.as_str()), Some("test"));
    }

    #[test]
    fn discovery_response_roundtrips_through_serde() {
        let json = r#"{"api_code_version":"v2","features":["a","b"]}"#;
        let resp: DiscoveryResponse = serde_json::from_str(json).unwrap();
        let serialized = serde_json::to_string(&resp).unwrap();
        let resp2: DiscoveryResponse = serde_json::from_str(&serialized).unwrap();
        assert_eq!(resp.data, resp2.data);
    }
}
