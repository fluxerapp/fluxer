// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::cache_policy::{
    UnfurlEntryExpiry, default_empty_entry_lifetime, every_unfurl_cache_key, provider_capability,
    unfurl_cache_key,
};
use crate::embed_normalizer::normalize_embeds;
use crate::media_proxy::MediaProxyClient;
use crate::resolvers::{self, ResolveContext, ResolverResult};
use crate::types::{InvalidatedResponse, NsfwMode, UnfurlRequest, UnfurlResponse, UnfurlResult};
use fluxer_svc::shard::ShardService;
use moka::future::Cache;
use std::sync::Arc;
use std::time::Duration;
use url::Url;

const L2_MAX_ENTRIES: u64 = 50_000;

pub struct UnfurlShard {
    cache: Cache<String, Arc<UnfurlResult>>,
    lifetime: Duration,
    http_client: reqwest::Client,
    resolvers: Vec<Box<dyn crate::resolvers::Resolver>>,
    media_proxy: MediaProxyClient,
    static_cdn_endpoint: String,
}

impl UnfurlShard {
    pub fn new(lifetime: Duration) -> Self {
        let http_client = external_http_client();
        let media_proxy_http_client = internal_http_client();

        let resolvers = resolvers::build_resolver_chain();

        let media_proxy_endpoint = std::env::var("FLUXER_MEDIA_PROXY_ENDPOINT")
            .ok()
            .filter(|v| !v.is_empty());
        let media_proxy_secret = std::env::var("FLUXER_MEDIA_PROXY_SECRET_KEY")
            .ok()
            .filter(|v| !v.is_empty());
        let media_proxy_public_endpoint = std::env::var("FLUXER_MEDIA_PROXY_PUBLIC_ENDPOINT")
            .ok()
            .filter(|v| !v.is_empty());
        let static_cdn_endpoint = std::env::var("FLUXER_UNFURL_STATIC_CDN_ENDPOINT")
            .or_else(|_| std::env::var("FLUXER_STATIC_CDN_ENDPOINT"))
            .unwrap_or_default();
        let (media_proxy_endpoint, media_proxy_secret) = match (
            media_proxy_endpoint,
            media_proxy_secret,
        ) {
            (Some(endpoint), Some(secret_key)) => (endpoint, secret_key),
            (Some(endpoint), None) => {
                panic!(
                    "media proxy endpoint configured without FLUXER_MEDIA_PROXY_SECRET_KEY: {endpoint}"
                );
            }
            (None, Some(_)) => {
                panic!(
                    "FLUXER_MEDIA_PROXY_SECRET_KEY configured without FLUXER_MEDIA_PROXY_ENDPOINT"
                );
            }
            (None, None) => {
                panic!(
                    "unfurl shard requires FLUXER_MEDIA_PROXY_ENDPOINT and FLUXER_MEDIA_PROXY_SECRET_KEY"
                );
            }
        };
        tracing::info!(
            endpoint = %media_proxy_endpoint,
            public_endpoint = ?media_proxy_public_endpoint,
            "media proxy client enabled"
        );
        let media_proxy = MediaProxyClient::new_with_public_endpoint(
            &media_proxy_endpoint,
            &media_proxy_secret,
            media_proxy_public_endpoint.as_deref(),
            media_proxy_http_client,
        );

        Self {
            cache: build_l2_cache(lifetime, default_empty_entry_lifetime()),
            lifetime,
            http_client,
            resolvers,
            media_proxy,
            static_cdn_endpoint,
        }
    }

    #[cfg(test)]
    fn for_test() -> Self {
        Self::for_test_with_lifetimes(long_lifetime(), default_empty_entry_lifetime())
    }

    #[cfg(test)]
    fn for_test_with_lifetimes(lifetime: Duration, empty_entry_lifetime: Duration) -> Self {
        Self {
            cache: build_l2_cache(lifetime, empty_entry_lifetime),
            lifetime,
            http_client: external_http_client(),
            resolvers: resolvers::build_resolver_chain(),
            media_proxy: MediaProxyClient::new_with_public_endpoint(
                "http://media-proxy.invalid",
                "test-secret",
                None,
                internal_http_client(),
            ),
            static_cdn_endpoint: String::new(),
        }
    }

    async fn resolve_url(
        &self,
        url_str: &str,
        nsfw_mode: NsfwMode,
        youtube_api_key: Option<&str>,
        klipy_api_key: Option<&str>,
    ) -> anyhow::Result<UnfurlResult> {
        let parsed = Url::parse(url_str)?;

        let (fetch_url, matched_resolver_idx) = self.find_transform(&parsed);

        let ctx = ResolveContext {
            url: fetch_url.clone(),
            original_url: parsed.clone(),
            http_client: self.http_client.clone(),
            nsfw_mode,
            media_proxy: &self.media_proxy,
            static_cdn_endpoint: &self.static_cdn_endpoint,
            youtube_api_key: youtube_api_key.map(str::to_owned),
            klipy_api_key: klipy_api_key.map(str::to_owned),
        };

        if let Some(idx) = matched_resolver_idx {
            match self.resolvers[idx].resolve(&ctx).await {
                Ok(result) if !result.embeds.is_empty() => {
                    return Ok(self.finalize_result(result));
                }
                Ok(_) => {}
                Err(err) => {
                    tracing::warn!(
                        error = %err,
                        url = %ctx.url,
                        "transformed unfurl resolver failed"
                    );
                }
            }
        }

        for (i, resolver) in self.resolvers.iter().enumerate() {
            if Some(i) == matched_resolver_idx {
                continue;
            }
            if resolver.matches(&fetch_url) {
                match resolver.resolve(&ctx).await {
                    Ok(result) if !result.embeds.is_empty() => {
                        return Ok(self.finalize_result(result));
                    }
                    Ok(_) => {}
                    Err(err) => {
                        tracing::warn!(
                            error = %err,
                            url = %ctx.url,
                            "unfurl resolver failed"
                        );
                    }
                }
            }
        }

        Ok(UnfurlResult {
            embeds: Vec::new(),
            cache_ttl_seconds: None,
        })
    }

    fn find_transform(&self, url: &Url) -> (Url, Option<usize>) {
        for (i, resolver) in self.resolvers.iter().enumerate() {
            if let Some(transformed) = resolver.transform_url(url) {
                return (transformed, Some(i));
            }
        }
        (url.clone(), None)
    }

    fn finalize_result(&self, result: ResolverResult) -> UnfurlResult {
        UnfurlResult {
            embeds: normalize_embeds(result.embeds, &self.media_proxy),
            cache_ttl_seconds: Some(self.lifetime.as_secs()),
        }
    }
}

#[cfg(test)]
fn long_lifetime() -> Duration {
    Duration::from_secs(30 * 60)
}

fn build_l2_cache(
    lifetime: Duration,
    empty_entry_lifetime: Duration,
) -> Cache<String, Arc<UnfurlResult>> {
    Cache::builder()
        .max_capacity(L2_MAX_ENTRIES)
        .time_to_live(lifetime)
        .expire_after(UnfurlEntryExpiry::new(empty_entry_lifetime))
        .build()
}

fn base_http_client_builder() -> reqwest::ClientBuilder {
    reqwest::Client::builder()
        .user_agent("Mozilla/5.0 (compatible; Fluxerbot/1.0; +https://fluxer.app)")
        .timeout(Duration::from_secs(10))
        .redirect(reqwest::redirect::Policy::none())
}

fn external_http_client() -> reqwest::Client {
    base_http_client_builder()
        .dns_resolver(std::sync::Arc::new(
            crate::network_policy::PinnedDnsResolver,
        ))
        .build()
        .expect("failed to build external HTTP client")
}

fn internal_http_client() -> reqwest::Client {
    base_http_client_builder()
        .build()
        .expect("failed to build internal HTTP client")
}

impl ShardService for UnfurlShard {
    type Request = UnfurlRequest;
    type Response = UnfurlResponse;

    fn service_name(&self) -> &str {
        "unfurl"
    }

    async fn handle(&self, request: UnfurlRequest) -> anyhow::Result<UnfurlResponse> {
        match request {
            UnfurlRequest::Unfurl {
                ref url,
                nsfw_mode,
                bypass_cache,
                cache_only,
                ref youtube_api_key,
                ref klipy_api_key,
            } => {
                let nsfw = nsfw_mode.unwrap_or_default();
                let cache_key = unfurl_cache_key(
                    url,
                    nsfw,
                    provider_capability(youtube_api_key.is_some(), klipy_api_key.is_some()),
                );

                if !bypass_cache && let Some(cached) = self.cache.get(&cache_key).await {
                    return Ok(UnfurlResponse::Resolved(cached));
                }
                if cache_only {
                    return Ok(UnfurlResponse::Resolved(Arc::new(UnfurlResult {
                        embeds: Vec::new(),
                        cache_ttl_seconds: None,
                    })));
                }

                let result = match self
                    .resolve_url(
                        url,
                        nsfw,
                        youtube_api_key.as_deref(),
                        klipy_api_key.as_deref(),
                    )
                    .await
                {
                    Ok(r) => Arc::new(r),
                    Err(err) => {
                        tracing::warn!(error = %err, url = %url, "failed to resolve URL");
                        return Ok(UnfurlResponse::Failed {
                            message: err.to_string(),
                        });
                    }
                };

                self.cache.insert(cache_key, result.clone()).await;
                Ok(UnfurlResponse::Resolved(result))
            }
            UnfurlRequest::Invalidate { ref url } => {
                for key in every_unfurl_cache_key(url) {
                    self.cache.invalidate(&key).await;
                }
                Ok(UnfurlResponse::Invalidated(InvalidatedResponse {
                    invalidated: true,
                }))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::MessageEmbed;

    const BLOCKED_URL: &str = "http://127.0.0.1/unreachable";

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

    fn keyless_block_key(url: &str) -> String {
        unfurl_cache_key(url, NsfwMode::Block, provider_capability(false, false))
    }

    fn embed_count(response: &UnfurlResponse) -> usize {
        match response {
            UnfurlResponse::Resolved(result) => result.embeds.len(),
            _ => panic!("expected a resolved response"),
        }
    }

    async fn seed_keyless_entry(shard: &UnfurlShard) {
        shard
            .cache
            .insert(
                unfurl_cache_key(
                    BLOCKED_URL,
                    NsfwMode::Block,
                    provider_capability(false, false),
                ),
                Arc::new(UnfurlResult {
                    embeds: vec![MessageEmbed::new("link")],
                    cache_ttl_seconds: Some(long_lifetime().as_secs()),
                }),
            )
            .await;
    }

    #[tokio::test]
    async fn a_shard_entry_resolved_without_a_youtube_key_is_not_served_once_one_is_configured() {
        let shard = UnfurlShard::for_test();
        seed_keyless_entry(&shard).await;

        assert_eq!(
            1,
            embed_count(&shard.handle(unfurl(BLOCKED_URL, None)).await.unwrap()),
            "the keyless entry must still serve keyless requests"
        );
        assert_eq!(
            0,
            embed_count(
                &shard
                    .handle(unfurl(BLOCKED_URL, Some("configured")))
                    .await
                    .unwrap()
            ),
            "a result resolved without a YouTube key must not be served once a key is configured"
        );
    }

    #[tokio::test]
    async fn invalidate_clears_the_shard_entry_for_every_provider_capability() {
        let shard = UnfurlShard::for_test();
        seed_keyless_entry(&shard).await;

        shard
            .handle(UnfurlRequest::Invalidate {
                url: BLOCKED_URL.to_owned(),
            })
            .await
            .unwrap();

        assert_eq!(
            0,
            embed_count(&shard.handle(unfurl(BLOCKED_URL, None)).await.unwrap())
        );
    }
    #[tokio::test]
    async fn a_url_that_produced_no_embed_is_written_to_the_shard_cache() {
        let shard = UnfurlShard::for_test();

        assert_eq!(
            0,
            embed_count(&shard.handle(unfurl(BLOCKED_URL, None)).await.unwrap())
        );
        assert!(
            shard
                .cache
                .get(&keyless_block_key(BLOCKED_URL))
                .await
                .is_some(),
            "an empty resolution must be remembered so the next message does not refetch the page"
        );
    }

    #[tokio::test]
    async fn a_cache_only_probe_never_writes_an_entry() {
        let shard = UnfurlShard::for_test();

        assert_eq!(
            0,
            embed_count(&shard.handle(probe(BLOCKED_URL)).await.unwrap())
        );
        assert!(
            shard
                .cache
                .get(&keyless_block_key(BLOCKED_URL))
                .await
                .is_none(),
            "a cache-only miss means unknown, not resolved-to-nothing"
        );
    }

    #[tokio::test]
    async fn an_unscanned_entry_is_not_served_to_a_scanning_request() {
        let shard = UnfurlShard::for_test();
        shard
            .cache
            .insert(
                unfurl_cache_key(
                    BLOCKED_URL,
                    NsfwMode::Allow,
                    provider_capability(false, false),
                ),
                Arc::new(UnfurlResult {
                    embeds: vec![MessageEmbed::new("link")],
                    cache_ttl_seconds: Some(long_lifetime().as_secs()),
                }),
            )
            .await;

        assert_eq!(
            0,
            embed_count(&shard.handle(unfurl(BLOCKED_URL, None)).await.unwrap()),
            "a result resolved with nsfw scanning off must not be served to a scanning request"
        );
    }
    #[tokio::test]
    async fn an_empty_shard_entry_lapses_long_before_a_populated_one() {
        let shard =
            UnfurlShard::for_test_with_lifetimes(long_lifetime(), Duration::from_millis(50));
        let empty_key = keyless_block_key("https://example.com/transient");
        let populated_key = keyless_block_key("https://example.com/stable");
        shard
            .cache
            .insert(
                empty_key.clone(),
                Arc::new(UnfurlResult {
                    embeds: Vec::new(),
                    cache_ttl_seconds: None,
                }),
            )
            .await;
        shard
            .cache
            .insert(
                populated_key.clone(),
                Arc::new(UnfurlResult {
                    embeds: vec![MessageEmbed::new("link")],
                    cache_ttl_seconds: Some(long_lifetime().as_secs()),
                }),
            )
            .await;

        tokio::time::sleep(Duration::from_millis(200)).await;

        assert!(
            shard.cache.get(&empty_key).await.is_none(),
            "a link that failed to unfurl must become retryable again quickly"
        );
        assert!(
            shard.cache.get(&populated_key).await.is_some(),
            "a real embed must keep the full lifetime"
        );
    }
    #[tokio::test]
    async fn the_configured_lifetime_governs_the_shard_cache_and_the_reported_ttl() {
        let shard = UnfurlShard::for_test_with_lifetimes(
            Duration::from_millis(80),
            default_empty_entry_lifetime(),
        );
        let finalized = shard.finalize_result(crate::resolvers::ResolverResult {
            embeds: vec![MessageEmbed::new("link")],
        });
        assert_eq!(
            Some(Duration::from_millis(80).as_secs()),
            finalized.cache_ttl_seconds,
            "the reported ttl must be the configured lifetime, not a compiled-in constant"
        );

        let key = keyless_block_key("https://example.com/configured");
        shard
            .cache
            .insert(
                key.clone(),
                Arc::new(UnfurlResult {
                    embeds: vec![MessageEmbed::new("link")],
                    cache_ttl_seconds: None,
                }),
            )
            .await;
        tokio::time::sleep(Duration::from_millis(220)).await;
        assert!(
            shard.cache.get(&key).await.is_none(),
            "the configured lifetime must govern the shard cache"
        );
    }
}
