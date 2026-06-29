// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::cache_policy::FIXED_UNFURL_CACHE_TTL_SECS;
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
const L2_TTL: Duration = Duration::from_secs(FIXED_UNFURL_CACHE_TTL_SECS);

pub struct UnfurlShard {
    cache: Cache<String, Arc<UnfurlResult>>,
    http_client: reqwest::Client,
    resolvers: Vec<Box<dyn crate::resolvers::Resolver>>,
    media_proxy: MediaProxyClient,
    static_cdn_endpoint: String,
}

impl UnfurlShard {
    pub fn new() -> Self {
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
            cache: Cache::builder()
                .max_capacity(L2_MAX_ENTRIES)
                .time_to_live(L2_TTL)
                .build(),
            http_client,
            resolvers,
            media_proxy,
            static_cdn_endpoint,
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
            cache_ttl_seconds: Some(FIXED_UNFURL_CACHE_TTL_SECS),
        }
    }
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
                let cache_key = unfurl_cache_key(url, nsfw);

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

                if !result.embeds.is_empty() {
                    self.cache.insert(cache_key, result.clone()).await;
                }
                Ok(UnfurlResponse::Resolved(result))
            }
            UnfurlRequest::Invalidate { ref url } => {
                self.cache
                    .invalidate(&unfurl_cache_key(url, NsfwMode::Block))
                    .await;
                self.cache
                    .invalidate(&unfurl_cache_key(url, NsfwMode::Flag))
                    .await;
                self.cache
                    .invalidate(&unfurl_cache_key(url, NsfwMode::Allow))
                    .await;
                Ok(UnfurlResponse::Invalidated(InvalidatedResponse {
                    invalidated: true,
                }))
            }
        }
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
