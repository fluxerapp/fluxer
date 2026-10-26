// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::activity_pub::signature::{self, RequestSigner};
use crate::cache_policy::{
    UnfurlEntryExpiry, default_empty_entry_lifetime, provider_capability, unfurl_cache_key,
};
use crate::embed_normalizer::normalize_embeds;
use crate::media_proxy::MediaProxyClient;
use crate::resolvers::{self, ResolveContext, ResolverResult};
use crate::types::{NsfwMode, UnfurlRequest, UnfurlResponse, UnfurlResult};
use fluxer_svc::config::optional_env;
use fluxer_svc::shard::ShardService;
use moka::future::Cache;
use ring::signature::RsaKeyPair;
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
    self_hosted: bool,
    youtube_api_key: Option<String>,
    klipy_api_key: Option<String>,
    activity_pub_key: Option<Arc<RsaKeyPair>>,
}

impl UnfurlShard {
    pub fn new(lifetime: Duration) -> Self {
        let http_client = external_http_client();
        let media_proxy_http_client = internal_http_client();

        let resolvers = resolvers::build_resolver_chain();

        let media_proxy_endpoint = optional_env("FLUXER_MEDIA_PROXY_ENDPOINT");
        let media_proxy_secret = optional_env("FLUXER_MEDIA_PROXY_SECRET_KEY");
        let media_proxy_public_endpoint = optional_env("FLUXER_MEDIA_PROXY_PUBLIC_ENDPOINT")
            .map(|v| fluxer_common::config::normalize_public_endpoint_from_env(&v));
        let self_hosted = fluxer_common::config::read_bool_env("FLUXER_SELF_HOSTED", false);
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
        let activity_pub_key =
            optional_env("FLUXER_ACTIVITYPUB_SIGNING_KEY_BASE64").map(|encoded| {
                let key = signature::parse_signing_key(&encoded).unwrap_or_else(|err| {
                    panic!("invalid FLUXER_ACTIVITYPUB_SIGNING_KEY_BASE64: {err}")
                });
                tracing::info!("ActivityPub request signing enabled");
                Arc::new(key)
            });

        Self {
            cache: build_l2_cache(lifetime, default_empty_entry_lifetime()),
            lifetime,
            http_client,
            resolvers,
            media_proxy,
            self_hosted,
            youtube_api_key: optional_env("FLUXER_YOUTUBE_API_KEY")
                .or_else(|| optional_env("YOUTUBE_API_KEY")),
            klipy_api_key: optional_env("FLUXER_KLIPY_API_KEY")
                .or_else(|| optional_env("KLIPY_API_KEY")),
            activity_pub_key,
        }
    }

    async fn resolve_url(
        &self,
        url_str: &str,
        nsfw_mode: NsfwMode,
        youtube_api_key: Option<&str>,
        klipy_api_key: Option<&str>,
        activity_pub_key_id: Option<&str>,
    ) -> anyhow::Result<UnfurlResult> {
        let parsed = Url::parse(url_str)?;

        if self.media_proxy.is_own_url(parsed.as_str()) {
            return Ok(self.resolve_own_media(&parsed, nsfw_mode).await);
        }

        let (fetch_url, matched_resolver_idx) = self.find_transform(&parsed);

        let ctx = ResolveContext {
            url: fetch_url.clone(),
            original_url: parsed.clone(),
            http_client: self.http_client.clone(),
            nsfw_mode,
            media_proxy: &self.media_proxy,
            self_hosted: self.self_hosted,
            youtube_api_key: youtube_api_key
                .map(str::to_owned)
                .or_else(|| self.youtube_api_key.clone()),
            klipy_api_key: klipy_api_key
                .map(str::to_owned)
                .or_else(|| self.klipy_api_key.clone()),
            activity_pub_signer: self
                .activity_pub_key
                .clone()
                .zip(activity_pub_key_id)
                .map(|(key, key_id)| RequestSigner::new(key, key_id.to_owned())),
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

    async fn resolve_own_media(&self, url: &Url, nsfw_mode: NsfwMode) -> UnfurlResult {
        match resolvers::media::build_own_media_embed(&self.media_proxy, url, nsfw_mode).await {
            Ok(Some(embed)) => self.finalize_result(ResolverResult {
                embeds: vec![embed],
            }),
            Ok(None) => UnfurlResult {
                embeds: Vec::new(),
                cache_ttl_seconds: None,
            },
            Err(err) => {
                tracing::warn!(error = %err, url = %url, "own media unfurl failed");
                UnfurlResult {
                    embeds: Vec::new(),
                    cache_ttl_seconds: None,
                }
            }
        }
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
        .no_gzip()
        .no_brotli()
        .no_deflate()
        .no_zstd()
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
                ref activity_pub_key_id,
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
                        activity_pub_key_id.as_deref(),
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
            UnfurlRequest::VerifyDomain { domain, token } => {
                match crate::domain_verification::verify_domain(&self.http_client, &domain, &token)
                    .await
                {
                    Ok(verified) => Ok(UnfurlResponse::Verified(verified)),
                    Err(err) => Ok(UnfurlResponse::Failed {
                        message: err.to_string(),
                    }),
                }
            }
            UnfurlRequest::Fetch {
                method,
                url,
                headers,
                body,
            } => {
                match crate::external_fetch::fetch(&self.http_client, &method, &url, &headers, body)
                    .await
                {
                    Ok(response) => Ok(UnfurlResponse::Fetched(response)),
                    Err(err) => Ok(UnfurlResponse::Failed {
                        message: err.to_string(),
                    }),
                }
            }
            UnfurlRequest::ResolveTxt { name } => {
                if let Err(err) = crate::dns_txt::validate_record_name(&name) {
                    return Ok(UnfurlResponse::Failed {
                        message: err.to_string(),
                    });
                }
                let records = crate::dns_txt::first_answer(&name, |records| !records.is_empty())
                    .await
                    .unwrap_or_default();
                Ok(UnfurlResponse::Txt(records))
            }
            UnfurlRequest::ActivityPubKey => Ok(UnfurlResponse::ActivityPubKey(
                self.activity_pub_key
                    .as_deref()
                    .map(signature::public_key_base64),
            )),
        }
    }
}
