// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::klipy::{KlipyClient, build_share_url, extract_slug_from_url};
use crate::media_proxy::MediaProxyUrlBuilder;
use crate::types::{GifCategoryTag, GifItem, GifRequest, GifServiceResponse};
use fluxer_svc::config::ServiceConfig;
use fluxer_svc::shard::ShardService;
use moka::future::Cache;
use std::collections::HashSet;
use std::future::Future;
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::sync::Mutex;

const SEARCH_SOFT_TTL: Duration = Duration::from_secs(30);
const SEARCH_HARD_TTL: Duration = Duration::from_secs(5 * 60);
const SUGGEST_SOFT_TTL: Duration = Duration::from_secs(60);
const SUGGEST_HARD_TTL: Duration = Duration::from_secs(10 * 60);
const FEATURED_GIFS_SOFT_TTL: Duration = Duration::from_secs(5 * 60);
const FEATURED_GIFS_HARD_TTL: Duration = Duration::from_secs(30 * 60);
const CATEGORIES_SOFT_TTL: Duration = Duration::from_secs(24 * 60 * 60);
const CATEGORIES_HARD_TTL: Duration = Duration::from_secs(48 * 60 * 60);
const RESOLVE_SOFT_TTL: Duration = Duration::from_secs(30 * 60);
const RESOLVE_HARD_TTL: Duration = Duration::from_secs(2 * 60 * 60);

#[derive(Clone)]
pub struct GifsShard {
    inner: Arc<GifsShardInner>,
}

struct GifsShardInner {
    klipy: KlipyClient,
    gif_lists: Cache<String, Cached<Vec<GifItem>>>,
    categories: Cache<String, Cached<Vec<GifCategoryTag>>>,
    suggestions: Cache<String, Cached<Vec<String>>>,
    resolved: Cache<String, Cached<Option<GifItem>>>,
    refreshing: Mutex<HashSet<String>>,
}

#[derive(Debug, Clone)]
struct Cached<T> {
    data: T,
    stored_at: Instant,
}

#[derive(Debug, Clone, Copy)]
struct CachePolicy {
    soft_ttl: Duration,
    hard_ttl: Duration,
}

impl CachePolicy {
    const fn new(soft_ttl: Duration, hard_ttl: Duration) -> Self {
        Self { soft_ttl, hard_ttl }
    }
}

impl<T> Cached<T> {
    fn new(data: T) -> Self {
        Self {
            data,
            stored_at: Instant::now(),
        }
    }

    fn age(&self) -> Duration {
        self.stored_at.elapsed()
    }
}

impl GifsShard {
    pub fn new(config: &ServiceConfig) -> anyhow::Result<Self> {
        let media_proxy = MediaProxyUrlBuilder::from_env()?;
        let klipy = KlipyClient::new(media_proxy)?;
        let max_capacity = config.cache_max_entries;
        let max_cache_ttl = CATEGORIES_HARD_TTL;
        Ok(Self {
            inner: Arc::new(GifsShardInner {
                klipy,
                gif_lists: build_cache(max_capacity, max_cache_ttl),
                categories: build_cache(max_capacity, max_cache_ttl),
                suggestions: build_cache(max_capacity, max_cache_ttl),
                resolved: build_cache(max_capacity, max_cache_ttl),
                refreshing: Mutex::new(HashSet::new()),
            }),
        })
    }

    async fn get_cached<T, Fetch, Fut>(
        &self,
        cache: Cache<String, Cached<T>>,
        key: String,
        policy: CachePolicy,
        fetch: Fetch,
    ) -> anyhow::Result<T>
    where
        T: Clone + Send + Sync + 'static,
        Fetch: Fn() -> Fut + Clone + Send + Sync + 'static,
        Fut: Future<Output = anyhow::Result<T>> + Send + 'static,
    {
        if let Some(cached) = cache.get(&key).await {
            let age = cached.age();
            if age <= policy.soft_ttl {
                return Ok(cached.data);
            }
            if age <= policy.hard_ttl {
                self.trigger_background_refresh(cache, key, policy, fetch);
                return Ok(cached.data);
            }
            cache.invalidate(&key).await;
        }

        let fetch_for_load = fetch.clone();
        let cached = cache
            .try_get_with(key, async move {
                let data = fetch_for_load().await?;
                Ok::<Cached<T>, anyhow::Error>(Cached::new(data))
            })
            .await
            .map_err(|error| anyhow::anyhow!("{}", error.as_ref()))?;
        Ok(cached.data)
    }

    fn trigger_background_refresh<T, Fetch, Fut>(
        &self,
        cache: Cache<String, Cached<T>>,
        key: String,
        _policy: CachePolicy,
        fetch: Fetch,
    ) where
        T: Clone + Send + Sync + 'static,
        Fetch: Fn() -> Fut + Clone + Send + Sync + 'static,
        Fut: Future<Output = anyhow::Result<T>> + Send + 'static,
    {
        let this = self.clone();
        tokio::spawn(async move {
            {
                let mut refreshing = this.inner.refreshing.lock().await;
                if !refreshing.insert(key.clone()) {
                    return;
                }
            }

            let result = fetch().await;
            match result {
                Ok(data) => {
                    cache.insert(key.clone(), Cached::new(data)).await;
                }
                Err(error) => {
                    tracing::debug!(error = %error, cache_key = %key, "background GIF cache refresh failed");
                }
            }

            let mut refreshing = this.inner.refreshing.lock().await;
            refreshing.remove(&key);
        });
    }

    async fn handle_available(&self, api_key: Option<String>) -> GifServiceResponse {
        GifServiceResponse::Available {
            available: api_key.as_deref().is_some_and(|key| !key.trim().is_empty()),
        }
    }

    async fn handle_search(
        &self,
        api_key: String,
        q: String,
        locale: String,
        country: String,
    ) -> anyhow::Result<GifServiceResponse> {
        let key = format!("search:{locale}:{country}:{q}");
        let this = self.clone();
        let gifs = self
            .get_cached(
                self.inner.gif_lists.clone(),
                key,
                CachePolicy::new(SEARCH_SOFT_TTL, SEARCH_HARD_TTL),
                move || {
                    let this = this.clone();
                    let api_key = api_key.clone();
                    let q = q.clone();
                    let locale = locale.clone();
                    let country = country.clone();
                    async move {
                        this.inner
                            .klipy
                            .search(&api_key, &q, &locale, &country, 50)
                            .await
                    }
                },
            )
            .await?;
        Ok(GifServiceResponse::SearchResults(gifs))
    }

    async fn handle_featured(
        &self,
        api_key: String,
        locale: String,
        country: String,
    ) -> anyhow::Result<GifServiceResponse> {
        let gifs_key = format!("featured_gifs:{locale}:{country}");
        let categories_key = format!("featured_categories:{locale}");
        let gifs_this = self.clone();
        let categories_this = self.clone();
        let api_key_for_gifs = api_key.clone();
        let locale_for_gifs = locale.clone();
        let country_for_gifs = country.clone();
        let api_key_for_categories = api_key;
        let locale_for_categories = locale;

        let gifs_future = self.get_cached(
            self.inner.gif_lists.clone(),
            gifs_key,
            CachePolicy::new(FEATURED_GIFS_SOFT_TTL, FEATURED_GIFS_HARD_TTL),
            move || {
                let this = gifs_this.clone();
                let api_key = api_key_for_gifs.clone();
                let locale = locale_for_gifs.clone();
                let country = country_for_gifs.clone();
                async move {
                    this.inner
                        .klipy
                        .featured_gifs(&api_key, &locale, &country)
                        .await
                }
            },
        );
        let categories_future = self.get_cached(
            self.inner.categories.clone(),
            categories_key,
            CachePolicy::new(CATEGORIES_SOFT_TTL, CATEGORIES_HARD_TTL),
            move || {
                let this = categories_this.clone();
                let api_key = api_key_for_categories.clone();
                let locale = locale_for_categories.clone();
                async move {
                    this.inner
                        .klipy
                        .featured_categories(&api_key, &locale)
                        .await
                }
            },
        );

        let (gifs, categories) = tokio::try_join!(gifs_future, categories_future)?;
        Ok(GifServiceResponse::Featured { gifs, categories })
    }

    async fn handle_trending(
        &self,
        api_key: String,
        locale: String,
        country: String,
    ) -> anyhow::Result<GifServiceResponse> {
        let key = format!("trending:{locale}:{country}");
        let this = self.clone();
        let gifs = self
            .get_cached(
                self.inner.gif_lists.clone(),
                key,
                CachePolicy::new(FEATURED_GIFS_SOFT_TTL, FEATURED_GIFS_HARD_TTL),
                move || {
                    let this = this.clone();
                    let api_key = api_key.clone();
                    let locale = locale.clone();
                    let country = country.clone();
                    async move {
                        this.inner
                            .klipy
                            .trending_gifs(&api_key, &locale, &country)
                            .await
                    }
                },
            )
            .await?;
        Ok(GifServiceResponse::TrendingResults(gifs))
    }

    async fn handle_suggest(
        &self,
        api_key: String,
        q: String,
        locale: String,
    ) -> anyhow::Result<GifServiceResponse> {
        let key = format!("suggest:{locale}:{q}");
        let this = self.clone();
        let suggestions = self
            .get_cached(
                self.inner.suggestions.clone(),
                key,
                CachePolicy::new(SUGGEST_SOFT_TTL, SUGGEST_HARD_TTL),
                move || {
                    let this = this.clone();
                    let api_key = api_key.clone();
                    let q = q.clone();
                    let locale = locale.clone();
                    async move { this.inner.klipy.suggestions(&api_key, &q, &locale).await }
                },
            )
            .await?;
        Ok(GifServiceResponse::Suggestions(suggestions))
    }

    async fn handle_resolve_by_url(
        &self,
        api_key: String,
        url: String,
        locale: String,
        country: String,
    ) -> anyhow::Result<GifServiceResponse> {
        let key = format!("resolve:{locale}:{country}:{url}");
        let this = self.clone();
        let gif = self
            .get_cached(
                self.inner.resolved.clone(),
                key,
                CachePolicy::new(RESOLVE_SOFT_TTL, RESOLVE_HARD_TTL),
                move || {
                    let this = this.clone();
                    let api_key = api_key.clone();
                    let url = url.clone();
                    let locale = locale.clone();
                    let country = country.clone();
                    async move {
                        this.inner
                            .klipy
                            .resolve_by_url(&api_key, &url, &locale, &country)
                            .await
                    }
                },
            )
            .await?;
        Ok(GifServiceResponse::Resolved { gif })
    }
}

impl ShardService for GifsShard {
    type Request = GifRequest;
    type Response = GifServiceResponse;

    fn service_name(&self) -> &str {
        "gifs"
    }

    async fn handle(&self, request: GifRequest) -> anyhow::Result<GifServiceResponse> {
        let response = match request {
            GifRequest::IsAvailable { api_key } => self.handle_available(api_key).await,
            GifRequest::Search {
                api_key,
                q,
                locale,
                country,
            } => self.handle_search(api_key, q, locale, country).await?,
            GifRequest::GetFeatured {
                api_key,
                locale,
                country,
            } => self.handle_featured(api_key, locale, country).await?,
            GifRequest::GetTrendingGifs {
                api_key,
                locale,
                country,
            } => self.handle_trending(api_key, locale, country).await?,
            GifRequest::Suggest { api_key, q, locale } => {
                self.handle_suggest(api_key, q, locale).await?
            }
            GifRequest::RegisterShare {
                api_key,
                id,
                q,
                locale,
                country,
            } => {
                self.inner
                    .klipy
                    .register_share(&api_key, &id, &q, &locale, &country)
                    .await?;
                GifServiceResponse::Registered
            }
            GifRequest::ResolveByUrl {
                api_key,
                url,
                locale,
                country,
            } => {
                self.handle_resolve_by_url(api_key, url, locale, country)
                    .await?
            }
            GifRequest::BuildShareUrl { slug } => GifServiceResponse::ShareUrl {
                url: build_share_url(&slug),
            },
            GifRequest::ExtractSlugFromUrl { url } => GifServiceResponse::ExtractedSlug {
                slug: extract_slug_from_url(&url),
            },
        };
        Ok(response)
    }
}

fn build_cache<T>(max_capacity: u64, time_to_live: Duration) -> Cache<String, Cached<T>>
where
    T: Clone + Send + Sync + 'static,
{
    Cache::builder()
        .max_capacity(max_capacity)
        .time_to_live(time_to_live)
        .build()
}
