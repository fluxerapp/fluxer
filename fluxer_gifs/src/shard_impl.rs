// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::klipy::{KlipyClient, build_share_url, extract_slug_from_url, resolve_cache_key};
use crate::media_proxy::MediaProxyUrlBuilder;
use crate::types::{GifCategoryTag, GifItem, GifMediaFormat, GifRequest, GifServiceResponse};
use fluxer_svc::config::optional_env;
use fluxer_svc::shard::ShardService;
use moka::notification::RemovalCause;
use moka::ops::compute::{CompResult, Op};
use moka::{Expiry, future::Cache};
use std::collections::HashSet;
use std::fmt::Write;
use std::future::Future;
use std::mem::size_of;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};
use tokio::sync::{Semaphore, TryAcquireError};
use tokio::time::MissedTickBehavior;

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
const DEFAULT_SHARD_CACHE_MAX_BYTES: u64 = 512 * 1024 * 1024;
const MIN_SHARD_CACHE_MAX_BYTES: u64 = 16 * 1024 * 1024;
const CACHE_BUDGET_PARTS: u64 = 16;
const GIF_LIST_CACHE_PARTS: u64 = 10;
const CATEGORY_CACHE_PARTS: u64 = 1;
const SUGGESTION_CACHE_PARTS: u64 = 1;
const RESOLVED_CACHE_PARTS: u64 = 4;
const MAX_CONCURRENT_REFRESHES: usize = 8;
const CACHE_MAINTENANCE_INTERVAL: Duration = Duration::from_secs(30);
const CACHE_ENTRY_OVERHEAD_BYTES: u64 = 256;
const BTREE_ENTRY_OVERHEAD_BYTES: u64 = 64;
const ARC_HEADER_BYTES: u64 = (2 * size_of::<usize>()) as u64;
const METRICS_ORDERING: Ordering = Ordering::Relaxed;

const _: () = assert!(
    GIF_LIST_CACHE_PARTS + CATEGORY_CACHE_PARTS + SUGGESTION_CACHE_PARTS + RESOLVED_CACHE_PARTS
        == CACHE_BUDGET_PARTS
);

#[derive(Clone)]
pub struct GifsShard {
    inner: Arc<GifsShardInner>,
}

struct GifsShardInner {
    klipy: KlipyClient,
    gif_lists: ManagedCache<Vec<GifItem>>,
    categories: ManagedCache<Vec<GifCategoryTag>>,
    suggestions: ManagedCache<Vec<String>>,
    resolved: ManagedCache<Option<GifItem>>,
    refreshing: Mutex<HashSet<RefreshKey>>,
    refresh_permits: Arc<Semaphore>,
}

struct ManagedCache<T> {
    cache: Cache<String, Cached<T>>,
    telemetry: Arc<CacheTelemetry>,
    max_weight_bytes: u64,
}

impl<T> Clone for ManagedCache<T> {
    fn clone(&self) -> Self {
        Self {
            cache: self.cache.clone(),
            telemetry: Arc::clone(&self.telemetry),
            max_weight_bytes: self.max_weight_bytes,
        }
    }
}

struct Cached<T> {
    data: Arc<T>,
    stored_at: Instant,
    generation: u64,
    policy: CachePolicy,
    retained_bytes: u32,
}

impl<T> Clone for Cached<T> {
    fn clone(&self) -> Self {
        Self {
            data: Arc::clone(&self.data),
            stored_at: self.stored_at,
            generation: self.generation,
            policy: self.policy,
            retained_bytes: self.retained_bytes,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct CachePolicy {
    soft_ttl: Duration,
    hard_ttl: Duration,
}

impl CachePolicy {
    const fn new(soft_ttl: Duration, hard_ttl: Duration) -> Self {
        assert!(soft_ttl.as_nanos() <= hard_ttl.as_nanos());
        Self { soft_ttl, hard_ttl }
    }
}

struct CachedExpiry;

impl<T> Expiry<String, Cached<T>> for CachedExpiry {
    fn expire_after_create(
        &self,
        _key: &String,
        value: &Cached<T>,
        _created_at: Instant,
    ) -> Option<Duration> {
        Some(value.policy.hard_ttl)
    }

    fn expire_after_update(
        &self,
        _key: &String,
        value: &Cached<T>,
        _updated_at: Instant,
        _duration_until_expiry: Option<Duration>,
    ) -> Option<Duration> {
        Some(value.policy.hard_ttl)
    }
}

#[derive(Clone, Hash, PartialEq, Eq)]
struct RefreshKey {
    cache_name: &'static str,
    key: String,
}

struct CacheTelemetry {
    name: &'static str,
    hits_total: AtomicU64,
    misses_total: AtomicU64,
    size_evictions_total: AtomicU64,
    expired_evictions_total: AtomicU64,
    refresh_success_total: AtomicU64,
    refresh_error_total: AtomicU64,
    refresh_superseded_total: AtomicU64,
    refresh_duplicate_total: AtomicU64,
    refresh_saturated_total: AtomicU64,
    refresh_in_flight: AtomicU64,
    next_generation: AtomicU64,
}

impl CacheTelemetry {
    fn new(name: &'static str) -> Self {
        Self {
            name,
            hits_total: AtomicU64::new(0),
            misses_total: AtomicU64::new(0),
            size_evictions_total: AtomicU64::new(0),
            expired_evictions_total: AtomicU64::new(0),
            refresh_success_total: AtomicU64::new(0),
            refresh_error_total: AtomicU64::new(0),
            refresh_superseded_total: AtomicU64::new(0),
            refresh_duplicate_total: AtomicU64::new(0),
            refresh_saturated_total: AtomicU64::new(0),
            refresh_in_flight: AtomicU64::new(0),
            next_generation: AtomicU64::new(1),
        }
    }

    fn next_generation(&self) -> u64 {
        self.next_generation
            .fetch_update(METRICS_ORDERING, METRICS_ORDERING, |generation| {
                generation.checked_add(1)
            })
            .expect("GIF cache generation exhausted")
    }

    fn record_removal(&self, cause: RemovalCause) {
        match cause {
            RemovalCause::Expired => {
                self.expired_evictions_total.fetch_add(1, METRICS_ORDERING);
            }
            RemovalCause::Size => {
                self.size_evictions_total.fetch_add(1, METRICS_ORDERING);
            }
            RemovalCause::Explicit | RemovalCause::Replaced => {}
        }
    }
}

struct RefreshRegistration {
    inner: Arc<GifsShardInner>,
    telemetry: Arc<CacheTelemetry>,
    key: RefreshKey,
}

impl Drop for RefreshRegistration {
    fn drop(&mut self) {
        let removed = self
            .inner
            .refreshing
            .lock()
            .expect("GIF refresh registry mutex poisoned")
            .remove(&self.key);
        assert!(removed, "GIF refresh registry lost an active key");
        self.telemetry
            .refresh_in_flight
            .fetch_sub(1, METRICS_ORDERING);
    }
}

#[derive(Clone, Copy)]
struct CacheBudgets {
    total: u64,
    gif_lists: u64,
    categories: u64,
    suggestions: u64,
    resolved: u64,
}

impl CacheBudgets {
    fn from_env() -> anyhow::Result<Self> {
        let total = match optional_env("FLUXER_GIFS_SHARD_CACHE_MAX_BYTES") {
            Some(value) => value.parse::<u64>().map_err(|error| {
                anyhow::anyhow!("invalid FLUXER_GIFS_SHARD_CACHE_MAX_BYTES: {error}")
            })?,
            None => DEFAULT_SHARD_CACHE_MAX_BYTES,
        };
        if total < MIN_SHARD_CACHE_MAX_BYTES {
            anyhow::bail!(
                "FLUXER_GIFS_SHARD_CACHE_MAX_BYTES must be at least {MIN_SHARD_CACHE_MAX_BYTES}"
            );
        }

        let part = total / CACHE_BUDGET_PARTS;
        let categories = part.saturating_mul(CATEGORY_CACHE_PARTS);
        let suggestions = part.saturating_mul(SUGGESTION_CACHE_PARTS);
        let resolved = part.saturating_mul(RESOLVED_CACHE_PARTS);
        let gif_lists = total
            .checked_sub(categories)
            .and_then(|remaining| remaining.checked_sub(suggestions))
            .and_then(|remaining| remaining.checked_sub(resolved))
            .expect("GIF cache budget partition exceeds total");
        assert!(gif_lists >= part.saturating_mul(GIF_LIST_CACHE_PARTS));

        Ok(Self {
            total,
            gif_lists,
            categories,
            suggestions,
            resolved,
        })
    }
}

trait HeapSize {
    fn heap_bytes(&self) -> u64;
}

impl HeapSize for String {
    fn heap_bytes(&self) -> u64 {
        usize_bytes(self.capacity())
    }
}

impl<T> HeapSize for Vec<T>
where
    T: HeapSize,
{
    fn heap_bytes(&self) -> u64 {
        self.iter().fold(
            usize_bytes(self.capacity()).saturating_mul(usize_bytes(size_of::<T>())),
            |bytes, value| bytes.saturating_add(value.heap_bytes()),
        )
    }
}

impl<T> HeapSize for Option<T>
where
    T: HeapSize,
{
    fn heap_bytes(&self) -> u64 {
        self.as_ref().map(HeapSize::heap_bytes).unwrap_or_default()
    }
}

impl HeapSize for GifMediaFormat {
    fn heap_bytes(&self) -> u64 {
        self.src
            .heap_bytes()
            .saturating_add(self.proxy_src.heap_bytes())
    }
}

impl HeapSize for GifItem {
    fn heap_bytes(&self) -> u64 {
        let string_bytes = [
            &self.id,
            &self.slug,
            &self.provider,
            &self.title,
            &self.url,
            &self.src,
            &self.proxy_src,
        ]
        .into_iter()
        .fold(0_u64, |bytes, value| {
            bytes.saturating_add(value.heap_bytes())
        });
        let media_bytes = self.media.iter().fold(0_u64, |bytes, (key, value)| {
            bytes
                .saturating_add(usize_bytes(size_of::<String>()))
                .saturating_add(usize_bytes(size_of::<GifMediaFormat>()))
                .saturating_add(BTREE_ENTRY_OVERHEAD_BYTES)
                .saturating_add(key.heap_bytes())
                .saturating_add(value.heap_bytes())
        });

        string_bytes
            .saturating_add(self.placeholder.heap_bytes())
            .saturating_add(media_bytes)
    }
}

impl HeapSize for GifCategoryTag {
    fn heap_bytes(&self) -> u64 {
        self.name
            .heap_bytes()
            .saturating_add(self.src.heap_bytes())
            .saturating_add(self.proxy_src.heap_bytes())
            .saturating_add(self.gif.heap_bytes())
    }
}

impl<T> Cached<T>
where
    T: HeapSize,
{
    fn new(data: T, policy: CachePolicy, generation: u64) -> Self {
        let retained_bytes = ARC_HEADER_BYTES
            .saturating_add(usize_bytes(size_of::<T>()))
            .saturating_add(data.heap_bytes());
        Self {
            data: Arc::new(data),
            stored_at: Instant::now(),
            generation,
            policy,
            retained_bytes: u32::try_from(retained_bytes).unwrap_or(u32::MAX).max(1),
        }
    }

    fn age(&self) -> Duration {
        self.stored_at.elapsed()
    }
}

impl GifsShard {
    pub fn new() -> anyhow::Result<Self> {
        let media_proxy = MediaProxyUrlBuilder::from_env()?;
        let klipy = KlipyClient::new(media_proxy)?;
        let budgets = CacheBudgets::from_env()?;
        tracing::info!(
            total_cache_max_bytes = budgets.total,
            gif_list_cache_max_bytes = budgets.gif_lists,
            category_cache_max_bytes = budgets.categories,
            suggestion_cache_max_bytes = budgets.suggestions,
            resolved_cache_max_bytes = budgets.resolved,
            "configured GIF shard cache budgets"
        );

        let inner = Arc::new(GifsShardInner {
            klipy,
            gif_lists: build_cache("gif_lists", budgets.gif_lists),
            categories: build_cache("categories", budgets.categories),
            suggestions: build_cache("suggestions", budgets.suggestions),
            resolved: build_cache("resolved", budgets.resolved),
            refreshing: Mutex::new(HashSet::new()),
            refresh_permits: Arc::new(Semaphore::new(MAX_CONCURRENT_REFRESHES)),
        });
        spawn_cache_maintenance(&inner);
        Ok(Self { inner })
    }

    async fn get_cached<T, Fetch, Fut>(
        &self,
        cache: ManagedCache<T>,
        key: String,
        policy: CachePolicy,
        fetch: Fetch,
    ) -> anyhow::Result<Arc<T>>
    where
        T: HeapSize + Send + Sync + 'static,
        Fetch: Fn() -> Fut + Clone + Send + Sync + 'static,
        Fut: Future<Output = anyhow::Result<T>> + Send + 'static,
    {
        if let Some(cached) = cache.cache.get(&key).await {
            assert_eq!(cached.policy, policy, "GIF cache policy mismatch for {key}");
            let age = cached.age();
            if age <= policy.hard_ttl {
                cache.telemetry.hits_total.fetch_add(1, METRICS_ORDERING);
                if age > policy.soft_ttl {
                    self.trigger_background_refresh(cache, key, policy, cached.generation, fetch);
                }
                return Ok(cached.data);
            }
            cache.cache.invalidate(&key).await;
        }

        cache.telemetry.misses_total.fetch_add(1, METRICS_ORDERING);
        let fetch_for_load = fetch.clone();
        let telemetry_for_load = Arc::clone(&cache.telemetry);
        let cached = cache
            .cache
            .try_get_with(key, async move {
                let data = fetch_for_load().await?;
                Ok::<Cached<T>, anyhow::Error>(Cached::new(
                    data,
                    policy,
                    telemetry_for_load.next_generation(),
                ))
            })
            .await
            .map_err(|error| anyhow::anyhow!("{}", error.as_ref()))?;
        Ok(cached.data)
    }

    fn trigger_background_refresh<T, Fetch, Fut>(
        &self,
        cache: ManagedCache<T>,
        key: String,
        policy: CachePolicy,
        observed_generation: u64,
        fetch: Fetch,
    ) where
        T: HeapSize + Send + Sync + 'static,
        Fetch: Fn() -> Fut + Clone + Send + Sync + 'static,
        Fut: Future<Output = anyhow::Result<T>> + Send + 'static,
    {
        let permit = match self.inner.refresh_permits.clone().try_acquire_owned() {
            Ok(permit) => permit,
            Err(TryAcquireError::NoPermits) => {
                cache
                    .telemetry
                    .refresh_saturated_total
                    .fetch_add(1, METRICS_ORDERING);
                return;
            }
            Err(TryAcquireError::Closed) => panic!("GIF refresh semaphore unexpectedly closed"),
        };
        let refresh_key = RefreshKey {
            cache_name: cache.telemetry.name,
            key: key.clone(),
        };
        {
            let mut refreshing = self
                .inner
                .refreshing
                .lock()
                .expect("GIF refresh registry mutex poisoned");
            if !refreshing.insert(refresh_key.clone()) {
                cache
                    .telemetry
                    .refresh_duplicate_total
                    .fetch_add(1, METRICS_ORDERING);
                return;
            }
        }
        cache
            .telemetry
            .refresh_in_flight
            .fetch_add(1, METRICS_ORDERING);
        let registration = RefreshRegistration {
            inner: Arc::clone(&self.inner),
            telemetry: Arc::clone(&cache.telemetry),
            key: refresh_key,
        };

        tokio::spawn(async move {
            let _permit = permit;
            let _registration = registration;
            match fetch().await {
                Ok(data) => {
                    let replacement = Cached::new(data, policy, cache.telemetry.next_generation());
                    let result = cache
                        .cache
                        .entry(key.clone())
                        .and_compute_with(move |current| {
                            let operation = match current {
                                Some(entry) if entry.value().generation == observed_generation => {
                                    Op::Put(replacement)
                                }
                                Some(_) => Op::Nop,
                                None => Op::Put(replacement),
                            };
                            std::future::ready(operation)
                        })
                        .await;
                    match result {
                        CompResult::Inserted(_) | CompResult::ReplacedWith(_) => {
                            cache
                                .telemetry
                                .refresh_success_total
                                .fetch_add(1, METRICS_ORDERING);
                        }
                        CompResult::Unchanged(_) => {
                            cache
                                .telemetry
                                .refresh_superseded_total
                                .fetch_add(1, METRICS_ORDERING);
                        }
                        CompResult::StillNone(_) | CompResult::Removed(_) => {
                            panic!("GIF cache refresh produced an impossible compute result");
                        }
                    }
                }
                Err(error) => {
                    cache
                        .telemetry
                        .refresh_error_total
                        .fetch_add(1, METRICS_ORDERING);
                    tracing::debug!(error = %error, cache_key = %key, "background GIF cache refresh failed");
                }
            }
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
        let key = format!("resolve:{}", resolve_cache_key(&url));
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

    fn render_cache_metrics<T>(&self, output: &mut String, cache: &ManagedCache<T>) {
        let telemetry = cache.telemetry.as_ref();
        let name = telemetry.name;
        let _ = writeln!(
            output,
            "fluxer_gifs_shard_cache_budget_bytes{{cache=\"{name}\"}} {}",
            cache.max_weight_bytes
        );
        let _ = writeln!(
            output,
            "fluxer_gifs_shard_cache_entries{{cache=\"{name}\"}} {}",
            cache.cache.entry_count()
        );
        let _ = writeln!(
            output,
            "fluxer_gifs_shard_cache_weighted_size_bytes{{cache=\"{name}\"}} {}",
            cache.cache.weighted_size()
        );
        let _ = writeln!(
            output,
            "fluxer_gifs_shard_cache_hits_total{{cache=\"{name}\"}} {}",
            telemetry.hits_total.load(METRICS_ORDERING)
        );
        let _ = writeln!(
            output,
            "fluxer_gifs_shard_cache_misses_total{{cache=\"{name}\"}} {}",
            telemetry.misses_total.load(METRICS_ORDERING)
        );
        let _ = writeln!(
            output,
            "fluxer_gifs_shard_cache_evictions_total{{cache=\"{name}\",cause=\"size\"}} {}",
            telemetry.size_evictions_total.load(METRICS_ORDERING)
        );
        let _ = writeln!(
            output,
            "fluxer_gifs_shard_cache_evictions_total{{cache=\"{name}\",cause=\"expired\"}} {}",
            telemetry.expired_evictions_total.load(METRICS_ORDERING)
        );
        let _ = writeln!(
            output,
            "fluxer_gifs_shard_cache_refreshes_total{{cache=\"{name}\",result=\"success\"}} {}",
            telemetry.refresh_success_total.load(METRICS_ORDERING)
        );
        let _ = writeln!(
            output,
            "fluxer_gifs_shard_cache_refreshes_total{{cache=\"{name}\",result=\"error\"}} {}",
            telemetry.refresh_error_total.load(METRICS_ORDERING)
        );
        let _ = writeln!(
            output,
            "fluxer_gifs_shard_cache_refreshes_total{{cache=\"{name}\",result=\"superseded\"}} {}",
            telemetry.refresh_superseded_total.load(METRICS_ORDERING)
        );
        let _ = writeln!(
            output,
            "fluxer_gifs_shard_cache_refreshes_total{{cache=\"{name}\",result=\"duplicate\"}} {}",
            telemetry.refresh_duplicate_total.load(METRICS_ORDERING)
        );
        let _ = writeln!(
            output,
            "fluxer_gifs_shard_cache_refreshes_total{{cache=\"{name}\",result=\"saturated\"}} {}",
            telemetry.refresh_saturated_total.load(METRICS_ORDERING)
        );
        let _ = writeln!(
            output,
            "fluxer_gifs_shard_cache_refreshes_in_flight{{cache=\"{name}\"}} {}",
            telemetry.refresh_in_flight.load(METRICS_ORDERING)
        );
    }
}

impl ShardService for GifsShard {
    type Request = GifRequest;
    type Response = GifServiceResponse;

    fn service_name(&self) -> &str {
        "gifs"
    }

    fn render_prometheus_metrics(&self, output: &mut String) {
        let _ = writeln!(output, "# TYPE fluxer_gifs_shard_cache_budget_bytes gauge");
        let _ = writeln!(output, "# TYPE fluxer_gifs_shard_cache_entries gauge");
        let _ = writeln!(
            output,
            "# TYPE fluxer_gifs_shard_cache_weighted_size_bytes gauge"
        );
        let _ = writeln!(output, "# TYPE fluxer_gifs_shard_cache_hits_total counter");
        let _ = writeln!(
            output,
            "# TYPE fluxer_gifs_shard_cache_misses_total counter"
        );
        let _ = writeln!(
            output,
            "# TYPE fluxer_gifs_shard_cache_evictions_total counter"
        );
        let _ = writeln!(
            output,
            "# TYPE fluxer_gifs_shard_cache_refreshes_total counter"
        );
        let _ = writeln!(
            output,
            "# TYPE fluxer_gifs_shard_cache_refreshes_in_flight gauge"
        );
        self.render_cache_metrics(output, &self.inner.gif_lists);
        self.render_cache_metrics(output, &self.inner.categories);
        self.render_cache_metrics(output, &self.inner.suggestions);
        self.render_cache_metrics(output, &self.inner.resolved);
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

fn usize_bytes(value: usize) -> u64 {
    u64::try_from(value).expect("usize must fit in u64")
}

fn cache_entry_weight<T>(key: &String, value: &Cached<T>) -> u32 {
    let bytes = CACHE_ENTRY_OVERHEAD_BYTES
        .saturating_add(usize_bytes(size_of::<Cached<T>>()))
        .saturating_add(usize_bytes(size_of::<String>()))
        .saturating_add(usize_bytes(key.capacity()))
        .saturating_add(u64::from(value.retained_bytes));
    u32::try_from(bytes).unwrap_or(u32::MAX).max(1)
}

fn build_cache<T>(name: &'static str, max_weight_bytes: u64) -> ManagedCache<T>
where
    T: HeapSize + Send + Sync + 'static,
{
    assert!(max_weight_bytes > 0);
    let telemetry = Arc::new(CacheTelemetry::new(name));
    let listener_telemetry = Arc::clone(&telemetry);
    let cache = Cache::<String, Cached<T>>::builder()
        .max_capacity(max_weight_bytes)
        .weigher(cache_entry_weight::<T>)
        .expire_after(CachedExpiry)
        .eviction_listener(move |_key, _value, cause| {
            listener_telemetry.record_removal(cause);
        })
        .build();
    ManagedCache {
        cache,
        telemetry,
        max_weight_bytes,
    }
}

fn spawn_cache_maintenance(inner: &Arc<GifsShardInner>) {
    let weak_inner = Arc::downgrade(inner);
    tokio::spawn(async move {
        let mut interval = tokio::time::interval(CACHE_MAINTENANCE_INTERVAL);
        interval.set_missed_tick_behavior(MissedTickBehavior::Skip);
        loop {
            interval.tick().await;
            let Some(inner) = weak_inner.upgrade() else {
                return;
            };
            tokio::join!(
                inner.gif_lists.cache.run_pending_tasks(),
                inner.categories.cache.run_pending_tasks(),
                inner.suggestions.cache.run_pending_tasks(),
                inner.resolved.cache.run_pending_tasks(),
            );
        }
    });
}
