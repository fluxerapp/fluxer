// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    byte_budget::{BudgetedBytes, ByteBudget},
    byte_cache::{ByteCache, ByteCacheInsertOutcome, ByteCacheSettings, ByteCacheValue},
    coalescer::{ByteCoalescer, CoalescerError},
    media_process::MediaBytes,
    metrics::cache::{CoalescerMetrics, TransformCacheMetrics},
    output_format::OutputFormat,
};
use std::{future::Future, sync::Arc, time::Instant};

#[derive(Clone, Debug)]
pub struct TransformCache {
    cached: Arc<ByteCache<CachedTransform>>,
    in_flight: Arc<ByteCoalescer>,
    metrics: Arc<TransformCacheMetrics>,
}

#[derive(Clone, Debug)]
pub struct CachedTransform {
    pub data: BudgetedBytes,
    pub format: OutputFormat,
}

impl ByteCacheValue for CachedTransform {
    fn bytes(&self) -> &BudgetedBytes {
        &self.data
    }
}

pub struct TransformCacheSettings {
    pub cache: ByteCacheSettings,
    pub budget: ByteBudget,
    pub max_in_flight: usize,
    pub max_waiters: usize,
    pub cache_metrics: Arc<TransformCacheMetrics>,
    pub coalescer_metrics: Arc<CoalescerMetrics>,
}

impl TransformCache {
    pub fn new(settings: TransformCacheSettings) -> Self {
        let eviction_metrics = Arc::clone(&settings.cache_metrics);
        Self {
            cached: Arc::new(ByteCache::new(settings.cache, move || {
                eviction_metrics.record_eviction();
            })),
            in_flight: Arc::new(ByteCoalescer::with_budget(
                settings.budget,
                settings.max_in_flight,
                settings.max_waiters,
                settings.coalescer_metrics,
            )),
            metrics: settings.cache_metrics,
        }
    }

    pub fn get(&self, key: &str) -> Option<CachedTransform> {
        let cached = self.cached.get(key)?;
        self.metrics.record_hit();
        Some(cached)
    }

    pub fn begin_shutdown(&self) {
        self.in_flight.begin_shutdown();
    }

    pub async fn wait_for_shutdown(&self) {
        self.in_flight.wait_for_shutdown().await;
    }

    pub async fn get_or_run<F, Fut>(
        &self,
        key: String,
        format: OutputFormat,
        deadline: Option<Instant>,
        work: F,
    ) -> Result<BudgetedBytes, CoalescerError>
    where
        F: FnOnce() -> Fut,
        Fut: Future<Output = anyhow::Result<MediaBytes>>,
    {
        if let Some(cached) = self.get(&key) {
            assert_eq!(cached.format, format);
            return Ok(cached.data);
        }
        self.metrics.record_miss();
        let cached = Arc::clone(&self.cached);
        let metrics = Arc::clone(&self.metrics);
        self.in_flight
            .run_once_until(key.clone(), deadline, work, {
                let cache_key = key;
                move |bytes| {
                    if let ByteCacheInsertOutcome::Rejected(_) = cached.put(
                        cache_key,
                        CachedTransform {
                            data: bytes.clone(),
                            format,
                        },
                    ) {
                        metrics.record_insert_rejected();
                    }
                }
            })
            .await
    }

    #[cfg(test)]
    fn settle(&self) {
        self.cached.settle();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::metrics::Metrics;
    use std::sync::atomic::{AtomicU32, Ordering};

    fn counter(rendered: &str, name: &str) -> u64 {
        rendered
            .lines()
            .find_map(|line| line.strip_prefix(name)?.trim().parse().ok())
            .expect("counter series is rendered")
    }

    fn transform_cache(
        metrics: &Metrics,
        capacity_bytes: usize,
        max_entry_bytes: usize,
    ) -> TransformCache {
        TransformCache::new(TransformCacheSettings {
            cache: ByteCacheSettings::clamped(capacity_bytes, max_entry_bytes, 60_000),
            budget: ByteBudget::new(1 << 20),
            max_in_flight: 8,
            max_waiters: 8,
            cache_metrics: metrics.transform_cache(),
            coalescer_metrics: metrics.coalescer(),
        })
    }

    #[tokio::test]
    async fn the_second_request_for_a_key_is_served_from_the_cache() {
        let metrics = Metrics::new();
        let cache = transform_cache(&metrics, 65_536, 65_536);
        let runs = AtomicU32::new(0);
        for _ in 0..2 {
            let bytes = cache
                .get_or_run(
                    "stored:abc|fmt=webp".to_owned(),
                    OutputFormat::WebP,
                    None,
                    || async {
                        runs.fetch_add(1, Ordering::SeqCst);
                        Ok(MediaBytes::from(vec![1, 2, 3, 4]))
                    },
                )
                .await
                .expect("transform result");
            assert_eq!(&[1, 2, 3, 4], bytes.as_ref());
        }
        assert_eq!(1, runs.load(Ordering::SeqCst));
        let rendered = metrics.render();
        assert!(rendered.contains("fluxer_media_proxy_transform_cache_hits_total 1\n"));
        assert!(rendered.contains("fluxer_media_proxy_transform_cache_misses_total 1\n"));
    }

    #[tokio::test]
    async fn an_output_over_the_entry_ceiling_is_returned_but_not_cached() {
        let metrics = Metrics::new();
        let cache = transform_cache(&metrics, 65_536, 1_024);
        let bytes = cache
            .get_or_run(
                "stored:big|fmt=png".to_owned(),
                OutputFormat::PNG,
                None,
                || async { Ok(MediaBytes::from(vec![0u8; 4_096])) },
            )
            .await
            .expect("transform result");
        assert_eq!(4_096, bytes.len());
        assert!(cache.get("stored:big|fmt=png").is_none());
        assert!(
            metrics
                .render()
                .contains("fluxer_media_proxy_transform_cache_insert_rejected_total 1\n")
        );
    }

    #[tokio::test]
    async fn entries_evicted_by_the_capacity_bound_are_counted() {
        let metrics = Metrics::new();
        let cache = transform_cache(&metrics, 8_192, 8_192);
        for index in 0..64 {
            cache
                .get_or_run(
                    format!("stored:{index}|fmt=gif"),
                    OutputFormat::GIF,
                    None,
                    || async { Ok(MediaBytes::from(vec![0u8; 512])) },
                )
                .await
                .expect("transform result");
        }
        cache.settle();
        let rendered = metrics.render();
        assert!(
            counter(
                &rendered,
                "fluxer_media_proxy_transform_cache_evictions_total"
            ) > 0
        );
        assert_eq!(
            0,
            counter(
                &rendered,
                "fluxer_media_proxy_transform_cache_insert_rejected_total"
            )
        );
    }

    #[tokio::test]
    async fn a_disabled_cache_still_runs_and_returns_the_transform() {
        let metrics = Metrics::new();
        let cache = transform_cache(&metrics, 0, 0);
        let bytes = cache
            .get_or_run(
                "stored:off|fmt=jpeg".to_owned(),
                OutputFormat::JPEG,
                None,
                || async { Ok(MediaBytes::from(vec![5, 6])) },
            )
            .await
            .expect("transform result");
        assert_eq!(&[5, 6], bytes.as_ref());
        assert!(cache.get("stored:off|fmt=jpeg").is_none());
        cache.begin_shutdown();
        cache.wait_for_shutdown().await;
    }
}
