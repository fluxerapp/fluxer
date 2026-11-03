// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    byte_budget::{BudgetedBytes, ByteBudget},
    byte_cache::{ByteCache, ByteCacheSettings, ByteCacheValue},
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
                eviction_metrics.transform_cache_evictions.inc();
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
        self.metrics.transform_cache_hits.inc();
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
        self.metrics.transform_cache_misses.inc();
        let cached = Arc::clone(&self.cached);
        let metrics = Arc::clone(&self.metrics);
        self.in_flight
            .run_once_until(key.clone(), deadline, work, {
                let cache_key = key;
                move |bytes| {
                    if !cached.put(
                        cache_key,
                        CachedTransform {
                            data: bytes.clone(),
                            format,
                        },
                    ) {
                        metrics.transform_cache_insert_rejected.inc();
                    }
                }
            })
            .await
    }
}
