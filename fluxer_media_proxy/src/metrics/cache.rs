// SPDX-License-Identifier: AGPL-3.0-or-later

#[derive(Debug, Default)]
pub struct CoalescerMetrics {
    pub coalescer_leader: Counter,
    pub coalescer_waiter: Counter,
    pub coalescer_waiter_rejected: Counter,
}

impl CoalescerMetrics {}

#[derive(Debug, Default)]
pub struct TransformCacheMetrics {
    pub transform_cache_hits: Counter,
    pub transform_cache_misses: Counter,
    pub transform_cache_insert_rejected: Counter,
    pub transform_cache_evictions: Counter,
}

impl TransformCacheMetrics {}

use super::Counter;
