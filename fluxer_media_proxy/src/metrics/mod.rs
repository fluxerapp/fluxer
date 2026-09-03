// SPDX-License-Identifier: AGPL-3.0-or-later

pub mod cache;
pub mod external;
pub mod histogram;
pub mod http_client;
pub mod nsfw;
pub mod relay;
mod rendering;
pub mod request;
pub mod storage;
pub mod transform;

#[cfg(test)]
mod tests;

use self::{
    cache::{CoalescerMetrics, TransformCacheMetrics},
    external::ExternalMetrics,
    http_client::HTTPClientMetrics,
    nsfw::NSFWMetrics,
    relay::RelayMetrics,
    rendering::render_metrics,
    request::RequestMetrics,
    storage::StorageMetrics,
    transform::{NativeTransformMetrics, TransformMetrics},
};
use libc::{CLOCK_MONOTONIC, clock_gettime, timespec};
use std::sync::Arc;
use std::time::Duration;

pub fn now_ms() -> i64 {
    let mut ts = timespec {
        tv_sec: 0,
        tv_nsec: 0,
    };
    let rc = unsafe { clock_gettime(CLOCK_MONOTONIC, &mut ts) };
    if rc != 0 {
        return 0;
    }
    ts.tv_sec
        .saturating_mul(1_000)
        .saturating_add(ts.tv_nsec / 1_000_000)
}

pub fn duration_millis(duration: Duration) -> u64 {
    u64::try_from(duration.as_millis()).unwrap_or(u64::MAX)
}

pub struct Metrics {
    request: Arc<RequestMetrics>,
    transform: Arc<TransformMetrics>,
    native_transform: Arc<NativeTransformMetrics>,
    coalescer: Arc<CoalescerMetrics>,
    transform_cache: Arc<TransformCacheMetrics>,
    storage: Arc<StorageMetrics>,
    nsfw: Arc<NSFWMetrics>,
    external: Arc<ExternalMetrics>,
    relay: Arc<RelayMetrics>,
    http_client: Arc<HTTPClientMetrics>,
    start_ms: i64,
}

impl Metrics {
    pub fn new() -> Self {
        Self {
            request: Arc::new(RequestMetrics::new()),
            transform: Arc::new(TransformMetrics::new()),
            native_transform: Arc::new(NativeTransformMetrics::new()),
            coalescer: Arc::new(CoalescerMetrics::new()),
            transform_cache: Arc::new(TransformCacheMetrics::new()),
            storage: Arc::new(StorageMetrics::new()),
            nsfw: Arc::new(NSFWMetrics::new()),
            external: Arc::new(ExternalMetrics::new()),
            relay: Arc::new(RelayMetrics::new()),
            http_client: Arc::new(HTTPClientMetrics::new()),
            start_ms: now_ms(),
        }
    }

    pub fn request(&self) -> Arc<RequestMetrics> {
        Arc::clone(&self.request)
    }

    pub fn transform(&self) -> Arc<TransformMetrics> {
        Arc::clone(&self.transform)
    }

    pub fn native_transform(&self) -> Arc<NativeTransformMetrics> {
        Arc::clone(&self.native_transform)
    }

    pub fn coalescer(&self) -> Arc<CoalescerMetrics> {
        Arc::clone(&self.coalescer)
    }

    pub fn transform_cache(&self) -> Arc<TransformCacheMetrics> {
        Arc::clone(&self.transform_cache)
    }

    pub fn storage(&self) -> Arc<StorageMetrics> {
        Arc::clone(&self.storage)
    }

    pub fn nsfw(&self) -> Arc<NSFWMetrics> {
        Arc::clone(&self.nsfw)
    }

    pub fn external(&self) -> Arc<ExternalMetrics> {
        Arc::clone(&self.external)
    }

    pub fn relay(&self) -> Arc<RelayMetrics> {
        Arc::clone(&self.relay)
    }

    pub fn http_client(&self) -> Arc<HTTPClientMetrics> {
        Arc::clone(&self.http_client)
    }

    pub fn render(&self) -> String {
        let mut out = String::new();
        render_metrics(&mut out, self)
            .expect("writing media proxy metrics to a String cannot fail");
        out
    }
}

impl Default for Metrics {
    fn default() -> Self {
        Self::new()
    }
}
