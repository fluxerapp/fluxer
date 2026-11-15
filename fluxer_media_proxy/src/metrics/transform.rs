// SPDX-License-Identifier: AGPL-3.0-or-later

use fluxer_common::metrics::Histogram;
use std::sync::atomic::{AtomicU64, Ordering};

#[derive(Default)]
pub struct TransformMetrics {
    pub transform_image_duration: Histogram,
    pub transform_video_duration: Histogram,
    pub transform_failures: Counter,
    pub decode_failures: Counter,
    pub hdr_tone_map_count: Counter,
    pub heif_hdr_gain_map_count: Counter,
    pub avif_libheif_decode_count: Counter,
    pub avif_libheif_decode_failures: Counter,
}

impl TransformMetrics {
    pub fn observe_image_duration(&self, ms: u64) {
        self.transform_image_duration.observe(ms);
    }

    pub fn observe_video_duration(&self, ms: u64) {
        self.transform_video_duration.observe(ms);
    }
}

pub struct NativeTransformMetrics {
    pub(super) native_transform_wait: Histogram,
    pub(super) native_transform_rejected: AtomicU64,
    pub(super) native_task_detached_duration: Histogram,
    pub(super) native_tasks_detached: AtomicU64,
    pub(super) native_tasks_detached_active: AtomicU64,
}

impl NativeTransformMetrics {
    pub(crate) fn new() -> Self {
        Self {
            native_transform_wait: Histogram::new(),
            native_transform_rejected: AtomicU64::new(0),
            native_task_detached_duration: Histogram::new(),
            native_tasks_detached: AtomicU64::new(0),
            native_tasks_detached_active: AtomicU64::new(0),
        }
    }

    pub fn observe_wait(&self, ms: u64) {
        self.native_transform_wait.observe(ms);
    }

    pub fn record_rejected(&self) {
        self.native_transform_rejected
            .fetch_add(1, Ordering::Relaxed);
    }

    pub fn record_detached_started(&self) {
        self.native_tasks_detached.fetch_add(1, Ordering::Relaxed);
        self.native_tasks_detached_active
            .fetch_add(1, Ordering::Relaxed);
    }

    pub fn record_detached_finished(&self, ms: u64) {
        let previous = self
            .native_tasks_detached_active
            .fetch_sub(1, Ordering::Relaxed);
        assert!(
            previous > 0,
            "detached native task count must not underflow"
        );
        self.native_task_detached_duration.observe(ms);
    }
}

use super::Counter;
