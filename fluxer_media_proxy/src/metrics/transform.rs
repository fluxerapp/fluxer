// SPDX-License-Identifier: AGPL-3.0-or-later

use super::histogram::Histogram;
use std::sync::atomic::{AtomicU64, Ordering};

pub struct TransformMetrics {
    pub(super) transform_image_duration: Histogram,
    pub(super) transform_video_duration: Histogram,
    pub(super) transform_failures: AtomicU64,
    pub(super) decode_failures: AtomicU64,
    pub(super) hdr_tone_map_count: AtomicU64,
    pub(super) heif_hdr_gain_map_count: AtomicU64,
    pub(super) avif_libheif_decode_count: AtomicU64,
    pub(super) avif_libheif_decode_failures: AtomicU64,
}

impl TransformMetrics {
    pub(crate) fn new() -> Self {
        Self {
            transform_image_duration: Histogram::new(),
            transform_video_duration: Histogram::new(),
            transform_failures: AtomicU64::new(0),
            decode_failures: AtomicU64::new(0),
            hdr_tone_map_count: AtomicU64::new(0),
            heif_hdr_gain_map_count: AtomicU64::new(0),
            avif_libheif_decode_count: AtomicU64::new(0),
            avif_libheif_decode_failures: AtomicU64::new(0),
        }
    }

    pub fn observe_image_duration(&self, ms: u64) {
        self.transform_image_duration.observe(ms);
    }

    pub fn observe_video_duration(&self, ms: u64) {
        self.transform_video_duration.observe(ms);
    }

    pub fn record_transform_failure(&self) {
        self.transform_failures.fetch_add(1, Ordering::Relaxed);
    }

    pub fn record_decode_failure(&self) {
        self.decode_failures.fetch_add(1, Ordering::Relaxed);
    }

    pub fn record_hdr_tone_map(&self) {
        self.hdr_tone_map_count.fetch_add(1, Ordering::Relaxed);
    }

    pub fn hdr_tone_map_count(&self) -> u64 {
        self.hdr_tone_map_count.load(Ordering::Relaxed)
    }

    pub fn record_heif_hdr_gain_map_detected(&self) {
        self.heif_hdr_gain_map_count.fetch_add(1, Ordering::Relaxed);
    }

    pub fn record_heif_primary_still_decode(&self) {
        self.avif_libheif_decode_count
            .fetch_add(1, Ordering::Relaxed);
    }

    pub fn record_heif_primary_still_decode_failure(&self) {
        self.avif_libheif_decode_failures
            .fetch_add(1, Ordering::Relaxed);
    }
}

pub struct NativeTransformMetrics {
    pub(super) native_transform_wait: Histogram,
    pub(super) native_transform_rejected: AtomicU64,
    pub(super) native_task_detached_duration: Histogram,
    pub(super) native_tasks_detached: AtomicU64,
    pub(super) native_tasks_detached_active: AtomicU64,
    pub(super) framebuffer_pool_borrows: AtomicU64,
    pub(super) framebuffer_pool_grow_events: AtomicU64,
}

impl NativeTransformMetrics {
    pub(crate) fn new() -> Self {
        Self {
            native_transform_wait: Histogram::new(),
            native_transform_rejected: AtomicU64::new(0),
            native_task_detached_duration: Histogram::new(),
            native_tasks_detached: AtomicU64::new(0),
            native_tasks_detached_active: AtomicU64::new(0),
            framebuffer_pool_borrows: AtomicU64::new(0),
            framebuffer_pool_grow_events: AtomicU64::new(0),
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

    pub fn record_framebuffer_pool_borrow(&self) {
        self.framebuffer_pool_borrows
            .fetch_add(1, Ordering::Relaxed);
    }

    pub fn record_framebuffer_pool_grow(&self) {
        self.framebuffer_pool_grow_events
            .fetch_add(1, Ordering::Relaxed);
    }
}
