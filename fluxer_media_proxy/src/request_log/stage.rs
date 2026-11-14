// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::metrics;
use std::{
    future::Future,
    sync::{
        Arc,
        atomic::{AtomicU64, Ordering},
    },
    time::Instant,
};

#[derive(Clone, Copy, Debug)]
pub enum Stage {
    Fetch,
    Transform,
    Nsfw,
}

#[derive(Debug, Default)]
pub(super) struct StageTimings {
    fetch_ms: AtomicU64,
    transform_ms: AtomicU64,
    nsfw_ms: AtomicU64,
}

#[derive(Clone, Copy, Debug)]
pub(super) struct StageTimingSnapshot {
    pub(super) fetch_ms: u64,
    pub(super) transform_ms: u64,
    pub(super) nsfw_ms: u64,
}

impl StageTimings {
    fn add(&self, stage: Stage, milliseconds: u64) {
        let slot = match stage {
            Stage::Fetch => &self.fetch_ms,
            Stage::Transform => &self.transform_ms,
            Stage::Nsfw => &self.nsfw_ms,
        };
        slot.fetch_add(milliseconds, Ordering::Relaxed);
    }

    pub(super) fn snapshot(&self) -> StageTimingSnapshot {
        StageTimingSnapshot {
            fetch_ms: self.fetch_ms.load(Ordering::Relaxed),
            transform_ms: self.transform_ms.load(Ordering::Relaxed),
            nsfw_ms: self.nsfw_ms.load(Ordering::Relaxed),
        }
    }
}

tokio::task_local! {
    static STAGES: Arc<StageTimings>;
}

pub fn record_stage(stage: Stage, milliseconds: u64) {
    let _ = STAGES.try_with(|stages| stages.add(stage, milliseconds));
}

pub async fn timed_stage<F, T>(stage: Stage, future: F) -> T
where
    F: Future<Output = T>,
{
    let started = Instant::now();
    let output = future.await;
    record_stage(stage, metrics::duration_millis(started.elapsed()));
    output
}

pub(super) async fn scope<F>(stages: Arc<StageTimings>, future: F) -> F::Output
where
    F: Future,
{
    STAGES.scope(stages, future).await
}
