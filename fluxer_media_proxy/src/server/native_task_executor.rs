// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    byte_budget::ByteBudget,
    media_process::{MediaError, NativeCost},
    metrics::{
        self,
        transform::{NativeTransformMetrics, TransformMetrics},
    },
    timed_semaphore::{TimedSemaphore, TimedSemaphoreError},
};
use parking_lot::Mutex;
use std::{
    sync::{
        Arc,
        atomic::{AtomicBool, AtomicU64, Ordering},
    },
    time::Instant,
};
use tokio::sync::{Notify, oneshot};
use tracing::{error, warn};

pub(in crate::server) struct NativeTaskExecutorSettings {
    pub(in crate::server) max_native_transforms: usize,
    pub(in crate::server) worker_queue_capacity: usize,
    pub(in crate::server) memory_budget_bytes: usize,
    pub(in crate::server) native_metrics: Arc<NativeTransformMetrics>,
    pub(in crate::server) transform_metrics: Arc<TransformMetrics>,
}

pub(in crate::server) struct NativeTaskExecutor {
    native_transforms: TimedSemaphore,
    memory: ByteBudget,
    memory_budget_bytes: usize,
    tasks: NativeTaskTracker,
    native_metrics: Arc<NativeTransformMetrics>,
    transform_metrics: Arc<TransformMetrics>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum NativeTaskState {
    Running,
    Detached,
    Complete,
}

struct NativeTaskWait {
    state: Arc<Mutex<NativeTaskState>>,
    metrics: Arc<NativeTransformMetrics>,
}

#[derive(Clone, Default)]
struct NativeTaskTracker {
    inner: Arc<NativeTaskTrackerInner>,
}

#[derive(Default)]
struct NativeTaskTrackerInner {
    active: AtomicU64,
    closed: AtomicBool,
    idle: Notify,
}

struct NativeTaskToken {
    tracker: NativeTaskTracker,
}

impl NativeTaskExecutor {
    pub(in crate::server) fn new(settings: NativeTaskExecutorSettings) -> Self {
        let NativeTaskExecutorSettings {
            max_native_transforms,
            worker_queue_capacity,
            memory_budget_bytes,
            native_metrics,
            transform_metrics,
        } = settings;
        assert!(memory_budget_bytes > 0);
        Self {
            native_transforms: TimedSemaphore::with_queue_capacity(
                max_native_transforms,
                worker_queue_capacity,
            ),
            memory: ByteBudget::new(memory_budget_bytes),
            memory_budget_bytes,
            tasks: NativeTaskTracker::default(),
            native_metrics,
            transform_metrics,
        }
    }

    pub(in crate::server) async fn run_native<T, C, F>(
        &self,
        deadline: Option<Instant>,
        cost: C,
        work: F,
    ) -> anyhow::Result<T>
    where
        T: Send + 'static,
        C: FnOnce() -> anyhow::Result<NativeCost> + Send + 'static,
        F: FnOnce() -> anyhow::Result<T> + Send + 'static,
    {
        let admission = match self.native_transforms.try_admit() {
            Ok(admission) => admission,
            Err(TimedSemaphoreError::QueueFull) => {
                self.native_metrics.record_rejected();
                return Err(TimedSemaphoreError::QueueFull.into());
            }
            Err(error) => return Err(error.into()),
        };
        let wait_started = Instant::now();
        let permit = self
            .native_transforms
            .wait_until_admitted(&admission, deadline)
            .await;
        let admitted = Instant::now();
        let waited = admitted.duration_since(wait_started);
        self.native_metrics
            .observe_wait(metrics::duration_millis(waited));
        let permit = permit?;
        let memory = self.memory.clone();
        let capacity = self.memory_budget_bytes;
        let span = tracing::Span::current();
        self.run_task(deadline, move || {
            let _span = span.enter();
            let _permit = permit;
            let bytes = match cost()? {
                NativeCost::Bytes(bytes) if bytes > capacity => {
                    warn!(
                        bytes,
                        capacity, "native work refused, larger than the memory budget"
                    );
                    return Err(MediaError::TooLargeToProcess.into());
                }
                NativeCost::Bytes(bytes) => bytes,
                NativeCost::UpToBudget(bytes) => bytes.min(capacity),
            };
            let Some(_reserved) = memory.try_reserve(bytes) else {
                warn!(bytes, capacity, "native work refused, memory budget in use");
                return Err(MediaError::AllocationFailed.into());
            };
            work()
        })
        .await
        .map_err(|error| {
            let ran_short = deadline
                .is_some_and(|deadline| deadline.saturating_duration_since(admitted) < waited);
            if ran_short && error.downcast_ref() == Some(&MediaError::RequestTimeout) {
                TimedSemaphoreError::RequestTimeout.into()
            } else {
                error
            }
        })
    }

    pub(in crate::server) fn begin_shutdown(&self) {
        self.tasks.close();
    }

    pub(in crate::server) async fn wait_for_shutdown(&self) {
        assert!(self.tasks.is_closed());
        self.tasks.wait().await;
    }

    async fn run_task<T, F>(&self, deadline: Option<Instant>, work: F) -> anyhow::Result<T>
    where
        T: Send + 'static,
        F: FnOnce() -> anyhow::Result<T> + Send + 'static,
    {
        let Some(token) = self.tasks.token() else {
            return Err(TimedSemaphoreError::Closed.into());
        };
        if deadline.is_some_and(|deadline| Instant::now() >= deadline) {
            return Err(TimedSemaphoreError::RequestTimeout.into());
        }
        let (sender, receiver) = oneshot::channel();
        let started = Instant::now();
        let state = Arc::new(Mutex::new(NativeTaskState::Running));
        let wait = NativeTaskWait {
            state: Arc::clone(&state),
            metrics: Arc::clone(&self.native_metrics),
        };
        let blocking = tokio::task::spawn_blocking(work);
        let native_metrics = Arc::clone(&self.native_metrics);
        let transform_metrics = Arc::clone(&self.transform_metrics);
        drop(tokio::spawn(async move {
            let _token = token;
            let result = match blocking.await {
                Ok(result) => result,
                Err(error) => Err(anyhow::anyhow!("native transform failed to join: {error}")),
            };
            let previous = {
                let mut state = state.lock();
                let previous = *state;
                *state = NativeTaskState::Complete;
                previous
            };
            if previous == NativeTaskState::Detached {
                native_metrics
                    .record_detached_finished(metrics::duration_millis(started.elapsed()));
            }
            if result
                .as_ref()
                .is_err_and(|error| error.downcast_ref() == Some(&MediaError::MediaDecodeFailed))
            {
                transform_metrics.decode_failures.inc();
            }
            if let Err(Err(error)) = sender.send(result) {
                error!(error = ?error, "detached native transform failed");
            }
        }));
        let completion = match deadline {
            Some(deadline) => match tokio::time::timeout_at(deadline.into(), receiver).await {
                Ok(completion) => completion,
                Err(_) => return Err(MediaError::RequestTimeout.into()),
            },
            None => receiver.await,
        };
        let completion =
            completion.map_err(|error| anyhow::anyhow!("native transform ended: {error}"))?;
        drop(wait);
        completion
    }
}

impl Drop for NativeTaskWait {
    fn drop(&mut self) {
        let mut state = self.state.lock();
        if *state == NativeTaskState::Running {
            self.metrics.record_detached_started();
            *state = NativeTaskState::Detached;
        }
    }
}

impl NativeTaskTracker {
    fn token(&self) -> Option<NativeTaskToken> {
        if self.inner.closed.load(Ordering::Acquire) {
            return None;
        }
        self.inner
            .active
            .try_update(Ordering::AcqRel, Ordering::Acquire, |active| {
                active.checked_add(1)
            })
            .expect("native task count must not overflow");
        Some(NativeTaskToken {
            tracker: self.clone(),
        })
    }

    fn close(&self) {
        self.inner.closed.store(true, Ordering::Release);
        self.inner.idle.notify_waiters();
    }

    fn is_closed(&self) -> bool {
        self.inner.closed.load(Ordering::Acquire)
    }

    async fn wait(&self) {
        loop {
            let idle = self.inner.idle.notified();
            tokio::pin!(idle);
            idle.as_mut().enable();
            if self.inner.active.load(Ordering::Acquire) == 0 {
                return;
            }
            idle.await;
        }
    }
}

impl Drop for NativeTaskToken {
    fn drop(&mut self) {
        let previous = self.tracker.inner.active.fetch_sub(1, Ordering::AcqRel);
        assert!(previous > 0, "native task count must stay positive");
        if previous == 1 {
            self.tracker.inner.idle.notify_waiters();
        }
    }
}
