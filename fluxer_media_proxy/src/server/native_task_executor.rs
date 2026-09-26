// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    byte_budget::ByteBudget,
    media_process::MediaError,
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
use tracing::error;

pub(in crate::server) struct NativeTaskExecutorSettings {
    pub(in crate::server) max_native_transforms: usize,
    pub(in crate::server) worker_queue_capacity: usize,
    pub(in crate::server) decoded_bytes_per_transform: usize,
    pub(in crate::server) native_metrics: Arc<NativeTransformMetrics>,
    pub(in crate::server) transform_metrics: Arc<TransformMetrics>,
}

pub(in crate::server) struct NativeTaskExecutor {
    native_transforms: TimedSemaphore,
    decoded_bytes: ByteBudget,
    decoded_bytes_per_transform: usize,
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
            decoded_bytes_per_transform,
            native_metrics,
            transform_metrics,
        } = settings;
        assert!(decoded_bytes_per_transform > 0);
        let decoded_bytes_capacity = decoded_bytes_per_transform
            .checked_mul(max_native_transforms)
            .expect("native decoded image budget must not overflow");
        Self {
            native_transforms: TimedSemaphore::with_queue_capacity(
                max_native_transforms,
                worker_queue_capacity,
            ),
            decoded_bytes: ByteBudget::new(decoded_bytes_capacity),
            decoded_bytes_per_transform,
            tasks: NativeTaskTracker::default(),
            native_metrics,
            transform_metrics,
        }
    }

    pub(in crate::server) async fn run_native<T, F>(
        &self,
        deadline: Option<Instant>,
        work: F,
    ) -> anyhow::Result<T>
    where
        T: Send + 'static,
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
        self.native_metrics
            .observe_wait(metrics::duration_millis(wait_started.elapsed()));
        let permit = permit?;
        let decoded_bytes = self
            .decoded_bytes
            .try_reserve(self.decoded_bytes_per_transform)
            .ok_or(MediaError::AllocationFailed)?;
        self.run_task(deadline, move || {
            let _permit = permit;
            let _decoded_bytes = decoded_bytes;
            work()
        })
        .await
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
                transform_metrics.record_decode_failure();
            }
            if let Err(Err(error)) = sender.send(result) {
                error!(error = ?error, "detached native transform failed");
            }
        }));
        let completion = match deadline {
            Some(deadline) => match tokio::time::timeout_at(deadline.into(), receiver).await {
                Ok(completion) => completion,
                Err(_) => return Err(TimedSemaphoreError::RequestTimeout.into()),
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
            .fetch_update(Ordering::AcqRel, Ordering::Acquire, |active| {
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::Duration;
    use tokio::sync::mpsc;

    fn executor(metrics: &metrics::Metrics, permits: usize, queue: usize) -> NativeTaskExecutor {
        NativeTaskExecutor::new(NativeTaskExecutorSettings {
            max_native_transforms: permits,
            worker_queue_capacity: queue,
            decoded_bytes_per_transform: 1024,
            native_metrics: metrics.native_transform(),
            transform_metrics: metrics.transform(),
        })
    }

    fn counter(rendered: &str, name: &str) -> u64 {
        rendered
            .lines()
            .find_map(|line| line.strip_prefix(name)?.trim().parse().ok())
            .expect("counter series is rendered")
    }

    #[tokio::test]
    async fn a_task_abandoned_at_its_deadline_is_counted_as_detached_until_it_finishes() {
        let metrics = metrics::Metrics::new();
        let executor = executor(&metrics, 1, 0);
        let (release, mut released) = mpsc::channel::<()>(1);
        let (finished, mut task_finished) = mpsc::channel::<()>(1);
        let error = executor
            .run_native(
                Some(Instant::now() + Duration::from_millis(30)),
                move || {
                    released.blocking_recv();
                    let _ = finished.blocking_send(());
                    Ok(())
                },
            )
            .await
            .expect_err("the task outlives its deadline");
        assert_eq!(
            Some(&TimedSemaphoreError::RequestTimeout),
            error.downcast_ref::<TimedSemaphoreError>()
        );
        let rendered = metrics.render();
        assert_eq!(
            1,
            counter(&rendered, "fluxer_media_proxy_native_tasks_detached_total")
        );
        assert_eq!(
            1,
            counter(&rendered, "fluxer_media_proxy_native_tasks_detached_active")
        );
        drop(release);
        task_finished.recv().await;
        for _ in 0..100 {
            if counter(
                &metrics.render(),
                "fluxer_media_proxy_native_tasks_detached_active",
            ) == 0
            {
                break;
            }
            tokio::time::sleep(Duration::from_millis(10)).await;
        }
        let rendered = metrics.render();
        assert_eq!(
            1,
            counter(&rendered, "fluxer_media_proxy_native_tasks_detached_total")
        );
        assert_eq!(
            0,
            counter(&rendered, "fluxer_media_proxy_native_tasks_detached_active"),
            "a detached task decrements the active gauge when it finishes"
        );
    }

    #[tokio::test]
    async fn a_task_that_completes_before_its_deadline_is_never_detached() {
        let metrics = metrics::Metrics::new();
        let executor = executor(&metrics, 1, 0);
        let value = executor
            .run_native(Some(Instant::now() + Duration::from_secs(30)), || Ok(7u32))
            .await
            .expect("the task completes");
        assert_eq!(7, value);
        let rendered = metrics.render();
        assert_eq!(
            0,
            counter(&rendered, "fluxer_media_proxy_native_tasks_detached_total")
        );
        assert_eq!(
            0,
            counter(&rendered, "fluxer_media_proxy_native_tasks_detached_active")
        );
    }

    #[tokio::test]
    async fn an_over_queued_transform_is_rejected_without_waiting() {
        let metrics = metrics::Metrics::new();
        let executor = Arc::new(executor(&metrics, 1, 0));
        let (release, mut released) = mpsc::channel::<()>(1);
        let holder = Arc::clone(&executor);
        let held = tokio::spawn(async move {
            holder
                .run_native(None, move || {
                    released.blocking_recv();
                    Ok(())
                })
                .await
        });
        tokio::time::sleep(Duration::from_millis(50)).await;
        let error = executor
            .run_native(None, || Ok(()))
            .await
            .expect_err("the admission queue is full");
        assert_eq!(
            Some(&TimedSemaphoreError::QueueFull),
            error.downcast_ref::<TimedSemaphoreError>()
        );
        assert_eq!(
            1,
            counter(
                &metrics.render(),
                "fluxer_media_proxy_native_transform_rejected_total"
            )
        );
        drop(release);
        held.await.expect("held task").expect("held work");
    }

    #[tokio::test]
    async fn a_closed_executor_refuses_new_work_and_drains() {
        let metrics = metrics::Metrics::new();
        let executor = executor(&metrics, 2, 4);
        executor.begin_shutdown();
        let error = executor
            .run_native(None, || Ok(()))
            .await
            .expect_err("a closed executor admits nothing");
        assert_eq!(
            Some(&TimedSemaphoreError::Closed),
            error.downcast_ref::<TimedSemaphoreError>()
        );
        executor.wait_for_shutdown().await;
    }
}
