// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    byte_budget::{BudgetedBytes, ByteBudget},
    media_process::{MediaBytes, MediaError},
    metrics::cache::CoalescerMetrics,
};
use parking_lot::Mutex;
use std::{
    collections::HashMap,
    future::Future,
    sync::Arc,
    time::{Duration, Instant},
};
use thiserror::Error;
use tokio::sync::{Notify, OwnedSemaphorePermit, Semaphore, TryAcquireError};

#[cfg(test)]
mod tests;

const TIMED_OUT_RETRY_AFTER: Duration = Duration::from_secs(60);

#[derive(Clone, Copy, Debug, Error, Eq, PartialEq)]
pub enum CoalescerError {
    #[error("native transform queue is full")]
    Overloaded,
    #[error("coalesced work is unavailable")]
    Unavailable,
    #[error("buffered output byte budget exhausted")]
    BufferBudgetExhausted,
    #[error("native transform allocation failed")]
    AllocationFailed,
    #[error("media too large to process")]
    TooLargeToProcess,
    #[error("request timed out")]
    RequestTimeout,
    #[error("coalesced work failed")]
    WorkFailed,
    #[error("coalesced work was cancelled")]
    WorkCancelled,
}

type SlotClaim = (
    Arc<Slot>,
    Option<OwnedSemaphorePermit>,
    Option<OwnedSemaphorePermit>,
);

#[derive(Debug)]
struct Slot {
    state: Mutex<Option<Result<BudgetedBytes, CoalescerError>>>,
    notify: Notify,
}

#[derive(Debug, Default)]
struct Keys {
    in_flight: HashMap<String, Arc<Slot>>,
    timed_out: HashMap<String, Instant>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum CoordinatorState {
    Active,
    Published,
}

struct CoordinatorGuard<'a> {
    coalescer: &'a ByteCoalescer,
    key: &'a str,
    slot: &'a Arc<Slot>,
    _permit: OwnedSemaphorePermit,
    state: CoordinatorState,
}

impl CoordinatorGuard<'_> {
    fn publish<C>(
        &mut self,
        result: Result<BudgetedBytes, CoalescerError>,
        timed_out: bool,
        complete: C,
    ) where
        C: FnOnce(&BudgetedBytes),
    {
        if let Ok(bytes) = &result {
            complete(bytes);
        }
        {
            let mut state = self.slot.state.lock();
            assert!(state.is_none());
            *state = Some(result);
        }
        self.coalescer.release(self.key, timed_out);
        self.state = CoordinatorState::Published;
        self.slot.notify.notify_waiters();
    }
}

impl Drop for CoordinatorGuard<'_> {
    fn drop(&mut self) {
        if self.state == CoordinatorState::Published {
            return;
        }
        {
            let mut state = self.slot.state.lock();
            if state.is_none() {
                *state = Some(Err(CoalescerError::WorkCancelled));
            }
        }
        self.coalescer.release(self.key, false);
        self.slot.notify.notify_waiters();
    }
}

#[derive(Debug)]
pub struct ByteCoalescer {
    keys: Mutex<Keys>,
    budget: ByteBudget,
    coordinator_capacity: Arc<Semaphore>,
    waiter_capacity: Arc<Semaphore>,
    idle: Notify,
    metrics: Arc<CoalescerMetrics>,
}

impl ByteCoalescer {
    pub const UNBOUNDED_CAPACITY: usize = Semaphore::MAX_PERMITS;

    pub fn with_budget(
        budget: ByteBudget,
        max_in_flight: usize,
        max_waiters: usize,
        metrics: Arc<CoalescerMetrics>,
    ) -> Self {
        assert!(max_in_flight > 0);
        assert!(max_waiters > 0);
        Self {
            keys: Mutex::new(Keys::default()),
            budget,
            coordinator_capacity: Arc::new(Semaphore::new(max_in_flight)),
            waiter_capacity: Arc::new(Semaphore::new(max_waiters)),
            idle: Notify::new(),
            metrics,
        }
    }

    pub fn begin_shutdown(&self) {
        self.coordinator_capacity.close();
        self.waiter_capacity.close();
    }

    pub async fn wait_for_shutdown(&self) {
        assert!(self.coordinator_capacity.is_closed());
        loop {
            let idle = self.idle.notified();
            tokio::pin!(idle);
            idle.as_mut().enable();
            if self.keys.lock().in_flight.is_empty() {
                return;
            }
            idle.await;
        }
    }

    pub async fn run_once_until<F, Fut, C>(
        &self,
        key: impl Into<String>,
        deadline: Option<Instant>,
        work: F,
        complete: C,
    ) -> Result<BudgetedBytes, CoalescerError>
    where
        F: FnOnce() -> Fut,
        Fut: Future<Output = anyhow::Result<MediaBytes>>,
        C: FnOnce(&BudgetedBytes),
    {
        let key = key.into();
        let (slot, coordinator_permit, waiter_permit) = self.claim(&key)?;
        let _waiter_permit = waiter_permit;

        if let Some(permit) = coordinator_permit {
            self.metrics.coalescer_leader.inc();
            let mut guard = CoordinatorGuard {
                coalescer: self,
                key: key.as_str(),
                slot: &slot,
                _permit: permit,
                state: CoordinatorState::Active,
            };
            let outcome = work().await;
            let timed_out = outcome.as_ref().is_err_and(|error| {
                error.downcast_ref::<MediaError>() == Some(&MediaError::RequestTimeout)
            });
            let result = outcome.map_err(coalesced_work_error).and_then(|data| {
                data.try_into_budgeted(&self.budget)
                    .ok_or(CoalescerError::BufferBudgetExhausted)
            });
            guard.publish(result.clone(), timed_out, complete);
            return result;
        }

        drop(work);
        drop(complete);
        self.metrics.coalescer_waiter.inc();
        loop {
            let notified = slot.notify.notified();
            tokio::pin!(notified);
            notified.as_mut().enable();
            if let Some(result) = slot.state.lock().as_ref().cloned() {
                return result;
            }
            if let Some(deadline) = deadline {
                if Instant::now() >= deadline {
                    return Err(CoalescerError::RequestTimeout);
                }
                if tokio::time::timeout_at(deadline.into(), notified)
                    .await
                    .is_err()
                {
                    return Err(CoalescerError::RequestTimeout);
                }
            } else {
                notified.await;
            }
        }
    }

    fn claim(&self, key: &str) -> Result<SlotClaim, CoalescerError> {
        let mut keys = self.keys.lock();
        if let Some(existing) = keys.in_flight.get(key) {
            let permit = self
                .waiter_capacity
                .clone()
                .try_acquire_owned()
                .map_err(|error| match error {
                    TryAcquireError::NoPermits => {
                        self.metrics.coalescer_waiter_rejected.inc();
                        CoalescerError::Overloaded
                    }
                    TryAcquireError::Closed => CoalescerError::Unavailable,
                })?;
            return Ok((existing.clone(), None, Some(permit)));
        }
        if keys
            .timed_out
            .get(key)
            .is_some_and(|until| Instant::now() < *until)
        {
            return Err(CoalescerError::RequestTimeout);
        }
        let permit = self
            .coordinator_capacity
            .clone()
            .try_acquire_owned()
            .map_err(|error| match error {
                TryAcquireError::NoPermits => CoalescerError::Overloaded,
                TryAcquireError::Closed => CoalescerError::Unavailable,
            })?;
        let slot = Arc::new(Slot {
            state: Mutex::new(None),
            notify: Notify::new(),
        });
        keys.in_flight.insert(key.to_owned(), slot.clone());
        Ok((slot, Some(permit), None))
    }

    fn release(&self, key: &str, timed_out: bool) {
        let idle = {
            let mut keys = self.keys.lock();
            let removed = keys.in_flight.remove(key);
            assert!(removed.is_some());
            if timed_out {
                let now = Instant::now();
                keys.timed_out.retain(|_, until| *until > now);
                keys.timed_out
                    .insert(key.to_owned(), now + TIMED_OUT_RETRY_AFTER);
            }
            keys.in_flight.is_empty()
        };
        if idle {
            self.idle.notify_waiters();
        }
    }
}

fn coalesced_work_error(error: anyhow::Error) -> CoalescerError {
    if let Some(known) = error.downcast_ref::<CoalescerError>().copied() {
        return known;
    }
    match error.downcast_ref::<MediaError>() {
        Some(MediaError::AllocationFailed) => return CoalescerError::AllocationFailed,
        Some(MediaError::TooLargeToProcess) => return CoalescerError::TooLargeToProcess,
        Some(MediaError::RequestTimeout) => return CoalescerError::RequestTimeout,
        _ => {}
    }
    tracing::error!(error = ?error, "coalesced work failed");
    CoalescerError::WorkFailed
}
