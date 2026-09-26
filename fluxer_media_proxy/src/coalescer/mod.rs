// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    byte_budget::{BudgetedBytes, ByteBudget},
    media_process::{MediaBytes, MediaError},
    metrics::cache::CoalescerMetrics,
};
use parking_lot::Mutex;
use std::{collections::HashMap, future::Future, sync::Arc, time::Instant};
use thiserror::Error;
use tokio::sync::{Notify, OwnedSemaphorePermit, Semaphore, TryAcquireError};

#[cfg(test)]
mod tests;

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
    fn publish<C>(&mut self, result: Result<BudgetedBytes, CoalescerError>, complete: C)
    where
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
        self.coalescer.release(self.key);
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
        self.coalescer.release(self.key);
        self.slot.notify.notify_waiters();
    }
}

#[derive(Debug)]
pub struct ByteCoalescer {
    in_flight: Mutex<HashMap<String, Arc<Slot>>>,
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
            in_flight: Mutex::new(HashMap::new()),
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
            if self.in_flight.lock().is_empty() {
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
            self.metrics.record_leader();
            let mut guard = CoordinatorGuard {
                coalescer: self,
                key: key.as_str(),
                slot: &slot,
                _permit: permit,
                state: CoordinatorState::Active,
            };
            let result = work().await.map_err(coalesced_work_error).and_then(|data| {
                data.try_into_budgeted(&self.budget)
                    .ok_or(CoalescerError::BufferBudgetExhausted)
            });
            guard.publish(result.clone(), complete);
            return result;
        }

        drop(work);
        drop(complete);
        self.metrics.record_waiter();
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
        let mut in_flight = self.in_flight.lock();
        if let Some(existing) = in_flight.get(key) {
            let permit = self
                .waiter_capacity
                .clone()
                .try_acquire_owned()
                .map_err(|error| match error {
                    TryAcquireError::NoPermits => {
                        self.metrics.record_waiter_rejected();
                        CoalescerError::Overloaded
                    }
                    TryAcquireError::Closed => CoalescerError::Unavailable,
                })?;
            return Ok((existing.clone(), None, Some(permit)));
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
        in_flight.insert(key.to_owned(), slot.clone());
        Ok((slot, Some(permit), None))
    }

    fn release(&self, key: &str) {
        let idle = {
            let mut in_flight = self.in_flight.lock();
            let removed = in_flight.remove(key);
            assert!(removed.is_some());
            in_flight.is_empty()
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
    if error.downcast_ref::<MediaError>() == Some(&MediaError::AllocationFailed) {
        return CoalescerError::AllocationFailed;
    }
    tracing::error!(error = ?error, "coalesced work failed");
    CoalescerError::WorkFailed
}
