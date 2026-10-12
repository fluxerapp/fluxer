// SPDX-License-Identifier: AGPL-3.0-or-later

use std::sync::Arc;
use std::time::Instant;
use thiserror::Error;
use tokio::sync::{OwnedSemaphorePermit, Semaphore, TryAcquireError};

#[derive(Debug, Error, Eq, PartialEq)]
pub enum TimedSemaphoreError {
    #[error("admission queue is full")]
    QueueFull,
    #[error("request timed out")]
    RequestTimeout,
    #[error("semaphore closed")]
    Closed,
}

#[derive(Clone, Debug)]
pub struct TimedSemaphore {
    execution: Arc<Semaphore>,
    admission: Arc<Semaphore>,
}

#[derive(Debug)]
pub struct TimedSemaphoreAdmission {
    _permit: OwnedSemaphorePermit,
}

#[derive(Debug)]
pub struct TimedSemaphorePermit {
    _execution: OwnedSemaphorePermit,
}

impl TimedSemaphore {
    pub fn with_queue_capacity(permits: usize, queue_capacity: usize) -> Self {
        let admission_capacity = permits
            .checked_add(queue_capacity)
            .expect("timed semaphore admission capacity overflow");
        Self {
            execution: Arc::new(Semaphore::new(permits)),
            admission: Arc::new(Semaphore::new(admission_capacity)),
        }
    }

    pub fn try_admit(&self) -> Result<TimedSemaphoreAdmission, TimedSemaphoreError> {
        let permit = self
            .admission
            .clone()
            .try_acquire_owned()
            .map_err(|error| match error {
                TryAcquireError::NoPermits => TimedSemaphoreError::QueueFull,
                TryAcquireError::Closed => TimedSemaphoreError::Closed,
            })?;
        Ok(TimedSemaphoreAdmission { _permit: permit })
    }

    pub async fn wait_until_admitted(
        &self,
        _admission: &TimedSemaphoreAdmission,
        deadline: Option<Instant>,
    ) -> Result<TimedSemaphorePermit, TimedSemaphoreError> {
        let execution = self.acquire_execution(deadline).await?;
        Ok(TimedSemaphorePermit {
            _execution: execution,
        })
    }

    async fn acquire_execution(
        &self,
        deadline: Option<Instant>,
    ) -> Result<OwnedSemaphorePermit, TimedSemaphoreError> {
        let acquire = self.execution.clone().acquire_owned();
        if let Some(deadline) = deadline {
            if Instant::now() >= deadline {
                return Err(TimedSemaphoreError::RequestTimeout);
            }
            tokio::time::timeout_at(deadline.into(), acquire)
                .await
                .map_err(|_| TimedSemaphoreError::RequestTimeout)?
                .map_err(|_| TimedSemaphoreError::Closed)
        } else {
            acquire.await.map_err(|_| TimedSemaphoreError::Closed)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::Duration;

    #[tokio::test]
    async fn queue_capacity_rejects_the_waiter_past_the_bound() {
        let sem = TimedSemaphore::with_queue_capacity(1, 2);
        let held_admission = sem.try_admit().unwrap();
        let _held = sem
            .wait_until_admitted(&held_admission, None)
            .await
            .unwrap();
        let first = sem.try_admit().unwrap();
        let _second = sem.try_admit().unwrap();
        assert_eq!(TimedSemaphoreError::QueueFull, sem.try_admit().unwrap_err());
        drop(first);
        let _third = sem.try_admit().unwrap();
    }

    #[tokio::test]
    async fn dropping_a_waiting_future_restores_the_permit_and_the_queue_slot() {
        let sem = TimedSemaphore::with_queue_capacity(1, 1);
        let held_admission = sem.try_admit().unwrap();
        let held = sem
            .wait_until_admitted(&held_admission, None)
            .await
            .unwrap();
        let mut waiting = Box::pin(async {
            let admission = sem.try_admit().unwrap();
            sem.wait_until_admitted(&admission, None).await
        });
        assert!(
            tokio::time::timeout(Duration::from_millis(20), &mut waiting)
                .await
                .is_err()
        );
        assert_eq!(TimedSemaphoreError::QueueFull, sem.try_admit().unwrap_err());
        drop(waiting);
        drop(held);
        drop(held_admission);
        let reacquired_admission = sem.try_admit().unwrap();
        let _reacquired = sem
            .wait_until_admitted(&reacquired_admission, None)
            .await
            .unwrap();
        let _queued = sem.try_admit().unwrap();
        assert_eq!(TimedSemaphoreError::QueueFull, sem.try_admit().unwrap_err());
    }

    #[tokio::test]
    async fn wait_until_admitted_reuses_the_admission_the_caller_holds() {
        let sem = TimedSemaphore::with_queue_capacity(1, 1);
        let admission = sem.try_admit().unwrap();
        let permit = sem
            .wait_until_admitted(
                &admission,
                Some(Instant::now() + Duration::from_millis(100)),
            )
            .await
            .unwrap();
        let _queued = sem.try_admit().unwrap();
        assert_eq!(TimedSemaphoreError::QueueFull, sem.try_admit().unwrap_err());
        drop(permit);
        drop(admission);
        let _readmitted = sem.try_admit().unwrap();
    }
}
