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
