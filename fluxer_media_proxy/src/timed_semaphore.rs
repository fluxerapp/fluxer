// SPDX-License-Identifier: AGPL-3.0-or-later

use std::time::Instant;
use thiserror::Error;
use tokio::sync::{OwnedSemaphorePermit, Semaphore, TryAcquireError};

#[derive(Debug, Error, Eq, PartialEq)]
pub enum TimedSemaphoreError {
    #[error("request timed out")]
    RequestTimeout,
    #[error("semaphore closed")]
    Closed,
}

#[derive(Clone, Debug)]
pub struct TimedSemaphore {
    inner: std::sync::Arc<Semaphore>,
}

impl TimedSemaphore {
    pub fn new(permits: usize) -> Self {
        Self {
            inner: std::sync::Arc::new(Semaphore::new(permits)),
        }
    }

    pub async fn wait_until(
        &self,
        deadline: Option<Instant>,
    ) -> Result<OwnedSemaphorePermit, TimedSemaphoreError> {
        let acquire = self.inner.clone().acquire_owned();
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

    pub fn try_wait(&self) -> Result<OwnedSemaphorePermit, TimedSemaphoreError> {
        self.inner
            .clone()
            .try_acquire_owned()
            .map_err(|error| match error {
                TryAcquireError::NoPermits => TimedSemaphoreError::RequestTimeout,
                TryAcquireError::Closed => TimedSemaphoreError::Closed,
            })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::Duration;

    #[tokio::test]
    async fn wait_until_times_out_when_no_permits_available() {
        let sem = TimedSemaphore::new(0);
        let err = sem
            .wait_until(Some(Instant::now() + Duration::from_millis(5)))
            .await
            .unwrap_err();
        assert_eq!(TimedSemaphoreError::RequestTimeout, err);
    }

    #[tokio::test]
    async fn wait_until_consumes_and_post_restores_permits() {
        let sem = TimedSemaphore::new(1);
        let permit = sem
            .wait_until(Some(Instant::now() + Duration::from_millis(100)))
            .await
            .unwrap();
        let err = sem
            .wait_until(Some(Instant::now() + Duration::from_millis(1)))
            .await
            .unwrap_err();
        assert_eq!(TimedSemaphoreError::RequestTimeout, err);
        drop(permit);
        let _permit2 = sem
            .wait_until(Some(Instant::now() + Duration::from_millis(100)))
            .await
            .unwrap();
    }
}
