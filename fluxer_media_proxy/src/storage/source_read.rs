// SPDX-License-Identifier: AGPL-3.0-or-later

use super::{BufferedStorageObject, StorageError};
use parking_lot::Mutex;
use std::{
    collections::HashMap,
    sync::{
        Arc,
        atomic::{AtomicUsize, Ordering},
    },
};
use tokio::sync::Notify;

const SOURCE_READ_MAX_IN_FLIGHT: usize = 256;
const SOURCE_READ_MAX_WAITERS: usize = 4_096;

#[derive(Clone)]
pub(super) struct SourceReadCoordinator {
    in_flight: Arc<Mutex<HashMap<String, Arc<SourceReadSlot>>>>,
    active_waiters: Arc<AtomicUsize>,
}

struct SourceReadSlot {
    state: Mutex<Option<Result<BufferedStorageObject, SourceReadFailure>>>,
    notify: Notify,
}

#[derive(Clone)]
enum SourceReadFailure {
    LeaderDropped,
    NotFound,
    InvalidKey,
    InvalidBucket,
    ReadOnlyStorage,
    StreamTooLong,
    InvalidS3Endpoint,
    ObjectChanged,
    BufferBudgetExhausted,
    BufferAllocationFailed,
    Backend(String),
}

pub(super) struct SourceReadLeader {
    in_flight: Arc<Mutex<HashMap<String, Arc<SourceReadSlot>>>>,
    key: String,
    slot: Arc<SourceReadSlot>,
    state: SourceReadLeaderState,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum SourceReadLeaderState {
    Active,
    Published,
}

pub(super) struct SourceReadWaiter {
    slot: Arc<SourceReadSlot>,
    active_waiters: Arc<AtomicUsize>,
}

pub(super) enum SourceReadClaim {
    Leader(SourceReadLeader),
    Waiter(SourceReadWaiter),
}

pub(super) enum SourceReadWaitOutcome {
    Retry,
    Completed(Result<BufferedStorageObject, StorageError>),
}

impl SourceReadCoordinator {
    pub(super) fn new() -> Self {
        Self {
            in_flight: Arc::new(Mutex::new(HashMap::new())),
            active_waiters: Arc::new(AtomicUsize::new(0)),
        }
    }

    pub(super) fn claim(&self, key: String) -> Result<SourceReadClaim, StorageError> {
        let mut in_flight = self.in_flight.lock();
        if let Some(existing) = in_flight.get(&key) {
            self.active_waiters
                .fetch_update(Ordering::AcqRel, Ordering::Acquire, |active| {
                    (active < SOURCE_READ_MAX_WAITERS).then_some(active + 1)
                })
                .map_err(|_| StorageError::SourceReadWaiterCapacityExhausted)?;
            return Ok(SourceReadClaim::Waiter(SourceReadWaiter {
                slot: Arc::clone(existing),
                active_waiters: Arc::clone(&self.active_waiters),
            }));
        }
        if in_flight.len() >= SOURCE_READ_MAX_IN_FLIGHT {
            return Err(StorageError::SourceReadCapacityExhausted);
        }
        let slot = Arc::new(SourceReadSlot {
            state: Mutex::new(None),
            notify: Notify::new(),
        });
        in_flight.insert(key.clone(), Arc::clone(&slot));
        Ok(SourceReadClaim::Leader(SourceReadLeader {
            in_flight: Arc::clone(&self.in_flight),
            key,
            slot,
            state: SourceReadLeaderState::Active,
        }))
    }
}

impl Drop for SourceReadWaiter {
    fn drop(&mut self) {
        let previous = self.active_waiters.fetch_sub(1, Ordering::AcqRel);
        assert!(previous > 0);
    }
}

impl SourceReadLeader {
    pub(super) fn publish(mut self, result: &Result<BufferedStorageObject, StorageError>) {
        let result = match result {
            Ok(object) => Ok(object.clone()),
            Err(error) => Err(SourceReadFailure::from(error)),
        };
        {
            let mut state = self.slot.state.lock();
            assert!(state.is_none());
            *state = Some(result);
        }
        let removed = self.in_flight.lock().remove(&self.key);
        assert!(removed.is_some());
        self.state = SourceReadLeaderState::Published;
        self.slot.notify.notify_waiters();
    }
}

impl Drop for SourceReadLeader {
    fn drop(&mut self) {
        if self.state == SourceReadLeaderState::Published {
            return;
        }
        {
            let mut state = self.slot.state.lock();
            if state.is_none() {
                *state = Some(Err(SourceReadFailure::LeaderDropped));
            }
        }
        let removed = self.in_flight.lock().remove(&self.key);
        assert!(removed.is_some());
        self.slot.notify.notify_waiters();
    }
}

impl SourceReadWaiter {
    pub(super) async fn wait(self) -> SourceReadWaitOutcome {
        loop {
            let notified = self.slot.notify.notified();
            tokio::pin!(notified);
            notified.as_mut().enable();
            if let Some(result) = self.slot.state.lock().as_ref().cloned() {
                return match result {
                    Err(SourceReadFailure::LeaderDropped) => SourceReadWaitOutcome::Retry,
                    result => SourceReadWaitOutcome::Completed(
                        result.map_err(SourceReadFailure::into_storage_error),
                    ),
                };
            }
            notified.await;
        }
    }
}

impl SourceReadFailure {
    fn into_storage_error(self) -> StorageError {
        match self {
            Self::LeaderDropped => unreachable!("dropped source read leaders are re-elected"),
            Self::NotFound => StorageError::NotFound,
            Self::InvalidKey => StorageError::InvalidKey,
            Self::InvalidBucket => StorageError::InvalidBucket,
            Self::ReadOnlyStorage => StorageError::ReadOnlyStorage,
            Self::StreamTooLong => StorageError::StreamTooLong,
            Self::InvalidS3Endpoint => StorageError::InvalidS3Endpoint,
            Self::ObjectChanged => StorageError::ObjectChanged,
            Self::BufferBudgetExhausted => StorageError::BufferBudgetExhausted,
            Self::BufferAllocationFailed => StorageError::BufferAllocationFailed,
            Self::Backend(error) => StorageError::CoalescedSourceReadFailed(error),
        }
    }
}

impl From<&StorageError> for SourceReadFailure {
    fn from(error: &StorageError) -> Self {
        match error {
            StorageError::NotFound => Self::NotFound,
            StorageError::InvalidKey => Self::InvalidKey,
            StorageError::InvalidBucket => Self::InvalidBucket,
            StorageError::ReadOnlyStorage => Self::ReadOnlyStorage,
            StorageError::StreamTooLong => Self::StreamTooLong,
            StorageError::InvalidS3Endpoint => Self::InvalidS3Endpoint,
            StorageError::ObjectChanged => Self::ObjectChanged,
            StorageError::BufferBudgetExhausted => Self::BufferBudgetExhausted,
            StorageError::BufferAllocationFailed => Self::BufferAllocationFailed,
            StorageError::SourceReadCapacityExhausted
            | StorageError::SourceReadWaiterCapacityExhausted
            | StorageError::SourceReadLeaderEnded
            | StorageError::CoalescedSourceReadFailed(_)
            | StorageError::ObjectStorage(_)
            | StorageError::S3(_)
            | StorageError::Io(_)
            | StorageError::Http(_)
            | StorageError::HttpMiddleware(_)
            | StorageError::Sign(_) => Self::Backend(error.to_string()),
        }
    }
}
