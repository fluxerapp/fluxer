// SPDX-License-Identifier: AGPL-3.0-or-later

use bytes::Bytes;
use parking_lot::Mutex;
use std::{collections::HashMap, future::Future, sync::Arc, time::Instant};
use thiserror::Error;
use tokio::sync::Notify;

#[derive(Clone, Copy, Debug, Error, Eq, PartialEq)]
pub enum CoalescerError {
    #[error("request timed out")]
    RequestTimeout,
    #[error("coalesced work failed")]
    WorkFailed,
}

#[derive(Debug)]
struct Slot {
    state: Mutex<Option<Result<Bytes, CoalescerError>>>,
    notify: Notify,
}

#[derive(Debug, Default)]
pub struct ByteCoalescer {
    in_flight: Mutex<HashMap<String, Arc<Slot>>>,
}

impl ByteCoalescer {
    pub fn new() -> Self {
        Self::default()
    }

    pub async fn run_once<F, Fut>(
        &self,
        key: impl Into<String>,
        work: F,
    ) -> Result<Bytes, CoalescerError>
    where
        F: FnOnce() -> Fut,
        Fut: Future<Output = anyhow::Result<Vec<u8>>>,
    {
        self.run_once_until(key, None, work).await
    }

    pub async fn run_once_until<F, Fut>(
        &self,
        key: impl Into<String>,
        deadline: Option<Instant>,
        work: F,
    ) -> Result<Bytes, CoalescerError>
    where
        F: FnOnce() -> Fut,
        Fut: Future<Output = anyhow::Result<Vec<u8>>>,
    {
        let key = key.into();
        let (slot, leader) = {
            let mut in_flight = self.in_flight.lock();
            if let Some(existing) = in_flight.get(&key) {
                (existing.clone(), false)
            } else {
                let slot = Arc::new(Slot {
                    state: Mutex::new(None),
                    notify: Notify::new(),
                });
                in_flight.insert(key.clone(), slot.clone());
                (slot, true)
            }
        };

        if leader {
            crate::metrics::GLOBAL
                .coalescer_leader
                .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
            let result = work().await.map(Bytes::from).map_err(coalesced_work_error);
            *slot.state.lock() = Some(result.clone());
            slot.notify.notify_waiters();
            self.in_flight.lock().remove(&key);
            result
        } else {
            drop(work);
            crate::metrics::GLOBAL
                .coalescer_waiter
                .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
            loop {
                if let Some(result) = slot.state.lock().as_ref().cloned() {
                    return result;
                }
                if let Some(deadline) = deadline {
                    let now = Instant::now();
                    if now >= deadline {
                        return Err(CoalescerError::RequestTimeout);
                    }
                    if tokio::time::timeout_at(deadline.into(), slot.notify.notified())
                        .await
                        .is_err()
                    {
                        return Err(CoalescerError::RequestTimeout);
                    }
                } else {
                    slot.notify.notified().await;
                }
            }
        }
    }
}

fn coalesced_work_error(error: anyhow::Error) -> CoalescerError {
    if let Some(known) = error.downcast_ref::<CoalescerError>().copied() {
        return known;
    }
    tracing::error!(error = ?error, "coalesced work failed");
    CoalescerError::WorkFailed
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicU32, Ordering};
    use tokio::time::{Duration, sleep};

    #[tokio::test]
    async fn single_thread_run_once_returns_work_output() {
        let coalescer = ByteCoalescer::new();
        let counter = AtomicU32::new(0);
        let result = coalescer
            .run_once("k", || async {
                counter.fetch_add(1, Ordering::SeqCst);
                Ok(b"hello".to_vec())
            })
            .await
            .unwrap();
        assert_eq!(b"hello", result.as_ref());
        assert_eq!(1, counter.load(Ordering::SeqCst));
    }

    #[tokio::test]
    async fn failure_propagates() {
        let coalescer = ByteCoalescer::new();
        let err = coalescer
            .run_once("k", || async {
                Err::<Vec<u8>, _>(anyhow::anyhow!("intentional"))
            })
            .await
            .unwrap_err();
        assert_eq!(CoalescerError::WorkFailed, err);
    }

    #[tokio::test]
    async fn waiter_can_time_out_behind_a_slow_leader() {
        let coalescer = Arc::new(ByteCoalescer::new());
        let leader = coalescer.clone();
        let task = tokio::spawn(async move {
            let _ = leader
                .run_once("slow-key", || async {
                    sleep(Duration::from_millis(50)).await;
                    Ok(b"slow".to_vec())
                })
                .await;
        });
        sleep(Duration::from_millis(5)).await;
        let err = coalescer
            .run_once_until(
                "slow-key",
                Some(Instant::now() + Duration::from_millis(1)),
                || async { Ok(b"should-not-run".to_vec()) },
            )
            .await
            .unwrap_err();
        assert_eq!(CoalescerError::RequestTimeout, err);
        task.await.unwrap();
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 4)]
    async fn concurrent_dedup_counter_not_more_than_total_calls() {
        let coalescer = Arc::new(ByteCoalescer::new());
        let counter = Arc::new(AtomicU32::new(0));
        let mut tasks = Vec::new();
        for _ in 0..4 {
            let c = coalescer.clone();
            let counter = counter.clone();
            tasks.push(tokio::spawn(async move {
                for _ in 0..50 {
                    let counter = counter.clone();
                    let result = c
                        .run_once("hot-key", || async move {
                            counter.fetch_add(1, Ordering::SeqCst);
                            sleep(Duration::from_millis(1)).await;
                            Ok(b"OK".to_vec())
                        })
                        .await
                        .unwrap();
                    assert_eq!(b"OK", result.as_ref());
                }
            }));
        }
        for task in tasks {
            task.await.unwrap();
        }
        let total = 4 * 50;
        let observed = counter.load(Ordering::SeqCst);
        assert!(observed > 0);
        assert!(observed <= total);
    }
}
