// SPDX-License-Identifier: AGPL-3.0-or-later

use super::*;
use crate::metrics::Metrics;
use std::sync::atomic::{AtomicU32, Ordering};
use tokio::time::{Duration, sleep};

fn coalescer() -> Arc<ByteCoalescer> {
    Arc::new(ByteCoalescer::with_budget(
        ByteBudget::new(1 << 20),
        8,
        64,
        Arc::new(CoalescerMetrics::new()),
    ))
}

#[tokio::test]
async fn single_thread_run_once_returns_work_output() {
    let coalescer = coalescer();
    let counter = AtomicU32::new(0);
    let result = coalescer
        .run_once_until(
            "k",
            None,
            || async {
                counter.fetch_add(1, Ordering::SeqCst);
                Ok(MediaBytes::from(b"hello".to_vec()))
            },
            |_| {},
        )
        .await
        .unwrap();
    assert_eq!(b"hello", result.as_ref());
    assert_eq!(1, counter.load(Ordering::SeqCst));
}

#[tokio::test]
async fn failure_propagates() {
    let coalescer = coalescer();
    let err = coalescer
        .run_once_until(
            "k",
            None,
            || async { Err::<MediaBytes, _>(anyhow::anyhow!("intentional")) },
            |_| {},
        )
        .await
        .unwrap_err();
    assert_eq!(CoalescerError::WorkFailed, err);
}

#[tokio::test]
async fn waiter_can_time_out_behind_a_slow_leader() {
    let coalescer = coalescer();
    let leader = coalescer.clone();
    let task = tokio::spawn(async move {
        let _ = leader
            .run_once_until(
                "slow-key",
                None,
                || async {
                    sleep(Duration::from_millis(50)).await;
                    Ok(MediaBytes::from(b"slow".to_vec()))
                },
                |_| {},
            )
            .await;
    });
    sleep(Duration::from_millis(5)).await;
    let err = coalescer
        .run_once_until(
            "slow-key",
            Some(Instant::now() + Duration::from_millis(1)),
            || async { Ok(MediaBytes::from(b"should-not-run".to_vec())) },
            |_| {},
        )
        .await
        .unwrap_err();
    assert_eq!(CoalescerError::RequestTimeout, err);
    task.await.unwrap();
}

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn concurrent_dedup_counter_not_more_than_total_calls() {
    let coalescer = coalescer();
    let counter = Arc::new(AtomicU32::new(0));
    let mut tasks = Vec::new();
    for _ in 0..4 {
        let c = coalescer.clone();
        let counter = counter.clone();
        tasks.push(tokio::spawn(async move {
            for _ in 0..50 {
                let counter = counter.clone();
                let result = c
                    .run_once_until(
                        "hot-key",
                        None,
                        || async move {
                            counter.fetch_add(1, Ordering::SeqCst);
                            sleep(Duration::from_millis(1)).await;
                            Ok(MediaBytes::from(b"OK".to_vec()))
                        },
                        |_| {},
                    )
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

#[tokio::test]
async fn cancelled_leader_does_not_poison_the_key() {
    let coalescer = coalescer();
    let leader = coalescer.clone();
    let task = tokio::spawn(async move {
        let _ = leader
            .run_once_until(
                "poison-key",
                None,
                || async {
                    sleep(Duration::from_secs(60)).await;
                    Ok(MediaBytes::from(b"never".to_vec()))
                },
                |_| {},
            )
            .await;
    });
    sleep(Duration::from_millis(20)).await;
    task.abort();
    let _ = task.await;

    let result = coalescer
        .run_once_until(
            "poison-key",
            Some(Instant::now() + Duration::from_secs(2)),
            || async { Ok(MediaBytes::from(b"recovered".to_vec())) },
            |_| {},
        )
        .await;
    assert_eq!(
        b"recovered".as_ref(),
        result
            .expect("cancelled leader left the key poisoned in in_flight")
            .as_ref()
    );
}

#[tokio::test]
async fn waiters_are_released_when_the_leader_is_cancelled() {
    let coalescer = coalescer();
    let leader = coalescer.clone();
    let task = tokio::spawn(async move {
        let _ = leader
            .run_once_until(
                "released-key",
                None,
                || async {
                    sleep(Duration::from_secs(60)).await;
                    Ok(MediaBytes::from(b"never".to_vec()))
                },
                |_| {},
            )
            .await;
    });
    sleep(Duration::from_millis(20)).await;

    let waiter_coalescer = coalescer.clone();
    let waiter = tokio::spawn(async move {
        waiter_coalescer
            .run_once_until(
                "released-key",
                Some(Instant::now() + Duration::from_secs(60)),
                || async { Ok(MediaBytes::from(b"should-not-run".to_vec())) },
                |_| {},
            )
            .await
    });
    sleep(Duration::from_millis(20)).await;
    task.abort();
    let _ = task.await;

    let released = tokio::time::timeout(Duration::from_secs(2), waiter).await;
    assert!(
        released.is_ok(),
        "waiter was not released when the leader was cancelled"
    );
    assert_eq!(
        Some(CoalescerError::WorkCancelled),
        released.unwrap().unwrap().err()
    );
}

#[tokio::test(flavor = "multi_thread", worker_threads = 8)]
async fn waiters_never_miss_a_completion_notification() {
    for round in 0..1000u32 {
        let coalescer = coalescer();
        let key = format!("race-{round}");
        let leader_key = key.clone();
        let leader = coalescer.clone();
        let lead = tokio::spawn(async move {
            let _ = leader
                .run_once_until(
                    leader_key,
                    None,
                    || async { Ok(MediaBytes::from(b"done".to_vec())) },
                    |_| {},
                )
                .await;
        });
        let mut waiters = Vec::new();
        for _ in 0..4 {
            let waiter_coalescer = coalescer.clone();
            let waiter_key = key.clone();
            waiters.push(tokio::spawn(async move {
                waiter_coalescer
                    .run_once_until(
                        waiter_key,
                        Some(Instant::now() + Duration::from_secs(10)),
                        || async { Ok(MediaBytes::from(b"waiter-ran-work".to_vec())) },
                        |_| {},
                    )
                    .await
            }));
        }
        let _ = lead.await;
        for waiter in waiters {
            assert!(
                !matches!(waiter.await, Ok(Err(CoalescerError::RequestTimeout))),
                "waiter missed the completion notification in round {round}"
            );
        }
    }
}

#[tokio::test]
async fn byte_coalescer_shares_one_result_with_waiters() {
    let metrics = Metrics::new();
    let coalescer = Arc::new(ByteCoalescer::with_budget(
        ByteBudget::new(1024),
        1,
        8,
        metrics.coalescer(),
    ));
    let published = Arc::new(Mutex::new(Vec::<u8>::new()));
    let (started_tx, started_rx) = tokio::sync::oneshot::channel();
    let (release_tx, release_rx) = tokio::sync::oneshot::channel();

    let leader = {
        let coalescer = Arc::clone(&coalescer);
        let published = Arc::clone(&published);
        tokio::spawn(async move {
            coalescer
                .run_once_until(
                    "same",
                    None,
                    move || async move {
                        started_tx.send(()).expect("leader start receiver");
                        release_rx.await.expect("leader release sender");
                        Ok(MediaBytes::from(vec![1, 2, 3]))
                    },
                    move |bytes| {
                        *published.lock() = bytes.as_ref().to_vec();
                    },
                )
                .await
        })
    };
    started_rx.await.expect("leader started");
    let waiter = {
        let coalescer = Arc::clone(&coalescer);
        let published = Arc::clone(&published);
        tokio::spawn(async move {
            coalescer
                .run_once_until(
                    "same",
                    None,
                    || async { Ok(MediaBytes::from(vec![7, 7, 7])) },
                    move |bytes| {
                        *published.lock() = bytes.as_ref().to_vec();
                    },
                )
                .await
        })
    };
    tokio::task::yield_now().await;
    release_tx.send(()).expect("leader release receiver");
    let leader_bytes = leader.await.expect("leader task").expect("leader result");
    let waiter_bytes = waiter.await.expect("waiter task").expect("waiter result");
    assert_eq!(leader_bytes.as_ref(), &[1, 2, 3]);
    assert_eq!(waiter_bytes.as_ref(), &[1, 2, 3]);
    assert_eq!(published.lock().as_slice(), &[1, 2, 3]);
    coalescer.begin_shutdown();
    coalescer.wait_for_shutdown().await;
}

#[tokio::test]
async fn byte_coalescer_enforces_capacity_budget_and_shutdown() {
    let metrics = Metrics::new();
    let coalescer = Arc::new(ByteCoalescer::with_budget(
        ByteBudget::new(0),
        1,
        8,
        metrics.coalescer(),
    ));
    assert_eq!(
        Err(CoalescerError::BufferBudgetExhausted),
        coalescer
            .run_once_until(
                "budget",
                None,
                || async { Ok(MediaBytes::from(vec![1])) },
                |_| {},
            )
            .await
            .map(|bytes| bytes.as_bytes().clone())
    );
    coalescer.begin_shutdown();
    assert_eq!(
        Err(CoalescerError::Unavailable),
        coalescer
            .run_once_until(
                "closed",
                None,
                || async { Ok(MediaBytes::from(vec![1])) },
                |_| {},
            )
            .await
            .map(|bytes| bytes.as_bytes().clone())
    );
    coalescer.wait_for_shutdown().await;
}

#[tokio::test]
async fn a_waiter_past_the_bound_is_rejected_as_overloaded() {
    let metrics = Metrics::new();
    let coalescer = Arc::new(ByteCoalescer::with_budget(
        ByteBudget::new(1024),
        1,
        1,
        metrics.coalescer(),
    ));
    let leader = coalescer.clone();
    let task = tokio::spawn(async move {
        leader
            .run_once_until(
                "bounded",
                None,
                || async {
                    sleep(Duration::from_millis(80)).await;
                    Ok(MediaBytes::from(vec![9]))
                },
                |_| {},
            )
            .await
    });
    sleep(Duration::from_millis(5)).await;

    let admitted = coalescer.clone();
    let waiter = tokio::spawn(async move {
        admitted
            .run_once_until(
                "bounded",
                None,
                || async { Ok(MediaBytes::from(vec![0])) },
                |_| {},
            )
            .await
    });
    sleep(Duration::from_millis(5)).await;

    assert_eq!(
        Err(CoalescerError::Overloaded),
        coalescer
            .run_once_until(
                "bounded",
                None,
                || async { Ok(MediaBytes::from(vec![0])) },
                |_| {},
            )
            .await
            .map(|bytes| bytes.as_bytes().clone())
    );
    assert_eq!(
        Err(CoalescerError::Overloaded),
        coalescer
            .run_once_until(
                "another-key",
                None,
                || async { Ok(MediaBytes::from(vec![0])) },
                |_| {},
            )
            .await
            .map(|bytes| bytes.as_bytes().clone())
    );
    assert_eq!(&[9], task.await.unwrap().unwrap().as_ref());
    assert_eq!(&[9], waiter.await.unwrap().unwrap().as_ref());
    assert!(
        metrics
            .render()
            .contains("fluxer_media_proxy_coalescer_waiter_rejected_total 1\n")
    );
}
