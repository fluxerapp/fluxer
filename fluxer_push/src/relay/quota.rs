// SPDX-License-Identifier: AGPL-3.0-or-later

use super::reject::{Reason, Rejection};
use crate::config::BucketConfig;
use crate::metrics::{BucketKey, Metrics};
use sha2::{Digest as _, Sha256};
use std::collections::HashMap;
use std::net::IpAddr;
use std::sync::Mutex;
use std::time::Instant;
use tokio::sync::{Semaphore, SemaphorePermit};

const BUSY_RETRY_AFTER_SECONDS: i64 = 1;
const RATE_LIMIT_RETRY_AFTER_SECONDS: i64 = 1;
const SECONDS_PER_MINUTE: f64 = 60.0;
const KEY_BYTES: usize = 16;
const IPV4_PREFIX_BYTES: usize = 3;
const IPV6_PREFIX_BYTES: usize = 8;

type Key = [u8; KEY_BYTES];

pub struct Quota {
    admissions: Semaphore,
    device_tokens: Buckets,
    sources: Option<Buckets>,
}

impl Quota {
    pub fn new(
        max_concurrent: usize,
        device_tokens: &BucketConfig,
        sources: Option<&BucketConfig>,
    ) -> Self {
        Self {
            admissions: Semaphore::new(max_concurrent),
            device_tokens: Buckets::new(device_tokens),
            sources: sources.map(Buckets::new),
        }
    }

    pub fn admit(&self) -> Result<SemaphorePermit<'_>, Rejection> {
        self.admissions
            .try_acquire()
            .map_err(|_| Rejection::after(Reason::RelayUnavailable, BUSY_RETRY_AFTER_SECONDS))
    }

    pub fn take(
        &self,
        metrics: &Metrics,
        device_token: &str,
        client_ip: IpAddr,
        now: Instant,
    ) -> Result<(), Rejection> {
        self.check(
            metrics,
            BucketKey::DeviceToken,
            &self.device_tokens,
            device_token_key(device_token),
            now,
        )?;
        let Some(sources) = self.sources.as_ref() else {
            return Ok(());
        };
        self.check(
            metrics,
            BucketKey::Source,
            sources,
            source_key(client_ip),
            now,
        )
    }

    fn check(
        &self,
        metrics: &Metrics,
        which: BucketKey,
        buckets: &Buckets,
        key: Key,
        now: Instant,
    ) -> Result<(), Rejection> {
        if buckets.take(key, now) {
            return Ok(());
        }
        metrics.record_bucket_drop(which);
        Err(Rejection::after(
            Reason::RateLimited,
            RATE_LIMIT_RETRY_AFTER_SECONDS,
        ))
    }
}

pub fn device_token_key(device_token: &str) -> Key {
    let digest = Sha256::digest(device_token.as_bytes());
    let mut key = [0u8; KEY_BYTES];
    key.copy_from_slice(&digest[..KEY_BYTES]);
    key
}

pub fn source_key(ip: IpAddr) -> Key {
    let mut key = [0u8; KEY_BYTES];
    match ip.to_canonical() {
        IpAddr::V4(ip) => {
            key[..IPV4_PREFIX_BYTES].copy_from_slice(&ip.octets()[..IPV4_PREFIX_BYTES])
        }
        IpAddr::V6(ip) => {
            key[..IPV6_PREFIX_BYTES].copy_from_slice(&ip.octets()[..IPV6_PREFIX_BYTES])
        }
    }
    key
}

struct Bucket {
    tokens: f64,
    refilled_at: Instant,
}

struct Held {
    live: HashMap<Key, Bucket>,
    aged: HashMap<Key, Bucket>,
}

pub struct Buckets {
    refill_per_second: f64,
    burst: f64,
    entries: usize,
    held: Mutex<Held>,
}

impl Buckets {
    pub fn new(cfg: &BucketConfig) -> Self {
        Self {
            refill_per_second: f64::from(cfg.per_minute) / SECONDS_PER_MINUTE,
            burst: f64::from(cfg.burst),
            entries: cfg.entries,
            held: Mutex::new(Held {
                live: HashMap::new(),
                aged: HashMap::new(),
            }),
        }
    }

    pub fn take(&self, key: Key, now: Instant) -> bool {
        let mut held = self
            .held
            .lock()
            .expect("the relay quota lock is not poisoned");
        let mut bucket = held
            .live
            .remove(&key)
            .or_else(|| held.aged.remove(&key))
            .unwrap_or(Bucket {
                tokens: self.burst,
                refilled_at: now,
            });

        let elapsed = now
            .saturating_duration_since(bucket.refilled_at)
            .as_secs_f64();
        bucket.refilled_at = now;
        bucket.tokens = (bucket.tokens + elapsed * self.refill_per_second).min(self.burst);
        let allowed = bucket.tokens >= 1.0;
        if allowed {
            bucket.tokens -= 1.0;
        }

        if held.live.len() >= self.entries {
            held.aged = std::mem::take(&mut held.live);
        }
        held.live.insert(key, bucket);
        allowed
    }
}
