// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::byte_budget::BudgetedBytes;
use moka::{policy::EvictionPolicy, sync::Cache as MokaCache};
use std::{mem::size_of, num::NonZeroUsize, time::Duration};
use thiserror::Error;

const BYTE_CACHE_ENTRY_OVERHEAD_BYTES: usize = 512;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ByteCacheSettings {
    limits: Option<ByteCacheLimits>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct ByteCacheLimits {
    capacity_bytes: u64,
    max_entry_bytes: NonZeroUsize,
    ttl: Duration,
}

#[derive(Clone, Copy, Debug, Eq, Error, PartialEq)]
pub enum ByteCacheSettingsError {
    #[error("byte cache capacity must be positive")]
    CapacityIsZero,
    #[error("byte cache capacity does not fit in u64")]
    CapacityExceedsU64,
    #[error("byte cache maximum entry size must be positive")]
    MaxEntryBytesIsZero,
    #[error("byte cache maximum entry size does not fit in u64")]
    MaxEntryBytesExceedsU64,
    #[error("byte cache maximum entry size exceeds its capacity")]
    MaxEntryBytesExceedsCapacity,
    #[error("byte cache TTL must be positive")]
    TtlIsZero,
}

impl ByteCacheSettings {
    pub const fn disabled() -> Self {
        Self { limits: None }
    }

    pub fn try_new(
        capacity_bytes: usize,
        max_entry_bytes: usize,
        ttl_ms: u64,
    ) -> Result<Self, ByteCacheSettingsError> {
        let capacity_bytes = u64::try_from(capacity_bytes)
            .map_err(|_| ByteCacheSettingsError::CapacityExceedsU64)?;
        if capacity_bytes == 0 {
            return Err(ByteCacheSettingsError::CapacityIsZero);
        }
        let max_entry_bytes = NonZeroUsize::new(max_entry_bytes)
            .ok_or(ByteCacheSettingsError::MaxEntryBytesIsZero)?;
        if u64::try_from(max_entry_bytes.get())
            .map_err(|_| ByteCacheSettingsError::MaxEntryBytesExceedsU64)?
            > capacity_bytes
        {
            return Err(ByteCacheSettingsError::MaxEntryBytesExceedsCapacity);
        }
        if ttl_ms == 0 {
            return Err(ByteCacheSettingsError::TtlIsZero);
        }
        Ok(Self {
            limits: Some(ByteCacheLimits {
                capacity_bytes,
                max_entry_bytes,
                ttl: Duration::from_millis(ttl_ms),
            }),
        })
    }

    pub fn clamped(capacity_bytes: usize, max_entry_bytes: usize, ttl_ms: u64) -> Self {
        Self::try_new(capacity_bytes, max_entry_bytes.min(capacity_bytes), ttl_ms)
            .unwrap_or_else(|_| Self::disabled())
    }
}

pub trait ByteCacheValue: Clone + Send + Sync + 'static {
    fn bytes(&self) -> &BudgetedBytes;
}

impl ByteCacheValue for BudgetedBytes {
    fn bytes(&self) -> &BudgetedBytes {
        self
    }
}

#[derive(Debug)]
pub struct ByteCache<T: ByteCacheValue = BudgetedBytes> {
    storage: Option<ByteCacheStorage<T>>,
}

#[derive(Debug)]
struct ByteCacheStorage<T: ByteCacheValue> {
    capacity_bytes: u64,
    max_entry_bytes: NonZeroUsize,
    inner: MokaCache<String, T>,
}

impl<T> ByteCache<T>
where
    T: ByteCacheValue,
{
    pub fn new(
        settings: ByteCacheSettings,
        on_eviction: impl Fn() + Send + Sync + 'static,
    ) -> Self {
        let Some(limits) = settings.limits else {
            return Self { storage: None };
        };
        let inner = MokaCache::builder()
            .max_capacity(limits.capacity_bytes)
            .eviction_policy(EvictionPolicy::tiny_lfu())
            .weigher(|key: &String, value: &T| {
                entry_weight(key.capacity(), value)
                    .expect("byte cache insertion validates that entry weight fits u32")
            })
            .eviction_listener(move |_key, _value, cause| {
                if cause.was_evicted() {
                    on_eviction();
                }
            })
            .time_to_live(limits.ttl)
            .build();
        Self {
            storage: Some(ByteCacheStorage {
                capacity_bytes: limits.capacity_bytes,
                max_entry_bytes: limits.max_entry_bytes,
                inner,
            }),
        }
    }

    pub fn get(&self, key: &str) -> Option<T> {
        self.storage.as_ref()?.inner.get(key)
    }

    #[must_use]
    pub fn put(&self, key: impl Into<String>, value: impl Into<T>) -> bool {
        let Some(storage) = self.storage.as_ref() else {
            return false;
        };
        let value = value.into();
        if value.bytes().is_empty() {
            return false;
        }
        if value.bytes().resident_bytes() > storage.max_entry_bytes.get() {
            return false;
        }
        let key = key.into();
        let Some(weight) = entry_weight(key.capacity(), &value) else {
            return false;
        };
        if u64::from(weight) > storage.capacity_bytes {
            return false;
        }
        storage.inner.insert(key, value);
        true
    }
}

fn entry_weight<T: ByteCacheValue>(key_bytes: usize, value: &T) -> Option<u32> {
    BYTE_CACHE_ENTRY_OVERHEAD_BYTES
        .checked_add(size_of::<String>())
        .and_then(|weight| weight.checked_add(size_of::<T>()))
        .and_then(|weight| weight.checked_add(key_bytes))
        .and_then(|weight| weight.checked_add(value.bytes().resident_bytes()))
        .and_then(|weight| u32::try_from(weight).ok())
}
