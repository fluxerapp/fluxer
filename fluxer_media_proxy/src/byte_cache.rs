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

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[must_use]
pub enum ByteCacheInsertOutcome {
    Inserted,
    Rejected(ByteCacheInsertRejection),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ByteCacheInsertRejection {
    CacheDisabled,
    EmptyValue,
    MaxEntryBytesExceeded,
    WeightExceedsCapacity,
    WeightExceedsU32,
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

    pub fn put(&self, key: impl Into<String>, value: impl Into<T>) -> ByteCacheInsertOutcome {
        let Some(storage) = self.storage.as_ref() else {
            return ByteCacheInsertOutcome::Rejected(ByteCacheInsertRejection::CacheDisabled);
        };
        let value = value.into();
        if value.bytes().is_empty() {
            return ByteCacheInsertOutcome::Rejected(ByteCacheInsertRejection::EmptyValue);
        }
        if value.bytes().resident_bytes() > storage.max_entry_bytes.get() {
            return ByteCacheInsertOutcome::Rejected(
                ByteCacheInsertRejection::MaxEntryBytesExceeded,
            );
        }
        let key = key.into();
        let Some(weight) = entry_weight(key.capacity(), &value) else {
            return ByteCacheInsertOutcome::Rejected(ByteCacheInsertRejection::WeightExceedsU32);
        };
        if u64::from(weight) > storage.capacity_bytes {
            return ByteCacheInsertOutcome::Rejected(
                ByteCacheInsertRejection::WeightExceedsCapacity,
            );
        }
        storage.inner.insert(key, value);
        ByteCacheInsertOutcome::Inserted
    }

    #[cfg(test)]
    pub(crate) fn settle(&self) {
        if let Some(storage) = self.storage.as_ref() {
            storage.inner.run_pending_tasks();
        }
    }

    #[cfg(test)]
    fn weighted_size(&self) -> u64 {
        self.storage
            .as_ref()
            .map(|storage| storage.inner.weighted_size())
            .unwrap_or(0)
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

#[cfg(test)]
mod tests {
    use super::*;
    use bytes::Bytes;
    use std::sync::{
        Arc,
        atomic::{AtomicU64, Ordering},
    };

    fn byte_cache(capacity_bytes: usize, max_entry_bytes: usize, ttl_ms: u64) -> ByteCache {
        ByteCache::new(
            ByteCacheSettings::clamped(capacity_bytes, max_entry_bytes, ttl_ms),
            || {},
        )
    }

    #[test]
    fn byte_cache_roundtrips_returning_shared_handles() {
        let cache = byte_cache(4096, 4096, 60_000);
        assert_eq!(
            ByteCacheInsertOutcome::Inserted,
            cache.put("a", Bytes::from_static(b"abc"))
        );
        let a1 = cache.get("a").unwrap();
        assert_eq!(b"abc", a1.as_ref());
        let a2 = cache.get("a").unwrap();
        assert_eq!(a1.as_ptr(), a2.as_ptr());
    }

    #[test]
    fn byte_cache_bounds_total_weighted_size() {
        let cache = byte_cache(4096, 4096, 60_000);
        for i in 0..50 {
            let _ = cache.put(format!("k{i}"), Bytes::from(vec![0u8; 4]));
        }
        cache.settle();
        assert!(cache.weighted_size() <= 4096);
    }

    #[test]
    fn byte_cache_skips_entries_over_max_entry_size() {
        let cache = byte_cache(4096, 4, 60_000);
        assert_eq!(
            ByteCacheInsertOutcome::Rejected(ByteCacheInsertRejection::MaxEntryBytesExceeded),
            cache.put("large", Bytes::from_static(b"12345"))
        );
        assert_eq!(
            None,
            cache.get("large").map(|value| value.as_bytes().clone())
        );
    }

    #[test]
    fn byte_cache_clamps_the_entry_ceiling_to_the_capacity() {
        let cache = byte_cache(4096, 8192, 60_000);
        assert_eq!(
            ByteCacheInsertOutcome::Rejected(ByteCacheInsertRejection::MaxEntryBytesExceeded),
            cache.put("over", Bytes::from(vec![7u8; 5000]))
        );
        assert_eq!(
            None,
            cache.get("over").map(|value| value.as_bytes().clone())
        );
        assert_eq!(
            ByteCacheInsertOutcome::Inserted,
            cache.put("fits", Bytes::from(vec![7u8; 3000]))
        );
        assert_eq!(
            Some(Bytes::from(vec![7u8; 3000])),
            cache.get("fits").map(|value| value.as_bytes().clone())
        );
    }

    #[test]
    fn byte_cache_disabled_when_capacity_or_ttl_is_zero() {
        let no_capacity = byte_cache(0, 4, 60_000);
        assert_eq!(
            ByteCacheInsertOutcome::Rejected(ByteCacheInsertRejection::CacheDisabled),
            no_capacity.put("a", Bytes::from_static(b"abc"))
        );
        assert_eq!(
            None,
            no_capacity.get("a").map(|value| value.as_bytes().clone())
        );

        let no_ttl = byte_cache(32, 4, 0);
        assert_eq!(
            ByteCacheInsertOutcome::Rejected(ByteCacheInsertRejection::CacheDisabled),
            no_ttl.put("a", Bytes::from_static(b"abc"))
        );
        assert_eq!(None, no_ttl.get("a").map(|value| value.as_bytes().clone()));

        let no_max_entry = byte_cache(4096, 0, 60_000);
        assert_eq!(
            ByteCacheInsertOutcome::Rejected(ByteCacheInsertRejection::CacheDisabled),
            no_max_entry.put("a", Bytes::from_static(b"abc"))
        );
    }

    #[test]
    fn byte_cache_settings_reject_every_invalid_shape() {
        assert_eq!(
            Err(ByteCacheSettingsError::CapacityIsZero),
            ByteCacheSettings::try_new(0, 1, 1)
        );
        assert_eq!(
            Err(ByteCacheSettingsError::MaxEntryBytesIsZero),
            ByteCacheSettings::try_new(16, 0, 1)
        );
        assert_eq!(
            Err(ByteCacheSettingsError::MaxEntryBytesExceedsCapacity),
            ByteCacheSettings::try_new(16, 17, 1)
        );
        assert_eq!(
            Err(ByteCacheSettingsError::TtlIsZero),
            ByteCacheSettings::try_new(16, 16, 0)
        );
        assert_eq!(
            ByteCacheSettings::disabled(),
            ByteCacheSettings::clamped(16, 17, 0)
        );
        assert_eq!(
            ByteCacheSettings::try_new(16, 16, 1),
            Ok(ByteCacheSettings::clamped(16, 17, 1))
        );
    }

    #[test]
    fn byte_cache_rejects_an_empty_value_and_a_weight_over_the_capacity() {
        let cache = byte_cache(2_048, 1_024, 60_000);
        assert_eq!(
            ByteCacheInsertOutcome::Rejected(ByteCacheInsertRejection::EmptyValue),
            cache.put("empty", Bytes::new())
        );
        assert_eq!(
            ByteCacheInsertOutcome::Rejected(ByteCacheInsertRejection::MaxEntryBytesExceeded),
            cache.put("large", Bytes::from(vec![0; 1_025]))
        );
        assert_eq!(
            ByteCacheInsertOutcome::Inserted,
            cache.put("0123456789abcdef", Bytes::from_static(b"0123456789abcdef"))
        );
        assert_eq!(
            b"0123456789abcdef",
            cache
                .get("0123456789abcdef")
                .expect("cached bytes")
                .as_ref()
        );

        let tight = byte_cache(1_024, 1_024, 60_000);
        assert_eq!(
            ByteCacheInsertOutcome::Rejected(ByteCacheInsertRejection::WeightExceedsCapacity),
            tight.put("key", Bytes::from(vec![0; 1_024]))
        );
    }

    #[test]
    fn byte_cache_reports_evictions_through_the_hook() {
        let evictions = Arc::new(AtomicU64::new(0));
        let recorded = Arc::clone(&evictions);
        let cache: ByteCache = ByteCache::new(
            ByteCacheSettings::clamped(8_192, 8_192, 60_000),
            move || {
                recorded.fetch_add(1, Ordering::Relaxed);
            },
        );
        for index in 0..64 {
            let _ = cache.put(format!("entry-{index}"), Bytes::from(vec![0u8; 512]));
        }
        cache.settle();
        assert!(cache.weighted_size() <= 8_192);
        assert!(evictions.load(Ordering::Relaxed) > 0);
    }
}
