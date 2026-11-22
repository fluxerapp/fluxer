// SPDX-License-Identifier: AGPL-3.0-or-later

use std::ops::{Deref, DerefMut};
#[cfg(feature = "fault")]
use std::sync::atomic::AtomicU64;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{Arc, OnceLock, RwLock, RwLockReadGuard, RwLockWriteGuard};

use crate::shared::PermsError;

pub(crate) const MAX_SHARDS: usize = 64;

static NEXT_SLOT: AtomicUsize = AtomicUsize::new(0);
static SHARDS: OnceLock<usize> = OnceLock::new();

thread_local! {
    static SLOT: usize = NEXT_SLOT.fetch_add(1, Ordering::Relaxed);
}

pub(crate) fn shard_count() -> usize {
    *SHARDS.get_or_init(|| {
        std::thread::available_parallelism()
            .map_or(1, std::num::NonZeroUsize::get)
            .next_power_of_two()
            .clamp(1, MAX_SHARDS)
    })
}

#[derive(Debug)]
#[repr(align(128))]
struct Shard<T>(RwLock<Arc<T>>);

#[derive(Debug)]
pub(crate) struct Sharded<T> {
    shards: Box<[Shard<T>]>,
    spare: Arc<T>,
    writing: AtomicBool,
    gate: RwLock<()>,
    #[cfg(feature = "fault")]
    stats: LockStats,
}

#[cfg(feature = "fault")]
#[derive(Debug, Default)]
struct LockStats {
    clones: AtomicU64,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub(crate) enum Wait {
    Block,
    Try,
}

impl Wait {
    pub(crate) fn of(dirty: bool) -> Wait {
        if dirty { Wait::Block } else { Wait::Try }
    }
}

fn taken<G>(found: std::sync::TryLockResult<G>) -> Result<G, PermsError> {
    match found {
        Ok(guard) => Ok(guard),
        Err(std::sync::TryLockError::WouldBlock) => Err(PermsError::NeedsDirty),
        Err(std::sync::TryLockError::Poisoned(_)) => Err(PermsError::Broken),
    }
}

pub(crate) struct ShardRead<'a, T>(RwLockReadGuard<'a, Arc<T>>);

impl<T> Deref for ShardRead<'_, T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.0
    }
}

pub(crate) struct ShardLock<'a, T> {
    guards: Vec<RwLockWriteGuard<'a, Arc<T>>>,
    current: Arc<T>,
    writing: &'a AtomicBool,
    _gate: RwLockWriteGuard<'a, ()>,
}

impl<T> Deref for ShardLock<'_, T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.current
    }
}

impl<T> ShardLock<'_, T> {
    pub(crate) fn replace(&mut self, value: T) -> Arc<T> {
        self.install(Arc::new(value))
    }

    fn install(&mut self, value: Arc<T>) -> Arc<T> {
        for guard in &mut self.guards {
            **guard = Arc::clone(&value);
        }
        std::mem::replace(&mut self.current, value)
    }
}

impl<T> Drop for ShardLock<'_, T> {
    fn drop(&mut self) {
        self.writing.store(false, Ordering::Release);
    }
}

pub(crate) struct ShardWrite<'a, T: Default> {
    lock: ShardLock<'a, T>,
    value: T,
}

impl<T: Default> Deref for ShardWrite<'_, T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.value
    }
}

impl<T: Default> DerefMut for ShardWrite<'_, T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.value
    }
}

impl<T: Default> Drop for ShardWrite<'_, T> {
    fn drop(&mut self) {
        let value = Arc::new(std::mem::take(&mut self.value));
        self.lock.install(value);
    }
}

impl<T: Default + Clone> Sharded<T> {
    pub(crate) fn new(value: T) -> Sharded<T> {
        let value = Arc::new(value);
        Sharded {
            shards: (0..shard_count())
                .map(|_| Shard(RwLock::new(Arc::clone(&value))))
                .collect(),
            spare: Arc::new(T::default()),
            writing: AtomicBool::new(false),
            gate: RwLock::new(()),
            #[cfg(feature = "fault")]
            stats: LockStats::default(),
        }
    }

    pub(crate) fn read(&self) -> Result<ShardRead<'_, T>, PermsError> {
        if self.writing.load(Ordering::Acquire) {
            drop(self.gate.read().map_err(|_| PermsError::Broken)?);
        }
        let slot = SLOT.try_with(|slot| *slot).unwrap_or(0);
        let index = slot.checked_rem(self.shards.len()).unwrap_or(0);
        let shard = self.shards.get(index).ok_or(PermsError::Broken)?;
        shard
            .0
            .read()
            .map(ShardRead)
            .map_err(|_| PermsError::Broken)
    }

    pub(crate) fn try_read(&self) -> Result<ShardRead<'_, T>, PermsError> {
        if self.writing.load(Ordering::Acquire) {
            return Err(PermsError::NeedsDirty);
        }
        let slot = SLOT.try_with(|slot| *slot).unwrap_or(0);
        let index = slot.checked_rem(self.shards.len()).unwrap_or(0);
        let shard = self.shards.get(index).ok_or(PermsError::Broken)?;
        match shard.0.try_read() {
            Ok(guard) => Ok(ShardRead(guard)),
            Err(std::sync::TryLockError::WouldBlock) => Err(PermsError::NeedsDirty),
            Err(std::sync::TryLockError::Poisoned(_)) => Err(PermsError::Broken),
        }
    }

    pub(crate) fn lock_with(&self, wait: Wait) -> Result<ShardLock<'_, T>, PermsError> {
        let gate = match wait {
            Wait::Block => self.gate.write().map_err(|_| PermsError::Broken)?,
            Wait::Try => taken(self.gate.try_write())?,
        };
        self.writing.store(true, Ordering::Release);
        let mut lock = ShardLock {
            guards: Vec::with_capacity(self.shards.len()),
            current: Arc::clone(&self.spare),
            writing: &self.writing,
            _gate: gate,
        };
        for shard in &self.shards {
            let guard = match wait {
                Wait::Block => shard.0.write().map_err(|_| PermsError::Broken)?,
                Wait::Try => taken(shard.0.try_write())?,
            };
            lock.guards.push(guard);
        }
        let current = lock.guards.first().map(|guard| Arc::clone(guard));
        lock.current = current.ok_or(PermsError::Broken)?;
        Ok(lock)
    }

    pub(crate) fn write(&self) -> Result<ShardWrite<'_, T>, PermsError> {
        self.write_with(Wait::Block)
    }

    pub(crate) fn write_with(&self, wait: Wait) -> Result<ShardWrite<'_, T>, PermsError> {
        let mut lock = self.lock_with(wait)?;
        let owned = lock.install(Arc::clone(&self.spare));
        let value = Arc::try_unwrap(owned).unwrap_or_else(|arc| {
            #[cfg(feature = "fault")]
            self.stats.clones.fetch_add(1, Ordering::Relaxed);
            (*arc).clone()
        });
        Ok(ShardWrite { lock, value })
    }

    #[cfg(feature = "fault")]
    pub(crate) fn clones_under_lock(&self) -> u64 {
        self.stats.clones.load(Ordering::Relaxed)
    }
}
