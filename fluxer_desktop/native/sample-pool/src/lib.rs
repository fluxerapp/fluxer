// SPDX-License-Identifier: AGPL-3.0-or-later

use std::cell::UnsafeCell;
use std::fmt;
use std::sync::atomic::{AtomicU32, AtomicU64, Ordering};
use std::sync::{Arc, Mutex, MutexGuard};

pub const PCM_SLOT_SAMPLES_MAX: usize = 16_384;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PcmPoolError {
    ZeroCapacity,
    ZeroSamplesPerSlot,
    SamplesPerSlotTooLarge(usize),
    PayloadTooLarge { offered: usize, capacity: usize },
}

impl fmt::Display for PcmPoolError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ZeroCapacity => write!(f, "SamplePool capacity must be > 0"),
            Self::ZeroSamplesPerSlot => write!(f, "SamplePool samples_per_slot must be > 0"),
            Self::SamplesPerSlotTooLarge(n) => write!(
                f,
                "SamplePool samples_per_slot {n} exceeds PCM_SLOT_SAMPLES_MAX={PCM_SLOT_SAMPLES_MAX}"
            ),
            Self::PayloadTooLarge { offered, capacity } => write!(
                f,
                "SamplePool payload {offered} samples exceeds slot capacity {capacity}"
            ),
        }
    }
}

impl std::error::Error for PcmPoolError {}

struct SlotCell<T> {
    inner: UnsafeCell<Box<[T]>>,
}

unsafe impl<T: Send> Send for SlotCell<T> {}
unsafe impl<T: Send + Sync> Sync for SlotCell<T> {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PcmPoolStats {
    pub acquired: u64,
    pub released: u64,
    pub dropped: u64,
    pub in_flight: u32,
}

struct SamplePoolInner<T> {
    slots: Vec<SlotCell<T>>,
    free: Mutex<Vec<usize>>,
    capacity: u32,
    samples_per_slot: u32,
    acquired_total: AtomicU64,
    released_total: AtomicU64,
    dropped_total: AtomicU64,
    in_flight: AtomicU32,
}

fn lock_free_list(free: &Mutex<Vec<usize>>) -> MutexGuard<'_, Vec<usize>> {
    free.lock().unwrap_or_else(|poisoned| poisoned.into_inner())
}

macro_rules! pool_assert {
    ($condition:expr) => {
        if CHECK_INVARIANTS {
            assert!($condition);
        } else {
            debug_assert!($condition);
        }
    };
}

macro_rules! pool_assert_eq {
    ($left:expr, $right:expr) => {
        if CHECK_INVARIANTS {
            assert_eq!($left, $right);
        } else {
            debug_assert_eq!($left, $right);
        }
    };
}

pub struct SamplePool<T: Copy + Default + Send + Sync, const CHECK_INVARIANTS: bool = true> {
    inner: Arc<SamplePoolInner<T>>,
}

impl<T: Copy + Default + Send + Sync, const CHECK_INVARIANTS: bool> fmt::Debug
    for SamplePool<T, CHECK_INVARIANTS>
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let stats = self.stats();
        f.debug_struct("SamplePool")
            .field("capacity", &self.inner.capacity)
            .field("samples_per_slot", &self.inner.samples_per_slot)
            .field("stats", &stats)
            .finish()
    }
}

impl<T: Copy + Default + Send + Sync, const CHECK_INVARIANTS: bool>
    SamplePool<T, CHECK_INVARIANTS>
{
    pub fn new(capacity: usize, samples_per_slot: usize) -> Result<Self, PcmPoolError> {
        if capacity == 0 {
            return Err(PcmPoolError::ZeroCapacity);
        }
        if samples_per_slot == 0 {
            return Err(PcmPoolError::ZeroSamplesPerSlot);
        }
        if samples_per_slot > PCM_SLOT_SAMPLES_MAX {
            return Err(PcmPoolError::SamplesPerSlotTooLarge(samples_per_slot));
        }
        assert!(capacity > 0);
        assert!(samples_per_slot > 0);
        assert!(samples_per_slot <= PCM_SLOT_SAMPLES_MAX);

        let mut slots: Vec<SlotCell<T>> = Vec::with_capacity(capacity);
        for _ in 0..capacity {
            let buf: Box<[T]> = vec![T::default(); samples_per_slot].into_boxed_slice();
            assert_eq!(buf.len(), samples_per_slot);
            slots.push(SlotCell {
                inner: UnsafeCell::new(buf),
            });
        }
        assert_eq!(slots.len(), capacity);

        let mut free: Vec<usize> = Vec::with_capacity(capacity);
        for index in 0..capacity {
            free.push(index);
        }
        assert_eq!(free.len(), capacity);

        let cap_u32 = u32::try_from(capacity).map_err(|_| PcmPoolError::ZeroCapacity)?;
        let sps_u32 =
            u32::try_from(samples_per_slot).map_err(|_| PcmPoolError::ZeroSamplesPerSlot)?;
        let inner = SamplePoolInner {
            slots,
            free: Mutex::new(free),
            capacity: cap_u32,
            samples_per_slot: sps_u32,
            acquired_total: AtomicU64::new(0),
            released_total: AtomicU64::new(0),
            dropped_total: AtomicU64::new(0),
            in_flight: AtomicU32::new(0),
        };
        Ok(Self {
            inner: Arc::new(inner),
        })
    }

    pub fn try_acquire(&self) -> Option<PooledSamples<T, CHECK_INVARIANTS>> {
        assert!(self.inner.capacity > 0);
        assert!(self.inner.samples_per_slot > 0);

        let mut free = lock_free_list(&self.inner.free);
        assert!(free.len() <= self.inner.capacity as usize);
        let index = match free.pop() {
            Some(idx) => idx,
            None => {
                drop(free);
                self.inner.dropped_total.fetch_add(1, Ordering::Relaxed);
                return None;
            }
        };
        assert!(index < self.inner.capacity as usize);
        self.inner.acquired_total.fetch_add(1, Ordering::Relaxed);
        let after = self.inner.in_flight.fetch_add(1, Ordering::AcqRel) + 1;
        assert!(after <= self.inner.capacity);
        drop(free);

        Some(PooledSamples {
            slot_index: index,
            filled_len: 0,
            pool: Arc::clone(&self.inner),
        })
    }

    pub fn capacity(&self) -> u32 {
        let cap = self.inner.capacity;
        assert!(cap > 0);
        assert!(cap as usize == self.inner.slots.len());
        cap
    }

    pub fn samples_per_slot(&self) -> u32 {
        let sps = self.inner.samples_per_slot;
        assert!(sps > 0);
        assert!(sps as usize <= PCM_SLOT_SAMPLES_MAX);
        sps
    }

    pub fn stats(&self) -> PcmPoolStats {
        assert!(self.inner.capacity > 0);
        let in_flight = self.inner.in_flight.load(Ordering::Acquire);
        assert!(in_flight <= self.inner.capacity);
        let acquired = self.inner.acquired_total.load(Ordering::Relaxed);
        let released = self.inner.released_total.load(Ordering::Relaxed);
        let dropped = self.inner.dropped_total.load(Ordering::Relaxed);
        assert!(released <= acquired);
        PcmPoolStats {
            acquired,
            released,
            dropped,
            in_flight,
        }
    }
}

impl<T: Copy + Default + Send + Sync, const CHECK_INVARIANTS: bool> Clone
    for SamplePool<T, CHECK_INVARIANTS>
{
    fn clone(&self) -> Self {
        Self {
            inner: Arc::clone(&self.inner),
        }
    }
}

pub struct PooledSamples<T: Copy + Default + Send + Sync, const CHECK_INVARIANTS: bool = true> {
    slot_index: usize,
    filled_len: usize,
    pool: Arc<SamplePoolInner<T>>,
}

impl<T: Copy + Default + Send + Sync, const CHECK_INVARIANTS: bool>
    PooledSamples<T, CHECK_INVARIANTS>
{
    pub fn write(&mut self, samples: &[T]) -> Result<(), PcmPoolError> {
        pool_assert!(self.slot_index < self.pool.capacity as usize);
        let cap = self.pool.samples_per_slot as usize;
        if samples.len() > cap {
            return Err(PcmPoolError::PayloadTooLarge {
                offered: samples.len(),
                capacity: cap,
            });
        }
        pool_assert!(samples.len() <= cap);

        let cell = &self.pool.slots[self.slot_index];
        let buf: &mut [T] = unsafe { &mut *cell.inner.get() };
        pool_assert_eq!(buf.len(), cap);
        if !samples.is_empty() {
            buf[..samples.len()].copy_from_slice(samples);
        }
        self.filled_len = samples.len();
        pool_assert!(self.filled_len <= cap);
        Ok(())
    }

    pub fn write_with<F>(&mut self, sample_count: usize, fill: F) -> Result<(), PcmPoolError>
    where
        F: FnOnce(&mut [T]),
    {
        pool_assert!(self.slot_index < self.pool.capacity as usize);
        let cap = self.pool.samples_per_slot as usize;
        if sample_count > cap {
            return Err(PcmPoolError::PayloadTooLarge {
                offered: sample_count,
                capacity: cap,
            });
        }
        pool_assert!(sample_count <= cap);

        let cell = &self.pool.slots[self.slot_index];
        let buf: &mut [T] = unsafe { &mut *cell.inner.get() };
        pool_assert_eq!(buf.len(), cap);
        fill(&mut buf[..sample_count]);
        self.filled_len = sample_count;
        pool_assert!(self.filled_len <= cap);
        Ok(())
    }

    pub fn unfilled_mut(&mut self) -> &mut [T] {
        pool_assert!(self.slot_index < self.pool.capacity as usize);
        let cap = self.pool.samples_per_slot as usize;
        pool_assert!(cap > 0);
        let cell = &self.pool.slots[self.slot_index];
        let buf: &mut [T] = unsafe { &mut *cell.inner.get() };
        pool_assert_eq!(buf.len(), cap);
        buf
    }

    pub fn set_filled_len(&mut self, len: usize) {
        assert!(self.slot_index < self.pool.capacity as usize);
        assert!(len <= self.pool.samples_per_slot as usize);
        self.filled_len = len;
    }

    pub fn filled_len(&self) -> usize {
        pool_assert!(self.slot_index < self.pool.capacity as usize);
        pool_assert!(self.filled_len <= self.pool.samples_per_slot as usize);
        self.filled_len
    }

    pub fn capacity(&self) -> usize {
        let cap = self.pool.samples_per_slot as usize;
        pool_assert!(cap > 0);
        pool_assert!(cap <= PCM_SLOT_SAMPLES_MAX);
        cap
    }

    pub fn as_mut_ptr(&mut self) -> *mut T {
        pool_assert!(self.slot_index < self.pool.capacity as usize);
        pool_assert!(self.filled_len <= self.pool.samples_per_slot as usize);
        let cell = &self.pool.slots[self.slot_index];
        let buf: &mut [T] = unsafe { &mut *cell.inner.get() };
        pool_assert_eq!(buf.len(), self.pool.samples_per_slot as usize);
        buf.as_mut_ptr()
    }

    pub fn into_external_parts(mut self) -> (*mut T, usize, Self) {
        pool_assert!(self.slot_index < self.pool.capacity as usize);
        pool_assert!(self.filled_len <= self.pool.samples_per_slot as usize);
        let len = self.filled_len;
        let ptr = self.as_mut_ptr();
        pool_assert!(!ptr.is_null());
        (ptr, len, self)
    }
}

impl<T: Copy + Default + Send + Sync, const CHECK_INVARIANTS: bool> Drop
    for PooledSamples<T, CHECK_INVARIANTS>
{
    fn drop(&mut self) {
        pool_assert!(self.slot_index < self.pool.capacity as usize);
        if self.slot_index >= self.pool.capacity as usize {
            return;
        }
        let mut free = lock_free_list(&self.pool.free);
        pool_assert!(free.len() < self.pool.capacity as usize);
        free.push(self.slot_index);
        let before = self.pool.in_flight.fetch_sub(1, Ordering::AcqRel);
        pool_assert!(before >= 1);
        self.pool.released_total.fetch_add(1, Ordering::Relaxed);
        drop(free);
    }
}
