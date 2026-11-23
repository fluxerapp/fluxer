// SPDX-License-Identifier: AGPL-3.0-or-later

use std::sync::{Mutex, MutexGuard, TryLockError};

use guild_rt::Scheduler;

use rustler::Resource;

use crate::stream::{Compressor, Decompressor};

pub struct CompressorResource {
    inner: Mutex<Compressor>,
}

pub struct DecompressorResource {
    inner: Mutex<Decompressor>,
}

pub(crate) enum Locked<'a, T> {
    Held(MutexGuard<'a, T>),
    Busy,
    Poisoned,
}

fn locked<T>(inner: &Mutex<T>, on: Scheduler) -> Locked<'_, T> {
    let found = match on {
        Scheduler::Normal => inner.try_lock(),
        Scheduler::Dirty => inner.lock().map_err(TryLockError::from),
    };
    match found {
        Ok(guard) => Locked::Held(guard),
        Err(TryLockError::WouldBlock) => Locked::Busy,
        Err(TryLockError::Poisoned(_)) => Locked::Poisoned,
    }
}

impl Resource for CompressorResource {}

impl Resource for DecompressorResource {}

impl CompressorResource {
    pub fn new(compressor: Compressor) -> CompressorResource {
        CompressorResource {
            inner: Mutex::new(compressor),
        }
    }

    pub(crate) fn lock(&self, on: Scheduler) -> Locked<'_, Compressor> {
        locked(&self.inner, on)
    }
}

impl DecompressorResource {
    pub fn new(decompressor: Decompressor) -> DecompressorResource {
        DecompressorResource {
            inner: Mutex::new(decompressor),
        }
    }

    pub(crate) fn lock(&self, on: Scheduler) -> Locked<'_, Decompressor> {
        locked(&self.inner, on)
    }
}
