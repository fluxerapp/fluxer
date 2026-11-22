// SPDX-License-Identifier: AGPL-3.0-or-later

use std::sync::atomic::{AtomicU8, AtomicU64, Ordering};
use std::sync::{Arc, Mutex, MutexGuard, PoisonError, TryLockError};

use guild_rt::{ResourceKind, Sink};
use ml_core::{Accounting, Core, CoreError, Payload};
use perm_core::PermsError;
use rustler::{Env, LocalPid, Monitor, Resource, ResourceArc};

pub const IN_PLACE_MEMBERS: u32 = 10_000;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Subscriber(pub u64);

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Failure {
    NeedsDirty,
    Broken,
    Gone,
    BadInput(String),
}

impl From<CoreError> for Failure {
    fn from(error: CoreError) -> Failure {
        match error {
            CoreError::NeedsDirty => Failure::NeedsDirty,
            CoreError::BadInput(path) => Failure::BadInput(path),
            CoreError::Perms(PermsError::NeedsDirty) => Failure::NeedsDirty,
            CoreError::Perms(PermsError::BadInput(path)) => Failure::BadInput(path),
            CoreError::Perms(PermsError::Gone) => Failure::Gone,
            CoreError::Perms(_) => Failure::Broken,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Status {
    Ready,
    Broken,
    Released,
}

impl Status {
    const fn code(self) -> u8 {
        match self {
            Status::Ready => 0,
            Status::Broken => 1,
            Status::Released => 2,
        }
    }

    const fn from_code(code: u8) -> Status {
        match code {
            0 => Status::Ready,
            1 => Status::Broken,
            _ => Status::Released,
        }
    }
}

pub struct Counts {
    packed: AtomicU64,
}

impl Counts {
    pub fn new() -> Counts {
        Counts {
            packed: AtomicU64::new(0),
        }
    }

    pub fn read(&self) -> (u32, u32) {
        let packed = self.packed.load(Ordering::Acquire);
        let total = u32::try_from(packed.checked_shr(32).unwrap_or(0)).unwrap_or(u32::MAX);
        let online = u32::try_from(packed & u64::from(u32::MAX)).unwrap_or(u32::MAX);
        (total, online)
    }

    pub fn store(&self, total: u32, online: u32) {
        let packed = u64::from(total).checked_shl(32).unwrap_or(0) | u64::from(online);
        self.packed.store(packed, Ordering::Release);
    }
}

impl Default for Counts {
    fn default() -> Counts {
        Counts::new()
    }
}

impl Resource for Counts {}

pub struct PayloadRes(Payload);

impl PayloadRes {
    pub fn new(payload: Payload) -> PayloadRes {
        PayloadRes(payload)
    }

    pub fn vm_bytes(&self) -> &[u8] {
        self.0.guarded().bytes
    }
}

impl Resource for PayloadRes {}

type Held = Option<Core<Subscriber>>;

struct Bytes {
    core: AtomicU64,
    fragments: AtomicU64,
    payloads: AtomicU64,
}

impl Bytes {
    const fn new() -> Bytes {
        Bytes {
            core: AtomicU64::new(0),
            fragments: AtomicU64::new(0),
            payloads: AtomicU64::new(0),
        }
    }

    fn report(&self, now: Accounting) {
        let parts = [
            (&self.core, now.core, ResourceKind::MemberListCore),
            (&self.fragments, now.fragments, ResourceKind::Fragments),
            (&self.payloads, now.payloads, ResourceKind::PayloadCache),
        ];
        for (cell, value, kind) in parts {
            let old = cell.swap(value, Ordering::Relaxed);
            Sink(kind).bytes_moved(old, value);
        }
    }
}

struct Shared {
    core: Mutex<Held>,
    bytes: Bytes,
}

impl Shared {
    fn take_and_clear(&self, mut guard: MutexGuard<'_, Held>) -> Held {
        let taken = guard.take();
        self.bytes.report(Accounting::default());
        taken
    }
}

struct Closing(Arc<Shared>);

impl Drop for Closing {
    fn drop(&mut self) {
        let guard = self.0.core.lock().unwrap_or_else(PoisonError::into_inner);
        let taken = self.0.take_and_clear(guard);
        drop(taken);
    }
}

pub struct MemberList {
    shared: Arc<Shared>,
    status: AtomicU8,
    counts: ResourceArc<Counts>,
}

impl MemberList {
    pub fn new(core: Option<Core<Subscriber>>, counts: ResourceArc<Counts>) -> MemberList {
        let status = if core.is_some() {
            Status::Ready
        } else {
            Status::Broken
        };
        Sink(ResourceKind::MemberListCore).opened();
        let bytes = Bytes::new();
        if let Some(core) = &core {
            let (total, online) = core.guild_counts();
            counts.store(total, online);
            bytes.report(core.accounting());
        }
        MemberList {
            shared: Arc::new(Shared {
                core: Mutex::new(core),
                bytes,
            }),
            status: AtomicU8::new(status.code()),
            counts,
        }
    }

    pub fn status(&self) -> Status {
        Status::from_code(self.status.load(Ordering::Acquire))
    }

    fn mark(&self, status: Status) {
        if self.status() != Status::Released {
            self.status.store(status.code(), Ordering::Release);
        }
    }

    pub fn counts(&self) -> &ResourceArc<Counts> {
        &self.counts
    }

    fn refresh(&self, core: &Core<Subscriber>) {
        let (total, online) = core.guild_counts();
        self.counts.store(total, online);
        self.shared.bytes.report(core.accounting());
    }

    fn ready(&self) -> Result<(), Failure> {
        match self.status() {
            Status::Ready => Ok(()),
            Status::Broken => Err(Failure::Broken),
            Status::Released => Err(Failure::Gone),
        }
    }

    fn poisoned(&self) -> Failure {
        self.mark(Status::Broken);
        Failure::Broken
    }

    pub fn with_core<R>(
        &self,
        work: impl FnOnce(&mut Core<Subscriber>) -> Result<R, Failure>,
    ) -> Result<R, Failure> {
        self.ready()?;
        let guard = self.shared.core.lock().map_err(|_| self.poisoned())?;
        self.work_on(guard, work)
    }

    pub fn try_with_core<R>(
        &self,
        work: impl FnOnce(&mut Core<Subscriber>) -> Result<R, Failure>,
    ) -> Result<R, Failure> {
        self.ready()?;
        let guard = match self.shared.core.try_lock() {
            Ok(guard) => guard,
            Err(TryLockError::WouldBlock) => return Err(Failure::NeedsDirty),
            Err(TryLockError::Poisoned(_)) => return Err(self.poisoned()),
        };
        self.work_on(guard, work)
    }

    fn work_on<R>(
        &self,
        mut guard: MutexGuard<'_, Held>,
        work: impl FnOnce(&mut Core<Subscriber>) -> Result<R, Failure>,
    ) -> Result<R, Failure> {
        let Some(core) = guard.as_mut() else {
            return Err(Failure::Gone);
        };
        let result = work(core);
        if self.status() != Status::Released {
            self.refresh(core);
        }
        result
    }

    pub fn read_even_broken<R>(&self, read: impl FnOnce(&Core<Subscriber>) -> R) -> Option<R> {
        let guard = self.shared.core.lock().unwrap_or_else(|poisoned| {
            self.mark(Status::Broken);
            poisoned.into_inner()
        });
        self.unreleased(&guard).map(read)
    }

    fn unreleased<'g>(&self, guard: &'g MutexGuard<'_, Held>) -> Option<&'g Core<Subscriber>> {
        if self.status() == Status::Released {
            return None;
        }
        guard.as_ref()
    }

    pub fn try_read_even_broken<R>(
        &self,
        read: impl FnOnce(&Core<Subscriber>) -> R,
    ) -> Result<Option<R>, Failure> {
        let guard = match self.shared.core.try_lock() {
            Ok(guard) => guard,
            Err(TryLockError::WouldBlock) => return Err(Failure::NeedsDirty),
            Err(TryLockError::Poisoned(poisoned)) => {
                self.mark(Status::Broken);
                poisoned.into_inner()
            }
        };
        Ok(self.unreleased(&guard).map(read))
    }

    pub fn shut(&self) {
        self.status
            .store(Status::Released.code(), Ordering::Release);
        let guard = self
            .shared
            .core
            .lock()
            .unwrap_or_else(PoisonError::into_inner);
        let taken = self.shared.take_and_clear(guard);
        dispose(taken);
    }

    pub fn shut_now(&self) {
        self.status
            .store(Status::Released.code(), Ordering::Release);
        let taken = match self.shared.core.try_lock() {
            Ok(guard) => self.shared.take_and_clear(guard),
            Err(TryLockError::Poisoned(poisoned)) => {
                self.shared.take_and_clear(poisoned.into_inner())
            }
            Err(TryLockError::WouldBlock) => {
                guild_rt::drop_later(
                    Closing(Arc::clone(&self.shared)),
                    ResourceKind::MemberListCore,
                );
                None
            }
        };
        dispose(taken);
    }
}

fn dispose(taken: Held) {
    if let Some(core) = taken {
        if core.guild_counts().0 > IN_PLACE_MEMBERS {
            guild_rt::drop_later(core, ResourceKind::MemberListCore);
        } else {
            drop(core);
        }
    }
}

impl Drop for MemberList {
    fn drop(&mut self) {
        let taken = match self.shared.core.try_lock() {
            Ok(guard) => self.shared.take_and_clear(guard),
            Err(TryLockError::Poisoned(poisoned)) => {
                self.shared.take_and_clear(poisoned.into_inner())
            }
            Err(TryLockError::WouldBlock) => None,
        };
        dispose(taken);
        Sink(ResourceKind::MemberListCore).closed();
    }
}

impl Resource for MemberList {
    const IMPLEMENTS_DOWN: bool = true;

    fn down<'a>(&'a self, _env: Env<'a>, _pid: LocalPid, _monitor: Monitor) {
        self.shut_now();
    }
}

#[cfg(test)]
mod tests {
    use ml_core::payloads::{LOOK_AHEAD, LOOK_BEHIND, PayloadWriter};

    use std::sync::atomic::Ordering;
    use std::sync::mpsc::sync_channel;
    use std::sync::{Arc, Mutex};

    use ml_core::Accounting;

    use super::{Bytes, Closing, PayloadRes, Shared};

    #[test]
    fn a_worker_that_reports_while_the_list_closes_is_cleared_after_it() {
        let shared = Arc::new(Shared {
            core: Mutex::new(None),
            bytes: Bytes::new(),
        });
        let (locked_tx, locked_rx) = sync_channel(0);
        let (go_tx, go_rx) = sync_channel::<()>(0);
        let worker_shared = Arc::clone(&shared);
        let worker = std::thread::spawn(move || {
            let guard = worker_shared.core.lock();
            let _ = locked_tx.send(());
            let _ = go_rx.recv();
            worker_shared.bytes.report(Accounting {
                core: 4096,
                fragments: 0,
                payloads: 0,
            });
            drop(guard);
        });
        let _ = locked_rx.recv();
        let closing = Closing(Arc::clone(&shared));
        let closer = std::thread::spawn(move || drop(closing));
        let _ = go_tx.send(());
        let _ = worker.join();
        let _ = closer.join();
        assert_eq!(shared.bytes.core.load(Ordering::Relaxed), 0);
    }

    #[test]
    fn bytes_handed_to_the_vm_sit_between_both_guards() {
        for body in [&b""[..], b"{}", b"{\"ops\":[1,2,3]}", &[0xff; 4096][..]] {
            let mut writer = PayloadWriter::with_capacity(body.len());
            writer.out().extend_from_slice(body);
            let payload = writer.finish();
            let guarded = payload.guarded();
            let held = PayloadRes::new(payload.clone());
            let vm = held.vm_bytes();
            assert_eq!(vm, body);
            assert_eq!(guarded.lead, &[0; LOOK_BEHIND]);
            assert_eq!(guarded.tail, &[0; LOOK_AHEAD]);
            assert_eq!(
                guarded.lead.as_ptr().addr() + LOOK_BEHIND,
                vm.as_ptr().addr()
            );
            assert_eq!(vm.as_ptr().addr() + vm.len(), guarded.tail.as_ptr().addr());
        }
    }
}
