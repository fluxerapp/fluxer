// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::backend::{BackendError, KeyedMutexBackend, TextureFormat};

pub const RING_SIZE: usize = 8;

pub const DUPLICATE_COUNT_MAX: u32 = 30;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SlotState {
    Free,
    Filling,
    Submitted,
    Dispatched,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RingError {
    FullDropped {
        dropped_so_far: u64,
    },
    BackendFailed {
        source: BackendError,
    },
    NotInitialised,
    AlreadyInitialised,
    UnknownSlot,
    UnexpectedSlotState {
        slot_index: u32,
        observed: SlotState,
    },
    PlatformUnsupported {
        reason: &'static str,
    },
    NotImplemented {
        what: &'static str,
    },
    SlotsExhausted,
}

impl std::fmt::Display for RingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::FullDropped { dropped_so_far } => {
                write!(f, "ring full; total dropped={dropped_so_far}")
            }
            Self::BackendFailed { source } => write!(f, "backend failed: {source}"),
            Self::NotInitialised => write!(f, "ring not initialised"),
            Self::AlreadyInitialised => write!(f, "ring already initialised"),
            Self::UnknownSlot => write!(f, "slot handle does not belong to this ring"),
            Self::UnexpectedSlotState {
                slot_index,
                observed,
            } => {
                write!(f, "slot {slot_index} in unexpected state {observed:?}")
            }
            Self::PlatformUnsupported { reason } => write!(f, "platform unsupported: {reason}"),
            Self::NotImplemented { what } => write!(f, "not implemented: {what}"),
            Self::SlotsExhausted => write!(f, "slot id space exhausted (all candidates in use)"),
        }
    }
}

impl std::error::Error for RingError {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct RingMetrics {
    pub submitted_count: u64,
    pub completed_count: u64,
    pub dropped_count: u64,
    pub dispatched_count: u64,
    pub lagged_count: u64,
}

pub struct EncoderReady<H: Clone> {
    pub handle: H,
    pub sequence: u64,
    pub slot_index: u32,
    pub duplicate_count: u32,
}

pub struct FillReservation<H: Clone> {
    pub handle: H,
    slot_index: u32,
    key: u64,
}

impl<H: Clone> FillReservation<H> {
    pub fn slot_index(&self) -> u32 {
        let index = self.slot_index;
        assert!((index as usize) < RING_SIZE_MAX, "slot_index within max");
        assert!(self.key < u64::MAX, "reservation key plausible");
        index
    }
}

struct SlotMeta {
    state: SlotState,
    sequence: u64,
    key: u64,
    duplicate_count: u32,
}

impl SlotMeta {
    const fn fresh() -> Self {
        Self {
            state: SlotState::Free,
            sequence: 0,
            key: 0,
            duplicate_count: 0,
        }
    }
}

pub struct EncoderInputRing<B: KeyedMutexBackend> {
    backend: B,
    slots: Vec<B::SlotHandle>,
    meta: Vec<SlotMeta>,
    metrics: RingMetrics,
    pending_lagged: u32,
    initialised: bool,
    width: u32,
    height: u32,
    format: TextureFormat,
}

impl<B: KeyedMutexBackend> EncoderInputRing<B> {
    pub fn new(backend: B) -> Self {
        let ring = Self {
            backend,
            slots: Vec::with_capacity(B::NUM_SLOTS),
            meta: Vec::with_capacity(B::NUM_SLOTS),
            metrics: RingMetrics::default(),
            pending_lagged: 0,
            initialised: false,
            width: 0,
            height: 0,
            format: TextureFormat::Nv12,
        };
        assert!(!ring.initialised, "fresh ring is uninitialised");
        assert_eq!(ring.slots.len(), 0, "fresh ring has no slots");
        ring
    }

    pub fn initialise(
        &mut self,
        width: u32,
        height: u32,
        format: TextureFormat,
    ) -> Result<(), RingError> {
        if self.initialised {
            return Err(RingError::AlreadyInitialised);
        }
        let handles = self
            .backend
            .create_slots(width, height, format)
            .map_err(|source| RingError::BackendFailed { source })?;
        assert_eq!(
            handles.len(),
            B::NUM_SLOTS,
            "backend returns NUM_SLOTS handles"
        );
        assert!(handles.len() <= RING_SIZE_MAX, "NUM_SLOTS within max");
        self.slots = handles;
        self.meta = (0..B::NUM_SLOTS).map(|_| SlotMeta::fresh()).collect();
        self.width = width;
        self.height = height;
        self.format = format;
        self.initialised = true;
        assert!(self.initialised, "initialised flipped");
        assert_eq!(self.slots.len(), self.meta.len(), "slots and meta align");
        Ok(())
    }

    pub fn submit<F>(&mut self, fill: F) -> Result<(), RingError>
    where
        F: FnOnce(&mut B::SlotHandle),
    {
        let mut reservation = self.reserve()?;
        fill(&mut reservation.handle);
        let sequence = self.commit(reservation)?;
        assert!(sequence > 0, "committed sequence positive");
        assert!(
            self.metrics.submitted_count >= sequence,
            "monotonic submitted"
        );
        Ok(())
    }

    pub fn submit_skip_oldest<F>(&mut self, fill: F) -> Result<(), RingError>
    where
        F: FnOnce(&mut B::SlotHandle),
    {
        let mut reservation = self.reserve_skip_oldest()?;
        fill(&mut reservation.handle);
        let sequence = self.commit(reservation)?;
        assert!(sequence > 0, "skip-oldest: committed sequence positive");
        assert!(
            self.metrics.submitted_count >= sequence,
            "skip-oldest: monotonic submitted"
        );
        Ok(())
    }

    pub fn reserve(&mut self) -> Result<FillReservation<B::SlotHandle>, RingError> {
        if !self.initialised {
            return Err(RingError::NotInitialised);
        }
        self.acquire_free_slot()
    }

    pub fn reserve_skip_oldest(&mut self) -> Result<FillReservation<B::SlotHandle>, RingError> {
        if !self.initialised {
            return Err(RingError::NotInitialised);
        }
        if self.find_free_slot().is_none() {
            let _ = self.evict_oldest_submitted();
        }
        self.acquire_free_slot()
    }

    fn acquire_free_slot(&mut self) -> Result<FillReservation<B::SlotHandle>, RingError> {
        assert!(self.initialised, "acquire requires initialised ring");
        assert_eq!(self.slots.len(), self.meta.len(), "slots and meta align");
        for index in 0..self.meta.len() {
            if self.meta[index].state != SlotState::Free {
                continue;
            }
            let key = self.meta[index].key;
            match self.backend.acquire_write(&self.slots[index], key) {
                Ok(()) => {
                    self.meta[index].state = SlotState::Filling;
                    return Ok(FillReservation {
                        handle: self.slots[index].clone(),
                        slot_index: index as u32,
                        key,
                    });
                }
                Err(BackendError::WouldBlock { .. }) => continue,
                Err(source) => return Err(RingError::BackendFailed { source }),
            }
        }
        self.fold_lagged(1);
        self.metrics.lagged_count = self.metrics.lagged_count.saturating_add(1);
        self.metrics.dropped_count = self.metrics.dropped_count.saturating_add(1);
        Err(RingError::FullDropped {
            dropped_so_far: self.metrics.dropped_count,
        })
    }

    pub fn commit(
        &mut self,
        reservation: FillReservation<B::SlotHandle>,
    ) -> Result<u64, RingError> {
        if !self.initialised {
            return Err(RingError::NotInitialised);
        }
        let index = reservation.slot_index as usize;
        if index >= self.meta.len() {
            return Err(RingError::UnknownSlot);
        }
        let observed = self.meta[index].state;
        if observed != SlotState::Filling {
            return Err(RingError::UnexpectedSlotState {
                slot_index: reservation.slot_index,
                observed,
            });
        }
        assert_eq!(
            self.meta[index].key, reservation.key,
            "reservation key matches slot"
        );
        let next_key = reservation.key.wrapping_add(1);
        self.backend
            .release_write(&self.slots[index], next_key)
            .map_err(|source| RingError::BackendFailed { source })?;
        let sequence = self.metrics.submitted_count.saturating_add(1);
        self.meta[index].state = SlotState::Submitted;
        self.meta[index].sequence = sequence;
        self.meta[index].key = next_key;
        self.meta[index].duplicate_count = self.pending_lagged;
        self.pending_lagged = 0;
        self.metrics.submitted_count = sequence;
        self.metrics.completed_count = self.metrics.completed_count.saturating_add(1);
        assert_eq!(
            self.meta[index].state,
            SlotState::Submitted,
            "post-commit submitted"
        );
        assert!(
            self.metrics.submitted_count >= sequence,
            "commit: monotonic submitted"
        );
        Ok(sequence)
    }

    pub fn cancel(&mut self, reservation: FillReservation<B::SlotHandle>) -> Result<(), RingError> {
        if !self.initialised {
            return Err(RingError::NotInitialised);
        }
        let index = reservation.slot_index as usize;
        if index >= self.meta.len() {
            return Err(RingError::UnknownSlot);
        }
        let observed = self.meta[index].state;
        if observed != SlotState::Filling {
            return Err(RingError::UnexpectedSlotState {
                slot_index: reservation.slot_index,
                observed,
            });
        }
        assert_eq!(
            self.meta[index].key, reservation.key,
            "cancel: reservation key matches slot"
        );
        let next_key = reservation.key.wrapping_add(1);
        self.backend
            .release_write(&self.slots[index], next_key)
            .map_err(|source| RingError::BackendFailed { source })?;
        self.backend.mark_consumed(&self.slots[index]);
        self.meta[index].state = SlotState::Free;
        self.meta[index].key = next_key;
        assert_eq!(self.meta[index].state, SlotState::Free, "post-cancel free");
        assert!(
            !self.backend.poll_complete(&self.slots[index]),
            "cancelled slot not complete"
        );
        Ok(())
    }

    fn fold_lagged(&mut self, amount: u32) {
        assert!(amount > 0, "fold amount positive");
        assert!(
            amount <= DUPLICATE_COUNT_MAX.saturating_add(1),
            "fold amount bounded"
        );
        let mut newest: Option<usize> = None;
        let mut newest_sequence: u64 = 0;
        for (i, meta) in self.meta.iter().enumerate() {
            if meta.state != SlotState::Submitted {
                continue;
            }
            if meta.sequence >= newest_sequence {
                newest_sequence = meta.sequence;
                newest = Some(i);
            }
        }
        match newest {
            Some(index) => {
                let total = self.meta[index].duplicate_count.saturating_add(amount);
                self.meta[index].duplicate_count = total.min(DUPLICATE_COUNT_MAX);
            }
            None => {
                let total = self.pending_lagged.saturating_add(amount);
                self.pending_lagged = total.min(DUPLICATE_COUNT_MAX);
            }
        }
        assert!(
            self.pending_lagged <= DUPLICATE_COUNT_MAX,
            "pending lag bounded"
        );
    }

    fn evict_oldest_submitted(&mut self) -> bool {
        let mut chosen: Option<usize> = None;
        let mut chosen_sequence: u64 = u64::MAX;
        for (i, meta) in self.meta.iter().enumerate() {
            if meta.state != SlotState::Submitted {
                continue;
            }
            if meta.sequence < chosen_sequence {
                chosen_sequence = meta.sequence;
                chosen = Some(i);
            }
        }
        let Some(index) = chosen else {
            return false;
        };
        assert_eq!(
            self.meta[index].state,
            SlotState::Submitted,
            "evict candidate is submitted"
        );
        assert!(
            chosen_sequence != u64::MAX,
            "evict candidate had real sequence"
        );
        let folded = self.meta[index].duplicate_count.saturating_add(1);
        self.backend.mark_consumed(&self.slots[index]);
        self.meta[index].state = SlotState::Free;
        self.meta[index].duplicate_count = 0;
        self.fold_lagged(folded);
        self.metrics.lagged_count = self.metrics.lagged_count.saturating_add(1);
        self.metrics.dropped_count = self.metrics.dropped_count.saturating_add(1);
        true
    }

    pub fn poll_next_ready(&mut self) -> Option<EncoderReady<B::SlotHandle>> {
        if !self.initialised {
            return None;
        }
        let mut chosen: Option<usize> = None;
        let mut chosen_sequence: u64 = u64::MAX;
        for i in 0..self.meta.len() {
            if self.meta[i].state != SlotState::Submitted {
                continue;
            }
            if !self.backend.poll_complete(&self.slots[i]) {
                continue;
            }
            if self.meta[i].sequence < chosen_sequence {
                chosen_sequence = self.meta[i].sequence;
                chosen = Some(i);
            }
        }
        let index = chosen?;
        assert_eq!(
            self.meta[index].state,
            SlotState::Submitted,
            "ready slot was submitted"
        );
        assert!(
            self.backend.poll_complete(&self.slots[index]),
            "ready slot is complete"
        );
        self.meta[index].state = SlotState::Dispatched;
        self.metrics.dispatched_count = self.metrics.dispatched_count.saturating_add(1);
        Some(EncoderReady {
            handle: self.slots[index].clone(),
            sequence: self.meta[index].sequence,
            slot_index: index as u32,
            duplicate_count: self.meta[index].duplicate_count,
        })
    }

    pub fn release_completed(
        &mut self,
        ready: EncoderReady<B::SlotHandle>,
    ) -> Result<(), RingError> {
        if !self.initialised {
            return Err(RingError::NotInitialised);
        }
        let index = ready.slot_index as usize;
        if index >= self.meta.len() {
            return Err(RingError::UnknownSlot);
        }
        let observed = self.meta[index].state;
        if observed != SlotState::Dispatched {
            return Err(RingError::UnexpectedSlotState {
                slot_index: ready.slot_index,
                observed,
            });
        }
        self.backend.mark_consumed(&self.slots[index]);
        self.meta[index].state = SlotState::Free;
        assert_eq!(self.meta[index].state, SlotState::Free, "post-release free");
        assert!(
            !self.backend.poll_complete(&self.slots[index]),
            "no longer reports complete"
        );
        Ok(())
    }

    pub fn metrics(&self) -> RingMetrics {
        assert!(
            self.metrics.completed_count <= self.metrics.submitted_count,
            "complete<=submit"
        );
        assert!(
            self.metrics.dispatched_count <= self.metrics.completed_count,
            "dispatch<=complete"
        );
        self.metrics
    }

    pub fn submitted_count(&self) -> u64 {
        self.metrics.submitted_count
    }

    pub fn completed_count(&self) -> u64 {
        self.metrics.completed_count
    }

    pub fn dropped_count(&self) -> u64 {
        self.metrics.dropped_count
    }

    pub fn dispatched_count(&self) -> u64 {
        self.metrics.dispatched_count
    }

    pub fn capacity(&self) -> usize {
        let cap = B::NUM_SLOTS;
        assert!(cap > 0, "NUM_SLOTS must be positive");
        assert!(cap <= RING_SIZE_MAX, "NUM_SLOTS within max");
        cap
    }

    pub fn free_count(&self) -> usize {
        let mut count: usize = 0;
        for meta in self.meta.iter() {
            if meta.state == SlotState::Free {
                count = count.saturating_add(1);
            }
        }
        assert!(count <= self.meta.len(), "free count within capacity");
        count
    }

    pub fn backend_mut(&mut self) -> &mut B {
        assert!(self.initialised, "backend access requires init");
        &mut self.backend
    }

    fn find_free_slot(&self) -> Option<usize> {
        for (i, meta) in self.meta.iter().enumerate() {
            if meta.state == SlotState::Free {
                return Some(i);
            }
        }
        None
    }
}

pub const RING_SIZE_MAX: usize = 16;

const _: () = assert!(RING_SIZE <= RING_SIZE_MAX, "RING_SIZE within max");

#[cfg(test)]
mod tests {
    use super::*;
    use crate::backend::{CpuMemcpyBackend, CpuSlotHandle};

    fn make_ring() -> EncoderInputRing<CpuMemcpyBackend> {
        let mut ring = EncoderInputRing::new(CpuMemcpyBackend::new());
        ring.initialise(64, 64, TextureFormat::Nv12).expect("init");
        ring
    }

    fn fill_noop(_: &mut CpuSlotHandle) {}

    #[test]
    fn submit_then_poll_returns_some() {
        let mut ring = make_ring();
        ring.submit(fill_noop).expect("submit");
        let ready = ring.poll_next_ready().expect("poll yields ready");
        assert_eq!(ready.sequence, 1);
        ring.release_completed(ready).expect("release");
    }

    #[test]
    fn eight_submits_dispatch_in_fifo_order() {
        let mut ring = make_ring();
        for _ in 0..8 {
            ring.submit(fill_noop).expect("submit");
        }
        assert_eq!(ring.submitted_count(), 8);
        let mut observed_seq: Vec<u64> = Vec::new();
        for _ in 0..8 {
            let ready = ring.poll_next_ready().expect("ready");
            observed_seq.push(ready.sequence);
            ring.release_completed(ready).expect("release");
        }
        assert_eq!(observed_seq, vec![1, 2, 3, 4, 5, 6, 7, 8]);
    }

    #[test]
    fn ninth_submit_when_full_returns_full_dropped() {
        let mut ring = make_ring();
        for _ in 0..8 {
            ring.submit(fill_noop).expect("submit");
        }
        let err = ring.submit(fill_noop).err();
        assert!(matches!(
            err,
            Some(RingError::FullDropped { dropped_so_far: 1 })
        ));
        assert_eq!(ring.dropped_count(), 1);
        let err2 = ring.submit(fill_noop).err();
        assert!(matches!(
            err2,
            Some(RingError::FullDropped { dropped_so_far: 2 })
        ));
        assert_eq!(ring.dropped_count(), 2);
    }

    #[test]
    fn release_completed_returns_slot_to_pool() {
        let mut ring = make_ring();
        ring.submit(fill_noop).expect("submit");
        let ready = ring.poll_next_ready().expect("ready");
        assert_eq!(ring.free_count(), 7);
        ring.release_completed(ready).expect("release");
        assert_eq!(ring.free_count(), 8);
        ring.submit(fill_noop).expect("re-submit after release");
    }

    #[test]
    fn pair_asserts_pass_under_random_submit_poll_release() {
        let mut ring = make_ring();
        let mut state: u64 = 0xcafef00d;
        let mut in_flight: Vec<EncoderReady<CpuSlotHandle>> = Vec::new();
        for _ in 0..1000 {
            state ^= state << 13;
            state ^= state >> 7;
            state ^= state << 17;
            let action = state % 3;
            match action {
                0 => {
                    let _ = ring.submit(fill_noop);
                }
                1 => {
                    if let Some(r) = ring.poll_next_ready() {
                        in_flight.push(r);
                    }
                }
                _ => {
                    if let Some(r) = in_flight.pop() {
                        ring.release_completed(r).expect("release");
                    }
                }
            }
            let metrics = ring.metrics();
            assert!(metrics.completed_count <= metrics.submitted_count);
            assert!(metrics.dispatched_count <= metrics.completed_count);
        }
        while let Some(r) = in_flight.pop() {
            ring.release_completed(r).expect("drain release");
        }
        while let Some(r) = ring.poll_next_ready() {
            ring.release_completed(r).expect("drain release post poll");
        }
        assert_eq!(ring.free_count(), ring.capacity());
    }

    #[test]
    #[allow(clippy::panic)]
    fn submit_with_panicking_closure_leaves_state_consistent() {
        use std::panic::{AssertUnwindSafe, catch_unwind};
        let mut ring = make_ring();
        ring.submit(fill_noop).expect("first submit ok");
        let result = catch_unwind(AssertUnwindSafe(|| {
            let _ = ring.submit(|_h: &mut CpuSlotHandle| panic!("user fill panicked"));
        }));
        assert!(result.is_err());
        let metrics = ring.metrics();
        assert!(metrics.submitted_count >= 1);
        assert!(metrics.completed_count <= metrics.submitted_count);
        assert!(metrics.dispatched_count <= metrics.completed_count);
        let ready = ring.poll_next_ready().expect("first frame still pollable");
        assert_eq!(ready.sequence, 1);
        ring.release_completed(ready).expect("release first");
    }

    #[test]
    fn determinism_same_sequence_yields_same_release_order() {
        fn run() -> Vec<u64> {
            let mut ring = make_ring();
            let mut released: Vec<u64> = Vec::new();
            for _ in 0..8 {
                ring.submit(fill_noop).expect("submit");
            }
            for _ in 0..8 {
                let r = ring.poll_next_ready().expect("ready");
                released.push(r.sequence);
                ring.release_completed(r).expect("release");
            }
            released
        }
        let a = run();
        let b = run();
        assert_eq!(a, b);
        assert_eq!(a, vec![1, 2, 3, 4, 5, 6, 7, 8]);
    }

    #[test]
    fn capacity_is_eight() {
        let ring = make_ring();
        assert_eq!(ring.capacity(), 8);
        assert_eq!(ring.free_count(), 8);
    }

    #[test]
    fn double_release_of_same_ready_rejected() {
        let mut ring = make_ring();
        ring.submit(fill_noop).expect("submit");
        let ready = ring.poll_next_ready().expect("ready");
        let cloned = EncoderReady {
            handle: ready.handle.clone(),
            sequence: ready.sequence,
            slot_index: ready.slot_index,
            duplicate_count: ready.duplicate_count,
        };
        ring.release_completed(ready).expect("first release");
        let err = ring.release_completed(cloned).err();
        assert!(matches!(err, Some(RingError::UnexpectedSlotState { .. })));
    }

    #[test]
    fn submit_before_init_returns_not_initialised() {
        let mut ring: EncoderInputRing<CpuMemcpyBackend> =
            EncoderInputRing::new(CpuMemcpyBackend::new());
        let err = ring.submit(fill_noop).err();
        assert!(matches!(err, Some(RingError::NotInitialised)));
    }

    #[test]
    fn re_initialise_rejected() {
        let mut ring = make_ring();
        let err = ring.initialise(64, 64, TextureFormat::Nv12).err();
        assert!(matches!(err, Some(RingError::AlreadyInitialised)));
    }

    #[test]
    fn poll_when_empty_returns_none() {
        let mut ring = make_ring();
        assert!(ring.poll_next_ready().is_none());
    }

    #[test]
    fn skip_oldest_rejects_when_every_slot_is_dispatched() {
        let mut ring = make_ring();
        for _ in 0..8 {
            ring.submit(fill_noop).expect("submit");
        }
        let mut in_flight: Vec<EncoderReady<CpuSlotHandle>> = Vec::new();
        for _ in 0..8 {
            in_flight.push(ring.poll_next_ready().expect("ready"));
        }
        assert_eq!(ring.free_count(), 0);
        let err = ring.submit_skip_oldest(fill_noop).err();
        assert!(matches!(
            err,
            Some(RingError::FullDropped { dropped_so_far: 1 })
        ));
        assert_eq!(ring.dropped_count(), 1);
        assert_eq!(ring.dispatched_count(), 8);
        for ready in in_flight.drain(..) {
            ring.release_completed(ready).expect("release");
        }
        assert_eq!(ring.free_count(), 8);
        ring.submit_skip_oldest(fill_noop)
            .expect("submit succeeds once dispatched slots are released");
    }

    #[test]
    fn skip_oldest_evicts_oldest_submitted_never_dispatched() {
        let mut ring = make_ring();
        for _ in 0..8 {
            ring.submit(fill_noop).expect("submit");
        }
        let mut dispatched: Vec<EncoderReady<CpuSlotHandle>> = Vec::new();
        for _ in 0..3 {
            dispatched.push(ring.poll_next_ready().expect("ready"));
        }
        assert_eq!(dispatched[0].sequence, 1);
        assert_eq!(dispatched[2].sequence, 3);
        ring.submit_skip_oldest(fill_noop)
            .expect("skip-oldest evicts a submitted slot");
        assert_eq!(ring.dropped_count(), 1);
        assert_eq!(ring.submitted_count(), 9);
        let mut remaining: Vec<u64> = Vec::new();
        while let Some(ready) = ring.poll_next_ready() {
            remaining.push(ready.sequence);
            ring.release_completed(ready).expect("release");
        }
        assert_eq!(remaining, vec![5, 6, 7, 8, 9], "sequence 4 was evicted");
        for ready in dispatched.drain(..) {
            ring.release_completed(ready).expect("release dispatched");
        }
        assert_eq!(ring.free_count(), 8);
    }

    #[test]
    fn reserve_then_commit_matches_submit_semantics() {
        let mut ring = make_ring();
        let reservation = ring.reserve().expect("reserve");
        assert_eq!(reservation.slot_index(), 0);
        assert_eq!(ring.free_count(), 7);
        assert_eq!(ring.submitted_count(), 0, "sequence assigned at commit");
        let sequence = ring.commit(reservation).expect("commit");
        assert_eq!(sequence, 1);
        let ready = ring.poll_next_ready().expect("ready");
        assert_eq!(ready.sequence, 1);
        assert_eq!(ready.duplicate_count, 0);
        ring.release_completed(ready).expect("release");
    }

    #[test]
    fn cancel_returns_slot_to_free_without_sequence() {
        let mut ring = make_ring();
        let reservation = ring.reserve().expect("reserve");
        ring.cancel(reservation).expect("cancel");
        assert_eq!(ring.free_count(), 8);
        assert_eq!(ring.submitted_count(), 0);
        assert!(ring.poll_next_ready().is_none());
        ring.submit(fill_noop).expect("submit after cancel");
        let ready = ring.poll_next_ready().expect("ready");
        assert_eq!(ready.sequence, 1);
        ring.release_completed(ready).expect("release");
    }

    #[test]
    fn commit_of_freed_reservation_rejected() {
        let mut ring = make_ring();
        let first = ring.reserve().expect("reserve");
        let index = first.slot_index();
        ring.cancel(first).expect("cancel");
        let second = ring.reserve().expect("re-reserve");
        assert_eq!(second.slot_index(), index, "same slot reused");
        ring.commit(second).expect("commit reused slot");
        let ready = ring.poll_next_ready().expect("ready");
        let stale = EncoderReady {
            handle: ready.handle.clone(),
            sequence: ready.sequence,
            slot_index: ready.slot_index,
            duplicate_count: ready.duplicate_count,
        };
        ring.release_completed(ready).expect("release");
        let err = ring.release_completed(stale).err();
        assert!(matches!(err, Some(RingError::UnexpectedSlotState { .. })));
    }

    #[test]
    fn skip_oldest_never_evicts_filling_slot() {
        let mut ring = make_ring();
        for _ in 0..7 {
            ring.submit(fill_noop).expect("submit");
        }
        let reservation = ring.reserve().expect("reserve eighth slot");
        let reserved_index = reservation.slot_index();
        ring.submit_skip_oldest(fill_noop)
            .expect("skip-oldest evicts a submitted slot");
        assert_eq!(ring.dropped_count(), 1);
        let sequence = ring.commit(reservation).expect("commit survives eviction");
        assert_eq!(sequence, 9);
        let mut seen_indices: Vec<u32> = Vec::new();
        while let Some(ready) = ring.poll_next_ready() {
            seen_indices.push(ready.slot_index);
            ring.release_completed(ready).expect("release");
        }
        assert!(seen_indices.contains(&reserved_index));
    }

    #[test]
    fn full_ring_submit_folds_lag_into_newest_submitted() {
        let mut ring = make_ring();
        for _ in 0..8 {
            ring.submit(fill_noop).expect("submit");
        }
        let err = ring.submit(fill_noop).err();
        assert!(matches!(
            err,
            Some(RingError::FullDropped { dropped_so_far: 1 })
        ));
        assert_eq!(ring.metrics().lagged_count, 1);
        let mut by_sequence: Vec<(u64, u32)> = Vec::new();
        while let Some(ready) = ring.poll_next_ready() {
            by_sequence.push((ready.sequence, ready.duplicate_count));
            ring.release_completed(ready).expect("release");
        }
        assert_eq!(by_sequence.len(), 8);
        for (sequence, duplicate_count) in by_sequence.iter().take(7) {
            assert_eq!(*duplicate_count, 0, "sequence {sequence} not duplicated");
        }
        assert_eq!(by_sequence[7], (8, 1), "newest holds the lagged frame");
    }

    #[test]
    fn eviction_conserves_duplicate_timing_slots() {
        let mut ring = make_ring();
        for _ in 0..8 {
            ring.submit(fill_noop).expect("submit");
        }
        let mut dispatched: Vec<EncoderReady<CpuSlotHandle>> = Vec::new();
        for _ in 0..7 {
            dispatched.push(ring.poll_next_ready().expect("ready"));
        }
        let _ = ring.submit(fill_noop).err();
        ring.submit_skip_oldest(fill_noop)
            .expect("evicts the only submitted slot");
        let ready = ring.poll_next_ready().expect("new frame ready");
        assert_eq!(ready.sequence, 9);
        assert_eq!(
            ready.duplicate_count, 2,
            "evicted frame plus its duplicate folded into successor"
        );
        assert_eq!(ring.metrics().lagged_count, 2);
        ring.release_completed(ready).expect("release");
        for ready in dispatched.drain(..) {
            ring.release_completed(ready).expect("release dispatched");
        }
    }

    #[test]
    fn all_dispatched_lag_attaches_to_next_submission() {
        let mut ring = make_ring();
        for _ in 0..8 {
            ring.submit(fill_noop).expect("submit");
        }
        let mut in_flight: Vec<EncoderReady<CpuSlotHandle>> = Vec::new();
        for _ in 0..8 {
            in_flight.push(ring.poll_next_ready().expect("ready"));
        }
        for _ in 0..2 {
            let err = ring.submit_skip_oldest(fill_noop).err();
            assert!(matches!(err, Some(RingError::FullDropped { .. })));
        }
        assert_eq!(ring.metrics().lagged_count, 2);
        let first = in_flight.remove(0);
        ring.release_completed(first).expect("release one");
        ring.submit_skip_oldest(fill_noop)
            .expect("submit after free");
        let ready = ring.poll_next_ready().expect("ready");
        assert_eq!(ready.sequence, 9);
        assert_eq!(ready.duplicate_count, 2, "pending lag attached");
        ring.release_completed(ready).expect("release");
        for ready in in_flight.drain(..) {
            ring.release_completed(ready).expect("release in flight");
        }
    }

    #[test]
    fn duplicate_count_saturates_at_named_cap() {
        let mut ring = make_ring();
        for _ in 0..8 {
            ring.submit(fill_noop).expect("submit");
        }
        for _ in 0..(DUPLICATE_COUNT_MAX + 5) {
            let _ = ring.submit(fill_noop).err();
        }
        let mut last: Option<(u64, u32)> = None;
        while let Some(ready) = ring.poll_next_ready() {
            last = Some((ready.sequence, ready.duplicate_count));
            ring.release_completed(ready).expect("release");
        }
        assert_eq!(
            last,
            Some((8, DUPLICATE_COUNT_MAX)),
            "duplicates saturate at cap"
        );
    }

    #[test]
    fn skip_oldest_repeated_rejection_counts_every_drop() {
        let mut ring = make_ring();
        for _ in 0..8 {
            ring.submit(fill_noop).expect("submit");
        }
        let mut in_flight: Vec<EncoderReady<CpuSlotHandle>> = Vec::new();
        for _ in 0..8 {
            in_flight.push(ring.poll_next_ready().expect("ready"));
        }
        for expected_drops in 1..=3_u64 {
            let err = ring.submit_skip_oldest(fill_noop).err();
            assert!(matches!(
                err,
                Some(RingError::FullDropped { dropped_so_far }) if dropped_so_far == expected_drops
            ));
        }
        assert_eq!(ring.dropped_count(), 3);
        while let Some(ready) = in_flight.pop() {
            ring.release_completed(ready).expect("release");
        }
    }
}
