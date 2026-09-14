// SPDX-License-Identifier: AGPL-3.0-or-later

use std::collections::{BTreeMap, BTreeSet};

use crate::ids::{Millis, RoomKey, TurnId};
use crate::ledger::MAP_ENTRY_OVERHEAD_BYTES;

pub const SEQUENCER_ENTRY_BYTES: u64 =
    (size_of::<RoomKey>() + size_of::<RoomTurns>() + MAP_ENTRY_OVERHEAD_BYTES) as u64;

const FIRST_TURN: TurnId = TurnId::new(1);
const FNV_OFFSET: u64 = 0xcbf2_9ce4_8422_2325;
const FNV_PRIME: u64 = 0x0000_0100_0000_01b3;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Digest(u64);

impl Default for Digest {
    fn default() -> Self {
        Self::new()
    }
}

impl Digest {
    pub const fn new() -> Self {
        Self(FNV_OFFSET)
    }

    pub const fn bytes(self, bytes: &[u8]) -> Self {
        let mut hash = self.0;
        let mut index = 0;
        while index < bytes.len() {
            hash ^= bytes[index] as u64;
            hash = hash.wrapping_mul(FNV_PRIME);
            index += 1;
        }
        Self(hash)
    }

    pub const fn text(self, text: &str) -> Self {
        self.bytes(text.as_bytes())
    }

    pub const fn number(self, value: u64) -> Self {
        Self(self.0 ^ mix(value)).bytes(&[0xff])
    }

    pub const fn flag(self, value: bool) -> Self {
        self.number(value as u64)
    }

    pub const fn finish(self) -> u64 {
        self.0
    }
}

pub fn digest_bytes(bytes: &[u8]) -> u64 {
    Digest::new().bytes(bytes).finish()
}

pub fn digest_str(text: &str) -> u64 {
    Digest::new().text(text).finish()
}

const fn mix(value: u64) -> u64 {
    let mut hash = value;
    hash ^= hash >> 30;
    hash = hash.wrapping_mul(0xbf58_476d_1ce4_e5b9);
    hash ^= hash >> 27;
    hash = hash.wrapping_mul(0x94d0_49bb_1331_11eb);
    hash ^ (hash >> 31)
}

fn fold_digest(receipts: &[ReadReceipt]) -> u64 {
    let mut digest = Digest::new();
    for receipt in receipts {
        digest = digest
            .number(receipt.digest)
            .number(u64::from(receipt.ticket));
    }
    digest.finish()
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TurnPurpose {
    RoomTurn,
    Preflight,
}

impl TurnPurpose {
    pub const fn label(self) -> &'static str {
        match self {
            Self::RoomTurn => "room_turn",
            Self::Preflight => "preflight",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TurnScope {
    pub room: RoomKey,
    pub purpose: TurnPurpose,
}

impl TurnScope {
    pub const fn room_turn(room: RoomKey) -> Self {
        Self {
            room,
            purpose: TurnPurpose::RoomTurn,
        }
    }

    pub const fn preflight(room: RoomKey) -> Self {
        Self {
            room,
            purpose: TurnPurpose::Preflight,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ReadSource {
    GatewayVoiceStates,
    GatewayPendingJoins,
    MediaParticipants,
    MediaRoomList,
    Census,
}

impl ReadSource {
    pub const fn label(self) -> &'static str {
        match self {
            Self::GatewayVoiceStates => "gateway_voice_states",
            Self::GatewayPendingJoins => "gateway_pending_joins",
            Self::MediaParticipants => "media_participants",
            Self::MediaRoomList => "media_room_list",
            Self::Census => "census",
        }
    }
}

impl std::fmt::Display for ReadSource {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str(self.label())
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ReadReceipt {
    ticket: u32,
    source: ReadSource,
    at: Millis,
    digest: u64,
}

impl ReadReceipt {
    pub const fn ticket(self) -> u32 {
        self.ticket
    }

    pub const fn source(self) -> ReadSource {
        self.source
    }

    pub const fn at(self) -> Millis {
        self.at
    }

    pub const fn digest(self) -> u64 {
        self.digest
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, thiserror::Error)]
pub enum TurnError {
    #[error("a read taken in turn {found} cannot be used in turn {expected}")]
    StaleRead { expected: TurnId, found: TurnId },
    #[error("turn {turn} took {issued} reads but the observation accounts for {accounted}")]
    UnaccountedReads {
        turn: TurnId,
        issued: usize,
        accounted: usize,
    },
    #[error("turn {turn} opened at {opened_at} cannot be sealed at {sealed_at}")]
    SealedBeforeOpened {
        turn: TurnId,
        opened_at: Millis,
        sealed_at: Millis,
    },
    #[error("turn {turn} was never issued by this sequencer")]
    UnknownTurn { turn: TurnId },
    #[error("turn {turn} carries a {read} read taken at {at}, outside the turn")]
    ReadOutsideTurn {
        turn: TurnId,
        read: ReadSource,
        at: Millis,
    },
}

#[derive(Debug)]
#[must_use = "an opened turn must be sealed or abandoned"]
pub struct TurnToken {
    turn: TurnId,
    scope: TurnScope,
    opened_at: Millis,
    receipts: Vec<ReadReceipt>,
    discarded: BTreeSet<u32>,
}

impl TurnToken {
    pub const fn turn(&self) -> TurnId {
        self.turn
    }

    pub const fn scope(&self) -> TurnScope {
        self.scope
    }

    pub const fn room(&self) -> RoomKey {
        self.scope.room
    }

    pub const fn opened_at(&self) -> Millis {
        self.opened_at
    }

    pub fn reads(&self) -> &[ReadReceipt] {
        &self.receipts
    }

    pub fn read<T>(&mut self, source: ReadSource, at: Millis, digest: u64, value: T) -> Fresh<T> {
        let ticket = u32::try_from(self.receipts.len()).unwrap_or(u32::MAX);
        self.receipts.push(ReadReceipt {
            ticket,
            source,
            at,
            digest,
        });
        Fresh {
            turn: self.turn,
            tickets: vec![ticket],
            value,
        }
    }

    pub fn discard<T>(&mut self, value: Fresh<T>) -> Result<T, TurnError> {
        if value.turn != self.turn {
            return Err(TurnError::StaleRead {
                expected: self.turn,
                found: value.turn,
            });
        }
        self.discarded.extend(value.tickets.iter().copied());
        Ok(value.value)
    }
}

#[derive(Debug)]
#[must_use = "a read must be sealed into its turn or discarded"]
pub struct Fresh<T> {
    turn: TurnId,
    tickets: Vec<u32>,
    value: T,
}

impl<T> Fresh<T> {
    pub fn turn(&self) -> TurnId {
        self.turn
    }

    pub fn peek(&self) -> &T {
        &self.value
    }

    pub fn map<U, F>(self, transform: F) -> Fresh<U>
    where
        F: FnOnce(T) -> U,
    {
        Fresh {
            turn: self.turn,
            tickets: self.tickets,
            value: transform(self.value),
        }
    }

    pub fn zip<U>(self, other: Fresh<U>) -> Result<Fresh<(T, U)>, TurnError> {
        if self.turn != other.turn {
            return Err(TurnError::StaleRead {
                expected: self.turn,
                found: other.turn,
            });
        }
        let mut tickets = self.tickets;
        tickets.extend(other.tickets);
        tickets.sort_unstable();
        tickets.dedup();
        Ok(Fresh {
            turn: self.turn,
            tickets,
            value: (self.value, other.value),
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TurnRecord {
    turn: TurnId,
    scope: TurnScope,
    opened_at: Millis,
    sealed_at: Millis,
    digest: u64,
    reads: Vec<ReadReceipt>,
}

impl TurnRecord {
    pub const fn turn(&self) -> TurnId {
        self.turn
    }

    pub const fn scope(&self) -> TurnScope {
        self.scope
    }

    pub const fn room(&self) -> RoomKey {
        self.scope.room
    }

    pub const fn opened_at(&self) -> Millis {
        self.opened_at
    }

    pub const fn sealed_at(&self) -> Millis {
        self.sealed_at
    }

    pub const fn digest(&self) -> u64 {
        self.digest
    }

    pub fn reads(&self) -> &[ReadReceipt] {
        &self.reads
    }

    pub const fn duration_ms(&self) -> u64 {
        self.sealed_at.saturating_since(self.opened_at)
    }

    pub const fn age_ms(&self, now: Millis) -> u64 {
        now.saturating_since(self.sealed_at)
    }

    pub const fn is_fresh(&self, now: Millis, max_age_ms: u64) -> bool {
        self.age_ms(now) <= max_age_ms
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Observed<T> {
    record: TurnRecord,
    value: T,
}

impl<T> Observed<T> {
    pub const fn record(&self) -> &TurnRecord {
        &self.record
    }

    pub const fn turn(&self) -> TurnId {
        self.record.turn
    }

    pub const fn scope(&self) -> TurnScope {
        self.record.scope
    }

    pub const fn room(&self) -> RoomKey {
        self.record.scope.room
    }

    pub const fn opened_at(&self) -> Millis {
        self.record.opened_at
    }

    pub const fn sealed_at(&self) -> Millis {
        self.record.sealed_at
    }

    pub const fn digest(&self) -> u64 {
        self.record.digest
    }

    pub fn reads(&self) -> &[ReadReceipt] {
        &self.record.reads
    }

    pub const fn age_ms(&self, now: Millis) -> u64 {
        self.record.age_ms(now)
    }

    pub const fn is_fresh(&self, now: Millis, max_age_ms: u64) -> bool {
        self.record.is_fresh(now, max_age_ms)
    }

    pub const fn value(&self) -> &T {
        &self.value
    }

    pub fn into_parts(self) -> (TurnRecord, T) {
        (self.record, self.value)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RoomTurns {
    last_opened: TurnId,
    last_sealed: Option<TurnRecord>,
}

impl RoomTurns {
    pub const fn last_opened(&self) -> TurnId {
        self.last_opened
    }

    pub fn last_sealed(&self) -> Option<&TurnRecord> {
        self.last_sealed.as_ref()
    }
}

#[derive(Clone, Debug)]
pub struct TurnSequencer {
    next: TurnId,
    opened_total: u64,
    sealed_total: u64,
    abandoned_total: u64,
    rooms: BTreeMap<RoomKey, RoomTurns>,
}

impl Default for TurnSequencer {
    fn default() -> Self {
        Self::new()
    }
}

impl TurnSequencer {
    pub fn tracked_bytes(&self) -> u64 {
        (self.rooms.len() as u64).saturating_mul(SEQUENCER_ENTRY_BYTES)
    }

    pub const fn new() -> Self {
        Self {
            next: FIRST_TURN,
            opened_total: 0,
            sealed_total: 0,
            abandoned_total: 0,
            rooms: BTreeMap::new(),
        }
    }

    pub const fn next_turn(&self) -> TurnId {
        self.next
    }

    pub const fn opened_total(&self) -> u64 {
        self.opened_total
    }

    pub const fn sealed_total(&self) -> u64 {
        self.sealed_total
    }

    pub const fn abandoned_total(&self) -> u64 {
        self.abandoned_total
    }

    pub fn tracked_rooms(&self) -> usize {
        self.rooms.len()
    }

    pub fn history(&self, room: &RoomKey) -> Option<&RoomTurns> {
        self.rooms.get(room)
    }

    pub fn last_sealed(&self, room: &RoomKey) -> Option<&TurnRecord> {
        self.rooms.get(room).and_then(RoomTurns::last_sealed)
    }

    pub fn is_latest_sealed(&self, room: &RoomKey, turn: TurnId) -> bool {
        self.last_sealed(room)
            .is_some_and(|record| record.turn == turn)
    }

    pub fn forget(&mut self, room: &RoomKey) -> bool {
        self.rooms.remove(room).is_some()
    }

    pub fn retain<F>(&mut self, mut keep: F)
    where
        F: FnMut(&RoomKey) -> bool,
    {
        self.rooms.retain(|room, _| keep(room));
    }

    pub fn open(&mut self, scope: TurnScope, at: Millis) -> TurnToken {
        let turn = self.next;
        self.next = self.next.next();
        self.opened_total = self.opened_total.saturating_add(1);
        self.rooms
            .entry(scope.room)
            .and_modify(|history| history.last_opened = turn)
            .or_insert(RoomTurns {
                last_opened: turn,
                last_sealed: None,
            });
        TurnToken {
            turn,
            scope,
            opened_at: at,
            receipts: Vec::new(),
            discarded: BTreeSet::new(),
        }
    }

    pub fn seal<T>(
        &mut self,
        token: TurnToken,
        at: Millis,
        value: Fresh<T>,
    ) -> Result<Observed<T>, TurnError> {
        if token.turn >= self.next {
            return Err(TurnError::UnknownTurn { turn: token.turn });
        }
        if value.turn != token.turn {
            return Err(TurnError::StaleRead {
                expected: token.turn,
                found: value.turn,
            });
        }
        if at < token.opened_at {
            return Err(TurnError::SealedBeforeOpened {
                turn: token.turn,
                opened_at: token.opened_at,
                sealed_at: at,
            });
        }

        for receipt in &token.receipts {
            if receipt.at < token.opened_at || receipt.at > at {
                return Err(TurnError::ReadOutsideTurn {
                    turn: token.turn,
                    read: receipt.source,
                    at: receipt.at,
                });
            }
        }

        let accounted: BTreeSet<u32> = value
            .tickets
            .iter()
            .copied()
            .chain(token.discarded.iter().copied())
            .collect();
        if accounted.len() != token.receipts.len() {
            return Err(TurnError::UnaccountedReads {
                turn: token.turn,
                issued: token.receipts.len(),
                accounted: accounted.len(),
            });
        }

        let record = TurnRecord {
            turn: token.turn,
            scope: token.scope,
            opened_at: token.opened_at,
            sealed_at: at,
            digest: fold_digest(&token.receipts),
            reads: token.receipts,
        };
        self.sealed_total = self.sealed_total.saturating_add(1);
        self.rooms
            .entry(record.scope.room)
            .and_modify(|history| history.last_sealed = Some(record.clone()))
            .or_insert_with(|| RoomTurns {
                last_opened: record.turn,
                last_sealed: Some(record.clone()),
            });

        Ok(Observed {
            record,
            value: value.value,
        })
    }

    pub fn abandon(&mut self, token: TurnToken) -> TurnId {
        self.abandoned_total = self.abandoned_total.saturating_add(1);
        token.turn
    }
}
