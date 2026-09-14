// SPDX-License-Identifier: AGPL-3.0-or-later

use std::cmp::Ordering;
use std::collections::{BTreeMap, BTreeSet, BinaryHeap};

use crate::config::ReconConfig;
use crate::ids::{Millis, RoomKey};
use crate::ledger::MAP_ENTRY_OVERHEAD_BYTES;
use crate::turn::Digest;

pub const SCHEDULE_ENTRY_BYTES: u64 = (size_of::<RoomKey>()
    + size_of::<RoomSchedule>()
    + size_of::<Due>()
    + (2 * MAP_ENTRY_OVERHEAD_BYTES)) as u64;

pub const GATEWAY_READS_PER_ROOM_TURN: u32 = 2;
pub const LIVEKIT_READS_PER_ROOM_TURN: u32 = 1;

const fn room_hash(room: RoomKey) -> u64 {
    match room {
        RoomKey::Guild {
            guild_id,
            channel_id,
        } => Digest::new()
            .text("guild")
            .number(guild_id.get())
            .number(channel_id.get())
            .finish(),
        RoomKey::Dm { channel_id } => Digest::new().text("dm").number(channel_id.get()).finish(),
    }
}

const fn tiebreak(room: RoomKey, stamp: u64) -> u64 {
    Digest::new().number(room_hash(room)).number(stamp).finish()
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Tier {
    Hot,
    Background,
}

impl Tier {
    pub const fn label(self) -> &'static str {
        match self {
            Self::Hot => "hot",
            Self::Background => "background",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Promotion {
    Promoted,
    Extended,
    Refused,
}

impl Promotion {
    pub const fn label(self) -> &'static str {
        match self {
            Self::Promoted => "promoted",
            Self::Extended => "extended",
            Self::Refused => "refused",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DeferReason {
    Budget,
    Unreadable,
}

impl DeferReason {
    pub const fn label(self) -> &'static str {
        match self {
            Self::Budget => "budget",
            Self::Unreadable => "unreadable",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SchedulerConfig {
    pub hot_period_ms: u64,
    pub background_period_ms: u64,
    pub max_hot_rooms: usize,
    pub suspicion_hold_ms: u64,
    pub turns_per_tick: usize,
}

impl SchedulerConfig {
    pub const fn from_config(config: &ReconConfig) -> Self {
        Self {
            hot_period_ms: config.hot_period_ms,
            background_period_ms: config.coverage_target_ms,
            max_hot_rooms: config.max_hot_rooms,
            suspicion_hold_ms: config.suspicion_hold_ms,
            turns_per_tick: config.turns_per_tick,
        }
    }

    pub const fn period_ms(&self, tier: Tier) -> u64 {
        match tier {
            Tier::Hot => self.hot_period_ms,
            Tier::Background => self.background_period_ms,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Due {
    at: Millis,
    tiebreak: u64,
    stamp: u64,
    room: RoomKey,
}

impl Ord for Due {
    fn cmp(&self, other: &Self) -> Ordering {
        other
            .at
            .cmp(&self.at)
            .then_with(|| other.tiebreak.cmp(&self.tiebreak))
            .then_with(|| other.room.cmp(&self.room))
            .then_with(|| other.stamp.cmp(&self.stamp))
    }
}

impl PartialOrd for Due {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct RoomSchedule {
    stamp: u64,
    tier: Tier,
    hot_until: Option<Millis>,
    due_at: Millis,
    in_flight: bool,
    served: u64,
}

#[derive(Clone, Debug)]
pub struct Scheduler {
    config: SchedulerConfig,
    heap: BinaryHeap<Due>,
    rooms: BTreeMap<RoomKey, RoomSchedule>,
    hot: BTreeSet<RoomKey>,
    stamps: u64,
    in_flight: usize,
    served_total: u64,
    starved_total: u64,
    deferred_total: u64,
    hot_refused_total: u64,
    demoted_total: u64,
}

impl Scheduler {
    pub fn tracked_bytes(&self) -> u64 {
        (self.rooms.len() as u64)
            .saturating_mul(SCHEDULE_ENTRY_BYTES)
            .saturating_add(
                (self.hot.len() as u64)
                    .saturating_mul((size_of::<RoomKey>() + MAP_ENTRY_OVERHEAD_BYTES) as u64),
            )
    }

    pub fn new(config: SchedulerConfig) -> Self {
        Self {
            config,
            heap: BinaryHeap::new(),
            rooms: BTreeMap::new(),
            hot: BTreeSet::new(),
            stamps: 0,
            in_flight: 0,
            served_total: 0,
            starved_total: 0,
            deferred_total: 0,
            hot_refused_total: 0,
            demoted_total: 0,
        }
    }

    pub const fn config(&self) -> SchedulerConfig {
        self.config
    }

    pub const fn turns_per_tick(&self) -> usize {
        self.config.turns_per_tick
    }

    pub fn tracked(&self) -> usize {
        self.rooms.len()
    }

    pub fn is_empty(&self) -> bool {
        self.rooms.is_empty()
    }

    pub fn hot_rooms(&self) -> usize {
        self.hot.len()
    }

    pub const fn in_flight(&self) -> usize {
        self.in_flight
    }

    pub const fn served_total(&self) -> u64 {
        self.served_total
    }

    pub const fn starved_total(&self) -> u64 {
        self.starved_total
    }

    pub const fn deferred_total(&self) -> u64 {
        self.deferred_total
    }

    pub const fn hot_refused_total(&self) -> u64 {
        self.hot_refused_total
    }

    pub const fn demoted_total(&self) -> u64 {
        self.demoted_total
    }

    pub fn tier(&self, room: &RoomKey) -> Option<Tier> {
        self.rooms.get(room).map(|entry| entry.tier)
    }

    pub fn due_at(&self, room: &RoomKey) -> Option<Millis> {
        self.rooms.get(room).map(|entry| entry.due_at)
    }

    pub fn served(&self, room: &RoomKey) -> Option<u64> {
        self.rooms.get(room).map(|entry| entry.served)
    }

    pub fn is_tracked(&self, room: &RoomKey) -> bool {
        self.rooms.contains_key(room)
    }

    pub fn track(&mut self, room: RoomKey, now: Millis) -> bool {
        if self.rooms.contains_key(&room) {
            return false;
        }
        let entry = RoomSchedule {
            stamp: self.next_stamp(),
            tier: Tier::Background,
            hot_until: None,
            due_at: now,
            in_flight: false,
            served: 0,
        };
        self.rooms.insert(room, entry);
        self.push(room, entry);
        true
    }

    pub fn forget(&mut self, room: &RoomKey) -> bool {
        self.hot.remove(room);
        match self.rooms.remove(room) {
            None => false,
            Some(entry) => {
                if entry.in_flight {
                    self.in_flight = self.in_flight.saturating_sub(1);
                }
                true
            }
        }
    }

    pub fn retain<F>(&mut self, mut keep: F)
    where
        F: FnMut(&RoomKey) -> bool,
    {
        let dropped: Vec<RoomKey> = self
            .rooms
            .keys()
            .copied()
            .filter(|room| !keep(room))
            .collect();
        for room in dropped {
            self.forget(&room);
        }
    }

    pub fn take_due(&mut self, now: Millis) -> Vec<RoomKey> {
        self.take_due_up_to(now, self.config.turns_per_tick)
    }

    pub fn take_due_up_to(&mut self, now: Millis, limit: usize) -> Vec<RoomKey> {
        let mut taken: Vec<RoomKey> = Vec::new();
        while taken.len() < limit {
            let Some(candidate) = self.heap.peek().copied() else {
                break;
            };
            if candidate.at > now {
                break;
            }
            self.heap.pop();
            let Some(entry) = self.rooms.get_mut(&candidate.room) else {
                continue;
            };
            if entry.stamp != candidate.stamp || entry.in_flight {
                continue;
            }
            entry.in_flight = true;
            self.in_flight = self.in_flight.saturating_add(1);
            taken.push(candidate.room);
        }
        taken
    }

    pub fn completed(&mut self, room: RoomKey, now: Millis) -> bool {
        let Some(mut entry) = self.rooms.get(&room).copied() else {
            return false;
        };
        self.release(&mut entry);
        entry.served = entry.served.saturating_add(1);
        self.served_total = self.served_total.saturating_add(1);
        entry.stamp = self.next_stamp();
        entry.due_at = now.saturating_add_millis(self.config.period_ms(entry.tier));
        self.rooms.insert(room, entry);
        self.push(room, entry);
        true
    }

    pub fn defer(&mut self, room: RoomKey, now: Millis, reason: DeferReason) -> bool {
        let Some(mut entry) = self.rooms.get(&room).copied() else {
            return false;
        };
        self.release(&mut entry);
        self.deferred_total = self.deferred_total.saturating_add(1);
        if matches!(reason, DeferReason::Budget) {
            self.starved_total = self.starved_total.saturating_add(1);
        }
        entry.stamp = self.next_stamp();
        entry.due_at = now;
        self.rooms.insert(room, entry);
        self.push(room, entry);
        true
    }

    pub fn promote(&mut self, room: RoomKey, now: Millis) -> Promotion {
        self.track(room, now);
        let Some(mut entry) = self.rooms.get(&room).copied() else {
            return Promotion::Refused;
        };
        let hot_until = now.saturating_add_millis(self.config.suspicion_hold_ms);

        if matches!(entry.tier, Tier::Hot) {
            entry.hot_until = Some(hot_until);
            self.rooms.insert(room, entry);
            self.hot.insert(room);
            return Promotion::Extended;
        }
        if self.hot.len() >= self.config.max_hot_rooms {
            self.hot_refused_total = self.hot_refused_total.saturating_add(1);
            return Promotion::Refused;
        }

        entry.tier = Tier::Hot;
        entry.hot_until = Some(hot_until);
        entry.stamp = self.next_stamp();
        entry.due_at = now;
        self.rooms.insert(room, entry);
        self.hot.insert(room);
        if !entry.in_flight {
            self.push(room, entry);
        }
        Promotion::Promoted
    }

    pub fn expire_hot(&mut self, now: Millis) -> usize {
        let expired: Vec<RoomKey> = self
            .hot
            .iter()
            .copied()
            .filter(|room| {
                self.rooms
                    .get(room)
                    .is_none_or(|entry| entry.hot_until.is_none_or(|until| until <= now))
            })
            .collect();

        for room in &expired {
            self.hot.remove(room);
            let Some(mut entry) = self.rooms.get(room).copied() else {
                continue;
            };
            entry.tier = Tier::Background;
            entry.hot_until = None;
            entry.stamp = self.next_stamp();
            entry.due_at = now.saturating_add_millis(self.config.background_period_ms);
            self.rooms.insert(*room, entry);
            if !entry.in_flight {
                self.push(*room, entry);
            }
            self.demoted_total = self.demoted_total.saturating_add(1);
        }
        expired.len()
    }

    fn release(&mut self, entry: &mut RoomSchedule) {
        if entry.in_flight {
            self.in_flight = self.in_flight.saturating_sub(1);
        }
        entry.in_flight = false;
    }

    fn next_stamp(&mut self) -> u64 {
        self.stamps = self.stamps.saturating_add(1);
        self.stamps
    }

    fn push(&mut self, room: RoomKey, entry: RoomSchedule) {
        self.heap.push(Due {
            at: entry.due_at,
            tiebreak: tiebreak(room, entry.stamp),
            stamp: entry.stamp,
            room,
        });
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Cadence {
    interval_ms: u64,
    next_at: Option<Millis>,
    fired_total: u64,
    forced: bool,
}

impl Cadence {
    pub const fn new(interval_ms: u64) -> Self {
        Self {
            interval_ms: if interval_ms == 0 { 1 } else { interval_ms },
            next_at: None,
            fired_total: 0,
            forced: false,
        }
    }

    pub const fn interval_ms(self) -> u64 {
        self.interval_ms
    }

    pub const fn next_due_at(self) -> Option<Millis> {
        self.next_at
    }

    pub const fn fired_total(self) -> u64 {
        self.fired_total
    }

    pub const fn force(&mut self) {
        self.forced = true;
    }

    pub fn due(&mut self, now: Millis) -> bool {
        let ready = self.forced || self.next_at.is_none_or(|at| now >= at);
        if !ready {
            return false;
        }
        self.forced = false;
        self.next_at = Some(now.saturating_add_millis(self.interval_ms));
        self.fired_total = self.fired_total.saturating_add(1);
        true
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct CoverageInputs {
    pub rooms: usize,
    pub gateway_reads_per_turn: u32,
    pub livekit_reads_per_turn: u32,
    pub gateway_read_rps: u32,
    pub livekit_read_rps: u32,
}

impl CoverageInputs {
    pub const fn for_rooms(rooms: usize, config: &ReconConfig) -> Self {
        Self {
            rooms,
            gateway_reads_per_turn: GATEWAY_READS_PER_ROOM_TURN,
            livekit_reads_per_turn: LIVEKIT_READS_PER_ROOM_TURN,
            gateway_read_rps: config.gateway_read_rps,
            livekit_read_rps: config.livekit_read_rps,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CoverageVerdict {
    WithinTarget,
    AboveTarget,
}

impl CoverageVerdict {
    pub const fn label(self) -> &'static str {
        match self {
            Self::WithinTarget => "within_target",
            Self::AboveTarget => "above_target",
        }
    }
}

fn lane_period_ms(rooms: usize, reads_per_turn: u32, rps: u32) -> u64 {
    let calls = u64::try_from(rooms)
        .unwrap_or(u64::MAX)
        .saturating_mul(u64::from(reads_per_turn));
    let rate = u64::from(rps.max(1));
    calls.saturating_mul(1_000).div_ceil(rate)
}

pub fn derived_coverage_period_ms(inputs: &CoverageInputs) -> u64 {
    let gateway = lane_period_ms(
        inputs.rooms,
        inputs.gateway_reads_per_turn,
        inputs.gateway_read_rps,
    );
    let livekit = lane_period_ms(
        inputs.rooms,
        inputs.livekit_reads_per_turn,
        inputs.livekit_read_rps,
    );
    gateway.max(livekit)
}

pub fn max_auditable_rooms(config: &ReconConfig) -> usize {
    let gateway = u64::from(config.gateway_read_rps)
        .saturating_mul(config.coverage_period_hard_cap_ms)
        .checked_div(1_000 * u64::from(GATEWAY_READS_PER_ROOM_TURN.max(1)))
        .unwrap_or(0);
    let livekit = u64::from(config.livekit_read_rps)
        .saturating_mul(config.coverage_period_hard_cap_ms)
        .checked_div(1_000 * u64::from(LIVEKIT_READS_PER_ROOM_TURN.max(1)))
        .unwrap_or(0);
    usize::try_from(gateway.min(livekit)).unwrap_or(usize::MAX)
}

pub fn check_coverage_period(
    period_ms: u64,
    target_ms: u64,
    hard_cap_ms: u64,
) -> anyhow::Result<CoverageVerdict> {
    if period_ms > hard_cap_ms {
        anyhow::bail!(
            "the derived coverage period is {period_ms} ms, above FLUXER_RECON_COVERAGE_PERIOD_HARD_CAP_MS ({hard_cap_ms} ms); the room count this process can audit at the configured read rates has been exceeded"
        );
    }
    if period_ms > target_ms {
        return Ok(CoverageVerdict::AboveTarget);
    }
    Ok(CoverageVerdict::WithinTarget)
}
