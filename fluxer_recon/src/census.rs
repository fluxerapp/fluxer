// SPDX-License-Identifier: AGPL-3.0-or-later

use std::collections::BTreeMap;

use crate::config::ReconConfig;
use crate::evidence::CensusCrossCheck;
use crate::gateway::{ActiveVoiceRooms, GatewayApi, GatewayFault, GatewayMethod};
use crate::ids::{Epoch, Millis, RoomKey};
use crate::ledger::MAP_ENTRY_OVERHEAD_BYTES;

pub const CENSUS_METHOD: GatewayMethod = GatewayMethod::ActiveVoiceRooms;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CensusRead {
    Counted(ActiveVoiceRooms),
    Unreadable(GatewayFault),
}

impl CensusRead {
    pub const fn is_readable(&self) -> bool {
        matches!(self, Self::Counted(_))
    }

    pub const fn counted(&self) -> Option<&ActiveVoiceRooms> {
        match self {
            Self::Counted(rooms) => Some(rooms),
            Self::Unreadable(_) => None,
        }
    }

    pub const fn fault(&self) -> Option<&GatewayFault> {
        match self {
            Self::Counted(_) => None,
            Self::Unreadable(fault) => Some(fault),
        }
    }

    pub const fn label(&self) -> &'static str {
        match self {
            Self::Counted(_) => "counted",
            Self::Unreadable(_) => "unreadable",
        }
    }
}

pub async fn read_census<G>(gateway: &G) -> CensusRead
where
    G: GatewayApi,
{
    match gateway.active_voice_rooms().await {
        Ok(rooms) => CensusRead::Counted(rooms),
        Err(fault) => CensusRead::Unreadable(fault),
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CensusSnapshot {
    counts: BTreeMap<RoomKey, u32>,
    node_count: u32,
    unparseable_rooms: u32,
    taken_at: Millis,
    epoch: Epoch,
}

impl CensusSnapshot {
    pub fn rooms(&self) -> impl Iterator<Item = (RoomKey, u32)> + '_ {
        self.counts.iter().map(|(room, count)| (*room, *count))
    }

    pub fn room_keys(&self) -> Vec<RoomKey> {
        self.counts.keys().copied().collect()
    }

    pub fn voice_state_count(&self, room: &RoomKey) -> Option<u32> {
        self.counts.get(room).copied()
    }

    pub const fn node_count(&self) -> u32 {
        self.node_count
    }

    pub const fn unparseable_rooms(&self) -> u32 {
        self.unparseable_rooms
    }

    pub const fn taken_at(&self) -> Millis {
        self.taken_at
    }

    pub const fn epoch(&self) -> Epoch {
        self.epoch
    }

    pub fn len(&self) -> usize {
        self.counts.len()
    }

    pub fn is_empty(&self) -> bool {
        self.counts.is_empty()
    }

    pub const fn age_ms(&self, now: Millis) -> u64 {
        now.saturating_since(self.taken_at)
    }

    pub const fn is_fresh(&self, now: Millis, max_age_ms: u64) -> bool {
        self.age_ms(now) <= max_age_ms
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct CensusLimits {
    pub max_age_ms: u64,
    pub interval_ms: u64,
}

impl CensusLimits {
    pub const fn from_config(config: &ReconConfig) -> Self {
        Self {
            max_age_ms: config.census_max_age_ms,
            interval_ms: config.census_interval_ms,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CensusOutcome {
    Applied { epoch: Epoch, rooms: usize },
    Unchanged { epoch: Epoch },
    Failed,
}

impl CensusOutcome {
    pub const fn label(self) -> &'static str {
        match self {
            Self::Applied { .. } => "applied",
            Self::Unchanged { .. } => "unchanged",
            Self::Failed => "failed",
        }
    }

    pub const fn kept_previous_snapshot(self) -> bool {
        match self {
            Self::Applied { .. } | Self::Unchanged { .. } => false,
            Self::Failed => true,
        }
    }

    pub const fn epoch(self) -> Option<Epoch> {
        match self {
            Self::Applied { epoch, .. } | Self::Unchanged { epoch } => Some(epoch),
            Self::Failed => None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct CensusCache {
    limits: CensusLimits,
    snapshot: Option<CensusSnapshot>,
    epoch: Epoch,
    last_failure_at: Option<Millis>,
    consecutive_failures: u32,
    applied_total: u64,
    unchanged_total: u64,
    failed_total: u64,
}

pub const CENSUS_ENTRY_BYTES: u64 =
    (size_of::<RoomKey>() + size_of::<u32>() + MAP_ENTRY_OVERHEAD_BYTES) as u64;

impl CensusCache {
    pub fn tracked_bytes(&self) -> u64 {
        self.snapshot.as_ref().map_or(0, |snapshot| {
            (snapshot.len() as u64).saturating_mul(CENSUS_ENTRY_BYTES)
        })
    }

    pub const fn new(limits: CensusLimits) -> Self {
        Self {
            limits,
            snapshot: None,
            epoch: Epoch::FIRST,
            last_failure_at: None,
            consecutive_failures: 0,
            applied_total: 0,
            unchanged_total: 0,
            failed_total: 0,
        }
    }

    pub const fn limits(&self) -> CensusLimits {
        self.limits
    }

    pub const fn snapshot(&self) -> Option<&CensusSnapshot> {
        self.snapshot.as_ref()
    }

    pub const fn epoch(&self) -> Epoch {
        self.epoch
    }

    pub const fn last_failure_at(&self) -> Option<Millis> {
        self.last_failure_at
    }

    pub const fn consecutive_failures(&self) -> u32 {
        self.consecutive_failures
    }

    pub const fn applied_total(&self) -> u64 {
        self.applied_total
    }

    pub const fn unchanged_total(&self) -> u64 {
        self.unchanged_total
    }

    pub const fn failed_total(&self) -> u64 {
        self.failed_total
    }

    pub fn age_ms(&self, now: Millis) -> Option<u64> {
        self.snapshot.as_ref().map(|snapshot| snapshot.age_ms(now))
    }

    pub fn is_fresh(&self, now: Millis) -> bool {
        self.snapshot
            .as_ref()
            .is_some_and(|snapshot| snapshot.is_fresh(now, self.limits.max_age_ms))
    }

    pub fn rooms(&self) -> Vec<RoomKey> {
        self.snapshot
            .as_ref()
            .map(CensusSnapshot::room_keys)
            .unwrap_or_default()
    }

    pub fn node_count(&self) -> Option<u32> {
        self.snapshot.as_ref().map(CensusSnapshot::node_count)
    }

    pub fn accept(&mut self, read: &CensusRead, now: Millis) -> CensusOutcome {
        let counted = match read {
            CensusRead::Unreadable(_) => {
                self.failed_total = self.failed_total.saturating_add(1);
                self.consecutive_failures = self.consecutive_failures.saturating_add(1);
                self.last_failure_at = Some(now);
                return CensusOutcome::Failed;
            }
            CensusRead::Counted(counted) => counted,
        };

        self.consecutive_failures = 0;
        let counts: BTreeMap<RoomKey, u32> = counted
            .rooms
            .iter()
            .map(|entry| (entry.room, entry.voice_state_count))
            .collect();

        let unchanged = self
            .snapshot
            .as_ref()
            .is_some_and(|snapshot| snapshot.counts == counts);
        if unchanged {
            self.unchanged_total = self.unchanged_total.saturating_add(1);
            if let Some(snapshot) = self.snapshot.as_mut() {
                snapshot.taken_at = now;
                snapshot.node_count = counted.node_count;
                snapshot.unparseable_rooms = counted.unparseable_rooms;
            }
            return CensusOutcome::Unchanged { epoch: self.epoch };
        }

        self.epoch = self.epoch.next();
        self.applied_total = self.applied_total.saturating_add(1);
        let rooms = counts.len();
        self.snapshot = Some(CensusSnapshot {
            counts,
            node_count: counted.node_count,
            unparseable_rooms: counted.unparseable_rooms,
            taken_at: now,
            epoch: self.epoch,
        });
        CensusOutcome::Applied {
            epoch: self.epoch,
            rooms,
        }
    }

    pub fn cross_check(&self, room: &RoomKey, now: Millis) -> CensusCrossCheck {
        let Some(snapshot) = self.snapshot.as_ref() else {
            return CensusCrossCheck::Missing;
        };
        if !snapshot.is_fresh(now, self.limits.max_age_ms) {
            return CensusCrossCheck::Stale;
        }
        CensusCrossCheck::Fresh {
            voice_state_count: snapshot.voice_state_count(room).unwrap_or(0),
        }
    }

    pub fn forget(&mut self) {
        self.snapshot = None;
    }
}
