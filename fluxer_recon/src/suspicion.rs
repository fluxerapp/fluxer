// SPDX-License-Identifier: AGPL-3.0-or-later

use std::collections::BTreeMap;

use crate::config::ReconConfig;
use crate::ids::{Location, Millis, RoomKey};
use crate::ledger::{LOCATION_MAP_BYTES, MAP_ENTRY_OVERHEAD_BYTES};

pub const SUSPICION_ENTRY_BYTES: u64 =
    (size_of::<RoomKey>() + size_of::<Hold>() + MAP_ENTRY_OVERHEAD_BYTES) as u64;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SuspicionSource {
    Webhook,
    WebhookDrop,
    Census,
    Discovery,
    Divergence,
    Control,
}

impl SuspicionSource {
    pub const ALL: [Self; 6] = [
        Self::Webhook,
        Self::WebhookDrop,
        Self::Census,
        Self::Discovery,
        Self::Divergence,
        Self::Control,
    ];

    pub const fn label(self) -> &'static str {
        match self {
            Self::Webhook => "webhook",
            Self::WebhookDrop => "webhook_drop",
            Self::Census => "census",
            Self::Discovery => "discovery",
            Self::Divergence => "divergence",
            Self::Control => "control",
        }
    }

    pub const fn index(self) -> usize {
        match self {
            Self::Webhook => 0,
            Self::WebhookDrop => 1,
            Self::Census => 2,
            Self::Discovery => 3,
            Self::Divergence => 4,
            Self::Control => 5,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SuspicionHint {
    room: RoomKey,
    source: SuspicionSource,
    at: Millis,
}

impl SuspicionHint {
    pub const fn new(room: RoomKey, source: SuspicionSource, at: Millis) -> Self {
        Self { room, source, at }
    }

    pub const fn room(&self) -> RoomKey {
        self.room
    }

    pub const fn source(&self) -> SuspicionSource {
        self.source
    }

    pub const fn at(&self) -> Millis {
        self.at
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Raised {
    Raised,
    Extended,
    Refused,
}

impl Raised {
    pub const fn label(self) -> &'static str {
        match self {
            Self::Raised => "raised",
            Self::Extended => "extended",
            Self::Refused => "refused",
        }
    }

    pub const fn is_held(self) -> bool {
        matches!(self, Self::Raised | Self::Extended)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SuspicionLimits {
    pub hold_ms: u64,
    pub max_rooms: usize,
}

impl SuspicionLimits {
    pub const fn from_config(config: &ReconConfig) -> Self {
        Self {
            hold_ms: config.suspicion_hold_ms,
            max_rooms: config.max_hot_rooms,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Hold {
    source: SuspicionSource,
    since: Millis,
    until: Millis,
    hints: u32,
}

impl Hold {
    pub const fn source(&self) -> SuspicionSource {
        self.source
    }

    pub const fn since(&self) -> Millis {
        self.since
    }

    pub const fn until(&self) -> Millis {
        self.until
    }

    pub const fn hints(&self) -> u32 {
        self.hints
    }

    pub const fn is_held(&self, now: Millis) -> bool {
        now.get() < self.until.get()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ServerHole {
    dropped: u32,
    since: Millis,
    events: u32,
}

impl ServerHole {
    pub const fn dropped(&self) -> u32 {
        self.dropped
    }

    pub const fn since(&self) -> Millis {
        self.since
    }

    pub const fn events(&self) -> u32 {
        self.events
    }
}

#[derive(Clone, Debug)]
pub struct SuspicionLane {
    limits: SuspicionLimits,
    holds: BTreeMap<RoomKey, Hold>,
    raised: Vec<RoomKey>,
    holes: BTreeMap<Location, ServerHole>,
    by_source: [u64; SuspicionSource::ALL.len()],
    raised_total: u64,
    extended_total: u64,
    refused_total: u64,
    expired_total: u64,
    holes_total: u64,
}

impl SuspicionLane {
    pub fn tracked_bytes(&self) -> u64 {
        (self.holds.len() as u64)
            .saturating_mul(SUSPICION_ENTRY_BYTES)
            .saturating_add((self.holes.len() as u64).saturating_mul(LOCATION_MAP_BYTES))
    }

    pub const fn new(limits: SuspicionLimits) -> Self {
        Self {
            limits,
            holds: BTreeMap::new(),
            raised: Vec::new(),
            holes: BTreeMap::new(),
            by_source: [0; SuspicionSource::ALL.len()],
            raised_total: 0,
            extended_total: 0,
            refused_total: 0,
            expired_total: 0,
            holes_total: 0,
        }
    }

    pub fn from_config(config: &ReconConfig) -> Self {
        Self::new(SuspicionLimits::from_config(config))
    }

    pub const fn limits(&self) -> SuspicionLimits {
        self.limits
    }

    pub const fn raised_total(&self) -> u64 {
        self.raised_total
    }

    pub const fn extended_total(&self) -> u64 {
        self.extended_total
    }

    pub const fn refused_total(&self) -> u64 {
        self.refused_total
    }

    pub const fn expired_total(&self) -> u64 {
        self.expired_total
    }

    pub const fn holes_total(&self) -> u64 {
        self.holes_total
    }

    pub const fn hints_from(&self, source: SuspicionSource) -> u64 {
        self.by_source[source.index()]
    }

    pub fn held(&self) -> usize {
        self.holds.len()
    }

    pub fn is_empty(&self) -> bool {
        self.holds.is_empty()
    }

    pub fn hold_for(&self, room: &RoomKey) -> Option<&Hold> {
        self.holds.get(room)
    }

    pub fn is_suspect(&self, room: &RoomKey, now: Millis) -> bool {
        self.holds.get(room).is_some_and(|hold| hold.is_held(now))
    }

    pub fn suspect_rooms(&self, now: Millis) -> Vec<RoomKey> {
        self.holds
            .iter()
            .filter(|(_, hold)| hold.is_held(now))
            .map(|(room, _)| *room)
            .collect()
    }

    pub fn note(&mut self, hint: SuspicionHint) -> Raised {
        self.by_source[hint.source.index()] = self.by_source[hint.source.index()].saturating_add(1);
        let until = hint.at.saturating_add_millis(self.limits.hold_ms);

        if let Some(hold) = self.holds.get_mut(&hint.room) {
            hold.until = until;
            hold.source = hint.source;
            hold.hints = hold.hints.saturating_add(1);
            self.extended_total = self.extended_total.saturating_add(1);
            return Raised::Extended;
        }

        if self.holds.len() >= self.limits.max_rooms {
            self.refused_total = self.refused_total.saturating_add(1);
            return Raised::Refused;
        }

        self.holds.insert(
            hint.room,
            Hold {
                source: hint.source,
                since: hint.at,
                until,
                hints: 1,
            },
        );
        self.raised.push(hint.room);
        self.raised_total = self.raised_total.saturating_add(1);
        Raised::Raised
    }

    pub fn note_room(&mut self, room: RoomKey, source: SuspicionSource, now: Millis) -> Raised {
        self.note(SuspicionHint::new(room, source, now))
    }

    pub fn drain_raised(&mut self) -> Vec<RoomKey> {
        std::mem::take(&mut self.raised)
    }

    pub fn expire(&mut self, now: Millis) -> usize {
        let expired: Vec<RoomKey> = self
            .holds
            .iter()
            .filter(|(_, hold)| !hold.is_held(now))
            .map(|(room, _)| *room)
            .collect();
        for room in &expired {
            self.holds.remove(room);
        }
        self.expired_total = self.expired_total.saturating_add(expired.len() as u64);
        expired.len()
    }

    pub fn forget(&mut self, room: &RoomKey) -> bool {
        self.raised.retain(|held| held != room);
        self.holds.remove(room).is_some()
    }

    pub fn note_dropped(&mut self, location: &Location, dropped: u32, now: Millis) -> bool {
        if dropped == 0 {
            return false;
        }
        match self.holes.get_mut(location) {
            Some(hole) => {
                hole.dropped = hole.dropped.saturating_add(dropped);
                hole.events = hole.events.saturating_add(1);
            }
            None => {
                self.holes.insert(
                    location.clone(),
                    ServerHole {
                        dropped,
                        since: now,
                        events: 1,
                    },
                );
                self.holes_total = self.holes_total.saturating_add(1);
            }
        }
        true
    }

    pub fn hole_for(&self, location: &Location) -> Option<&ServerHole> {
        self.holes.get(location)
    }

    pub fn holed_servers(&self) -> Vec<Location> {
        self.holes.keys().cloned().collect()
    }

    pub fn clear_hole(&mut self, location: &Location) -> bool {
        self.holes.remove(location).is_some()
    }

    pub fn holes(&self) -> usize {
        self.holes.len()
    }
}
