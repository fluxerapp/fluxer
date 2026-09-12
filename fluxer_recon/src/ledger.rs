// SPDX-License-Identifier: AGPL-3.0-or-later

use std::collections::BTreeMap;
use std::mem::size_of;

use crate::decide::DecideParams;
use crate::evidence::{CliffWindow, PriorConnection};
use crate::gateway::Nonce;
use crate::ids::{ConnectionId, ConnectionKey, Epoch, Location, Millis, RoomKey, TurnId, UserId};

const FNV_OFFSET: u64 = 0xcbf2_9ce4_8422_2325;
const FNV_PRIME: u64 = 0x0000_0100_0000_01b3;
const CONNECTION_ID_HEAP_BYTES: usize = 40;
pub const MAX_ROSTER_STAMPS: usize = 16;
const ROSTER_STAMP_BYTES: usize = 16;
const ROOM_OVERHEAD_BYTES: usize = 256 + MAX_ROSTER_STAMPS * ROSTER_STAMP_BYTES;

pub const MAP_ENTRY_OVERHEAD_BYTES: usize = 48;
pub const LOCATION_MAP_BYTES: u64 = 640;
pub const LOCATION_ENTRY_BYTES: u64 = 64;

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Fingerprint(u64);

impl Fingerprint {
    pub const fn get(self) -> u64 {
        self.0
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Hasher(u64);

impl Hasher {
    const fn new() -> Self {
        Self(FNV_OFFSET)
    }

    const fn byte(&mut self, value: u8) {
        self.0 ^= value as u64;
        self.0 = self.0.wrapping_mul(FNV_PRIME);
    }

    const fn u64(&mut self, value: u64) {
        let bytes = value.to_le_bytes();
        let mut index = 0;
        while index < bytes.len() {
            self.byte(bytes[index]);
            index += 1;
        }
    }

    fn text(&mut self, value: &str) {
        for byte in value.as_bytes() {
            self.byte(*byte);
        }
        self.byte(0);
    }

    const fn finish(self) -> Fingerprint {
        Fingerprint(self.0)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct FingerprintInput<'a> {
    pub room: RoomKey,
    pub connection: &'a ConnectionId,
    pub user_id: UserId,
    pub gateway_siblings: &'a [ConnectionId],
    pub media_locations: &'a [Location],
    pub participant_joined_at: Option<Millis>,
    pub census_epoch: Epoch,
    pub topology_epoch: Epoch,
}

pub fn fingerprint(input: &FingerprintInput<'_>) -> Fingerprint {
    let mut hasher = Hasher::new();

    match input.room {
        RoomKey::Guild {
            guild_id,
            channel_id,
        } => {
            hasher.byte(1);
            hasher.u64(guild_id.get());
            hasher.u64(channel_id.get());
        }
        RoomKey::Dm { channel_id } => {
            hasher.byte(2);
            hasher.u64(channel_id.get());
        }
    }

    hasher.text(input.connection.as_str());
    hasher.u64(input.user_id.get());

    hasher.u64(input.gateway_siblings.len() as u64);
    for sibling in input.gateway_siblings {
        hasher.text(sibling.as_str());
    }

    hasher.u64(input.media_locations.len() as u64);
    for location in input.media_locations {
        hasher.text(location.region.as_str());
        hasher.text(location.server.as_str());
    }

    match input.participant_joined_at {
        None => hasher.byte(0),
        Some(joined_at) => {
            hasher.byte(1);
            hasher.u64(joined_at.get());
        }
    }

    hasher.u64(input.census_epoch.get());
    hasher.u64(input.topology_epoch.get());
    hasher.finish()
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LocationIndex(u16);

impl LocationIndex {
    pub const fn get(self) -> u16 {
        self.0
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct LocationTable {
    entries: Vec<Location>,
}

impl LocationTable {
    pub const fn new() -> Self {
        Self {
            entries: Vec::new(),
        }
    }

    pub fn intern(&mut self, location: &Location) -> Option<LocationIndex> {
        if let Some(position) = self.entries.iter().position(|entry| entry == location) {
            return u16::try_from(position).ok().map(LocationIndex);
        }
        let index = u16::try_from(self.entries.len()).ok()?;
        self.entries.push(location.clone());
        Some(LocationIndex(index))
    }

    pub fn get(&self, index: LocationIndex) -> Option<&Location> {
        self.entries.get(usize::from(index.0))
    }

    pub fn index_of(&self, location: &Location) -> Option<LocationIndex> {
        self.entries
            .iter()
            .position(|entry| entry == location)
            .and_then(|position| u16::try_from(position).ok())
            .map(LocationIndex)
    }

    pub fn len(&self) -> usize {
        self.entries.len()
    }

    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RepairVerdict {
    Repaired,
    NoChange,
    NotRepairable,
}

impl RepairVerdict {
    pub const fn label(self) -> &'static str {
        match self {
            Self::Repaired => "repaired",
            Self::NoChange => "no_change",
            Self::NotRepairable => "not_repairable",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ActionKind {
    RemoveGatewayState,
    RemoveParticipant,
}

impl ActionKind {
    pub const fn label(self) -> &'static str {
        match self {
            Self::RemoveGatewayState => "remove_gateway_state",
            Self::RemoveParticipant => "remove_participant",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PriorDivergence {
    GatewayOnly,
    MediaOnly,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum WedgedReason {
    RepairExhausted,
    ActionIneffective,
}

impl WedgedReason {
    pub const fn label(self) -> &'static str {
        match self {
            Self::RepairExhausted => "repair_exhausted",
            Self::ActionIneffective => "action_ineffective",
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ConnectionState {
    Nascent,
    Consistent,
    PendingJoin {
        expires_at: crate::ids::WallMillis,
        nonce: Nonce,
        last_confirm: Millis,
    },
    GatewayOnly {
        since: Millis,
        corroborations: u32,
        fingerprint: Fingerprint,
        last_corroboration: Millis,
    },
    MediaOnly {
        since: Millis,
        corroborations: u32,
        fingerprint: Fingerprint,
        last_corroboration: Millis,
        location: LocationIndex,
        participant_joined_at: Millis,
        repair_verdict: Option<RepairVerdict>,
    },
    Repairing {
        since: Millis,
        last_attempt: Millis,
    },
    ActionTaken {
        kind: ActionKind,
        at: Millis,
        authorizing_turn: TurnId,
        prior: PriorDivergence,
        since: Millis,
    },
    Wedged {
        reason: WedgedReason,
        at: Millis,
        fingerprint: Fingerprint,
    },
    Retired {
        at: Millis,
    },
}

impl ConnectionState {
    pub const fn label(&self) -> &'static str {
        match self {
            Self::Nascent => "nascent",
            Self::Consistent => "consistent",
            Self::PendingJoin { .. } => "pending_join",
            Self::GatewayOnly { .. } => "gateway_only",
            Self::MediaOnly { .. } => "media_only",
            Self::Repairing { .. } => "repairing",
            Self::ActionTaken { .. } => "action_taken",
            Self::Wedged { .. } => "wedged",
            Self::Retired { .. } => "retired",
        }
    }

    pub const fn stored_fingerprint(&self) -> Option<Fingerprint> {
        match self {
            Self::GatewayOnly { fingerprint, .. }
            | Self::MediaOnly { fingerprint, .. }
            | Self::Wedged { fingerprint, .. } => Some(*fingerprint),
            Self::Nascent
            | Self::Consistent
            | Self::PendingJoin { .. }
            | Self::Repairing { .. }
            | Self::ActionTaken { .. }
            | Self::Retired { .. } => None,
        }
    }

    pub const fn corroborations(&self) -> u32 {
        match self {
            Self::GatewayOnly { corroborations, .. } | Self::MediaOnly { corroborations, .. } => {
                *corroborations
            }
            Self::Nascent
            | Self::Consistent
            | Self::PendingJoin { .. }
            | Self::Repairing { .. }
            | Self::ActionTaken { .. }
            | Self::Wedged { .. }
            | Self::Retired { .. } => 0,
        }
    }

    pub const fn divergence_since(&self) -> Option<Millis> {
        match self {
            Self::GatewayOnly { since, .. }
            | Self::MediaOnly { since, .. }
            | Self::Repairing { since, .. }
            | Self::ActionTaken { since, .. } => Some(*since),
            Self::Nascent
            | Self::Consistent
            | Self::PendingJoin { .. }
            | Self::Wedged { .. }
            | Self::Retired { .. } => None,
        }
    }

    pub const fn is_divergent(&self) -> bool {
        match self {
            Self::GatewayOnly { .. }
            | Self::MediaOnly { .. }
            | Self::Repairing { .. }
            | Self::ActionTaken { .. }
            | Self::Wedged { .. } => true,
            Self::Nascent | Self::Consistent | Self::PendingJoin { .. } | Self::Retired { .. } => {
                false
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ConnectionLedger {
    pub connection: ConnectionId,
    pub user_id: UserId,
    pub state: ConnectionState,
    pub last_seen: Millis,
    pub action_attempts: u8,
    pub repair_attempts: u8,
    pub last_action_at: Option<Millis>,
    pub last_repair_verdict: Option<RepairVerdict>,
    pub last_location: Option<LocationIndex>,
}

impl ConnectionLedger {
    pub fn new(connection: ConnectionId, user_id: UserId, now: Millis) -> Self {
        Self {
            connection,
            user_id,
            state: ConnectionState::Nascent,
            last_seen: now,
            action_attempts: 0,
            repair_attempts: 0,
            last_action_at: None,
            last_repair_verdict: None,
            last_location: None,
        }
    }

    pub const fn is_divergent(&self) -> bool {
        self.state.is_divergent()
    }

    pub const fn believed_home(&self) -> Option<LocationIndex> {
        match self.state {
            ConnectionState::Retired { .. } => None,
            ConnectionState::Nascent
            | ConnectionState::Consistent
            | ConnectionState::PendingJoin { .. }
            | ConnectionState::GatewayOnly { .. }
            | ConnectionState::MediaOnly { .. }
            | ConnectionState::Repairing { .. }
            | ConnectionState::ActionTaken { .. }
            | ConnectionState::Wedged { .. } => self.last_location,
        }
    }

    pub fn clear_evidence(&mut self) {
        self.state = ConnectionState::Nascent;
        self.last_repair_verdict = None;
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RoomLedger {
    room: RoomKey,
    connections: Vec<ConnectionLedger>,
    last_seen: Millis,
    last_turn: TurnId,
    gateway_cliff: CliffWindow,
    media_cliff: CliffWindow,
    partially_unreadable: bool,
    no_candidates: bool,
    last_known_locations: Vec<LocationIndex>,
    roster_stamps: Vec<(LocationIndex, Millis)>,
}

impl RoomLedger {
    fn new(room: RoomKey, now: Millis) -> Self {
        Self {
            room,
            connections: Vec::new(),
            last_seen: now,
            last_turn: TurnId::FIRST,
            gateway_cliff: CliffWindow::clear(),
            media_cliff: CliffWindow::clear(),
            partially_unreadable: false,
            no_candidates: false,
            last_known_locations: Vec::new(),
            roster_stamps: Vec::new(),
        }
    }

    pub const fn room(&self) -> RoomKey {
        self.room
    }

    pub fn connections(&self) -> &[ConnectionLedger] {
        &self.connections
    }

    pub fn connection(&self, connection: &ConnectionId) -> Option<&ConnectionLedger> {
        self.connections
            .iter()
            .find(|entry| &entry.connection == connection)
    }

    pub fn connection_mut(&mut self, connection: &ConnectionId) -> Option<&mut ConnectionLedger> {
        self.connections
            .iter_mut()
            .find(|entry| &entry.connection == connection)
    }

    pub const fn last_seen(&self) -> Millis {
        self.last_seen
    }

    pub const fn last_turn(&self) -> TurnId {
        self.last_turn
    }

    pub const fn note_turn(&mut self, turn: TurnId, now: Millis) {
        self.last_turn = turn;
        self.last_seen = now;
    }

    pub const fn gateway_cliff(&self) -> CliffWindow {
        self.gateway_cliff
    }

    pub const fn gateway_cliff_mut(&mut self) -> &mut CliffWindow {
        &mut self.gateway_cliff
    }

    pub const fn media_cliff(&self) -> CliffWindow {
        self.media_cliff
    }

    pub const fn media_cliff_mut(&mut self) -> &mut CliffWindow {
        &mut self.media_cliff
    }

    pub const fn partially_unreadable(&self) -> bool {
        self.partially_unreadable
    }

    pub const fn set_partially_unreadable(&mut self, value: bool) {
        self.partially_unreadable = value;
    }

    pub const fn no_candidates(&self) -> bool {
        self.no_candidates
    }

    pub const fn set_no_candidates(&mut self, value: bool) {
        self.no_candidates = value;
    }

    pub fn last_known_locations(&self) -> &[LocationIndex] {
        &self.last_known_locations
    }

    pub fn note_location(&mut self, index: LocationIndex) {
        if !self.last_known_locations.contains(&index) {
            self.last_known_locations.push(index);
        }
    }

    pub fn note_roster_read(&mut self, index: LocationIndex, at: Millis) {
        if let Some(stamp) = self
            .roster_stamps
            .iter_mut()
            .find(|(stamped, _)| *stamped == index)
        {
            stamp.1 = at;
            return;
        }
        if self.roster_stamps.len() >= MAX_ROSTER_STAMPS {
            let oldest = self
                .roster_stamps
                .iter()
                .enumerate()
                .min_by_key(|(_, (_, at))| at.get())
                .map(|(position, _)| position);
            if let Some(position) = oldest {
                self.roster_stamps.remove(position);
            }
        }
        self.roster_stamps.push((index, at));
    }

    pub fn roster_read_at(&self, index: LocationIndex) -> Option<Millis> {
        self.roster_stamps
            .iter()
            .find(|(stamped, _)| *stamped == index)
            .map(|(_, at)| *at)
    }

    pub fn retain_roster_stamps(&mut self, live: &[LocationIndex]) {
        self.roster_stamps.retain(|(index, _)| live.contains(index));
    }

    pub fn roster_stamps(&self) -> &[(LocationIndex, Millis)] {
        &self.roster_stamps
    }

    pub fn believed_homes(&self) -> Vec<LocationIndex> {
        let mut homes: Vec<LocationIndex> = self
            .connections
            .iter()
            .filter_map(ConnectionLedger::believed_home)
            .collect();
        homes.sort_by_key(|index| index.get());
        homes.dedup();
        homes
    }

    pub fn believed_home_counts(&self) -> Vec<(LocationIndex, u32)> {
        let mut counts: Vec<(LocationIndex, u32)> = Vec::new();
        for home in self
            .connections
            .iter()
            .filter_map(ConnectionLedger::believed_home)
        {
            match counts.iter_mut().find(|(index, _)| *index == home) {
                Some((_, count)) => *count = count.saturating_add(1),
                None => counts.push((home, 1)),
            }
        }
        counts.sort_by_key(|(index, _)| index.get());
        counts
    }

    pub fn prior_connections(&self) -> Vec<PriorConnection> {
        self.connections
            .iter()
            .map(|entry| PriorConnection {
                connection: entry.connection.clone(),
                was_consistent: matches!(entry.state, ConnectionState::Consistent),
            })
            .collect()
    }

    pub fn reset_corroborations(&mut self) {
        for entry in &mut self.connections {
            if entry.is_divergent() {
                entry.clear_evidence();
            }
        }
    }

    pub fn divergent_connections(&self) -> usize {
        self.connections
            .iter()
            .filter(|entry| entry.is_divergent())
            .count()
    }

    fn is_cold(&self) -> bool {
        self.connections.iter().all(|entry| {
            matches!(
                entry.state,
                ConnectionState::Consistent
                    | ConnectionState::Nascent
                    | ConnectionState::Retired { .. }
            )
        })
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct LedgerLimits {
    pub max_tracked_rooms: usize,
    pub max_tracked_connections: usize,
    pub max_connections_per_room: usize,
    pub memory_budget_bytes: u64,
    pub retired_grace_ms: u64,
}

impl Default for LedgerLimits {
    fn default() -> Self {
        Self {
            max_tracked_rooms: 8_192,
            max_tracked_connections: 65_536,
            max_connections_per_room: 512,
            memory_budget_bytes: 16_777_216,
            retired_grace_ms: 60_000,
        }
    }
}

pub const CONNECTION_ENTRY_BYTES: u64 =
    (size_of::<ConnectionLedger>() + CONNECTION_ID_HEAP_BYTES) as u64;
pub const ROOM_ENTRY_BYTES: u64 = (size_of::<RoomLedger>() + ROOM_OVERHEAD_BYTES) as u64;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AdmissionRefusal {
    RoomCapacity,
    ConnectionCapacity,
    RoomConnectionCapacity,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Ledger {
    rooms: BTreeMap<RoomKey, RoomLedger>,
    locations: LocationTable,
    limits: LedgerLimits,
    params: DecideParams,
    tracked_connections: usize,
    evicted_rooms_total: u64,
    evicted_connections_total: u64,
}

impl Ledger {
    pub fn new(limits: LedgerLimits, params: DecideParams) -> Self {
        Self {
            rooms: BTreeMap::new(),
            locations: LocationTable::new(),
            limits,
            params,
            tracked_connections: 0,
            evicted_rooms_total: 0,
            evicted_connections_total: 0,
        }
    }

    pub const fn limits(&self) -> LedgerLimits {
        self.limits
    }

    pub const fn params(&self) -> &DecideParams {
        &self.params
    }

    pub const fn locations(&self) -> &LocationTable {
        &self.locations
    }

    pub fn intern_location(&mut self, location: &Location) -> Option<LocationIndex> {
        self.locations.intern(location)
    }

    pub fn location(&self, index: LocationIndex) -> Option<&Location> {
        self.locations.get(index)
    }

    pub fn location_index(&self, location: &Location) -> Option<LocationIndex> {
        self.locations.index_of(location)
    }

    pub fn rooms(&self) -> impl Iterator<Item = (&RoomKey, &RoomLedger)> {
        self.rooms.iter()
    }

    pub fn room(&self, room: &RoomKey) -> Option<&RoomLedger> {
        self.rooms.get(room)
    }

    pub fn room_mut(&mut self, room: &RoomKey) -> Option<&mut RoomLedger> {
        self.rooms.get_mut(room)
    }

    pub fn connection(&self, key: &ConnectionKey) -> Option<&ConnectionLedger> {
        self.rooms
            .get(&key.room)
            .and_then(|room| room.connection(&key.connection))
    }

    pub fn connection_mut(&mut self, key: &ConnectionKey) -> Option<&mut ConnectionLedger> {
        self.rooms
            .get_mut(&key.room)
            .and_then(|room| room.connection_mut(&key.connection))
    }

    pub fn ensure_room(
        &mut self,
        room: RoomKey,
        now: Millis,
    ) -> Result<&mut RoomLedger, AdmissionRefusal> {
        if !self.rooms.contains_key(&room) && self.rooms.len() >= self.limits.max_tracked_rooms {
            return Err(AdmissionRefusal::RoomCapacity);
        }
        Ok(self
            .rooms
            .entry(room)
            .or_insert_with(|| RoomLedger::new(room, now)))
    }

    pub fn upsert_connection(
        &mut self,
        room: RoomKey,
        connection: &ConnectionId,
        user_id: UserId,
        now: Millis,
    ) -> Result<&mut ConnectionLedger, AdmissionRefusal> {
        let limits = self.limits;
        let tracked = self.tracked_connections;
        let room_entry = self.ensure_room(room, now)?;

        let existing = room_entry
            .connections
            .iter()
            .position(|entry| &entry.connection == connection);

        let index = match existing {
            Some(index) => index,
            None => {
                if room_entry.connections.len() >= limits.max_connections_per_room {
                    room_entry.partially_unreadable = true;
                    return Err(AdmissionRefusal::RoomConnectionCapacity);
                }
                if tracked >= limits.max_tracked_connections {
                    return Err(AdmissionRefusal::ConnectionCapacity);
                }
                room_entry.connections.push(ConnectionLedger::new(
                    connection.clone(),
                    user_id,
                    now,
                ));
                self.tracked_connections = tracked.saturating_add(1);
                self.rooms
                    .get(&room)
                    .map(|entry| entry.connections.len().saturating_sub(1))
                    .unwrap_or(0)
            }
        };

        let entry = self
            .rooms
            .get_mut(&room)
            .and_then(|room_ledger| room_ledger.connections.get_mut(index))
            .expect("the connection was just admitted");
        entry.user_id = user_id;
        entry.last_seen = now;
        Ok(entry)
    }

    pub fn tracked_rooms(&self) -> usize {
        self.rooms.len()
    }

    pub const fn tracked_connections(&self) -> usize {
        self.tracked_connections
    }

    pub fn tracked_bytes(&self) -> u64 {
        (self.rooms.len() as u64)
            .saturating_mul(ROOM_ENTRY_BYTES)
            .saturating_add(
                (self.tracked_connections as u64).saturating_mul(CONNECTION_ENTRY_BYTES),
            )
    }

    pub const fn evicted_rooms_total(&self) -> u64 {
        self.evicted_rooms_total
    }

    pub const fn evicted_connections_total(&self) -> u64 {
        self.evicted_connections_total
    }

    pub fn divergent_connections(&self) -> usize {
        self.rooms
            .values()
            .map(RoomLedger::divergent_connections)
            .sum()
    }

    pub fn divergent_fraction(&self) -> f64 {
        if self.tracked_connections == 0 {
            return 0.0;
        }
        self.divergent_connections() as f64 / self.tracked_connections as f64
    }

    pub fn reset_all_corroborations(&mut self) {
        for room in self.rooms.values_mut() {
            room.reset_corroborations();
        }
    }

    pub fn prune_retired(&mut self, now: Millis) -> usize {
        let grace = self.limits.retired_grace_ms;
        let mut dropped = 0;

        for room in self.rooms.values_mut() {
            room.connections.retain(|entry| match entry.state {
                ConnectionState::Retired { at } => {
                    let expired = now.saturating_since(at) >= grace;
                    if expired {
                        dropped += 1;
                    }
                    !expired
                }
                ConnectionState::Nascent
                | ConnectionState::Consistent
                | ConnectionState::PendingJoin { .. }
                | ConnectionState::GatewayOnly { .. }
                | ConnectionState::MediaOnly { .. }
                | ConnectionState::Repairing { .. }
                | ConnectionState::ActionTaken { .. }
                | ConnectionState::Wedged { .. } => true,
            });
        }

        self.tracked_connections = self.tracked_connections.saturating_sub(dropped);
        self.evicted_connections_total = self
            .evicted_connections_total
            .saturating_add(dropped as u64);
        dropped
    }

    pub fn drop_empty_rooms(&mut self) -> usize {
        let before = self.rooms.len();
        self.rooms.retain(|_, room| !room.connections.is_empty());
        before.saturating_sub(self.rooms.len())
    }

    pub fn evict_to_budget(&mut self) -> usize {
        let mut evicted = 0;

        while self.tracked_bytes() > self.limits.memory_budget_bytes
            || self.rooms.len() > self.limits.max_tracked_rooms
            || self.tracked_connections > self.limits.max_tracked_connections
        {
            let Some(victim) = self.coldest_room() else {
                break;
            };
            if self.evict_room(&victim) {
                evicted += 1;
            } else {
                break;
            }
        }

        evicted
    }

    pub fn evict_room(&mut self, room: &RoomKey) -> bool {
        match self.rooms.remove(room) {
            None => false,
            Some(entry) => {
                self.tracked_connections = self
                    .tracked_connections
                    .saturating_sub(entry.connections.len());
                self.evicted_connections_total = self
                    .evicted_connections_total
                    .saturating_add(entry.connections.len() as u64);
                self.evicted_rooms_total = self.evicted_rooms_total.saturating_add(1);
                true
            }
        }
    }

    pub fn coldest_room(&self) -> Option<RoomKey> {
        let cold = self
            .rooms
            .values()
            .filter(|room| room.is_cold())
            .min_by_key(|room| (room.last_seen(), room.room()))
            .map(RoomLedger::room);

        cold.or_else(|| {
            self.rooms
                .values()
                .min_by_key(|room| (room.last_seen(), room.room()))
                .map(RoomLedger::room)
        })
    }

    pub fn record_action_issued(
        &mut self,
        key: &ConnectionKey,
        kind: ActionKind,
        at: Millis,
        authorizing_turn: TurnId,
    ) -> bool {
        let Some(entry) = self.connection_mut(key) else {
            return false;
        };

        let (prior, since) = match &entry.state {
            ConnectionState::GatewayOnly { since, .. } => (PriorDivergence::GatewayOnly, *since),
            ConnectionState::MediaOnly { since, .. } => (PriorDivergence::MediaOnly, *since),
            ConnectionState::Nascent
            | ConnectionState::Consistent
            | ConnectionState::PendingJoin { .. }
            | ConnectionState::Repairing { .. }
            | ConnectionState::ActionTaken { .. }
            | ConnectionState::Wedged { .. }
            | ConnectionState::Retired { .. } => return false,
        };

        entry.action_attempts = entry.action_attempts.saturating_add(1);
        entry.last_action_at = Some(at);
        entry.state = ConnectionState::ActionTaken {
            kind,
            at,
            authorizing_turn,
            prior,
            since,
        };
        true
    }

    pub fn refund_action_attempt(&mut self, key: &ConnectionKey) -> bool {
        let Some(entry) = self.connection_mut(key) else {
            return false;
        };
        if entry.action_attempts == 0 {
            return false;
        }
        entry.action_attempts = entry.action_attempts.saturating_sub(1);
        true
    }

    pub fn record_repair_issued(&mut self, key: &ConnectionKey, at: Millis) -> bool {
        let Some(entry) = self.connection_mut(key) else {
            return false;
        };
        entry.repair_attempts = entry.repair_attempts.saturating_add(1);
        entry.last_action_at = Some(at);
        if let ConnectionState::Repairing { last_attempt, .. } = &mut entry.state {
            *last_attempt = at;
        }
        true
    }

    pub fn record_repair_verdict(&mut self, key: &ConnectionKey, verdict: RepairVerdict) -> bool {
        let Some(entry) = self.connection_mut(key) else {
            return false;
        };
        entry.last_repair_verdict = Some(verdict);
        true
    }

    pub fn record_confirm_issued(&mut self, key: &ConnectionKey, at: Millis) -> bool {
        let Some(entry) = self.connection_mut(key) else {
            return false;
        };
        entry.last_action_at = Some(at);
        if let ConnectionState::PendingJoin { last_confirm, .. } = &mut entry.state {
            *last_confirm = at;
        }
        true
    }
}
