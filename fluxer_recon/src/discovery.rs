// SPDX-License-Identifier: AGPL-3.0-or-later

use std::collections::{BTreeMap, BTreeSet};

use crate::census::CensusSnapshot;
use crate::clock::Clock;
use crate::config::ReconConfig;
use crate::evidence::{
    CandidateSet, CandidateSources, CensusCrossCheck, GatewayAuthorityInput, GatewayRead,
    GatewayVoiceState, LocationProbe, MediaAuthorityInput, MediaParticipant, PendingJoin,
    ProbeResult, RoomObservation, RoomReads, ServerCliffConfig, ServerCliffDetector, Side,
    SideAuthority, TopologyFreshness, UnknownReason, gateway_authority, media_authority,
    unread_believed_home,
};
use crate::gateway::GatewayApi;
use crate::health::{ProbeOutcome, ServerHealth, ServerHealthMap};
use crate::ids::{ConnectionId, Epoch, Location, Millis, RoomKey, TurnId, WallMillis};
use crate::ledger::{LOCATION_ENTRY_BYTES, LOCATION_MAP_BYTES, MAP_ENTRY_OVERHEAD_BYTES};
use crate::livekit::{LiveKitApi, LiveKitFault, ParticipantRecord, ReadResult};
use crate::names::{parse_participant_identity, parse_room_name};
use crate::turn::{Digest, Fresh, ReadSource, TurnError, TurnToken};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum RoomSource {
    GatewayCensus,
    MediaRoomList,
    PinnedServer,
    Ledger,
    Suspicion,
}

impl RoomSource {
    pub const ALL: [Self; 5] = [
        Self::GatewayCensus,
        Self::MediaRoomList,
        Self::PinnedServer,
        Self::Ledger,
        Self::Suspicion,
    ];

    pub const fn label(self) -> &'static str {
        match self {
            Self::GatewayCensus => "gateway_census",
            Self::MediaRoomList => "media_room_list",
            Self::PinnedServer => "pinned_server",
            Self::Ledger => "ledger",
            Self::Suspicion => "suspicion",
        }
    }

    pub const fn index(self) -> usize {
        match self {
            Self::GatewayCensus => 0,
            Self::MediaRoomList => 1,
            Self::PinnedServer => 2,
            Self::Ledger => 3,
            Self::Suspicion => 4,
        }
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct RoomProvenance {
    seen: [Option<Millis>; RoomSource::ALL.len()],
}

impl RoomProvenance {
    pub const fn new() -> Self {
        Self {
            seen: [None; RoomSource::ALL.len()],
        }
    }

    const fn note(&mut self, source: RoomSource, now: Millis) {
        self.seen[source.index()] = Some(now);
    }

    pub const fn has(&self, source: RoomSource) -> bool {
        self.seen[source.index()].is_some()
    }

    pub const fn last_seen_by(&self, source: RoomSource) -> Option<Millis> {
        self.seen[source.index()]
    }

    pub fn count(&self) -> usize {
        self.seen.iter().filter(|entry| entry.is_some()).count()
    }

    pub fn sources(&self) -> Vec<RoomSource> {
        RoomSource::ALL
            .into_iter()
            .filter(|source| self.has(*source))
            .collect()
    }

    pub fn only(&self, source: RoomSource) -> bool {
        self.has(source) && self.count() == 1
    }

    pub const fn is_gateway_only(&self) -> bool {
        self.has(RoomSource::GatewayCensus) && !self.has(RoomSource::MediaRoomList)
    }

    pub const fn is_media_only(&self) -> bool {
        self.has(RoomSource::MediaRoomList) && !self.has(RoomSource::GatewayCensus)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Admission {
    Admitted,
    Refreshed,
    Refused,
}

impl Admission {
    pub const fn label(self) -> &'static str {
        match self {
            Self::Admitted => "admitted",
            Self::Refreshed => "refreshed",
            Self::Refused => "refused",
        }
    }

    pub const fn is_tracked(self) -> bool {
        matches!(self, Self::Admitted | Self::Refreshed)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct DirectoryLimits {
    pub max_rooms: usize,
}

impl DirectoryLimits {
    pub const fn from_config(config: &ReconConfig) -> Self {
        Self {
            max_rooms: config.max_tracked_rooms,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct RoomEntry {
    provenance: RoomProvenance,
    locations: BTreeMap<Location, Millis>,
    pinned: Option<Location>,
    first_at: Millis,
    last_at: Millis,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ServerRoomList {
    Listed {
        rooms: Vec<RoomKey>,
        unparseable: usize,
    },
    Unreadable(LiveKitFault),
}

impl ServerRoomList {
    pub const fn is_readable(&self) -> bool {
        matches!(self, Self::Listed { .. })
    }

    pub const fn is_complete(&self) -> bool {
        matches!(self, Self::Listed { unparseable: 0, .. })
    }

    pub fn rooms(&self) -> &[RoomKey] {
        match self {
            Self::Listed { rooms, .. } => rooms,
            Self::Unreadable(_) => &[],
        }
    }

    pub const fn unparseable(&self) -> usize {
        match self {
            Self::Listed { unparseable, .. } => *unparseable,
            Self::Unreadable(_) => 0,
        }
    }

    pub const fn fault(&self) -> Option<LiveKitFault> {
        match self {
            Self::Listed { .. } => None,
            Self::Unreadable(fault) => Some(*fault),
        }
    }

    pub const fn outcome(&self) -> ProbeOutcome {
        match self {
            Self::Listed { .. } => ProbeOutcome::Success,
            Self::Unreadable(fault) => {
                if fault.is_auth_failure() {
                    ProbeOutcome::AuthFailure
                } else {
                    ProbeOutcome::Failure
                }
            }
        }
    }

    pub const fn label(&self) -> &'static str {
        match self {
            Self::Listed { .. } => "listed",
            Self::Unreadable(_) => "unreadable",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RoomListEffect {
    Applied {
        listed: usize,
        added: usize,
        withdrawn: usize,
    },
    AdditiveOnly {
        listed: usize,
        added: usize,
        unparseable: usize,
    },
    Unreadable(LiveKitFault),
}

impl RoomListEffect {
    pub const fn label(self) -> &'static str {
        match self {
            Self::Applied { .. } => "applied",
            Self::AdditiveOnly { .. } => "additive_only",
            Self::Unreadable(_) => "unreadable",
        }
    }

    pub const fn withdrew_locations(self) -> bool {
        matches!(self, Self::Applied { .. })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DiscoveryPass {
    pub at: Millis,
    pub servers: usize,
    pub readable: usize,
    pub complete: usize,
    pub rooms_added: usize,
    pub locations_withdrawn: usize,
    pub unreadable: Vec<(Location, LiveKitFault)>,
}

impl DiscoveryPass {
    pub const fn empty(at: Millis) -> Self {
        Self {
            at,
            servers: 0,
            readable: 0,
            complete: 0,
            rooms_added: 0,
            locations_withdrawn: 0,
            unreadable: Vec::new(),
        }
    }

    pub fn is_complete(&self) -> bool {
        self.servers > 0 && self.complete == self.servers
    }

    pub fn unreadable_servers(&self) -> usize {
        self.unreadable.len()
    }
}

#[derive(Clone, Debug)]
pub struct RoomDirectory {
    limits: DirectoryLimits,
    rooms: BTreeMap<RoomKey, RoomEntry>,
    listings: BTreeMap<Location, Millis>,
    relocated: Vec<RoomKey>,
    admitted_total: u64,
    refused_total: u64,
    forgotten_total: u64,
    unreadable_lists_total: u64,
    incomplete_lists_total: u64,
    withdrawn_total: u64,
}

pub const DIRECTORY_ENTRY_BYTES: u64 =
    (size_of::<RoomKey>() + size_of::<RoomEntry>() + MAP_ENTRY_OVERHEAD_BYTES) as u64;

impl RoomDirectory {
    pub fn tracked_bytes(&self) -> u64 {
        self.rooms
            .values()
            .map(|entry| {
                let map = if entry.locations.is_empty() {
                    0
                } else {
                    LOCATION_MAP_BYTES
                };
                DIRECTORY_ENTRY_BYTES.saturating_add(map).saturating_add(
                    (entry.locations.len() as u64).saturating_mul(LOCATION_ENTRY_BYTES),
                )
            })
            .fold(0u64, u64::saturating_add)
            .saturating_add((self.listings.len() as u64).saturating_mul(LOCATION_ENTRY_BYTES))
    }

    pub const fn new(limits: DirectoryLimits) -> Self {
        Self {
            limits,
            rooms: BTreeMap::new(),
            listings: BTreeMap::new(),
            relocated: Vec::new(),
            admitted_total: 0,
            refused_total: 0,
            forgotten_total: 0,
            unreadable_lists_total: 0,
            incomplete_lists_total: 0,
            withdrawn_total: 0,
        }
    }

    pub const fn limits(&self) -> DirectoryLimits {
        self.limits
    }

    pub fn len(&self) -> usize {
        self.rooms.len()
    }

    pub fn is_empty(&self) -> bool {
        self.rooms.is_empty()
    }

    pub const fn admitted_total(&self) -> u64 {
        self.admitted_total
    }

    pub const fn refused_total(&self) -> u64 {
        self.refused_total
    }

    pub const fn forgotten_total(&self) -> u64 {
        self.forgotten_total
    }

    pub const fn unreadable_lists_total(&self) -> u64 {
        self.unreadable_lists_total
    }

    pub const fn incomplete_lists_total(&self) -> u64 {
        self.incomplete_lists_total
    }

    pub const fn withdrawn_total(&self) -> u64 {
        self.withdrawn_total
    }

    pub fn rooms(&self) -> impl Iterator<Item = (RoomKey, &RoomProvenance)> {
        self.rooms
            .iter()
            .map(|(room, entry)| (*room, &entry.provenance))
    }

    pub fn room_keys(&self) -> Vec<RoomKey> {
        self.rooms.keys().copied().collect()
    }

    pub fn contains(&self, room: &RoomKey) -> bool {
        self.rooms.contains_key(room)
    }

    pub fn provenance(&self, room: &RoomKey) -> Option<&RoomProvenance> {
        self.rooms.get(room).map(|entry| &entry.provenance)
    }

    pub fn known_only_to(&self, source: RoomSource) -> Vec<RoomKey> {
        self.rooms
            .iter()
            .filter(|(_, entry)| entry.provenance.only(source))
            .map(|(room, _)| *room)
            .collect()
    }

    pub fn gateway_only_rooms(&self) -> Vec<RoomKey> {
        self.rooms
            .iter()
            .filter(|(_, entry)| entry.provenance.is_gateway_only())
            .map(|(room, _)| *room)
            .collect()
    }

    pub fn media_only_rooms(&self) -> Vec<RoomKey> {
        self.rooms
            .iter()
            .filter(|(_, entry)| entry.provenance.is_media_only())
            .map(|(room, _)| *room)
            .collect()
    }

    pub fn listed_completely_at(&self, location: &Location) -> Option<Millis> {
        self.listings.get(location).copied()
    }

    pub fn room_is_absent_from(&self, location: &Location, room: &RoomKey) -> bool {
        self.listings.contains_key(location)
            && !self
                .rooms
                .get(room)
                .is_some_and(|entry| entry.locations.contains_key(location))
    }

    pub fn forget_location(&mut self, location: &Location) {
        self.listings.remove(location);
    }

    pub fn locations_for(&self, room: &RoomKey) -> Vec<Location> {
        self.rooms
            .get(room)
            .map(|entry| entry.locations.keys().cloned().collect())
            .unwrap_or_default()
    }

    pub fn pinned_for(&self, room: &RoomKey) -> Option<Location> {
        self.rooms.get(room).and_then(|entry| entry.pinned.clone())
    }

    pub fn first_seen_at(&self, room: &RoomKey) -> Option<Millis> {
        self.rooms.get(room).map(|entry| entry.first_at)
    }

    pub fn last_seen_at(&self, room: &RoomKey) -> Option<Millis> {
        self.rooms.get(room).map(|entry| entry.last_at)
    }

    pub fn note(&mut self, room: RoomKey, source: RoomSource, now: Millis) -> Admission {
        if let Some(entry) = self.rooms.get_mut(&room) {
            entry.provenance.note(source, now);
            entry.last_at = now;
            return Admission::Refreshed;
        }
        if self.rooms.len() >= self.limits.max_rooms {
            self.refused_total = self.refused_total.saturating_add(1);
            return Admission::Refused;
        }
        let mut provenance = RoomProvenance::new();
        provenance.note(source, now);
        self.rooms.insert(
            room,
            RoomEntry {
                provenance,
                locations: BTreeMap::new(),
                pinned: None,
                first_at: now,
                last_at: now,
            },
        );
        self.admitted_total = self.admitted_total.saturating_add(1);
        Admission::Admitted
    }

    pub fn note_census(&mut self, snapshot: &CensusSnapshot, now: Millis) -> Vec<RoomKey> {
        snapshot
            .room_keys()
            .into_iter()
            .filter(|room| self.note(*room, RoomSource::GatewayCensus, now) == Admission::Admitted)
            .collect()
    }

    pub fn drain_relocated(&mut self) -> Vec<RoomKey> {
        let mut moved = std::mem::take(&mut self.relocated);
        moved.sort_unstable();
        moved.dedup();
        moved
    }

    pub fn note_pinned(&mut self, room: RoomKey, location: Location, now: Millis) -> Admission {
        let admission = self.note(room, RoomSource::PinnedServer, now);
        if admission.is_tracked()
            && let Some(entry) = self.rooms.get_mut(&room)
        {
            entry.locations.insert(location.clone(), now);
            entry.pinned = Some(location);
        }
        admission
    }

    pub fn note_ledger(&mut self, room: RoomKey, now: Millis) -> Admission {
        self.note(room, RoomSource::Ledger, now)
    }

    pub fn note_suspicion(&mut self, room: RoomKey, now: Millis) -> Admission {
        self.note(room, RoomSource::Suspicion, now)
    }

    pub fn apply_room_list(
        &mut self,
        location: &Location,
        list: &ServerRoomList,
        now: Millis,
    ) -> RoomListEffect {
        let (rooms, unparseable) = match list {
            ServerRoomList::Unreadable(fault) => {
                self.unreadable_lists_total = self.unreadable_lists_total.saturating_add(1);
                self.listings.remove(location);
                return RoomListEffect::Unreadable(*fault);
            }
            ServerRoomList::Listed { rooms, unparseable } => (rooms, *unparseable),
        };

        let listed: BTreeSet<RoomKey> = rooms.iter().copied().collect();
        let mut added = 0usize;
        for room in &listed {
            if self.note(*room, RoomSource::MediaRoomList, now) == Admission::Admitted {
                added = added.saturating_add(1);
            }
            if let Some(entry) = self.rooms.get_mut(room) {
                let elsewhere =
                    !entry.locations.is_empty() && !entry.locations.contains_key(location);
                entry.locations.insert(location.clone(), now);
                if elsewhere {
                    self.relocated.push(*room);
                }
            }
        }

        if unparseable > 0 {
            self.incomplete_lists_total = self.incomplete_lists_total.saturating_add(1);
            self.listings.remove(location);
            return RoomListEffect::AdditiveOnly {
                listed: listed.len(),
                added,
                unparseable,
            };
        }

        let mut withdrawn = 0usize;
        for (room, entry) in &mut self.rooms {
            if listed.contains(room) {
                continue;
            }
            if entry.pinned.as_ref() == Some(location) {
                continue;
            }
            if entry.locations.remove(location).is_some() {
                withdrawn = withdrawn.saturating_add(1);
            }
        }
        self.withdrawn_total = self.withdrawn_total.saturating_add(withdrawn as u64);
        self.listings.insert(location.clone(), now);

        RoomListEffect::Applied {
            listed: listed.len(),
            added,
            withdrawn,
        }
    }

    pub fn apply_pass(
        &mut self,
        results: &[(Location, ServerRoomList)],
        now: Millis,
    ) -> DiscoveryPass {
        let mut pass = DiscoveryPass::empty(now);
        pass.servers = results.len();

        for (location, list) in results {
            match self.apply_room_list(location, list, now) {
                RoomListEffect::Unreadable(fault) => {
                    pass.unreadable.push((location.clone(), fault));
                }
                RoomListEffect::AdditiveOnly { added, .. } => {
                    pass.readable = pass.readable.saturating_add(1);
                    pass.rooms_added = pass.rooms_added.saturating_add(added);
                }
                RoomListEffect::Applied {
                    added, withdrawn, ..
                } => {
                    pass.readable = pass.readable.saturating_add(1);
                    pass.complete = pass.complete.saturating_add(1);
                    pass.rooms_added = pass.rooms_added.saturating_add(added);
                    pass.locations_withdrawn = pass.locations_withdrawn.saturating_add(withdrawn);
                }
            }
        }

        pass
    }

    pub fn sources_for(
        &self,
        room: &RoomKey,
        gateway_hints: Vec<Location>,
        ledger_last_known: Vec<Location>,
    ) -> CandidateSources {
        CandidateSources {
            discovered: self.locations_for(room),
            gateway_hints,
            pinned: self.pinned_for(room),
            ledger_last_known,
        }
    }

    pub fn forget(&mut self, room: &RoomKey) -> bool {
        let removed = self.rooms.remove(room).is_some();
        if removed {
            self.forgotten_total = self.forgotten_total.saturating_add(1);
        }
        removed
    }

    pub fn retain<F>(&mut self, mut keep: F)
    where
        F: FnMut(&RoomKey, &RoomProvenance) -> bool,
    {
        let dropped: Vec<RoomKey> = self
            .rooms
            .iter()
            .filter(|(room, entry)| !keep(room, &entry.provenance))
            .map(|(room, _)| *room)
            .collect();
        for room in dropped {
            self.forget(&room);
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ReadClock {
    pub now: Millis,
    pub wall_now: WallMillis,
}

impl ReadClock {
    pub fn from_clock(clock: &dyn Clock) -> Self {
        Self {
            now: clock.now(),
            wall_now: clock.wall_now(),
        }
    }

    pub const fn join_instant(&self, joined_at_unix_seconds: u64) -> Millis {
        if joined_at_unix_seconds == 0 {
            return self.now;
        }
        let joined_wall = WallMillis::new(joined_at_unix_seconds.saturating_mul(1_000));
        let age_ms = self.wall_now.saturating_since(joined_wall);
        Millis::new(self.now.get().saturating_sub(age_ms))
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct RoomRoster {
    participants: Vec<MediaParticipant>,
    unparseable_identities: usize,
}

impl RoomRoster {
    pub fn participants(&self) -> &[MediaParticipant] {
        &self.participants
    }

    pub const fn unparseable_identities(&self) -> usize {
        self.unparseable_identities
    }

    pub fn len(&self) -> usize {
        self.participants.len()
    }

    pub fn is_empty(&self) -> bool {
        self.participants.is_empty()
    }

    pub fn connections(&self) -> Vec<ConnectionId> {
        let mut connections: Vec<ConnectionId> = self
            .participants
            .iter()
            .map(|participant| participant.connection.clone())
            .collect();
        connections.sort();
        connections.dedup();
        connections
    }

    fn digest(&self) -> u64 {
        let mut digest = Digest::new().number(self.unparseable_identities as u64);
        for participant in &self.participants {
            digest = digest
                .text(participant.connection.as_str())
                .number(participant.user_id.get())
                .number(participant.joined_at.get())
                .flag(participant.is_publisher);
        }
        digest.finish()
    }
}

pub fn roster_from_records(
    records: &[ParticipantRecord],
    location: &Location,
    clock: ReadClock,
) -> RoomRoster {
    let mut participants: Vec<MediaParticipant> = Vec::new();
    let mut unparseable_identities = 0usize;

    for record in records {
        match parse_participant_identity(&record.identity) {
            Err(_) => unparseable_identities = unparseable_identities.saturating_add(1),
            Ok(identity) => participants.push(MediaParticipant {
                connection: identity.connection_id,
                user_id: identity.user_id,
                location: location.clone(),
                joined_at: clock.join_instant(record.joined_at_unix_seconds),
                is_publisher: record.is_publisher,
            }),
        }
    }

    RoomRoster {
        participants,
        unparseable_identities,
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LocationReadout {
    location: Location,
    roster: ReadResult<RoomRoster>,
    at: Millis,
}

impl LocationReadout {
    pub const fn read(location: Location, roster: RoomRoster, at: Millis) -> Self {
        Self {
            location,
            roster: ReadResult::Read(roster),
            at,
        }
    }

    pub const fn unreadable(location: Location, fault: LiveKitFault, at: Millis) -> Self {
        Self {
            location,
            roster: ReadResult::Unreadable(fault),
            at,
        }
    }

    pub const fn location(&self) -> &Location {
        &self.location
    }

    pub const fn at(&self) -> Millis {
        self.at
    }

    pub fn is_readable(&self) -> bool {
        self.roster.is_readable()
    }

    pub fn roster(&self) -> Option<&RoomRoster> {
        self.roster.as_read()
    }

    pub fn fault(&self) -> Option<LiveKitFault> {
        self.roster.fault()
    }

    pub fn participants_seen(&self) -> u32 {
        self.roster
            .as_read()
            .map_or(0, |roster| u32::try_from(roster.len()).unwrap_or(u32::MAX))
    }

    pub fn outcome(&self) -> ProbeOutcome {
        match &self.roster {
            ReadResult::Read(_) => ProbeOutcome::Success,
            ReadResult::Unreadable(fault) if fault.is_auth_failure() => ProbeOutcome::AuthFailure,
            ReadResult::Unreadable(_) => ProbeOutcome::Failure,
        }
    }

    pub fn probe(&self, health: ServerHealth, cliffed: bool) -> LocationProbe {
        let result = match &self.roster {
            ReadResult::Unreadable(fault) => ProbeResult::Failed(*fault),
            ReadResult::Read(roster) => ProbeResult::Ok {
                participants: roster.participants.clone(),
                unparseable_identities: roster.unparseable_identities,
            },
        };
        LocationProbe {
            location: self.location.clone(),
            result,
            health,
            cliffed,
        }
    }

    fn digest(&self) -> u64 {
        let digest = Digest::new()
            .text(self.location.region.as_str())
            .text(self.location.server.as_str());
        match &self.roster {
            ReadResult::Unreadable(fault) => digest.text("unreadable").text(fault.label()).finish(),
            ReadResult::Read(roster) => digest.text("read").number(roster.digest()).finish(),
        }
    }
}

pub fn record_health(readouts: &[LocationReadout], health: &mut ServerHealthMap, now: Millis) {
    for readout in readouts {
        health.record(&readout.location, readout.outcome(), now);
    }
}

fn digest_gateway_read(read: &GatewayRead) -> u64 {
    match read {
        GatewayRead::Failed(fault) => Digest::new().text("failed").text(fault.label()).finish(),
        GatewayRead::Ok { states } => {
            let mut digest = Digest::new().text("ok");
            for state in states {
                digest = digest
                    .text(state.connection.as_ref().map_or("", ConnectionId::as_str))
                    .flag(state.connection.is_some())
                    .number(state.user_id.get())
                    .number(state.channel_id.get());
            }
            digest.finish()
        }
    }
}

fn digest_pending_joins(joins: &[PendingJoin]) -> u64 {
    let mut digest = Digest::new().text("pending_joins");
    for join in joins {
        digest = digest
            .text(join.connection.as_str())
            .number(join.user_id.get())
            .number(join.expires_at.get());
    }
    digest.finish()
}

pub async fn list_server_rooms<L>(livekit: &L, location: &Location) -> ServerRoomList
where
    L: LiveKitApi,
{
    match livekit.list_rooms(location).await {
        ReadResult::Unreadable(fault) => ServerRoomList::Unreadable(fault),
        ReadResult::Read(names) => {
            let mut rooms: Vec<RoomKey> = Vec::new();
            let mut unparseable = 0usize;
            for name in &names {
                match parse_room_name(name) {
                    Err(_) => unparseable = unparseable.saturating_add(1),
                    Ok(room) => rooms.push(room),
                }
            }
            rooms.sort();
            rooms.dedup();
            ServerRoomList::Listed { rooms, unparseable }
        }
    }
}

pub async fn list_fleet_rooms<L, F>(
    livekit: &L,
    locations: &[Location],
    mut admit: F,
) -> Vec<(Location, ServerRoomList)>
where
    L: LiveKitApi,
    F: FnMut(&Location) -> bool,
{
    let mut results: Vec<(Location, ServerRoomList)> = Vec::new();
    for location in locations {
        if !admit(location) {
            continue;
        }
        let list = list_server_rooms(livekit, location).await;
        results.push((location.clone(), list));
    }
    results
}

pub async fn read_location_roster<L>(
    livekit: &L,
    location: &Location,
    room: RoomKey,
    clock: ReadClock,
) -> LocationReadout
where
    L: LiveKitApi,
{
    match livekit.list_participants(location, room).await {
        ReadResult::Unreadable(fault) => {
            LocationReadout::unreadable(location.clone(), fault, clock.now)
        }
        ReadResult::Read(records) => LocationReadout::read(
            location.clone(),
            roster_from_records(&records, location, clock),
            clock.now,
        ),
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct CliffVerdict {
    pub fraction: bool,
    pub disputed: bool,
}

impl CliffVerdict {
    pub const fn is_cliffed(self) -> bool {
        self.fraction || self.disputed
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ServerPass {
    pub location: Location,
    pub participants_seen: u32,
    pub expected_participants: u32,
    pub expectations_accounted: bool,
    pub coverage_complete: bool,
}

impl ServerPass {
    pub const fn new(
        location: Location,
        participants_seen: u32,
        expected_participants: u32,
    ) -> Self {
        Self {
            location,
            participants_seen,
            expected_participants,
            expectations_accounted: false,
            coverage_complete: true,
        }
    }

    #[must_use]
    pub const fn accounted(mut self) -> Self {
        self.expectations_accounted = true;
        self
    }

    pub const fn says_nobody_is_expected(&self) -> bool {
        self.expectations_accounted && self.expected_participants == 0
    }

    #[must_use]
    pub const fn partially_covered(mut self) -> Self {
        self.coverage_complete = false;
        self
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ExpectationSource {
    View,
    Ledger,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct PassCounters {
    seen: BTreeMap<Location, u32>,
    expected: BTreeMap<Location, u32>,
    seen_by_room: BTreeMap<(Location, RoomKey), u32>,
    expected_by_room: BTreeMap<(Location, RoomKey, ExpectationSource), u32>,
    expectations_accounted: bool,
    skipped: BTreeSet<Location>,
}

impl PassCounters {
    pub const fn new() -> Self {
        Self {
            seen: BTreeMap::new(),
            expected: BTreeMap::new(),
            seen_by_room: BTreeMap::new(),
            expected_by_room: BTreeMap::new(),
            expectations_accounted: false,
            skipped: BTreeSet::new(),
        }
    }

    pub const fn note_expectations_accounted(&mut self) {
        self.expectations_accounted = true;
    }

    pub const fn expectations_accounted(&self) -> bool {
        self.expectations_accounted
    }

    pub fn note_skipped(&mut self, location: &Location) {
        self.skipped.insert(location.clone());
    }

    pub fn is_fully_covered(&self, location: &Location) -> bool {
        !self.skipped.contains(location)
    }

    pub fn note_seen(&mut self, location: &Location, count: u32) {
        let entry = self.seen.entry(location.clone()).or_insert(0);
        *entry = entry.saturating_add(count);
    }

    pub fn note_expected(&mut self, location: &Location, count: u32) {
        let entry = self.expected.entry(location.clone()).or_insert(0);
        *entry = entry.saturating_add(count);
    }

    pub fn note_room_seen(&mut self, location: &Location, room: RoomKey, count: u32) {
        let previous = self
            .seen_by_room
            .insert((location.clone(), room), count)
            .unwrap_or(0);
        let entry = self.seen.entry(location.clone()).or_insert(0);
        *entry = entry.saturating_sub(previous).saturating_add(count);
    }

    pub fn note_room_expected(
        &mut self,
        location: &Location,
        room: RoomKey,
        source: ExpectationSource,
        count: u32,
    ) {
        let previous = self
            .expected_by_room
            .insert((location.clone(), room, source), count)
            .unwrap_or(0);
        let entry = self.expected.entry(location.clone()).or_insert(0);
        *entry = entry.saturating_sub(previous).saturating_add(count);
    }

    pub fn note_view(&mut self, view: &RoomView) {
        let room = view.observation().room;
        let readable = view.readable_locations();
        for readout in view.readouts() {
            if !readout.is_readable() {
                continue;
            }
            self.note_room_seen(readout.location(), room, readout.participants_seen());
        }
        for location in view.observation().candidates.locations() {
            if !readable.contains(location) {
                self.note_skipped(location);
            }
        }
        for (location, expected) in view.expectations() {
            self.note_room_expected(location, room, ExpectationSource::View, *expected);
        }
    }

    pub fn seen(&self, location: &Location) -> u32 {
        self.seen.get(location).copied().unwrap_or(0)
    }

    pub fn expected(&self, location: &Location) -> u32 {
        self.expected.get(location).copied().unwrap_or(0)
    }

    pub fn locations(&self) -> Vec<Location> {
        let mut locations: Vec<Location> = self
            .seen
            .keys()
            .chain(self.expected.keys())
            .cloned()
            .collect();
        locations.sort();
        locations.dedup();
        locations
    }

    pub fn passes(&self) -> Vec<ServerPass> {
        self.seen
            .iter()
            .map(|(location, participants_seen)| ServerPass {
                location: location.clone(),
                participants_seen: *participants_seen,
                expected_participants: self.expected(location),
                expectations_accounted: self.expectations_accounted,
                coverage_complete: self.is_fully_covered(location),
            })
            .collect()
    }

    pub fn is_empty(&self) -> bool {
        self.seen.is_empty() && self.expected.is_empty()
    }

    pub fn clear(&mut self) {
        self.seen.clear();
        self.expected.clear();
        self.seen_by_room.clear();
        self.expected_by_room.clear();
        self.expectations_accounted = false;
        self.skipped.clear();
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
struct Watch {
    detector: ServerCliffDetector,
    baseline: u32,
    last_nonempty_at: Option<Millis>,
    dispute_since: Option<Millis>,
    expected_at_dispute: u32,
}

#[derive(Clone, Debug)]
pub struct ServerCliffWatch {
    config: ServerCliffConfig,
    watches: BTreeMap<Location, Watch>,
    tripped_total: u64,
    disputed_total: u64,
    released_total: u64,
}

impl ServerCliffWatch {
    pub const fn new(config: ServerCliffConfig) -> Self {
        Self {
            config,
            watches: BTreeMap::new(),
            tripped_total: 0,
            disputed_total: 0,
            released_total: 0,
        }
    }

    pub fn from_config(config: &ReconConfig) -> Self {
        Self::new(ServerCliffConfig {
            fraction: config.server_cliff_fraction,
            hold_ms: config.server_cliff_hold_ms,
        })
    }

    pub const fn config(&self) -> ServerCliffConfig {
        self.config
    }

    pub const fn tripped_total(&self) -> u64 {
        self.tripped_total
    }

    pub const fn disputed_total(&self) -> u64 {
        self.disputed_total
    }

    pub const fn released_total(&self) -> u64 {
        self.released_total
    }

    pub fn tracked(&self) -> usize {
        self.watches.len()
    }

    pub fn observe(&mut self, pass: &ServerPass, now: Millis) -> CliffVerdict {
        let config = self.config;
        let watch = self.watches.entry(pass.location.clone()).or_default();

        let fraction = if pass.coverage_complete {
            watch.detector.observe(pass.participants_seen, now, config)
        } else {
            watch.detector.is_cliffed(now, config)
        };
        let mut armed = false;
        let mut released = false;

        if pass.participants_seen > 0 {
            watch.baseline = pass.participants_seen;
            watch.last_nonempty_at = Some(now);
            watch.expected_at_dispute = 0;
            released = watch.dispute_since.take().is_some();
        } else if watch.baseline > 0 || pass.expected_participants > 0 {
            if watch.dispute_since.is_none() {
                watch.dispute_since = Some(now);
                armed = true;
            }
            watch.expected_at_dispute = pass.expected_participants;
            if let Some(since) = watch.dispute_since
                && pass.says_nobody_is_expected()
                && now.saturating_since(since) >= config.hold_ms
            {
                watch.dispute_since = None;
                watch.expected_at_dispute = 0;
                released = true;
            }
        }

        if pass.coverage_complete
            && pass.says_nobody_is_expected()
            && let Some(since) = watch.detector.cliffed_since()
            && now.saturating_since(since) >= config.hold_ms
        {
            watch.detector.release();
        }

        let disputed = watch.dispute_since.is_some();
        if armed {
            self.disputed_total = self.disputed_total.saturating_add(1);
        }
        if released {
            self.released_total = self.released_total.saturating_add(1);
        }
        if fraction {
            self.tripped_total = self.tripped_total.saturating_add(1);
        }

        CliffVerdict { fraction, disputed }
    }

    pub fn observe_pass(&mut self, counters: &PassCounters, now: Millis) -> Vec<Location> {
        counters
            .passes()
            .into_iter()
            .filter_map(|pass| {
                let verdict = self.observe(&pass, now);
                verdict.is_cliffed().then_some(pass.location)
            })
            .collect()
    }

    pub fn is_cliffed(&self, location: &Location, now: Millis) -> bool {
        self.watches.get(location).is_some_and(|watch| {
            watch.dispute_since.is_some() || watch.detector.is_cliffed(now, self.config)
        })
    }

    pub fn cliffed_since(&self, location: &Location) -> Option<Millis> {
        let watch = self.watches.get(location)?;
        match (watch.dispute_since, watch.detector.cliffed_since()) {
            (Some(dispute), Some(fraction)) => Some(dispute.min(fraction)),
            (Some(dispute), None) => Some(dispute),
            (None, fraction) => fraction,
        }
    }

    pub fn is_disputed(&self, location: &Location) -> bool {
        self.watches
            .get(location)
            .is_some_and(|watch| watch.dispute_since.is_some())
    }

    pub fn disputed(&self) -> Vec<Location> {
        self.watches
            .iter()
            .filter(|(_, watch)| watch.dispute_since.is_some())
            .map(|(location, _)| location.clone())
            .collect()
    }

    pub fn cliffed(&self, now: Millis) -> Vec<Location> {
        self.watches
            .keys()
            .filter(|location| self.is_cliffed(location, now))
            .cloned()
            .collect()
    }

    pub fn forget(&mut self, location: &Location) -> bool {
        self.watches.remove(location).is_some()
    }

    pub fn retain<F>(&mut self, mut keep: F)
    where
        F: FnMut(&Location) -> bool,
    {
        self.watches.retain(|location, _| keep(location));
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RoomTurnReads {
    pub gateway: GatewayRead,
    pub pending_joins: Vec<PendingJoin>,
    pub pending_joins_read: bool,
    pub gateway_calls: u32,
    pub readouts: Vec<LocationReadout>,
}

impl RoomTurnReads {
    pub fn gateway_connections(&self) -> Vec<ConnectionId> {
        let mut connections: Vec<ConnectionId> = match &self.gateway {
            GatewayRead::Failed(_) => Vec::new(),
            GatewayRead::Ok { states } => states
                .iter()
                .filter_map(|state| state.connection.clone())
                .collect(),
        };
        connections.sort();
        connections.dedup();
        connections
    }

    pub fn media_connections(&self) -> Vec<ConnectionId> {
        let mut connections: Vec<ConnectionId> = self
            .readouts
            .iter()
            .filter_map(LocationReadout::roster)
            .flat_map(RoomRoster::connections)
            .collect();
        connections.sort();
        connections.dedup();
        connections
    }

    pub fn gateway_states(&self) -> &[GatewayVoiceState] {
        match &self.gateway {
            GatewayRead::Ok { states } => states,
            GatewayRead::Failed(_) => &[],
        }
    }

    pub fn media_only_connections(&self) -> Vec<ConnectionId> {
        let gateway = self.gateway_connections();
        self.media_connections()
            .into_iter()
            .filter(|connection| !gateway.contains(connection))
            .collect()
    }
}

#[derive(Clone, Copy, Debug)]
pub struct RoomContext<'a> {
    pub room: RoomKey,
    pub turn: TurnId,
    pub at: Millis,
    pub wall_at: WallMillis,
    pub candidates: &'a CandidateSet,
    pub health: &'a ServerHealthMap,
    pub cliffs: &'a ServerCliffWatch,
    pub holed: &'a [Location],
    pub believed_homes: &'a [Location],
    pub room_cliffed: bool,
    pub census: CensusCrossCheck,
    pub topology: TopologyFreshness,
    pub census_epoch: Epoch,
    pub topology_epoch: Epoch,
    pub max_connections_per_room: usize,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RoomView {
    observation: RoomObservation,
    gateway: GatewayRead,
    readouts: Vec<LocationReadout>,
    probes: Vec<LocationProbe>,
    expectations: BTreeMap<Location, u32>,
    pending_joins_read: bool,
}

impl RoomView {
    pub const fn observation(&self) -> &RoomObservation {
        &self.observation
    }

    pub const fn room(&self) -> RoomKey {
        self.observation.room
    }

    pub const fn turn(&self) -> TurnId {
        self.observation.turn
    }

    pub const fn at(&self) -> Millis {
        self.observation.at
    }

    pub const fn authority(&self) -> &Side<SideAuthority> {
        &self.observation.authority
    }

    pub const fn gateway_read(&self) -> &GatewayRead {
        &self.gateway
    }

    pub const fn gateway_readable(&self) -> bool {
        matches!(self.gateway, GatewayRead::Ok { .. })
    }

    pub const fn pending_joins_read(&self) -> bool {
        self.pending_joins_read
    }

    pub fn readouts(&self) -> &[LocationReadout] {
        &self.readouts
    }

    pub fn probes(&self) -> &[LocationProbe] {
        &self.probes
    }

    pub const fn expectations(&self) -> &BTreeMap<Location, u32> {
        &self.expectations
    }

    pub fn readable_locations(&self) -> Vec<Location> {
        self.readouts
            .iter()
            .filter(|readout| readout.is_readable())
            .map(|readout| readout.location().clone())
            .collect()
    }

    pub fn unreadable_locations(&self) -> Vec<(Location, LiveKitFault)> {
        self.readouts
            .iter()
            .filter_map(|readout| {
                readout
                    .fault()
                    .map(|fault| (readout.location().clone(), fault))
            })
            .collect()
    }

    pub fn unprobed_candidates(&self) -> Vec<Location> {
        self.observation
            .candidates
            .locations()
            .iter()
            .filter(|location| {
                !self
                    .readouts
                    .iter()
                    .any(|readout| readout.location() == *location)
            })
            .cloned()
            .collect()
    }

    pub fn participants_seen(&self) -> u32 {
        self.readouts
            .iter()
            .map(LocationReadout::participants_seen)
            .fold(0u32, u32::saturating_add)
    }

    pub fn locations_seen(&self) -> Vec<Location> {
        let mut locations: Vec<Location> = self
            .readouts
            .iter()
            .filter(|readout| readout.participants_seen() > 0)
            .map(|readout| readout.location().clone())
            .collect();
        locations.sort();
        locations.dedup();
        locations
    }
}

fn expectations_from(reads: &RoomTurnReads) -> BTreeMap<Location, u32> {
    let media = reads.media_connections();
    let mut expectations: BTreeMap<Location, u32> = BTreeMap::new();

    for state in reads.gateway_states() {
        let Some(connection) = state.connection.as_ref() else {
            continue;
        };
        if media.contains(connection) {
            continue;
        }
        let Some(hint) = state.hint.as_ref() else {
            continue;
        };
        let entry = expectations.entry(hint.clone()).or_insert(0);
        *entry = entry.saturating_add(1);
    }

    expectations
}

pub fn observe_room(reads: RoomTurnReads, context: &RoomContext<'_>) -> RoomView {
    let probes: Vec<LocationProbe> = reads
        .readouts
        .iter()
        .map(|readout| {
            let location = readout.location();
            let cliffed =
                context.cliffs.is_cliffed(location, context.at) || context.holed.contains(location);
            readout.probe(context.health.health(location), cliffed)
        })
        .collect();

    let mut connections = reads.gateway_connections();
    connections.extend(reads.media_connections());
    connections.sort();
    connections.dedup();

    let gateway_side = gateway_authority(&GatewayAuthorityInput {
        read: &reads.gateway,
        room_cliffed: context.room_cliffed,
        census: context.census,
        topology: context.topology,
    });
    let media_side = match unread_believed_home(context.believed_homes, context.candidates, &probes)
    {
        Some(_) => SideAuthority::Unknown(UnknownReason::MediaLocationErrored(
            LiveKitFault::ServerMissing,
        )),
        None => media_authority(&MediaAuthorityInput {
            candidates: context.candidates,
            probes: &probes,
            connections_in_room: connections.len(),
            max_connections_per_room: context.max_connections_per_room,
        }),
    };

    let observation = RoomObservation::assemble(
        &RoomReads {
            room: context.room,
            turn: context.turn,
            at: context.at,
            wall_at: context.wall_at,
            gateway: &reads.gateway,
            probes: &probes,
            pending_joins: reads.pending_joins.clone(),
            candidates: context.candidates.clone(),
            census_epoch: context.census_epoch,
            topology_epoch: context.topology_epoch,
        },
        Side::new(gateway_side, media_side),
    );

    RoomView {
        expectations: expectations_from(&reads),
        observation,
        gateway: reads.gateway,
        readouts: reads.readouts,
        probes,
        pending_joins_read: reads.pending_joins_read,
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct RoomReadPlan<'a> {
    pub room: RoomKey,
    pub locations: &'a [Location],
}

pub async fn read_room<G, L, F>(
    gateway: &G,
    livekit: &L,
    plan: &RoomReadPlan<'_>,
    clock: &dyn Clock,
    mut admit: F,
    token: &mut TurnToken,
) -> Result<Fresh<RoomTurnReads>, TurnError>
where
    G: GatewayApi,
    L: LiveKitApi,
    F: FnMut(&Location) -> bool,
{
    let states = gateway.voice_states_for_channel(plan.room).await;
    let gateway_read = match states {
        Ok(states) => GatewayRead::Ok { states },
        Err(fault) => GatewayRead::Failed(fault),
    };
    let fresh_gateway = token.read(
        ReadSource::GatewayVoiceStates,
        clock.now(),
        digest_gateway_read(&gateway_read),
        gateway_read,
    );

    let mut readouts: Vec<LocationReadout> = Vec::new();
    for location in plan.locations {
        if !admit(location) {
            continue;
        }
        let readout =
            read_location_roster(livekit, location, plan.room, ReadClock::from_clock(clock)).await;
        readouts.push(readout);
    }

    let mut fresh_readouts = match readouts.first() {
        None => token.read(
            ReadSource::MediaParticipants,
            clock.now(),
            Digest::new().text("no_locations_read").finish(),
            Vec::new(),
        ),
        Some(first) => token.read(
            ReadSource::MediaParticipants,
            first.at(),
            first.digest(),
            vec![first.clone()],
        ),
    };
    for readout in readouts.iter().skip(1) {
        let next = token.read(
            ReadSource::MediaParticipants,
            readout.at(),
            readout.digest(),
            readout.clone(),
        );
        fresh_readouts = fresh_readouts.zip(next)?.map(|(mut all, one)| {
            all.push(one);
            all
        });
    }

    let reads = fresh_gateway
        .zip(fresh_readouts)?
        .map(|(gateway, readouts)| RoomTurnReads {
            gateway,
            pending_joins: Vec::new(),
            pending_joins_read: false,
            gateway_calls: 1,
            readouts,
        });

    let gateway_readable = matches!(reads.peek().gateway, GatewayRead::Ok { .. });
    if !gateway_readable || reads.peek().media_only_connections().is_empty() {
        return Ok(reads);
    }

    let joins = gateway.pending_joins_for_channel(plan.room).await;
    let at = clock.now();
    match joins {
        Ok(joins) => {
            let fresh_joins = token.read(
                ReadSource::GatewayPendingJoins,
                at,
                digest_pending_joins(&joins),
                joins,
            );
            Ok(reads.zip(fresh_joins)?.map(|(mut reads, joins)| {
                reads.pending_joins = joins;
                reads.pending_joins_read = true;
                reads.gateway_calls = 2;
                reads
            }))
        }
        Err(fault) => {
            let fresh_fault = token.read(
                ReadSource::GatewayPendingJoins,
                at,
                Digest::new().text("failed").text(fault.label()).finish(),
                fault,
            );
            Ok(reads.zip(fresh_fault)?.map(|(mut reads, fault)| {
                reads.gateway = GatewayRead::Failed(fault);
                reads.pending_joins = Vec::new();
                reads.pending_joins_read = false;
                reads.gateway_calls = 2;
                reads
            }))
        }
    }
}
