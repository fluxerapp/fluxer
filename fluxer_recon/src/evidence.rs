// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::gateway::{GatewayFault, Nonce};
use crate::health::ServerHealth;
use crate::ids::{
    ChannelId, ConnectionId, Epoch, GuildId, Location, Millis, RoomKey, TurnId, UserId, WallMillis,
};
use crate::livekit::LiveKitFault;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SideKind {
    Gateway,
    Media,
}

impl SideKind {
    pub const ALL: [Self; 2] = [Self::Gateway, Self::Media];

    pub const fn label(self) -> &'static str {
        match self {
            Self::Gateway => "gateway",
            Self::Media => "media",
        }
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct Side<T> {
    pub gateway: T,
    pub media: T,
}

impl<T> Side<T> {
    pub const fn new(gateway: T, media: T) -> Self {
        Self { gateway, media }
    }

    pub const fn get(&self, kind: SideKind) -> &T {
        match kind {
            SideKind::Gateway => &self.gateway,
            SideKind::Media => &self.media,
        }
    }

    pub fn map<U, F: Fn(SideKind, &T) -> U>(&self, f: F) -> Side<U> {
        Side {
            gateway: f(SideKind::Gateway, &self.gateway),
            media: f(SideKind::Media, &self.media),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum UnknownReason {
    GatewayReadFailed(GatewayFault),
    GatewayCliff,
    CensusDisagrees,
    MediaLocationErrored(LiveKitFault),
    MediaServerUnhealthy,
    MediaServerCliff,
    NoCandidateLocations,
    TopologyStale,
    UnparseableIdentityInRoom,
    RoomPartiallyUnreadable,
}

impl UnknownReason {
    pub const fn label(&self) -> &'static str {
        match self {
            Self::GatewayReadFailed(_) => "gateway_read_failed",
            Self::GatewayCliff => "gateway_cliff",
            Self::CensusDisagrees => "census_disagrees",
            Self::MediaLocationErrored(_) => "media_location_errored",
            Self::MediaServerUnhealthy => "media_server_unhealthy",
            Self::MediaServerCliff => "media_server_cliff",
            Self::NoCandidateLocations => "no_candidate_locations",
            Self::TopologyStale => "topology_stale",
            Self::UnparseableIdentityInRoom => "unparseable_identity_in_room",
            Self::RoomPartiallyUnreadable => "room_partially_unreadable",
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Presence {
    Present,
    Absent,
    Unknown(UnknownReason),
}

impl Presence {
    pub const fn is_present(&self) -> bool {
        matches!(self, Self::Present)
    }

    pub const fn is_absent(&self) -> bool {
        matches!(self, Self::Absent)
    }

    pub const fn is_unknown(&self) -> bool {
        matches!(self, Self::Unknown(_))
    }

    pub const fn unknown_reason(&self) -> Option<&UnknownReason> {
        match self {
            Self::Unknown(reason) => Some(reason),
            Self::Present | Self::Absent => None,
        }
    }

    pub const fn label(&self) -> &'static str {
        match self {
            Self::Present => "present",
            Self::Absent => "absent",
            Self::Unknown(_) => "unknown",
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SideAuthority {
    Authoritative,
    Unknown(UnknownReason),
}

impl SideAuthority {
    pub const fn is_authoritative(&self) -> bool {
        matches!(self, Self::Authoritative)
    }

    pub const fn unknown_reason(&self) -> Option<&UnknownReason> {
        match self {
            Self::Authoritative => None,
            Self::Unknown(reason) => Some(reason),
        }
    }

    pub fn project(&self, sighted: bool) -> Presence {
        match self {
            Self::Unknown(reason) => Presence::Unknown(reason.clone()),
            Self::Authoritative => {
                if sighted {
                    Presence::Present
                } else {
                    Presence::Absent
                }
            }
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct RoomCliffInput {
    pub dropped: usize,
    pub remaining: usize,
}

pub const fn room_cliff_trips(input: RoomCliffInput) -> bool {
    input.dropped >= 2 || (input.dropped >= 1 && input.remaining == 0)
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PriorConnection {
    pub connection: ConnectionId,
    pub was_consistent: bool,
}

pub fn room_cliff_input(
    prior: &[PriorConnection],
    gateway_now: &[ConnectionId],
    media_now: &[ConnectionId],
) -> RoomCliffInput {
    let dropped = prior
        .iter()
        .filter(|entry| entry.was_consistent)
        .filter(|entry| !gateway_now.contains(&entry.connection))
        .filter(|entry| media_now.contains(&entry.connection))
        .count();

    RoomCliffInput {
        dropped,
        remaining: gateway_now.len(),
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct CliffWindow {
    since: Option<Millis>,
    last: Option<Millis>,
}

impl CliffWindow {
    pub const fn clear() -> Self {
        Self {
            since: None,
            last: None,
        }
    }

    pub const fn since(&self) -> Option<Millis> {
        self.since
    }

    pub const fn trip(&mut self, now: Millis) {
        if self.since.is_none() {
            self.since = Some(now);
        }
        self.last = Some(now);
    }

    pub const fn release(&mut self) {
        self.since = None;
        self.last = None;
    }

    pub const fn is_held(&self, now: Millis, hold_ms: u64) -> bool {
        match self.last {
            None => false,
            Some(last) => now.saturating_since(last) < hold_ms,
        }
    }

    pub const fn expire(&mut self, now: Millis, hold_ms: u64) {
        if !self.is_held(now, hold_ms) {
            self.release();
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ServerCliffConfig {
    pub fraction: f64,
    pub hold_ms: u64,
}

pub const SERVER_CLIFF_TRAILING_PASSES: usize = 5;

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct ServerCliffDetector {
    trailing: [u32; SERVER_CLIFF_TRAILING_PASSES],
    filled: usize,
    cursor: usize,
    window: CliffWindow,
}

impl ServerCliffDetector {
    pub const fn new() -> Self {
        Self {
            trailing: [0; SERVER_CLIFF_TRAILING_PASSES],
            filled: 0,
            cursor: 0,
            window: CliffWindow::clear(),
        }
    }

    pub fn observe(
        &mut self,
        participants_seen: u32,
        now: Millis,
        config: ServerCliffConfig,
    ) -> bool {
        let trailing_max = self.trailing_max();
        let tripped = if trailing_max == 0 {
            false
        } else {
            let drop =
                f64::from(trailing_max.saturating_sub(participants_seen)) / f64::from(trailing_max);
            drop > config.fraction
        };

        if tripped {
            self.window.trip(now);
        } else {
            self.record(participants_seen);
            self.window.expire(now, config.hold_ms);
        }

        self.is_cliffed(now, config)
    }

    pub const fn release(&mut self) {
        self.trailing = [0; SERVER_CLIFF_TRAILING_PASSES];
        self.filled = 0;
        self.cursor = 0;
        self.window.release();
    }

    fn record(&mut self, participants_seen: u32) {
        self.trailing[self.cursor] = participants_seen;
        self.cursor = (self.cursor + 1) % SERVER_CLIFF_TRAILING_PASSES;
        self.filled = self
            .filled
            .saturating_add(1)
            .min(SERVER_CLIFF_TRAILING_PASSES);
    }

    pub const fn is_cliffed(&self, now: Millis, config: ServerCliffConfig) -> bool {
        self.window.is_held(now, config.hold_ms)
    }

    pub const fn cliffed_since(&self) -> Option<Millis> {
        self.window.since()
    }

    fn trailing_max(&self) -> u32 {
        self.trailing
            .iter()
            .take(self.filled)
            .copied()
            .max()
            .unwrap_or(0)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum GatewayRead {
    Ok { states: Vec<GatewayVoiceState> },
    Failed(GatewayFault),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct GatewayVoiceState {
    pub connection: Option<ConnectionId>,
    pub user_id: UserId,
    pub channel_id: ChannelId,
    pub guild_id: Option<GuildId>,
    pub hint: Option<Location>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PendingJoin {
    pub connection: ConnectionId,
    pub user_id: UserId,
    pub expires_at: WallMillis,
    pub nonce: Nonce,
}

impl PendingJoin {
    pub const fn is_unexpired(&self, wall_now: WallMillis, skew_ms: u64) -> bool {
        wall_now.get() <= self.expires_at.get().saturating_add(skew_ms)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CensusCrossCheck {
    Fresh { voice_state_count: u32 },
    Stale,
    Missing,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TopologyFreshness {
    pub age_ms: u64,
    pub max_age_ms: u64,
}

impl TopologyFreshness {
    pub const fn is_stale(self) -> bool {
        self.age_ms > self.max_age_ms
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct GatewayAuthorityInput<'a> {
    pub read: &'a GatewayRead,
    pub room_cliffed: bool,
    pub census: CensusCrossCheck,
    pub topology: TopologyFreshness,
}

pub fn gateway_authority(input: &GatewayAuthorityInput<'_>) -> SideAuthority {
    let states = match input.read {
        GatewayRead::Failed(fault) => {
            return SideAuthority::Unknown(UnknownReason::GatewayReadFailed(fault.clone()));
        }
        GatewayRead::Ok { states } => states,
    };

    if input.room_cliffed {
        return SideAuthority::Unknown(UnknownReason::GatewayCliff);
    }

    if let CensusCrossCheck::Fresh { voice_state_count } = input.census
        && voice_state_count > 0
        && states.is_empty()
    {
        return SideAuthority::Unknown(UnknownReason::CensusDisagrees);
    }

    if input.topology.is_stale() {
        return SideAuthority::Unknown(UnknownReason::TopologyStale);
    }

    SideAuthority::Authoritative
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ProbeResult {
    Ok {
        participants: Vec<MediaParticipant>,
        unparseable_identities: usize,
    },
    Failed(LiveKitFault),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MediaParticipant {
    pub connection: ConnectionId,
    pub user_id: UserId,
    pub location: Location,
    pub joined_at: Millis,
    pub is_publisher: bool,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MediaSighting {
    pub connection: ConnectionId,
    pub user_id: UserId,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LocationProbe {
    pub location: Location,
    pub result: ProbeResult,
    pub health: ServerHealth,
    pub cliffed: bool,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct CandidateSet {
    locations: Vec<Location>,
}

impl CandidateSet {
    pub fn from_locations(mut locations: Vec<Location>) -> Self {
        locations.sort();
        locations.dedup();
        Self { locations }
    }

    pub fn locations(&self) -> &[Location] {
        &self.locations
    }

    pub fn is_empty(&self) -> bool {
        self.locations.is_empty()
    }

    pub fn len(&self) -> usize {
        self.locations.len()
    }

    pub fn contains(&self, location: &Location) -> bool {
        self.locations.binary_search(location).is_ok()
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct CandidateSources {
    pub discovered: Vec<Location>,
    pub gateway_hints: Vec<Location>,
    pub pinned: Option<Location>,
    pub ledger_last_known: Vec<Location>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ServerRecord {
    pub location: Location,
    pub removed_at: Option<Millis>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TopologyLens<'a> {
    pub servers: &'a [ServerRecord],
    pub now: Millis,
    pub drain_grace_ms: u64,
}

impl TopologyLens<'_> {
    pub fn live_locations(&self) -> Vec<Location> {
        let mut locations: Vec<Location> = self
            .servers
            .iter()
            .filter(|record| !self.is_drained(&record.location))
            .map(|record| record.location.clone())
            .collect();
        locations.sort();
        locations.dedup();
        locations
    }

    fn is_drained(&self, location: &Location) -> bool {
        self.servers
            .iter()
            .find(|record| &record.location == location)
            .and_then(|record| record.removed_at)
            .is_some_and(|removed_at| self.now.saturating_since(removed_at) > self.drain_grace_ms)
    }
}

pub fn candidate_set(sources: &CandidateSources, topology: &TopologyLens<'_>) -> CandidateSet {
    let mut locations: Vec<Location> = sources
        .discovered
        .iter()
        .chain(sources.gateway_hints.iter())
        .chain(sources.pinned.iter())
        .chain(sources.ledger_last_known.iter())
        .filter(|location| !topology.is_drained(location))
        .cloned()
        .collect();

    locations.sort();
    locations.dedup();
    CandidateSet { locations }
}

pub fn unread_believed_home<'a>(
    homes: &'a [Location],
    candidates: &CandidateSet,
    probes: &[LocationProbe],
) -> Option<&'a Location> {
    homes.iter().find(|home| {
        !candidates.contains(home)
            || !probes.iter().any(|probe| {
                &&probe.location == home && matches!(probe.result, ProbeResult::Ok { .. })
            })
    })
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MediaAuthorityInput<'a> {
    pub candidates: &'a CandidateSet,
    pub probes: &'a [LocationProbe],
    pub connections_in_room: usize,
    pub max_connections_per_room: usize,
}

pub fn media_authority(input: &MediaAuthorityInput<'_>) -> SideAuthority {
    if input.candidates.is_empty() {
        return SideAuthority::Unknown(UnknownReason::NoCandidateLocations);
    }

    for location in input.candidates.locations() {
        let Some(probe) = input
            .probes
            .iter()
            .find(|probe| &probe.location == location)
        else {
            return SideAuthority::Unknown(UnknownReason::MediaLocationErrored(
                LiveKitFault::ServerMissing,
            ));
        };

        match &probe.result {
            ProbeResult::Failed(fault) => {
                return SideAuthority::Unknown(UnknownReason::MediaLocationErrored(*fault));
            }
            ProbeResult::Ok {
                unparseable_identities,
                participants: _,
            } => {
                if *unparseable_identities > 0 {
                    return SideAuthority::Unknown(UnknownReason::UnparseableIdentityInRoom);
                }
            }
        }

        if !probe.health.is_healthy() {
            return SideAuthority::Unknown(UnknownReason::MediaServerUnhealthy);
        }

        if probe.cliffed {
            return SideAuthority::Unknown(UnknownReason::MediaServerCliff);
        }
    }

    if input.connections_in_room > input.max_connections_per_room {
        return SideAuthority::Unknown(UnknownReason::RoomPartiallyUnreadable);
    }

    SideAuthority::Authoritative
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ConnectionObservation {
    pub connection: ConnectionId,
    pub user_id: UserId,
    pub presence: Side<Presence>,
    pub gateway_siblings: Vec<ConnectionId>,
    pub media_locations: Vec<Location>,
    pub participant_joined_at: Option<Millis>,
    pub gateway_state_has_connection_id: bool,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RoomObservation {
    pub room: RoomKey,
    pub turn: TurnId,
    pub at: Millis,
    pub wall_at: WallMillis,
    pub authority: Side<SideAuthority>,
    pub connections: Vec<ConnectionObservation>,
    pub pending_joins: Vec<PendingJoin>,
    pub candidates: CandidateSet,
    pub census_epoch: Epoch,
    pub topology_epoch: Epoch,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RoomReads<'a> {
    pub room: RoomKey,
    pub turn: TurnId,
    pub at: Millis,
    pub wall_at: WallMillis,
    pub gateway: &'a GatewayRead,
    pub probes: &'a [LocationProbe],
    pub pending_joins: Vec<PendingJoin>,
    pub candidates: CandidateSet,
    pub census_epoch: Epoch,
    pub topology_epoch: Epoch,
}

impl RoomObservation {
    pub fn assemble(reads: &RoomReads<'_>, authority: Side<SideAuthority>) -> Self {
        let gateway_states: &[GatewayVoiceState] = match reads.gateway {
            GatewayRead::Ok { states } => states,
            GatewayRead::Failed(_) => &[],
        };

        let participants: Vec<&MediaParticipant> = reads
            .probes
            .iter()
            .filter_map(|probe| match &probe.result {
                ProbeResult::Ok { participants, .. } => Some(participants.iter()),
                ProbeResult::Failed(_) => None,
            })
            .flatten()
            .collect();

        let mut keys: Vec<(ConnectionId, UserId)> = gateway_states
            .iter()
            .filter_map(|state| {
                state
                    .connection
                    .as_ref()
                    .map(|connection| (connection.clone(), state.user_id))
            })
            .chain(
                participants
                    .iter()
                    .map(|participant| (participant.connection.clone(), participant.user_id)),
            )
            .collect();
        keys.sort();
        keys.dedup();

        let connections = keys
            .into_iter()
            .map(|(connection, user_id)| {
                let gateway_sighted = gateway_states
                    .iter()
                    .any(|state| state.connection.as_ref() == Some(&connection));

                let media_sightings: Vec<&&MediaParticipant> = participants
                    .iter()
                    .filter(|participant| participant.connection == connection)
                    .collect();

                let media_sighted = !media_sightings.is_empty();

                let mut gateway_siblings: Vec<ConnectionId> = gateway_states
                    .iter()
                    .filter(|state| state.user_id == user_id)
                    .filter_map(|state| state.connection.clone())
                    .collect();
                gateway_siblings.sort();
                gateway_siblings.dedup();

                let mut media_locations: Vec<Location> = media_sightings
                    .iter()
                    .map(|participant| participant.location.clone())
                    .collect();
                media_locations.sort();
                media_locations.dedup();

                let participant_joined_at = media_sightings
                    .iter()
                    .map(|participant| participant.joined_at)
                    .min();

                let gateway_state_has_connection_id = !gateway_states
                    .iter()
                    .any(|state| state.user_id == user_id && state.connection.is_none());

                ConnectionObservation {
                    presence: Side::new(
                        authority.gateway.project(gateway_sighted),
                        authority.media.project(media_sighted),
                    ),
                    connection,
                    user_id,
                    gateway_siblings,
                    media_locations,
                    participant_joined_at,
                    gateway_state_has_connection_id,
                }
            })
            .collect();

        Self {
            room: reads.room,
            turn: reads.turn,
            at: reads.at,
            wall_at: reads.wall_at,
            authority,
            connections,
            pending_joins: reads.pending_joins.clone(),
            candidates: reads.candidates.clone(),
            census_epoch: reads.census_epoch,
            topology_epoch: reads.topology_epoch,
        }
    }

    pub fn observation_for(&self, connection: &ConnectionId) -> Option<&ConnectionObservation> {
        self.connections
            .iter()
            .find(|entry| &entry.connection == connection)
    }

    pub fn unsighted_presence(&self) -> Side<Presence> {
        Side::new(
            self.authority.gateway.project(false),
            self.authority.media.project(false),
        )
    }

    pub fn pending_join_for(
        &self,
        connection: &ConnectionId,
        wall_now: WallMillis,
        skew_ms: u64,
    ) -> Option<&PendingJoin> {
        self.pending_joins
            .iter()
            .find(|join| &join.connection == connection && join.is_unexpired(wall_now, skew_ms))
    }

    pub fn user_has_pending_join(
        &self,
        user_id: UserId,
        wall_now: WallMillis,
        skew_ms: u64,
    ) -> bool {
        self.pending_joins
            .iter()
            .any(|join| join.user_id == user_id && join.is_unexpired(wall_now, skew_ms))
    }
}
