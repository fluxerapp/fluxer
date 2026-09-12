// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::config::ConnectionIdGuard;
use crate::evidence::{MediaSighting, PendingJoin};
use crate::health::ServerHealth;
use crate::ids::{ConnectionId, Location, Millis, RoomKey, TurnId, UserId, WallMillis};
use crate::ledger::RepairVerdict;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AbortReason {
    SiblingConnection,
    NoConnectionId,
    PendingJoin,
    PreflightStale,
    PreflightDisagreed,
    JoinedAfterDivergence,
    UnknownGateway,
    UnknownMedia,
    ServerUnhealthy,
    Budget,
    Mode,
    Warmup,
    DmPosture,
    Breaker,
}

impl AbortReason {
    pub const ALL: [Self; 14] = [
        Self::SiblingConnection,
        Self::NoConnectionId,
        Self::PendingJoin,
        Self::PreflightStale,
        Self::PreflightDisagreed,
        Self::JoinedAfterDivergence,
        Self::UnknownGateway,
        Self::UnknownMedia,
        Self::ServerUnhealthy,
        Self::Budget,
        Self::Mode,
        Self::Warmup,
        Self::DmPosture,
        Self::Breaker,
    ];

    pub const fn label(self) -> &'static str {
        match self {
            Self::SiblingConnection => "sibling_connection",
            Self::NoConnectionId => "no_connection_id",
            Self::PendingJoin => "pending_join",
            Self::PreflightStale => "preflight_stale",
            Self::PreflightDisagreed => "preflight_disagreed",
            Self::JoinedAfterDivergence => "joined_after_divergence",
            Self::UnknownGateway => "unknown_gateway",
            Self::UnknownMedia => "unknown_media",
            Self::ServerUnhealthy => "server_unhealthy",
            Self::Budget => "budget",
            Self::Mode => "mode",
            Self::Warmup => "warmup",
            Self::DmPosture => "dm_posture",
            Self::Breaker => "breaker",
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PreflightVoiceState {
    pub user_id: UserId,
    pub connection: Option<ConnectionId>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct GatewayPreflight {
    pub turn: TurnId,
    pub taken_at: Millis,
    pub voice_states: Vec<PreflightVoiceState>,
    pub pending_joins: Vec<PendingJoin>,
    pub media_present: Vec<MediaSighting>,
}

impl GatewayPreflight {
    pub const fn is_fresh(&self, now: Millis, max_age_ms: u64) -> bool {
        now.saturating_since(self.taken_at) <= max_age_ms
    }

    pub fn states_for(&self, user_id: UserId) -> impl Iterator<Item = &PreflightVoiceState> {
        self.voice_states
            .iter()
            .filter(move |state| state.user_id == user_id)
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

    pub fn media_holds(&self, connection: &ConnectionId) -> bool {
        self.media_present
            .iter()
            .any(|sighting| &sighting.connection == connection)
    }

    pub fn media_legs_of(&self, user_id: UserId) -> Vec<ConnectionId> {
        self.media_present
            .iter()
            .filter(|sighting| sighting.user_id == user_id)
            .map(|sighting| sighting.connection.clone())
            .collect()
    }

    pub fn joining_connections_of(
        &self,
        user_id: UserId,
        wall_now: WallMillis,
        skew_ms: u64,
    ) -> Vec<ConnectionId> {
        self.pending_joins
            .iter()
            .filter(|join| join.user_id == user_id && join.is_unexpired(wall_now, skew_ms))
            .map(|join| join.connection.clone())
            .collect()
    }

    pub fn live_connections_of(
        &self,
        user_id: UserId,
        window: PreflightWindow,
    ) -> Vec<ConnectionId> {
        let joining =
            self.joining_connections_of(user_id, window.wall_now, window.pending_join_skew_ms);
        let mut live: Vec<ConnectionId> = self
            .states_for(user_id)
            .filter_map(|state| state.connection.clone())
            .collect();
        live.extend(self.media_legs_of(user_id));
        live.retain(|connection| !joining.contains(connection));
        live.sort();
        live.dedup();
        live
    }

    pub fn gateway_holds(&self, connection: &ConnectionId) -> bool {
        self.voice_states
            .iter()
            .any(|state| state.connection.as_ref() == Some(connection))
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum UnknownMediaCause {
    UnreadRequired,
    Unasked,
    NoCustody,
    UserHomeSilent,
    NoStamp,
    LiveServerSilent,
}

impl UnknownMediaCause {
    pub const ALL: [Self; 6] = [
        Self::UnreadRequired,
        Self::Unasked,
        Self::NoCustody,
        Self::UserHomeSilent,
        Self::NoStamp,
        Self::LiveServerSilent,
    ];

    pub const fn label(self) -> &'static str {
        match self {
            Self::UnreadRequired => "unread_required",
            Self::Unasked => "unasked",
            Self::NoCustody => "no_custody",
            Self::UserHomeSilent => "user_home_silent",
            Self::NoStamp => "no_stamp",
            Self::LiveServerSilent => "live_server_silent",
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct MediaCoverage {
    fleet: Vec<Location>,
    required: Vec<Location>,
    read: Vec<Location>,
    absent: Vec<Location>,
    unreachable: Vec<Location>,
}

impl MediaCoverage {
    pub fn over(fleet: Vec<Location>, required: Vec<Location>) -> Self {
        let mut fleet = fleet;
        fleet.sort();
        fleet.dedup();
        let mut required = required;
        required.sort();
        required.dedup();
        required.retain(|location| fleet.contains(location));
        Self {
            fleet,
            required,
            read: Vec::new(),
            absent: Vec::new(),
            unreachable: Vec::new(),
        }
    }

    pub fn note_read(&mut self, location: &Location) {
        if self.fleet.contains(location) && !self.read.contains(location) {
            self.read.push(location.clone());
        }
    }

    pub fn note_absent(&mut self, location: &Location) {
        if self.fleet.contains(location)
            && !self.read.contains(location)
            && !self.absent.contains(location)
        {
            self.absent.push(location.clone());
        }
    }

    pub fn note_unreachable(&mut self, location: &Location) {
        if self.fleet.contains(location) && !self.unreachable.contains(location) {
            self.unreachable.push(location.clone());
        }
    }

    pub fn fleet(&self) -> &[Location] {
        &self.fleet
    }

    pub fn required(&self) -> &[Location] {
        &self.required
    }

    pub fn read(&self) -> &[Location] {
        &self.read
    }

    pub fn absent(&self) -> &[Location] {
        &self.absent
    }

    pub fn unreachable(&self) -> &[Location] {
        &self.unreachable
    }

    pub fn answered(&self) -> Vec<Location> {
        let mut answered = self.read.clone();
        for location in &self.absent {
            if !answered.contains(location) {
                answered.push(location.clone());
            }
        }
        answered.sort();
        answered
    }

    pub fn silent(&self) -> Vec<Location> {
        self.fleet
            .iter()
            .filter(|location| !self.read.contains(location) && !self.absent.contains(location))
            .cloned()
            .collect()
    }

    pub fn unasked(&self) -> Option<&Location> {
        self.fleet.iter().find(|location| {
            !self.read.contains(location)
                && !self.absent.contains(location)
                && !self.unreachable.contains(location)
        })
    }

    pub fn unread(&self) -> Option<&Location> {
        self.required
            .iter()
            .find(|location| !self.read.contains(location) && !self.absent.contains(location))
    }

    pub fn is_complete(&self) -> bool {
        !self.read.is_empty() && self.unasked().is_none() && self.unread().is_none()
    }
}

pub const fn connection_id_is_honoured(posture: ConnectionIdGuard, room: RoomKey) -> bool {
    match room {
        RoomKey::Guild { .. } => matches!(posture, ConnectionIdGuard::Honoured),
        RoomKey::Dm { .. } => false,
    }
}

pub fn media_coverage_cause(
    coverage: &MediaCoverage,
    posture: ConnectionIdGuard,
    room: RoomKey,
) -> Option<UnknownMediaCause> {
    if connection_id_is_honoured(posture, room) {
        return None;
    }
    if coverage.unread().is_some() {
        return Some(UnknownMediaCause::UnreadRequired);
    }
    if coverage.is_complete() {
        None
    } else {
        Some(UnknownMediaCause::Unasked)
    }
}

pub fn media_coverage_refusal(
    coverage: &MediaCoverage,
    posture: ConnectionIdGuard,
    room: RoomKey,
) -> Option<AbortReason> {
    media_coverage_cause(coverage, posture, room).map(|_| AbortReason::UnknownMedia)
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MediaCustody<'a> {
    pub room: RoomKey,
    pub posture: ConnectionIdGuard,
    pub fleet: &'a [Location],
    pub answered: &'a [Location],
    pub read: &'a [Location],
    pub target_home: Option<&'a Location>,
    pub user_homes: &'a [Location],
    pub stamped: &'a [Location],
}

impl MediaCustody<'_> {
    fn silent(&self) -> Vec<&Location> {
        self.fleet
            .iter()
            .filter(|location| !self.answered.contains(location))
            .collect()
    }

    fn target_is_held(&self) -> bool {
        self.target_home
            .is_some_and(|home| self.fleet.contains(home) && self.read.contains(home))
    }
}

pub fn media_custody_cause(custody: &MediaCustody<'_>) -> Option<UnknownMediaCause> {
    let silent = custody.silent();
    if silent.is_empty() {
        return None;
    }
    if connection_id_is_honoured(custody.posture, custody.room) {
        return if silent
            .iter()
            .all(|location| custody.stamped.contains(location))
        {
            None
        } else {
            Some(UnknownMediaCause::NoStamp)
        };
    }
    if !custody.target_is_held() {
        return Some(UnknownMediaCause::NoCustody);
    }
    if custody.user_homes.iter().any(|home| silent.contains(&home)) {
        return Some(UnknownMediaCause::UserHomeSilent);
    }
    None
}

pub fn media_custody_refusal(custody: &MediaCustody<'_>) -> Option<AbortReason> {
    media_custody_cause(custody).map(|_| AbortReason::UnknownMedia)
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FleetSilence<'a> {
    pub room: RoomKey,
    pub posture: ConnectionIdGuard,
    pub fleet: &'a [Location],
    pub answered: &'a [Location],
    pub written_off: &'a [Location],
}

impl FleetSilence<'_> {
    fn silent_and_still_expected(&self) -> Option<&Location> {
        self.fleet.iter().find(|location| {
            !self.answered.contains(location) && !self.written_off.contains(location)
        })
    }
}

pub fn fleet_silence_cause(silence: &FleetSilence<'_>) -> Option<UnknownMediaCause> {
    if connection_id_is_honoured(silence.posture, silence.room) {
        return None;
    }
    silence
        .silent_and_still_expected()
        .map(|_| UnknownMediaCause::LiveServerSilent)
}

pub fn fleet_silence_refusal(silence: &FleetSilence<'_>) -> Option<AbortReason> {
    fleet_silence_cause(silence).map(|_| AbortReason::UnknownMedia)
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct PreflightWindow {
    pub now: Millis,
    pub wall_now: WallMillis,
    pub preflight_max_age_ms: u64,
    pub pending_join_skew_ms: u64,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct DmPosture {
    pub dm_gateway_eviction: bool,
    pub dm_media_eviction: bool,
}

impl Default for DmPosture {
    fn default() -> Self {
        Self {
            dm_gateway_eviction: true,
            dm_media_eviction: false,
        }
    }
}

impl DmPosture {
    pub const fn allows_gateway_removal(self, room: RoomKey) -> bool {
        match room {
            RoomKey::Guild { .. } => true,
            RoomKey::Dm { .. } => self.dm_gateway_eviction,
        }
    }

    pub const fn allows_media_removal(self, room: RoomKey) -> bool {
        match room {
            RoomKey::Guild { .. } => true,
            RoomKey::Dm { .. } => self.dm_media_eviction,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct GatewayRemovalGuard<'a> {
    pub room: RoomKey,
    pub target: &'a ConnectionId,
    pub user_id: UserId,
    pub posture: ConnectionIdGuard,
    pub dm_posture: DmPosture,
    pub preflight: &'a GatewayPreflight,
    pub window: PreflightWindow,
}

pub fn gateway_removal_preflight(guard: &GatewayRemovalGuard<'_>) -> Result<(), AbortReason> {
    if !guard
        .preflight
        .is_fresh(guard.window.now, guard.window.preflight_max_age_ms)
    {
        return Err(AbortReason::PreflightStale);
    }

    if !guard.dm_posture.allows_gateway_removal(guard.room) {
        return Err(AbortReason::DmPosture);
    }

    let states: Vec<&PreflightVoiceState> = guard.preflight.states_for(guard.user_id).collect();

    if states.iter().any(|state| state.connection.is_none()) {
        return Err(AbortReason::NoConnectionId);
    }

    if connection_id_is_honoured(guard.posture, guard.room) {
        if !states
            .iter()
            .any(|state| state.connection.as_ref() == Some(guard.target))
        {
            return Err(AbortReason::PreflightDisagreed);
        }
    } else {
        let live = guard
            .preflight
            .live_connections_of(guard.user_id, guard.window);
        if live.len() != 1 {
            return Err(AbortReason::SiblingConnection);
        }
        if &live[0] != guard.target {
            return Err(AbortReason::SiblingConnection);
        }
    }

    if guard.preflight.user_has_pending_join(
        guard.user_id,
        guard.window.wall_now,
        guard.window.pending_join_skew_ms,
    ) {
        return Err(AbortReason::PendingJoin);
    }

    if guard.preflight.media_holds(guard.target) {
        return Err(AbortReason::PreflightDisagreed);
    }

    Ok(())
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MediaRemovalGuard<'a> {
    pub room: RoomKey,
    pub target: &'a ConnectionId,
    pub user_id: UserId,
    pub dm_posture: DmPosture,
    pub divergence_since: Millis,
    pub participant_joined_at: Millis,
    pub repair_verdict: Option<RepairVerdict>,
    pub server_health: ServerHealth,
    pub preflight: &'a GatewayPreflight,
    pub window: PreflightWindow,
}

pub fn media_removal_preflight(guard: &MediaRemovalGuard<'_>) -> Result<(), AbortReason> {
    if !guard
        .preflight
        .is_fresh(guard.window.now, guard.window.preflight_max_age_ms)
    {
        return Err(AbortReason::PreflightStale);
    }

    if !guard.dm_posture.allows_media_removal(guard.room) {
        return Err(AbortReason::DmPosture);
    }

    if guard
        .preflight
        .states_for(guard.user_id)
        .any(|state| state.connection.is_none())
    {
        return Err(AbortReason::NoConnectionId);
    }

    if guard.participant_joined_at > guard.divergence_since {
        return Err(AbortReason::JoinedAfterDivergence);
    }

    if matches!(guard.room, RoomKey::Guild { .. })
        && guard.repair_verdict != Some(RepairVerdict::NotRepairable)
    {
        return Err(AbortReason::PreflightDisagreed);
    }

    if guard.preflight.gateway_holds(guard.target) {
        return Err(AbortReason::PreflightDisagreed);
    }

    if guard.preflight.user_has_pending_join(
        guard.user_id,
        guard.window.wall_now,
        guard.window.pending_join_skew_ms,
    ) {
        return Err(AbortReason::PendingJoin);
    }

    if !guard.server_health.is_healthy() {
        return Err(AbortReason::ServerUnhealthy);
    }

    Ok(())
}
