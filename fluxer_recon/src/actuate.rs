// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::budget::{
    AuthorizationOutcome, AuthorizationRequest, BudgetClass, MutationAuthorization,
};
use crate::clock::Clock;
use crate::config::{ConnectionIdGuard, ReconConfig};
use crate::decide::{Decision, DecisionAction};
use crate::evidence::{GatewayVoiceState, MediaSighting, PendingJoin};
use crate::gateway::codes::{GatewayError, GatewayErrorCode};
use crate::gateway::{DisconnectOutcome, GatewayApi, GatewayFault, Nonce, RepairOutcome};
use crate::guards::{
    AbortReason, DmPosture, FleetSilence, GatewayPreflight, GatewayRemovalGuard, MediaCoverage,
    MediaCustody, MediaRemovalGuard, PreflightVoiceState, PreflightWindow, UnknownMediaCause,
    connection_id_is_honoured, fleet_silence_cause, gateway_removal_preflight,
    media_coverage_cause, media_coverage_refusal, media_custody_cause, media_removal_preflight,
};
use crate::health::ModeClamp;
use crate::ids::{ConnectionId, ConnectionKey, Location, Millis, RoomKey, TurnId, UserId};
use crate::ledger::{
    ActionKind, ConnectionLedger, ConnectionState, Ledger, LocationIndex, RepairVerdict, RoomLedger,
};
use crate::livekit::{LiveKitApi, LiveKitFault, ReadResult, RemoveOutcome};
use crate::metrics::{MutationOutcome, ReconMetrics};
use crate::names::{ParticipantIdentity, parse_participant_identity};
use crate::runtime::{RuntimeState, Shared};
use crate::turn::{Digest, Fresh, ReadSource, TurnError, TurnRecord, TurnScope, TurnSequencer};

pub const PREFLIGHT_GATEWAY_READS: u32 = 2;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Lane {
    Constructive,
    Destructive,
}

impl Lane {
    pub const fn label(self) -> &'static str {
        match self {
            Self::Constructive => "constructive",
            Self::Destructive => "destructive",
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Actuation {
    Held,
    Refused {
        lane: Lane,
        reason: AbortReason,
    },
    Counterfactual {
        lane: Lane,
        reason: AbortReason,
    },
    Issued {
        lane: Lane,
        call: &'static str,
        outcome: &'static str,
    },
    Failed {
        lane: Lane,
        call: &'static str,
        fault: &'static str,
    },
}

impl Actuation {
    pub const fn called_out(&self) -> bool {
        matches!(self, Self::Issued { .. } | Self::Failed { .. })
    }

    pub const fn mutated(&self) -> bool {
        matches!(self, Self::Issued { .. })
    }

    pub const fn lane(&self) -> Option<Lane> {
        match self {
            Self::Held => None,
            Self::Refused { lane, .. }
            | Self::Counterfactual { lane, .. }
            | Self::Issued { lane, .. }
            | Self::Failed { lane, .. } => Some(*lane),
        }
    }

    pub const fn refusal(&self) -> Option<AbortReason> {
        match self {
            Self::Held | Self::Issued { .. } | Self::Failed { .. } => None,
            Self::Refused { reason, .. } | Self::Counterfactual { reason, .. } => Some(*reason),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ActuationPolicy {
    pub dm_posture: DmPosture,
    pub connection_id_guard: ConnectionIdGuard,
    pub preflight_max_age_ms: u64,
    pub pending_join_skew_ms: u64,
}

impl ActuationPolicy {
    pub const fn from_config(config: &ReconConfig) -> Self {
        Self {
            dm_posture: DmPosture {
                dm_gateway_eviction: config.dm_gateway_eviction,
                dm_media_eviction: config.dm_media_eviction,
            },
            connection_id_guard: config.gateway_connection_id_guard,
            preflight_max_age_ms: config.preflight_max_age_ms,
            pending_join_skew_ms: config.pending_join_skew_ms,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TurnFacts {
    pub room: RoomKey,
    pub authorizing_turn: TurnId,
    pub media: Vec<MediaSighting>,
}

impl TurnFacts {
    pub fn media_connections(&self) -> Vec<ConnectionId> {
        self.media
            .iter()
            .map(|sighting| sighting.connection.clone())
            .collect()
    }

    pub fn media_siblings_of(&self, user_id: UserId, target: &ConnectionId) -> Vec<ConnectionId> {
        self.media
            .iter()
            .filter(|sighting| sighting.user_id == user_id && &sighting.connection != target)
            .map(|sighting| sighting.connection.clone())
            .collect()
    }
}

pub struct Ports<'a, G, L> {
    pub gateway: &'a G,
    pub livekit: &'a L,
    pub clock: &'a dyn Clock,
    pub shared: &'a Shared,
    pub policy: ActuationPolicy,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PreflightReads {
    pub voice_states: Vec<GatewayVoiceState>,
    pub pending_joins: Vec<PendingJoin>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PreflightError {
    Gateway(GatewayFault),
    Turn(TurnError),
}

impl PreflightError {
    pub const fn abort_reason(&self) -> AbortReason {
        match self {
            Self::Gateway(_) => AbortReason::UnknownGateway,
            Self::Turn(_) => AbortReason::PreflightDisagreed,
        }
    }

    pub const fn label(&self) -> &'static str {
        match self {
            Self::Gateway(fault) => fault.label(),
            Self::Turn(_) => "turn_violation",
        }
    }
}

fn digest_voice_states(states: &[GatewayVoiceState]) -> u64 {
    let mut digest = Digest::new().text("preflight_voice_states");
    for state in states {
        digest = digest
            .text(state.connection.as_ref().map_or("", ConnectionId::as_str))
            .flag(state.connection.is_some())
            .number(state.user_id.get())
            .number(state.channel_id.get());
    }
    digest.finish()
}

fn digest_pending_joins(joins: &[PendingJoin]) -> u64 {
    let mut digest = Digest::new().text("preflight_pending_joins");
    for join in joins {
        digest = digest
            .text(join.connection.as_str())
            .number(join.user_id.get())
            .number(join.expires_at.get());
    }
    digest.finish()
}

pub fn open_preflight(
    state: &mut RuntimeState,
    room: RoomKey,
    now: Millis,
) -> Result<crate::turn::TurnToken, AbortReason> {
    if !state
        .governor
        .try_acquire_many(BudgetClass::GatewayRead, PREFLIGHT_GATEWAY_READS, now)
    {
        return Err(AbortReason::Budget);
    }
    Ok(state.sequencer.open(TurnScope::preflight(room), now))
}

pub async fn read_preflight<G>(
    gateway: &G,
    room: RoomKey,
    clock: &dyn Clock,
    token: &mut crate::turn::TurnToken,
) -> Result<Fresh<PreflightReads>, PreflightError>
where
    G: GatewayApi,
{
    let states = gateway
        .voice_states_for_channel(room)
        .await
        .map_err(PreflightError::Gateway)?;
    let fresh_states = token.read(
        ReadSource::GatewayVoiceStates,
        clock.now(),
        digest_voice_states(&states),
        states,
    );

    let joins = match gateway.pending_joins_for_channel(room).await {
        Ok(joins) => joins,
        Err(fault) => {
            let _ = token.discard(fresh_states);
            return Err(PreflightError::Gateway(fault));
        }
    };
    let fresh_joins = token.read(
        ReadSource::GatewayPendingJoins,
        clock.now(),
        digest_pending_joins(&joins),
        joins,
    );

    fresh_states
        .zip(fresh_joins)
        .map(|both| {
            both.map(|(voice_states, pending_joins)| PreflightReads {
                voice_states,
                pending_joins,
            })
        })
        .map_err(PreflightError::Turn)
}

pub fn seal_preflight(
    sequencer: &mut TurnSequencer,
    token: crate::turn::TurnToken,
    at: Millis,
    reads: Fresh<PreflightReads>,
    media_present: Vec<MediaSighting>,
) -> Result<(TurnRecord, GatewayPreflight), TurnError> {
    let turn = token.turn();
    let taken_at = token.opened_at();
    let preflight = reads.map(|reads| GatewayPreflight {
        turn,
        taken_at,
        voice_states: reads
            .voice_states
            .iter()
            .map(|state| PreflightVoiceState {
                user_id: state.user_id,
                connection: state.connection.clone(),
            })
            .collect(),
        pending_joins: reads.pending_joins,
        media_present,
    });
    sequencer
        .seal(token, at, preflight)
        .map(crate::turn::Observed::into_parts)
}

pub fn preflight_is_current(
    sequencer: &TurnSequencer,
    room: RoomKey,
    record: &TurnRecord,
    now: Millis,
    max_age_ms: u64,
) -> Result<(), AbortReason> {
    if !sequencer.is_latest_sealed(&room, record.turn()) {
        return Err(AbortReason::PreflightDisagreed);
    }
    if !record.is_fresh(now, max_age_ms) {
        return Err(AbortReason::PreflightStale);
    }
    Ok(())
}

pub fn media_sibling_refusal(
    facts: &TurnFacts,
    target: &ConnectionId,
    user_id: UserId,
    posture: ConnectionIdGuard,
) -> Option<AbortReason> {
    if connection_id_is_honoured(posture, facts.room) {
        return None;
    }
    if facts.media_siblings_of(user_id, target).is_empty() {
        None
    } else {
        Some(AbortReason::SiblingConnection)
    }
}

fn media_divergence_facts(
    ledger: &Ledger,
    key: &ConnectionKey,
) -> Option<(Millis, Millis, Option<RepairVerdict>)> {
    let entry = ledger.connection(key)?;
    match &entry.state {
        ConnectionState::MediaOnly {
            since,
            participant_joined_at,
            repair_verdict,
            ..
        } => Some((*since, *participant_joined_at, *repair_verdict)),
        ConnectionState::Nascent
        | ConnectionState::Consistent
        | ConnectionState::PendingJoin { .. }
        | ConnectionState::GatewayOnly { .. }
        | ConnectionState::Repairing { .. }
        | ConnectionState::ActionTaken { .. }
        | ConnectionState::Wedged { .. }
        | ConnectionState::Retired { .. } => None,
    }
}

fn mutations_are_paused(fault: &GatewayFault) -> bool {
    matches!(
        fault,
        GatewayFault::NotOk(GatewayError::Known(GatewayErrorCode::EventMutationsPaused))
    )
}

fn note_mutation_fault(state: &mut RuntimeState, fault: &GatewayFault, now: Millis) {
    if mutations_are_paused(fault)
        && let Some(until) = state.mutations_pause.note_refused(now)
    {
        state
            .governor
            .clamps_mut()
            .engage_until(ModeClamp::MutationsPaused, until);
        tracing::warn!(
            hold_ms = state.mutations_pause.hold_ms(),
            windows = state.mutations_pause.windows(),
            "the gateway is refusing mutations, holding the destructive lane for one backoff \
             window that expires on its own"
        );
    }
}

fn note_mutation_success(state: &mut RuntimeState) {
    state.mutations_pause.clear();
    state
        .governor
        .clamps_mut()
        .set(ModeClamp::MutationsPaused, false);
}

fn refuse(metrics: &ReconMetrics, lane: Lane, reason: AbortReason) -> Actuation {
    if matches!(lane, Lane::Destructive) {
        metrics.record_eviction_aborted(reason);
    }
    Actuation::Refused { lane, reason }
}

fn counterfactual(metrics: &ReconMetrics, lane: Lane, reason: AbortReason) -> Actuation {
    if matches!(lane, Lane::Destructive) {
        metrics.record_eviction_aborted(reason);
    }
    Actuation::Counterfactual { lane, reason }
}

fn hold_gateway_slot(shared: &Shared) -> bool {
    shared.with_state_mut(|state| state.governor.gateway_inflight().try_acquire())
}

fn release_gateway_slot(shared: &Shared) {
    shared.with_state_mut(|state| state.governor.gateway_inflight().release());
}

fn hold_media_slot(shared: &Shared, location: &Location) -> bool {
    shared.with_state_mut(|state| {
        if !state.governor.livekit_inflight().try_acquire() {
            return false;
        }
        if !state.governor.server_inflight(location).try_acquire() {
            state.governor.livekit_inflight().release();
            return false;
        }
        true
    })
}

fn release_media_slot(shared: &Shared, location: &Location) {
    shared.with_state_mut(|state| {
        state.governor.server_inflight(location).release();
        state.governor.livekit_inflight().release();
    });
}

fn constructive_gate(
    state: &mut RuntimeState,
    connection: &ConnectionKey,
    now: Millis,
) -> Result<(), (bool, AbortReason)> {
    match state.governor.authorize_constructive(connection, now) {
        Ok(()) => Ok(()),
        Err(AbortReason::Mode) => Err((true, AbortReason::Mode)),
        Err(reason) => Err((false, reason)),
    }
}

pub async fn apply_decision<G, L>(
    ports: &Ports<'_, G, L>,
    facts: &TurnFacts,
    decision: &Decision,
) -> Actuation
where
    G: GatewayApi,
    L: LiveKitApi,
{
    let metrics = ports.shared.metrics().clone();

    if let Some(reason) = decision.blocked_by {
        return Actuation::Refused {
            lane: if decision.is_destructive() {
                Lane::Destructive
            } else {
                Lane::Constructive
            },
            reason,
        };
    }

    match &decision.action {
        DecisionAction::Hold => Actuation::Held,
        DecisionAction::ConfirmConnection { nonce } => {
            issue_confirm(ports, &metrics, decision, nonce).await
        }
        DecisionAction::RepairState => issue_repair(ports, &metrics, decision).await,
        DecisionAction::RemoveGatewayState => {
            remove_gateway_state(ports, &metrics, facts, decision).await
        }
        DecisionAction::RemoveParticipant { location } => {
            remove_participant(ports, &metrics, facts, decision, location).await
        }
    }
}

async fn issue_confirm<G, L>(
    ports: &Ports<'_, G, L>,
    metrics: &ReconMetrics,
    decision: &Decision,
    nonce: &Nonce,
) -> Actuation
where
    G: GatewayApi,
    L: LiveKitApi,
{
    let now = ports.clock.now();
    let key = decision.connection.clone();
    let room = key.room;

    if let Err((counterfactual_gate, reason)) = ports
        .shared
        .with_state_mut(|state| constructive_gate(state, &key, now))
    {
        return if counterfactual_gate {
            counterfactual(metrics, Lane::Constructive, reason)
        } else {
            refuse(metrics, Lane::Constructive, reason)
        };
    }

    if !hold_gateway_slot(ports.shared) {
        return refuse(metrics, Lane::Constructive, AbortReason::Budget);
    }
    let started = ports.clock.now();
    let result = ports
        .gateway
        .confirm_connection(room, &key.connection, nonce)
        .await;
    release_gateway_slot(ports.shared);
    metrics.observe_mutation_duration(ports.clock.elapsed_since(started));

    let settled = ports.clock.now();
    ports.shared.with_state_mut(|state| {
        state.ledger.record_confirm_issued(&key, settled);
        match &result {
            Err(fault) => note_mutation_fault(state, fault, settled),
            Ok(_) => note_mutation_success(state),
        }
    });

    match result {
        Err(fault) => {
            metrics.record_confirm("failed");
            metrics.record_gateway_rpc("confirm_connection", fault.label());
            Actuation::Failed {
                lane: Lane::Constructive,
                call: "confirm_connection",
                fault: fault.label(),
            }
        }
        Ok(outcome) => {
            metrics.record_confirm(outcome.label());
            metrics.record_gateway_rpc("confirm_connection", outcome.label());
            Actuation::Issued {
                lane: Lane::Constructive,
                call: "confirm_connection",
                outcome: outcome.label(),
            }
        }
    }
}

async fn issue_repair<G, L>(
    ports: &Ports<'_, G, L>,
    metrics: &ReconMetrics,
    decision: &Decision,
) -> Actuation
where
    G: GatewayApi,
    L: LiveKitApi,
{
    let now = ports.clock.now();
    let key = decision.connection.clone();
    let room = key.room;

    if let Err((counterfactual_gate, reason)) = ports
        .shared
        .with_state_mut(|state| constructive_gate(state, &key, now))
    {
        return if counterfactual_gate {
            counterfactual(metrics, Lane::Constructive, reason)
        } else {
            refuse(metrics, Lane::Constructive, reason)
        };
    }

    if !hold_gateway_slot(ports.shared) {
        return refuse(metrics, Lane::Constructive, AbortReason::Budget);
    }
    let started = ports.clock.now();
    let result = ports
        .gateway
        .repair_state_from_cache(room, decision.user_id, &key.connection)
        .await;
    release_gateway_slot(ports.shared);
    metrics.observe_mutation_duration(ports.clock.elapsed_since(started));

    let settled = ports.clock.now();
    let verdict = result.as_ref().ok().map(RepairOutcome::verdict);
    ports.shared.with_state_mut(|state| {
        state.ledger.record_repair_issued(&key, settled);
        match &result {
            Err(fault) => note_mutation_fault(state, fault, settled),
            Ok(_) => note_mutation_success(state),
        }
        if let Some(verdict) = verdict {
            state.ledger.record_repair_verdict(&key, verdict);
        }
    });

    match result {
        Err(fault) => {
            metrics.record_repair("failed");
            metrics.record_gateway_rpc("repair_state_from_cache", fault.label());
            Actuation::Failed {
                lane: Lane::Constructive,
                call: "repair_state_from_cache",
                fault: fault.label(),
            }
        }
        Ok(outcome) => {
            metrics.record_repair_verdict(outcome.verdict());
            metrics.record_gateway_rpc("repair_state_from_cache", outcome.label());
            Actuation::Issued {
                lane: Lane::Constructive,
                call: "repair_state_from_cache",
                outcome: outcome.label(),
            }
        }
    }
}

struct AuthorizedRemoval {
    authorization: MutationAuthorization,
}

enum RemovalGate {
    Authorized(AuthorizedRemoval),
    Denied {
        counterfactual: bool,
        reason: AbortReason,
        cause: Option<UnknownMediaCause>,
    },
}

pub async fn sweep_media<L>(
    livekit: &L,
    room: RoomKey,
    fleet: Vec<Location>,
    required: Vec<Location>,
    budget: impl FnMut(&Location) -> bool,
) -> (Vec<MediaSighting>, MediaCoverage)
where
    L: LiveKitApi,
{
    sweep_rosters(livekit, room, fleet, required, &[], budget).await
}

pub async fn sweep_rosters<L>(
    livekit: &L,
    room: RoomKey,
    fleet: Vec<Location>,
    required: Vec<Location>,
    absent_ok: &[Location],
    mut budget: impl FnMut(&Location) -> bool,
) -> (Vec<MediaSighting>, MediaCoverage)
where
    L: LiveKitApi,
{
    let mut coverage = MediaCoverage::over(fleet, required);
    let mut sightings: Vec<MediaSighting> = Vec::new();

    for location in coverage.fleet().to_vec() {
        if !budget(&location) {
            continue;
        }
        let records = match livekit.list_participants(&location, room).await {
            ReadResult::Read(records) => records,
            ReadResult::Unreadable(LiveKitFault::NotFound) if absent_ok.contains(&location) => {
                coverage.note_absent(&location);
                continue;
            }
            ReadResult::Unreadable(_) => {
                coverage.note_unreachable(&location);
                continue;
            }
        };
        let mut seen: Vec<MediaSighting> = Vec::with_capacity(records.len());
        let mut readable = true;
        for record in &records {
            match parse_participant_identity(&record.identity) {
                Err(_) => readable = false,
                Ok(identity) => seen.push(MediaSighting {
                    connection: identity.connection_id,
                    user_id: identity.user_id,
                }),
            }
        }
        if readable {
            coverage.note_read(&location);
            sightings.extend(seen);
        }
    }

    sightings.sort_by(|left, right| left.connection.cmp(&right.connection));
    sightings.dedup_by(|left, right| left.connection == right.connection);
    (sightings, coverage)
}

fn fleet_locations(state: &RuntimeState, now: Millis) -> Vec<Location> {
    state.topology.lens(now).live_locations()
}

fn believed_footprint(state: &RuntimeState, room: RoomKey) -> Vec<Location> {
    let mut locations = state.directory.locations_for(&room);
    let homes = state
        .ledger
        .room(&room)
        .map(RoomLedger::believed_homes)
        .unwrap_or_default();
    for index in homes {
        if let Some(location) = state.ledger.location(index) {
            locations.push(location.clone());
        }
    }
    locations.sort();
    locations.dedup();
    locations
}

fn absent_ok_locations(state: &RuntimeState, room: RoomKey, fleet: &[Location]) -> Vec<Location> {
    fleet
        .iter()
        .filter(|location| state.directory.room_is_absent_from(location, &room))
        .cloned()
        .collect()
}

fn written_off_locations(state: &RuntimeState, fleet: &[Location]) -> Vec<Location> {
    fleet
        .iter()
        .filter(|location| state.server_health.health(location).is_written_off())
        .cloned()
        .collect()
}

fn unprobeable_locations(state: &RuntimeState, fleet: &[Location], now: Millis) -> Vec<Location> {
    fleet
        .iter()
        .filter(|location| !state.server_health.may_probe(location, now))
        .cloned()
        .collect()
}

fn record_roster_stamps(
    state: &mut RuntimeState,
    room: RoomKey,
    fleet: &[Location],
    read: &[Location],
    at: Millis,
) {
    let live: Vec<LocationIndex> = fleet
        .iter()
        .filter_map(|location| state.ledger.intern_location(location))
        .collect();
    let indices: Vec<LocationIndex> = read
        .iter()
        .filter_map(|location| state.ledger.location_index(location))
        .collect();
    let Some(entry) = state.ledger.room_mut(&room) else {
        return;
    };
    entry.retain_roster_stamps(&live);
    for index in indices {
        entry.note_roster_read(index, at);
    }
}

async fn sweep_fleet_media<G, L>(
    ports: &Ports<'_, G, L>,
    room: RoomKey,
) -> (Vec<MediaSighting>, MediaCoverage)
where
    G: GatewayApi,
    L: LiveKitApi,
{
    let now = ports.clock.now();
    let (fleet, required, absent_ok, unprobeable) = ports.shared.with_state(|state| {
        let fleet = fleet_locations(state, now);
        let required = believed_footprint(state, room);
        let absent_ok = absent_ok_locations(state, room, &fleet);
        let unprobeable = unprobeable_locations(state, &fleet, now);
        (fleet, required, absent_ok, unprobeable)
    });
    let (sightings, mut coverage) = sweep_rosters(
        ports.livekit,
        room,
        fleet,
        required,
        &absent_ok,
        |location| {
            if unprobeable.contains(location) {
                return false;
            }
            ports.shared.with_state_mut(|state| {
                let at = ports.clock.now();
                if !state.governor.try_acquire_server_read(location, 1, at) {
                    return false;
                }
                if state.governor.try_acquire(BudgetClass::LiveKitRead, at) {
                    return true;
                }
                state.governor.refund_server_read(location, 1);
                false
            })
        },
    )
    .await;
    for location in &unprobeable {
        coverage.note_unreachable(location);
    }
    let settled = ports.clock.now();
    let swept_fleet = coverage.fleet().to_vec();
    let read = coverage.read().to_vec();
    ports.shared.with_state_mut(|state| {
        record_roster_stamps(state, room, &swept_fleet, &read, settled);
    });
    (sightings, coverage)
}

async fn take_preflight<G, L>(
    ports: &Ports<'_, G, L>,
    room: RoomKey,
    media_present: Vec<MediaSighting>,
) -> Result<(TurnRecord, GatewayPreflight), AbortReason>
where
    G: GatewayApi,
    L: LiveKitApi,
{
    let opened_at = ports.clock.now();
    let mut token = ports
        .shared
        .with_state_mut(|state| open_preflight(state, room, opened_at))?;

    if !hold_gateway_slot(ports.shared) {
        ports
            .shared
            .with_state_mut(|state| state.sequencer.abandon(token));
        return Err(AbortReason::Budget);
    }
    let reads = read_preflight(ports.gateway, room, ports.clock, &mut token).await;
    release_gateway_slot(ports.shared);
    let sealed_at = ports.clock.now();

    ports.shared.with_state_mut(|state| match reads {
        Err(error) => {
            state.sequencer.abandon(token);
            Err(error.abort_reason())
        }
        Ok(reads) => seal_preflight(&mut state.sequencer, token, sealed_at, reads, media_present)
            .map_err(|_| AbortReason::PreflightDisagreed),
    })
}

fn render_locations(locations: &[Location]) -> String {
    locations
        .iter()
        .map(ToString::to_string)
        .collect::<Vec<String>>()
        .join(",")
}

struct CustodyView {
    target_home: Option<Location>,
    user_homes: Vec<Location>,
    stamped: Vec<Location>,
}

fn custody_view(
    state: &RuntimeState,
    key: &ConnectionKey,
    user_id: UserId,
    fleet: &[Location],
) -> CustodyView {
    let entry = state.ledger.connection(key);
    let target_home = entry
        .and_then(|entry| entry.last_location)
        .and_then(|index| state.ledger.location(index).cloned());
    let divergence_since = entry
        .and_then(|entry| entry.state.divergence_since())
        .unwrap_or(Millis::new(0));
    let room_entry = state.ledger.room(&key.room);
    let user_homes = room_entry
        .map(|room_entry| {
            room_entry
                .connections()
                .iter()
                .filter(|connection| connection.user_id == user_id)
                .filter_map(ConnectionLedger::believed_home)
                .filter_map(|index| state.ledger.location(index).cloned())
                .collect::<Vec<Location>>()
        })
        .unwrap_or_default();
    let stamped = fleet
        .iter()
        .filter(|location| {
            state
                .ledger
                .location_index(location)
                .and_then(|index| room_entry.and_then(|entry| entry.roster_read_at(index)))
                .is_some_and(|at| at >= divergence_since)
        })
        .cloned()
        .collect();
    CustodyView {
        target_home,
        user_homes,
        stamped,
    }
}

async fn remove_gateway_state<G, L>(
    ports: &Ports<'_, G, L>,
    metrics: &ReconMetrics,
    facts: &TurnFacts,
    decision: &Decision,
) -> Actuation
where
    G: GatewayApi,
    L: LiveKitApi,
{
    let key = decision.connection.clone();
    let room = key.room;
    let target = key.connection.clone();
    let user_id = decision.user_id;

    let (swept, coverage) = sweep_fleet_media(ports, room).await;
    if let Some(reason) = media_coverage_refusal(&coverage, ports.policy.connection_id_guard, room)
    {
        if let Some(cause) = media_coverage_cause(&coverage, ports.policy.connection_id_guard, room)
        {
            metrics.record_unknown_media_cause(cause);
        }
        return refuse(metrics, Lane::Destructive, reason);
    }

    let fleet = coverage.fleet().to_vec();
    let answered = coverage.answered();
    let read = coverage.read().to_vec();
    let silent = coverage.silent();

    let (record, preflight) = match take_preflight(ports, room, swept).await {
        Err(reason) => return refuse(metrics, Lane::Destructive, reason),
        Ok(taken) => taken,
    };

    let now = ports.clock.now();
    let window = PreflightWindow {
        now,
        wall_now: ports.clock.wall_now(),
        preflight_max_age_ms: ports.policy.preflight_max_age_ms,
        pending_join_skew_ms: ports.policy.pending_join_skew_ms,
    };

    let gate = ports.shared.with_state_mut(|state| {
        if let Err(reason) = preflight_is_current(
            &state.sequencer,
            room,
            &record,
            now,
            ports.policy.preflight_max_age_ms,
        ) {
            return RemovalGate::Denied {
                counterfactual: false,
                reason,
                cause: None,
            };
        }
        if let Err(reason) = gateway_removal_preflight(&GatewayRemovalGuard {
            room,
            target: &target,
            user_id,
            posture: ports.policy.connection_id_guard,
            dm_posture: ports.policy.dm_posture,
            preflight: &preflight,
            window,
        }) {
            return RemovalGate::Denied {
                counterfactual: false,
                reason,
                cause: None,
            };
        }
        if let Some(reason) =
            media_sibling_refusal(facts, &target, user_id, ports.policy.connection_id_guard)
        {
            return RemovalGate::Denied {
                counterfactual: false,
                reason,
                cause: None,
            };
        }
        let ledger_view = custody_view(state, &key, user_id, &fleet);
        if let Some(cause) = media_custody_cause(&MediaCustody {
            room,
            posture: ports.policy.connection_id_guard,
            fleet: &fleet,
            answered: &answered,
            read: &read,
            target_home: ledger_view.target_home.as_ref(),
            user_homes: &ledger_view.user_homes,
            stamped: &ledger_view.stamped,
        }) {
            return RemovalGate::Denied {
                counterfactual: false,
                reason: AbortReason::UnknownMedia,
                cause: Some(cause),
            };
        }
        let written_off = written_off_locations(state, &fleet);
        if let Some(cause) = fleet_silence_cause(&FleetSilence {
            room,
            posture: ports.policy.connection_id_guard,
            fleet: &fleet,
            answered: &answered,
            written_off: &written_off,
        }) {
            return RemovalGate::Denied {
                counterfactual: false,
                reason: AbortReason::UnknownMedia,
                cause: Some(cause),
            };
        }
        authorize(
            state,
            &AuthorizationRequest {
                connection: key.clone(),
                user_id,
                kind: ActionKind::RemoveGatewayState,
                location: None,
                authorizing_turn: facts.authorizing_turn,
                preflight_turn: record.turn(),
                preflight_at: record.opened_at(),
                now,
            },
        )
    });

    let authorization = match gate {
        RemovalGate::Denied {
            counterfactual: is_counterfactual,
            reason,
            cause,
        } => {
            if let Some(cause) = cause {
                metrics.record_unknown_media_cause(cause);
                tracing::info!(
                    channel_id = room.channel_id().get(),
                    guild_id = room.guild_id().map(|guild| guild.get()),
                    connection = %target,
                    user_id = user_id.get(),
                    cause = cause.label(),
                    silent = %render_locations(&silent),
                    "a user scoped disconnect was held because the fleet did not answer in full"
                );
            }
            return if is_counterfactual {
                counterfactual(metrics, Lane::Destructive, reason)
            } else {
                refuse(metrics, Lane::Destructive, reason)
            };
        }
        RemovalGate::Authorized(authorized) => authorized.authorization,
    };

    if !hold_gateway_slot(ports.shared) {
        ports.shared.with_state_mut(|state| {
            state.governor.refund_identity(&key);
        });
        return refuse(metrics, Lane::Destructive, AbortReason::Budget);
    }
    metrics.record_mutation(
        ActionKind::RemoveGatewayState,
        room.scope(),
        MutationOutcome::Issued,
    );
    let started = ports.clock.now();
    let result = execute_gateway_removal(ports.gateway, authorization).await;
    release_gateway_slot(ports.shared);
    metrics.observe_mutation_duration(ports.clock.elapsed_since(started));

    let settled = ports.clock.now();
    ports.shared.with_state_mut(|state| {
        state.ledger.record_action_issued(
            &key,
            ActionKind::RemoveGatewayState,
            settled,
            facts.authorizing_turn,
        );
        match &result {
            Err(fault) => {
                note_mutation_fault(state, fault, settled);
                if mutations_are_paused(fault) {
                    state.governor.refund_identity(&key);
                    state.ledger.refund_action_attempt(&key);
                }
            }
            Ok(_) => note_mutation_success(state),
        }
    });

    match result {
        Err(fault) => {
            metrics.record_mutation(
                ActionKind::RemoveGatewayState,
                room.scope(),
                MutationOutcome::Failed,
            );
            metrics.record_gateway_rpc("disconnect_user_if_in_channel", fault.label());
            Actuation::Failed {
                lane: Lane::Destructive,
                call: "disconnect_user_if_in_channel",
                fault: fault.label(),
            }
        }
        Ok(outcome) => {
            metrics.record_mutation(
                ActionKind::RemoveGatewayState,
                room.scope(),
                disconnect_outcome(&outcome),
            );
            metrics.record_gateway_rpc("disconnect_user_if_in_channel", outcome.label());
            Actuation::Issued {
                lane: Lane::Destructive,
                call: "disconnect_user_if_in_channel",
                outcome: outcome.label(),
            }
        }
    }
}

const fn disconnect_outcome(outcome: &DisconnectOutcome) -> MutationOutcome {
    match outcome {
        DisconnectOutcome::Removed => MutationOutcome::Succeeded,
        DisconnectOutcome::Ignored { .. } | DisconnectOutcome::CallNotFound => {
            MutationOutcome::NoChange
        }
        DisconnectOutcome::Failed(_) => MutationOutcome::Failed,
    }
}

pub async fn execute_gateway_removal<G>(
    gateway: &G,
    authorization: MutationAuthorization,
) -> Result<DisconnectOutcome, GatewayFault>
where
    G: GatewayApi,
{
    let room = authorization.connection.room;
    let connection = authorization.connection.connection.clone();
    let user_id = authorization.user_id;
    drop(authorization);

    gateway
        .disconnect_user_if_in_channel(room, user_id, Some(&connection))
        .await
}

async fn remove_participant<G, L>(
    ports: &Ports<'_, G, L>,
    metrics: &ReconMetrics,
    facts: &TurnFacts,
    decision: &Decision,
    location: &Location,
) -> Actuation
where
    G: GatewayApi,
    L: LiveKitApi,
{
    let key = decision.connection.clone();
    let room = key.room;
    let target = key.connection.clone();
    let user_id = decision.user_id;

    let Some((since, joined_at, verdict)) = ports
        .shared
        .with_state(|state| media_divergence_facts(&state.ledger, &key))
    else {
        return refuse(metrics, Lane::Destructive, AbortReason::PreflightDisagreed);
    };

    let (record, preflight) = match take_preflight(ports, room, facts.media.clone()).await {
        Err(reason) => return refuse(metrics, Lane::Destructive, reason),
        Ok(taken) => taken,
    };

    let now = ports.clock.now();
    let window = PreflightWindow {
        now,
        wall_now: ports.clock.wall_now(),
        preflight_max_age_ms: ports.policy.preflight_max_age_ms,
        pending_join_skew_ms: ports.policy.pending_join_skew_ms,
    };

    let gate = ports.shared.with_state_mut(|state| {
        if let Err(reason) = preflight_is_current(
            &state.sequencer,
            room,
            &record,
            now,
            ports.policy.preflight_max_age_ms,
        ) {
            return RemovalGate::Denied {
                counterfactual: false,
                reason,
                cause: None,
            };
        }
        if let Err(reason) = media_removal_preflight(&MediaRemovalGuard {
            room,
            target: &target,
            user_id,
            dm_posture: ports.policy.dm_posture,
            divergence_since: since,
            participant_joined_at: joined_at,
            repair_verdict: verdict,
            server_health: state.server_health.health(location),
            preflight: &preflight,
            window,
        }) {
            return RemovalGate::Denied {
                counterfactual: false,
                reason,
                cause: None,
            };
        }
        authorize(
            state,
            &AuthorizationRequest {
                connection: key.clone(),
                user_id,
                kind: ActionKind::RemoveParticipant,
                location: Some(location.clone()),
                authorizing_turn: facts.authorizing_turn,
                preflight_turn: record.turn(),
                preflight_at: record.opened_at(),
                now,
            },
        )
    });

    let authorization = match gate {
        RemovalGate::Denied {
            counterfactual: is_counterfactual,
            reason,
            cause,
        } => {
            if let Some(cause) = cause {
                metrics.record_unknown_media_cause(cause);
            }
            return if is_counterfactual {
                counterfactual(metrics, Lane::Destructive, reason)
            } else {
                refuse(metrics, Lane::Destructive, reason)
            };
        }
        RemovalGate::Authorized(authorized) => authorized.authorization,
    };

    if !hold_media_slot(ports.shared, location) {
        ports.shared.with_state_mut(|state| {
            state.governor.refund_identity(&key);
        });
        return refuse(metrics, Lane::Destructive, AbortReason::Budget);
    }
    metrics.record_mutation(
        ActionKind::RemoveParticipant,
        room.scope(),
        MutationOutcome::Issued,
    );
    let started = ports.clock.now();
    let outcome = execute_media_removal(ports.livekit, authorization, location.clone()).await;
    release_media_slot(ports.shared, location);
    metrics.observe_mutation_duration(ports.clock.elapsed_since(started));

    let settled = ports.clock.now();
    ports.shared.with_state_mut(|state| {
        state.ledger.record_action_issued(
            &key,
            ActionKind::RemoveParticipant,
            settled,
            facts.authorizing_turn,
        );
    });

    metrics.record_mutation(
        ActionKind::RemoveParticipant,
        room.scope(),
        remove_outcome(outcome),
    );
    metrics.record_livekit_call("remove_participant", outcome.label());

    if outcome.is_success() {
        Actuation::Issued {
            lane: Lane::Destructive,
            call: "remove_participant",
            outcome: outcome.label(),
        }
    } else {
        Actuation::Failed {
            lane: Lane::Destructive,
            call: "remove_participant",
            fault: outcome.label(),
        }
    }
}

const fn remove_outcome(outcome: RemoveOutcome) -> MutationOutcome {
    if outcome.is_success() {
        MutationOutcome::Succeeded
    } else {
        MutationOutcome::Failed
    }
}

pub async fn execute_media_removal<L>(
    livekit: &L,
    authorization: MutationAuthorization,
    location: Location,
) -> RemoveOutcome
where
    L: LiveKitApi,
{
    let room = authorization.connection.room;
    let identity = ParticipantIdentity {
        user_id: authorization.user_id,
        connection_id: authorization.connection.connection.clone(),
    };
    drop(authorization);

    livekit.remove_participant(&location, room, &identity).await
}

fn authorize(state: &mut RuntimeState, request: &AuthorizationRequest) -> RemovalGate {
    match state.governor.authorize(request) {
        AuthorizationOutcome::Denied(reason) => RemovalGate::Denied {
            counterfactual: matches!(reason, AbortReason::Mode),
            reason,
            cause: None,
        },
        AuthorizationOutcome::Granted(authorization) => {
            RemovalGate::Authorized(AuthorizedRemoval { authorization })
        }
    }
}
