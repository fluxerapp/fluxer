// SPDX-License-Identifier: AGPL-3.0-or-later

use std::sync::Arc;
use std::time::Duration;

use tokio::sync::RwLock;

use fluxer_svc::config::{DatabaseBackend, ServiceConfig};
use fluxer_svc::transport::Transport;

use crate::actuate::{ActuationPolicy, Ports, TurnFacts, apply_decision};
use crate::budget::{BreakerEvent, BudgetClass};
use crate::census::{CensusOutcome, read_census};
use crate::clock::{Clock, SharedClock, WallAnchor};
use crate::config::ReconConfig;
use crate::decide::{Decision, decide};
use crate::discovery::{
    ExpectationSource, LocationReadout, RoomContext, RoomReadPlan, RoomTurnReads, RoomView,
    ServerRoomList, list_fleet_rooms, observe_room, read_room, record_health,
};
use crate::evidence::{
    CandidateSet, GatewayRead, GatewayVoiceState, MediaSighting, SideKind, TopologyFreshness,
    candidate_set, room_cliff_input, room_cliff_trips,
};
use crate::gateway::GatewayApi;
use crate::gateway::nats::NatsGateway;
use crate::health::ModeClamp;
use crate::ids::{Location, Millis, RoomKey, TurnId, WallMillis};
use crate::ledger::{LocationIndex, RoomLedger};
use crate::livekit::twirp::{TwirpLiveKit, TwirpTimeouts};
use crate::livekit::{
    LiveKitApi, LiveKitFault, ParticipantRecord, ReadResult, RemoveOutcome, ServerCredentials,
};
use crate::metrics::ReconMetrics;
use crate::names::ParticipantIdentity;
use crate::observe::{DecisionJournal, ObservedSet, journal_decisions};
use crate::runtime::{HOUSEKEEP_INTERVAL_MS, RuntimeState, Shared, TickInputs};
use crate::schedule::{
    Cadence, CoverageInputs, CoverageVerdict, DeferReason, GATEWAY_READS_PER_ROOM_TURN,
    check_coverage_period, derived_coverage_period_ms, max_auditable_rooms,
};
use crate::suspicion::{SuspicionHint, SuspicionSource};
use crate::topology::{InternalEndpoint, RefreshOutcome, TopologyStore, VoiceServer};
use crate::turn::Fresh;
use crate::turn::{TurnScope, TurnToken};

pub const CADENCE_POLL_MS: u64 = 250;
pub const FORCED_TOPOLOGY_REFRESH_FLOOR_MS: u64 = 60_000;
pub const DISCOVERY_LIST_ROOMS_COST: u32 = 1;

pub trait Uplink: Send + Sync {
    fn is_connected(&self) -> bool;
}

impl<T: Transport> Uplink for NatsGateway<T> {
    fn is_connected(&self) -> bool {
        self.transport().is_connected()
    }
}

pub trait Fleet {
    fn install(&mut self, servers: Vec<ServerCredentials>) -> anyhow::Result<()>;
}

impl Fleet for TwirpLiveKit {
    fn install(&mut self, servers: Vec<ServerCredentials>) -> anyhow::Result<()> {
        self.replace_servers(servers)
    }
}

#[derive(Debug)]
pub struct SharedFleet<L> {
    inner: Arc<RwLock<L>>,
}

impl<L> Clone for SharedFleet<L> {
    fn clone(&self) -> Self {
        Self {
            inner: Arc::clone(&self.inner),
        }
    }
}

impl<L> SharedFleet<L> {
    pub fn new(livekit: L) -> Self {
        Self {
            inner: Arc::new(RwLock::new(livekit)),
        }
    }
}

impl<L: Fleet + Send + Sync> SharedFleet<L> {
    pub async fn install(&self, servers: Vec<ServerCredentials>) -> anyhow::Result<()> {
        self.inner.write().await.install(servers)
    }
}

impl<L: LiveKitApi + Send + Sync> LiveKitApi for SharedFleet<L> {
    async fn list_rooms(&self, location: &Location) -> ReadResult<Vec<Box<str>>> {
        self.inner.read().await.list_rooms(location).await
    }

    async fn list_participants(
        &self,
        location: &Location,
        room: RoomKey,
    ) -> ReadResult<Vec<ParticipantRecord>> {
        self.inner
            .read()
            .await
            .list_participants(location, room)
            .await
    }

    async fn remove_participant(
        &self,
        location: &Location,
        room: RoomKey,
        identity: &ParticipantIdentity,
    ) -> RemoveOutcome {
        self.inner
            .read()
            .await
            .remove_participant(location, room, identity)
            .await
    }
}

pub struct GuardedClock {
    inner: SharedClock,
    anchor: WallAnchor,
}

impl std::fmt::Debug for GuardedClock {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter
            .debug_struct("GuardedClock")
            .field("anchor", &self.anchor)
            .finish()
    }
}

impl GuardedClock {
    pub fn anchored(inner: SharedClock) -> Self {
        let anchor = WallAnchor::new(inner.now(), inner.wall_now());
        Self { inner, anchor }
    }

    pub fn shared(inner: SharedClock) -> SharedClock {
        Arc::new(Self::anchored(inner))
    }
}

impl Clock for GuardedClock {
    fn now(&self) -> Millis {
        self.inner.now()
    }

    fn wall_now(&self) -> WallMillis {
        self.anchor.guard(self.inner.now(), self.inner.wall_now())
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TurnOutcome {
    Completed,
    Unreadable,
    Starved,
    Broken,
}

impl TurnOutcome {
    pub const fn label(self) -> &'static str {
        match self {
            Self::Completed => "completed",
            Self::Unreadable => "unreadable",
            Self::Starved => "starved",
            Self::Broken => "broken",
        }
    }
}

struct RoomTurnPlan {
    token: TurnToken,
    locations: Vec<Location>,
    gateway_reads: u32,
}

struct RoomTurnPermits {
    locations: Vec<Location>,
    livekit_held: bool,
}

struct TurnReading {
    decisions: Vec<Decision>,
    media: Vec<MediaSighting>,
    turn: TurnId,
    outcome: TurnOutcome,
    divergent: bool,
}

pub struct ReconEngine<G, L> {
    shared: Shared,
    gateway: G,
    livekit: L,
    clock: SharedClock,
    policy: ActuationPolicy,
}

impl<G, L> ReconEngine<G, L>
where
    G: GatewayApi + Uplink,
    L: LiveKitApi,
{
    pub fn new(shared: Shared, gateway: G, livekit: L, clock: SharedClock) -> Self {
        let policy = ActuationPolicy::from_config(shared.config());
        Self {
            shared,
            gateway,
            livekit,
            clock,
            policy,
        }
    }

    pub fn shared(&self) -> &Shared {
        &self.shared
    }

    fn config(&self) -> &ReconConfig {
        self.shared.config()
    }

    fn metrics(&self) -> Arc<ReconMetrics> {
        Arc::clone(self.shared.metrics())
    }

    pub async fn step(&self) {
        let now = self.clock.now();

        if let Some(report) = self.shared.tick_if_due(
            TickInputs {
                now,
                nats_connected: self.gateway.is_connected(),
                last_decision_at: self.shared.journal().last_at_ms().map(Millis::new),
            },
            HOUSEKEEP_INTERVAL_MS,
        ) {
            if matches!(report.breaker, BreakerEvent::Tripped) {
                tracing::error!(
                    divergent_fraction = report.divergent_fraction,
                    "divergent fraction over budget, breaker tripped and every corroboration reset"
                );
            }
            if self.shared.readiness().mark_ready() {
                tracing::info!(
                    mode = report.effective_mode.as_str(),
                    tick_ms = self.config().tick_ms,
                    "recon engine ready"
                );
            }
        }

        let due = self
            .shared
            .with_state_mut(|state| admit_and_take_due(state, now));

        for room in due {
            let started = self.clock.now();
            let outcome = self.room_turn(room).await;
            let settled = self.clock.now();
            self.metrics()
                .observe_turn_duration(settled.saturating_since(started));
            self.shared
                .with_state_mut(|state| settle_turn(state, room, outcome, settled));
            if matches!(outcome, TurnOutcome::Starved) {
                self.metrics().record_scheduler_starvation();
            }
        }

        self.close_pass_if_complete();
    }

    fn close_pass_if_complete(&self) {
        let now = self.clock.now();
        let target_ms = self.config().coverage_target_ms;
        let hard_cap_ms = self.config().coverage_period_hard_cap_ms;
        let config = self.config().clone();

        let closed = self.shared.with_state_mut(|state| {
            state
                .pass_pending
                .retain(|room| state.scheduler.is_tracked(room));
            if !state.pass_pending.is_empty() {
                return None;
            }
            if now.saturating_since(state.pass_started_at) < target_ms {
                return None;
            }
            if state.scheduler.served_total() == 0 {
                return None;
            }

            state.counters.note_expectations_accounted();
            let cliffed = state.cliffs.observe_pass(&state.counters, now);
            state.counters.clear();
            for location in &cliffed {
                let rooms: Vec<RoomKey> = state
                    .directory
                    .room_keys()
                    .into_iter()
                    .filter(|room| state.directory.locations_for(room).contains(location))
                    .collect();
                for room in rooms {
                    if let Some(entry) = state.ledger.room_mut(&room) {
                        entry.reset_corroborations();
                    }
                }
            }

            let period_ms = now.saturating_since(state.pass_started_at);
            state.coverage_period_ms = period_ms;
            state.pass_started_at = now;
            state.pass_pending = state
                .directory
                .room_keys()
                .into_iter()
                .filter(|room| state.scheduler.is_tracked(room))
                .collect();
            state.governor.warmup_mut().note_coverage_pass();

            let rooms = state.directory.len();
            Some((period_ms, rooms, cliffed))
        });

        let Some((period_ms, rooms, cliffed)) = closed else {
            return;
        };

        let derived = derived_coverage_period_ms(&CoverageInputs::for_rooms(rooms, &config));
        let verdict = check_coverage_period(derived, target_ms, hard_cap_ms);
        let overrun = verdict.is_err();
        self.shared.with_state_mut(|state| {
            state
                .governor
                .clamps_mut()
                .set(ModeClamp::CoverageOverrun, overrun);
        });
        match verdict {
            Err(error) => {
                tracing::error!(
                    error = %error,
                    rooms,
                    derived_coverage_period_ms = derived,
                    max_auditable_rooms = max_auditable_rooms(&config),
                    "the room count is beyond what this process can audit at the configured read \
                     rates; the destructive lane is clamped off until the read rates or the room \
                     count change"
                );
            }
            Ok(CoverageVerdict::AboveTarget) if derived > target_ms.saturating_mul(2) => {
                tracing::warn!(
                    rooms,
                    derived_coverage_period_ms = derived,
                    coverage_target_ms = target_ms,
                    "the derived coverage period is more than twice the target"
                );
            }
            Ok(_) => {}
        }

        tracing::debug!(
            period_ms,
            rooms,
            cliffed = cliffed.len(),
            "coverage pass complete"
        );
    }

    async fn room_turn(&self, room: RoomKey) -> TurnOutcome {
        let opened_at = self.clock.now();
        let Some(plan) = self
            .shared
            .with_state_mut(|state| plan_room_turn(state, room, opened_at))
        else {
            return TurnOutcome::Starved;
        };

        let permits = RoomTurnPermits {
            locations: plan.locations.clone(),
            livekit_held: !plan.locations.is_empty(),
        };
        let outcome = self.take_room_turn(room, plan).await;
        self.shared
            .with_state_mut(|state| release_room_turn(state, &permits));
        outcome
    }

    async fn take_room_turn(&self, room: RoomKey, plan: RoomTurnPlan) -> TurnOutcome {
        let RoomTurnPlan {
            mut token,
            locations,
            gateway_reads,
        } = plan;

        let reads = read_room(
            &self.gateway,
            &self.livekit,
            &RoomReadPlan {
                room,
                locations: &locations,
            },
            self.clock.as_ref(),
            |_| true,
            &mut token,
        )
        .await;

        let fresh = match reads {
            Err(error) => {
                tracing::error!(
                    ?error,
                    channel_id = room.channel_id().get(),
                    "a room read broke the turn accounting, abandoning the turn"
                );
                self.shared.with_state_mut(|state| {
                    state.sequencer.abandon(token);
                });
                return TurnOutcome::Broken;
            }
            Ok(fresh) => fresh,
        };

        let at = self.clock.now();
        let wall_at = self.clock.wall_now();
        let metrics = self.metrics();
        let reading = self.shared.with_state_mut(|state| {
            observe_and_decide(
                state,
                &ObserveInputs {
                    config: self.config(),
                    metrics: &metrics,
                    journal: self.shared.journal(),
                    room,
                    at,
                    wall_at,
                    gateway_reads,
                },
                token,
                fresh,
            )
        });

        let Some(reading) = reading else {
            return TurnOutcome::Broken;
        };

        if reading.divergent {
            let hint = SuspicionHint::new(room, SuspicionSource::Divergence, at);
            self.shared.with_state_mut(|state| {
                state.suspicion.note(hint);
            });
        }

        let facts = TurnFacts {
            room,
            authorizing_turn: reading.turn,
            media: reading.media,
        };
        let ports = Ports {
            gateway: &self.gateway,
            livekit: &self.livekit,
            clock: self.clock.as_ref(),
            shared: &self.shared,
            policy: self.policy,
        };

        for decision in &reading.decisions {
            let actuation = apply_decision(&ports, &facts, decision).await;
            if actuation.called_out() {
                tracing::info!(
                    channel_id = room.channel_id().get(),
                    guild_id = room.guild_id().map(|guild| guild.get()),
                    connection_id = decision.connection.connection.as_str(),
                    user_id = decision.user_id.get(),
                    action = decision.action.label(),
                    outcome = ?actuation,
                    "recon issued a reconciliation call"
                );
            }
        }

        reading.outcome
    }
}

fn admit_and_take_due(state: &mut RuntimeState, now: Millis) -> Vec<RoomKey> {
    for room in state.directory.room_keys() {
        if state.scheduler.track(room, now) {
            state.pass_pending.insert(room);
        }
    }

    for room in state.suspicion.drain_raised() {
        state.scheduler.promote(room, now);
    }

    let free = state
        .governor
        .limits()
        .max_inflight_room_turns
        .min(state.scheduler.turns_per_tick());
    state.scheduler.take_due_up_to(now, free)
}

fn settle_turn(state: &mut RuntimeState, room: RoomKey, outcome: TurnOutcome, now: Millis) {
    match outcome {
        TurnOutcome::Completed | TurnOutcome::Unreadable => {
            state.scheduler.completed(room, now);
            state.pass_pending.remove(&room);
        }
        TurnOutcome::Starved => {
            state.scheduler.defer(room, now, DeferReason::Budget);
        }
        TurnOutcome::Broken => {
            state.scheduler.defer(room, now, DeferReason::Unreadable);
        }
    }
}

fn ledger_last_known(state: &RuntimeState, room: RoomKey) -> Vec<Location> {
    let indexes = state
        .ledger
        .room(&room)
        .map(|entry| entry.last_known_locations().to_vec())
        .unwrap_or_default();
    indexes
        .into_iter()
        .filter_map(|index| state.ledger.location(index).cloned())
        .collect()
}

fn believed_homes(state: &RuntimeState, room: RoomKey) -> Vec<Location> {
    let indexes = state
        .ledger
        .room(&room)
        .map(RoomLedger::believed_homes)
        .unwrap_or_default();
    indexes
        .into_iter()
        .filter_map(|index| state.ledger.location(index).cloned())
        .collect()
}

fn planned_candidates(state: &RuntimeState, room: RoomKey, now: Millis) -> CandidateSet {
    let sources = state
        .directory
        .sources_for(&room, Vec::new(), ledger_last_known(state, room));
    candidate_set(&sources, &state.topology.lens(now))
}

fn record_read_calls(metrics: &ReconMetrics, reads: &RoomTurnReads) {
    let gateway_outcome = match &reads.gateway {
        GatewayRead::Ok { .. } => "ok",
        GatewayRead::Failed(fault) => fault.label(),
    };
    if reads.gateway_calls > 1 {
        metrics.record_gateway_rpc("voice_states_for_channel", "ok");
        metrics.record_gateway_rpc("pending_joins_for_channel", gateway_outcome);
    } else {
        metrics.record_gateway_rpc("voice_states_for_channel", gateway_outcome);
    }

    for readout in &reads.readouts {
        let outcome = readout.fault().map_or("ok", LiveKitFault::label);
        metrics.record_livekit_call("list_participants", outcome);
    }
}

fn release_room_turn(state: &mut RuntimeState, permits: &RoomTurnPermits) {
    for location in &permits.locations {
        state.governor.server_inflight(location).release();
    }
    if permits.livekit_held {
        state.governor.livekit_inflight().release();
    }
    state.governor.gateway_inflight().release();
    state.governor.room_turns().release();
}

fn abandon_room_turn(state: &mut RuntimeState, permits: &RoomTurnPermits) {
    for location in &permits.locations {
        state.governor.refund_server_read(location, 1);
    }
    release_room_turn(state, permits);
}

fn admit_locations(
    state: &mut RuntimeState,
    candidates: Vec<Location>,
    now: Millis,
) -> Option<Vec<Location>> {
    let mut admitted: Vec<Location> = Vec::new();
    for location in candidates {
        if !state.governor.server_inflight(&location).try_acquire() {
            release_locations(state, &admitted);
            return None;
        }
        if !state.governor.try_acquire_server_read(&location, 1, now) {
            state.governor.server_inflight(&location).release();
            release_locations(state, &admitted);
            return None;
        }
        admitted.push(location);
    }
    Some(admitted)
}

fn release_locations(state: &mut RuntimeState, locations: &[Location]) {
    for location in locations {
        state.governor.refund_server_read(location, 1);
        state.governor.server_inflight(location).release();
    }
}

fn plan_room_turn(state: &mut RuntimeState, room: RoomKey, now: Millis) -> Option<RoomTurnPlan> {
    let candidates = planned_candidates(state, room, now);
    let offered: Vec<Location> = candidates
        .locations()
        .iter()
        .filter(|location| state.server_health.may_probe(location, now))
        .cloned()
        .collect();

    let gateway_reads = GATEWAY_READS_PER_ROOM_TURN;

    if !state.governor.room_turns().try_acquire() {
        return None;
    }
    if !state.governor.gateway_inflight().try_acquire() {
        state.governor.room_turns().release();
        return None;
    }

    let Some(locations) = admit_locations(state, offered, now) else {
        state.governor.gateway_inflight().release();
        state.governor.room_turns().release();
        return None;
    };
    let livekit_reads = u32::try_from(locations.len()).unwrap_or(u32::MAX);
    let livekit_held = livekit_reads > 0 && state.governor.livekit_inflight().try_acquire();
    let permits = RoomTurnPermits {
        locations: locations.clone(),
        livekit_held,
    };

    if livekit_reads > 0 && !livekit_held {
        abandon_room_turn(state, &permits);
        return None;
    }
    if state.governor.tokens(BudgetClass::GatewayRead, now) < f64::from(gateway_reads) {
        abandon_room_turn(state, &permits);
        return None;
    }
    if state.governor.tokens(BudgetClass::LiveKitRead, now) < f64::from(livekit_reads) {
        abandon_room_turn(state, &permits);
        return None;
    }
    if !state
        .governor
        .try_acquire_many(BudgetClass::GatewayRead, gateway_reads, now)
    {
        abandon_room_turn(state, &permits);
        return None;
    }
    if livekit_reads > 0
        && !state
            .governor
            .try_acquire_many(BudgetClass::LiveKitRead, livekit_reads, now)
    {
        state
            .governor
            .refund(BudgetClass::GatewayRead, gateway_reads);
        abandon_room_turn(state, &permits);
        return None;
    }

    Some(RoomTurnPlan {
        token: state.sequencer.open(TurnScope::room_turn(room), now),
        locations,
        gateway_reads,
    })
}

fn gateway_hints(states: &[GatewayVoiceState]) -> Vec<Location> {
    let mut hints: Vec<Location> = states
        .iter()
        .filter_map(|state| state.hint.clone())
        .collect();
    hints.sort();
    hints.dedup();
    hints
}

fn media_sightings(reads: &RoomTurnReads) -> Vec<MediaSighting> {
    let mut sightings: Vec<MediaSighting> = Vec::new();
    for readout in &reads.readouts {
        let Some(roster) = readout.roster() else {
            continue;
        };
        for participant in roster.participants() {
            sightings.push(MediaSighting {
                connection: participant.connection.clone(),
                user_id: participant.user_id,
            });
        }
    }
    sightings.sort_by(|left, right| left.connection.cmp(&right.connection));
    sightings.dedup_by(|left, right| left.connection == right.connection);
    sightings
}

fn note_roster_reads(
    state: &mut RuntimeState,
    room: RoomKey,
    readouts: &[LocationReadout],
    at: Millis,
) {
    let indices: Vec<LocationIndex> = readouts
        .iter()
        .filter(|readout| readout.roster().is_some())
        .filter_map(|readout| state.ledger.intern_location(readout.location()))
        .collect();
    if indices.is_empty() {
        return;
    }
    if state.ledger.ensure_room(room, at).is_err() {
        return;
    }
    let Some(entry) = state.ledger.room_mut(&room) else {
        return;
    };
    for index in indices {
        entry.note_roster_read(index, at);
    }
}

fn note_expected_from_ledger(state: &mut RuntimeState, room: RoomKey) {
    let Some(entry) = state.ledger.room(&room) else {
        return;
    };
    let counts: Vec<(Location, u32)> = entry
        .believed_home_counts()
        .into_iter()
        .filter_map(|(index, count)| {
            state
                .ledger
                .location(index)
                .cloned()
                .map(|location| (location, count))
        })
        .collect();
    for (location, count) in counts {
        state
            .counters
            .note_room_expected(&location, room, ExpectationSource::Ledger, count);
    }
}

struct ObserveInputs<'a> {
    config: &'a ReconConfig,
    metrics: &'a ReconMetrics,
    journal: &'a DecisionJournal,
    room: RoomKey,
    at: Millis,
    wall_at: WallMillis,
    gateway_reads: u32,
}

fn observe_and_decide(
    state: &mut RuntimeState,
    inputs: &ObserveInputs<'_>,
    token: TurnToken,
    fresh: Fresh<RoomTurnReads>,
) -> Option<TurnReading> {
    let ObserveInputs {
        config,
        metrics,
        journal,
        room,
        at,
        wall_at,
        gateway_reads,
    } = *inputs;

    let turn = token.turn();
    state.turn = turn;

    record_read_calls(metrics, fresh.peek());

    let unused_gateway_reads = gateway_reads.saturating_sub(fresh.peek().gateway_calls);
    if unused_gateway_reads > 0 {
        state
            .governor
            .refund(BudgetClass::GatewayRead, unused_gateway_reads);
    }

    record_health(&fresh.peek().readouts, &mut state.server_health, at);
    note_roster_reads(state, room, &fresh.peek().readouts, at);
    if fresh
        .peek()
        .readouts
        .iter()
        .any(|readout| readout.fault().is_some_and(LiveKitFault::is_auth_failure))
    {
        state.topology_refresh_requested = true;
    }

    let hints = gateway_hints(fresh.peek().gateway_states());
    for hint in &hints {
        state.directory.note_pinned(room, hint.clone(), at);
    }

    let gateway_connections = fresh.peek().gateway_connections();
    let media_connections = fresh.peek().media_connections();
    let readable = matches!(fresh.peek().gateway, GatewayRead::Ok { .. });

    let prior = state
        .ledger
        .room(&room)
        .map(RoomLedger::prior_connections)
        .unwrap_or_default();
    let cliff_input = room_cliff_input(&prior, &gateway_connections, &media_connections);
    let hold_ms = config.room_cliff_hold_ms;
    if state.ledger.ensure_room(room, at).is_ok()
        && let Some(entry) = state.ledger.room_mut(&room)
    {
        if readable && room_cliff_trips(cliff_input) {
            entry.gateway_cliff_mut().trip(at);
            entry.reset_corroborations();
        } else {
            entry.gateway_cliff_mut().expire(at, hold_ms);
        }
    }
    let room_cliffed = state
        .ledger
        .room(&room)
        .is_some_and(|entry| entry.gateway_cliff().is_held(at, hold_ms));

    let media_hold_ms = config.server_cliff_hold_ms;
    let readouts = &fresh.peek().readouts;
    let media_fully_readable =
        !readouts.is_empty() && readouts.iter().all(LocationReadout::is_readable);
    let media_cliff_input = room_cliff_input(&prior, &media_connections, &gateway_connections);
    if let Some(entry) = state.ledger.room_mut(&room) {
        if media_fully_readable && room_cliff_trips(media_cliff_input) {
            entry.media_cliff_mut().trip(at);
            entry.reset_corroborations();
        } else {
            entry.media_cliff_mut().expire(at, media_hold_ms);
        }
    }
    let media_cliffed = state
        .ledger
        .room(&room)
        .is_some_and(|entry| entry.media_cliff().is_held(at, media_hold_ms));

    let sources = state
        .directory
        .sources_for(&room, hints, ledger_last_known(state, room));
    let candidates = candidate_set(&sources, &state.topology.lens(at));
    let census = state.census.cross_check(&room, at);
    let topology = TopologyFreshness {
        age_ms: state.topology.age_ms(at).unwrap_or(u64::MAX),
        max_age_ms: config.topology_max_age_ms,
    };
    let census_epoch = state.census.epoch();
    let topology_epoch = state.topology.epoch();
    let media = media_sightings(fresh.peek());
    let homes = believed_homes(state, room);

    let observed = {
        let mut holed = state.suspicion.holed_servers();
        if media_cliffed {
            holed.extend(candidates.locations().iter().cloned());
            holed.sort();
            holed.dedup();
        }
        let context = RoomContext {
            room,
            turn,
            at,
            wall_at,
            candidates: &candidates,
            health: &state.server_health,
            cliffs: &state.cliffs,
            holed: &holed,
            believed_homes: &homes,
            room_cliffed,
            census,
            topology,
            census_epoch,
            topology_epoch,
            max_connections_per_room: config.max_connections_per_room,
        };
        let view = fresh.map(|reads| observe_room(reads, &context));
        state.counters.note_view(view.peek());
        state.sequencer.seal(token, at, view)
    };

    let view: RoomView = match observed {
        Err(error) => {
            tracing::error!(
                ?error,
                channel_id = room.channel_id().get(),
                "sealing a room turn failed, so no decision was taken from it"
            );
            return None;
        }
        Ok(observed) => observed.into_parts().1,
    };

    note_expected_from_ledger(state, room);

    let gateway_ok = view.authority().gateway.is_authoritative();
    let media_ok = view.authority().media.is_authoritative();
    let budget = state.governor.view(at);
    let outcome = decide(&mut state.ledger, view.observation(), at, &budget);
    let mode = state.governor.effective_mode();

    for decision in &outcome.decisions {
        metrics.record_decision(decision);
    }
    for (side, reason) in [
        (SideKind::Gateway, view.authority().gateway.unknown_reason()),
        (SideKind::Media, view.authority().media.unknown_reason()),
    ] {
        if let Some(reason) = reason {
            metrics.record_unknown(side, reason.label());
        }
    }

    let ledger_snapshot = &state.ledger;
    journal_decisions(
        journal,
        &ObservedSet {
            set: &outcome,
            observation: view.observation(),
            mode,
            pending_join_skew_ms: config.pending_join_skew_ms,
        },
        |decision| ledger_snapshot.connection(&decision.connection).cloned(),
    );

    let divergent = state
        .ledger
        .room(&room)
        .is_some_and(|entry| entry.divergent_connections() > 0);

    if state.ledger.room(&room).is_some() {
        state.directory.note_ledger(room, at);
    }

    Some(TurnReading {
        decisions: outcome.decisions,
        media,
        turn,
        outcome: if gateway_ok && media_ok {
            TurnOutcome::Completed
        } else {
            TurnOutcome::Unreadable
        },
        divergent,
    })
}

pub async fn run_engine<G, L>(engine: ReconEngine<G, L>) -> anyhow::Result<()>
where
    G: GatewayApi + Uplink,
    L: LiveKitApi,
{
    let tick = Duration::from_millis(engine.config().tick_ms.max(1));
    let mut ticker = tokio::time::interval(tick);
    ticker.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Delay);

    loop {
        ticker.tick().await;
        engine.step().await;
    }
}

pub async fn run_census<G>(shared: Shared, gateway: G, clock: SharedClock) -> anyhow::Result<()>
where
    G: GatewayApi,
{
    let mut cadence = Cadence::new(shared.config().census_interval_ms.max(1));
    let mut ticker = tokio::time::interval(Duration::from_millis(CADENCE_POLL_MS));
    ticker.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Delay);

    loop {
        ticker.tick().await;
        let now = clock.now();
        if !cadence.due(now) {
            continue;
        }

        take_census(&shared, &gateway, clock.as_ref()).await;
    }
}

pub async fn take_census<G>(shared: &Shared, gateway: &G, clock: &dyn Clock)
where
    G: GatewayApi,
{
    if !shared.with_state_mut(|state| state.governor.gateway_inflight().try_acquire()) {
        return;
    }
    let read = read_census(gateway).await;
    shared.with_state_mut(|state| state.governor.gateway_inflight().release());
    let label = read.label();
    shared
        .metrics()
        .record_gateway_rpc("active_voice_rooms", label);
    let at = clock.now();
    let outcome = shared.with_state_mut(|state| {
        let outcome = state.census.accept(&read, at);
        let baseline = state.census.applied_total() <= 1;
        if let Some(snapshot) = state.census.snapshot() {
            state.census_at = Some(snapshot.taken_at());
            let seeded = state.directory.note_census(snapshot, at);
            if !seeded.is_empty() {
                tracing::debug!(
                    seeded = seeded.len(),
                    baseline,
                    "the census seeded rooms into the directory"
                );
            }
            if !baseline {
                for room in seeded {
                    state.suspicion.note_room(room, SuspicionSource::Census, at);
                }
            }
        }
        outcome
    });

    match outcome {
        CensusOutcome::Failed => {
            tracing::warn!(
                read = label,
                "the census was unreadable, keeping the last good snapshot and skipping the \
                 cross-check rather than stalling the service"
            );
        }
        CensusOutcome::Applied { rooms, .. } => {
            tracing::debug!(rooms, "census applied");
        }
        CensusOutcome::Unchanged { .. } => {}
    }
}

pub async fn run_discovery<L>(
    shared: Shared,
    fleet: SharedFleet<L>,
    store: Arc<dyn TopologyStore>,
    clock: SharedClock,
) -> anyhow::Result<()>
where
    L: LiveKitApi + Fleet + Send + Sync,
{
    let mut topology_cadence = Cadence::new(shared.config().topology_refresh_ms.max(1));
    let mut rooms_cadence = Cadence::new(shared.config().discovery_interval_ms.max(1));
    let mut forced = Cadence::new(FORCED_TOPOLOGY_REFRESH_FLOOR_MS);
    let mut ticker = tokio::time::interval(Duration::from_millis(CADENCE_POLL_MS));
    ticker.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Delay);

    loop {
        ticker.tick().await;
        let now = clock.now();

        if shared.with_state_mut(RuntimeState::take_topology_refresh_request) && forced.due(now) {
            topology_cadence.force();
        }

        if topology_cadence.due(now) {
            refresh_topology(&shared, &fleet, store.as_ref(), clock.as_ref()).await;
        }

        if rooms_cadence.due(now) {
            discover_rooms(&shared, &fleet, clock.as_ref()).await;
        }
    }
}

pub fn internal_endpoint(config: &ReconConfig) -> InternalEndpoint {
    InternalEndpoint {
        url: config.livekit_internal_url.clone(),
        default_region_id: config.livekit_default_region_id.clone(),
    }
}

fn credentials_of(servers: &[VoiceServer], internal: &InternalEndpoint) -> Vec<ServerCredentials> {
    servers
        .iter()
        .map(|server| ServerCredentials {
            location: server.location().clone(),
            endpoint: Box::from(internal.resolve(server)),
            api_key: Box::from(server.api_key()),
            api_secret: server.api_secret().clone(),
        })
        .collect()
}

pub async fn refresh_topology<L>(
    shared: &Shared,
    fleet: &SharedFleet<L>,
    store: &dyn TopologyStore,
    clock: &dyn Clock,
) where
    L: LiveKitApi + Fleet + Send + Sync,
{
    let loaded = store.load().await;
    let at = clock.now();
    let internal = internal_endpoint(shared.config());

    let (outcome, credentials) = shared.with_state_mut(|state| match loaded {
        Err(error) => {
            tracing::warn!(
                backend = store.backend(),
                error = %error,
                "the topology load failed, keeping the last good snapshot and letting it age out"
            );
            (state.topology.note_failure(at), Vec::new())
        }
        Ok(servers) => {
            let outcome = state.topology.accept(servers, at);
            let credentials = credentials_of(state.topology.servers(), &internal);
            if !outcome.kept_previous_snapshot() {
                state.topology_loaded_at = Some(at);
                state.topology_servers = state.topology.servers().len();
            }
            (outcome, credentials)
        }
    });

    match outcome {
        RefreshOutcome::Applied { epoch, servers } => {
            if let Err(error) = fleet.install(credentials).await {
                tracing::error!(error = %error, "the livekit fleet refused a topology snapshot");
            }
            tracing::info!(
                backend = store.backend(),
                epoch = epoch.get(),
                servers,
                "topology applied"
            );
        }
        RefreshOutcome::Unchanged { .. } => {}
        RefreshOutcome::RejectedEmpty => {
            tracing::error!(
                backend = store.backend(),
                "the topology load returned zero servers, which is refused: an empty fleet would \
                 make every room's media side look empty rather than unreadable"
            );
        }
        RefreshOutcome::Failed => {}
    }
}

pub async fn discover_rooms<L>(shared: &Shared, fleet: &SharedFleet<L>, clock: &dyn Clock)
where
    L: LiveKitApi + Fleet + Send + Sync,
{
    let now = clock.now();
    let locations = shared.with_state(|state| {
        state
            .topology
            .servers()
            .iter()
            .map(|server| server.location().clone())
            .collect::<Vec<Location>>()
    });
    if locations.is_empty() {
        return;
    }

    let admitted = shared.with_state_mut(|state| {
        let mut admitted: Vec<Location> = Vec::new();
        if !state.governor.livekit_inflight().try_acquire() {
            return admitted;
        }
        for location in &locations {
            if !state.server_health.may_probe(location, now) {
                continue;
            }
            if !state.governor.server_inflight(location).try_acquire() {
                continue;
            }
            if !state
                .governor
                .try_acquire_server_read(location, DISCOVERY_LIST_ROOMS_COST, now)
            {
                state.governor.server_inflight(location).release();
                continue;
            }
            if !state.governor.try_acquire_many(
                BudgetClass::LiveKitRead,
                DISCOVERY_LIST_ROOMS_COST,
                now,
            ) {
                state
                    .governor
                    .refund_server_read(location, DISCOVERY_LIST_ROOMS_COST);
                state.governor.server_inflight(location).release();
                break;
            }
            admitted.push(location.clone());
        }
        if admitted.is_empty() {
            state.governor.livekit_inflight().release();
        }
        admitted
    });

    let results = list_fleet_rooms(fleet, &admitted, |_| true).await;
    let at = clock.now();

    for (_, list) in &results {
        let outcome = match list {
            ServerRoomList::Listed { .. } => "ok",
            ServerRoomList::Unreadable(fault) => fault.label(),
        };
        shared.metrics().record_livekit_call("list_rooms", outcome);
    }

    shared.with_state_mut(|state| {
        if !admitted.is_empty() {
            state.governor.livekit_inflight().release();
        }
        for location in &admitted {
            state.governor.server_inflight(location).release();
        }
    });

    shared.with_state_mut(|state| {
        for (location, list) in &results {
            state.server_health.record(location, list.outcome(), at);
            if list.fault().is_some_and(LiveKitFault::is_auth_failure) {
                state.topology_refresh_requested = true;
            }
            let filled = list.is_complete()
                && state
                    .suspicion
                    .hole_for(location)
                    .is_some_and(|hole| hole.since().get() <= now.get())
                && state.suspicion.clear_hole(location);
            if filled {
                tracing::info!(
                    region = location.region.as_str(),
                    server = location.server.as_str(),
                    "a full room list read after the dropped webhook closed that server's hole"
                );
            }
        }
        let pass = state.directory.apply_pass(&results, at);
        for room in state.directory.drain_relocated() {
            state
                .suspicion
                .note_room(room, SuspicionSource::Discovery, at);
        }
        if pass.unreadable_servers() > 0 {
            tracing::debug!(
                servers = pass.servers,
                unreadable = pass.unreadable_servers(),
                "a discovery pass was incomplete, so no location was withdrawn from those servers"
            );
        }
    });
}

pub async fn connect_topology(service: &ServiceConfig) -> anyhow::Result<Arc<dyn TopologyStore>> {
    match service.database_backend {
        DatabaseBackend::Postgres => {
            let config = fluxer_svc::postgres::PostgresConfig::from_service_config(service);
            let store = crate::topology::postgres::PostgresTopology::connect(&config).await?;
            Ok(Arc::new(store))
        }
        DatabaseBackend::Cassandra => connect_scylla_topology(service).await,
    }
}

#[cfg(feature = "scylla")]
async fn connect_scylla_topology(
    service: &ServiceConfig,
) -> anyhow::Result<Arc<dyn TopologyStore>> {
    let config = fluxer_svc::scylla::ScyllaConfig::from_service_config(service);
    let store = crate::topology::scylla::ScyllaTopology::connect(&config).await?;
    Ok(Arc::new(store))
}

#[cfg(not(feature = "scylla"))]
async fn connect_scylla_topology(
    _service: &ServiceConfig,
) -> anyhow::Result<Arc<dyn TopologyStore>> {
    anyhow::bail!(
        "FLUXER_DATABASE_BACKEND selects cassandra but this binary was built without the scylla feature"
    )
}

pub fn build_livekit(
    servers: &[VoiceServer],
    internal: &InternalEndpoint,
) -> anyhow::Result<TwirpLiveKit> {
    TwirpLiveKit::new(credentials_of(servers, internal), TwirpTimeouts::DEFAULT)
}
