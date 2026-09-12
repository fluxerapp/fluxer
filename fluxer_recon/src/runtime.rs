// SPDX-License-Identifier: AGPL-3.0-or-later

use std::collections::{BTreeMap, BTreeSet};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex, MutexGuard, PoisonError};
use std::time::{SystemTime, UNIX_EPOCH};

use fluxer_svc::config::ServiceConfig;

use crate::budget::{BreakerEvent, BreakerLimits, Governor, GovernorLimits};
use crate::census::{CensusCache, CensusLimits};
use crate::config::ReconConfig;
use crate::decide::DecideParams;
use crate::discovery::{DirectoryLimits, PassCounters, RoomDirectory, ServerCliffWatch};
use crate::health::{ModeClamp, PauseBackoff, ReconMode, ServerHealthMap, WarmupGate};
use crate::ids::{Millis, RoomKey, TurnId};
use crate::ledger::{ConnectionState, Ledger, LedgerLimits, MAP_ENTRY_OVERHEAD_BYTES};
use crate::metrics::ReconMetrics;
use crate::observe::DecisionJournal;
use crate::schedule::{
    CoverageInputs, Scheduler, SchedulerConfig, derived_coverage_period_ms, max_auditable_rooms,
};
use crate::suspicion::SuspicionLane;
use crate::topology::{TopologyCache, TopologyLimits};
use crate::turn::TurnSequencer;

pub use crate::singleton::{
    InstanceAnnounce, InstanceIdentity, InstanceProbe, PEER_CLAMP_RELEASE_AFTER_MS, PeerRecord,
    PeerVerdict, SINGLETON_PROBE_DEADLINE_MS, SINGLETON_PROBE_INTERVAL_MS, SingletonLayer,
    SingletonResponse, peer_verdict, run_singleton, should_answer_probe,
};

pub const SERVICE_NAME: &str = "recon";
pub const RETIRED_GRACE_MS: u64 = 60_000;

pub const RUNTIME_TASKS: [&str; 6] = [
    "http",
    "engine",
    "census",
    "discovery",
    "control",
    "singleton",
];
pub const WEBHOOK_TASK: &str = "webhook";
pub const OPTIONAL_TASKS: [&str; 1] = [WEBHOOK_TASK];
pub const HOUSEKEEP_INTERVAL_MS: u64 = 1_000;

pub fn runtime_tasks(config: &ReconConfig) -> Vec<&'static str> {
    let mut names = RUNTIME_TASKS.to_vec();
    if config.webhook_enabled {
        names.push(WEBHOOK_TASK);
    }
    names
}

pub fn monotonic_now() -> Millis {
    Millis::new(fluxer_svc::metrics::now_ms().max(0) as u64)
}

pub fn wall_now_ms() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_or(0, |elapsed| {
            elapsed.as_millis().min(u128::from(u64::MAX)) as u64
        })
}

#[derive(Debug)]
pub struct Readiness {
    flag: Arc<AtomicBool>,
    has_been_ready: AtomicBool,
    draining: AtomicBool,
}

impl Default for Readiness {
    fn default() -> Self {
        Self::new()
    }
}

impl Readiness {
    pub fn new() -> Self {
        Self {
            flag: Arc::new(AtomicBool::new(false)),
            has_been_ready: AtomicBool::new(false),
            draining: AtomicBool::new(false),
        }
    }

    pub fn flag(&self) -> Arc<AtomicBool> {
        Arc::clone(&self.flag)
    }

    pub fn mark_ready(&self) -> bool {
        if self.draining.load(Ordering::SeqCst) {
            return false;
        }
        self.flag.store(true, Ordering::SeqCst);
        !self.has_been_ready.swap(true, Ordering::SeqCst)
    }

    pub fn is_ready(&self) -> bool {
        self.flag.load(Ordering::SeqCst)
    }

    pub fn has_been_ready(&self) -> bool {
        self.has_been_ready.load(Ordering::SeqCst)
    }

    pub fn begin_shutdown(&self) {
        self.draining.store(true, Ordering::SeqCst);
        self.flag.store(false, Ordering::SeqCst);
    }

    pub fn is_draining(&self) -> bool {
        self.draining.load(Ordering::SeqCst)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ModeTransition {
    pub configured: ReconMode,
    pub previous_override: Option<ReconMode>,
    pub requested: Option<ReconMode>,
    pub effective_before: ReconMode,
    pub effective_after: ReconMode,
}

impl ModeTransition {
    pub fn changed(&self) -> bool {
        self.effective_before != self.effective_after
    }

    pub fn escalated(&self) -> bool {
        self.effective_after > self.effective_before
    }
}

pub fn governor_limits(config: &ReconConfig) -> GovernorLimits {
    GovernorLimits {
        gateway_read_rps: config.gateway_read_rps,
        gateway_mutate_rpm: config.gateway_mutate_rpm,
        livekit_read_rps: config.livekit_read_rps,
        livekit_read_rps_per_server: config.livekit_read_rps_per_server,
        livekit_mutate_rpm: config.livekit_mutate_rpm,
        max_inflight_room_turns: config.max_inflight_room_turns,
        max_inflight_gateway: config.max_inflight_gateway,
        max_inflight_livekit: config.max_inflight_livekit,
        max_inflight_per_server: config.max_inflight_per_server,
        max_mutations_per_room_per_min: config.max_mutations_per_room_per_min,
        max_mutations_per_guild_per_min: config.max_mutations_per_guild_per_min,
        max_mutations_per_min: config.max_mutations_per_min,
        max_action_attempts: config.max_action_attempts,
        preflight_max_age_ms: config.preflight_max_age_ms,
        breaker: BreakerLimits {
            max_divergent_fraction: config.max_divergent_fraction,
            auto_reset_ms: config.breaker_auto_reset_ms,
            max_auto_resets: config.max_breaker_auto_resets,
        },
    }
}

pub fn ledger_limits(config: &ReconConfig) -> LedgerLimits {
    LedgerLimits {
        max_tracked_rooms: config.max_tracked_rooms,
        max_tracked_connections: config.max_tracked_connections,
        max_connections_per_room: config.max_connections_per_room,
        memory_budget_bytes: config.memory_budget_bytes,
        retired_grace_ms: RETIRED_GRACE_MS,
    }
}

#[derive(Debug)]
pub struct RuntimeState {
    pub governor: Governor,
    pub ledger: Ledger,
    pub server_health: ServerHealthMap,
    pub turn: TurnId,
    pub sequencer: TurnSequencer,
    pub scheduler: Scheduler,
    pub directory: RoomDirectory,
    pub census: CensusCache,
    pub topology: TopologyCache,
    pub suspicion: SuspicionLane,
    pub cliffs: ServerCliffWatch,
    pub counters: PassCounters,
    pub pass_started_at: Millis,
    pub pass_pending: BTreeSet<RoomKey>,
    pub topology_loaded_at: Option<Millis>,
    pub topology_servers: usize,
    pub census_at: Option<Millis>,
    pub coverage_period_ms: u64,
    pub peer: Option<PeerRecord>,
    pub unknown_since: BTreeMap<RoomKey, Millis>,
    pub last_housekeep_at: Option<Millis>,
    pub topology_refresh_requested: bool,
    pub mutations_pause: PauseBackoff,
}

impl RuntimeState {
    pub fn new(config: &ReconConfig, now: Millis) -> Self {
        let warmup = WarmupGate::new(now, config.warmup_seconds.saturating_mul(1_000));
        let mut governor = Governor::new(config.mode, governor_limits(config), warmup, now);
        let clamps = governor.clamps_mut();
        clamps.set(ModeClamp::Warmup, true);
        clamps.set(ModeClamp::ColdStart, true);
        clamps.set(ModeClamp::TopologyStale, true);
        Self {
            governor,
            ledger: Ledger::new(ledger_limits(config), DecideParams::from_config(config)),
            server_health: ServerHealthMap::new(),
            turn: TurnId::FIRST,
            sequencer: TurnSequencer::new(),
            scheduler: Scheduler::new(SchedulerConfig::from_config(config)),
            directory: RoomDirectory::new(DirectoryLimits::from_config(config)),
            census: CensusCache::new(CensusLimits::from_config(config)),
            topology: TopologyCache::new(TopologyLimits::from_config(config)),
            suspicion: SuspicionLane::from_config(config),
            cliffs: ServerCliffWatch::from_config(config),
            counters: PassCounters::new(),
            pass_started_at: now,
            pass_pending: BTreeSet::new(),
            topology_loaded_at: None,
            topology_servers: 0,
            census_at: None,
            coverage_period_ms: config.coverage_target_ms,
            peer: None,
            unknown_since: BTreeMap::new(),
            last_housekeep_at: None,
            topology_refresh_requested: false,
            mutations_pause: PauseBackoff::new(),
        }
    }

    pub fn forget_room(&mut self, room: &RoomKey) {
        self.directory.forget(room);
        self.scheduler.forget(room);
        self.sequencer.forget(room);
        self.suspicion.forget(room);
        self.pass_pending.remove(room);
        self.unknown_since.remove(room);
        self.governor.forget_room(room);
    }

    pub fn tracked_bytes(&self) -> u64 {
        let room_key_entry = (size_of::<RoomKey>() + MAP_ENTRY_OVERHEAD_BYTES) as u64;
        self.ledger
            .tracked_bytes()
            .saturating_add(self.directory.tracked_bytes())
            .saturating_add(self.scheduler.tracked_bytes())
            .saturating_add(self.sequencer.tracked_bytes())
            .saturating_add(self.suspicion.tracked_bytes())
            .saturating_add(self.governor.mutations().tracked_bytes())
            .saturating_add(self.census.tracked_bytes())
            .saturating_add((self.pass_pending.len() as u64).saturating_mul(room_key_entry))
            .saturating_add(
                (self.unknown_since.len() as u64)
                    .saturating_mul(room_key_entry.saturating_add(size_of::<Millis>() as u64)),
            )
    }

    pub const fn take_topology_refresh_request(&mut self) -> bool {
        let requested = self.topology_refresh_requested;
        self.topology_refresh_requested = false;
        requested
    }

    pub fn next_turn(&mut self) -> TurnId {
        self.turn = self.turn.next();
        self.turn
    }

    pub fn topology_age_ms(&self, now: Millis) -> Option<u64> {
        self.topology_loaded_at
            .map(|loaded| now.saturating_since(loaded))
    }

    pub fn census_age_ms(&self, now: Millis) -> Option<u64> {
        self.census_at.map(|at| now.saturating_since(at))
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TickInputs {
    pub now: Millis,
    pub nats_connected: bool,
    pub last_decision_at: Option<Millis>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct HousekeepReport {
    pub effective_mode: ReconMode,
    pub breaker: BreakerEvent,
    pub divergent_fraction: f64,
    pub pruned_connections: usize,
    pub dropped_rooms: usize,
    pub evicted_rooms: usize,
    pub unknown_rooms: usize,
    pub wedged_connections: usize,
}

pub const ROOM_RETENTION_MS: u64 = 600_000;

fn last_seen_by_any_source(provenance: &crate::discovery::RoomProvenance) -> Option<Millis> {
    provenance
        .sources()
        .into_iter()
        .filter_map(|source| provenance.last_seen_by(source))
        .max()
}

fn prune_room_set(state: &mut RuntimeState, now: Millis) {
    let ledger = &state.ledger;
    let scheduler = &mut state.scheduler;
    let sequencer = &mut state.sequencer;
    let suspicion = &mut state.suspicion;
    let pending = &mut state.pass_pending;

    let mut forgotten: Vec<RoomKey> = Vec::new();
    state.directory.retain(|room, provenance| {
        if ledger.room(room).is_some() {
            return true;
        }
        let cold = last_seen_by_any_source(provenance)
            .is_none_or(|seen| now.saturating_since(seen) > ROOM_RETENTION_MS);
        if cold {
            forgotten.push(*room);
        }
        !cold
    });

    for room in &forgotten {
        scheduler.forget(room);
        sequencer.forget(room);
        suspicion.forget(room);
        pending.remove(room);
    }

    pending.retain(|room| scheduler.is_tracked(room));

    for room in &forgotten {
        state.governor.forget_room(room);
        state.unknown_since.remove(room);
    }
}

pub const MAX_EVICTIONS_PER_HOUSEKEEP: usize = 256;

fn evict_to_budget(state: &mut RuntimeState) -> usize {
    let limits = state.ledger.limits();
    let mut evicted = 0;

    while evicted < MAX_EVICTIONS_PER_HOUSEKEEP
        && (state.tracked_bytes() > limits.memory_budget_bytes
            || state.ledger.tracked_rooms() > limits.max_tracked_rooms
            || state.ledger.tracked_connections() > limits.max_tracked_connections)
    {
        let Some(victim) = state.ledger.coldest_room() else {
            break;
        };
        if !state.ledger.evict_room(&victim) {
            break;
        }
        state.forget_room(&victim);
        evicted += 1;
    }

    evicted
}

fn room_is_unknown(room: &crate::ledger::RoomLedger, config: &ReconConfig, now: Millis) -> bool {
    room.no_candidates()
        || room.partially_unreadable()
        || room.gateway_cliff().is_held(now, config.room_cliff_hold_ms)
        || room.media_cliff().is_held(now, config.server_cliff_hold_ms)
}

pub fn housekeep(
    state: &mut RuntimeState,
    config: &ReconConfig,
    metrics: &ReconMetrics,
    inputs: TickInputs,
) -> HousekeepReport {
    let now = inputs.now;

    state.governor.clamps_mut().release_expired(now);
    state.mutations_pause.decay(now);

    let warmup_pending = !state.governor.warmup().elapsed_complete(now);
    let coverage_pending = !state.governor.warmup().coverage_pass_complete();
    let topology_stale = state
        .topology_age_ms(now)
        .is_none_or(|age| age > config.topology_max_age_ms);
    let peer_present = state
        .peer
        .as_ref()
        .is_some_and(|peer| now.saturating_since(peer.last_seen_at) < PEER_CLAMP_RELEASE_AFTER_MS);

    {
        let clamps = state.governor.clamps_mut();
        clamps.set(ModeClamp::Warmup, warmup_pending);
        clamps.set(ModeClamp::ColdStart, coverage_pending);
        clamps.set(ModeClamp::NatsReconnect, !inputs.nats_connected);
        clamps.set(ModeClamp::TopologyStale, topology_stale);
        clamps.set(ModeClamp::SingletonConflict, peer_present);
    }

    let divergent_fraction = state.ledger.divergent_fraction();
    let breaker = state.governor.observe_divergence(divergent_fraction, now);
    if matches!(breaker, BreakerEvent::Tripped) {
        metrics.record_breaker_trip();
        state.ledger.reset_all_corroborations();
    }

    let pruned_connections = state.ledger.prune_retired(now);
    let dropped_rooms = state.ledger.drop_empty_rooms();
    state.governor.prune_mutations(now);
    let evicted_rooms = evict_to_budget(state);

    state.suspicion.expire(now);
    state.scheduler.expire_hot(now);
    prune_room_set(state, now);

    let live_servers = state.topology.lens(now).live_locations();
    state.governor.retain_servers(&live_servers);

    let mut no_candidate_rooms = 0;
    let mut wedged_connections = 0;
    let mut oldest_staleness_ms = 0;
    let mut unknown_keys: Vec<RoomKey> = Vec::new();

    for (key, room) in state.ledger.rooms() {
        oldest_staleness_ms = oldest_staleness_ms.max(now.saturating_since(room.last_seen()));
        if room.no_candidates() {
            no_candidate_rooms += 1;
        }
        if room_is_unknown(room, config, now) {
            unknown_keys.push(*key);
        }
        wedged_connections += room
            .connections()
            .iter()
            .filter(|entry| matches!(entry.state, ConnectionState::Wedged { .. }))
            .count();
    }

    let unknown_rooms = unknown_keys.len();
    state
        .unknown_since
        .retain(|key, _| unknown_keys.binary_search(key).is_ok());
    for key in unknown_keys {
        state.unknown_since.entry(key).or_insert(now);
    }

    let oldest_unknown_ms = state
        .unknown_since
        .values()
        .map(|since| now.saturating_since(*since))
        .max()
        .unwrap_or(0);

    let tracked_rooms = state.ledger.tracked_rooms();
    let effective_mode = state.governor.effective_mode();

    metrics.set_mode(effective_mode);
    for clamp in ModeClamp::ALL {
        metrics.set_mode_clamp(clamp, state.governor.clamps().is_engaged(clamp));
    }
    metrics.set_ledger(
        tracked_rooms,
        state.ledger.tracked_connections(),
        state.tracked_bytes(),
        state
            .ledger
            .evicted_rooms_total()
            .saturating_add(state.ledger.evicted_connections_total()),
    );
    metrics.set_wedged_connections(wedged_connections);
    metrics.set_rooms_no_candidates(no_candidate_rooms);
    metrics.set_rooms_unknown_ratio(if tracked_rooms == 0 {
        0.0
    } else {
        unknown_rooms as f64 / tracked_rooms as f64
    });
    metrics.set_oldest_unknown_room_seconds(oldest_unknown_ms as f64 / 1_000.0);
    metrics.set_room_staleness_seconds(oldest_staleness_ms as f64 / 1_000.0);
    metrics.set_coverage_period_seconds(state.coverage_period_ms as f64 / 1_000.0);
    metrics.set_topology_age_seconds(
        state
            .topology_age_ms(now)
            .map_or(f64::INFINITY, |age| age as f64 / 1_000.0),
    );
    metrics.set_census_age_seconds(
        state
            .census_age_ms(now)
            .map_or(f64::INFINITY, |age| age as f64 / 1_000.0),
    );
    metrics.set_last_decision_age_seconds(inputs.last_decision_at.map_or(f64::INFINITY, |at| {
        now.saturating_since(at) as f64 / 1_000.0
    }));
    for (location, tracker) in state.server_health.tracked() {
        metrics.set_server_health(location, tracker.state());
    }

    state.last_housekeep_at = Some(now);

    HousekeepReport {
        effective_mode,
        breaker,
        divergent_fraction,
        pruned_connections,
        dropped_rooms,
        evicted_rooms,
        unknown_rooms,
        wedged_connections,
    }
}

pub struct SharedInner {
    config: ReconConfig,
    instance: InstanceIdentity,
    metrics: Arc<ReconMetrics>,
    journal: DecisionJournal,
    readiness: Readiness,
    boot_at: Millis,
    state: Mutex<RuntimeState>,
}

#[derive(Clone)]
pub struct Shared(Arc<SharedInner>);

impl Shared {
    pub fn new(config: ReconConfig, instance: InstanceIdentity, now: Millis) -> Self {
        let state = RuntimeState::new(&config, now);
        Self(Arc::new(SharedInner {
            config,
            instance,
            metrics: Arc::new(ReconMetrics::new()),
            journal: DecisionJournal::default(),
            readiness: Readiness::new(),
            boot_at: now,
            state: Mutex::new(state),
        }))
    }

    pub fn config(&self) -> &ReconConfig {
        &self.0.config
    }

    pub fn instance(&self) -> &InstanceIdentity {
        &self.0.instance
    }

    pub fn metrics(&self) -> &Arc<ReconMetrics> {
        &self.0.metrics
    }

    pub fn journal(&self) -> &DecisionJournal {
        &self.0.journal
    }

    pub fn readiness(&self) -> &Readiness {
        &self.0.readiness
    }

    pub fn boot_at(&self) -> Millis {
        self.0.boot_at
    }

    fn lock(&self) -> MutexGuard<'_, RuntimeState> {
        self.0.state.lock().unwrap_or_else(PoisonError::into_inner)
    }

    pub fn with_state<R>(&self, action: impl FnOnce(&RuntimeState) -> R) -> R {
        action(&self.lock())
    }

    pub fn with_state_mut<R>(&self, action: impl FnOnce(&mut RuntimeState) -> R) -> R {
        action(&mut self.lock())
    }

    pub fn effective_mode(&self) -> ReconMode {
        self.with_state(|state| state.governor.effective_mode())
    }

    pub fn tick(&self, inputs: TickInputs) -> HousekeepReport {
        let config = self.0.config.clone();
        let metrics = Arc::clone(&self.0.metrics);
        self.with_state_mut(|state| housekeep(state, &config, &metrics, inputs))
    }

    pub fn tick_if_due(&self, inputs: TickInputs, interval_ms: u64) -> Option<HousekeepReport> {
        let due = self.with_state(|state| {
            state
                .last_housekeep_at
                .is_none_or(|last| inputs.now.saturating_since(last) >= interval_ms)
        });
        due.then(|| self.tick(inputs))
    }

    pub fn set_mode_override(&self, requested: Option<ReconMode>) -> ModeTransition {
        self.with_state_mut(|state| {
            let configured = state.governor.configured_mode();
            let previous_override = state.governor.runtime_override();
            let effective_before = state.governor.effective_mode();
            state.governor.set_runtime_override(requested);
            let effective_after = state.governor.effective_mode();
            ModeTransition {
                configured,
                previous_override,
                requested,
                effective_before,
                effective_after,
            }
        })
    }

    pub fn note_peer(&self, announce: &InstanceAnnounce, now: Millis) {
        self.0.metrics.record_peer_detected();
        self.with_state_mut(|state| {
            let detections = state
                .peer
                .as_ref()
                .filter(|peer| peer.instance_id == announce.instance_id)
                .map_or(0, |peer| peer.detections);
            let first_seen_at = state
                .peer
                .as_ref()
                .filter(|peer| peer.instance_id == announce.instance_id)
                .map_or(now, |peer| peer.first_seen_at);
            state.peer = Some(PeerRecord {
                instance_id: announce.instance_id.clone(),
                started_at: announce.started_at,
                first_seen_at,
                last_seen_at: now,
                detections: detections.saturating_add(1),
            });
            state
                .governor
                .clamps_mut()
                .set(ModeClamp::SingletonConflict, true);
        });
    }
}

pub fn assert_single_replica(
    service_name: &str,
    shard_count: u32,
    shard_id: u32,
) -> anyhow::Result<()> {
    if service_name != SERVICE_NAME {
        anyhow::bail!(
            "FLUXER_SVC_NAME must be {SERVICE_NAME} so the control subject and metric namespace agree, got {service_name}"
        );
    }
    if shard_count != 1 {
        anyhow::bail!(
            "FLUXER_SVC_SHARD_COUNT must be 1 for a single-replica service, got {shard_count}"
        );
    }
    if shard_id != 0 {
        anyhow::bail!("FLUXER_SVC_SHARD_ID must be 0 for a single-replica service, got {shard_id}");
    }
    Ok(())
}

pub fn assert_coverage_budget(config: &ReconConfig) -> anyhow::Result<()> {
    if config.coverage_target_ms > config.coverage_period_hard_cap_ms {
        anyhow::bail!(
            "FLUXER_RECON_COVERAGE_TARGET_MS ({}) exceeds FLUXER_RECON_COVERAGE_PERIOD_HARD_CAP_MS ({})",
            config.coverage_target_ms,
            config.coverage_period_hard_cap_ms
        );
    }
    if config.tick_ms == 0 {
        anyhow::bail!("FLUXER_RECON_TICK_MS must be greater than zero");
    }
    if config.worker_threads == 0 {
        anyhow::bail!("FLUXER_RECON_WORKER_THREADS must be greater than zero");
    }

    let auditable = max_auditable_rooms(config);
    let derived =
        derived_coverage_period_ms(&CoverageInputs::for_rooms(config.expected_rooms, config));
    if derived > config.coverage_period_hard_cap_ms {
        anyhow::bail!(
            "FLUXER_RECON_EXPECTED_ROOMS ({}) needs a coverage period of {derived} ms at the configured read rates, above FLUXER_RECON_COVERAGE_PERIOD_HARD_CAP_MS ({}); this process can audit {auditable} rooms within that cap",
            config.expected_rooms,
            config.coverage_period_hard_cap_ms
        );
    }

    tracing::info!(
        expected_rooms = config.expected_rooms,
        derived_coverage_period_ms = derived,
        max_auditable_rooms = auditable,
        "the configured read rates admit this many rooms inside the coverage hard cap"
    );
    Ok(())
}

pub fn boot_assertions(service: &ServiceConfig, config: &ReconConfig) -> anyhow::Result<()> {
    assert_single_replica(&service.service_name, service.shard_count, service.shard_id)?;
    assert_coverage_budget(config)
}
