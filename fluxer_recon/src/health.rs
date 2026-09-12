// SPDX-License-Identifier: AGPL-3.0-or-later

use std::collections::BTreeMap;

use crate::ids::{Location, Millis};

pub const DEGRADED_AT_FAILURES: u32 = 3;
pub const UNREACHABLE_AT_FAILURES: u32 = 10;
pub const QUARANTINED_AT_FAILURES: u32 = 30;
pub const SUCCESSES_TO_RECOVER: u32 = 3;
pub const UNREACHABLE_PROBE_INTERVAL_MS: u64 = 10_000;
pub const QUARANTINED_PROBE_INTERVAL_MS: u64 = 60_000;
pub const MUTATIONS_PAUSED_BACKOFF_MS: u64 = 15_000;
pub const MUTATIONS_PAUSED_MAX_BACKOFF_MS: u64 = 120_000;
pub const MUTATIONS_PAUSED_QUIET_MS: u64 = 300_000;
pub const MUTATIONS_PAUSED_MAX_DOUBLINGS: u32 = 3;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ReconMode {
    Halted,
    Observing,
    Constructive,
    Enforcing,
}

impl ReconMode {
    pub fn parse(value: &str) -> Option<Self> {
        match value.trim().to_ascii_lowercase().as_str() {
            "halted" => Some(Self::Halted),
            "observing" => Some(Self::Observing),
            "constructive" => Some(Self::Constructive),
            "enforcing" => Some(Self::Enforcing),
            _ => None,
        }
    }

    pub const fn as_str(self) -> &'static str {
        match self {
            Self::Halted => "halted",
            Self::Observing => "observing",
            Self::Constructive => "constructive",
            Self::Enforcing => "enforcing",
        }
    }

    pub const fn allows_constructive(self) -> bool {
        matches!(self, Self::Constructive | Self::Enforcing)
    }

    pub const fn allows_destructive(self) -> bool {
        matches!(self, Self::Enforcing)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ModeClamp {
    Warmup,
    ColdStart,
    BreakerTripped,
    SingletonConflict,
    TopologyStale,
    NatsReconnect,
    MutationsPaused,
    CoverageOverrun,
}

impl ModeClamp {
    pub const ALL: [Self; 8] = [
        Self::Warmup,
        Self::ColdStart,
        Self::BreakerTripped,
        Self::SingletonConflict,
        Self::TopologyStale,
        Self::NatsReconnect,
        Self::MutationsPaused,
        Self::CoverageOverrun,
    ];

    pub const fn label(self) -> &'static str {
        match self {
            Self::Warmup => "warmup",
            Self::ColdStart => "cold_start",
            Self::BreakerTripped => "breaker_tripped",
            Self::SingletonConflict => "singleton_conflict",
            Self::TopologyStale => "topology_stale",
            Self::NatsReconnect => "nats_reconnect",
            Self::MutationsPaused => "mutations_paused",
            Self::CoverageOverrun => "coverage_overrun",
        }
    }

    pub fn parse(value: &str) -> Option<Self> {
        let wanted = value.trim().to_ascii_lowercase();
        Self::ALL.into_iter().find(|clamp| clamp.label() == wanted)
    }

    pub const fn is_self_clearing(self) -> bool {
        match self {
            Self::Warmup
            | Self::ColdStart
            | Self::SingletonConflict
            | Self::TopologyStale
            | Self::NatsReconnect
            | Self::MutationsPaused
            | Self::CoverageOverrun => true,
            Self::BreakerTripped => false,
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct ModeClamps {
    active: BTreeMap<ModeClamp, Option<Millis>>,
}

impl ModeClamps {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn set(&mut self, clamp: ModeClamp, engaged: bool) {
        if engaged {
            self.active.insert(clamp, None);
        } else {
            self.active.remove(&clamp);
        }
    }

    pub fn engage_until(&mut self, clamp: ModeClamp, until: Millis) {
        self.active.insert(clamp, Some(until));
    }

    pub fn release_expired(&mut self, now: Millis) {
        self.active
            .retain(|_, until| until.is_none_or(|deadline| now < deadline));
    }

    pub fn is_engaged(&self, clamp: ModeClamp) -> bool {
        self.active.contains_key(&clamp)
    }

    pub fn deadline(&self, clamp: ModeClamp) -> Option<Millis> {
        self.active.get(&clamp).copied().flatten()
    }

    pub fn release(&mut self, clamp: ModeClamp) -> bool {
        self.active.remove(&clamp).is_some()
    }

    pub fn engaged(&self) -> impl Iterator<Item = ModeClamp> + '_ {
        self.active.keys().copied()
    }

    pub fn is_empty(&self) -> bool {
        self.active.is_empty()
    }

    pub fn ceiling(&self) -> ReconMode {
        if self.active.is_empty() {
            ReconMode::Enforcing
        } else {
            ReconMode::Constructive
        }
    }
}

pub fn effective_mode(
    configured: ReconMode,
    runtime_override: Option<ReconMode>,
    clamps: &ModeClamps,
) -> ReconMode {
    let requested = match runtime_override {
        None => configured,
        Some(override_mode) => configured.min(override_mode),
    };
    requested.min(clamps.ceiling())
}

pub const fn mutations_paused_hold_ms(windows: u32) -> u64 {
    if windows == 0 {
        return 0;
    }
    let doublings = if windows - 1 > MUTATIONS_PAUSED_MAX_DOUBLINGS {
        MUTATIONS_PAUSED_MAX_DOUBLINGS
    } else {
        windows - 1
    };
    let hold = MUTATIONS_PAUSED_BACKOFF_MS.saturating_mul(1u64 << doublings);
    if hold > MUTATIONS_PAUSED_MAX_BACKOFF_MS {
        MUTATIONS_PAUSED_MAX_BACKOFF_MS
    } else {
        hold
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct PauseBackoff {
    windows: u32,
    until: Option<Millis>,
}

impl PauseBackoff {
    pub const fn new() -> Self {
        Self {
            windows: 0,
            until: None,
        }
    }

    pub const fn windows(&self) -> u32 {
        self.windows
    }

    pub const fn until(&self) -> Option<Millis> {
        self.until
    }

    pub const fn hold_ms(&self) -> u64 {
        mutations_paused_hold_ms(self.windows)
    }

    pub const fn is_holding(&self, now: Millis) -> bool {
        match self.until {
            None => false,
            Some(until) => now.get() < until.get(),
        }
    }

    pub const fn remaining_ms(&self, now: Millis) -> u64 {
        match self.until {
            None => 0,
            Some(until) => until.saturating_since(now),
        }
    }

    pub const fn note_refused(&mut self, now: Millis) -> Option<Millis> {
        if self.is_holding(now) {
            return None;
        }
        self.windows = self.windows.saturating_add(1);
        let until = now.saturating_add_millis(mutations_paused_hold_ms(self.windows));
        self.until = Some(until);
        Some(until)
    }

    pub const fn clear(&mut self) {
        self.windows = 0;
        self.until = None;
    }

    pub const fn decay(&mut self, now: Millis) -> bool {
        let Some(until) = self.until else {
            return false;
        };
        if now.get() < until.get() {
            return false;
        }
        if now.saturating_since(until) < MUTATIONS_PAUSED_QUIET_MS {
            return false;
        }
        self.clear();
        true
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct WarmupGate {
    started_at: Millis,
    warmup_ms: u64,
    coverage_pass_complete: bool,
}

impl WarmupGate {
    pub const fn new(started_at: Millis, warmup_ms: u64) -> Self {
        Self {
            started_at,
            warmup_ms,
            coverage_pass_complete: false,
        }
    }

    pub const fn note_coverage_pass(&mut self) {
        self.coverage_pass_complete = true;
    }

    pub const fn coverage_pass_complete(&self) -> bool {
        self.coverage_pass_complete
    }

    pub const fn elapsed_complete(&self, now: Millis) -> bool {
        now.saturating_since(self.started_at) >= self.warmup_ms
    }

    pub const fn is_complete(&self, now: Millis) -> bool {
        self.elapsed_complete(now) && self.coverage_pass_complete
    }

    pub const fn force_complete(&mut self) {
        self.warmup_ms = 0;
        self.coverage_pass_complete = true;
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ServerHealth {
    Healthy,
    Degraded,
    Unreachable,
    Quarantined,
}

impl ServerHealth {
    pub const fn label(self) -> &'static str {
        match self {
            Self::Healthy => "healthy",
            Self::Degraded => "degraded",
            Self::Unreachable => "unreachable",
            Self::Quarantined => "quarantined",
        }
    }

    pub const fn is_healthy(self) -> bool {
        matches!(self, Self::Healthy)
    }

    pub const fn is_written_off(self) -> bool {
        matches!(self, Self::Unreachable | Self::Quarantined)
    }

    pub const fn severity(self) -> u8 {
        match self {
            Self::Healthy => 0,
            Self::Degraded => 1,
            Self::Unreachable => 2,
            Self::Quarantined => 3,
        }
    }

    pub const fn worse_of(self, other: Self) -> Self {
        if self.severity() >= other.severity() {
            self
        } else {
            other
        }
    }

    pub const fn probe_interval_ms(self) -> Option<u64> {
        match self {
            Self::Healthy | Self::Degraded => None,
            Self::Unreachable => Some(UNREACHABLE_PROBE_INTERVAL_MS),
            Self::Quarantined => Some(QUARANTINED_PROBE_INTERVAL_MS),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ProbeOutcome {
    Success,
    Failure,
    AuthFailure,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ServerHealthTracker {
    state: ServerHealth,
    consecutive_failures: u32,
    consecutive_successes: u32,
    last_probe_at: Option<Millis>,
    ever_answered: bool,
}

impl Default for ServerHealthTracker {
    fn default() -> Self {
        Self::new()
    }
}

impl ServerHealthTracker {
    pub const fn new() -> Self {
        Self {
            state: ServerHealth::Healthy,
            consecutive_failures: 0,
            consecutive_successes: 0,
            last_probe_at: None,
            ever_answered: false,
        }
    }

    pub const fn state(&self) -> ServerHealth {
        self.state
    }

    pub const fn consecutive_failures(&self) -> u32 {
        self.consecutive_failures
    }

    pub const fn ever_answered(&self) -> bool {
        self.ever_answered
    }

    pub const fn record(&mut self, outcome: ProbeOutcome, now: Millis) {
        self.last_probe_at = Some(now);
        match outcome {
            ProbeOutcome::Success => {
                self.ever_answered = true;
                self.consecutive_failures = 0;
                self.consecutive_successes = self.consecutive_successes.saturating_add(1);
                if self.consecutive_successes >= SUCCESSES_TO_RECOVER {
                    self.state = ServerHealth::Healthy;
                }
            }
            ProbeOutcome::Failure => {
                self.consecutive_successes = 0;
                self.consecutive_failures = self.consecutive_failures.saturating_add(1);
                self.state = self
                    .state
                    .worse_of(failure_state(self.consecutive_failures));
            }
            ProbeOutcome::AuthFailure => {
                self.consecutive_successes = 0;
                self.consecutive_failures = self.consecutive_failures.saturating_add(1);
                self.state = ServerHealth::Quarantined;
            }
        }
    }

    pub const fn may_probe(&self, now: Millis) -> bool {
        match (self.state.probe_interval_ms(), self.last_probe_at) {
            (None, _) => true,
            (Some(_), None) => true,
            (Some(interval), Some(last)) => now.saturating_since(last) >= interval,
        }
    }

    pub const fn last_probe_at(&self) -> Option<Millis> {
        self.last_probe_at
    }
}

const fn failure_state(consecutive_failures: u32) -> ServerHealth {
    if consecutive_failures >= QUARANTINED_AT_FAILURES {
        ServerHealth::Quarantined
    } else if consecutive_failures >= UNREACHABLE_AT_FAILURES {
        ServerHealth::Unreachable
    } else if consecutive_failures >= DEGRADED_AT_FAILURES {
        ServerHealth::Degraded
    } else {
        ServerHealth::Healthy
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct ServerHealthMap {
    servers: BTreeMap<Location, ServerHealthTracker>,
}

impl ServerHealthMap {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn record(&mut self, location: &Location, outcome: ProbeOutcome, now: Millis) {
        self.servers
            .entry(location.clone())
            .or_default()
            .record(outcome, now);
    }

    pub fn health(&self, location: &Location) -> ServerHealth {
        self.servers
            .get(location)
            .map_or(ServerHealth::Healthy, ServerHealthTracker::state)
    }

    pub fn ever_answered(&self, location: &Location) -> bool {
        self.servers
            .get(location)
            .is_some_and(ServerHealthTracker::ever_answered)
    }

    pub fn may_probe(&self, location: &Location, now: Millis) -> bool {
        self.servers
            .get(location)
            .is_none_or(|tracker| tracker.may_probe(now))
    }

    pub fn tracked(&self) -> impl Iterator<Item = (&Location, &ServerHealthTracker)> {
        self.servers.iter()
    }

    pub fn forget(&mut self, location: &Location) {
        self.servers.remove(location);
    }

    pub fn len(&self) -> usize {
        self.servers.len()
    }

    pub fn is_empty(&self) -> bool {
        self.servers.is_empty()
    }
}
