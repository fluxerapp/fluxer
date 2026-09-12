// SPDX-License-Identifier: AGPL-3.0-or-later

use std::collections::BTreeMap;
use std::fmt::Write;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex, PoisonError};

use fluxer_svc::metrics::AdditionalMetricsRenderer;

use crate::decide::{Decision, DecisionAction};
use crate::evidence::SideKind;
use crate::guards::{AbortReason, UnknownMediaCause};
use crate::health::{ModeClamp, ReconMode, ServerHealth};
use crate::ids::{Location, Scope};
use crate::ledger::{ActionKind, RepairVerdict};

const PREFIX: &str = "fluxer_recon";
const ORDERING: Ordering = Ordering::Relaxed;

const DURATION_BUCKETS_MS: [u64; 13] = [
    1, 5, 10, 25, 50, 100, 250, 500, 1_000, 2_500, 5_000, 10_000, 30_000,
];

pub const DECISION_LABELS: [&str; 5] = [
    "hold",
    "confirm_connection",
    "repair_state",
    "remove_gateway_state",
    "remove_participant",
];

pub const REPAIR_OUTCOMES: [&str; 4] = ["repaired", "no_change", "not_repairable", "failed"];

pub const CONFIRM_OUTCOMES: [&str; 4] =
    ["confirmed", "already_confirmed", "call_not_found", "failed"];

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MutationOutcome {
    Issued,
    Succeeded,
    NoChange,
    Failed,
}

impl MutationOutcome {
    pub const ALL: [Self; 4] = [Self::Issued, Self::Succeeded, Self::NoChange, Self::Failed];

    pub const fn label(self) -> &'static str {
        match self {
            Self::Issued => "issued",
            Self::Succeeded => "succeeded",
            Self::NoChange => "no_change",
            Self::Failed => "failed",
        }
    }
}

pub const fn scope_label(scope: Scope) -> &'static str {
    match scope {
        Scope::Guild => "guild",
        Scope::Dm => "dm",
    }
}

#[derive(Debug, Default)]
struct Counter(AtomicU64);

impl Counter {
    fn increment(&self) {
        self.0.fetch_add(1, ORDERING);
    }

    fn set(&self, value: u64) {
        self.0.store(value, ORDERING);
    }

    fn get(&self) -> u64 {
        self.0.load(ORDERING)
    }
}

#[derive(Debug, Default)]
struct Gauge(AtomicU64);

impl Gauge {
    fn set(&self, value: f64) {
        self.0.store(value.to_bits(), ORDERING);
    }

    fn get(&self) -> f64 {
        f64::from_bits(self.0.load(ORDERING))
    }
}

#[derive(Debug, Default)]
struct LabelledSeries {
    values: Mutex<BTreeMap<String, f64>>,
}

impl LabelledSeries {
    fn seeded<I: IntoIterator<Item = String>>(labels: I) -> Self {
        let series = Self::default();
        for label in labels {
            series.set(label, 0.0);
        }
        series
    }

    fn lock(&self) -> std::sync::MutexGuard<'_, BTreeMap<String, f64>> {
        self.values.lock().unwrap_or_else(PoisonError::into_inner)
    }

    fn increment(&self, labels: String) {
        *self.lock().entry(labels).or_insert(0.0) += 1.0;
    }

    fn set(&self, labels: String, value: f64) {
        self.lock().insert(labels, value);
    }

    fn get(&self, labels: &str) -> Option<f64> {
        self.lock().get(labels).copied()
    }

    fn render(&self, out: &mut String, name: &str, kind: &str) {
        let values = self.lock();
        let _ = writeln!(out, "# TYPE {PREFIX}_{name} {kind}");
        for (labels, value) in values.iter() {
            let _ = writeln!(out, "{PREFIX}_{name}{{{labels}}} {value}");
        }
    }
}

#[derive(Debug)]
struct DurationHistogram {
    buckets: [AtomicU64; DURATION_BUCKETS_MS.len()],
    count: AtomicU64,
    sum_ms: AtomicU64,
}

impl Default for DurationHistogram {
    fn default() -> Self {
        Self {
            buckets: [const { AtomicU64::new(0) }; DURATION_BUCKETS_MS.len()],
            count: AtomicU64::new(0),
            sum_ms: AtomicU64::new(0),
        }
    }
}

impl DurationHistogram {
    fn observe(&self, ms: u64) {
        for (index, upper) in DURATION_BUCKETS_MS.iter().copied().enumerate() {
            if ms <= upper {
                self.buckets[index].fetch_add(1, ORDERING);
                break;
            }
        }
        self.count.fetch_add(1, ORDERING);
        self.sum_ms.fetch_add(ms, ORDERING);
    }

    fn render(&self, out: &mut String, name: &str) {
        let _ = writeln!(out, "# TYPE {PREFIX}_{name} histogram");
        let mut cumulative = 0;
        for (index, upper) in DURATION_BUCKETS_MS.iter().copied().enumerate() {
            cumulative += self.buckets[index].load(ORDERING);
            let _ = writeln!(out, "{PREFIX}_{name}_bucket{{le=\"{upper}\"}} {cumulative}");
        }
        let count = self.count.load(ORDERING);
        let _ = writeln!(out, "{PREFIX}_{name}_bucket{{le=\"+Inf\"}} {count}");
        let _ = writeln!(out, "{PREFIX}_{name}_sum {}", self.sum_ms.load(ORDERING));
        let _ = writeln!(out, "{PREFIX}_{name}_count {count}");
    }
}

fn escape_label(value: &str) -> String {
    let mut escaped = String::with_capacity(value.len());
    for character in value.chars() {
        match character {
            '\\' => escaped.push_str("\\\\"),
            '"' => escaped.push_str("\\\""),
            '\n' => escaped.push_str("\\n"),
            other => escaped.push(other),
        }
    }
    escaped
}

fn one_label(key: &str, value: &str) -> String {
    format!("{key}=\"{}\"", escape_label(value))
}

fn two_labels(first: (&str, &str), second: (&str, &str)) -> String {
    format!(
        "{}=\"{}\",{}=\"{}\"",
        first.0,
        escape_label(first.1),
        second.0,
        escape_label(second.1)
    )
}

fn three_labels(first: (&str, &str), second: (&str, &str), third: (&str, &str)) -> String {
    format!(
        "{}=\"{}\",{}=\"{}\",{}=\"{}\"",
        first.0,
        escape_label(first.1),
        second.0,
        escape_label(second.1),
        third.0,
        escape_label(third.1)
    )
}

#[derive(Debug)]
pub struct ReconMetrics {
    decisions: LabelledSeries,
    evictions_aborted: LabelledSeries,
    unknown_media_causes: LabelledSeries,
    mutations: LabelledSeries,
    repairs: LabelledSeries,
    confirms: LabelledSeries,
    unknown: LabelledSeries,
    gateway_rpc: LabelledSeries,
    livekit_calls: LabelledSeries,
    server_health: LabelledSeries,
    webhook_last_event_age_seconds: LabelledSeries,
    mode: LabelledSeries,
    mode_clamp: LabelledSeries,
    ledger_evicted_total: Counter,
    scheduler_starved_total: Counter,
    tripped_total: Counter,
    peer_detected_total: Counter,
    rooms_unknown_ratio: Gauge,
    oldest_unknown_room_seconds: Gauge,
    rooms_no_candidates: Gauge,
    wedged_connections: Gauge,
    ledger_rooms: Gauge,
    ledger_connections: Gauge,
    ledger_bytes: Gauge,
    coverage_period_seconds: Gauge,
    room_staleness_seconds: Gauge,
    topology_age_seconds: Gauge,
    census_age_seconds: Gauge,
    last_decision_age_seconds: Gauge,
    turn_duration: DurationHistogram,
    mutation_duration: DurationHistogram,
}

impl Default for ReconMetrics {
    fn default() -> Self {
        Self::new()
    }
}

impl ReconMetrics {
    pub fn new() -> Self {
        Self {
            decisions: LabelledSeries::seeded(
                DECISION_LABELS
                    .iter()
                    .map(|decision| one_label("decision", decision)),
            ),
            evictions_aborted: LabelledSeries::seeded(
                AbortReason::ALL
                    .iter()
                    .map(|reason| one_label("reason", reason.label())),
            ),
            unknown_media_causes: LabelledSeries::seeded(
                UnknownMediaCause::ALL
                    .iter()
                    .map(|cause| one_label("cause", cause.label())),
            ),
            mutations: LabelledSeries::default(),
            repairs: LabelledSeries::seeded(
                REPAIR_OUTCOMES
                    .iter()
                    .map(|outcome| one_label("outcome", outcome)),
            ),
            confirms: LabelledSeries::seeded(
                CONFIRM_OUTCOMES
                    .iter()
                    .map(|outcome| one_label("outcome", outcome)),
            ),
            unknown: LabelledSeries::default(),
            gateway_rpc: LabelledSeries::default(),
            livekit_calls: LabelledSeries::default(),
            server_health: LabelledSeries::default(),
            webhook_last_event_age_seconds: LabelledSeries::default(),
            mode: LabelledSeries::seeded(
                [
                    ReconMode::Halted,
                    ReconMode::Observing,
                    ReconMode::Constructive,
                    ReconMode::Enforcing,
                ]
                .iter()
                .map(|mode| one_label("mode", mode.as_str())),
            ),
            mode_clamp: LabelledSeries::seeded(
                ModeClamp::ALL
                    .iter()
                    .map(|clamp| one_label("clamp", clamp.label())),
            ),
            ledger_evicted_total: Counter::default(),
            scheduler_starved_total: Counter::default(),
            tripped_total: Counter::default(),
            peer_detected_total: Counter::default(),
            rooms_unknown_ratio: Gauge::default(),
            oldest_unknown_room_seconds: Gauge::default(),
            rooms_no_candidates: Gauge::default(),
            wedged_connections: Gauge::default(),
            ledger_rooms: Gauge::default(),
            ledger_connections: Gauge::default(),
            ledger_bytes: Gauge::default(),
            coverage_period_seconds: Gauge::default(),
            room_staleness_seconds: Gauge::default(),
            topology_age_seconds: Gauge::default(),
            census_age_seconds: Gauge::default(),
            last_decision_age_seconds: Gauge::default(),
            turn_duration: DurationHistogram::default(),
            mutation_duration: DurationHistogram::default(),
        }
    }

    pub fn renderer(metrics: &Arc<Self>) -> AdditionalMetricsRenderer {
        let metrics = Arc::clone(metrics);
        Arc::new(move |out: &mut String| metrics.render(out))
    }

    pub fn record_decision(&self, decision: &Decision) {
        self.decisions
            .increment(one_label("decision", decision.action.label()));
        if let Some(reason) = decision.blocked_by
            && decision.is_destructive()
        {
            self.evictions_aborted
                .increment(one_label("reason", reason.label()));
        }
    }

    pub fn record_eviction_aborted(&self, reason: AbortReason) {
        self.evictions_aborted
            .increment(one_label("reason", reason.label()));
    }

    pub fn record_unknown_media_cause(&self, cause: UnknownMediaCause) {
        self.unknown_media_causes
            .increment(one_label("cause", cause.label()));
    }

    pub fn unknown_media_cause_count(&self, cause: UnknownMediaCause) -> u64 {
        self.unknown_media_causes
            .get(&one_label("cause", cause.label()))
            .unwrap_or(0.0) as u64
    }

    pub fn record_mutation(&self, kind: ActionKind, scope: Scope, outcome: MutationOutcome) {
        self.mutations.increment(three_labels(
            ("kind", kind.label()),
            ("scope", scope_label(scope)),
            ("outcome", outcome.label()),
        ));
    }

    pub fn record_repair(&self, outcome: &str) {
        self.repairs.increment(one_label("outcome", outcome));
    }

    pub fn record_repair_verdict(&self, verdict: RepairVerdict) {
        self.record_repair(verdict.label());
    }

    pub fn record_confirm(&self, outcome: &str) {
        self.confirms.increment(one_label("outcome", outcome));
    }

    pub fn record_unknown(&self, side: SideKind, reason: &str) {
        self.unknown
            .increment(two_labels(("side", side.label()), ("reason", reason)));
    }

    pub fn record_gateway_rpc(&self, method: &str, outcome: &str) {
        self.gateway_rpc
            .increment(two_labels(("method", method), ("outcome", outcome)));
    }

    pub fn record_livekit_call(&self, method: &str, outcome: &str) {
        self.livekit_calls
            .increment(two_labels(("method", method), ("outcome", outcome)));
    }

    pub fn record_breaker_trip(&self) {
        self.tripped_total.increment();
    }

    pub fn record_peer_detected(&self) {
        self.peer_detected_total.increment();
    }

    pub fn record_scheduler_starvation(&self) {
        self.scheduler_starved_total.increment();
    }

    pub fn observe_turn_duration(&self, ms: u64) {
        self.turn_duration.observe(ms);
    }

    pub fn observe_mutation_duration(&self, ms: u64) {
        self.mutation_duration.observe(ms);
    }

    pub fn set_mode(&self, effective: ReconMode) {
        for mode in [
            ReconMode::Halted,
            ReconMode::Observing,
            ReconMode::Constructive,
            ReconMode::Enforcing,
        ] {
            let value = f64::from(u8::from(mode == effective));
            self.mode.set(one_label("mode", mode.as_str()), value);
        }
    }

    pub fn set_mode_clamp(&self, clamp: ModeClamp, engaged: bool) {
        self.mode_clamp.set(
            one_label("clamp", clamp.label()),
            f64::from(u8::from(engaged)),
        );
    }

    pub fn set_server_health(&self, location: &Location, health: ServerHealth) {
        self.server_health.set(
            two_labels(
                ("region", location.region.as_str()),
                ("server", location.server.as_str()),
            ),
            f64::from(health.severity()),
        );
    }

    pub fn set_webhook_last_event_age(&self, server: &str, seconds: f64) {
        self.webhook_last_event_age_seconds
            .set(one_label("server", server), seconds);
    }

    pub fn set_ledger(&self, rooms: usize, connections: usize, bytes: u64, evicted_total: u64) {
        self.ledger_rooms.set(rooms as f64);
        self.ledger_connections.set(connections as f64);
        self.ledger_bytes.set(bytes as f64);
        self.ledger_evicted_total.set(evicted_total);
    }

    pub fn set_rooms_unknown_ratio(&self, ratio: f64) {
        self.rooms_unknown_ratio.set(ratio);
    }

    pub fn set_oldest_unknown_room_seconds(&self, seconds: f64) {
        self.oldest_unknown_room_seconds.set(seconds);
    }

    pub fn set_rooms_no_candidates(&self, rooms: usize) {
        self.rooms_no_candidates.set(rooms as f64);
    }

    pub fn set_wedged_connections(&self, connections: usize) {
        self.wedged_connections.set(connections as f64);
    }

    pub fn set_coverage_period_seconds(&self, seconds: f64) {
        self.coverage_period_seconds.set(seconds);
    }

    pub fn set_room_staleness_seconds(&self, seconds: f64) {
        self.room_staleness_seconds.set(seconds);
    }

    pub fn set_topology_age_seconds(&self, seconds: f64) {
        self.topology_age_seconds.set(seconds);
    }

    pub fn set_census_age_seconds(&self, seconds: f64) {
        self.census_age_seconds.set(seconds);
    }

    pub fn set_last_decision_age_seconds(&self, seconds: f64) {
        self.last_decision_age_seconds.set(seconds);
    }

    pub fn eviction_aborted_count(&self, reason: AbortReason) -> u64 {
        self.evictions_aborted
            .get(&one_label("reason", reason.label()))
            .unwrap_or(0.0) as u64
    }

    pub fn unknown_count(&self, side: SideKind, reason: &str) -> u64 {
        self.unknown
            .get(&two_labels(("side", side.label()), ("reason", reason)))
            .unwrap_or(0.0) as u64
    }

    pub fn gateway_rpc_count(&self, method: &str, outcome: &str) -> u64 {
        self.gateway_rpc
            .get(&two_labels(("method", method), ("outcome", outcome)))
            .unwrap_or(0.0) as u64
    }

    pub fn livekit_call_count(&self, method: &str, outcome: &str) -> u64 {
        self.livekit_calls
            .get(&two_labels(("method", method), ("outcome", outcome)))
            .unwrap_or(0.0) as u64
    }

    pub fn decision_count(&self, action: &DecisionAction) -> u64 {
        self.decisions
            .get(&one_label("decision", action.label()))
            .unwrap_or(0.0) as u64
    }

    pub fn peer_detected_total(&self) -> u64 {
        self.peer_detected_total.get()
    }

    pub fn tripped_total(&self) -> u64 {
        self.tripped_total.get()
    }

    pub fn scheduler_starved_total(&self) -> u64 {
        self.scheduler_starved_total.get()
    }

    pub fn ledger_evicted_total(&self) -> u64 {
        self.ledger_evicted_total.get()
    }

    pub fn render(&self, out: &mut String) {
        self.decisions.render(out, "decisions_total", "counter");
        self.mutations.render(out, "mutations_total", "counter");
        self.evictions_aborted
            .render(out, "evictions_aborted_total", "counter");
        self.unknown_media_causes
            .render(out, "unknown_media_refusals_total", "counter");
        self.repairs.render(out, "repairs_total", "counter");
        self.confirms.render(out, "confirms_total", "counter");
        self.unknown.render(out, "unknown_total", "counter");
        self.gateway_rpc.render(out, "gateway_rpc_total", "counter");
        self.livekit_calls
            .render(out, "livekit_calls_total", "counter");

        render_counter(out, "ledger_evicted_total", &self.ledger_evicted_total);
        render_counter(
            out,
            "scheduler_starved_total",
            &self.scheduler_starved_total,
        );
        render_counter(out, "tripped_total", &self.tripped_total);
        render_counter(out, "peer_detected_total", &self.peer_detected_total);

        self.mode.render(out, "mode", "gauge");
        self.mode_clamp.render(out, "mode_clamp", "gauge");
        self.server_health.render(out, "server_health", "gauge");
        self.webhook_last_event_age_seconds
            .render(out, "webhook_last_event_age_seconds", "gauge");

        render_gauge(out, "rooms_unknown_ratio", &self.rooms_unknown_ratio);
        render_gauge(
            out,
            "oldest_unknown_room_seconds",
            &self.oldest_unknown_room_seconds,
        );
        render_gauge(out, "rooms_no_candidates", &self.rooms_no_candidates);
        render_gauge(out, "wedged_connections", &self.wedged_connections);
        render_gauge(out, "ledger_rooms", &self.ledger_rooms);
        render_gauge(out, "ledger_connections", &self.ledger_connections);
        render_gauge(out, "ledger_bytes", &self.ledger_bytes);
        render_gauge(
            out,
            "coverage_period_seconds",
            &self.coverage_period_seconds,
        );
        render_gauge(out, "room_staleness_seconds", &self.room_staleness_seconds);
        render_gauge(out, "topology_age_seconds", &self.topology_age_seconds);
        render_gauge(out, "census_age_seconds", &self.census_age_seconds);
        render_gauge(
            out,
            "last_decision_age_seconds",
            &self.last_decision_age_seconds,
        );

        self.turn_duration.render(out, "turn_duration_ms");
        self.mutation_duration.render(out, "mutation_duration_ms");
    }
}

fn render_counter(out: &mut String, name: &str, counter: &Counter) {
    let _ = writeln!(out, "# TYPE {PREFIX}_{name} counter");
    let _ = writeln!(out, "{PREFIX}_{name} {}", counter.get());
}

fn render_gauge(out: &mut String, name: &str, gauge: &Gauge) {
    let _ = writeln!(out, "# TYPE {PREFIX}_{name} gauge");
    let _ = writeln!(out, "{PREFIX}_{name} {}", gauge.get());
}
