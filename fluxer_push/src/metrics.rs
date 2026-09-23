// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::relay::reject::{REASON_COUNT, Reason};
use crate::rollout::{RolloutOutcome, RolloutSnapshot};
use fluxer_svc::metrics::now_ms;
use std::fmt::{self, Write as _};
use std::sync::atomic::{AtomicU64, Ordering};

const ORDERING: Ordering = Ordering::Relaxed;

pub fn elapsed_ms(started_ms: i64) -> u64 {
    u64::try_from(now_ms() - started_ms).unwrap_or(0)
}

const HISTOGRAM_BUCKETS_MS: &[u64] = &[
    1, 5, 10, 25, 50, 100, 250, 500, 1000, 2500, 5000, 10000, 30000,
];

struct Histogram {
    buckets: [AtomicU64; 13],
    inf: AtomicU64,
    sum_ms: AtomicU64,
    count: AtomicU64,
}

impl Histogram {
    const fn new() -> Self {
        Self {
            buckets: [const { AtomicU64::new(0) }; 13],
            inf: AtomicU64::new(0),
            sum_ms: AtomicU64::new(0),
            count: AtomicU64::new(0),
        }
    }

    fn observe(&self, ms: u64) {
        for (index, upper) in HISTOGRAM_BUCKETS_MS.iter().copied().enumerate() {
            if ms <= upper {
                self.buckets[index].fetch_add(1, ORDERING);
                break;
            }
        }
        self.inf.fetch_add(1, ORDERING);
        self.sum_ms.fetch_add(ms, ORDERING);
        self.count.fetch_add(1, ORDERING);
    }

    fn render_series(&self, out: &mut String, name: &str, labels: &str) -> fmt::Result {
        let mut cumulative = 0;
        for (index, upper) in HISTOGRAM_BUCKETS_MS.iter().copied().enumerate() {
            cumulative += self.buckets[index].load(ORDERING);
            writeln!(out, "{name}_bucket{{{labels},le=\"{upper}\"}} {cumulative}")?;
        }
        writeln!(
            out,
            "{name}_bucket{{{labels},le=\"+Inf\"}} {}",
            self.inf.load(ORDERING)
        )?;
        writeln!(out, "{name}_sum{{{labels}}} {}", self.sum_ms.load(ORDERING))?;
        writeln!(
            out,
            "{name}_count{{{labels}}} {}",
            self.count.load(ORDERING)
        )
    }
}

impl Default for Histogram {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(usize)]
pub enum JobKind {
    Message,
    Clear,
    Ring,
}

impl JobKind {
    pub const ALL: [Self; 3] = [Self::Message, Self::Clear, Self::Ring];

    pub fn label(self) -> &'static str {
        match self {
            Self::Message => "message",
            Self::Clear => "clear",
            Self::Ring => "ring",
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(usize)]
pub enum JobRejection {
    Decode,
    QueueFull,
}

impl JobRejection {
    pub const ALL: [Self; 2] = [Self::Decode, Self::QueueFull];

    pub fn label(self) -> &'static str {
        match self {
            Self::Decode => "decode",
            Self::QueueFull => "queue_full",
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(usize)]
pub enum Provider {
    WebPush,
    UnifiedPush,
    Fcm,
    Apns,
    ApnsVoip,
}

impl Provider {
    pub const ALL: [Self; 5] = [
        Self::WebPush,
        Self::UnifiedPush,
        Self::Fcm,
        Self::Apns,
        Self::ApnsVoip,
    ];

    pub fn label(self) -> &'static str {
        match self {
            Self::WebPush => "web_push",
            Self::UnifiedPush => "unified_push",
            Self::Fcm => "fcm",
            Self::Apns => "apns",
            Self::ApnsVoip => "apns_voip",
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(usize)]
pub enum SendResult {
    Accepted,
    TokenInvalid,
    Permanent,
    Transient,
}

impl SendResult {
    pub const ALL: [Self; 4] = [
        Self::Accepted,
        Self::TokenInvalid,
        Self::Permanent,
        Self::Transient,
    ];

    pub fn label(self) -> &'static str {
        match self {
            Self::Accepted => "accepted",
            Self::TokenInvalid => "token_invalid",
            Self::Permanent => "permanent",
            Self::Transient => "transient",
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(usize)]
pub enum RelayLeg {
    Apns,
    Fcm,
    ApnsVoip,
}

impl RelayLeg {
    pub const ALL: [Self; 3] = [Self::Apns, Self::Fcm, Self::ApnsVoip];

    pub fn label(self) -> &'static str {
        match self {
            Self::Apns => "apns",
            Self::Fcm => "fcm",
            Self::ApnsVoip => "apns_voip",
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(usize)]
pub enum RelayResult {
    Accepted,
    Rejected,
    Failed,
}

impl RelayResult {
    pub const ALL: [Self; 3] = [Self::Accepted, Self::Rejected, Self::Failed];

    pub fn label(self) -> &'static str {
        match self {
            Self::Accepted => "accepted",
            Self::Rejected => "rejected",
            Self::Failed => "failed",
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(usize)]
pub enum AuthProvider {
    Vapid,
    Apns,
    Fcm,
}

impl AuthProvider {
    pub const ALL: [Self; 3] = [Self::Vapid, Self::Apns, Self::Fcm];

    pub fn label(self) -> &'static str {
        match self {
            Self::Vapid => "vapid",
            Self::Apns => "apns",
            Self::Fcm => "fcm",
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(usize)]
pub enum RpcMethod {
    GetPushSubscriptions,
    GetBadgeCounts,
    DeletePushSubscriptions,
    GetPushServiceDeliveryConfig,
}

impl RpcMethod {
    pub const ALL: [Self; 4] = [
        Self::GetPushSubscriptions,
        Self::GetBadgeCounts,
        Self::DeletePushSubscriptions,
        Self::GetPushServiceDeliveryConfig,
    ];

    pub fn label(self) -> &'static str {
        match self {
            Self::GetPushSubscriptions => "get_push_subscriptions",
            Self::GetBadgeCounts => "get_badge_counts",
            Self::DeletePushSubscriptions => "delete_push_subscriptions",
            Self::GetPushServiceDeliveryConfig => "get_push_service_delivery_config",
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(usize)]
pub enum DeliveryRoute {
    WebPush,
    LegacyApns,
    LegacyFcm,
}

impl DeliveryRoute {
    pub const ALL: [Self; 3] = [Self::WebPush, Self::LegacyApns, Self::LegacyFcm];

    pub fn label(self) -> &'static str {
        match self {
            Self::WebPush => "web_push",
            Self::LegacyApns => "legacy_apns",
            Self::LegacyFcm => "legacy_fcm",
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(usize)]
pub enum BucketKey {
    DeviceToken,
    Source,
}

impl BucketKey {
    pub const ALL: [Self; 2] = [Self::DeviceToken, Self::Source];

    pub fn label(self) -> &'static str {
        match self {
            Self::DeviceToken => "device_token",
            Self::Source => "source",
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(usize)]
pub enum PayloadShrink {
    Media,
    Icons,
    Body,
    Minimal,
}

impl PayloadShrink {
    pub const ALL: [Self; 4] = [Self::Media, Self::Icons, Self::Body, Self::Minimal];

    pub fn label(self) -> &'static str {
        match self {
            Self::Media => "media",
            Self::Icons => "icons",
            Self::Body => "body",
            Self::Minimal => "minimal",
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(usize)]
pub enum RpcOutcome {
    Ok,
    Error,
}

impl RpcOutcome {
    pub const ALL: [Self; 2] = [Self::Ok, Self::Error];

    pub fn label(self) -> &'static str {
        match self {
            Self::Ok => "ok",
            Self::Error => "error",
        }
    }
}

const JOB_KIND_COUNT: usize = JobKind::ALL.len();
const JOB_REJECTION_COUNT: usize = JobRejection::ALL.len();
const PROVIDER_COUNT: usize = Provider::ALL.len();
const SEND_RESULT_COUNT: usize = SendResult::ALL.len();
const AUTH_PROVIDER_COUNT: usize = AuthProvider::ALL.len();
const RELAY_LEG_COUNT: usize = RelayLeg::ALL.len();
const RELAY_RESULT_COUNT: usize = RelayResult::ALL.len();
const RPC_METHOD_COUNT: usize = RpcMethod::ALL.len();
const BUCKET_KEY_COUNT: usize = BucketKey::ALL.len();
const PAYLOAD_SHRINK_COUNT: usize = PayloadShrink::ALL.len();
const DELIVERY_ROUTE_COUNT: usize = DeliveryRoute::ALL.len();
const RPC_OUTCOME_COUNT: usize = RpcOutcome::ALL.len();
const ROLLOUT_OUTCOME_COUNT: usize = RolloutOutcome::ALL.len();

pub struct Metrics {
    jobs_received: [AtomicU64; JOB_KIND_COUNT],
    jobs_rejected: [AtomicU64; JOB_REJECTION_COUNT],
    jobs_completed: [AtomicU64; JOB_KIND_COUNT],
    recipients: AtomicU64,
    subscriptions: AtomicU64,
    sends: [[AtomicU64; SEND_RESULT_COUNT]; PROVIDER_COUNT],
    token_deletions: [AtomicU64; PROVIDER_COUNT],
    payload_shrinks: [AtomicU64; PAYLOAD_SHRINK_COUNT],
    own_relay_shortcuts: AtomicU64,
    auth_tokens_minted: [AtomicU64; AUTH_PROVIDER_COUNT],
    rpc_requests: [[AtomicU64; RPC_OUTCOME_COUNT]; RPC_METHOD_COUNT],
    rollout_updates: [AtomicU64; ROLLOUT_OUTCOME_COUNT],
    delivery_routes: [[AtomicU64; SEND_RESULT_COUNT]; DELIVERY_ROUTE_COUNT],
    relay_served: [[AtomicU64; RELAY_RESULT_COUNT]; RELAY_LEG_COUNT],
    relay_vendor_requests: [[AtomicU64; RELAY_RESULT_COUNT]; RELAY_LEG_COUNT],
    relay_rejected: [AtomicU64; REASON_COUNT],
    relay_bucket_drops: [AtomicU64; BUCKET_KEY_COUNT],
    rings_suppressed: AtomicU64,
    job_duration: [Histogram; JOB_KIND_COUNT],
    rpc_duration: [Histogram; RPC_METHOD_COUNT],
    send_duration: [Histogram; PROVIDER_COUNT],
    queue_depth: AtomicU64,
    rollout_enabled: AtomicU64,
    rollout_basis_points: AtomicU64,
    rollout_config_version: AtomicU64,
    start_ms: i64,
}

impl Metrics {
    pub fn new() -> Self {
        Self {
            jobs_received: [const { AtomicU64::new(0) }; JOB_KIND_COUNT],
            jobs_rejected: [const { AtomicU64::new(0) }; JOB_REJECTION_COUNT],
            jobs_completed: [const { AtomicU64::new(0) }; JOB_KIND_COUNT],
            recipients: AtomicU64::new(0),
            subscriptions: AtomicU64::new(0),
            sends: [const { [const { AtomicU64::new(0) }; SEND_RESULT_COUNT] }; PROVIDER_COUNT],
            token_deletions: [const { AtomicU64::new(0) }; PROVIDER_COUNT],
            payload_shrinks: [const { AtomicU64::new(0) }; PAYLOAD_SHRINK_COUNT],
            own_relay_shortcuts: AtomicU64::new(0),
            auth_tokens_minted: [const { AtomicU64::new(0) }; AUTH_PROVIDER_COUNT],
            rpc_requests: [const { [const { AtomicU64::new(0) }; RPC_OUTCOME_COUNT] };
                RPC_METHOD_COUNT],
            rollout_updates: [const { AtomicU64::new(0) }; ROLLOUT_OUTCOME_COUNT],
            delivery_routes: [const { [const { AtomicU64::new(0) }; SEND_RESULT_COUNT] };
                DELIVERY_ROUTE_COUNT],
            relay_served: [const { [const { AtomicU64::new(0) }; RELAY_RESULT_COUNT] };
                RELAY_LEG_COUNT],
            relay_vendor_requests: [const { [const { AtomicU64::new(0) }; RELAY_RESULT_COUNT] };
                RELAY_LEG_COUNT],
            relay_rejected: [const { AtomicU64::new(0) }; REASON_COUNT],
            relay_bucket_drops: [const { AtomicU64::new(0) }; BUCKET_KEY_COUNT],
            rings_suppressed: AtomicU64::new(0),
            job_duration: [const { Histogram::new() }; JOB_KIND_COUNT],
            rpc_duration: [const { Histogram::new() }; RPC_METHOD_COUNT],
            send_duration: [const { Histogram::new() }; PROVIDER_COUNT],
            queue_depth: AtomicU64::new(0),
            rollout_enabled: AtomicU64::new(0),
            rollout_basis_points: AtomicU64::new(0),
            rollout_config_version: AtomicU64::new(0),
            start_ms: now_ms(),
        }
    }

    pub fn record_job_received(&self, kind: JobKind) {
        self.jobs_received[kind as usize].fetch_add(1, ORDERING);
    }

    pub fn record_job_rejected(&self, reason: JobRejection) {
        self.jobs_rejected[reason as usize].fetch_add(1, ORDERING);
    }

    pub fn record_job_completed(&self, kind: JobKind, duration_ms: u64) {
        self.jobs_completed[kind as usize].fetch_add(1, ORDERING);
        self.job_duration[kind as usize].observe(duration_ms);
    }

    pub fn record_recipients(&self, count: u64) {
        self.recipients.fetch_add(count, ORDERING);
    }

    pub fn record_subscriptions(&self, count: u64) {
        self.subscriptions.fetch_add(count, ORDERING);
    }

    pub fn record_send(&self, provider: Provider, result: SendResult, duration_ms: u64) {
        self.sends[provider as usize][result as usize].fetch_add(1, ORDERING);
        self.send_duration[provider as usize].observe(duration_ms);
    }

    pub fn record_token_deletion(&self, provider: Provider) {
        self.token_deletions[provider as usize].fetch_add(1, ORDERING);
    }

    pub fn record_own_relay_shortcut(&self) {
        self.own_relay_shortcuts.fetch_add(1, ORDERING);
    }

    pub fn record_payload_shrink(&self, step: PayloadShrink) {
        self.payload_shrinks[step as usize].fetch_add(1, ORDERING);
    }

    pub fn record_auth_token_minted(&self, provider: AuthProvider) {
        self.auth_tokens_minted[provider as usize].fetch_add(1, ORDERING);
    }

    pub fn record_rpc(&self, method: RpcMethod, outcome: RpcOutcome, duration_ms: u64) {
        self.rpc_requests[method as usize][outcome as usize].fetch_add(1, ORDERING);
        self.rpc_duration[method as usize].observe(duration_ms);
    }

    pub fn record_rollout_update(&self, outcome: RolloutOutcome) {
        self.rollout_updates[outcome as usize].fetch_add(1, ORDERING);
    }

    pub fn record_rollout_snapshot(&self, snapshot: &RolloutSnapshot) {
        self.rollout_enabled
            .store(u64::from(snapshot.enabled), ORDERING);
        self.rollout_basis_points
            .store(u64::from(snapshot.rollout_basis_points), ORDERING);
        self.rollout_config_version
            .store(snapshot.config_version, ORDERING);
    }

    pub fn record_delivery_route(&self, route: DeliveryRoute, result: SendResult) {
        self.delivery_routes[route as usize][result as usize].fetch_add(1, ORDERING);
    }

    pub fn record_relay_served(&self, leg: RelayLeg, result: RelayResult) {
        self.relay_served[leg as usize][result as usize].fetch_add(1, ORDERING);
    }

    pub fn record_relay_vendor_request(&self, leg: RelayLeg, result: RelayResult) {
        self.relay_vendor_requests[leg as usize][result as usize].fetch_add(1, ORDERING);
    }

    pub fn record_relay_rejected(&self, reason: Reason) {
        self.relay_rejected[reason as usize].fetch_add(1, ORDERING);
    }

    pub fn record_bucket_drop(&self, key: BucketKey) {
        self.relay_bucket_drops[key as usize].fetch_add(1, ORDERING);
    }

    pub fn record_ring_suppressed(&self) {
        self.rings_suppressed.fetch_add(1, ORDERING);
    }

    pub fn set_queue_depth(&self, depth: u64) {
        self.queue_depth.store(depth, ORDERING);
    }

    pub fn render(&self) -> String {
        let mut out = String::new();
        self.render_into(&mut out)
            .expect("writing push metrics to a String cannot fail");
        out
    }

    fn render_into(&self, out: &mut String) -> fmt::Result {
        render_labelled_counter(
            out,
            "fluxer_push_jobs_received_total",
            "kind",
            JobKind::ALL.map(JobKind::label),
            &self.jobs_received,
        )?;
        render_labelled_counter(
            out,
            "fluxer_push_jobs_rejected_total",
            "reason",
            JobRejection::ALL.map(JobRejection::label),
            &self.jobs_rejected,
        )?;
        render_labelled_counter(
            out,
            "fluxer_push_jobs_completed_total",
            "kind",
            JobKind::ALL.map(JobKind::label),
            &self.jobs_completed,
        )?;
        render_counter(out, "fluxer_push_recipients_total", &self.recipients)?;
        render_counter(out, "fluxer_push_subscriptions_total", &self.subscriptions)?;

        render_labelled_grid(
            out,
            "fluxer_push_sends_total",
            ("provider", Provider::ALL.map(Provider::label)),
            ("result", SendResult::ALL.map(SendResult::label)),
            &self.sends,
        )?;

        render_labelled_counter(
            out,
            "fluxer_push_token_deletions_total",
            "provider",
            Provider::ALL.map(Provider::label),
            &self.token_deletions,
        )?;
        render_counter(
            out,
            "fluxer_push_own_relay_shortcuts_total",
            &self.own_relay_shortcuts,
        )?;
        render_labelled_counter(
            out,
            "fluxer_push_payload_shrinks_total",
            "step",
            PayloadShrink::ALL.map(PayloadShrink::label),
            &self.payload_shrinks,
        )?;
        render_labelled_counter(
            out,
            "fluxer_push_auth_tokens_minted_total",
            "provider",
            AuthProvider::ALL.map(AuthProvider::label),
            &self.auth_tokens_minted,
        )?;

        render_labelled_grid(
            out,
            "fluxer_push_rpc_requests_total",
            ("method", RpcMethod::ALL.map(RpcMethod::label)),
            ("result", RpcOutcome::ALL.map(RpcOutcome::label)),
            &self.rpc_requests,
        )?;

        render_labelled_counter(
            out,
            "fluxer_push_rollout_updates_total",
            "result",
            RolloutOutcome::ALL.map(RolloutOutcome::label),
            &self.rollout_updates,
        )?;
        render_labelled_histogram(
            out,
            "fluxer_push_job_duration_ms",
            "kind",
            JobKind::ALL.map(JobKind::label),
            &self.job_duration,
        )?;
        render_labelled_histogram(
            out,
            "fluxer_push_rpc_duration_ms",
            "method",
            RpcMethod::ALL.map(RpcMethod::label),
            &self.rpc_duration,
        )?;
        render_labelled_histogram(
            out,
            "fluxer_push_send_duration_ms",
            "provider",
            Provider::ALL.map(Provider::label),
            &self.send_duration,
        )?;
        render_labelled_grid(
            out,
            "fluxer_push_delivery_routes_total",
            ("route", DeliveryRoute::ALL.map(DeliveryRoute::label)),
            ("result", SendResult::ALL.map(SendResult::label)),
            &self.delivery_routes,
        )?;
        render_labelled_grid(
            out,
            "fluxer_push_relay_served_total",
            ("leg", RelayLeg::ALL.map(RelayLeg::label)),
            ("result", RelayResult::ALL.map(RelayResult::label)),
            &self.relay_served,
        )?;
        render_labelled_grid(
            out,
            "fluxer_push_relay_vendor_requests_total",
            ("leg", RelayLeg::ALL.map(RelayLeg::label)),
            ("result", RelayResult::ALL.map(RelayResult::label)),
            &self.relay_vendor_requests,
        )?;
        render_labelled_counter(
            out,
            "fluxer_push_relay_rejected_total",
            "reason",
            Reason::ALL.map(Reason::label),
            &self.relay_rejected,
        )?;
        render_labelled_counter(
            out,
            "fluxer_push_relay_token_bucket_drops_total",
            "key",
            BucketKey::ALL.map(BucketKey::label),
            &self.relay_bucket_drops,
        )?;
        render_counter(
            out,
            "fluxer_push_rings_suppressed_total",
            &self.rings_suppressed,
        )?;
        render_gauge(out, "fluxer_push_queue_depth", &self.queue_depth)?;
        render_gauge(out, "fluxer_push_rollout_enabled", &self.rollout_enabled)?;
        render_gauge(
            out,
            "fluxer_push_rollout_basis_points",
            &self.rollout_basis_points,
        )?;
        render_gauge(
            out,
            "fluxer_push_rollout_config_version",
            &self.rollout_config_version,
        )?;

        writeln!(out, "# TYPE fluxer_push_uptime_seconds gauge")?;
        writeln!(
            out,
            "fluxer_push_uptime_seconds {:.3}",
            (now_ms() - self.start_ms) as f64 / 1000.0
        )
    }
}

impl Default for Metrics {
    fn default() -> Self {
        Self::new()
    }
}

fn render_counter(out: &mut String, name: &str, counter: &AtomicU64) -> fmt::Result {
    writeln!(out, "# TYPE {name} counter")?;
    writeln!(out, "{name} {}", counter.load(ORDERING))
}

fn render_gauge(out: &mut String, name: &str, gauge: &AtomicU64) -> fmt::Result {
    writeln!(out, "# TYPE {name} gauge")?;
    writeln!(out, "{name} {}", gauge.load(ORDERING))
}

fn render_labelled_counter<const N: usize>(
    out: &mut String,
    name: &str,
    label: &str,
    values: [&'static str; N],
    counters: &[AtomicU64; N],
) -> fmt::Result {
    writeln!(out, "# TYPE {name} counter")?;
    for (index, value) in values.into_iter().enumerate() {
        writeln!(
            out,
            "{name}{{{label}=\"{value}\"}} {}",
            counters[index].load(ORDERING)
        )?;
    }
    Ok(())
}

fn render_labelled_grid<const R: usize, const C: usize>(
    out: &mut String,
    name: &str,
    (row_label, rows): (&str, [&'static str; R]),
    (column_label, columns): (&str, [&'static str; C]),
    counters: &[[AtomicU64; C]; R],
) -> fmt::Result {
    writeln!(out, "# TYPE {name} counter")?;
    for (row_index, row) in rows.into_iter().enumerate() {
        for (column_index, column) in columns.into_iter().enumerate() {
            writeln!(
                out,
                "{name}{{{row_label}=\"{row}\",{column_label}=\"{column}\"}} {}",
                counters[row_index][column_index].load(ORDERING)
            )?;
        }
    }
    Ok(())
}

fn render_labelled_histogram<const N: usize>(
    out: &mut String,
    name: &str,
    label: &str,
    values: [&'static str; N],
    histograms: &[Histogram; N],
) -> fmt::Result {
    writeln!(out, "# TYPE {name} histogram")?;
    for (index, value) in values.into_iter().enumerate() {
        histograms[index].render_series(out, name, &format!("{label}=\"{value}\""))?;
    }
    Ok(())
}
