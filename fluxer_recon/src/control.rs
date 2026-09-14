// SPDX-License-Identifier: AGPL-3.0-or-later

use serde::{Deserialize, Serialize};

use fluxer_svc::transport::{Transport, TransportMessage, TransportSubscriber, reply_message};

use crate::budget::BudgetClass;
use crate::health::{ModeClamp, ReconMode};
use crate::ids::{ChannelId, GuildId, Millis, RoomKey};
use crate::runtime::{Shared, monotonic_now};
use crate::singleton::{RESUBSCRIBE_BACKOFF_MS, next_backoff_ms};
use crate::suspicion::SuspicionSource;

pub const CONTROL_SUBJECT: &str = "svc.recon";
pub const INSTANCE_SUBJECT: &str = "svc.recon.instance";
pub const EXPLAIN_DECISION_LIMIT: usize = 16;

#[derive(Clone, Debug, Deserialize, PartialEq, Eq)]
#[serde(untagged)]
pub enum RawId {
    Number(u64),
    Text(String),
}

impl RawId {
    pub fn value(&self) -> Option<u64> {
        match self {
            Self::Number(value) => Some(*value),
            Self::Text(text) => parse_u64_strict(text),
        }
    }
}

fn parse_u64_strict(value: &str) -> Option<u64> {
    if value.is_empty() || !value.bytes().all(|byte| byte.is_ascii_digit()) {
        return None;
    }
    value.parse::<u64>().ok()
}

#[derive(Clone, Debug, Deserialize, PartialEq, Eq)]
#[serde(tag = "method", rename_all = "snake_case")]
pub enum ControlRequest {
    Status,
    Explain {
        #[serde(default)]
        guild_id: Option<RawId>,
        channel_id: RawId,
    },
    Mode {
        #[serde(default)]
        mode: Option<String>,
    },
    Budget,
    Topology {
        #[serde(default)]
        refresh: Option<bool>,
    },
    Clamps {
        #[serde(default)]
        release: Option<String>,
    },
    Suspect {
        #[serde(default)]
        guild_id: Option<RawId>,
        channel_id: RawId,
    },
    Cliffs {
        #[serde(default)]
        release: Option<bool>,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ControlError {
    DecodeFailed,
    UnknownMode,
    UnknownClamp,
    InvalidChannelId,
    InvalidGuildId,
}

impl ControlError {
    pub const fn label(self) -> &'static str {
        match self {
            Self::DecodeFailed => "decode_failed",
            Self::UnknownMode => "unknown_mode",
            Self::UnknownClamp => "unknown_clamp",
            Self::InvalidChannelId => "invalid_channel_id",
            Self::InvalidGuildId => "invalid_guild_id",
        }
    }
}

#[derive(Clone, Debug, Serialize)]
pub struct ErrorResponse {
    pub ok: bool,
    pub error: &'static str,
}

#[derive(Clone, Debug, Serialize)]
pub struct ModeResponse {
    pub ok: bool,
    pub configured: &'static str,
    pub runtime_override: Option<&'static str>,
    pub effective: &'static str,
    pub clamps: Vec<&'static str>,
    pub changed: bool,
}

#[derive(Clone, Debug, Serialize)]
pub struct ServerHealthEntry {
    pub region: String,
    pub server: String,
    pub health: &'static str,
    pub consecutive_failures: u32,
}

#[derive(Clone, Debug, Serialize)]
pub struct PeerEntry {
    pub instance_id: String,
    pub started_at: u64,
    pub first_seen_ms_ago: u64,
    pub last_seen_ms_ago: u64,
    pub detections: u64,
}

#[derive(Clone, Debug, Serialize)]
pub struct StatusResponse {
    pub ok: bool,
    pub instance_id: String,
    pub started_at: u64,
    pub uptime_ms: u64,
    pub ready: bool,
    pub draining: bool,
    pub configured_mode: &'static str,
    pub runtime_override: Option<&'static str>,
    pub effective_mode: &'static str,
    pub clamps: Vec<&'static str>,
    pub warmup_elapsed: bool,
    pub coverage_pass_complete: bool,
    pub tracked_rooms: usize,
    pub tracked_connections: usize,
    pub tracked_bytes: u64,
    pub divergent_connections: usize,
    pub divergent_fraction: f64,
    pub coverage_period_ms: u64,
    pub breaker_tripped: bool,
    pub breaker_trips_total: u64,
    pub breaker_auto_resets_used: u32,
    pub breaker_held_for_manual_reset: bool,
    pub mutations_paused_ms_remaining: u64,
    pub topology_age_ms: Option<u64>,
    pub topology_servers: usize,
    pub census_age_ms: Option<u64>,
    pub last_turn: u64,
    pub servers: Vec<ServerHealthEntry>,
    pub peer: Option<PeerEntry>,
}

#[derive(Clone, Debug, Serialize)]
pub struct BucketEntry {
    pub class: &'static str,
    pub tokens: f64,
}

#[derive(Clone, Debug, Serialize)]
pub struct DenialEntry {
    pub reason: &'static str,
    pub count: u64,
}

#[derive(Clone, Debug, Serialize)]
pub struct BudgetResponse {
    pub ok: bool,
    pub effective_mode: &'static str,
    pub buckets: Vec<BucketEntry>,
    pub mutations_issued_total: u64,
    pub mutations_last_minute: u32,
    pub max_mutations_per_min: u32,
    pub max_action_attempts: u32,
    pub pending_actions: usize,
    pub scheduler_starved_total: u64,
    pub ledger_evicted_total: u64,
    pub denials: Vec<DenialEntry>,
}

#[derive(Clone, Debug, Serialize)]
pub struct ConnectionDump {
    pub connection_id: String,
    pub user_id: String,
    pub state: &'static str,
    pub corroborations: u32,
    pub divergence_since_ms: Option<u64>,
    pub fingerprint: Option<String>,
    pub last_seen_ms_ago: u64,
    pub action_attempts: u8,
    pub repair_attempts: u8,
    pub last_repair_verdict: Option<&'static str>,
}

#[derive(Clone, Debug, Serialize)]
pub struct ExplainResponse {
    pub ok: bool,
    pub scope: &'static str,
    pub guild_id: Option<String>,
    pub channel_id: String,
    pub tracked: bool,
    pub last_turn: Option<u64>,
    pub last_seen_ms_ago: Option<u64>,
    pub partially_unreadable: bool,
    pub no_candidates: bool,
    pub gateway_cliff_since_ms_ago: Option<u64>,
    pub media_cliff_since_ms_ago: Option<u64>,
    pub last_known_locations: Vec<String>,
    pub connections: Vec<ConnectionDump>,
    pub decisions: Vec<crate::observe::DecisionRecord>,
}

#[derive(Clone, Debug, Serialize)]
pub struct ClampEntry {
    pub clamp: &'static str,
    pub engaged: bool,
    pub self_clearing: bool,
    pub expires_in_ms: Option<u64>,
}

#[derive(Clone, Debug, Serialize)]
pub struct ClampsResponse {
    pub ok: bool,
    pub configured_mode: &'static str,
    pub effective_mode: &'static str,
    pub ceiling: &'static str,
    pub clamps: Vec<ClampEntry>,
    pub released: Vec<&'static str>,
    pub breaker_tripped: bool,
    pub breaker_auto_resets_used: u32,
    pub breaker_held_for_manual_reset: bool,
    pub mutations_paused_windows: u32,
    pub mutations_paused_hold_ms: u64,
}

#[derive(Clone, Debug, Serialize)]
pub struct CliffsResponse {
    pub ok: bool,
    pub cliffed: Vec<String>,
    pub disputed: Vec<String>,
    pub holed: Vec<String>,
    pub released: Vec<String>,
    pub hold_ms: u64,
    pub tripped_total: u64,
    pub disputed_total: u64,
    pub holes_total: u64,
}

#[derive(Clone, Debug, Serialize)]
pub struct SuspectResponse {
    pub ok: bool,
    pub scope: &'static str,
    pub guild_id: Option<String>,
    pub channel_id: String,
    pub admitted: &'static str,
    pub raised: &'static str,
    pub hold_ms: u64,
    pub suspect_rooms: usize,
}

#[derive(Clone, Debug, Serialize)]
pub struct TopologyResponse {
    pub ok: bool,
    pub servers: usize,
    pub age_ms: Option<u64>,
    pub max_age_ms: u64,
    pub stale: bool,
    pub refresh_requested: bool,
}

fn clamp_labels(shared: &Shared) -> Vec<&'static str> {
    shared.with_state(|state| {
        state
            .governor
            .clamps()
            .engaged()
            .map(ModeClamp::label)
            .collect()
    })
}

fn error(code: ControlError) -> serde_json::Value {
    serde_json::json!(ErrorResponse {
        ok: false,
        error: code.label(),
    })
}

fn room_key(guild_id: Option<&RawId>, channel_id: &RawId) -> Result<RoomKey, ControlError> {
    let channel = channel_id.value().ok_or(ControlError::InvalidChannelId)?;
    match guild_id {
        None => Ok(RoomKey::Dm {
            channel_id: ChannelId::new(channel),
        }),
        Some(raw) => {
            let guild = raw.value().ok_or(ControlError::InvalidGuildId)?;
            Ok(RoomKey::Guild {
                guild_id: GuildId::new(guild),
                channel_id: ChannelId::new(channel),
            })
        }
    }
}

pub fn status(shared: &Shared, now: Millis) -> StatusResponse {
    shared.with_state(|state| StatusResponse {
        ok: true,
        instance_id: shared.instance().id.clone(),
        started_at: shared.instance().started_at,
        uptime_ms: now.saturating_since(shared.boot_at()),
        ready: shared.readiness().is_ready(),
        draining: shared.readiness().is_draining(),
        configured_mode: state.governor.configured_mode().as_str(),
        runtime_override: state.governor.runtime_override().map(ReconMode::as_str),
        effective_mode: state.governor.effective_mode().as_str(),
        clamps: state
            .governor
            .clamps()
            .engaged()
            .map(ModeClamp::label)
            .collect(),
        warmup_elapsed: state.governor.warmup().elapsed_complete(now),
        coverage_pass_complete: state.governor.warmup().coverage_pass_complete(),
        tracked_rooms: state.ledger.tracked_rooms(),
        tracked_connections: state.ledger.tracked_connections(),
        tracked_bytes: state.ledger.tracked_bytes(),
        divergent_connections: state.ledger.divergent_connections(),
        divergent_fraction: state.ledger.divergent_fraction(),
        coverage_period_ms: state.coverage_period_ms,
        breaker_tripped: state.governor.breaker().is_tripped(),
        breaker_trips_total: state.governor.breaker().trips_total(),
        breaker_auto_resets_used: state.governor.breaker().auto_resets_used(),
        breaker_held_for_manual_reset: state
            .governor
            .breaker()
            .held_for_manual_reset(state.governor.limits().breaker),
        mutations_paused_ms_remaining: state.mutations_pause.remaining_ms(now),
        topology_age_ms: state.topology_age_ms(now),
        topology_servers: state.topology_servers,
        census_age_ms: state.census_age_ms(now),
        last_turn: state.turn.get(),
        servers: state
            .server_health
            .tracked()
            .map(|(location, tracker)| ServerHealthEntry {
                region: location.region.to_string(),
                server: location.server.to_string(),
                health: tracker.state().label(),
                consecutive_failures: tracker.consecutive_failures(),
            })
            .collect(),
        peer: state.peer.as_ref().map(|peer| PeerEntry {
            instance_id: peer.instance_id.clone(),
            started_at: peer.started_at,
            first_seen_ms_ago: now.saturating_since(peer.first_seen_at),
            last_seen_ms_ago: now.saturating_since(peer.last_seen_at),
            detections: peer.detections,
        }),
    })
}

pub fn budget(shared: &Shared, now: Millis) -> BudgetResponse {
    let metrics = shared.metrics().clone();
    shared.with_state_mut(|state| {
        let buckets = [
            BudgetClass::GatewayRead,
            BudgetClass::GatewayMutate,
            BudgetClass::LiveKitRead,
            BudgetClass::LiveKitMutate,
        ]
        .into_iter()
        .map(|class| BucketEntry {
            class: class.label(),
            tokens: state.governor.tokens(class, now),
        })
        .collect();

        BudgetResponse {
            ok: true,
            effective_mode: state.governor.effective_mode().as_str(),
            buckets,
            mutations_issued_total: state.governor.mutations().issued_total(),
            mutations_last_minute: state.governor.mutations().global_count(now),
            max_mutations_per_min: state.governor.limits().max_mutations_per_min,
            max_action_attempts: state.governor.limits().max_action_attempts,
            pending_actions: state.governor.pending_actions(),
            scheduler_starved_total: metrics.scheduler_starved_total(),
            ledger_evicted_total: metrics.ledger_evicted_total(),
            denials: state
                .governor
                .denials()
                .map(|(reason, count)| DenialEntry {
                    reason: reason.label(),
                    count: *count,
                })
                .collect(),
        }
    })
}

pub fn explain(shared: &Shared, room: RoomKey, now: Millis) -> ExplainResponse {
    let decisions = shared.journal().for_room(room, EXPLAIN_DECISION_LIMIT);
    shared.with_state(|state| {
        let entry = state.ledger.room(&room);
        ExplainResponse {
            ok: true,
            scope: crate::metrics::scope_label(room.scope()),
            guild_id: room.guild_id().map(|guild| guild.to_string()),
            channel_id: room.channel_id().to_string(),
            tracked: entry.is_some(),
            last_turn: entry.map(|room| room.last_turn().get()),
            last_seen_ms_ago: entry.map(|room| now.saturating_since(room.last_seen())),
            partially_unreadable: entry.is_some_and(|room| room.partially_unreadable()),
            no_candidates: entry.is_some_and(|room| room.no_candidates()),
            gateway_cliff_since_ms_ago: entry
                .and_then(|room| room.gateway_cliff().since())
                .map(|since| now.saturating_since(since)),
            media_cliff_since_ms_ago: entry
                .and_then(|room| room.media_cliff().since())
                .map(|since| now.saturating_since(since)),
            last_known_locations: entry
                .map(|room| {
                    room.last_known_locations()
                        .iter()
                        .filter_map(|index| state.ledger.location(*index))
                        .map(ToString::to_string)
                        .collect()
                })
                .unwrap_or_default(),
            connections: entry
                .map(|room| {
                    room.connections()
                        .iter()
                        .map(|connection| ConnectionDump {
                            connection_id: connection.connection.to_string(),
                            user_id: connection.user_id.to_string(),
                            state: connection.state.label(),
                            corroborations: connection.state.corroborations(),
                            divergence_since_ms: connection
                                .state
                                .divergence_since()
                                .map(|since| now.saturating_since(since)),
                            fingerprint: connection
                                .state
                                .stored_fingerprint()
                                .map(|fingerprint| format!("{:016x}", fingerprint.get())),
                            last_seen_ms_ago: now.saturating_since(connection.last_seen),
                            action_attempts: connection.action_attempts,
                            repair_attempts: connection.repair_attempts,
                            last_repair_verdict: connection
                                .last_repair_verdict
                                .map(|verdict| verdict.label()),
                        })
                        .collect()
                })
                .unwrap_or_default(),
            decisions,
        }
    })
}

pub fn set_mode(shared: &Shared, requested: Option<&str>) -> Result<ModeResponse, ControlError> {
    let parsed = match requested {
        None => None,
        Some(raw) => Some(ReconMode::parse(raw).ok_or(ControlError::UnknownMode)?),
    };

    let transition = shared.set_mode_override(parsed);

    tracing::warn!(
        configured = transition.configured.as_str(),
        previous_override = transition.previous_override.map(ReconMode::as_str),
        requested = transition.requested.map(ReconMode::as_str),
        effective_before = transition.effective_before.as_str(),
        effective_after = transition.effective_after.as_str(),
        "recon mode override applied over the control plane"
    );

    Ok(ModeResponse {
        ok: true,
        configured: transition.configured.as_str(),
        runtime_override: transition.requested.map(ReconMode::as_str),
        effective: transition.effective_after.as_str(),
        clamps: clamp_labels(shared),
        changed: transition.changed(),
    })
}

pub fn read_mode(shared: &Shared) -> ModeResponse {
    shared.with_state(|state| ModeResponse {
        ok: true,
        configured: state.governor.configured_mode().as_str(),
        runtime_override: state.governor.runtime_override().map(ReconMode::as_str),
        effective: state.governor.effective_mode().as_str(),
        clamps: state
            .governor
            .clamps()
            .engaged()
            .map(ModeClamp::label)
            .collect(),
        changed: false,
    })
}

pub fn topology(shared: &Shared, refresh: bool, now: Millis) -> TopologyResponse {
    let max_age_ms = shared.config().topology_max_age_ms;
    shared.with_state_mut(|state| {
        if refresh {
            state.topology_refresh_requested = true;
        }
        let age_ms = state.topology_age_ms(now);
        TopologyResponse {
            ok: true,
            servers: state.topology_servers,
            age_ms,
            max_age_ms,
            stale: age_ms.is_none_or(|age| age > max_age_ms),
            refresh_requested: state.topology_refresh_requested,
        }
    })
}

fn release_clamp(shared: &Shared, clamp: ModeClamp) -> bool {
    shared.with_state_mut(|state| {
        let was_engaged = state.governor.clamps().is_engaged(clamp);
        match clamp {
            ModeClamp::BreakerTripped => {
                state.governor.reset_breaker();
            }
            ModeClamp::MutationsPaused => state.mutations_pause.clear(),
            ModeClamp::Warmup => state.governor.warmup_mut().force_complete(),
            ModeClamp::ColdStart => state.governor.warmup_mut().note_coverage_pass(),
            ModeClamp::SingletonConflict => state.peer = None,
            ModeClamp::TopologyStale | ModeClamp::NatsReconnect | ModeClamp::CoverageOverrun => {}
        }
        state.governor.clamps_mut().release(clamp);
        was_engaged
    })
}

pub fn clamps(
    shared: &Shared,
    release: Option<&str>,
    now: Millis,
) -> Result<ClampsResponse, ControlError> {
    let wanted: Vec<ModeClamp> = match release {
        None => Vec::new(),
        Some(name) if name.trim().eq_ignore_ascii_case("all") => ModeClamp::ALL.to_vec(),
        Some(name) => vec![ModeClamp::parse(name).ok_or(ControlError::UnknownClamp)?],
    };

    let mut released: Vec<&'static str> = Vec::new();
    for clamp in wanted {
        if release_clamp(shared, clamp) {
            released.push(clamp.label());
        }
    }

    if !released.is_empty() {
        tracing::warn!(
            released = ?released,
            "an operator released mode clamps over the control plane, so the effective mode may \
             rise before the condition that engaged them is gone"
        );
    }

    Ok(shared.with_state_mut(|state| {
        let limits = state.governor.limits().breaker;
        ClampsResponse {
            ok: true,
            configured_mode: state.governor.configured_mode().as_str(),
            effective_mode: state.governor.effective_mode().as_str(),
            ceiling: state.governor.clamps().ceiling().as_str(),
            clamps: ModeClamp::ALL
                .into_iter()
                .map(|clamp| ClampEntry {
                    clamp: clamp.label(),
                    engaged: state.governor.clamps().is_engaged(clamp),
                    self_clearing: clamp.is_self_clearing(),
                    expires_in_ms: state
                        .governor
                        .clamps()
                        .deadline(clamp)
                        .map(|until| until.saturating_since(now)),
                })
                .collect(),
            released,
            breaker_tripped: state.governor.breaker().is_tripped(),
            breaker_auto_resets_used: state.governor.breaker().auto_resets_used(),
            breaker_held_for_manual_reset: state.governor.breaker().held_for_manual_reset(limits),
            mutations_paused_windows: state.mutations_pause.windows(),
            mutations_paused_hold_ms: state.mutations_pause.hold_ms(),
        }
    }))
}

pub fn cliffs(shared: &Shared, release: bool, now: Millis) -> CliffsResponse {
    let hold_ms = shared.config().server_cliff_hold_ms;
    let response = shared.with_state_mut(|state| {
        let cliffed = state.cliffs.cliffed(now);
        let disputed = state.cliffs.disputed();
        let holed = state.suspicion.holed_servers();
        let mut released: Vec<String> = Vec::new();

        if release {
            for location in cliffed.iter().chain(holed.iter()) {
                let forgotten = state.cliffs.forget(location);
                let filled = state.suspicion.clear_hole(location);
                if forgotten || filled {
                    released.push(location.to_string());
                }
            }
            released.sort();
            released.dedup();
        }

        CliffsResponse {
            ok: true,
            cliffed: cliffed.iter().map(ToString::to_string).collect(),
            disputed: disputed.iter().map(ToString::to_string).collect(),
            holed: holed.iter().map(ToString::to_string).collect(),
            released,
            hold_ms,
            tripped_total: state.cliffs.tripped_total(),
            disputed_total: state.cliffs.disputed_total(),
            holes_total: state.suspicion.holes_total(),
        }
    });

    if !response.released.is_empty() {
        tracing::warn!(
            released = ?response.released,
            "an operator dropped server holds over the control plane, so those servers read as \
             authoritative again before their hold window ended"
        );
    }

    response
}

pub fn suspect(shared: &Shared, room: RoomKey, now: Millis) -> SuspectResponse {
    let (admitted, raised, suspect_rooms, hold_ms) = shared.with_state_mut(|state| {
        let admitted = state.directory.note_suspicion(room, now);
        let raised = state
            .suspicion
            .note_room(room, SuspicionSource::Control, now);
        (
            admitted.label(),
            raised.label(),
            state.suspicion.suspect_rooms(now).len(),
            state.suspicion.limits().hold_ms,
        )
    });

    tracing::info!(
        channel_id = room.channel_id().get(),
        guild_id = room.guild_id().map(|guild| guild.get()),
        admitted,
        raised,
        "an operator raised suspicion over the control plane, which only reorders reads"
    );

    SuspectResponse {
        ok: true,
        scope: crate::metrics::scope_label(room.scope()),
        guild_id: room.guild_id().map(|guild| guild.to_string()),
        channel_id: room.channel_id().to_string(),
        admitted,
        raised,
        hold_ms,
        suspect_rooms,
    }
}

pub fn dispatch(shared: &Shared, request: &ControlRequest, now: Millis) -> serde_json::Value {
    match request {
        ControlRequest::Status => serde_json::json!(status(shared, now)),
        ControlRequest::Budget => serde_json::json!(budget(shared, now)),
        ControlRequest::Mode { mode } => match mode {
            None => serde_json::json!(read_mode(shared)),
            Some(raw) => match set_mode(shared, Some(raw)) {
                Ok(response) => serde_json::json!(response),
                Err(code) => error(code),
            },
        },
        ControlRequest::Explain {
            guild_id,
            channel_id,
        } => match room_key(guild_id.as_ref(), channel_id) {
            Ok(room) => serde_json::json!(explain(shared, room, now)),
            Err(code) => error(code),
        },
        ControlRequest::Topology { refresh } => {
            serde_json::json!(topology(shared, refresh.unwrap_or(false), now))
        }
        ControlRequest::Clamps { release } => match clamps(shared, release.as_deref(), now) {
            Ok(response) => serde_json::json!(response),
            Err(code) => error(code),
        },
        ControlRequest::Suspect {
            guild_id,
            channel_id,
        } => match room_key(guild_id.as_ref(), channel_id) {
            Ok(room) => serde_json::json!(suspect(shared, room, now)),
            Err(code) => error(code),
        },
        ControlRequest::Cliffs { release } => {
            serde_json::json!(cliffs(shared, release.unwrap_or(false), now))
        }
    }
}

pub fn handle(shared: &Shared, payload: &[u8], now: Millis) -> Vec<u8> {
    let response = match serde_json::from_slice::<ControlRequest>(payload) {
        Ok(request) => dispatch(shared, &request, now),
        Err(parse_error) => {
            tracing::debug!(error = %parse_error, "rejecting an undecodable control request");
            error(ControlError::DecodeFailed)
        }
    };
    serde_json::to_vec(&response)
        .unwrap_or_else(|_| b"{\"ok\":false,\"error\":\"encode_failed\"}".to_vec())
}

pub async fn run_control<T: Transport>(shared: Shared, transport: T) -> anyhow::Result<()> {
    let mut backoff_ms = RESUBSCRIBE_BACKOFF_MS;

    loop {
        let mut subscription = match transport.subscribe(CONTROL_SUBJECT).await {
            Ok(subscription) => {
                backoff_ms = RESUBSCRIBE_BACKOFF_MS;
                subscription
            }
            Err(error) => {
                tracing::error!(
                    error = %error,
                    subject = CONTROL_SUBJECT,
                    retry_in_ms = backoff_ms,
                    "the control plane could not subscribe, retrying rather than ending the task, \
                     because the escape hatch for a clamped service must outlive a broken \
                     subscription"
                );
                tokio::time::sleep(std::time::Duration::from_millis(backoff_ms)).await;
                backoff_ms = next_backoff_ms(backoff_ms);
                continue;
            }
        };

        tracing::info!(
            subject = CONTROL_SUBJECT,
            instance_id = shared.instance().id,
            "recon control plane listening"
        );

        loop {
            tokio::select! {
                message = subscription.next() => {
                    let Some(message) = message else {
                        tracing::warn!("control subscription ended, will re-subscribe");
                        break;
                    };
                    if !message.has_reply() {
                        continue;
                    }
                    let response = handle(&shared, message.payload(), monotonic_now());
                    if let Err(error) = reply_message(&message, &transport, &response).await {
                        tracing::debug!(error = %error, "failed to answer a control request");
                    }
                }
                _ = transport.wait_for_reconnect() => {
                    tracing::info!("NATS reconnected, re-subscribing the control plane");
                    break;
                }
            }
        }
    }
}
