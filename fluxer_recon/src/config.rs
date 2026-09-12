// SPDX-License-Identifier: AGPL-3.0-or-later

use std::env;
use std::fmt::Display;
use std::str::FromStr;

use crate::health::ReconMode;
use crate::topology::default_region_id;

const MIN_COVERAGE_TARGET_MS: u64 = 1_000;
const MIN_REQUIRED_CORROBORATIONS: u32 = 2;
const READ_RPS_RANGE: (u32, u32) = (1, 200);
const MUTATE_RPM_RANGE: (u32, u32) = (0, 600);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ConnectionIdGuard {
    AssumeAbsent,
    Honoured,
}

impl ConnectionIdGuard {
    pub fn parse(value: &str) -> Option<Self> {
        match value.trim().to_ascii_lowercase().as_str() {
            "assume_absent" => Some(Self::AssumeAbsent),
            "honored" | "honoured" => Some(Self::Honoured),
            _ => None,
        }
    }

    pub const fn as_str(self) -> &'static str {
        match self {
            Self::AssumeAbsent => "assume_absent",
            Self::Honoured => "honored",
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ReconConfig {
    pub mode: ReconMode,
    pub worker_threads: usize,
    pub tick_ms: u64,
    pub turns_per_tick: usize,
    pub coverage_target_ms: u64,
    pub coverage_period_hard_cap_ms: u64,
    pub expected_rooms: usize,
    pub hot_period_ms: u64,
    pub max_hot_rooms: usize,
    pub suspicion_hold_ms: u64,
    pub required_corroborations: u32,
    pub min_corroboration_gap_ms: u64,
    pub max_corroboration_window_ms: u64,
    pub min_divergence_ms: u64,
    pub preflight_max_age_ms: u64,
    pub warmup_seconds: u64,
    pub pending_join_skew_ms: u64,
    pub census_interval_ms: u64,
    pub census_max_age_ms: u64,
    pub discovery_interval_ms: u64,
    pub topology_refresh_ms: u64,
    pub topology_max_age_ms: u64,
    pub topology_drain_grace_ms: u64,
    pub room_cliff_hold_ms: u64,
    pub server_cliff_hold_ms: u64,
    pub server_cliff_fraction: f64,
    pub gateway_read_rps: u32,
    pub gateway_mutate_rpm: u32,
    pub livekit_read_rps: u32,
    pub livekit_read_rps_per_server: u32,
    pub livekit_mutate_rpm: u32,
    pub max_inflight_room_turns: usize,
    pub max_inflight_gateway: usize,
    pub max_inflight_livekit: usize,
    pub max_inflight_per_server: usize,
    pub max_mutations_per_room_per_min: u32,
    pub max_mutations_per_guild_per_min: u32,
    pub max_mutations_per_min: u32,
    pub max_action_attempts: u32,
    pub max_divergent_fraction: f64,
    pub breaker_auto_reset_ms: u64,
    pub max_breaker_auto_resets: u32,
    pub max_tracked_rooms: usize,
    pub max_tracked_connections: usize,
    pub max_connections_per_room: usize,
    pub memory_budget_bytes: u64,
    pub dm_media_eviction: bool,
    pub dm_gateway_eviction: bool,
    pub gateway_connection_id_guard: ConnectionIdGuard,
    pub webhook_enabled: bool,
    pub webhook_port: u16,
    pub livekit_internal_url: Option<String>,
    pub livekit_default_region_id: Option<String>,
}

impl ReconConfig {
    pub fn from_env() -> anyhow::Result<Self> {
        Self::from_env_reader(|name| env::var(name).ok())
    }

    pub fn from_env_reader<F>(get: F) -> anyhow::Result<Self>
    where
        F: Fn(&str) -> Option<String>,
    {
        let shard_count = number(&get, "FLUXER_SVC_SHARD_COUNT", 1u32)?;
        let shard_id = number(&get, "FLUXER_SVC_SHARD_ID", 0u32)?;
        if shard_count != 1 || shard_id != 0 {
            anyhow::bail!(
                "fluxer-recon runs as a single replica: FLUXER_SVC_SHARD_COUNT must be 1 and FLUXER_SVC_SHARD_ID must be 0, got {shard_count} and {shard_id}"
            );
        }

        let config = Self {
            mode: mode(&get, "FLUXER_RECON_MODE", ReconMode::Observing)?,
            worker_threads: number(&get, "FLUXER_RECON_WORKER_THREADS", 2)?,
            tick_ms: number(&get, "FLUXER_RECON_TICK_MS", 50)?,
            turns_per_tick: number(&get, "FLUXER_RECON_TURNS_PER_TICK", 4)?,
            coverage_target_ms: number(&get, "FLUXER_RECON_COVERAGE_TARGET_MS", 30_000)?,
            coverage_period_hard_cap_ms: number(
                &get,
                "FLUXER_RECON_COVERAGE_PERIOD_HARD_CAP_MS",
                120_000,
            )?,
            expected_rooms: number(&get, "FLUXER_RECON_EXPECTED_ROOMS", 800)?,
            hot_period_ms: number(&get, "FLUXER_RECON_HOT_PERIOD_MS", 1_000)?,
            max_hot_rooms: number(&get, "FLUXER_RECON_MAX_HOT_ROOMS", 64)?,
            suspicion_hold_ms: number(&get, "FLUXER_RECON_SUSPICION_HOLD_MS", 30_000)?,
            required_corroborations: number(&get, "FLUXER_RECON_REQUIRED_CORROBORATIONS", 3)?,
            min_corroboration_gap_ms: number(&get, "FLUXER_RECON_MIN_CORROBORATION_GAP_MS", 1_000)?,
            max_corroboration_window_ms: number(
                &get,
                "FLUXER_RECON_MAX_CORROBORATION_WINDOW_MS",
                60_000,
            )?,
            min_divergence_ms: number(&get, "FLUXER_RECON_MIN_DIVERGENCE_MS", 15_000)?,
            preflight_max_age_ms: number(&get, "FLUXER_RECON_PREFLIGHT_MAX_AGE_MS", 1_000)?,
            warmup_seconds: number(&get, "FLUXER_RECON_WARMUP_SECONDS", 120)?,
            pending_join_skew_ms: number(&get, "FLUXER_RECON_PENDING_JOIN_SKEW_MS", 5_000)?,
            census_interval_ms: number(&get, "FLUXER_RECON_CENSUS_INTERVAL_MS", 60_000)?,
            census_max_age_ms: number(&get, "FLUXER_RECON_CENSUS_MAX_AGE_MS", 300_000)?,
            discovery_interval_ms: number(&get, "FLUXER_RECON_DISCOVERY_INTERVAL_MS", 15_000)?,
            topology_refresh_ms: number(&get, "FLUXER_RECON_TOPOLOGY_REFRESH_MS", 60_000)?,
            topology_max_age_ms: number(&get, "FLUXER_RECON_TOPOLOGY_MAX_AGE_MS", 600_000)?,
            topology_drain_grace_ms: number(&get, "FLUXER_RECON_TOPOLOGY_DRAIN_GRACE_MS", 600_000)?,
            room_cliff_hold_ms: number(&get, "FLUXER_RECON_ROOM_CLIFF_HOLD_MS", 60_000)?,
            server_cliff_hold_ms: number(&get, "FLUXER_RECON_SERVER_CLIFF_HOLD_MS", 300_000)?,
            server_cliff_fraction: number(&get, "FLUXER_RECON_SERVER_CLIFF_FRACTION", 0.5)?,
            gateway_read_rps: clamped(
                number(&get, "FLUXER_RECON_GATEWAY_READ_RPS", 40)?,
                READ_RPS_RANGE,
            ),
            gateway_mutate_rpm: clamped(
                number(&get, "FLUXER_RECON_GATEWAY_MUTATE_RPM", 30)?,
                MUTATE_RPM_RANGE,
            ),
            livekit_read_rps: clamped(
                number(&get, "FLUXER_RECON_LIVEKIT_READ_RPS", 40)?,
                READ_RPS_RANGE,
            ),
            livekit_read_rps_per_server: clamped(
                number(&get, "FLUXER_RECON_LIVEKIT_READ_RPS_PER_SERVER", 40)?,
                READ_RPS_RANGE,
            ),
            livekit_mutate_rpm: clamped(
                number(&get, "FLUXER_RECON_LIVEKIT_MUTATE_RPM", 30)?,
                MUTATE_RPM_RANGE,
            ),
            max_inflight_room_turns: number(&get, "FLUXER_RECON_MAX_INFLIGHT_ROOM_TURNS", 4)?,
            max_inflight_gateway: number(&get, "FLUXER_RECON_MAX_INFLIGHT_GATEWAY", 8)?,
            max_inflight_livekit: number(&get, "FLUXER_RECON_MAX_INFLIGHT_LIVEKIT", 8)?,
            max_inflight_per_server: number(&get, "FLUXER_RECON_MAX_INFLIGHT_PER_SERVER", 2)?,
            max_mutations_per_room_per_min: number(
                &get,
                "FLUXER_RECON_MAX_MUTATIONS_PER_ROOM_PER_MIN",
                4,
            )?,
            max_mutations_per_guild_per_min: number(
                &get,
                "FLUXER_RECON_MAX_MUTATIONS_PER_GUILD_PER_MIN",
                8,
            )?,
            max_mutations_per_min: number(&get, "FLUXER_RECON_MAX_MUTATIONS_PER_MIN", 30)?,
            max_action_attempts: number(&get, "FLUXER_RECON_MAX_ACTION_ATTEMPTS", 3)?,
            max_divergent_fraction: number(&get, "FLUXER_RECON_MAX_DIVERGENT_FRACTION", 0.10)?,
            breaker_auto_reset_ms: number(&get, "FLUXER_RECON_BREAKER_AUTO_RESET_MS", 900_000)?,
            max_breaker_auto_resets: number(&get, "FLUXER_RECON_MAX_BREAKER_AUTO_RESETS", 2)?,
            max_tracked_rooms: number(&get, "FLUXER_RECON_MAX_TRACKED_ROOMS", 8_192)?,
            max_tracked_connections: number(&get, "FLUXER_RECON_MAX_TRACKED_CONNECTIONS", 65_536)?,
            max_connections_per_room: number(&get, "FLUXER_RECON_MAX_CONNECTIONS_PER_ROOM", 512)?,
            memory_budget_bytes: number(&get, "FLUXER_RECON_MEMORY_BUDGET_BYTES", 16_777_216)?,
            dm_media_eviction: boolean(&get, "FLUXER_RECON_DM_MEDIA_EVICTION", false)?,
            dm_gateway_eviction: boolean(&get, "FLUXER_RECON_DM_GATEWAY_EVICTION", true)?,
            gateway_connection_id_guard: connection_id_guard(
                &get,
                "FLUXER_RECON_GATEWAY_CONNECTION_ID_GUARD",
                ConnectionIdGuard::AssumeAbsent,
            )?,
            webhook_enabled: boolean(&get, "FLUXER_RECON_WEBHOOK_ENABLED", false)?,
            webhook_port: number(&get, "FLUXER_RECON_WEBHOOK_PORT", 8_092)?,
            livekit_internal_url: text(&get, "FLUXER_LIVEKIT_INTERNAL_URL"),
            livekit_default_region_id: text(&get, "FLUXER_LIVEKIT_DEFAULT_REGION")
                .as_deref()
                .and_then(default_region_id),
        };

        validate(&config)?;
        Ok(config)
    }
}

fn validate(config: &ReconConfig) -> anyhow::Result<()> {
    positive_usize(config.worker_threads, "FLUXER_RECON_WORKER_THREADS")?;
    positive_usize(config.turns_per_tick, "FLUXER_RECON_TURNS_PER_TICK")?;
    positive_usize(
        config.max_inflight_room_turns,
        "FLUXER_RECON_MAX_INFLIGHT_ROOM_TURNS",
    )?;
    positive_usize(
        config.max_inflight_gateway,
        "FLUXER_RECON_MAX_INFLIGHT_GATEWAY",
    )?;
    positive_usize(
        config.max_inflight_livekit,
        "FLUXER_RECON_MAX_INFLIGHT_LIVEKIT",
    )?;
    positive_usize(
        config.max_inflight_per_server,
        "FLUXER_RECON_MAX_INFLIGHT_PER_SERVER",
    )?;
    positive_usize(config.expected_rooms, "FLUXER_RECON_EXPECTED_ROOMS")?;
    positive_usize(config.max_tracked_rooms, "FLUXER_RECON_MAX_TRACKED_ROOMS")?;
    positive_usize(
        config.max_tracked_connections,
        "FLUXER_RECON_MAX_TRACKED_CONNECTIONS",
    )?;
    positive_usize(
        config.max_connections_per_room,
        "FLUXER_RECON_MAX_CONNECTIONS_PER_ROOM",
    )?;
    positive_millis(config.tick_ms, "FLUXER_RECON_TICK_MS")?;
    positive_millis(config.hot_period_ms, "FLUXER_RECON_HOT_PERIOD_MS")?;
    positive_millis(config.census_interval_ms, "FLUXER_RECON_CENSUS_INTERVAL_MS")?;
    positive_millis(
        config.discovery_interval_ms,
        "FLUXER_RECON_DISCOVERY_INTERVAL_MS",
    )?;
    positive_millis(
        config.topology_refresh_ms,
        "FLUXER_RECON_TOPOLOGY_REFRESH_MS",
    )?;
    positive_millis(
        config.preflight_max_age_ms,
        "FLUXER_RECON_PREFLIGHT_MAX_AGE_MS",
    )?;
    positive_millis(
        config.min_corroboration_gap_ms,
        "FLUXER_RECON_MIN_CORROBORATION_GAP_MS",
    )?;
    fraction(
        config.server_cliff_fraction,
        "FLUXER_RECON_SERVER_CLIFF_FRACTION",
    )?;
    fraction(
        config.max_divergent_fraction,
        "FLUXER_RECON_MAX_DIVERGENT_FRACTION",
    )?;

    if config.required_corroborations < MIN_REQUIRED_CORROBORATIONS {
        anyhow::bail!(
            "FLUXER_RECON_REQUIRED_CORROBORATIONS must be at least {MIN_REQUIRED_CORROBORATIONS}, got {}",
            config.required_corroborations
        );
    }

    let spread = config
        .min_corroboration_gap_ms
        .saturating_mul(u64::from(config.required_corroborations.saturating_sub(1)));
    if config.max_corroboration_window_ms <= spread {
        anyhow::bail!(
            "FLUXER_RECON_MAX_CORROBORATION_WINDOW_MS ({}) must exceed the gap times one fewer than the required corroborations ({spread})",
            config.max_corroboration_window_ms
        );
    }

    if config.coverage_target_ms < MIN_COVERAGE_TARGET_MS {
        anyhow::bail!(
            "FLUXER_RECON_COVERAGE_TARGET_MS must be at least {MIN_COVERAGE_TARGET_MS}, got {}",
            config.coverage_target_ms
        );
    }

    if config.coverage_target_ms > config.coverage_period_hard_cap_ms {
        anyhow::bail!(
            "FLUXER_RECON_COVERAGE_TARGET_MS ({}) must not exceed FLUXER_RECON_COVERAGE_PERIOD_HARD_CAP_MS ({})",
            config.coverage_target_ms,
            config.coverage_period_hard_cap_ms
        );
    }

    Ok(())
}

fn text<F>(get: &F, name: &str) -> Option<String>
where
    F: Fn(&str) -> Option<String>,
{
    get(name)
        .map(|value| value.trim().to_owned())
        .filter(|value| !value.is_empty())
}

fn positive_usize(value: usize, name: &str) -> anyhow::Result<()> {
    if value == 0 {
        anyhow::bail!("{name} must be greater than zero");
    }
    Ok(())
}

fn positive_millis(value: u64, name: &str) -> anyhow::Result<()> {
    if value == 0 {
        anyhow::bail!("{name} must be greater than zero");
    }
    Ok(())
}

fn fraction(value: f64, name: &str) -> anyhow::Result<()> {
    if !(value > 0.0 && value <= 1.0) {
        anyhow::bail!("{name} must lie in (0, 1], got {value}");
    }
    Ok(())
}

fn clamped(value: u32, range: (u32, u32)) -> u32 {
    let (low, high) = range;
    value.clamp(low, high)
}

fn read<F>(get: &F, name: &str) -> Option<String>
where
    F: Fn(&str) -> Option<String>,
{
    get(name).filter(|value| !value.is_empty())
}

fn number<F, T>(get: &F, name: &str, default: T) -> anyhow::Result<T>
where
    F: Fn(&str) -> Option<String>,
    T: FromStr,
    T::Err: Display,
{
    match read(get, name) {
        None => Ok(default),
        Some(raw) => raw
            .trim()
            .parse::<T>()
            .map_err(|error| anyhow::anyhow!("invalid {name}: {raw} ({error})")),
    }
}

fn boolean<F>(get: &F, name: &str, default: bool) -> anyhow::Result<bool>
where
    F: Fn(&str) -> Option<String>,
{
    match read(get, name) {
        None => Ok(default),
        Some(raw) => parse_bool(&raw)
            .ok_or_else(|| anyhow::anyhow!("invalid {name}: {raw} is not a boolean")),
    }
}

fn mode<F>(get: &F, name: &str, default: ReconMode) -> anyhow::Result<ReconMode>
where
    F: Fn(&str) -> Option<String>,
{
    match read(get, name) {
        None => Ok(default),
        Some(raw) => ReconMode::parse(&raw).ok_or_else(|| anyhow::anyhow!("invalid {name}: {raw}")),
    }
}

fn connection_id_guard<F>(
    get: &F,
    name: &str,
    default: ConnectionIdGuard,
) -> anyhow::Result<ConnectionIdGuard>
where
    F: Fn(&str) -> Option<String>,
{
    match read(get, name) {
        None => Ok(default),
        Some(raw) => {
            ConnectionIdGuard::parse(&raw).ok_or_else(|| anyhow::anyhow!("invalid {name}: {raw}"))
        }
    }
}

fn parse_bool(value: &str) -> Option<bool> {
    match value.trim().to_ascii_lowercase().as_str() {
        "1" | "true" | "yes" | "y" | "on" => Some(true),
        "0" | "false" | "no" | "n" | "off" => Some(false),
        _ => None,
    }
}
