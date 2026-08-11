// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::config::{AppProxyConfig, ReleaseChannel};
use axum::http::{HeaderMap, header};
use std::net::{IpAddr, Ipv4Addr};

const TIME_FREEZE_EXEMPT_CLIENT_IP: IpAddr = IpAddr::V4(Ipv4Addr::new(188, 149, 230, 148));
const CANARY_USER_AGENT_PRODUCT: &str = "FluxerCanary/";
const CANARY_LIVE_WEB_CLIENT_MIN_VERSION: CalVer = CalVer {
    year: 2026,
    month_day: 614,
    micro: 0,
};

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
struct CalVer {
    year: u16,
    month_day: u16,
    micro: u32,
}

#[derive(Clone, Debug)]
pub struct FrozenSnapshot {
    pub sha: String,
    pub index_html: Vec<u8>,
    pub sw_js: Vec<u8>,
    pub version_json: Vec<u8>,
}

#[derive(Clone, Debug)]
pub struct TimeFreezeConfig {
    pub enabled: bool,
    pub release_channel: ReleaseChannel,
    pub snapshot: Option<FrozenSnapshot>,
    pub client_exempt: bool,
}

#[derive(Clone, Debug)]
pub struct TimeFreezeDebug {
    pub decision: TimeFreezeDecision,
    pub snapshot_sha: Option<String>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TimeFreezeDecision {
    Frozen,
    Live,
    NoSnapshot,
}

pub fn load_time_freeze_config_for_request(
    app_config: &AppProxyConfig,
    headers: &HeaderMap,
) -> TimeFreezeConfig {
    let client_exempt = is_time_freeze_exempt_client(
        headers,
        app_config.trust_client_ip_header,
        &app_config.client_ip_header_name,
    );
    load_time_freeze_config_for_index_source(
        app_config.release_channel,
        client_exempt,
        app_config.index_upstream_url.is_some(),
        app_config.time_freeze_enabled,
    )
}

pub fn load_time_freeze_config(
    release_channel: ReleaseChannel,
    client_exempt: bool,
) -> TimeFreezeConfig {
    load_time_freeze_config_for_index_source(release_channel, client_exempt, false, true)
}

pub fn load_time_freeze_config_for_index_source(
    release_channel: ReleaseChannel,
    client_exempt: bool,
    has_index_upstream: bool,
    enabled: bool,
) -> TimeFreezeConfig {
    if !enabled || has_index_upstream {
        return TimeFreezeConfig {
            enabled,
            release_channel,
            snapshot: None,
            client_exempt,
        };
    }

    let snapshot: Option<FrozenSnapshot> = load_frozen_snapshot(release_channel);

    TimeFreezeConfig {
        enabled,
        release_channel,
        snapshot,
        client_exempt,
    }
}

#[cfg(feature = "time-freeze")]
fn load_frozen_snapshot(release_channel: ReleaseChannel) -> Option<FrozenSnapshot> {
    match release_channel {
        ReleaseChannel::Stable => Some(crate::frozen_snapshots::STABLE_SNAPSHOT.clone()),
        ReleaseChannel::Canary => None,
    }
}

#[cfg(not(feature = "time-freeze"))]
fn load_frozen_snapshot(_release_channel: ReleaseChannel) -> Option<FrozenSnapshot> {
    None
}

pub fn should_serve_frozen(config: &TimeFreezeConfig) -> Option<&FrozenSnapshot> {
    if !config.enabled {
        return None;
    }
    if config.client_exempt {
        return None;
    }
    config.snapshot.as_ref()
}

pub fn describe_decision(config: &TimeFreezeConfig) -> TimeFreezeDebug {
    let decision = match &config.snapshot {
        None => TimeFreezeDecision::NoSnapshot,
        Some(_) if should_serve_frozen(config).is_some() => TimeFreezeDecision::Frozen,
        Some(_) => TimeFreezeDecision::Live,
    };
    TimeFreezeDebug {
        decision,
        snapshot_sha: config.snapshot.as_ref().map(|s| s.sha.clone()),
    }
}

#[cfg(feature = "time-freeze")]
pub fn time_freeze_debug_header(config: &TimeFreezeConfig) -> Option<String> {
    if !config.enabled {
        return None;
    }

    Some(format_debug_header(&describe_decision(config)))
}

#[cfg(not(feature = "time-freeze"))]
pub fn time_freeze_debug_header(_config: &TimeFreezeConfig) -> Option<String> {
    None
}

pub fn is_time_freeze_exempt_client(
    headers: &HeaderMap,
    trust_client_ip_header: bool,
    header_name: &str,
) -> bool {
    extract_client_ip(headers, trust_client_ip_header, header_name)
        .is_some_and(|ip| ip == TIME_FREEZE_EXEMPT_CLIENT_IP)
        || is_time_freeze_exempt_user_agent(headers)
}

fn is_time_freeze_exempt_user_agent(headers: &HeaderMap) -> bool {
    let Some(user_agent) = headers
        .get(header::USER_AGENT)
        .and_then(|value| value.to_str().ok())
    else {
        return false;
    };

    user_agent
        .match_indices(CANARY_USER_AGENT_PRODUCT)
        .any(|(index, _)| {
            let version_start = index + CANARY_USER_AGENT_PRODUCT.len();
            let version = user_agent[version_start..]
                .chars()
                .take_while(|ch| ch.is_ascii_digit() || *ch == '.')
                .collect::<String>();
            parse_calver(&version)
                .is_some_and(|version| version >= CANARY_LIVE_WEB_CLIENT_MIN_VERSION)
        })
}

fn parse_calver(value: &str) -> Option<CalVer> {
    let mut parts = value.split('.');
    let year = parts.next()?;
    let month_day = parts.next()?;
    let micro = parts.next()?;
    if parts.next().is_some()
        || year.len() != 4
        || !valid_nonzero_digits(year, 4)
        || !(month_day.len() == 3 || month_day.len() == 4)
        || !valid_nonzero_digits(month_day, month_day.len())
        || !valid_micro(micro)
    {
        return None;
    }

    let year = year.parse().ok()?;
    let month_day_value: u16 = month_day.parse().ok()?;
    let month = month_day_value / 100;
    let day = month_day_value % 100;
    if !(1..=12).contains(&month) || !(1..=31).contains(&day) {
        return None;
    }

    let micro = micro.parse().ok()?;
    let hour = micro / 10_000;
    let minute = (micro / 100) % 100;
    let second = micro % 100;
    if hour > 23 || minute > 59 || second > 59 {
        return None;
    }

    Some(CalVer {
        year,
        month_day: month_day_value,
        micro,
    })
}

fn valid_nonzero_digits(value: &str, max_len: usize) -> bool {
    !value.is_empty()
        && value.len() <= max_len
        && !value.starts_with('0')
        && value.bytes().all(|byte| byte.is_ascii_digit())
}

fn valid_micro(value: &str) -> bool {
    value == "0" || valid_nonzero_digits(value, 6)
}

fn extract_client_ip(
    headers: &HeaderMap,
    trust_client_ip_header: bool,
    header_name: &str,
) -> Option<IpAddr> {
    if !trust_client_ip_header {
        return None;
    }
    let header_value = headers.get(header_name)?.to_str().ok()?;
    let first_hop = header_value.split(',').next()?.trim();
    parse_ip(first_hop)
}

fn parse_ip(value: &str) -> Option<IpAddr> {
    if value.is_empty() {
        return None;
    }
    if let Ok(ip) = value.parse() {
        return Some(ip);
    }
    if let Some(stripped) = value
        .strip_prefix('[')
        .and_then(|value| value.split_once(']').map(|(host, _)| host))
    {
        return stripped.parse().ok();
    }
    if value.matches(':').count() == 1
        && let Some((host, _port)) = value.rsplit_once(':')
    {
        return host.parse().ok();
    }
    None
}

pub fn format_debug_header(debug: &TimeFreezeDebug) -> String {
    let decision_str = match debug.decision {
        TimeFreezeDecision::Frozen => "frozen",
        TimeFreezeDecision::Live => "live",
        TimeFreezeDecision::NoSnapshot => "no-snapshot",
    };
    match &debug.snapshot_sha {
        Some(sha) => format!("{decision_str}; sha={sha}"),
        None => decision_str.to_owned(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use axum::http::{HeaderMap, HeaderValue};

    #[cfg(feature = "time-freeze")]
    #[test]
    fn stable_channel_has_frozen_snapshot() {
        let config = load_time_freeze_config(ReleaseChannel::Stable, false);
        assert!(
            config.snapshot.is_some(),
            "stable channel should have a frozen snapshot"
        );
        assert!(should_serve_frozen(&config).is_some());
    }

    #[cfg(feature = "time-freeze")]
    #[test]
    fn canary_channel_has_no_frozen_snapshot() {
        let config = load_time_freeze_config(ReleaseChannel::Canary, false);
        assert_eq!(config.release_channel, ReleaseChannel::Canary);
        assert!(config.snapshot.is_none());
        assert!(should_serve_frozen(&config).is_none());

        let debug = describe_decision(&config);
        assert_eq!(debug.decision, TimeFreezeDecision::NoSnapshot);
        assert!(debug.snapshot_sha.is_none());
    }

    #[test]
    fn index_upstream_disables_frozen_snapshot() {
        let config =
            load_time_freeze_config_for_index_source(ReleaseChannel::Canary, false, true, true);
        assert!(config.enabled);
        assert_eq!(config.release_channel, ReleaseChannel::Canary);
        assert!(config.snapshot.is_none());
        assert!(should_serve_frozen(&config).is_none());

        let debug = describe_decision(&config);
        assert_eq!(debug.decision, TimeFreezeDecision::NoSnapshot);
        assert!(debug.snapshot_sha.is_none());
    }

    #[test]
    fn disabled_time_freeze_has_no_snapshot_or_debug_header() {
        let config =
            load_time_freeze_config_for_index_source(ReleaseChannel::Stable, false, false, false);

        assert!(!config.enabled);
        assert!(config.snapshot.is_none());
        assert!(should_serve_frozen(&config).is_none());
        assert!(time_freeze_debug_header(&config).is_none());
    }

    #[cfg(not(feature = "time-freeze"))]
    #[test]
    fn no_feature_build_has_no_frozen_snapshot() {
        let config = load_time_freeze_config(ReleaseChannel::Stable, false);

        assert!(config.enabled);
        assert!(config.snapshot.is_none());
        assert!(should_serve_frozen(&config).is_none());
        assert!(time_freeze_debug_header(&config).is_none());
    }

    #[cfg(feature = "time-freeze")]
    #[test]
    fn exempt_client_gets_live_content_even_with_snapshot() {
        let config = load_time_freeze_config(ReleaseChannel::Stable, true);
        assert!(config.snapshot.is_some());
        assert!(should_serve_frozen(&config).is_none());

        let debug = describe_decision(&config);
        assert_eq!(debug.decision, TimeFreezeDecision::Live);
    }

    #[test]
    fn describe_decision_no_snapshot() {
        let config = TimeFreezeConfig {
            enabled: true,
            release_channel: ReleaseChannel::Stable,
            snapshot: None,
            client_exempt: false,
        };
        let debug = describe_decision(&config);
        assert_eq!(debug.decision, TimeFreezeDecision::NoSnapshot);
        assert!(debug.snapshot_sha.is_none());
    }

    #[test]
    fn describe_decision_frozen() {
        let config = TimeFreezeConfig {
            enabled: true,
            release_channel: ReleaseChannel::Stable,
            snapshot: Some(FrozenSnapshot {
                sha: "deadbeef".to_owned(),
                index_html: vec![],
                sw_js: vec![],
                version_json: vec![],
            }),
            client_exempt: false,
        };
        let debug = describe_decision(&config);
        assert_eq!(debug.decision, TimeFreezeDecision::Frozen);
        assert_eq!(debug.snapshot_sha.as_deref(), Some("deadbeef"));
    }

    #[test]
    fn format_debug_header_no_snapshot() {
        let debug = TimeFreezeDebug {
            decision: TimeFreezeDecision::NoSnapshot,
            snapshot_sha: None,
        };
        assert_eq!(format_debug_header(&debug), "no-snapshot");
    }

    #[test]
    fn format_debug_header_frozen() {
        let debug = TimeFreezeDebug {
            decision: TimeFreezeDecision::Frozen,
            snapshot_sha: Some("abc".to_owned()),
        };
        assert_eq!(format_debug_header(&debug), "frozen; sha=abc");
    }

    #[test]
    fn format_debug_header_live() {
        let debug = TimeFreezeDebug {
            decision: TimeFreezeDecision::Live,
            snapshot_sha: Some("def".to_owned()),
        };
        assert_eq!(format_debug_header(&debug), "live; sha=def");
    }

    #[test]
    fn recognizes_exempt_x_real_ip_when_trusted() {
        let mut headers = HeaderMap::new();
        headers.insert("x-real-ip", HeaderValue::from_static("188.149.230.148"));

        assert!(is_time_freeze_exempt_client(&headers, true, "x-real-ip"));
        assert!(!is_time_freeze_exempt_client(&headers, false, "x-real-ip"));
    }

    #[test]
    fn recognizes_exempt_first_forwarded_ip() {
        let mut headers = HeaderMap::new();
        headers.insert(
            "x-forwarded-for",
            HeaderValue::from_static("188.149.230.148, 10.0.0.4"),
        );

        assert!(is_time_freeze_exempt_client(
            &headers,
            true,
            "x-forwarded-for"
        ));
    }

    #[test]
    fn non_exempt_ip_stays_frozen() {
        let mut headers = HeaderMap::new();
        headers.insert("x-real-ip", HeaderValue::from_static("203.0.113.10"));

        assert!(!is_time_freeze_exempt_client(&headers, true, "x-real-ip"));
    }

    #[test]
    fn canary_user_agent_at_min_version_is_exempt() {
        let mut headers = HeaderMap::new();
        headers.insert(
            header::USER_AGENT,
            HeaderValue::from_static("FluxerCanary/2026.614.0"),
        );

        assert!(is_time_freeze_exempt_client(&headers, false, "x-real-ip"));
    }

    #[test]
    fn canary_user_agent_newer_than_min_version_is_exempt() {
        let mut headers = HeaderMap::new();
        headers.insert(
            header::USER_AGENT,
            HeaderValue::from_static("Mozilla/5.0 FluxerCanary/2026.614.83512 Electron/99.0"),
        );

        assert!(is_time_freeze_exempt_client(&headers, false, "x-real-ip"));
    }

    #[test]
    fn canary_user_agent_before_min_version_stays_frozen() {
        let mut headers = HeaderMap::new();
        headers.insert(
            header::USER_AGENT,
            HeaderValue::from_static("FluxerCanary/2026.613.235959"),
        );

        assert!(!is_time_freeze_exempt_client(&headers, false, "x-real-ip"));
    }

    #[test]
    fn malformed_canary_user_agent_stays_frozen() {
        let mut headers = HeaderMap::new();
        headers.insert(
            header::USER_AGENT,
            HeaderValue::from_static("FluxerCanary/2026.614.not-a-calver"),
        );

        assert!(!is_time_freeze_exempt_client(&headers, false, "x-real-ip"));
    }

    #[test]
    fn stable_user_agent_stays_frozen() {
        let mut headers = HeaderMap::new();
        headers.insert(
            header::USER_AGENT,
            HeaderValue::from_static("FluxerStable/2026.614.83512"),
        );

        assert!(!is_time_freeze_exempt_client(&headers, false, "x-real-ip"));
    }
}
