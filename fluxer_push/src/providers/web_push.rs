// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::crypto;
use crate::payload::{self, RecordKind};
use crate::providers::SendOutcome;
use crate::resolver;
use crate::server::AppState;
use crate::subscription::Subscription;
use crate::vendor::{Unreachable, is_transient_status};
use rand::RngExt as _;
use reqwest::header::{AUTHORIZATION, CONTENT_ENCODING, CONTENT_TYPE};
use serde_json::Value;
use std::net::IpAddr;
use std::time::Duration;
use tracing::warn;
use url::{Host, Url};

pub const RECORD_SIZE: usize = 2816;

const HEADER_BYTES: usize = 86;
const TAG_BYTES: usize = 16;
const PADDING_DELIMITER_BYTES: usize = 1;
pub const PLAINTEXT_BUDGET: usize =
    RECORD_SIZE - HEADER_BYTES - TAG_BYTES - PADDING_DELIMITER_BYTES;

const MAX_TRANSIENT_RETRIES: u32 = 2;
const BASE_RETRY_DELAY_MS: u64 = 200;
const MAX_RETRY_DELAY_MS: u64 = 2_000;
const ALERT_TTL_SECONDS: &str = "86400";
const CLEAR_TTL_SECONDS: &str = "86400";
const RING_TTL_SECONDS: &str = "0";
const TTL_HEADER: &str = "TTL";
const URGENCY_HEADER: &str = "Urgency";
const ALERT_URGENCY: &str = "high";
const CLEAR_URGENCY: &str = "low";
const OCTET_STREAM: &str = "application/octet-stream";
const AES128GCM: &str = "aes128gcm";
const NOT_FOUND: u16 = 404;
const GONE: u16 = 410;
const MAX_HOSTNAME_BYTES: usize = 253;
const MAX_LABEL_BYTES: usize = 63;

pub async fn send(state: &AppState, sub: &Subscription, envelope: &Value) -> SendOutcome {
    if !endpoint_is_allowed(&sub.endpoint) {
        return SendOutcome::permanent("endpoint_rejected");
    }
    let (Some(p256dh), Some(auth)) = (sub.p256dh_key.as_deref(), sub.auth_key.as_deref()) else {
        return SendOutcome::permanent("missing_keys");
    };
    let (Ok(p256dh), Ok(auth)) = (
        crypto::decode_subscription_key(p256dh),
        crypto::decode_subscription_key(auth),
    ) else {
        return SendOutcome::permanent("invalid_keys");
    };

    let vapid = &state.cfg.vapid;
    let token = match state
        .tokens
        .vapid(&origin_of(&sub.endpoint), vapid, &state.metrics)
        .await
    {
        Ok(token) => token,
        Err(error) => return SendOutcome::permanent(format!("vapid_token: {error}")),
    };
    let authorization = format!("vapid t={token}, k={}", vapid.public_key);

    let (plaintext, shrunk) = payload::fit(envelope, PLAINTEXT_BUDGET);
    if let Some(step) = shrunk {
        state.metrics.record_payload_shrink(step);
    }
    let body = match crypto::encrypt_aes128gcm(&plaintext, &p256dh, &auth, RECORD_SIZE) {
        Ok(body) => body,
        Err(error) => return SendOutcome::permanent(format!("encrypt: {error}")),
    };
    let (ttl_seconds, urgency) = delivery_headers(envelope);

    let mut attempt: u32 = 0;
    loop {
        let response = state
            .web_push_http
            .post(&sub.endpoint)
            .header(TTL_HEADER, ttl_seconds)
            .header(URGENCY_HEADER, urgency)
            .header(CONTENT_TYPE, OCTET_STREAM)
            .header(CONTENT_ENCODING, AES128GCM)
            .header(AUTHORIZATION, &authorization)
            .body(body.clone())
            .send()
            .await;

        let status = match response {
            Ok(response) => response.status().as_u16(),
            Err(error) => {
                let unreachable = Unreachable::of(&error);
                warn!(
                    error = %error.without_url(),
                    kind = unreachable.label(),
                    endpoint = %origin_of(&sub.endpoint),
                    "web push request did not complete"
                );
                if unreachable.is_permanent() {
                    return SendOutcome::permanent(unreachable.label());
                }
                if attempt >= MAX_TRANSIENT_RETRIES {
                    return SendOutcome::transient(unreachable.label());
                }
                tokio::time::sleep(retry_delay(attempt)).await;
                attempt += 1;
                continue;
            }
        };
        if is_transient_status(status) && attempt < MAX_TRANSIENT_RETRIES {
            tokio::time::sleep(retry_delay(attempt)).await;
            attempt += 1;
            continue;
        }
        return classify(status);
    }
}

fn delivery_headers(envelope: &Value) -> (&'static str, &'static str) {
    match payload::record_kind(envelope) {
        RecordKind::Message => (ALERT_TTL_SECONDS, ALERT_URGENCY),
        RecordKind::Clear => (CLEAR_TTL_SECONDS, CLEAR_URGENCY),
        RecordKind::Ring => (RING_TTL_SECONDS, ALERT_URGENCY),
    }
}

fn classify(status: u16) -> SendOutcome {
    match status {
        200..=299 => SendOutcome::Accepted,
        GONE => SendOutcome::TokenInvalid { reason: "expired" },
        NOT_FOUND => SendOutcome::TokenInvalid {
            reason: "not_found",
        },
        _ if is_transient_status(status) => SendOutcome::transient(format!("http_{status}")),
        _ => SendOutcome::permanent(format!("http_{status}")),
    }
}

fn endpoint_is_allowed(endpoint: &str) -> bool {
    let Ok(url) = Url::parse(endpoint) else {
        return false;
    };
    if url.scheme() != "https" {
        return false;
    }
    if !url.username().is_empty() || url.password().is_some() {
        return false;
    }
    if !matches!(url.port_or_known_default(), Some(80 | 443)) {
        return false;
    }
    match url.host() {
        Some(Host::Domain(host)) => is_public_hostname(host),
        Some(Host::Ipv4(ip)) => !resolver::is_blocked(IpAddr::V4(ip)),
        Some(Host::Ipv6(ip)) => !resolver::is_blocked(IpAddr::V6(ip)),
        None => false,
    }
}

fn is_public_hostname(host: &str) -> bool {
    let host = host.strip_suffix('.').unwrap_or(host);
    if host.is_empty() || host.len() > MAX_HOSTNAME_BYTES || !host.contains('.') {
        return false;
    }
    let mut labels = host.split('.');
    let mut top_level = "";
    for label in &mut labels {
        if !is_hostname_label(label) {
            return false;
        }
        top_level = label;
    }
    !top_level.bytes().all(|byte| byte.is_ascii_digit())
}

fn is_hostname_label(label: &str) -> bool {
    let bytes = label.as_bytes();
    let (Some(first), Some(last)) = (bytes.first(), bytes.last()) else {
        return false;
    };
    bytes.len() <= MAX_LABEL_BYTES
        && first.is_ascii_alphanumeric()
        && last.is_ascii_alphanumeric()
        && bytes
            .iter()
            .all(|byte| byte.is_ascii_alphanumeric() || *byte == b'-')
}

fn origin_of(endpoint: &str) -> String {
    match endpoint.split_once("://") {
        Some((scheme, rest)) => {
            let host = rest.split('/').next().unwrap_or(rest);
            format!("{scheme}://{host}")
        }
        None => endpoint.to_owned(),
    }
}

fn retry_delay(attempt: u32) -> Duration {
    let base = MAX_RETRY_DELAY_MS.min(
        BASE_RETRY_DELAY_MS
            .checked_shl(attempt)
            .unwrap_or(MAX_RETRY_DELAY_MS),
    );
    let jitter = rand::rng().random_range(1..=(base / 4).max(1));
    Duration::from_millis(MAX_RETRY_DELAY_MS.min(base + jitter - 1))
}
