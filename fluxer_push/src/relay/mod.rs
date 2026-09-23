// SPDX-License-Identifier: AGPL-3.0-or-later

mod client_ip;
pub mod envelope;
mod quota;
pub mod reject;

use crate::config::{ProviderEnvironment, RelayConfig};
use crate::metrics::{Metrics, RelayLeg, RelayResult};
use crate::server::{Sidecar, serve, sidecar_router};
use crate::tokens::{TokenCache, TokenError};
use crate::unix_seconds;
use crate::vendor::{self, ApnsRequest, DeadToken, Refusal, VendorOutcome};
use axum::Router;
use axum::body::Body;
use axum::extract::{ConnectInfo, Path, State};
use axum::http::{HeaderMap, HeaderValue, StatusCode, header};
use axum::response::{IntoResponse, Response};
use axum::routing::post;
use base64::prelude::*;
use envelope::Urgency;
use fluxer_svc::shutdown::wait_for_shutdown;
use quota::Quota;
use rand::Rng as _;
use reject::{Reason, Rejection};
use sha2::{Digest as _, Sha256};
use std::net::SocketAddr;
use std::sync::{Arc, LazyLock};
use std::time::{Duration, Instant};
use tracing::{info, warn};

pub const APNS_ROUTE: &str = "/relay/v1/apns/{app_id}/{environment}/{device_token}";
pub const APNS_VOIP_ROUTE: &str = "/relay/v1/apns-voip/{app_id}/{environment}/{device_token}";
pub const FCM_ROUTE: &str = "/relay/v1/fcm/{app_id}/{device_token}";

const AES128GCM: &str = "aes128gcm";
const TTL_HEADER: &str = "ttl";
const URGENCY_HEADER: &str = "urgency";
const JSON_CONTENT_TYPE: &str = "application/json";
const DIGEST_BYTES: usize = 8;
const LOG_SALT_BYTES: usize = 16;

static LOG_SALT: LazyLock<[u8; LOG_SALT_BYTES]> = LazyLock::new(|| {
    let mut salt = [0u8; LOG_SALT_BYTES];
    rand::rng().fill_bytes(&mut salt);
    salt
});
const MIN_APNS_DEVICE_TOKEN_LEN: usize = 64;
const MAX_APNS_DEVICE_TOKEN_LEN: usize = 256;
const MAX_FCM_DEVICE_TOKEN_LEN: usize = 512;
const MAX_TTL_SECONDS: i64 = 86_400;
const BODY_READ_TIMEOUT: Duration = Duration::from_secs(15);

pub struct AppState {
    pub(crate) cfg: RelayConfig,
    pub(crate) metrics: Arc<Metrics>,
    pub(crate) sidecar: Arc<Sidecar>,
    quota: Quota,
    http: reqwest::Client,
    apns_http: reqwest::Client,
    tokens: TokenCache,
}

impl AppState {
    pub(crate) fn try_new(cfg: RelayConfig) -> anyhow::Result<Self> {
        let metrics = Arc::new(Metrics::new());
        Ok(Self {
            sidecar: Arc::new(Sidecar::new(Arc::clone(&metrics))),
            quota: Quota::new(
                cfg.max_concurrent,
                &cfg.device_token_bucket,
                cfg.source_bucket.as_ref(),
            ),
            http: vendor::http_client()?,
            apns_http: vendor::apns_http_client()?,
            tokens: TokenCache::new(),
            metrics,
            cfg,
        })
    }
}

pub async fn run(cfg: RelayConfig) -> anyhow::Result<()> {
    let state = Arc::new(AppState::try_new(cfg)?);
    let addr = state.cfg.bind_addr;
    let serving = serve(addr, router(Arc::clone(&state))).await?;
    state.sidecar.set_serving(true);
    info!(
        %addr,
        apns = state.cfg.apns.is_some(),
        fcm = state.cfg.fcm.is_some(),
        max_body_bytes = state.cfg.max_body_bytes,
        source_rate_limit = source_rate_limit_owner(&state.cfg),
        trust_client_ip_header = state.cfg.trust_client_ip_header,
        "push relay listening"
    );

    wait_for_shutdown().await;
    state.sidecar.set_serving(false);
    serving.stop().await;
    Ok(())
}

fn source_rate_limit_owner(cfg: &RelayConfig) -> &'static str {
    if cfg.source_bucket.is_some() {
        "relay"
    } else {
        "edge"
    }
}

pub fn router(state: Arc<AppState>) -> Router {
    let sidecar = Arc::clone(&state.sidecar);
    Router::new()
        .route(APNS_ROUTE, post(apns_route))
        .route(APNS_VOIP_ROUTE, post(apns_voip_route))
        .route(FCM_ROUTE, post(fcm_route))
        .fallback(unmatched_route)
        .with_state(state)
        .merge(sidecar_router(sidecar))
}

async fn unmatched_route(State(state): State<Arc<AppState>>) -> Response {
    let rejection = Rejection::new(Reason::BadRequest);
    state.metrics.record_relay_rejected(rejection.reason);
    respond(rejection.reason.status(), Some(rejection))
}

async fn apns_route(
    State(state): State<Arc<AppState>>,
    ConnectInfo(peer): ConnectInfo<SocketAddr>,
    Path(path): Path<(String, String, String)>,
    headers: HeaderMap,
    body: Body,
) -> Response {
    apns_leg(&state, RelayLeg::Apns, peer, path, headers, body).await
}

async fn apns_voip_route(
    State(state): State<Arc<AppState>>,
    ConnectInfo(peer): ConnectInfo<SocketAddr>,
    Path(path): Path<(String, String, String)>,
    headers: HeaderMap,
    body: Body,
) -> Response {
    apns_leg(&state, RelayLeg::ApnsVoip, peer, path, headers, body).await
}

async fn apns_leg(
    state: &AppState,
    leg: RelayLeg,
    peer: SocketAddr,
    (app_id, environment, device_token): (String, String, String),
    headers: HeaderMap,
    body: Body,
) -> Response {
    relay(
        state,
        Incoming {
            leg,
            app_id,
            environment: ProviderEnvironment::from_label(&environment),
            device_token,
            peer,
        },
        headers,
        body,
    )
    .await
}

async fn fcm_route(
    State(state): State<Arc<AppState>>,
    ConnectInfo(peer): ConnectInfo<SocketAddr>,
    Path((app_id, device_token)): Path<(String, String)>,
    headers: HeaderMap,
    body: Body,
) -> Response {
    relay(
        &state,
        Incoming {
            leg: RelayLeg::Fcm,
            app_id,
            environment: None,
            device_token,
            peer,
        },
        headers,
        body,
    )
    .await
}

struct Incoming {
    leg: RelayLeg,
    app_id: String,
    environment: Option<ProviderEnvironment>,
    device_token: String,
    peer: SocketAddr,
}

struct Delivery {
    urgency: Urgency,
    ttl_seconds: i64,
}

enum Target<'a> {
    Apns {
        environment: ProviderEnvironment,
        topic: &'a str,
    },
    Fcm {
        project_id: &'a str,
    },
}

async fn relay(state: &AppState, incoming: Incoming, headers: HeaderMap, body: Body) -> Response {
    let started = Instant::now();
    let mut bytes = 0;
    let outcome = forward(state, &incoming, &headers, body, &mut bytes).await;

    let rejection = outcome.err();
    let status = rejection.map_or(StatusCode::OK, |rejection| rejection.reason.status());
    state.metrics.record_relay_served(
        incoming.leg,
        if status.is_success() {
            RelayResult::Accepted
        } else if status.is_server_error() {
            RelayResult::Failed
        } else {
            RelayResult::Rejected
        },
    );
    if let Some(rejection) = rejection {
        state.metrics.record_relay_rejected(rejection.reason);
    }

    info!(
        leg = incoming.leg.label(),
        app_id = incoming.app_id,
        environment = incoming.environment.map_or("-", ProviderEnvironment::label),
        device_token = digest(&incoming.device_token),
        status = status.as_u16(),
        reason = rejection.map_or("accepted", |rejection| rejection.reason.label()),
        bytes,
        duration_ms = started.elapsed().as_millis(),
        "relay request"
    );
    respond(status, rejection)
}

async fn forward(
    state: &AppState,
    incoming: &Incoming,
    headers: &HeaderMap,
    body: Body,
    bytes: &mut usize,
) -> Result<(), Rejection> {
    let _permit = state.quota.admit()?;
    let target = resolve(state, incoming)?;
    if !device_token_is_shaped(incoming.leg, &incoming.device_token) {
        return Err(Rejection::new(Reason::DeviceTokenInvalid));
    }
    if !is_aes128gcm(headers) {
        return Err(Rejection::new(Reason::BadRequest));
    }
    let delivery = Delivery {
        urgency: Urgency::from_header(header(headers, URGENCY_HEADER))
            .ok_or(Rejection::new(Reason::BadRequest))?,
        ttl_seconds: ttl_seconds(headers)?,
    };
    state.quota.take(
        &state.metrics,
        &incoming.device_token,
        client_ip::for_rate_limit(&state.cfg, incoming.peer, headers),
        Instant::now(),
    )?;

    let body = tokio::time::timeout(
        BODY_READ_TIMEOUT,
        axum::body::to_bytes(body, state.cfg.max_body_bytes),
    )
    .await
    .map_err(|_| Rejection::new(Reason::BadRequest))?
    .map_err(|_| Rejection::new(Reason::PayloadTooLarge))?;
    *bytes = body.len();
    if body.is_empty() {
        return Err(Rejection::new(Reason::BadRequest));
    }
    let payload = envelope::encode_payload(&body);

    match target {
        Target::Apns { environment, topic } => {
            send_apns(state, incoming, &delivery, &payload, environment, topic).await
        }
        Target::Fcm { project_id } => {
            send_fcm(state, incoming, &delivery, &payload, project_id).await
        }
    }
}

fn resolve<'a>(state: &'a AppState, incoming: &Incoming) -> Result<Target<'a>, Rejection> {
    let unknown = Rejection::new(Reason::AppUnknown);
    match incoming.leg {
        RelayLeg::Apns | RelayLeg::ApnsVoip => {
            let cfg = state.cfg.apns.as_ref().ok_or(unknown)?;
            let environment = incoming.environment.ok_or(unknown)?;
            let topic = match incoming.leg {
                RelayLeg::ApnsVoip => cfg.voip_topic_for(&incoming.app_id, environment),
                _ => cfg.topic_for(&incoming.app_id, environment),
            };
            Ok(Target::Apns {
                environment,
                topic: topic.ok_or(unknown)?,
            })
        }
        RelayLeg::Fcm => {
            let cfg = state.cfg.fcm.as_ref().ok_or(unknown)?;
            Ok(Target::Fcm {
                project_id: cfg.listed_project_id(&incoming.app_id).ok_or(unknown)?,
            })
        }
    }
}

async fn send_apns(
    state: &AppState,
    incoming: &Incoming,
    delivery: &Delivery,
    payload: &str,
    environment: ProviderEnvironment,
    topic: &str,
) -> Result<(), Rejection> {
    let cfg = state
        .cfg
        .apns
        .as_ref()
        .ok_or(Rejection::new(Reason::AppUnknown))?;
    let (headers, body) = match incoming.leg {
        RelayLeg::ApnsVoip => (
            envelope::apns_voip_headers(),
            envelope::apns_voip_body(payload)?,
        ),
        _ => (
            envelope::apns_headers(delivery.urgency, unix_seconds(), delivery.ttl_seconds),
            envelope::apns_body(payload, delivery.urgency)?,
        ),
    };
    let request = ApnsRequest {
        environment,
        topic,
        device_token: &incoming.device_token,
        headers: &headers,
        body,
    };
    let outcome = vendor::send_apns(
        &state.apns_http,
        &state.tokens,
        &state.metrics,
        cfg,
        request,
    )
    .await;
    finish(state, incoming.leg, outcome)
}

async fn send_fcm(
    state: &AppState,
    incoming: &Incoming,
    delivery: &Delivery,
    payload: &str,
    project_id: &str,
) -> Result<(), Rejection> {
    let cfg = state
        .cfg
        .fcm
        .as_ref()
        .ok_or(Rejection::new(Reason::AppUnknown))?;
    let body = envelope::fcm_body(
        &incoming.device_token,
        payload,
        delivery.urgency,
        delivery.ttl_seconds,
    )?;
    let outcome = vendor::send_fcm(
        &state.http,
        &state.tokens,
        &state.metrics,
        cfg,
        project_id,
        body,
    )
    .await;
    finish(state, RelayLeg::Fcm, outcome)
}

fn finish(
    state: &AppState,
    leg: RelayLeg,
    outcome: Result<VendorOutcome, TokenError>,
) -> Result<(), Rejection> {
    let outcome = outcome.map_err(|error| {
        warn!(%error, leg = leg.label(), "the relay could not mint its own vendor credential");
        Rejection::new(Reason::Internal)
    })?;
    let (result, verdict) = match outcome {
        VendorOutcome::Accepted => (RelayResult::Accepted, Ok(())),
        VendorOutcome::Unreachable(_) => (
            RelayResult::Failed,
            Err(Rejection::new(Reason::ProviderUnavailable)),
        ),
        VendorOutcome::Refused(refusal) => (
            if refusal.is_transient() {
                RelayResult::Failed
            } else {
                RelayResult::Rejected
            },
            Err(Rejection::new(refusal_reason(&refusal))),
        ),
    };
    state.metrics.record_relay_vendor_request(leg, result);
    verdict
}

fn refusal_reason(refusal: &Refusal) -> Reason {
    match refusal.dead_token {
        Some(DeadToken::Gone(_)) => Reason::DeviceTokenGone,
        Some(DeadToken::Invalid(_)) => Reason::DeviceTokenInvalid,
        None if refusal.is_transient() => Reason::ProviderUnavailable,
        None => Reason::BadRequest,
    }
}

fn device_token_is_shaped(leg: RelayLeg, device_token: &str) -> bool {
    match leg {
        RelayLeg::Apns | RelayLeg::ApnsVoip => {
            (MIN_APNS_DEVICE_TOKEN_LEN..=MAX_APNS_DEVICE_TOKEN_LEN).contains(&device_token.len())
                && device_token.len().is_multiple_of(2)
                && device_token.bytes().all(|byte| byte.is_ascii_hexdigit())
        }
        RelayLeg::Fcm => {
            !device_token.is_empty()
                && device_token.len() <= MAX_FCM_DEVICE_TOKEN_LEN
                && device_token.bytes().all(is_fcm_token_byte)
        }
    }
}

fn is_fcm_token_byte(byte: u8) -> bool {
    byte.is_ascii_alphanumeric() || matches!(byte, b':' | b'-' | b'_' | b'.')
}

fn is_aes128gcm(headers: &HeaderMap) -> bool {
    header(headers, header::CONTENT_ENCODING.as_str())
        .is_some_and(|value| value.eq_ignore_ascii_case(AES128GCM))
}

fn ttl_seconds(headers: &HeaderMap) -> Result<i64, Rejection> {
    let raw = header(headers, TTL_HEADER).ok_or(Rejection::new(Reason::BadRequest))?;
    let parsed = raw
        .parse::<i64>()
        .map_err(|_| Rejection::new(Reason::BadRequest))?;
    Ok(parsed.clamp(0, MAX_TTL_SECONDS))
}

fn header<'a>(headers: &'a HeaderMap, name: &str) -> Option<&'a str> {
    headers
        .get(name)
        .and_then(|value| value.to_str().ok())
        .map(str::trim)
        .filter(|value| !value.is_empty())
}

fn respond(status: StatusCode, rejection: Option<Rejection>) -> Response {
    let mut response = match rejection {
        None => status.into_response(),
        Some(rejection) => (
            status,
            [(header::CONTENT_TYPE, JSON_CONTENT_TYPE)],
            rejection.body(),
        )
            .into_response(),
    };
    if let Some(seconds) = rejection.and_then(|rejection| rejection.retry_after)
        && let Ok(value) = HeaderValue::from_str(&seconds.to_string())
    {
        response.headers_mut().insert(header::RETRY_AFTER, value);
    }
    response
}

fn digest(value: &str) -> String {
    if value.is_empty() {
        return "-".to_owned();
    }
    let mut hasher = Sha256::new();
    hasher.update(*LOG_SALT);
    hasher.update(value.as_bytes());
    BASE64_URL_SAFE_NO_PAD.encode(&hasher.finalize()[..DIGEST_BYTES])
}

#[cfg(test)]
mod tests {
    use super::*;

    fn hex(len: usize) -> String {
        "a".repeat(len)
    }

    #[test]
    fn apns_accepts_every_token_length_apple_hands_out() {
        for len in [64, 128, 160, 200, 256] {
            assert!(
                device_token_is_shaped(RelayLeg::Apns, &hex(len)),
                "{len} hex characters must be accepted"
            );
        }
        assert!(device_token_is_shaped(RelayLeg::Apns, &"A".repeat(64)));
        assert!(device_token_is_shaped(RelayLeg::ApnsVoip, &hex(160)));
    }

    #[test]
    fn apns_rejects_tokens_that_are_not_even_length_hex() {
        for token in [hex(62), hex(63), hex(161), hex(258), "z".repeat(64)] {
            assert!(
                !device_token_is_shaped(RelayLeg::Apns, &token),
                "{token} must be rejected"
            );
        }
    }

    #[test]
    fn the_log_digest_is_not_a_bare_hash_of_the_token() {
        const TOKEN: &str = "3dbc5a5ef1a1c1666afc26f466e1b3ebaaf4c66d92dddeb0fd1b69c49641d4cd";
        let unsalted =
            BASE64_URL_SAFE_NO_PAD.encode(&Sha256::digest(TOKEN.as_bytes())[..DIGEST_BYTES]);
        assert_ne!(digest(TOKEN), unsalted);
        assert_eq!(digest(TOKEN), digest(TOKEN));
        assert_eq!(digest(""), "-");
    }

    #[test]
    fn a_clear_survives_a_device_that_is_offline_for_a_day() {
        assert_eq!(
            Urgency::Background.ttl_cap_seconds(),
            Urgency::Alert.ttl_cap_seconds(),
            "a clear must outlive the alert it removes"
        );
    }

    #[test]
    fn a_clear_stays_silent_on_the_apns_leg() {
        let body = envelope::apns_body("payload", Urgency::Background).expect("body fits");
        let parsed: serde_json::Value = serde_json::from_slice(&body).expect("body is json");
        assert_eq!(parsed["aps"]["content-available"], 1);
        assert!(parsed["aps"].get("alert").is_none());
        let headers = envelope::apns_headers(Urgency::Background, 0, 86_400);
        let push_type = headers
            .iter()
            .find(|(name, _)| name == "apns-push-type")
            .map(|(_, value)| value.as_str());
        assert_eq!(push_type, Some("background"));
    }

    #[test]
    fn payload_too_large_answers_413() {
        assert_eq!(
            Reason::PayloadTooLarge.status(),
            StatusCode::PAYLOAD_TOO_LARGE
        );
    }
}
