// SPDX-License-Identifier: AGPL-3.0-or-later

use std::net::SocketAddr;

use axum::extract::State;
use axum::http::{HeaderMap, StatusCode, header};
use base64::prelude::*;
use hmac::{Hmac, KeyInit, Mac};
use serde::Deserialize;
use serde_json::Value;
use sha2::{Digest, Sha256};

use crate::clock::SharedClock;
use crate::ids::RoomKey;
use crate::ids::{ApiSecret, Location, Millis};
use crate::names::{ParticipantIdentity, parse_participant_identity, parse_room_name};
use crate::runtime::{RuntimeState, Shared};
use crate::suspicion::{Raised, SuspicionSource};

type HmacSha256 = Hmac<Sha256>;

pub const WEBHOOK_PATH: &str = "/livekit/webhook";
pub const MAX_WEBHOOK_BODY_BYTES: usize = 64 * 1024;

const HEADER_ALG: &str = "HS256";

#[derive(Clone, Copy, Debug, PartialEq, Eq, thiserror::Error)]
pub enum WebhookError {
    #[error("the webhook token is not three dot separated parts")]
    Malformed,
    #[error("the webhook token is not signed with HS256")]
    UnsupportedHeader,
    #[error("a webhook token part is not base64url")]
    BadBase64,
    #[error("the webhook token does not decode as json")]
    BadJson,
    #[error("the webhook token signature does not verify")]
    BadSignature,
    #[error("the webhook token names an api key this fleet does not carry")]
    UnknownIssuer,
    #[error("the webhook token carries no body digest")]
    MissingDigest,
    #[error("the webhook body does not match the digest the token signed")]
    DigestMismatch,
    #[error("the webhook token has expired")]
    Expired,
    #[error("the webhook body does not decode as json")]
    BadBody,
}

impl WebhookError {
    pub const fn label(self) -> &'static str {
        match self {
            Self::Malformed => "malformed",
            Self::UnsupportedHeader => "unsupported_header",
            Self::BadBase64 => "bad_base64",
            Self::BadJson => "bad_json",
            Self::BadSignature => "bad_signature",
            Self::UnknownIssuer => "unknown_issuer",
            Self::MissingDigest => "missing_digest",
            Self::DigestMismatch => "digest_mismatch",
            Self::Expired => "expired",
            Self::BadBody => "bad_body",
        }
    }

    pub const fn is_authentication_failure(self) -> bool {
        match self {
            Self::Malformed
            | Self::UnsupportedHeader
            | Self::BadBase64
            | Self::BadJson
            | Self::BadSignature
            | Self::UnknownIssuer
            | Self::MissingDigest
            | Self::DigestMismatch
            | Self::Expired => true,
            Self::BadBody => false,
        }
    }
}

#[derive(Clone, Debug, Deserialize, PartialEq, Eq)]
struct TokenHeader {
    alg: String,
}

#[derive(Clone, Debug, Default, Deserialize, PartialEq, Eq)]
pub struct WebhookClaims {
    #[serde(default)]
    pub iss: String,
    #[serde(default)]
    pub exp: u64,
    #[serde(default)]
    pub sha256: Option<String>,
}

fn decode_part(part: &str) -> Result<Vec<u8>, WebhookError> {
    BASE64_URL_SAFE_NO_PAD
        .decode(part.as_bytes())
        .or_else(|_| BASE64_STANDARD.decode(part.as_bytes()))
        .map_err(|_| WebhookError::BadBase64)
}

fn decode_digest(value: &str) -> Option<Vec<u8>> {
    BASE64_STANDARD
        .decode(value.as_bytes())
        .or_else(|_| BASE64_URL_SAFE_NO_PAD.decode(value.as_bytes()))
        .ok()
}

pub fn body_digest(body: &[u8]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(body);
    BASE64_STANDARD.encode(hasher.finalize())
}

pub fn verify_webhook_token(
    token: &str,
    api_key: &str,
    api_secret: &ApiSecret,
    body: &[u8],
    now_unix_seconds: u64,
) -> Result<WebhookClaims, WebhookError> {
    let token = token.trim();
    let token = token.strip_prefix("Bearer ").unwrap_or(token).trim();

    let mut parts = token.split('.');
    let (Some(header_b64), Some(payload_b64), Some(signature_b64), None) =
        (parts.next(), parts.next(), parts.next(), parts.next())
    else {
        return Err(WebhookError::Malformed);
    };

    let header_bytes = decode_part(header_b64)?;
    let header: TokenHeader =
        serde_json::from_slice(&header_bytes).map_err(|_| WebhookError::BadJson)?;
    if header.alg != HEADER_ALG {
        return Err(WebhookError::UnsupportedHeader);
    }

    let payload_bytes = decode_part(payload_b64)?;
    let claims: WebhookClaims =
        serde_json::from_slice(&payload_bytes).map_err(|_| WebhookError::BadJson)?;
    if claims.iss != api_key {
        return Err(WebhookError::UnknownIssuer);
    }

    let signature = decode_part(signature_b64)?;
    let mut mac = HmacSha256::new_from_slice(api_secret.expose().as_bytes())
        .expect("hmac accepts any key length");
    mac.update(header_b64.as_bytes());
    mac.update(b".");
    mac.update(payload_b64.as_bytes());
    mac.verify_slice(&signature)
        .map_err(|_| WebhookError::BadSignature)?;

    let Some(claimed) = claims.sha256.as_deref() else {
        return Err(WebhookError::MissingDigest);
    };
    let (Some(claimed), Some(actual)) = (decode_digest(claimed), decode_digest(&body_digest(body)))
    else {
        return Err(WebhookError::DigestMismatch);
    };
    if claimed != actual {
        return Err(WebhookError::DigestMismatch);
    }

    if claims.exp > 0 && now_unix_seconds >= claims.exp {
        return Err(WebhookError::Expired);
    }

    Ok(claims)
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum WebhookEventKind {
    RoomStarted,
    RoomFinished,
    ParticipantJoined,
    ParticipantLeft,
    TrackPublished,
    TrackUnpublished,
    Unrecognised,
}

impl WebhookEventKind {
    pub const ALL: [Self; 7] = [
        Self::RoomStarted,
        Self::RoomFinished,
        Self::ParticipantJoined,
        Self::ParticipantLeft,
        Self::TrackPublished,
        Self::TrackUnpublished,
        Self::Unrecognised,
    ];

    pub const fn wire(self) -> &'static str {
        match self {
            Self::RoomStarted => "room_started",
            Self::RoomFinished => "room_finished",
            Self::ParticipantJoined => "participant_joined",
            Self::ParticipantLeft => "participant_left",
            Self::TrackPublished => "track_published",
            Self::TrackUnpublished => "track_unpublished",
            Self::Unrecognised => "unrecognised",
        }
    }

    pub fn from_wire(value: &str) -> Self {
        match value {
            "room_started" => Self::RoomStarted,
            "room_finished" => Self::RoomFinished,
            "participant_joined" => Self::ParticipantJoined,
            "participant_left" => Self::ParticipantLeft,
            "track_published" => Self::TrackPublished,
            "track_unpublished" => Self::TrackUnpublished,
            _ => Self::Unrecognised,
        }
    }

    pub const fn changes_the_roster(self) -> bool {
        match self {
            Self::RoomFinished
            | Self::ParticipantJoined
            | Self::ParticipantLeft
            | Self::TrackPublished
            | Self::TrackUnpublished => true,
            Self::RoomStarted | Self::Unrecognised => false,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct WebhookEvent {
    pub kind: WebhookEventKind,
    pub room: Option<RoomKey>,
    pub identity: Option<ParticipantIdentity>,
    pub num_dropped: u32,
    pub created_at_unix_seconds: u64,
    pub unparseable_room: bool,
    pub unparseable_identity: bool,
}

fn text_of(value: &Value) -> Option<&str> {
    value.as_str()
}

fn number_of(value: &Value) -> Option<u64> {
    match value {
        Value::Number(number) => number.as_u64(),
        Value::String(raw) => raw.parse::<u64>().ok(),
        Value::Null | Value::Bool(_) | Value::Array(_) | Value::Object(_) => None,
    }
}

pub fn decode_event(body: &[u8]) -> Result<WebhookEvent, WebhookError> {
    let value: Value = serde_json::from_slice(body).map_err(|_| WebhookError::BadBody)?;
    if !value.is_object() {
        return Err(WebhookError::BadBody);
    }

    let kind = value
        .get("event")
        .and_then(text_of)
        .map_or(WebhookEventKind::Unrecognised, WebhookEventKind::from_wire);

    let named_room = value.get("room").and_then(|room| room.get("name"));
    let room = named_room.and_then(text_of).map(parse_room_name);
    let named_identity = value
        .get("participant")
        .and_then(|participant| participant.get("identity"));
    let identity = named_identity
        .and_then(text_of)
        .map(parse_participant_identity);

    Ok(WebhookEvent {
        kind,
        room: room
            .as_ref()
            .and_then(|parsed| parsed.as_ref().ok())
            .copied(),
        identity: identity
            .as_ref()
            .and_then(|parsed| parsed.as_ref().ok())
            .cloned(),
        num_dropped: value
            .get("numDropped")
            .or_else(|| value.get("num_dropped"))
            .and_then(number_of)
            .and_then(|dropped| u32::try_from(dropped).ok())
            .unwrap_or(0),
        created_at_unix_seconds: value
            .get("createdAt")
            .or_else(|| value.get("created_at"))
            .and_then(number_of)
            .unwrap_or(0),
        unparseable_room: named_room.is_some() && room.is_none_or(|parsed| parsed.is_err()),
        unparseable_identity: named_identity.is_some()
            && identity.is_none_or(|parsed| parsed.is_err()),
    })
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Ingested {
    pub raised: Option<Raised>,
    pub holed: bool,
}

impl Ingested {
    pub const fn nothing() -> Self {
        Self {
            raised: None,
            holed: false,
        }
    }
}

pub fn ingest(
    state: &mut RuntimeState,
    event: &WebhookEvent,
    location: &Location,
    at: Millis,
) -> Ingested {
    let holed = state
        .suspicion
        .note_dropped(location, event.num_dropped, at);
    let source = if holed {
        SuspicionSource::WebhookDrop
    } else {
        SuspicionSource::Webhook
    };

    let raised = match event.room {
        None => None,
        Some(room) => {
            if !holed && !event.kind.changes_the_roster() {
                None
            } else {
                state.directory.note_suspicion(room, at);
                Some(state.suspicion.note_room(room, source, at))
            }
        }
    };

    Ingested { raised, holed }
}

#[derive(Clone)]
pub struct WebhookLane {
    shared: Shared,
    clock: SharedClock,
}

impl WebhookLane {
    pub const fn new(shared: Shared, clock: SharedClock) -> Self {
        Self { shared, clock }
    }
}

impl std::fmt::Debug for WebhookLane {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.debug_struct("WebhookLane").finish()
    }
}

fn credentials(shared: &Shared) -> Vec<(Location, Box<str>, ApiSecret)> {
    shared.with_state(|state| {
        state
            .topology
            .servers()
            .iter()
            .map(|server| {
                (
                    server.location().clone(),
                    Box::from(server.api_key()),
                    server.api_secret().clone(),
                )
            })
            .collect()
    })
}

pub fn authenticate(
    shared: &Shared,
    token: &str,
    body: &[u8],
    now_unix_seconds: u64,
) -> Result<Location, WebhookError> {
    let mut failure = WebhookError::UnknownIssuer;
    for (location, api_key, api_secret) in credentials(shared) {
        match verify_webhook_token(token, &api_key, &api_secret, body, now_unix_seconds) {
            Ok(_) => return Ok(location),
            Err(WebhookError::UnknownIssuer) => {}
            Err(error) => failure = error,
        }
    }
    Err(failure)
}

pub fn accept(
    shared: &Shared,
    clock: &SharedClock,
    token: &str,
    body: &[u8],
) -> Result<Ingested, WebhookError> {
    let wall_now = clock.wall_now().get() / 1_000;
    let location = authenticate(shared, token, body, wall_now)?;
    let event = decode_event(body)?;
    let at = clock.now();

    let ingested = shared.with_state_mut(|state| ingest(state, &event, &location, at));

    let age_seconds = if event.created_at_unix_seconds == 0 {
        0.0
    } else {
        wall_now.saturating_sub(event.created_at_unix_seconds) as f64
    };
    shared
        .metrics()
        .set_webhook_last_event_age(location.server.as_str(), age_seconds);

    Ok(ingested)
}

async fn receive(
    State(lane): State<WebhookLane>,
    headers: HeaderMap,
    body: axum::body::Bytes,
) -> StatusCode {
    if body.len() > MAX_WEBHOOK_BODY_BYTES {
        return StatusCode::PAYLOAD_TOO_LARGE;
    }
    let token = headers
        .get(header::AUTHORIZATION)
        .and_then(|value| value.to_str().ok())
        .unwrap_or_default();

    match accept(&lane.shared, &lane.clock, token, &body) {
        Ok(ingested) => {
            tracing::debug!(
                raised = ?ingested.raised,
                holed = ingested.holed,
                "a livekit webhook raised suspicion, which only reorders reads"
            );
            StatusCode::NO_CONTENT
        }
        Err(error) => {
            tracing::warn!(
                error = error.label(),
                "a livekit webhook was refused, so detection stays on the polling path"
            );
            if error.is_authentication_failure() {
                StatusCode::UNAUTHORIZED
            } else {
                StatusCode::BAD_REQUEST
            }
        }
    }
}

pub fn router(shared: Shared, clock: SharedClock) -> axum::Router {
    axum::Router::new()
        .route(WEBHOOK_PATH, axum::routing::post(receive))
        .with_state(WebhookLane::new(shared, clock))
}

pub async fn run_webhook(
    shared: Shared,
    clock: SharedClock,
    address: SocketAddr,
) -> anyhow::Result<()> {
    let listener = tokio::net::TcpListener::bind(address).await?;
    tracing::info!(
        addr = %address,
        path = WEBHOOK_PATH,
        "webhook listener accepting livekit events, which are suspicion only"
    );
    axum::serve(listener, router(shared, clock)).await?;
    Ok(())
}
