// SPDX-License-Identifier: AGPL-3.0-or-later

use super::reject::{Reason, Rejection};
use base64::prelude::*;
use serde_json::{Value, json};

pub const FORMAT_VERSION: u64 = 1;
pub const ALERT_TTL_CAP_SECONDS: i64 = 86_400;
pub const BACKGROUND_TTL_CAP_SECONDS: i64 = 86_400;
pub const APNS_BODY_MAX_BYTES: usize = 4_096;
pub const FCM_DATA_MAX_BYTES: usize = 4_096;

const ALERT_LOC_KEY: &str = "PUSH_NEW_MESSAGE";
const APNS_PUSH_TYPE_HEADER: &str = "apns-push-type";
const APNS_PRIORITY_HEADER: &str = "apns-priority";
const APNS_EXPIRATION_HEADER: &str = "apns-expiration";
const CONTENT_TYPE_HEADER: &str = "content-type";
const JSON_CONTENT_TYPE: &str = "application/json";
const VOIP_PUSH_TYPE: &str = "voip";
const VOIP_PRIORITY: &str = "10";
const VOIP_EXPIRATION: &str = "0";

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Urgency {
    Alert,
    Background,
}

impl Urgency {
    pub fn from_header(raw: Option<&str>) -> Option<Self> {
        match raw.map(str::trim) {
            None => Some(Self::Alert),
            Some(value) => match value.to_ascii_lowercase().as_str() {
                "high" | "normal" => Some(Self::Alert),
                "low" | "very-low" => Some(Self::Background),
                _ => None,
            },
        }
    }

    pub fn ttl_cap_seconds(self) -> i64 {
        match self {
            Self::Alert => ALERT_TTL_CAP_SECONDS,
            Self::Background => BACKGROUND_TTL_CAP_SECONDS,
        }
    }
}

pub fn encode_payload(body: &[u8]) -> String {
    BASE64_URL_SAFE_NO_PAD.encode(body)
}

pub fn apns_body(payload: &str, urgency: Urgency) -> Result<Vec<u8>, Rejection> {
    let aps = match urgency {
        Urgency::Alert => json!({
            "alert": {"loc-key": ALERT_LOC_KEY, "loc-args": []},
            "mutable-content": 1,
            "interruption-level": "active",
        }),
        Urgency::Background => json!({"content-available": 1}),
    };
    let body = serialize(&json!({"aps": aps, "v": FORMAT_VERSION, "p": payload}));
    if body.len() > APNS_BODY_MAX_BYTES {
        return Err(Rejection::new(Reason::PayloadTooLarge));
    }
    Ok(body)
}

pub fn apns_voip_body(payload: &str) -> Result<Vec<u8>, Rejection> {
    let body = serialize(&json!({"aps": {}, "v": FORMAT_VERSION, "p": payload}));
    if body.len() > APNS_BODY_MAX_BYTES {
        return Err(Rejection::new(Reason::PayloadTooLarge));
    }
    Ok(body)
}

pub fn apns_voip_headers() -> Vec<(String, String)> {
    vec![
        (APNS_PUSH_TYPE_HEADER.to_owned(), VOIP_PUSH_TYPE.to_owned()),
        (APNS_PRIORITY_HEADER.to_owned(), VOIP_PRIORITY.to_owned()),
        (
            APNS_EXPIRATION_HEADER.to_owned(),
            VOIP_EXPIRATION.to_owned(),
        ),
        (CONTENT_TYPE_HEADER.to_owned(), JSON_CONTENT_TYPE.to_owned()),
    ]
}

pub fn apns_headers(urgency: Urgency, now_unix: i64, ttl_seconds: i64) -> Vec<(String, String)> {
    let (push_type, priority) = match urgency {
        Urgency::Alert => ("alert", "10"),
        Urgency::Background => ("background", "5"),
    };
    vec![
        (APNS_PUSH_TYPE_HEADER.to_owned(), push_type.to_owned()),
        (APNS_PRIORITY_HEADER.to_owned(), priority.to_owned()),
        (
            APNS_EXPIRATION_HEADER.to_owned(),
            apns_expiration(now_unix, ttl_seconds, urgency).to_string(),
        ),
        (CONTENT_TYPE_HEADER.to_owned(), JSON_CONTENT_TYPE.to_owned()),
    ]
}

pub fn fcm_body(
    device_token: &str,
    payload: &str,
    urgency: Urgency,
    ttl_seconds: i64,
) -> Result<Vec<u8>, Rejection> {
    let data = json!({"v": FORMAT_VERSION.to_string(), "p": payload});
    if serialize(&data).len() > FCM_DATA_MAX_BYTES {
        return Err(Rejection::new(Reason::PayloadTooLarge));
    }
    let priority = match urgency {
        Urgency::Alert => "HIGH",
        Urgency::Background => "NORMAL",
    };
    Ok(serialize(&json!({
        "message": {
            "token": device_token,
            "data": data,
            "android": {
                "priority": priority,
                "ttl": format!("{}s", capped_ttl(ttl_seconds, urgency)),
            },
        },
    })))
}

fn apns_expiration(now_unix: i64, ttl_seconds: i64, urgency: Urgency) -> i64 {
    let ttl = capped_ttl(ttl_seconds, urgency);
    if ttl == 0 { 0 } else { now_unix + ttl }
}

fn capped_ttl(ttl_seconds: i64, urgency: Urgency) -> i64 {
    ttl_seconds.clamp(0, urgency.ttl_cap_seconds())
}

fn serialize(value: &Value) -> Vec<u8> {
    serde_json::to_vec(value).expect("a json value serialises")
}
