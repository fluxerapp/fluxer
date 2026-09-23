// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::config::{ApnsConfig, FcmConfig, ProviderEnvironment};
use crate::metrics::Metrics;
use crate::resolver::PublicOnlyResolver;
use crate::tokens::{TokenCache, TokenError};
use reqwest::header::{AUTHORIZATION, CONTENT_TYPE};
use reqwest::redirect::Policy;
use serde_json::Value;
use std::time::Duration;
use tracing::warn;

const HTTP_TIMEOUT: Duration = Duration::from_secs(10);
const APNS_TIMEOUT: Duration = Duration::from_secs(5);
const CONNECT_TIMEOUT: Duration = Duration::from_secs(5);
const APNS_TOPIC_HEADER: &str = "apns-topic";
pub const FCM_CONTENT_TYPE: &str = "application/json; charset=UTF-8";
const TOO_MANY_REQUESTS: u16 = 429;
const DNS_ERROR_MARKER: &str = "dns error";
const MAX_ERROR_BODY_BYTES: usize = 8_192;
const HTTP_ERROR: &str = "http_error";
const UNREGISTERED: &str = "UNREGISTERED";
const INVALID_ARGUMENT: &str = "INVALID_ARGUMENT";

pub fn http_client() -> reqwest::Result<reqwest::Client> {
    reqwest::Client::builder()
        .redirect(Policy::none())
        .connect_timeout(CONNECT_TIMEOUT)
        .timeout(HTTP_TIMEOUT)
        .build()
}

pub fn web_push_http_client() -> reqwest::Result<reqwest::Client> {
    reqwest::Client::builder()
        .redirect(Policy::none())
        .dns_resolver(PublicOnlyResolver)
        .connect_timeout(CONNECT_TIMEOUT)
        .timeout(HTTP_TIMEOUT)
        .build()
}

pub fn apns_http_client() -> reqwest::Result<reqwest::Client> {
    reqwest::Client::builder()
        .redirect(Policy::none())
        .http2_prior_knowledge()
        .connect_timeout(CONNECT_TIMEOUT)
        .timeout(APNS_TIMEOUT)
        .build()
}

pub struct ApnsRequest<'a> {
    pub environment: ProviderEnvironment,
    pub topic: &'a str,
    pub device_token: &'a str,
    pub headers: &'a [(String, String)],
    pub body: Vec<u8>,
}

#[derive(Debug, Eq, PartialEq)]
pub enum VendorOutcome {
    Accepted,
    Refused(Refusal),
    Unreachable(Unreachable),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Unreachable {
    Dns,
    Transport,
}

impl Unreachable {
    pub fn label(self) -> &'static str {
        match self {
            Self::Dns => "dns",
            Self::Transport => "transport",
        }
    }

    pub fn is_permanent(self) -> bool {
        matches!(self, Self::Dns)
    }

    pub fn of(error: &reqwest::Error) -> Self {
        if names_no_host(error) {
            Self::Dns
        } else {
            Self::Transport
        }
    }
}

fn names_no_host(error: &reqwest::Error) -> bool {
    let mut current = std::error::Error::source(error);
    while let Some(error) = current {
        if error.to_string().contains(DNS_ERROR_MARKER) {
            return true;
        }
        current = error.source();
    }
    false
}

#[derive(Debug, Eq, PartialEq)]
pub struct Refusal {
    pub status: u16,
    pub reason: String,
    pub dead_token: Option<DeadToken>,
}

impl Refusal {
    pub fn is_transient(&self) -> bool {
        is_transient_status(self.status)
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum DeadToken {
    Gone(&'static str),
    Invalid(&'static str),
}

impl DeadToken {
    pub fn label(self) -> &'static str {
        match self {
            Self::Gone(label) | Self::Invalid(label) => label,
        }
    }
}

pub async fn send_apns(
    http: &reqwest::Client,
    tokens: &TokenCache,
    metrics: &Metrics,
    cfg: &ApnsConfig,
    request: ApnsRequest<'_>,
) -> Result<VendorOutcome, TokenError> {
    let token = tokens.apns(cfg, metrics).await?;
    let url = format!(
        "{}/3/device/{}",
        cfg.base_url(request.environment).trim_end_matches('/'),
        request.device_token
    );
    let mut builder = http
        .post(&url)
        .header(AUTHORIZATION, format!("bearer {token}"))
        .header(APNS_TOPIC_HEADER, request.topic)
        .body(request.body);
    for (name, value) in request.headers {
        builder = builder.header(name, value);
    }
    Ok(outcome(builder.send().await, apns_refusal).await)
}

pub async fn send_fcm(
    http: &reqwest::Client,
    tokens: &TokenCache,
    metrics: &Metrics,
    cfg: &FcmConfig,
    project_id: &str,
    body: Vec<u8>,
) -> Result<VendorOutcome, TokenError> {
    let token = tokens.fcm(cfg, http, metrics).await?;
    let url = format!(
        "{}/v1/projects/{project_id}/messages:send",
        cfg.base_url.trim_end_matches('/')
    );
    let response = http
        .post(&url)
        .header(AUTHORIZATION, format!("Bearer {token}"))
        .header(CONTENT_TYPE, FCM_CONTENT_TYPE)
        .body(body)
        .send()
        .await;
    Ok(outcome(response, fcm_refusal).await)
}

async fn outcome(
    response: reqwest::Result<reqwest::Response>,
    refusal: fn(u16, &[u8]) -> Refusal,
) -> VendorOutcome {
    let response = match response {
        Ok(response) => response,
        Err(error) => {
            let unreachable = Unreachable::of(&error);
            warn!(
                error = %error,
                kind = unreachable.label(),
                "vendor request did not complete"
            );
            return VendorOutcome::Unreachable(unreachable);
        }
    };
    if response.status().is_success() {
        return VendorOutcome::Accepted;
    }
    let status = response.status().as_u16();
    VendorOutcome::Refused(refusal(status, &read_error_body(response).await))
}

fn apns_refusal(status: u16, body: &[u8]) -> Refusal {
    let reason = reason_field(body).unwrap_or_else(|| format!("http_{status}"));
    Refusal {
        status,
        dead_token: apns_dead_token(status, &reason),
        reason,
    }
}

fn fcm_refusal(status: u16, body: &[u8]) -> Refusal {
    let reason = fcm_error_code(body);
    Refusal {
        status,
        dead_token: fcm_dead_token(&reason),
        reason,
    }
}

fn apns_dead_token(status: u16, reason: &str) -> Option<DeadToken> {
    match (status, reason) {
        (_, "Unregistered") => Some(DeadToken::Gone("unregistered")),
        (410, _) => Some(DeadToken::Gone("gone")),
        (400, "BadDeviceToken") => Some(DeadToken::Invalid("bad_device_token")),
        (400, "DeviceTokenNotForTopic") => Some(DeadToken::Invalid("device_token_not_for_topic")),
        _ => None,
    }
}

fn fcm_dead_token(code: &str) -> Option<DeadToken> {
    match code {
        UNREGISTERED => Some(DeadToken::Gone("unregistered")),
        INVALID_ARGUMENT => Some(DeadToken::Invalid("invalid_argument")),
        _ => None,
    }
}

pub fn reason_field(body: &[u8]) -> Option<String> {
    let parsed: Value = serde_json::from_slice(body).ok()?;
    Some(parsed.get("reason")?.as_str()?.to_owned())
}

fn fcm_error_code(body: &[u8]) -> String {
    let Ok(parsed) = serde_json::from_slice::<Value>(body) else {
        return HTTP_ERROR.to_owned();
    };
    let Some(error) = parsed.get("error") else {
        return HTTP_ERROR.to_owned();
    };
    if let Some(details) = error.get("details").and_then(Value::as_array) {
        return details
            .iter()
            .find_map(|detail| detail.get("errorCode").and_then(Value::as_str))
            .unwrap_or(HTTP_ERROR)
            .to_owned();
    }
    error
        .get("status")
        .and_then(Value::as_str)
        .unwrap_or(HTTP_ERROR)
        .to_owned()
}

pub fn is_transient_status(status: u16) -> bool {
    status >= 500 || status == TOO_MANY_REQUESTS
}

pub async fn read_error_body(response: reqwest::Response) -> Vec<u8> {
    let mut body = response
        .bytes()
        .await
        .map(|bytes| bytes.to_vec())
        .unwrap_or_default();
    body.truncate(MAX_ERROR_BODY_BYTES);
    body
}

#[cfg(test)]
mod tests {
    use super::*;

    async fn error_for(url: &str) -> reqwest::Error {
        reqwest::Client::builder()
            .no_proxy()
            .connect_timeout(CONNECT_TIMEOUT)
            .build()
            .expect("the http client builds")
            .post(url)
            .send()
            .await
            .expect_err("the request cannot complete")
    }

    #[tokio::test]
    async fn a_host_that_does_not_resolve_is_permanent() {
        let error = error_for("https://push.invalid/relay/v1/apns/stable/production/token").await;
        assert_eq!(Unreachable::of(&error), Unreachable::Dns);
        assert!(Unreachable::of(&error).is_permanent());
    }

    #[tokio::test]
    async fn a_refused_connection_stays_retryable() {
        let error = error_for("http://127.0.0.1:1/").await;
        assert_eq!(Unreachable::of(&error), Unreachable::Transport);
        assert!(!Unreachable::of(&error).is_permanent());
    }
}
