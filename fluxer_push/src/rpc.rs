// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::config::{RPC_AUTH_HEADER, RpcConfig};
use crate::metrics::{Metrics, RpcMethod, RpcOutcome, elapsed_ms};
use crate::secret::SecretString;
use crate::subscription::Subscription;
use fluxer_svc::metrics::now_ms;
use rand::RngExt as _;
use serde::Deserialize;
use serde::de::{DeserializeOwned, IgnoredAny};
use serde_json::{Value, json};
use std::collections::HashMap;
use std::sync::Arc;
use std::time::Duration;
use thiserror::Error;
use tracing::warn;

const USER_BATCH_MAX: usize = 2_000;
const DELETION_BATCH_MAX: usize = 100;

const REQUEST_TIMEOUT: Duration = Duration::from_secs(5);
const MAX_ATTEMPTS: u32 = 3;
const BASE_BACKOFF_MS: u64 = 200;
const MAX_BACKOFF_MS: u64 = 2_000;
const JITTER_MS: u64 = 200;
const ERROR_MESSAGE_MAX: usize = 512;

#[derive(Debug, Error)]
pub enum RpcError {
    #[error("internal rpc transport failed: {0}")]
    Transport(#[from] reqwest::Error),
    #[error("internal rpc returned {code}: {message}")]
    Status { code: u16, message: String },
    #[error("internal rpc response could not be decoded: {0}")]
    Decode(#[from] serde_json::Error),
}

impl RpcError {
    pub(crate) fn is_retryable(&self) -> bool {
        match self {
            Self::Transport(_) => true,
            Self::Status { code, .. } => *code >= 500,
            Self::Decode(_) => false,
        }
    }
}

pub struct RpcClient {
    http: reqwest::Client,
    url: String,
    auth: SecretString,
    metrics: Arc<Metrics>,
}

impl RpcClient {
    pub fn new(cfg: &RpcConfig, http: reqwest::Client, metrics: Arc<Metrics>) -> Self {
        Self {
            http,
            url: cfg.url.clone(),
            auth: cfg.auth_token.clone(),
            metrics,
        }
    }

    pub async fn rollout_config(&self, method: RpcMethod) -> Result<Value, RpcError> {
        let data: RolloutConfigData = self.call(method, &json!({"type": method.label()})).await?;
        Ok(data.config)
    }

    pub async fn badge_counts(
        &self,
        user_ids: &[String],
    ) -> Result<HashMap<String, u32>, RpcError> {
        let mut counts = HashMap::new();
        for batch in user_ids.chunks(USER_BATCH_MAX) {
            let data: BadgeCountsData = self
                .call(
                    RpcMethod::GetBadgeCounts,
                    &json!({"type": "get_badge_counts", "user_ids": batch}),
                )
                .await?;
            counts.extend(data.badge_counts);
        }
        Ok(counts)
    }

    pub async fn push_subscriptions(
        &self,
        user_ids: &[String],
    ) -> Result<HashMap<String, Vec<Subscription>>, RpcError> {
        let mut subscriptions = HashMap::new();
        for batch in user_ids.chunks(USER_BATCH_MAX) {
            let data: HashMap<String, Vec<Subscription>> = self
                .call(
                    RpcMethod::GetPushSubscriptions,
                    &json!({"type": "get_push_subscriptions", "user_ids": batch}),
                )
                .await?;
            subscriptions.extend(data);
        }
        Ok(subscriptions)
    }

    pub async fn delete_push_subscriptions(
        &self,
        subscriptions: &[(String, String)],
    ) -> Result<(), RpcError> {
        for batch in subscriptions.chunks(DELETION_BATCH_MAX) {
            let entries = batch
                .iter()
                .map(|(user_id, subscription_id)| {
                    json!({"user_id": user_id, "subscription_id": subscription_id})
                })
                .collect::<Vec<Value>>();
            self.call::<IgnoredAny>(
                RpcMethod::DeletePushSubscriptions,
                &json!({"type": "delete_push_subscriptions", "subscriptions": entries}),
            )
            .await?;
        }
        Ok(())
    }

    async fn call<T: DeserializeOwned>(
        &self,
        method: RpcMethod,
        body: &Value,
    ) -> Result<T, RpcError> {
        let mut attempt = 1;
        loop {
            let error = match self.attempt(method, body).await {
                Ok(data) => return Ok(data),
                Err(error) => error,
            };
            if !error.is_retryable() || attempt >= MAX_ATTEMPTS {
                return Err(error);
            }
            let delay_ms = backoff_delay_ms(attempt);
            warn!(
                method = method.label(),
                attempt,
                max_attempts = MAX_ATTEMPTS,
                delay_ms,
                error = %error,
                "internal rpc retrying"
            );
            tokio::time::sleep(Duration::from_millis(delay_ms)).await;
            attempt += 1;
        }
    }

    async fn attempt<T: DeserializeOwned>(
        &self,
        method: RpcMethod,
        body: &Value,
    ) -> Result<T, RpcError> {
        let started_ms = now_ms();
        let result = self.request(body).await;
        let duration_ms = elapsed_ms(started_ms);
        let outcome = if result.is_ok() {
            RpcOutcome::Ok
        } else {
            RpcOutcome::Error
        };
        self.metrics.record_rpc(method, outcome, duration_ms);
        result
    }

    async fn request<T: DeserializeOwned>(&self, body: &Value) -> Result<T, RpcError> {
        let response = self
            .http
            .post(&self.url)
            .header(RPC_AUTH_HEADER, self.auth.expose())
            .timeout(REQUEST_TIMEOUT)
            .json(body)
            .send()
            .await?;
        let code = response.status().as_u16();
        let payload = response.bytes().await?;
        if !(200..300).contains(&code) {
            return Err(RpcError::Status {
                code,
                message: error_message(&payload),
            });
        }
        let mut envelope: HashMap<String, Value> = serde_json::from_slice(&payload)?;
        let data = envelope.remove("data").unwrap_or_else(|| json!({}));
        Ok(serde_json::from_value(data)?)
    }
}

#[derive(Deserialize)]
struct RolloutConfigData {
    config: Value,
}

#[derive(Deserialize)]
struct BadgeCountsData {
    #[serde(default)]
    badge_counts: HashMap<String, u32>,
}

fn backoff_delay_ms(attempt: u32) -> u64 {
    let exponential = BASE_BACKOFF_MS.saturating_mul(1u64 << (attempt - 1).min(16));
    exponential.min(MAX_BACKOFF_MS) + rand::rng().random_range(0..JITTER_MS)
}

fn error_message(payload: &[u8]) -> String {
    serde_json::from_slice::<Value>(payload)
        .ok()
        .and_then(|body| {
            body.get("message")
                .and_then(Value::as_str)
                .map(ToOwned::to_owned)
        })
        .unwrap_or_else(|| String::from_utf8_lossy(payload).into_owned())
        .chars()
        .take(ERROR_MESSAGE_MAX)
        .collect()
}
