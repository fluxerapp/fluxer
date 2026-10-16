// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::config::{ApnsConfig, FcmConfig, VapidConfig};
use crate::crypto::{self, CryptoError, ES256};
use crate::metrics::{AuthProvider, Metrics};
use crate::unix_seconds;
use serde_json::{Value, json};
use std::collections::HashMap;
use thiserror::Error;
use tokio::sync::Mutex;

const VAPID_TOKEN_TTL_SECONDS: i64 = 43_200;
const VAPID_TOKEN_SKEW_SECONDS: i64 = 60;
const MAX_VAPID_AUDIENCES: usize = 10_000;
const APNS_TOKEN_TTL_SECONDS: i64 = 50 * 60;
const FCM_ASSERTION_TTL_SECONDS: i64 = 3_600;
const FCM_TOKEN_SKEW_SECONDS: i64 = 60;
const FCM_DEFAULT_EXPIRES_IN_SECONDS: i64 = 3_600;
const FCM_SCOPE: &str = "https://www.googleapis.com/auth/firebase.messaging";
const FCM_GRANT_TYPE: &str = "urn%3Aietf%3Aparams%3Aoauth%3Agrant-type%3Ajwt-bearer";
const FORM_CONTENT_TYPE: &str = "application/x-www-form-urlencoded";

#[derive(Debug, Error)]
pub enum TokenError {
    #[error(transparent)]
    Crypto(#[from] CryptoError),
    #[error("the token request failed")]
    Request(#[from] reqwest::Error),
    #[error("the token endpoint returned status {0}")]
    Status(u16),
    #[error("the token response has no access_token")]
    Malformed,
}

struct Cached {
    token: String,
    expires_at: i64,
}

#[derive(Default)]
pub struct TokenCache {
    vapid: Mutex<HashMap<String, Cached>>,
    apns: Mutex<Option<Cached>>,
    fcm: Mutex<Option<Cached>>,
}

impl TokenCache {
    pub fn new() -> Self {
        Self::default()
    }

    pub async fn vapid(
        &self,
        audience: &str,
        cfg: &VapidConfig,
        metrics: &Metrics,
    ) -> Result<String, TokenError> {
        let now = unix_seconds();
        let mut cache = self.vapid.lock().await;
        if let Some(cached) = cache.get(audience)
            && cached.expires_at - VAPID_TOKEN_SKEW_SECONDS > now
        {
            return Ok(cached.token.clone());
        }

        let expires_at = now + VAPID_TOKEN_TTL_SECONDS;
        let key = crypto::parse_p256_private_scalar(cfg.private_key.expose())?;
        let token = crypto::es256_jwt(
            &json!({"alg": ES256, "typ": "JWT"}),
            &json!({
                "sub": format!("mailto:{}", cfg.email),
                "aud": audience,
                "exp": expires_at,
            }),
            &key,
        )?;
        metrics.record_auth_token_minted(AuthProvider::Vapid);
        if cache.len() >= MAX_VAPID_AUDIENCES {
            cache.retain(|_, cached| cached.expires_at - VAPID_TOKEN_SKEW_SECONDS > now);
        }
        if cache.len() >= MAX_VAPID_AUDIENCES {
            cache.clear();
        }
        cache.insert(
            audience.to_owned(),
            Cached {
                token: token.clone(),
                expires_at,
            },
        );
        Ok(token)
    }

    pub async fn apns(&self, cfg: &ApnsConfig, metrics: &Metrics) -> Result<String, TokenError> {
        let now = unix_seconds();
        let mut cache = self.apns.lock().await;
        if let Some(cached) = cache.as_ref()
            && cached.expires_at > now
        {
            return Ok(cached.token.clone());
        }

        let key = crypto::parse_p256_pkcs8_pem(cfg.private_key.expose())?;
        let token = crypto::es256_jwt(
            &json!({"alg": ES256, "kid": cfg.key_id}),
            &json!({"iss": cfg.team_id, "iat": now}),
            &key,
        )?;
        metrics.record_auth_token_minted(AuthProvider::Apns);
        *cache = Some(Cached {
            token: token.clone(),
            expires_at: now + APNS_TOKEN_TTL_SECONDS,
        });
        Ok(token)
    }

    pub async fn fcm(
        &self,
        cfg: &FcmConfig,
        http: &reqwest::Client,
        metrics: &Metrics,
    ) -> Result<String, TokenError> {
        let now = unix_seconds();
        let mut cache = self.fcm.lock().await;
        if let Some(cached) = cache.as_ref()
            && cached.expires_at - FCM_TOKEN_SKEW_SECONDS > now
        {
            return Ok(cached.token.clone());
        }

        let key = crypto::parse_rsa_pkcs8_pem(cfg.private_key.expose())?;
        let assertion = crypto::rs256_jwt(
            &json!({"alg": "RS256", "typ": "JWT"}),
            &json!({
                "iss": cfg.client_email,
                "scope": FCM_SCOPE,
                "aud": cfg.token_uri,
                "iat": now,
                "exp": now + FCM_ASSERTION_TTL_SECONDS,
            }),
            &key,
        )?;

        let response = http
            .post(&cfg.token_uri)
            .header(reqwest::header::CONTENT_TYPE, FORM_CONTENT_TYPE)
            .body(format!("grant_type={FCM_GRANT_TYPE}&assertion={assertion}"))
            .send()
            .await?;
        let status = response.status();
        if !status.is_success() {
            return Err(TokenError::Status(status.as_u16()));
        }
        let body: Value =
            serde_json::from_slice(&response.bytes().await?).map_err(|_| TokenError::Malformed)?;
        let token = body
            .get("access_token")
            .and_then(Value::as_str)
            .ok_or(TokenError::Malformed)?
            .to_owned();

        metrics.record_auth_token_minted(AuthProvider::Fcm);
        *cache = Some(Cached {
            token: token.clone(),
            expires_at: now + normalize_expires_in(body.get("expires_in")),
        });
        Ok(token)
    }
}

fn normalize_expires_in(value: Option<&Value>) -> i64 {
    let parsed = match value {
        Some(Value::Number(number)) => number.as_i64(),
        Some(Value::String(text)) => text.trim().parse::<i64>().ok(),
        _ => None,
    };
    parsed
        .filter(|seconds| *seconds > 0)
        .unwrap_or(FCM_DEFAULT_EXPIRES_IN_SECONDS)
}
