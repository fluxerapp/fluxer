// SPDX-License-Identifier: AGPL-3.0-or-later

use super::ServiceAccount;
use base64::Engine;
use base64::engine::general_purpose::{STANDARD, URL_SAFE_NO_PAD};
use ring::rand::SystemRandom;
use ring::signature::{RSA_PKCS1_SHA256, RsaKeyPair};
use serde_json::{Value, json};
use std::sync::Mutex;
use std::time::Duration;
use tokio::sync::Semaphore;

const SCOPE: &str = "https://www.googleapis.com/auth/playintegrity";
const GRANT_TYPE: &str = "urn%3Aietf%3Aparams%3Aoauth%3Agrant-type%3Ajwt-bearer";
const DECODE_BASE_URL: &str = "https://playintegrity.googleapis.com/v1";
const ASSERTION_TTL_SECONDS: i64 = 3_600;
const TOKEN_SKEW_SECONDS: i64 = 60;
const DEFAULT_EXPIRES_IN_SECONDS: i64 = 3_600;
const EXCHANGE_TIMEOUT: Duration = Duration::from_secs(4);
const REQUEST_TIMEOUT: Duration = Duration::from_secs(3);
const CONNECT_TIMEOUT: Duration = Duration::from_secs(2);
const MAX_CONCURRENT_EXCHANGES: usize = 8;

#[derive(Debug)]
pub enum DecodeError {
    TokenRejected,
    Refused(u16),
    Unavailable(String),
}

struct CachedToken {
    token: String,
    expires_at: i64,
}

pub struct GoogleClient {
    http: reqwest::Client,
    account: ServiceAccount,
    key: RsaKeyPair,
    random: SystemRandom,
    token: Mutex<Option<CachedToken>>,
    exchanges: Semaphore,
}

impl GoogleClient {
    pub fn new(account: ServiceAccount) -> anyhow::Result<Self> {
        let key = parse_rsa_pkcs8_pem(&account.private_key)
            .ok_or_else(|| anyhow::anyhow!("play integrity service account key is not RSA"))?;
        let http = reqwest::Client::builder()
            .timeout(REQUEST_TIMEOUT)
            .connect_timeout(CONNECT_TIMEOUT)
            .build()?;
        Ok(Self {
            http,
            account,
            key,
            random: SystemRandom::new(),
            token: Mutex::new(None),
            exchanges: Semaphore::new(MAX_CONCURRENT_EXCHANGES),
        })
    }

    pub async fn decode_integrity_token(
        &self,
        package_name: &str,
        integrity_token: &str,
        now_seconds: i64,
    ) -> Result<Value, DecodeError> {
        let _permit = self.exchanges.try_acquire().map_err(|_| {
            DecodeError::Unavailable("too many google exchanges in flight".to_owned())
        })?;
        tokio::time::timeout(
            EXCHANGE_TIMEOUT,
            self.exchange(package_name, integrity_token, now_seconds),
        )
        .await
        .map_err(|_| DecodeError::Unavailable("google exchange timed out".to_owned()))?
    }

    async fn exchange(
        &self,
        package_name: &str,
        integrity_token: &str,
        now_seconds: i64,
    ) -> Result<Value, DecodeError> {
        let access_token = self.access_token(now_seconds).await?;
        let response = self
            .http
            .post(format!(
                "{DECODE_BASE_URL}/{package_name}:decodeIntegrityToken"
            ))
            .bearer_auth(access_token)
            .json(&json!({"integrityToken": integrity_token}))
            .send()
            .await
            .map_err(|error| DecodeError::Unavailable(format!("decode request failed: {error}")))?;
        let status = response.status();
        match status {
            reqwest::StatusCode::BAD_REQUEST => return Err(DecodeError::TokenRejected),
            reqwest::StatusCode::FORBIDDEN | reqwest::StatusCode::NOT_FOUND => {
                return Err(DecodeError::Refused(status.as_u16()));
            }
            reqwest::StatusCode::UNAUTHORIZED => self.clear_cached_token(),
            _ => {}
        }
        if !status.is_success() {
            return Err(DecodeError::Unavailable(format!(
                "decode returned status {}",
                status.as_u16()
            )));
        }
        let mut body: Value = response.json().await.map_err(|error| {
            DecodeError::Unavailable(format!("decode response unreadable: {error}"))
        })?;
        match body.get_mut("tokenPayloadExternal").map(Value::take) {
            Some(payload @ Value::Object(_)) => Ok(payload),
            _ => Err(DecodeError::Unavailable(
                "decode response has no tokenPayloadExternal".to_owned(),
            )),
        }
    }

    fn cached_token(&self, now_seconds: i64) -> Option<String> {
        let cache = self.token.lock().ok()?;
        cache
            .as_ref()
            .filter(|cached| cached.expires_at - TOKEN_SKEW_SECONDS > now_seconds)
            .map(|cached| cached.token.clone())
    }

    fn clear_cached_token(&self) {
        if let Ok(mut cache) = self.token.lock() {
            *cache = None;
        }
    }

    async fn access_token(&self, now_seconds: i64) -> Result<String, DecodeError> {
        if let Some(token) = self.cached_token(now_seconds) {
            return Ok(token);
        }
        let assertion = self
            .assertion(now_seconds)
            .ok_or_else(|| DecodeError::Unavailable("signing the assertion failed".to_owned()))?;
        let response = self
            .http
            .post(&self.account.token_uri)
            .header(
                reqwest::header::CONTENT_TYPE,
                "application/x-www-form-urlencoded",
            )
            .body(format!("grant_type={GRANT_TYPE}&assertion={assertion}"))
            .send()
            .await
            .map_err(|error| DecodeError::Unavailable(format!("token request failed: {error}")))?;
        let status = response.status();
        if !status.is_success() {
            return Err(DecodeError::Unavailable(format!(
                "token endpoint returned status {}",
                status.as_u16()
            )));
        }
        let body: Value = response.json().await.map_err(|error| {
            DecodeError::Unavailable(format!("token response unreadable: {error}"))
        })?;
        let token = body
            .get("access_token")
            .and_then(Value::as_str)
            .filter(|token| !token.is_empty())
            .ok_or_else(|| {
                DecodeError::Unavailable("token response has no access_token".to_owned())
            })?
            .to_owned();
        let expires_in = body
            .get("expires_in")
            .and_then(Value::as_i64)
            .filter(|seconds| *seconds > 0)
            .unwrap_or(DEFAULT_EXPIRES_IN_SECONDS);
        if let Ok(mut cache) = self.token.lock() {
            *cache = Some(CachedToken {
                token: token.clone(),
                expires_at: now_seconds + expires_in,
            });
        }
        Ok(token)
    }

    fn assertion(&self, now_seconds: i64) -> Option<String> {
        let header = json!({"alg": "RS256", "typ": "JWT"});
        let claims = json!({
            "iss": self.account.client_email,
            "scope": SCOPE,
            "aud": self.account.token_uri,
            "iat": now_seconds,
            "exp": now_seconds + ASSERTION_TTL_SECONDS,
        });
        let signing_input = format!(
            "{}.{}",
            URL_SAFE_NO_PAD.encode(serde_json::to_vec(&header).ok()?),
            URL_SAFE_NO_PAD.encode(serde_json::to_vec(&claims).ok()?)
        );
        let mut signature = vec![0u8; self.key.public().modulus_len()];
        self.key
            .sign(
                &RSA_PKCS1_SHA256,
                &self.random,
                signing_input.as_bytes(),
                &mut signature,
            )
            .ok()?;
        Some(format!(
            "{signing_input}.{}",
            URL_SAFE_NO_PAD.encode(signature)
        ))
    }
}

pub fn parse_rsa_pkcs8_pem(pem: &str) -> Option<RsaKeyPair> {
    let body: String = pem
        .replace("\\n", "\n")
        .lines()
        .filter(|line| !line.trim().starts_with("-----"))
        .flat_map(|line| line.chars().filter(|c| !c.is_ascii_whitespace()))
        .collect();
    RsaKeyPair::from_pkcs8(&STANDARD.decode(body).ok()?).ok()
}
