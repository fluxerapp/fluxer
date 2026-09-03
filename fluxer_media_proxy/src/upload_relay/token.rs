// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{secret::SecretBytes, upload_relay::RelayError};
use base64::{Engine as _, engine::general_purpose::URL_SAFE_NO_PAD};
use hmac::{Hmac, KeyInit, Mac};
use http::Method;
use serde::{Deserialize, Serialize};
use sha2::Sha256;
use std::time::{SystemTime, UNIX_EPOCH};
use thiserror::Error;

type HmacSha256 = Hmac<Sha256>;

const MAX_TOKEN_BYTES: usize = 16 * 1024;
const SIGNATURE_BYTES: usize = 32;

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum TokenMethod {
    Put,
}

impl TokenMethod {
    pub fn http(self) -> Method {
        match self {
            Self::Put => Method::PUT,
        }
    }
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct TokenPayload {
    pub b: String,
    pub k: String,
    pub m: TokenMethod,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub u: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub p: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub ct: Option<String>,
    pub mb: u64,
    pub e: u64,
}

#[derive(Debug, Error, Eq, PartialEq)]
pub enum TokenError {
    #[error("malformed token")]
    Malformed,
    #[error("bad token encoding")]
    BadEncoding,
    #[error("bad token JSON")]
    BadJson,
    #[error("bad token signature")]
    BadSignature,
    #[error("expired token")]
    Expired,
}

pub fn encode_token(token: &TokenPayload, secret: &[u8]) -> anyhow::Result<String> {
    let payload = serde_json::to_vec(token)?;
    let encoded_payload = URL_SAFE_NO_PAD.encode(payload);
    let mut mac = HmacSha256::new_from_slice(secret).expect("HMAC accepts any key length");
    mac.update(encoded_payload.as_bytes());
    let sig = mac.finalize().into_bytes();
    Ok(format!(
        "{}.{}",
        encoded_payload,
        URL_SAFE_NO_PAD.encode(sig)
    ))
}

pub fn decode_token(raw: &str, secret: &[u8], now_unix: u64) -> Result<TokenPayload, TokenError> {
    if raw.len() > MAX_TOKEN_BYTES {
        return Err(TokenError::Malformed);
    }
    let (payload_b64, sig_b64) = raw.split_once('.').ok_or(TokenError::Malformed)?;
    let sig = decode_canonical_base64(sig_b64)?;
    if sig.expose().len() != SIGNATURE_BYTES {
        return Err(TokenError::BadSignature);
    }
    let mut mac = HmacSha256::new_from_slice(secret).expect("HMAC accepts any key length");
    mac.update(payload_b64.as_bytes());
    let expected = mac.finalize().into_bytes();
    let mut diff = 0u8;
    for (a, b) in sig.expose().iter().zip(expected.iter()) {
        diff |= a ^ b;
    }
    if diff != 0 {
        return Err(TokenError::BadSignature);
    }
    let payload = decode_canonical_base64(payload_b64)?;
    let parsed: TokenPayload =
        serde_json::from_slice(payload.expose()).map_err(|_| TokenError::BadJson)?;
    if now_unix >= parsed.e {
        return Err(TokenError::Expired);
    }
    Ok(parsed)
}

fn decode_canonical_base64(raw: &str) -> Result<SecretBytes, TokenError> {
    let mut decoded = Vec::new();
    decoded
        .try_reserve_exact(raw.len())
        .map_err(|_| TokenError::BadEncoding)?;
    decoded.resize(raw.len(), 0);
    let decoded_len = URL_SAFE_NO_PAD
        .decode_slice(raw, decoded.as_mut_slice())
        .map_err(|_| TokenError::BadEncoding)?;
    decoded.truncate(decoded_len);
    let decoded = SecretBytes::new(decoded);
    let mut canonical = Vec::new();
    canonical
        .try_reserve_exact(raw.len())
        .map_err(|_| TokenError::BadEncoding)?;
    canonical.resize(raw.len(), 0);
    let canonical_len = URL_SAFE_NO_PAD
        .encode_slice(decoded.expose(), canonical.as_mut_slice())
        .map_err(|_| TokenError::BadEncoding)?;
    if canonical.get(..canonical_len) != Some(raw.as_bytes()) {
        return Err(TokenError::BadEncoding);
    }
    Ok(decoded)
}

pub fn map_token_error(err: TokenError) -> RelayError {
    match err {
        TokenError::Expired => RelayError::RelayTokenExpired,
        _ => RelayError::InvalidToken,
    }
}

pub fn token_from_query(raw: Option<&str>) -> Result<&str, RelayError> {
    let token = raw.ok_or(RelayError::MissingToken)?;
    if token.is_empty() {
        return Err(RelayError::InvalidToken);
    }
    if token.len() > MAX_TOKEN_BYTES {
        return Err(RelayError::InvalidToken);
    }
    Ok(token)
}

pub fn now_unix() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_secs())
        .unwrap_or(0)
}
