// SPDX-License-Identifier: AGPL-3.0-or-later

use base64::prelude::*;
use hmac::{Hmac, KeyInit, Mac};
use serde::{Deserialize, Serialize};
use sha2::Sha256;

use crate::ids::ApiSecret;

type HmacSha256 = Hmac<Sha256>;

pub const ADMIN_TOKEN_TTL_SECONDS: u64 = 600;

const HEADER_JSON: &str = r#"{"alg":"HS256","typ":"JWT"}"#;
const HEADER_ALG: &str = "HS256";
const HEADER_TYP: &str = "JWT";

#[derive(Clone, Copy, Debug, PartialEq, Eq, thiserror::Error)]
pub enum TokenError {
    #[error("token is not three dot-separated parts")]
    Malformed,
    #[error("token header is not an HS256 JWT header")]
    UnsupportedHeader,
    #[error("token carries invalid base64url")]
    BadBase64,
    #[error("token carries invalid json")]
    BadJson,
    #[error("token signature does not verify")]
    BadSignature,
    #[error("token issuer does not match the api key")]
    IssuerMismatch,
    #[error("token has expired")]
    Expired,
    #[error("token is not valid yet")]
    NotYetValid,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum VideoGrant {
    RoomList,
    RoomAdmin { room: String },
}

impl VideoGrant {
    pub fn claim(&self) -> VideoGrantClaim {
        match self {
            Self::RoomList => VideoGrantClaim {
                room_list: Some(true),
                room_admin: None,
                room: None,
            },
            Self::RoomAdmin { room } => VideoGrantClaim {
                room_list: None,
                room_admin: Some(true),
                room: Some(room.clone()),
            },
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct VideoGrantClaim {
    #[serde(rename = "roomList", default, skip_serializing_if = "Option::is_none")]
    pub room_list: Option<bool>,
    #[serde(rename = "roomAdmin", default, skip_serializing_if = "Option::is_none")]
    pub room_admin: Option<bool>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub room: Option<String>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct AdminClaims {
    pub iss: String,
    pub exp: u64,
    pub nbf: u64,
    pub video: VideoGrantClaim,
}

#[derive(Clone, Debug, PartialEq, Eq, Deserialize)]
struct TokenHeader {
    alg: String,
    typ: String,
}

pub fn admin_claims(api_key: &str, grant: &VideoGrant, now_unix_seconds: u64) -> AdminClaims {
    AdminClaims {
        iss: api_key.to_owned(),
        exp: now_unix_seconds.saturating_add(ADMIN_TOKEN_TTL_SECONDS),
        nbf: 0,
        video: grant.claim(),
    }
}

pub fn mint_admin_token(
    api_key: &str,
    api_secret: &ApiSecret,
    grant: &VideoGrant,
    now_unix_seconds: u64,
) -> String {
    encode_admin_token(&admin_claims(api_key, grant, now_unix_seconds), api_secret)
}

pub fn encode_admin_token(claims: &AdminClaims, api_secret: &ApiSecret) -> String {
    let header = BASE64_URL_SAFE_NO_PAD.encode(HEADER_JSON.as_bytes());
    let payload = BASE64_URL_SAFE_NO_PAD
        .encode(serde_json::to_vec(claims).expect("admin claims serialise to json"));
    let signing_input = format!("{header}.{payload}");
    let signature = sign(signing_input.as_bytes(), api_secret);
    format!(
        "{signing_input}.{}",
        BASE64_URL_SAFE_NO_PAD.encode(signature)
    )
}

pub fn verify_admin_token(
    token: &str,
    api_key: &str,
    api_secret: &ApiSecret,
    now_unix_seconds: u64,
) -> Result<AdminClaims, TokenError> {
    let mut parts = token.split('.');
    let (Some(header_b64), Some(payload_b64), Some(signature_b64), None) =
        (parts.next(), parts.next(), parts.next(), parts.next())
    else {
        return Err(TokenError::Malformed);
    };

    let header_bytes = decode_part(header_b64)?;
    let header: TokenHeader =
        serde_json::from_slice(&header_bytes).map_err(|_| TokenError::BadJson)?;
    if header.alg != HEADER_ALG || header.typ != HEADER_TYP {
        return Err(TokenError::UnsupportedHeader);
    }

    let signature = decode_part(signature_b64)?;
    let mut mac = HmacSha256::new_from_slice(api_secret.expose().as_bytes())
        .expect("hmac accepts any key length");
    mac.update(header_b64.as_bytes());
    mac.update(b".");
    mac.update(payload_b64.as_bytes());
    mac.verify_slice(&signature)
        .map_err(|_| TokenError::BadSignature)?;

    let payload_bytes = decode_part(payload_b64)?;
    let claims: AdminClaims =
        serde_json::from_slice(&payload_bytes).map_err(|_| TokenError::BadJson)?;
    if claims.iss != api_key {
        return Err(TokenError::IssuerMismatch);
    }
    if now_unix_seconds >= claims.exp {
        return Err(TokenError::Expired);
    }
    if now_unix_seconds < claims.nbf {
        return Err(TokenError::NotYetValid);
    }
    Ok(claims)
}

fn decode_part(part: &str) -> Result<Vec<u8>, TokenError> {
    BASE64_URL_SAFE_NO_PAD
        .decode(part.as_bytes())
        .map_err(|_| TokenError::BadBase64)
}

fn sign(input: &[u8], api_secret: &ApiSecret) -> Vec<u8> {
    let mut mac = HmacSha256::new_from_slice(api_secret.expose().as_bytes())
        .expect("hmac accepts any key length");
    mac.update(input);
    mac.finalize().into_bytes().to_vec()
}
