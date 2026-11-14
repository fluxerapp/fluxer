// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::config::AdminConfig;
use axum::{
    http::{HeaderValue, header},
    response::Response,
};
use base64::{Engine as _, engine::general_purpose::URL_SAFE_NO_PAD};
use hmac::{Hmac, KeyInit, Mac};
use rand::RngExt;
use serde::{Deserialize, Serialize};
use sha2::Sha256;
use std::time::{SystemTime, UNIX_EPOCH};

type HmacSha256 = Hmac<Sha256>;

const MAX_AGE_SECONDS: u64 = 60 * 60 * 24 * 7;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Session {
    #[serde(rename = "userId")]
    pub user_id: String,
    #[serde(rename = "accessToken")]
    pub access_token: String,
    #[serde(rename = "createdAt")]
    pub created_at: u64,
}

pub fn create_session(user_id: &str, access_token: &str, secret_key: &str) -> String {
    let now = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("system clock before Unix epoch")
        .as_secs();
    let session = Session {
        user_id: user_id.to_owned(),
        access_token: access_token.to_owned(),
        created_at: now,
    };
    let json = serde_json::to_string(&session).expect("session serialization cannot fail");
    let encoded = URL_SAFE_NO_PAD.encode(json.as_bytes());
    sign_data(&encoded, secret_key)
}

pub fn parse_session(cookie_value: &str, secret_key: &str) -> Option<Session> {
    let data = verify_signature(cookie_value, secret_key)?;
    let decoded_bytes = URL_SAFE_NO_PAD.decode(data.as_bytes()).ok()?;
    let decoded_str = String::from_utf8(decoded_bytes).ok()?;
    let session: Session = serde_json::from_str(&decoded_str).ok()?;
    let now = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("system clock before Unix epoch")
        .as_secs();
    if now.saturating_sub(session.created_at) > MAX_AGE_SECONDS {
        return None;
    }
    Some(session)
}

fn sign_data(data: &str, secret_key: &str) -> String {
    let mut mac =
        HmacSha256::new_from_slice(secret_key.as_bytes()).expect("HMAC can take key of any size");
    mac.update(data.as_bytes());
    let signature = URL_SAFE_NO_PAD.encode(mac.finalize().into_bytes());
    format!("{data}.{signature}")
}

fn verify_signature<'a>(signed_data: &'a str, secret_key: &str) -> Option<&'a str> {
    let dot_index = signed_data.rfind('.')?;
    let data = &signed_data[..dot_index];
    let provided_signature = &signed_data[dot_index + 1..];
    let mut mac =
        HmacSha256::new_from_slice(secret_key.as_bytes()).expect("HMAC can take key of any size");
    mac.update(data.as_bytes());
    let expected_signature = URL_SAFE_NO_PAD.encode(mac.finalize().into_bytes());
    let provided_bytes = URL_SAFE_NO_PAD.decode(provided_signature).ok()?;
    let expected_bytes = URL_SAFE_NO_PAD.decode(expected_signature.as_bytes()).ok()?;
    if provided_bytes.len() != expected_bytes.len() {
        return None;
    }
    let mut diff = 0u8;
    for (a, b) in provided_bytes.iter().zip(expected_bytes.iter()) {
        diff |= a ^ b;
    }
    if diff == 0 { Some(data) } else { None }
}

pub fn create_csrf_token(user_id: &str, secret_key: &str) -> String {
    let mut rng = rand::rng();
    let nonce: [u8; 16] = rng.random();
    let payload = URL_SAFE_NO_PAD.encode(format!(
        "{}:{}",
        URL_SAFE_NO_PAD.encode(user_id.as_bytes()),
        URL_SAFE_NO_PAD.encode(nonce)
    ));
    sign_data(&payload, secret_key)
}

pub fn verify_csrf_token(token: &str, user_id: &str, secret_key: &str) -> bool {
    let Some(data) = verify_signature(token, secret_key) else {
        return false;
    };
    let Ok(decoded) = URL_SAFE_NO_PAD.decode(data.as_bytes()) else {
        return false;
    };
    let Ok(payload) = String::from_utf8(decoded) else {
        return false;
    };
    let Some((encoded_uid, _nonce)) = payload.split_once(':') else {
        return false;
    };
    match URL_SAFE_NO_PAD.decode(encoded_uid.as_bytes()) {
        Ok(uid_bytes) => uid_bytes == user_id.as_bytes(),
        Err(_) => false,
    }
}

pub const SESSION_COOKIE_NAME: &str = "admin_session";
pub const SESSION_MAX_AGE: i64 = MAX_AGE_SECONDS as i64;

pub(crate) fn admin_cookie_path(config: &AdminConfig) -> &str {
    if config.base_path.is_empty() {
        "/"
    } else {
        config.base_path.as_str()
    }
}

pub(crate) fn clear_cookie_value(name: &str, path: &str) -> String {
    format!("{name}=; Path={path}; HttpOnly; SameSite=Lax; Max-Age=0")
}

pub(crate) fn clear_cookie_values(name: &str, config: &AdminConfig) -> Vec<String> {
    let admin_path = admin_cookie_path(config);
    let mut cookies = vec![clear_cookie_value(name, admin_path)];
    if admin_path != "/" {
        cookies.push(clear_cookie_value(name, "/"));
    }
    cookies
}

pub(crate) fn append_cookie(response: &mut Response, cookie: &str) {
    if let Ok(value) = HeaderValue::from_str(cookie) {
        response.headers_mut().append(header::SET_COOKIE, value);
    }
}

pub(crate) fn append_cookies(response: &mut Response, cookies: &[String]) {
    for cookie in cookies {
        append_cookie(response, cookie);
    }
}
