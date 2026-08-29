// SPDX-License-Identifier: AGPL-3.0-or-later

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

pub const LEGACY_SESSION_COOKIE_NAME: &str = "session";
pub const SESSION_COOKIE_NAME: &str = "admin_session";
pub const SESSION_MAX_AGE: i64 = MAX_AGE_SECONDS as i64;

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_SECRET: &str = "test-secret-key-for-unit-tests";

    #[test]
    fn create_session_produces_valid_base64url_and_hmac() {
        let token = create_session("12345", "tok_abc", TEST_SECRET);
        let parts: Vec<&str> = token.rsplitn(2, '.').collect();
        assert_eq!(parts.len(), 2, "session must contain a dot separator");
        let payload = parts[1];
        let sig = parts[0];
        assert!(
            URL_SAFE_NO_PAD.decode(payload.as_bytes()).is_ok(),
            "payload must be valid base64url"
        );
        assert!(
            URL_SAFE_NO_PAD.decode(sig.as_bytes()).is_ok(),
            "signature must be valid base64url"
        );
    }

    #[test]
    fn parse_session_decodes_created_session() {
        let token = create_session("99999", "tok_xyz", TEST_SECRET);
        let session = parse_session(&token, TEST_SECRET).expect("must parse");
        assert_eq!(session.user_id, "99999");
        assert_eq!(session.access_token, "tok_xyz");
        assert!(session.created_at > 0);
    }

    #[test]
    fn parse_session_rejects_tampered_signature() {
        let token = create_session("12345", "tok_abc", TEST_SECRET);
        let dot = token.rfind('.').unwrap();
        let mut tampered = token.clone();
        let replacement = if tampered.as_bytes()[dot + 1] == b'A' {
            'B'
        } else {
            'A'
        };
        unsafe {
            tampered.as_bytes_mut()[dot + 1] = replacement as u8;
        }
        assert!(
            parse_session(&tampered, TEST_SECRET).is_none(),
            "tampered signature must be rejected"
        );
    }

    #[test]
    fn parse_session_rejects_wrong_secret() {
        let token = create_session("12345", "tok_abc", TEST_SECRET);
        assert!(
            parse_session(&token, "wrong-secret").is_none(),
            "wrong secret must be rejected"
        );
    }

    #[test]
    fn parse_session_rejects_expired() {
        let session = Session {
            user_id: "12345".to_owned(),
            access_token: "tok_old".to_owned(),
            created_at: 1_000_000,
        };
        let json = serde_json::to_string(&session).unwrap();
        let encoded = URL_SAFE_NO_PAD.encode(json.as_bytes());
        let signed = sign_data(&encoded, TEST_SECRET);
        assert!(
            parse_session(&signed, TEST_SECRET).is_none(),
            "expired session must be rejected"
        );
    }

    #[test]
    fn parse_session_returns_none_for_garbage() {
        assert!(parse_session("", TEST_SECRET).is_none());
        assert!(parse_session("not-a-session", TEST_SECRET).is_none());
        assert!(parse_session("abc.def.ghi", TEST_SECRET).is_none());
        assert!(parse_session(".....", TEST_SECRET).is_none());
        assert!(parse_session("\0\0\0", TEST_SECRET).is_none());
    }

    #[test]
    fn session_json_uses_camel_case() {
        let session = Session {
            user_id: "1".to_owned(),
            access_token: "t".to_owned(),
            created_at: 100,
        };
        let json = serde_json::to_string(&session).unwrap();
        assert!(json.contains("\"userId\""), "must use camelCase userId");
        assert!(
            json.contains("\"accessToken\""),
            "must use camelCase accessToken"
        );
        assert!(
            json.contains("\"createdAt\""),
            "must use camelCase createdAt"
        );
        assert!(!json.contains("user_id"), "must not use snake_case");
    }

    #[test]
    fn round_trip_preserves_fields() {
        let token = create_session("67890", "tok_roundtrip", TEST_SECRET);
        let session = parse_session(&token, TEST_SECRET).expect("must parse");
        assert_eq!(session.user_id, "67890");
        assert_eq!(session.access_token, "tok_roundtrip");
        let now = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs();
        assert!(
            now.saturating_sub(session.created_at) < 5,
            "created_at should be recent"
        );
    }
}
