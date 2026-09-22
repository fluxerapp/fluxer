// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::client::AdminApiClient,
    config::AdminConfig,
    oauth2,
    session::{self, LEGACY_SESSION_COOKIE_NAME, SESSION_COOKIE_NAME, SESSION_MAX_AGE},
    state::AppState,
    templates::pages::login,
};
use axum::{
    Json, Router,
    extract::{Query, State},
    http::{HeaderMap, HeaderValue, StatusCode, header},
    response::{Html, IntoResponse, Redirect, Response},
    routing::{get, post},
};
use base64::{Engine as _, engine::general_purpose::URL_SAFE_NO_PAD};
use hmac::{Hmac, KeyInit, Mac};
use rand::RngExt;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::time::{SystemTime, UNIX_EPOCH};

type HmacSha256 = Hmac<Sha256>;

const OAUTH_STATE_MAX_AGE_SECONDS: u64 = 300;
const OAUTH_STATE_NONCE_BYTES: usize = 16;
const OAUTH_STATE_VERIFIER_HASH_BYTES: usize = 32;
const OAUTH_STATE_PAYLOAD_BYTES: usize =
    std::mem::size_of::<u64>() + OAUTH_STATE_NONCE_BYTES + OAUTH_STATE_VERIFIER_HASH_BYTES;
const OAUTH_FLOW_STORAGE_KEY: &str = "fluxer-admin-oauth-flow";
const OAUTH_FINISH_HEADER: &str = "x-fluxer-admin-oauth";

#[derive(Deserialize)]
struct LoginQuery {
    error: Option<String>,
}

#[derive(Deserialize)]
struct CallbackQuery {
    code: Option<String>,
    state: Option<String>,
}

#[derive(Deserialize)]
struct CallbackFinishBody {
    code: String,
    state: String,
    verifier: String,
}

#[derive(Serialize)]
struct CallbackFinishResponse {
    redirect_to: String,
}

pub fn router() -> Router<AppState> {
    Router::new()
        .route("/login", get(login_page))
        .route("/auth/start", get(auth_start))
        .route(
            "/oauth2_callback",
            get(oauth2_callback_page).post(oauth2_callback_finish),
        )
        .route("/logout", post(logout))
        .route("/logout", get(logout_get))
}

async fn login_page(
    State(state): State<AppState>,
    headers: axum::http::HeaderMap,
    Query(query): Query<LoginQuery>,
) -> Response {
    let config = state.config();
    if let Some(session_value) = extract_cookie(&headers, SESSION_COOKIE_NAME)
        && session::parse_session(&session_value, &config.secret_key_base).is_some()
    {
        return Redirect::to(&format!("{}/dashboard", config.base_path)).into_response();
    }
    let error_msg = match query.error.as_deref() {
        Some("oauth_failed") => Some("Authentication failed. Please try again."),
        Some("missing_admin_acl") => Some(
            "Access denied: missing admin:authenticate permission. \
             Ask an administrator to grant access.",
        ),
        Some(_) => Some("Login error. Please try again."),
        None => None,
    };
    Html(login::login_page(config, error_msg).into_string()).into_response()
}

async fn auth_start(State(state): State<AppState>) -> Response {
    let config = state.config();
    let verifier = random_nonce();
    let oauth_state = generate_oauth_state(&config.secret_key_base, &verifier);
    let authorize_url = oauth2::authorize_url(config, &oauth_state);
    Html(oauth_start_page(
        config,
        &authorize_url,
        &oauth_state,
        &verifier,
    ))
    .into_response()
}

async fn oauth2_callback_page(
    State(state): State<AppState>,
    Query(query): Query<CallbackQuery>,
) -> Response {
    let config = state.config();
    Html(oauth_callback_page(
        config,
        query.code.as_deref(),
        query.state.as_deref(),
    ))
    .into_response()
}

async fn oauth2_callback_finish(
    State(state): State<AppState>,
    headers: HeaderMap,
    Json(body): Json<CallbackFinishBody>,
) -> Response {
    let config = state.config();
    let base = &config.base_path;

    if headers
        .get(OAUTH_FINISH_HEADER)
        .and_then(|value| value.to_str().ok())
        != Some("1")
    {
        return oauth_finish_response(
            StatusCode::FORBIDDEN,
            &format!("{base}/login?error=oauth_failed"),
            &[],
        );
    }

    if !verify_oauth_state(&config.secret_key_base, &body.state, &body.verifier) {
        tracing::error!("OAuth2 state token is invalid, expired, or malformed");
        return oauth_finish_response(
            StatusCode::BAD_REQUEST,
            &format!("{base}/login?error=oauth_failed"),
            &[],
        );
    }

    let token = match oauth2::exchange_code(state.http_client(), config, &body.code).await {
        Ok(t) => t,
        Err(err) => {
            tracing::error!(%err, "OAuth2 token exchange failed");
            return oauth_finish_response(
                StatusCode::BAD_REQUEST,
                &format!("{base}/login?error=oauth_failed"),
                &[],
            );
        }
    };

    let user = match oauth2::fetch_user_info(state.http_client(), config, &token.access_token).await
    {
        Ok(u) => u,
        Err(err) => {
            tracing::error!(%err, "OAuth2 user info fetch failed");
            return oauth_finish_response(
                StatusCode::BAD_REQUEST,
                &format!("{base}/login?error=oauth_failed"),
                &[],
            );
        }
    };

    let temp_session = session::Session {
        user_id: user.id.clone(),
        access_token: token.access_token.clone(),
        created_at: 0,
    };
    let api_client = AdminApiClient::new(state.http_client(), config, &temp_session);
    if api_client.get_current_admin().await.is_err() {
        return oauth_finish_response(
            StatusCode::FORBIDDEN,
            &format!("{base}/login?error=missing_admin_acl"),
            &[],
        );
    }

    let session_cookie_value =
        session::create_session(&user.id, &token.access_token, &config.secret_key_base);
    let secure = if config.secure_cookies() {
        "; Secure"
    } else {
        ""
    };
    let session_cookie = format!(
        "{}={}; Path={}; HttpOnly; SameSite=Lax; Max-Age={}{}",
        SESSION_COOKIE_NAME,
        session_cookie_value,
        admin_cookie_path(config),
        SESSION_MAX_AGE,
        secure
    );

    let mut cookies = clear_session_cookie_values(config);
    cookies.push(session_cookie);
    oauth_finish_response(StatusCode::OK, &format!("{base}/dashboard"), &cookies)
}

async fn logout(State(state): State<AppState>, headers: axum::http::HeaderMap) -> Response {
    let config = state.config();
    if let Some(session_value) = extract_cookie(&headers, SESSION_COOKIE_NAME)
        && let Some(session) = session::parse_session(&session_value, &config.secret_key_base)
    {
        let _ = oauth2::revoke_token(state.http_client(), config, &session.access_token).await;
    }
    redirect_with_cookies(
        &format!("{}/login", config.base_path),
        &clear_session_cookie_values(config),
    )
}

async fn logout_get(State(state): State<AppState>, headers: axum::http::HeaderMap) -> Response {
    let config = state.config();
    if let Some(session_value) = extract_cookie(&headers, SESSION_COOKIE_NAME)
        && session::parse_session(&session_value, &config.secret_key_base).is_some()
    {
        return Redirect::to(&format!("{}/dashboard", config.base_path)).into_response();
    }
    Redirect::to(&format!("{}/login", config.base_path)).into_response()
}

fn extract_cookie(headers: &axum::http::HeaderMap, name: &str) -> Option<String> {
    let cookie_header = headers.get(header::COOKIE)?.to_str().ok()?;
    let prefix = format!("{name}=");
    for pair in cookie_header.split(';') {
        let pair = pair.trim();
        if let Some(value) = pair.strip_prefix(&prefix) {
            let trimmed = value.trim();
            if !trimmed.is_empty() {
                return Some(trimmed.to_owned());
            }
        }
    }
    None
}

fn oauth_start_page(
    config: &AdminConfig,
    authorize_url: &str,
    state: &str,
    verifier: &str,
) -> String {
    let storage_key = json_string(OAUTH_FLOW_STORAGE_KEY);
    let authorize_url = json_string(authorize_url);
    let login_url = json_string(&format!("{}/login?error=oauth_failed", config.base_path));
    let state = json_string(state);
    let verifier = json_string(verifier);
    format!(
        r#"<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Fluxer Admin</title>
</head>
<body>
<p>Starting authentication...</p>
<script>
(function () {{
	var flow = {{state: {state}, verifier: {verifier}, createdAt: Date.now()}};
	try {{
		window.sessionStorage.setItem({storage_key}, JSON.stringify(flow));
		window.location.replace({authorize_url});
	}} catch (error) {{
		window.location.replace({login_url});
	}}
}})();
</script>
</body>
</html>"#
    )
}

fn oauth_callback_page(config: &AdminConfig, code: Option<&str>, state: Option<&str>) -> String {
    let storage_key = json_string(OAUTH_FLOW_STORAGE_KEY);
    let login_url = json_string(&format!("{}/login?error=oauth_failed", config.base_path));
    let code = json_string(code.unwrap_or_default());
    let state = json_string(state.unwrap_or_default());
    format!(
        r#"<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Fluxer Admin</title>
</head>
<body>
<p>Completing authentication...</p>
<script>
(function () {{
	var storageKey = {storage_key};
	var loginUrl = {login_url};
	var code = {code};
	var state = {state};
	function fail(clearFlow) {{
		if (clearFlow) {{
			try {{ window.sessionStorage.removeItem(storageKey); }} catch (error) {{}}
		}}
		window.location.replace(loginUrl);
	}}
	if (!code || !state) {{
		fail(false);
		return;
	}}
	var flow = null;
	try {{
		flow = JSON.parse(window.sessionStorage.getItem(storageKey) || 'null');
	}} catch (error) {{
		flow = null;
	}}
	if (!flow || flow.state !== state || !flow.verifier) {{
		fail(false);
		return;
	}}
	try {{ window.sessionStorage.removeItem(storageKey); }} catch (error) {{}}
	window.fetch(window.location.pathname, {{
		method: 'POST',
		credentials: 'same-origin',
		headers: {{
			'Content-Type': 'application/json',
			'X-Fluxer-Admin-OAuth': '1'
		}},
		body: JSON.stringify({{code: code, state: state, verifier: flow.verifier}})
	}})
		.then(function (response) {{
			return response.json().catch(function () {{ return null; }});
		}})
		.then(function (body) {{
			window.location.replace((body && body.redirect_to) || loginUrl);
		}})
		.catch(function () {{
			fail(true);
		}});
}})();
</script>
</body>
</html>"#
    )
}

pub(crate) fn json_string(value: &str) -> String {
    serde_json::to_string(value)
        .expect("JSON string serialization cannot fail")
        .replace('<', "\\u003c")
        .replace('>', "\\u003e")
        .replace('&', "\\u0026")
        .replace('\u{2028}', "\\u2028")
        .replace('\u{2029}', "\\u2029")
}

fn admin_cookie_path(config: &AdminConfig) -> &str {
    if config.base_path.is_empty() {
        "/"
    } else {
        config.base_path.as_str()
    }
}

fn clear_cookie_value(name: &str, path: &str) -> String {
    format!("{name}=; Path={path}; HttpOnly; SameSite=Lax; Max-Age=0")
}

fn clear_cookie_values(name: &str, config: &AdminConfig) -> Vec<String> {
    let admin_path = admin_cookie_path(config);
    let mut cookies = vec![clear_cookie_value(name, admin_path)];
    if admin_path != "/" {
        cookies.push(clear_cookie_value(name, "/"));
    }
    cookies
}

fn clear_session_cookie_values(config: &AdminConfig) -> Vec<String> {
    let mut cookies = clear_cookie_values(SESSION_COOKIE_NAME, config);
    cookies.extend(clear_cookie_values(LEGACY_SESSION_COOKIE_NAME, config));
    cookies
}

fn append_cookie(response: &mut Response, cookie: &str) {
    if let Ok(value) = HeaderValue::from_str(cookie) {
        response.headers_mut().append(header::SET_COOKIE, value);
    }
}

fn append_cookies(response: &mut Response, cookies: &[String]) {
    for cookie in cookies {
        append_cookie(response, cookie);
    }
}

fn redirect_with_cookies(url: &str, cookies: &[String]) -> Response {
    let mut response = Redirect::to(url).into_response();
    append_cookies(&mut response, cookies);
    response
}

fn oauth_finish_response(status: StatusCode, redirect_to: &str, cookies: &[String]) -> Response {
    let mut response = (
        status,
        Json(CallbackFinishResponse {
            redirect_to: redirect_to.to_owned(),
        }),
    )
        .into_response();
    append_cookies(&mut response, cookies);
    response
}

fn generate_oauth_state(secret_key: &str, verifier: &str) -> String {
    sign_oauth_state(secret_key, now_seconds(), random_state_nonce(), verifier)
}

fn sign_oauth_state(
    secret_key: &str,
    issued_at: u64,
    nonce: [u8; OAUTH_STATE_NONCE_BYTES],
    verifier: &str,
) -> String {
    let encoded_payload = encode_oauth_state_payload(issued_at, nonce, verifier);
    let signature = sign_oauth_state_payload(secret_key, &encoded_payload);
    format!("{encoded_payload}.{signature}")
}

fn verify_oauth_state(secret_key: &str, state: &str, verifier: &str) -> bool {
    let Some((payload, signature)) = state.rsplit_once('.') else {
        return false;
    };
    if !constant_time_eq(
        signature.as_bytes(),
        sign_oauth_state_payload(secret_key, payload).as_bytes(),
    ) {
        return false;
    }
    let Ok(decoded) = URL_SAFE_NO_PAD.decode(payload.as_bytes()) else {
        return false;
    };
    if decoded.len() != OAUTH_STATE_PAYLOAD_BYTES {
        return false;
    }
    let mut issued_at_bytes = [0u8; std::mem::size_of::<u64>()];
    issued_at_bytes.copy_from_slice(&decoded[..std::mem::size_of::<u64>()]);
    let issued_at = u64::from_be_bytes(issued_at_bytes);
    let verifier_hash_start = std::mem::size_of::<u64>() + OAUTH_STATE_NONCE_BYTES;
    let actual_verifier_hash = &decoded[verifier_hash_start..];
    if !constant_time_eq(
        actual_verifier_hash,
        verifier_hash_bytes(verifier).as_slice(),
    ) {
        return false;
    }
    let now = now_seconds();
    issued_at <= now && now - issued_at <= OAUTH_STATE_MAX_AGE_SECONDS
}

fn encode_oauth_state_payload(
    issued_at: u64,
    nonce: [u8; OAUTH_STATE_NONCE_BYTES],
    verifier: &str,
) -> String {
    let mut payload = Vec::with_capacity(OAUTH_STATE_PAYLOAD_BYTES);
    payload.extend_from_slice(&issued_at.to_be_bytes());
    payload.extend_from_slice(&nonce);
    payload.extend_from_slice(&verifier_hash_bytes(verifier));
    URL_SAFE_NO_PAD.encode(payload)
}

fn verifier_hash_bytes(verifier: &str) -> [u8; OAUTH_STATE_VERIFIER_HASH_BYTES] {
    let mut hasher = Sha256::new();
    hasher.update(b"fluxer-admin-oauth-verifier:");
    hasher.update(verifier.as_bytes());
    let digest = hasher.finalize();
    let mut bytes = [0u8; OAUTH_STATE_VERIFIER_HASH_BYTES];
    bytes.copy_from_slice(&digest);
    bytes
}

fn sign_oauth_state_payload(secret_key: &str, payload: &str) -> String {
    let mut mac =
        HmacSha256::new_from_slice(secret_key.as_bytes()).expect("HMAC can take key of any size");
    mac.update(b"fluxer-admin-oauth-state:");
    mac.update(payload.as_bytes());
    URL_SAFE_NO_PAD.encode(mac.finalize().into_bytes())
}

fn constant_time_eq(a: &[u8], b: &[u8]) -> bool {
    if a.len() != b.len() {
        return false;
    }
    let mut diff = 0u8;
    for (left, right) in a.iter().zip(b.iter()) {
        diff |= left ^ right;
    }
    diff == 0
}

fn random_nonce() -> String {
    let mut rng = rand::rng();
    let bytes: [u8; 32] = rng.random();
    bytes.iter().map(|b| format!("{b:02x}")).collect()
}

fn random_state_nonce() -> [u8; OAUTH_STATE_NONCE_BYTES] {
    rand::rng().random()
}

fn now_seconds() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("system clock before Unix epoch")
        .as_secs()
}

#[cfg(test)]
mod tests {
    use super::*;

    const SECRET: &str = "test-admin-state-secret";
    const VERIFIER: &str = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef";

    #[test]
    fn generated_oauth_state_verifies() {
        let state = generate_oauth_state(SECRET, VERIFIER);
        assert!(verify_oauth_state(SECRET, &state, VERIFIER));
    }

    #[test]
    fn generated_oauth_state_fits_api_state_limit() {
        let state = generate_oauth_state(SECRET, VERIFIER);
        assert!(state.len() <= 256, "state length was {}", state.len());
    }

    #[test]
    fn oauth_state_rejects_tampering() {
        let mut state = generate_oauth_state(SECRET, VERIFIER);
        let last = state.pop().unwrap();
        state.push(if last == 'A' { 'B' } else { 'A' });
        assert!(!verify_oauth_state(SECRET, &state, VERIFIER));
    }

    #[test]
    fn oauth_state_rejects_wrong_secret() {
        let state = generate_oauth_state(SECRET, VERIFIER);
        assert!(!verify_oauth_state("different-secret", &state, VERIFIER));
    }

    #[test]
    fn oauth_state_rejects_wrong_verifier() {
        let state = generate_oauth_state(SECRET, VERIFIER);
        assert!(!verify_oauth_state(
            SECRET,
            &state,
            "fedcba9876543210fedcba9876543210fedcba9876543210fedcba9876543210"
        ));
    }

    #[test]
    fn oauth_state_rejects_expired_payloads() {
        let issued_at = now_seconds().saturating_sub(OAUTH_STATE_MAX_AGE_SECONDS + 1);
        let state = sign_oauth_state(SECRET, issued_at, random_state_nonce(), VERIFIER);
        assert!(!verify_oauth_state(SECRET, &state, VERIFIER));
    }

    #[test]
    fn oauth_state_rejects_future_payloads() {
        let issued_at = now_seconds() + 1;
        let state = sign_oauth_state(SECRET, issued_at, random_state_nonce(), VERIFIER);
        assert!(!verify_oauth_state(SECRET, &state, VERIFIER));
    }

    #[test]
    fn json_string_is_safe_inside_script_tags() {
        let encoded = json_string("</script><script>alert(1)</script>");
        assert!(!encoded.contains("</script>"));
        assert!(encoded.contains("\\u003c/script\\u003e"));
    }
}
