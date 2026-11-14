// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::client::{AdminApiClient, ApiError},
    config::AdminConfig,
    middleware::csrf,
    oauth2,
    session::{
        self, SESSION_COOKIE_NAME, SESSION_MAX_AGE, admin_cookie_path, append_cookies,
        clear_cookie_values,
    },
    sso,
    state::AppState,
    templates::{layout_scripts::HISTORY_CACHE_CLEAR_SCRIPT, pages::login},
};
use axum::{
    Json, Router,
    extract::{Query, State},
    http::{HeaderMap, StatusCode, header},
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

const OAUTH_STATE_MAX_AGE_SECONDS: u64 = 1800;
const OAUTH_STATE_NONCE_BYTES: usize = 16;
const OAUTH_STATE_VERIFIER_HASH_BYTES: usize = 32;
const OAUTH_STATE_PAYLOAD_BYTES: usize =
    std::mem::size_of::<u64>() + OAUTH_STATE_NONCE_BYTES + OAUTH_STATE_VERIFIER_HASH_BYTES;
const OAUTH_FLOW_STORAGE_KEY: &str = "fluxer-admin-oauth-flow";
const SSO_FLOW_STORAGE_KEY: &str = "fluxer-admin-sso-flow";
const OAUTH_FINISH_HEADER: &str = "x-fluxer-admin-oauth";
const OAUTH_STATE_PURPOSE: &str = "oauth";
const SSO_STATE_PURPOSE: &str = "sso";
const OAUTH_FAILED: &str = "oauth_failed";
const SSO_FAILED: &str = "sso_failed";
const SSO_UNAVAILABLE: &str = "sso_unavailable";

#[derive(Deserialize)]
struct LoginQuery {
    error: Option<String>,
}

#[derive(Deserialize)]
struct CallbackQuery {
    code: Option<String>,
    state: Option<String>,
    iss: Option<String>,
}

#[derive(Deserialize)]
struct CallbackFinishBody {
    code: String,
    state: String,
    verifier: String,
    iss: Option<String>,
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
        .route("/sso/start", get(sso_start))
        .route(
            "/sso/callback",
            get(sso_callback_page).post(sso_callback_finish),
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
    let error_msg = query.error.as_deref().map(login_error_message);
    let branding = state.self_hosted_branding().await;
    Html(login::login_page(config, branding.as_ref(), error_msg).into_string()).into_response()
}

async fn auth_start(State(state): State<AppState>) -> Response {
    let config = state.config();
    if let Some(response) = sso_only_redirect(config) {
        return response;
    }
    let verifier = random_nonce();
    let oauth_state = generate_oauth_state(&config.secret_key_base, OAUTH_STATE_PURPOSE, &verifier);
    let authorize_url = oauth2::authorize_url(config, &oauth_state);
    Html(oauth_start_page(
        config,
        OAUTH_FLOW_STORAGE_KEY,
        OAUTH_FAILED,
        &authorize_url,
        &oauth_state,
        &verifier,
    ))
    .into_response()
}

async fn sso_start(State(state): State<AppState>) -> Response {
    let config = state.config();
    let base = &config.base_path;
    let Some(sso_config) = config.sso.as_ref() else {
        return Redirect::to(&format!("{base}/login")).into_response();
    };
    let Some(metadata) = state.sso_provider().await else {
        return Redirect::to(&format!("{base}/login?error={SSO_UNAVAILABLE}")).into_response();
    };
    let verifier = random_nonce();
    let sso_state = generate_oauth_state(&config.secret_key_base, SSO_STATE_PURPOSE, &verifier);
    let authorize_url = match sso::authorize_url(
        &metadata,
        sso_config,
        &sso_state,
        &sso::nonce(&config.secret_key_base, &verifier),
        &sso::code_challenge(&verifier),
    ) {
        Ok(url) => url,
        Err(err) => {
            tracing::error!(%err, "SSO authorization URL is invalid");
            return Redirect::to(&format!("{base}/login?error={SSO_UNAVAILABLE}")).into_response();
        }
    };
    Html(oauth_start_page(
        config,
        SSO_FLOW_STORAGE_KEY,
        SSO_FAILED,
        &authorize_url,
        &sso_state,
        &verifier,
    ))
    .into_response()
}

async fn sso_callback_page(
    State(state): State<AppState>,
    Query(query): Query<CallbackQuery>,
) -> Response {
    let config = state.config();
    if config.sso.is_none() {
        return Redirect::to(&format!("{}/login", config.base_path)).into_response();
    }
    Html(oauth_callback_page(
        config,
        SSO_FLOW_STORAGE_KEY,
        SSO_FAILED,
        query.code.as_deref(),
        query.state.as_deref(),
        query.iss.as_deref(),
    ))
    .into_response()
}

async fn sso_callback_finish(
    State(state): State<AppState>,
    headers: HeaderMap,
    Json(body): Json<CallbackFinishBody>,
) -> Response {
    let config = state.config();
    let base = &config.base_path;
    let failed = |status: StatusCode, code: &str| {
        oauth_finish_response(status, &format!("{base}/login?error={code}"), &[])
    };

    let Some(sso_config) = config.sso.as_ref() else {
        return failed(StatusCode::NOT_FOUND, SSO_FAILED);
    };
    if !has_finish_header(&headers) {
        return failed(StatusCode::FORBIDDEN, SSO_FAILED);
    }
    if !verify_oauth_state(
        &config.secret_key_base,
        SSO_STATE_PURPOSE,
        &body.state,
        &body.verifier,
    ) {
        tracing::error!("SSO state token is invalid, expired, or malformed");
        return failed(StatusCode::BAD_REQUEST, SSO_FAILED);
    }
    if let Some(iss) = body.iss.as_deref().filter(|iss| !iss.is_empty())
        && iss.trim_end_matches('/') != sso_config.issuer.trim_end_matches('/')
    {
        tracing::error!("SSO callback came from a different issuer");
        return failed(StatusCode::BAD_REQUEST, SSO_FAILED);
    }
    let Some(metadata) = state.sso_provider().await else {
        return failed(StatusCode::BAD_GATEWAY, SSO_UNAVAILABLE);
    };
    let id_token = match sso::exchange_code(
        state.sso_http_client(),
        &metadata,
        sso_config,
        &body.code,
        &body.verifier,
    )
    .await
    {
        Ok(id_token) => id_token,
        Err(err) => {
            tracing::error!(%err, "SSO token exchange failed");
            return failed(StatusCode::BAD_REQUEST, SSO_FAILED);
        }
    };
    let nonce = sso::nonce(&config.secret_key_base, &body.verifier);
    let signed_in = match sso::sign_in(state.http_client(), config, &id_token, &nonce).await {
        Ok(signed_in) => signed_in,
        Err(err) => {
            let code = sso_sign_in_error_code(&err);
            if code == "missing_admin_acl" {
                tracing::warn!(%err, "SSO sign-in refused by the API");
                return failed(StatusCode::FORBIDDEN, code);
            }
            tracing::error!(%err, "SSO sign-in through the API failed");
            return failed(StatusCode::BAD_GATEWAY, code);
        }
    };
    finish_admin_sign_in(&state, &signed_in.user_id, &signed_in.access_token).await
}

fn has_finish_header(headers: &HeaderMap) -> bool {
    headers
        .get(OAUTH_FINISH_HEADER)
        .and_then(|value| value.to_str().ok())
        == Some("1")
}

async fn oauth2_callback_page(
    State(state): State<AppState>,
    Query(query): Query<CallbackQuery>,
) -> Response {
    let config = state.config();
    if let Some(response) = sso_only_redirect(config) {
        return response;
    }
    Html(oauth_callback_page(
        config,
        OAUTH_FLOW_STORAGE_KEY,
        OAUTH_FAILED,
        query.code.as_deref(),
        query.state.as_deref(),
        None,
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

    if config.sso.as_ref().is_some_and(|sso| sso.only) {
        return oauth_finish_response(
            StatusCode::FORBIDDEN,
            &format!("{base}/login?error=sso_only"),
            &[],
        );
    }

    if !has_finish_header(&headers) {
        return oauth_finish_response(
            StatusCode::FORBIDDEN,
            &format!("{base}/login?error=oauth_failed"),
            &[],
        );
    }

    if !verify_oauth_state(
        &config.secret_key_base,
        OAUTH_STATE_PURPOSE,
        &body.state,
        &body.verifier,
    ) {
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

    finish_admin_sign_in(&state, &user.id, &token.access_token).await
}

async fn finish_admin_sign_in(state: &AppState, user_id: &str, access_token: &str) -> Response {
    let config = state.config();
    let base = &config.base_path;
    let temp_session = session::Session {
        user_id: user_id.to_owned(),
        access_token: access_token.to_owned(),
        created_at: 0,
    };
    let api_client = AdminApiClient::new(state.http_client(), config, &temp_session);
    if let Err(err) = api_client.get_current_admin().await {
        let code = admin_check_error_code(&err);
        let status = if code == "missing_admin_acl" {
            tracing::warn!(%err, %user_id, "Admin access check denied");
            StatusCode::FORBIDDEN
        } else {
            tracing::error!(%err, %user_id, "Admin access check failed");
            StatusCode::BAD_GATEWAY
        };
        return oauth_finish_response(status, &format!("{base}/login?error={code}"), &[]);
    }

    let session_cookie_value =
        session::create_session(user_id, access_token, &config.secret_key_base);
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

    let mut cookies = clear_cookie_values(SESSION_COOKIE_NAME, config);
    cookies.push(session_cookie);
    oauth_finish_response(StatusCode::OK, &format!("{base}/dashboard"), &cookies)
}

async fn logout(State(state): State<AppState>, headers: axum::http::HeaderMap) -> Response {
    let config = state.config();
    if !csrf::is_same_site_request(&headers, config.admin_origin().as_deref()) {
        return StatusCode::FORBIDDEN.into_response();
    }
    if let Some(session_value) = extract_cookie(&headers, SESSION_COOKIE_NAME)
        && let Some(session) = session::parse_session(&session_value, &config.secret_key_base)
    {
        let _ = oauth2::revoke_token(state.http_client(), config, &session.access_token).await;
    }
    redirect_with_cookies(
        &format!("{}/login", config.base_path),
        &clear_cookie_values(SESSION_COOKIE_NAME, config),
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
    storage_key: &str,
    failure_code: &str,
    authorize_url: &str,
    state: &str,
    verifier: &str,
) -> String {
    let storage_key = json_string(storage_key);
    let authorize_url = json_string(authorize_url);
    let login_url = json_string(&format!("{}/login?error={failure_code}", config.base_path));
    let state = json_string(state);
    let verifier = json_string(verifier);
    format!(
        r#"<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Signing in</title>
</head>
<body>
<p>Starting authentication...</p>
<script>
{HISTORY_CACHE_CLEAR_SCRIPT}
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

fn oauth_callback_page(
    config: &AdminConfig,
    storage_key: &str,
    failure_code: &str,
    code: Option<&str>,
    state: Option<&str>,
    iss: Option<&str>,
) -> String {
    let storage_key = json_string(storage_key);
    let login_url = json_string(&format!("{}/login?error={failure_code}", config.base_path));
    let code = json_string(code.unwrap_or_default());
    let state = json_string(state.unwrap_or_default());
    let iss = iss.map_or_else(|| "null".to_owned(), json_string);
    format!(
        r#"<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Signing in</title>
</head>
<body>
<p>Completing authentication...</p>
<script>
(function () {{
	var storageKey = {storage_key};
	var loginUrl = {login_url};
	var code = {code};
	var state = {state};
	var iss = {iss};
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
		body: JSON.stringify({{code: code, state: state, verifier: flow.verifier, iss: iss}})
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

fn generate_oauth_state(secret_key: &str, purpose: &str, verifier: &str) -> String {
    sign_oauth_state(
        secret_key,
        purpose,
        now_seconds(),
        random_state_nonce(),
        verifier,
    )
}

fn sign_oauth_state(
    secret_key: &str,
    purpose: &str,
    issued_at: u64,
    nonce: [u8; OAUTH_STATE_NONCE_BYTES],
    verifier: &str,
) -> String {
    let encoded_payload = encode_oauth_state_payload(issued_at, nonce, verifier);
    let signature = sign_oauth_state_payload(secret_key, purpose, &encoded_payload);
    format!("{encoded_payload}.{signature}")
}

fn verify_oauth_state(secret_key: &str, purpose: &str, state: &str, verifier: &str) -> bool {
    let Some((payload, signature)) = state.rsplit_once('.') else {
        return false;
    };
    if !constant_time_eq(
        signature.as_bytes(),
        sign_oauth_state_payload(secret_key, purpose, payload).as_bytes(),
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

fn sign_oauth_state_payload(secret_key: &str, purpose: &str, payload: &str) -> String {
    let mut mac =
        HmacSha256::new_from_slice(secret_key.as_bytes()).expect("HMAC can take key of any size");
    mac.update(format!("fluxer-admin-{purpose}-state:").as_bytes());
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

fn admin_check_error_code(err: &ApiError) -> &'static str {
    match err {
        ApiError::Http {
            status: 401 | 403, ..
        } => "missing_admin_acl",
        ApiError::Http {
            status: 404 | 405, ..
        }
        | ApiError::Parse(_) => "admin_api_mismatch",
        ApiError::Network(_) => "admin_api_unreachable",
        ApiError::Http { .. } => "admin_check_failed",
    }
}

fn sso_sign_in_error_code(err: &ApiError) -> &'static str {
    match err {
        ApiError::Http {
            status: 403,
            message,
            ..
        } if message.contains("FEATURE_TEMPORARILY_DISABLED") => SSO_FAILED,
        ApiError::Http { status: 403, .. } => "missing_admin_acl",
        ApiError::Http {
            status: 400 | 401, ..
        } => SSO_FAILED,
        ApiError::Http { status: 503, .. } => SSO_UNAVAILABLE,
        ApiError::Http {
            status: 404 | 405, ..
        }
        | ApiError::Parse(_) => "admin_api_mismatch",
        ApiError::Network(_) => "admin_api_unreachable",
        ApiError::Http { .. } => "admin_check_failed",
    }
}

fn sso_only_redirect(config: &crate::config::AdminConfig) -> Option<Response> {
    config.sso.as_ref().filter(|sso| sso.only).map(|_| {
        Redirect::to(&format!("{}/login?error=sso_only", config.base_path)).into_response()
    })
}

fn login_error_message(code: &str) -> &'static str {
    match code {
        "sso_only" => "Signing in with your Fluxer account is turned off here. Use single sign-on.",
        "oauth_failed" => "Authentication failed. Please try again.",
        "sso_failed" => "Single sign-on failed. Try again.",
        "sso_unavailable" => "Could not reach the sign-on provider. Try again in a moment.",
        "missing_admin_acl" => {
            "Access denied: missing admin:authenticate permission. \
             Ask an administrator to grant access."
        }
        "admin_api_mismatch" => {
            "Could not verify admin access because the API response did not match this admin panel. \
             Make sure the admin and API services run the same release."
        }
        "admin_api_unreachable" => {
            "Could not connect to the API to verify admin access. \
             Check that the admin service can connect to the API and try again."
        }
        "admin_check_failed" => {
            "Could not verify admin access. \
             Try again in a moment and check the admin service logs if it keeps failing."
        }
        _ => "Login error. Please try again.",
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn state_signed_for_one_flow_fails_for_the_other() {
        let secret = "test-secret";
        let verifier = random_nonce();
        let oauth_state = generate_oauth_state(secret, OAUTH_STATE_PURPOSE, &verifier);
        let sso_state = generate_oauth_state(secret, SSO_STATE_PURPOSE, &verifier);
        assert!(verify_oauth_state(
            secret,
            OAUTH_STATE_PURPOSE,
            &oauth_state,
            &verifier
        ));
        assert!(verify_oauth_state(
            secret,
            SSO_STATE_PURPOSE,
            &sso_state,
            &verifier
        ));
        assert!(!verify_oauth_state(
            secret,
            SSO_STATE_PURPOSE,
            &oauth_state,
            &verifier
        ));
        assert!(!verify_oauth_state(
            secret,
            OAUTH_STATE_PURPOSE,
            &sso_state,
            &verifier
        ));
    }
}
