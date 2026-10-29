// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::client::{AdminApiClient, ApiError},
    config::AdminConfig,
    oauth2,
    session::{
        self, SESSION_COOKIE_NAME, SESSION_MAX_AGE, admin_cookie_path, append_cookies,
        clear_cookie_values,
    },
    state::AppState,
    templates::pages::login,
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
    let error_msg = query.error.as_deref().map(login_error_message);
    let branding = state.self_hosted_branding().await;
    Html(login::login_page(config, branding.as_ref(), error_msg).into_string()).into_response()
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
    if let Err(err) = api_client.get_current_admin().await {
        let code = admin_check_error_code(&err);
        let status = if code == "missing_admin_acl" {
            tracing::warn!(%err, user_id = %user.id, "Admin access check denied");
            StatusCode::FORBIDDEN
        } else {
            tracing::error!(%err, user_id = %user.id, "Admin access check failed");
            StatusCode::BAD_GATEWAY
        };
        return oauth_finish_response(status, &format!("{base}/login?error={code}"), &[]);
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

    let mut cookies = clear_cookie_values(SESSION_COOKIE_NAME, config);
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
<title>Signing in</title>
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

fn login_error_message(code: &str) -> &'static str {
    match code {
        "oauth_failed" => "Authentication failed. Please try again.",
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
    fn oauth_state_survives_ten_minutes_on_the_consent_page() {
        let verifier = "verifier";
        let issued = |age: u64| sign_oauth_state("secret", now_seconds() - age, [7; 16], verifier);
        assert!(verify_oauth_state("secret", &issued(10 * 60), verifier));
        assert!(!verify_oauth_state(
            "secret",
            &issued(OAUTH_STATE_MAX_AGE_SECONDS + 60),
            verifier
        ));
    }

    const TEST_SECRET: &str = "test-secret";

    fn wildcard_admin_me_body() -> serde_json::Value {
        serde_json::from_str(
            r#"{
            "user": {
                "id": "100",
                "username": "owner",
                "discriminator": 1,
                "global_name": null,
                "bot": false,
                "system": false,
                "flags": "0",
                "premium_flags": 0,
                "avatar": null,
                "banner": null,
                "bio": null,
                "pronouns": null,
                "accent_color": null,
                "email": "owner@example.com",
                "email_verified": true,
                "email_bounced": false,
                "date_of_birth": null,
                "locale": null,
                "premium_type": null,
                "premium_since": null,
                "premium_until": null,
                "premium_grace_ends_at": null,
                "premium_lifetime_sequence": null,
                "profile_badges": [],
                "temp_banned_until": null,
                "pending_deletion_at": null,
                "pending_bulk_message_deletion_at": null,
                "deletion_reason_code": null,
                "deletion_public_reason": null,
                "deletion_audit_log_reason": null,
                "deletion_scheduled_by": null,
                "deletion_scheduled_at": null,
                "enforcement_decision_id": null,
                "acls": ["*"],
                "traits": [],
                "badge_ids": [],
                "has_totp": false,
                "has_password": true,
                "claimed": true,
                "account_tier": "verified",
                "date_of_birth_required": false,
                "authenticator_types": [],
                "last_active_at": null,
                "last_active_ip": null,
                "last_active_ip_reverse": null,
                "last_active_location": null
            },
            "pending_registration_count": null
        }"#,
        )
        .unwrap()
    }

    async fn finish_login(
        admin_me_status: u16,
        admin_me_body: serde_json::Value,
    ) -> (StatusCode, String) {
        let admin_me_status = StatusCode::from_u16(admin_me_status).unwrap();
        let api = Router::new()
            .route(
                "/oauth2/token",
                post(|| async { Json(serde_json::json!({"access_token": "token"})) }),
            )
            .route(
                "/users/@me",
                get(|| async { Json(serde_json::json!({"id": "100"})) }),
            )
            .route(
                "/admin/users/@me",
                get(move || async move { (admin_me_status, Json(admin_me_body)) }),
            );
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
        let api_endpoint = format!("http://{}", listener.local_addr().unwrap());
        tokio::spawn(async move { axum::serve(listener, api).await.unwrap() });

        let state = AppState::new(AdminConfig {
            host: "127.0.0.1".into(),
            port: 0,
            secret_key_base: TEST_SECRET.into(),
            base_path: String::new(),
            api_endpoint: api_endpoint.clone(),
            api_public_endpoint: api_endpoint,
            media_endpoint: String::new(),
            static_cdn_endpoint: String::new(),
            reports_bucket_origin: String::new(),
            admin_endpoint: "http://admin.test".into(),
            web_app_endpoint: "http://app.test".into(),
            oauth_client_id: "1234567890123456789".into(),
            oauth_client_secret: "client-secret".into(),
            oauth_redirect_uri: "http://admin.test/oauth2_callback".into(),
            build_version: "test".into(),
            self_hosted: true,
            proxy: crate::config::ProxyConfig {
                trust_client_ip_header: false,
                client_ip_header_name: "x-forwarded-for".into(),
            },
        });
        let mut headers = HeaderMap::new();
        headers.insert(OAUTH_FINISH_HEADER, "1".parse().unwrap());
        let verifier = "verifier".to_owned();
        let body = CallbackFinishBody {
            code: "code".into(),
            state: generate_oauth_state(TEST_SECRET, &verifier),
            verifier,
        };
        let response = oauth2_callback_finish(State(state), headers, Json(body)).await;
        let status = response.status();
        let bytes = axum::body::to_bytes(response.into_body(), usize::MAX)
            .await
            .unwrap();
        let body: serde_json::Value = serde_json::from_slice(&bytes).unwrap();
        (status, body["redirect_to"].as_str().unwrap().to_owned())
    }

    #[tokio::test]
    async fn wildcard_admin_signs_in_when_the_api_matches() {
        let (status, redirect_to) = finish_login(200, wildcard_admin_me_body()).await;
        assert_eq!(status, StatusCode::OK);
        assert_eq!(redirect_to, "/dashboard");
    }

    #[tokio::test]
    async fn account_without_admin_acl_is_told_it_lacks_the_acl() {
        let (status, redirect_to) = finish_login(
            403,
            serde_json::json!({"code": "MISSING_PERMISSIONS", "message": "Missing permissions"}),
        )
        .await;
        assert_eq!(status, StatusCode::FORBIDDEN);
        assert_eq!(redirect_to, "/login?error=missing_admin_acl");
    }

    #[tokio::test]
    async fn wildcard_admin_on_an_older_api_is_not_told_it_lacks_the_acl() {
        let mut body = wildcard_admin_me_body();
        body["user"].as_object_mut().unwrap().remove("badge_ids");
        let (status, redirect_to) = finish_login(200, body).await;
        assert_eq!(status, StatusCode::BAD_GATEWAY);
        assert_eq!(redirect_to, "/login?error=admin_api_mismatch");
    }

    #[tokio::test]
    async fn api_without_the_current_admin_route_is_not_reported_as_missing_acl() {
        let (status, redirect_to) = finish_login(
            404,
            serde_json::json!({"code": "NOT_FOUND", "message": "Not found"}),
        )
        .await;
        assert_eq!(status, StatusCode::BAD_GATEWAY);
        assert_eq!(redirect_to, "/login?error=admin_api_mismatch");
    }

    #[tokio::test]
    async fn rate_limited_admin_check_is_not_reported_as_missing_acl() {
        let (status, redirect_to) = finish_login(
            429,
            serde_json::json!({"code": "RATE_LIMITED", "message": "Too many requests"}),
        )
        .await;
        assert_eq!(status, StatusCode::BAD_GATEWAY);
        assert_eq!(redirect_to, "/login?error=admin_check_failed");
    }

    #[test]
    fn transport_failures_point_at_connectivity() {
        let err = ApiError::Network("connection refused".into());
        assert_eq!(admin_check_error_code(&err), "admin_api_unreachable");
    }

    #[test]
    fn each_admin_check_failure_has_its_own_message() {
        let messages = [
            "missing_admin_acl",
            "admin_api_mismatch",
            "admin_api_unreachable",
            "admin_check_failed",
        ]
        .map(login_error_message);
        for (index, message) in messages.iter().enumerate() {
            assert_ne!(*message, login_error_message("unknown"));
            assert!(!messages[index + 1..].contains(message), "{message}");
        }
    }
}
