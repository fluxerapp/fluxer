// SPDX-License-Identifier: AGPL-3.0-or-later

#![recursion_limit = "256"]

use axum::{
    Json, Router,
    body::{Body, to_bytes},
    extract::State,
    http::{Method, Request, StatusCode, Uri, header},
    response::{IntoResponse, Response},
};
use fluxer_admin::{
    build_router,
    config::{AdminConfig, ProxyConfig, RuntimeEnv},
    session,
};
use serde_json::{Value, json};
use std::sync::{Arc, Mutex};
use tokio::net::TcpListener;
use tower::ServiceExt;

const SECRET_KEY: &str = "password-reset-link-test-secret";
const ADMIN_ID: &str = "1500000000000000000";
const TARGET_ID: &str = "1500000000000000042";
const RESET_URL: &str = "https://chat.example.test/reset#token=one-time-reset-token";

#[derive(Clone)]
struct MockApi {
    account_identity: &'static str,
    admin_acls: Vec<&'static str>,
    requests: Arc<Mutex<Vec<String>>>,
}

#[tokio::test]
async fn creating_a_reset_link_shows_the_url_once_with_a_copy_button() {
    let app = setup(true, "username", vec!["*"]).await;
    let csrf_token = csrf_token(&app).await;
    let (status, body) = post_form(
        &app,
        &format!("/users/{TARGET_ID}?action=create_password_reset_link&tab=account"),
        &format!("_csrf={csrf_token}"),
    )
    .await;
    assert_eq!(status, StatusCode::OK);
    assert!(app.saw(&format!(
        "POST /admin/users/{TARGET_ID}/password-reset-link"
    )));
    assert!(body.contains("Copy this link now. It is shown only once."));
    assert!(body.contains(&format!(r#"value="{RESET_URL}""#)));
    assert!(body.contains(&format!(r#"data-copy-value="{RESET_URL}""#)));
    assert!(body.contains("Copy Link"));

    let page = get(&app, &format!("/users/{TARGET_ID}?tab=account")).await;
    assert!(page.contains("Create Password Reset Link"));
    assert!(!page.contains(RESET_URL));
}

#[tokio::test]
async fn an_htmx_reset_link_request_gets_only_the_result_fragment() {
    let app = setup(true, "username", vec!["*"]).await;
    let page = get(&app, &format!("/users/{TARGET_ID}?tab=account")).await;
    assert!(page.contains(r##"hx-target="#password-reset-link-result""##));
    assert!(page.contains(r#"hx-push-url="false""#));
    let csrf_token = csrf_token(&app).await;
    let (status, body) = post_form_with_headers(
        &app,
        &format!("/users/{TARGET_ID}?action=create_password_reset_link&tab=account"),
        &format!("_csrf={csrf_token}"),
        &[
            ("HX-Request", "true"),
            ("HX-Target", "password-reset-link-result"),
        ],
    )
    .await;
    assert_eq!(status, StatusCode::OK);
    assert!(
        body.starts_with(r#"<div id="password-reset-link-result""#),
        "{body}"
    );
    assert!(body.contains(r#"hx-history="false""#));
    assert!(body.contains(&format!(r#"data-copy-value="{RESET_URL}""#)));
    assert!(!body.contains("<html"));
    assert!(!body.contains("Create Password Reset Link"));
}

#[tokio::test]
async fn revoking_a_recovery_kit_calls_the_api() {
    let app = setup(true, "username", vec!["*"]).await;
    let page = get(&app, &format!("/users/{TARGET_ID}?tab=account")).await;
    assert!(page.contains("Revoke Recovery Kit"));
    let csrf_token = csrf_token(&app).await;
    let (status, _) = post_form(
        &app,
        &format!("/users/{TARGET_ID}?action=revoke_recovery_kit&tab=account"),
        &format!("_csrf={csrf_token}"),
    )
    .await;
    assert!(status.is_redirection() || status.is_success(), "{status}");
    assert!(app.saw(&format!("DELETE /admin/users/{TARGET_ID}/recovery-kit")));
}

#[tokio::test]
async fn the_revoke_recovery_kit_action_needs_its_acl_and_a_username_instance() {
    let without_acl = setup(
        true,
        "username",
        vec![
            "admin:authenticate",
            "user:lookup",
            "user:create:password_reset_link",
        ],
    )
    .await;
    let page = get(&without_acl, &format!("/users/{TARGET_ID}?tab=account")).await;
    assert!(page.contains("Create Password Reset Link"));
    assert!(!page.contains("Revoke Recovery Kit"));

    let with_acl = setup(
        true,
        "username",
        vec![
            "admin:authenticate",
            "user:lookup",
            "user:delete:recovery_kit",
        ],
    )
    .await;
    let page = get(&with_acl, &format!("/users/{TARGET_ID}?tab=account")).await;
    assert!(page.contains("Revoke Recovery Kit"));

    let email = setup(true, "email", vec!["*"]).await;
    let page = get(&email, &format!("/users/{TARGET_ID}?tab=account")).await;
    assert!(!page.contains("Revoke Recovery Kit"));
}

#[tokio::test]
async fn username_instances_hide_email_actions_on_the_account_tab() {
    let app = setup(true, "username", vec!["*"]).await;
    let page = get(&app, &format!("/users/{TARGET_ID}?tab=account")).await;
    assert!(page.contains("Create Password Reset Link"));
    assert!(!page.contains("Send Password Reset"));
    assert!(!page.contains("Change Email"));
    assert!(!page.contains("Verify Email"));
}

#[tokio::test]
async fn the_reset_link_action_needs_its_acl() {
    let app = setup(true, "username", vec!["admin:authenticate", "user:lookup"]).await;
    let page = get(&app, &format!("/users/{TARGET_ID}?tab=account")).await;
    assert!(page.contains("Terminate All Sessions"));
    assert!(!page.contains("Create Password Reset Link"));
    assert!(!page.contains("Send Password Reset"));
}

#[tokio::test]
async fn email_instances_keep_the_email_actions() {
    let app = setup(true, "email", vec!["*"]).await;
    let page = get(&app, &format!("/users/{TARGET_ID}?tab=account")).await;
    assert!(page.contains("Send Password Reset"));
    assert!(page.contains("Change Email"));
    assert!(page.contains("Verify Email"));
    assert!(!page.contains("Create Password Reset Link"));
}

#[tokio::test]
async fn the_email_ban_notice_stays_after_a_ban_action_on_a_username_instance() {
    let notice = "Accounts have no email address, so email bans have no effect.";
    let username = setup(true, "username", vec!["*"]).await;
    let username_csrf = csrf_token(&username).await;
    let (status, body) = post_form(
        &username,
        "/email-bans?action=ban",
        &format!("_csrf={username_csrf}&email="),
    )
    .await;
    assert_eq!(status, StatusCode::OK);
    assert!(body.contains("Value is required"));
    assert!(body.contains(notice));

    let email = setup(true, "email", vec!["*"]).await;
    let email_csrf = csrf_token(&email).await;
    let (_, body) = post_form(
        &email,
        "/email-bans?action=ban",
        &format!("_csrf={email_csrf}&email="),
    )
    .await;
    assert!(body.contains("Value is required"));
    assert!(!body.contains(notice));
}

#[tokio::test]
async fn hosted_admin_never_asks_discovery_for_the_sign_in_method() {
    let app = setup(false, "username", vec!["*"]).await;
    let page = get(&app, &format!("/users/{TARGET_ID}?tab=account")).await;
    assert!(page.contains("Send Password Reset"));
    assert!(!page.contains("Create Password Reset Link"));
    assert!(!app.saw("GET /.well-known/fluxer"));
}

struct TestApp {
    router: Router,
    session_cookie: String,
    requests: Arc<Mutex<Vec<String>>>,
}

impl TestApp {
    fn saw(&self, route: &str) -> bool {
        self.requests
            .lock()
            .expect("requests")
            .iter()
            .any(|seen| seen == route)
    }
}

async fn setup(
    self_hosted: bool,
    account_identity: &'static str,
    admin_acls: Vec<&'static str>,
) -> TestApp {
    let requests = Arc::new(Mutex::new(Vec::new()));
    let api_endpoint = spawn_mock_api(MockApi {
        account_identity,
        admin_acls,
        requests: Arc::clone(&requests),
    })
    .await;
    let router = build_router(test_config(api_endpoint, self_hosted));
    let session_value = session::create_session(ADMIN_ID, "test-token", SECRET_KEY);
    TestApp {
        router,
        session_cookie: format!("{}={session_value}", session::SESSION_COOKIE_NAME),
        requests,
    }
}

async fn get(app: &TestApp, uri: &str) -> String {
    let response = app
        .router
        .clone()
        .oneshot(
            Request::builder()
                .method(Method::GET)
                .uri(uri)
                .header(header::COOKIE, &app.session_cookie)
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(response.status(), StatusCode::OK, "{uri}");
    body_text(response).await
}

async fn csrf_token(app: &TestApp) -> String {
    let response = app
        .router
        .clone()
        .oneshot(
            Request::builder()
                .method(Method::GET)
                .uri(format!("/users/{TARGET_ID}?tab=account"))
                .header(header::COOKIE, &app.session_cookie)
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(response.status(), StatusCode::OK);
    response
        .headers()
        .get_all(header::SET_COOKIE)
        .iter()
        .filter_map(|value| value.to_str().ok())
        .find_map(|value| {
            let pair = value.split(';').next()?;
            let token = pair
                .strip_prefix("__Host-csrf_token=")
                .or_else(|| pair.strip_prefix("csrf_token="))?;
            (!token.is_empty()).then(|| token.to_owned())
        })
        .expect("csrf_token cookie")
}

async fn post_form(app: &TestApp, uri: &str, body: &str) -> (StatusCode, String) {
    post_form_with_headers(app, uri, body, &[]).await
}

async fn post_form_with_headers(
    app: &TestApp,
    uri: &str,
    body: &str,
    headers: &[(&str, &str)],
) -> (StatusCode, String) {
    let csrf = body
        .split('&')
        .find_map(|pair| pair.strip_prefix("_csrf="))
        .expect("form carries a csrf token");
    let mut request = Request::builder()
        .method(Method::POST)
        .uri(uri)
        .header(header::CONTENT_TYPE, "application/x-www-form-urlencoded")
        .header(
            header::COOKIE,
            format!("{}; __Host-csrf_token={csrf}", app.session_cookie),
        );
    for (name, value) in headers {
        request = request.header(*name, *value);
    }
    let response = app
        .router
        .clone()
        .oneshot(request.body(Body::from(body.to_owned())).unwrap())
        .await
        .unwrap();
    let status = response.status();
    (status, body_text(response).await)
}

async fn body_text(response: Response) -> String {
    let bytes = to_bytes(response.into_body(), usize::MAX).await.unwrap();
    String::from_utf8(bytes.to_vec()).unwrap()
}

async fn spawn_mock_api(mock: MockApi) -> String {
    let listener = TcpListener::bind(("127.0.0.1", 0)).await.unwrap();
    let addr = listener.local_addr().unwrap();
    tokio::spawn(async move {
        axum::serve(listener, Router::new().fallback(mock_api).with_state(mock))
            .await
            .unwrap();
    });
    format!("http://{addr}")
}

async fn mock_api(State(mock): State<MockApi>, method: Method, uri: Uri) -> Response {
    let path = uri.path().to_owned();
    mock.requests
        .lock()
        .expect("requests")
        .push(format!("{method} {path}"));
    let target_user = format!("/admin/users/{TARGET_ID}");
    let target_sessions = format!("{target_user}/sessions");
    let target_credentials = format!("{target_user}/webauthn-credentials");
    let target_reset_link = format!("{target_user}/password-reset-link");
    let target_recovery_kit = format!("{target_user}/recovery-kit");
    match (method, path.as_str()) {
        (Method::GET, "/admin/users/@me") => Json(json!({
            "user": user(ADMIN_ID, "AdminUser", &mock.admin_acls)
        }))
        .into_response(),
        (Method::GET, "/.well-known/fluxer") => Json(json!({
            "features": {
                "premium_enabled": false,
                "account_identity": mock.account_identity
            }
        }))
        .into_response(),
        (Method::GET, p) if p == target_user => Json(json!({
            "users": [user(TARGET_ID, "member", &[])]
        }))
        .into_response(),
        (Method::GET, p) if p == target_sessions => Json(json!({ "sessions": [] })).into_response(),
        (Method::GET, p) if p == target_credentials => Json(json!([])).into_response(),
        (Method::POST, p) if p == target_reset_link => Json(json!({
            "url": RESET_URL,
            "expires_at": "2026-10-01T13:00:00.000Z"
        }))
        .into_response(),
        (Method::DELETE, p) if p == target_recovery_kit => StatusCode::NO_CONTENT.into_response(),
        _ => (
            StatusCode::NOT_FOUND,
            Json(json!({ "message": "not found" })),
        )
            .into_response(),
    }
}

fn user(id: &str, username: &str, acls: &[&str]) -> Value {
    json!({
        "id": id,
        "username": username,
        "discriminator": 1,
        "avatar": null,
        "banner": null,
        "email": null,
        "email_verified": false,
        "email_bounced": false,
        "global_name": username,
        "bio": null,
        "pronouns": null,
        "accent_color": null,
        "date_of_birth": null,
        "locale": "en-GB",
        "acls": acls,
        "traits": [],
        "flags": "0",
        "premium_flags": 0,
        "bot": false,
        "system": false,
        "premium_type": null,
        "premium_since": null,
        "premium_until": null,
        "premium_grace_ends_at": null,
        "premium_lifetime_sequence": null,
        "has_totp": false,
        "authenticator_types": [],
        "temp_banned_until": null,
        "pending_deletion_at": null,
        "pending_bulk_message_deletion_at": null,
        "deletion_reason_code": null,
        "deletion_public_reason": null,
        "deletion_audit_log_reason": null,
        "deletion_scheduled_by": null,
        "deletion_scheduled_at": null,
        "last_active_at": null,
        "last_active_ip": null,
        "last_active_ip_reverse": null,
        "last_active_location": null
    })
}

fn test_config(api_endpoint: String, self_hosted: bool) -> AdminConfig {
    AdminConfig {
        env: RuntimeEnv::Test,
        host: "127.0.0.1".to_owned(),
        port: 0,
        secret_key_base: SECRET_KEY.to_owned(),
        base_path: String::new(),
        api_endpoint,
        media_endpoint: "https://media.example.test".to_owned(),
        static_cdn_endpoint: "https://static.example.test".to_owned(),
        reports_bucket_origin: String::new(),
        admin_endpoint: "https://admin.example.test".to_owned(),
        web_app_endpoint: "https://app.example.test".to_owned(),
        oauth_client_id: "admin-client".to_owned(),
        oauth_client_secret: "admin-secret".to_owned(),
        oauth_redirect_uri: "https://admin.example.test/callback".to_owned(),
        build_version: "test".to_owned(),
        self_hosted,
        proxy: ProxyConfig {
            trust_client_ip_header: false,
            client_ip_header_name: "x-forwarded-for".to_owned(),
        },
    }
}
