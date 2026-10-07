// SPDX-License-Identifier: AGPL-3.0-or-later

#![recursion_limit = "256"]

use axum::{
    Json, Router,
    body::{Body, to_bytes},
    extract::State,
    http::{HeaderMap, Method, Request, StatusCode, Uri, header},
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

const SECRET_KEY: &str = "notification-writes-test-secret";
const USER_ID: &str = "1500000000000000001";
const REPORT_ID: &str = "1600000000000000001";

#[derive(Clone)]
struct CapturedRequest {
    route: String,
    audit_log_reason: Option<String>,
    body: Value,
}

type CapturedRequests = Arc<Mutex<Vec<CapturedRequest>>>;

async fn submit(uri: &str, fields: &str) -> CapturedRequest {
    let app = setup().await;
    let csrf_token = csrf_token(&app).await;
    let status = post_form(&app, uri, &format!("_csrf={csrf_token}&{fields}")).await;
    assert_eq!(status, StatusCode::SEE_OTHER);
    let captured = app.captured.lock().expect("captured requests");
    assert_eq!(captured.len(), 1, "expected one write request");
    captured[0].clone()
}

#[tokio::test]
async fn temp_ban_sends_notify_user_from_the_checkbox() {
    let uri = format!("/users/{USER_ID}?action=temp_ban&tab=moderation");
    let checked = submit(
        &uri,
        "duration=24&reason=Spam&notify_user_present=1&notify_user=true",
    )
    .await;
    assert_eq!(checked.route, format!("PUT /admin/users/{USER_ID}/ban"));
    assert_eq!(checked.body["notify_user"], json!(true));
    let unchecked = submit(&uri, "duration=24&reason=Spam&notify_user_present=1").await;
    assert_eq!(unchecked.body["notify_user"], json!(false));
    let stale_form = submit(&uri, "duration=24&reason=Spam").await;
    assert_eq!(stale_form.body["notify_user"], json!(true));
}

#[tokio::test]
async fn unban_sends_the_public_reason_in_the_body_and_the_private_reason_as_a_header() {
    let uri = format!("/users/{USER_ID}?action=unban&tab=moderation");
    let checked = submit(
        &uri,
        "public_reason=Appeal%20accepted&private_reason=Private%20staff%20note&notify_user_present=1&notify_user=true",
    )
    .await;
    assert_eq!(checked.route, format!("DELETE /admin/users/{USER_ID}/ban"));
    assert_eq!(checked.body["notify_user"], json!(true));
    assert_eq!(checked.body["public_reason"], json!("Appeal accepted"));
    assert_eq!(
        checked.audit_log_reason.as_deref(),
        Some("Private staff note")
    );
    assert!(!checked.body.to_string().contains("Private staff note"));
    let unchecked = submit(
        &uri,
        "private_reason=Private%20staff%20note&notify_user_present=1",
    )
    .await;
    assert_eq!(unchecked.body["notify_user"], json!(false));
    assert!(!unchecked.body.to_string().contains("Private staff note"));
    let stale_form = submit(&uri, "").await;
    assert_eq!(stale_form.body["notify_user"], json!(true));
}

#[tokio::test]
async fn schedule_deletion_sends_notify_user_from_the_checkbox() {
    let uri = format!("/users/{USER_ID}?action=schedule_deletion&tab=moderation");
    let checked = submit(
        &uri,
        "reason_code=3&days_until_deletion=60&notify_user_present=1&notify_user=true",
    )
    .await;
    assert_eq!(
        checked.route,
        format!("PUT /admin/users/{USER_ID}/deletion")
    );
    assert_eq!(checked.body["notify_user"], json!(true));
    let unchecked = submit(
        &uri,
        "reason_code=3&days_until_deletion=60&notify_user_present=1",
    )
    .await;
    assert_eq!(unchecked.body["notify_user"], json!(false));
    let stale_form = submit(&uri, "reason_code=3&days_until_deletion=60").await;
    assert_eq!(stale_form.body["notify_user"], json!(true));
}

#[tokio::test]
async fn bulk_schedule_deletion_sends_notify_user_from_the_checkbox() {
    let uri = "/bulk-actions?action=bulk-schedule-user-deletion";
    let fields = format!("user_ids={USER_ID}&reason_code=3&days_until_deletion=60");
    let checked = submit(
        uri,
        &format!("{fields}&notify_user_present=1&notify_user=true"),
    )
    .await;
    assert_eq!(checked.route, "POST /admin/bulk-jobs");
    assert_eq!(checked.body["task"], json!("schedule_user_deletion"));
    assert_eq!(checked.body["notify_user"], json!(true));
    let unchecked = submit(uri, &format!("{fields}&notify_user_present=1")).await;
    assert_eq!(unchecked.body["notify_user"], json!(false));
    let stale_form = submit(uri, &fields).await;
    assert_eq!(stale_form.body["notify_user"], json!(true));
}

#[tokio::test]
async fn report_resolve_sends_notify_reporter_from_the_checkbox() {
    let uri = format!("/reports/{REPORT_ID}/resolve");
    let checked = submit(
        &uri,
        "resolution=actioned&public_comment=Handled&notify_reporter_present=1&notify_reporter=true",
    )
    .await;
    assert_eq!(checked.route, format!("PATCH /admin/reports/{REPORT_ID}"));
    assert_eq!(checked.body["public_comment"], json!("Handled"));
    assert_eq!(checked.body["resolution"], json!("actioned"));
    assert_eq!(checked.body["notify_reporter"], json!(true));
    let unchecked = submit(
        &uri,
        "resolution=no_violation&public_comment=Handled&notify_reporter_present=1",
    )
    .await;
    assert_eq!(unchecked.body["notify_reporter"], json!(false));
    let stale_form = submit(&uri, "resolution=duplicate&public_comment=Handled").await;
    assert_eq!(stale_form.body["notify_reporter"], json!(true));
}

struct TestApp {
    router: Router,
    session_cookie: String,
    captured: CapturedRequests,
}

async fn setup() -> TestApp {
    let captured: CapturedRequests = Arc::new(Mutex::new(Vec::new()));
    let api_endpoint = spawn_mock_api(Arc::clone(&captured)).await;
    let router = build_router(test_config(api_endpoint));
    let session_value = session::create_session("1500000000000000000", "test-token", SECRET_KEY);
    TestApp {
        router,
        session_cookie: format!("{}={session_value}", session::SESSION_COOKIE_NAME),
        captured,
    }
}

async fn csrf_token(app: &TestApp) -> String {
    let response = app
        .router
        .clone()
        .oneshot(
            Request::builder()
                .method(Method::GET)
                .uri("/voice-regions")
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

async fn post_form(app: &TestApp, uri: &str, body: &str) -> StatusCode {
    let csrf = body
        .split('&')
        .find_map(|pair| pair.strip_prefix("_csrf="))
        .expect("form has a csrf token");
    let response = app
        .router
        .clone()
        .oneshot(
            Request::builder()
                .method(Method::POST)
                .uri(uri)
                .header(header::CONTENT_TYPE, "application/x-www-form-urlencoded")
                .header(
                    header::COOKIE,
                    format!("{}; __Host-csrf_token={csrf}", app.session_cookie),
                )
                .body(Body::from(body.to_owned()))
                .unwrap(),
        )
        .await
        .unwrap();
    response.status()
}

async fn spawn_mock_api(captured: CapturedRequests) -> String {
    let listener = TcpListener::bind(("127.0.0.1", 0)).await.unwrap();
    let addr = listener.local_addr().unwrap();
    tokio::spawn(async move {
        axum::serve(
            listener,
            Router::new().fallback(mock_api).with_state(captured),
        )
        .await
        .unwrap();
    });
    format!("http://{addr}")
}

async fn mock_api(
    State(captured): State<CapturedRequests>,
    method: Method,
    uri: Uri,
    headers: HeaderMap,
    request: Request<Body>,
) -> Response {
    let path = uri.path().to_owned();
    if method != Method::GET {
        let bytes = to_bytes(request.into_body(), usize::MAX).await.unwrap();
        let body: Value = serde_json::from_slice(&bytes).unwrap_or(Value::Null);
        let audit_log_reason = headers
            .get("x-audit-log-reason")
            .and_then(|value| value.to_str().ok())
            .map(ToOwned::to_owned);
        captured
            .lock()
            .expect("captured requests")
            .push(CapturedRequest {
                route: format!("{method} {path}"),
                audit_log_reason,
                body,
            });
    }
    match (method, path.as_str()) {
        (Method::GET, "/admin/users/@me") => Json(json!({ "user": admin_user() })).into_response(),
        (Method::GET, "/admin/voice/regions") => {
            Json(json!({ "regions": [region()] })).into_response()
        }
        (Method::PUT | Method::DELETE, _) if path.starts_with("/admin/users/") => {
            Json(json!({ "user": admin_user() })).into_response()
        }
        (Method::POST, "/admin/bulk-jobs") => Json(json!({ "job_id": "1" })).into_response(),
        (Method::PATCH, _) if path.starts_with("/admin/reports/") => Json(json!({
            "report_id": REPORT_ID,
            "status": 1,
            "resolved_at": null,
            "public_comment": null
        }))
        .into_response(),
        _ => (
            StatusCode::NOT_FOUND,
            Json(json!({ "message": "not found" })),
        )
            .into_response(),
    }
}

fn region() -> Value {
    json!({
        "id": "europe-north",
        "name": "Northern Europe",
        "emoji": "flag",
        "latitude": 59.33,
        "longitude": 18.06,
        "is_default": true,
        "vip_only": false,
        "required_guild_features": [],
        "allowed_guild_ids": [],
        "allowed_user_ids": [],
        "created_at": null,
        "updated_at": null
    })
}

fn admin_user() -> Value {
    json!({
        "id": "1500000000000000000",
        "username": "AdminUser",
        "discriminator": 1,
        "avatar": null,
        "banner": null,
        "email": "admin@example.com",
        "email_verified": true,
        "email_bounced": false,
        "global_name": "AdminUser",
        "bio": null,
        "pronouns": null,
        "accent_color": null,
        "date_of_birth": null,
        "locale": "en-US",
        "acls": ["*"],
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

fn test_config(api_endpoint: String) -> AdminConfig {
    AdminConfig {
        env: RuntimeEnv::Test,
        host: "127.0.0.1".to_owned(),
        port: 0,
        secret_key_base: SECRET_KEY.to_owned(),
        base_path: String::new(),
        api_endpoint,
        media_endpoint: "https://media.example.test".to_owned(),
        static_cdn_endpoint: "https://static.example.test".to_owned(),
        reports_bucket_origin: "https://reports.example.test".to_owned(),
        admin_endpoint: "https://admin.example.test".to_owned(),
        web_app_endpoint: "https://app.example.test".to_owned(),
        oauth_client_id: "admin-client".to_owned(),
        oauth_client_secret: "admin-secret".to_owned(),
        oauth_redirect_uri: "https://admin.example.test/callback".to_owned(),
        build_version: "test".to_owned(),
        self_hosted: false,
        proxy: ProxyConfig {
            trust_client_ip_header: false,
            client_ip_header_name: "x-forwarded-for".to_owned(),
        },
    }
}
