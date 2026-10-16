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
    config::{AdminConfig, ProxyConfig},
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
async fn unban_sends_the_internal_note_as_a_header_and_no_body() {
    let uri = format!("/users/{USER_ID}?action=unban&tab=moderation");
    let captured = submit(&uri, "internal_note=Private%20staff%20note").await;
    assert_eq!(captured.route, format!("DELETE /admin/users/{USER_ID}/ban"));
    assert_eq!(
        captured.audit_log_reason.as_deref(),
        Some("Private staff note")
    );
    assert!(!captured.body.to_string().contains("Private staff note"));
}

const DECISION_FIELDS: &str = "reason_code=3&source=own_review&automation=none&statement=You%20sent%20the%20same%20link%20to%20many%20people.";

#[tokio::test]
async fn close_sends_the_basis_and_never_a_notify_flag() {
    let uri = format!("/users/{USER_ID}?action=close&tab=moderation");
    let holder = submit(
        &uri,
        "basis=holder_request&days_until_deletion=30&internal_note=Asked%20by%20email",
    )
    .await;
    assert_eq!(holder.route, format!("PUT /admin/users/{USER_ID}/deletion"));
    assert_eq!(
        holder.body,
        json!({"basis": {"type": "holder_request"}, "days_until_deletion": 30})
    );
    assert_eq!(holder.audit_log_reason.as_deref(), Some("Asked by email"));
    let enforcement = submit(
        &uri,
        &format!(
            "basis=enforcement&days_until_deletion=74&{DECISION_FIELDS}&internal_note=Spam%20review&remove_evidence=true"
        ),
    )
    .await;
    assert_eq!(enforcement.body["basis"]["type"], json!("enforcement"));
    assert_eq!(
        enforcement.body["basis"]["decision"]["reason_code"],
        json!(3)
    );
    assert_eq!(enforcement.body["remove_evidence"], json!(true));
    assert_eq!(enforcement.audit_log_reason.as_deref(), Some("Spam review"));
    assert!(enforcement.body.get("notify_user").is_none());
}

#[tokio::test]
async fn cancel_deletion_sends_only_the_pending_deletion() {
    let uri = format!("/users/{USER_ID}?action=cancel_deletion&tab=moderation");
    let captured = submit(
        &uri,
        "expected_pending_deletion_at=2026-12-01T00%3A00%3A00.000Z&confirm=true&internal_note=Asked%20to%20keep%20it",
    )
    .await;
    assert_eq!(
        captured.route,
        format!("DELETE /admin/users/{USER_ID}/deletion")
    );
    assert_eq!(
        captured.body,
        json!({"expected_pending_deletion_at": "2026-12-01T00:00:00.000Z"})
    );
    assert_eq!(
        captured.audit_log_reason.as_deref(),
        Some("Asked to keep it")
    );
}

#[tokio::test]
async fn limit_features_sends_one_limit_with_its_end() {
    let uri = format!("/users/{USER_ID}?action=limit_features&tab=moderation");
    let captured = submit(
        &uri,
        "limit=reports&ends_on=2026-12-01&reason_code=29&source=own_review&automation=none&statement=False%20reports&internal_note=Report%20review",
    )
    .await;
    assert_eq!(
        captured.route,
        format!("PUT /admin/users/{USER_ID}/feature-limits")
    );
    assert_eq!(captured.body["limit"], json!("reports"));
    assert_eq!(captured.body["ends_at"], json!("2026-12-01T00:00:00Z"));
    assert_eq!(captured.body["decision"]["reason_code"], json!(29));
    let purchases = submit(
        &uri,
        "limit=purchases&ends_on=2026-12-01&reason_code=14&source=own_review&automation=none&statement=Disputes&internal_note=Billing",
    )
    .await;
    assert!(purchases.body.get("ends_at").is_none());
}

#[tokio::test]
async fn bulk_close_sends_one_bulk_decision() {
    let captured = submit(
        "/bulk-actions?action=bulk-schedule-user-deletion",
        &format!(
            "user_ids={USER_ID}&basis=enforcement&days_until_deletion=74&reason_code=3&automation=none&statement=Same%20invite%20link&internal_note=Spam%20sweep"
        ),
    )
    .await;
    assert_eq!(captured.route, "POST /admin/bulk-jobs");
    assert_eq!(captured.body["task"], json!("schedule_user_deletion"));
    assert_eq!(
        captured.body["basis"],
        json!({
            "type": "enforcement",
            "decision": {
                "reason_code": 3,
                "source": "own_review",
                "automation": "none",
                "statement": "Same invite link"
            }
        })
    );
    assert_eq!(captured.audit_log_reason.as_deref(), Some("Spam sweep"));
}

#[tokio::test]
async fn report_resolve_sends_notify_reporter_from_the_checkbox() {
    let uri = format!("/reports/{REPORT_ID}/resolve");
    let checked = submit(
        &uri,
        "resolution=actioned&outcome=removed&public_comment=Handled&notify_reporter_present=1&notify_reporter=true",
    )
    .await;
    assert_eq!(checked.route, format!("PATCH /admin/reports/{REPORT_ID}"));
    assert_eq!(checked.body["public_comment"], json!("Handled"));
    assert_eq!(checked.body["resolution"], json!("actioned"));
    assert_eq!(checked.body["outcome"], json!("removed"));
    assert_eq!(checked.body["notify_reporter"], json!(true));
    let unchecked = submit(
        &uri,
        "resolution=no_violation&public_comment=Handled&notify_reporter_present=1",
    )
    .await;
    assert_eq!(unchecked.body["notify_reporter"], json!(false));
    assert_eq!(unchecked.body["outcome"], json!("no_violation"));
    let duplicate = submit(
        &uri,
        "resolution=duplicate&outcome=restricted&duplicate_of=1400000000000000001&public_comment=Handled",
    )
    .await;
    assert_eq!(duplicate.body["notify_reporter"], json!(true));
    assert!(duplicate.body.get("outcome").is_none());
    assert_eq!(duplicate.body["duplicate_of"], json!("1400000000000000001"));
    let warned = submit(&uri, "resolution=actioned&outcome=warned").await;
    assert_eq!(warned.body["outcome"], json!("warned"));
    let other_action = submit(&uri, "resolution=actioned&outcome=action_taken").await;
    assert_eq!(other_action.body["outcome"], json!("action_taken"));
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
