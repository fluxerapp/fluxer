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

const SECRET_KEY: &str = "enforcement-panel-test-secret";
const ADMIN_ID: &str = "1500000000000000000";
const USER_ID: &str = "1500000000000000001";
const DECISION_ID: &str = "1900000000000000001";
const REPORT_ID: &str = "1600000000000000001";
const CHANNEL_ID: &str = "1700000000000000001";
const MESSAGE_ID: &str = "1700000000000000002";
const SUSPENDED_UNTIL: &str = "2099-01-01T00:00:00.000Z";
const DISABLED: u64 = 1 << 38;
const NETWORK_IP: &str = "45.33.10.1";
const SHARED_IP: &str = "45.33.10.2";

#[derive(Clone)]
struct Captured {
    route: String,
    audit_log_reason: Option<String>,
    body: Value,
}

struct Mock {
    admin_acls: Vec<&'static str>,
    user: Value,
    decisions: Vec<Value>,
    decision: Value,
    audit_logs: Vec<Value>,
    captured: Mutex<Vec<Captured>>,
}

impl Mock {
    fn new(user: Value) -> Self {
        Self {
            admin_acls: vec!["*"],
            user,
            decisions: Vec::new(),
            decision: Value::Null,
            audit_logs: Vec::new(),
            captured: Mutex::new(Vec::new()),
        }
    }
}

struct TestApp {
    router: Router,
    session_cookie: String,
    mock: Arc<Mock>,
}

async fn setup(mock: Mock) -> TestApp {
    let mock = Arc::new(mock);
    let listener = TcpListener::bind(("127.0.0.1", 0)).await.unwrap();
    let addr = listener.local_addr().unwrap();
    let state = Arc::clone(&mock);
    tokio::spawn(async move {
        axum::serve(listener, Router::new().fallback(mock_api).with_state(state))
            .await
            .unwrap();
    });
    let router = build_router(test_config(format!("http://{addr}")));
    let session_value = session::create_session(ADMIN_ID, "test-token", SECRET_KEY);
    TestApp {
        router,
        session_cookie: format!("{}={session_value}", session::SESSION_COOKIE_NAME),
        mock,
    }
}

async fn get(app: &TestApp, uri: &str) -> (StatusCode, HeaderMap, String) {
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
    let status = response.status();
    let headers = response.headers().clone();
    let body = to_bytes(response.into_body(), usize::MAX).await.unwrap();
    (status, headers, String::from_utf8(body.to_vec()).unwrap())
}

async fn page(app: &TestApp, uri: &str) -> String {
    let (status, _, body) = get(app, uri).await;
    assert_eq!(status, StatusCode::OK, "{body}");
    body
}

async fn post(app: &TestApp, uri: &str, fields: &str) -> Vec<Captured> {
    let (_, headers, _) = get(app, "/decisions").await;
    let csrf = headers
        .get_all(header::SET_COOKIE)
        .iter()
        .filter_map(|value| value.to_str().ok())
        .find_map(|value| {
            let pair = value.split(';').next()?;
            pair.strip_prefix("__Host-csrf_token=")
                .or_else(|| pair.strip_prefix("csrf_token="))
                .map(ToOwned::to_owned)
        })
        .expect("csrf cookie");
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
                .body(Body::from(format!("_csrf={csrf}&{fields}")))
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(response.status(), StatusCode::SEE_OTHER);
    app.mock.captured.lock().unwrap().clone()
}

async fn mock_api(
    State(mock): State<Arc<Mock>>,
    method: Method,
    uri: Uri,
    headers: HeaderMap,
    request: Request<Body>,
) -> Response {
    let path = uri.path().to_owned();
    if method != Method::GET {
        let bytes = to_bytes(request.into_body(), usize::MAX).await.unwrap();
        mock.captured.lock().unwrap().push(Captured {
            route: format!("{method} {path}"),
            audit_log_reason: headers
                .get("x-audit-log-reason")
                .and_then(|value| value.to_str().ok())
                .map(ToOwned::to_owned),
            body: serde_json::from_slice(&bytes).unwrap_or(Value::Null),
        });
    }
    let user_path = format!("/admin/users/{USER_ID}");
    let decision_path = format!("/admin/enforcement-decisions/{DECISION_ID}");
    match (method, path.as_str()) {
        (Method::GET, "/admin/users/@me") => {
            let mut admin = user(ADMIN_ID, "AdminUser");
            admin["acls"] = json!(mock.admin_acls);
            Json(json!({ "user": admin })).into_response()
        }
        (Method::GET, "/admin/users") => Json(json!({
            "users": [user(ADMIN_ID, "AdminUser"), mock.user.clone()]
        }))
        .into_response(),
        (Method::GET, p) if p == user_path => {
            Json(json!({ "users": [mock.user.clone()] })).into_response()
        }
        (Method::GET, p) if p == format!("{user_path}/network-block-candidates") => Json(json!({
            "networks": [
                {"ip": NETWORK_IP, "last_seen_at": "2026-10-13T09:00:00.000Z", "skip_reason": null, "other_unrestricted_accounts": 0},
                {"ip": SHARED_IP, "last_seen_at": "2026-10-12T09:00:00.000Z", "skip_reason": null, "other_unrestricted_accounts": 2},
                {"ip": "10.0.0.5", "last_seen_at": "2026-10-11T09:00:00.000Z", "skip_reason": "special_purpose", "other_unrestricted_accounts": 0}
            ]
        }))
        .into_response(),
        (Method::GET, "/admin/enforcement-decisions") => {
            Json(json!({ "decisions": mock.decisions })).into_response()
        }
        (Method::GET, p) if p == decision_path => Json(mock.decision.clone()).into_response(),
        (Method::GET, "/admin/audit-logs") => Json(json!({
            "logs": mock.audit_logs,
            "total": mock.audit_logs.len()
        }))
        .into_response(),
        (Method::PUT | Method::DELETE | Method::PATCH, p) if p.starts_with(&user_path) => {
            Json(json!({ "user": mock.user.clone(), "skipped_ips": [] })).into_response()
        }
        (Method::POST, p) if p == format!("{user_path}/warnings") => {
            Json(json!({ "decision_id": DECISION_ID })).into_response()
        }
        (Method::POST, p) if p == format!("{decision_path}/end") => {
            Json(mock.decision.clone()).into_response()
        }
        (Method::POST, p) if p == format!("{decision_path}/notes") => Json(json!({
            "note_id": "1",
            "author_id": ADMIN_ID,
            "body": "x",
            "created_at": "2026-10-14T12:00:00.000Z"
        }))
        .into_response(),
        _ => (
            StatusCode::NOT_FOUND,
            Json(json!({ "code": "UNKNOWN", "message": "not found" })),
        )
            .into_response(),
    }
}

fn user(id: &str, username: &str) -> Value {
    json!({
        "id": id,
        "username": username,
        "discriminator": 1,
        "avatar": null,
        "banner": null,
        "email": "user@example.com",
        "email_verified": true,
        "email_bounced": false,
        "global_name": username,
        "bio": null,
        "pronouns": null,
        "accent_color": null,
        "date_of_birth": null,
        "locale": "en-US",
        "acls": [],
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
        "enforcement_decision_id": null,
        "last_active_at": null,
        "last_active_ip": null,
        "last_active_ip_reverse": null,
        "last_active_location": null
    })
}

fn subject() -> Value {
    user(USER_ID, "Subject")
}

fn suspended_subject() -> Value {
    let mut subject = subject();
    subject["temp_banned_until"] = json!(SUSPENDED_UNTIL);
    subject["flags"] = json!(DISABLED.to_string());
    subject["enforcement_decision_id"] = json!(DECISION_ID);
    subject
}

fn list_item(reason_code: i32) -> Value {
    json!({
        "decision_id": DECISION_ID,
        "subject_type": "user",
        "subject_id": USER_ID,
        "subject_name": null,
        "recipient_user_id": USER_ID,
        "kind": "suspension",
        "reason_code": reason_code,
        "source": "report",
        "automation": "none",
        "statement": null,
        "rights_holder": null,
        "ends_at": SUSPENDED_UNTIL,
        "deletion_at": null,
        "limit": null,
        "profile_fields": [],
        "removed_message_count": null,
        "removed_all_messages": false,
        "payment_at": null,
        "report_ids": [REPORT_ID],
        "decided_by": ADMIN_ID,
        "created_at": "2026-10-14T12:00:00.000Z",
        "status": "active",
        "end_outcome": null,
        "ended_at": null,
        "ended_by": null,
        "end_statement": null,
        "notice_status": "sent",
        "notice_channels": ["email"],
        "notice_locale": "en-US",
        "notice_text": "We suspended your account.",
        "notice_sent_at": "2026-10-14T12:00:01.000Z",
        "expired": false,
        "replaced": false
    })
}

fn full_decision() -> Value {
    let mut decision = list_item(11);
    decision["statement"] = json!("Stop sending these messages.");
    decision["notes"] = json!([{
        "note_id": "1950000000000000001",
        "author_id": ADMIN_ID,
        "body": "Ticket 42, three reports",
        "created_at": "2026-10-14T12:00:00.000Z"
    }]);
    decision["evidence"] = json!([{
        "message_id": MESSAGE_ID,
        "channel_id": CHANNEL_ID,
        "timestamp": "2026-10-13T09:00:00.000Z",
        "content": "the offending message",
        "attachments": [{"filename": "proof.png", "size": "1024", "content_type": "image/png"}],
        "attachment_count": 1
    }]);
    decision["profile_values"] = Value::Null;
    decision["network_blocks"] =
        json!([{"ip": NETWORK_IP, "expires_at": "2027-01-12T12:00:00.000Z"}]);
    decision["notice"] = json!({
        "decision_id": DECISION_ID,
        "kind": "suspension",
        "permanent": false,
        "issued_at": "Oct 14, 2026",
        "ends_at": "Jan 1, 2099",
        "deletion_at": null,
        "summary": "We suspended your account until Jan 1, 2099.",
        "effects": ["You can't sign in until then."],
        "rule": {"title": "Harassment", "url": "https://fluxer.com/guidelines#1-harassment-threats-and-privacy"},
        "facts": ["You sent messages that harass people."],
        "statement": "Stop sending these messages.",
        "how_we_decided": ["We reviewed this after a report."],
        "appeal": {"email": "appeals@fluxer.com", "deadline": "Dec 13, 2026", "account_has_email": true},
        "redress": []
    });
    decision
}

fn redacted_decision() -> Value {
    let mut decision = full_decision();
    decision["report_ids"] = json!([]);
    decision["evidence"][0]["content"] = Value::Null;
    decision["evidence"][0]["attachments"] = Value::Null;
    decision
}

fn decision_mock(reason_code: i32) -> Mock {
    let mut mock = Mock::new(suspended_subject());
    mock.decisions = vec![list_item(reason_code)];
    mock.decision = full_decision();
    mock
}

fn moderation_uri() -> String {
    format!("/users/{USER_ID}?tab=moderation")
}

fn current_restriction(html: &str) -> &str {
    let current = section(html, "data-current-restriction");
    current
        .find("Suspend account")
        .or_else(|| current.find("Account deletion"))
        .map_or(current, |end| &current[..end])
}

fn section<'a>(html: &'a str, marker: &str) -> &'a str {
    let start = html
        .find(marker)
        .unwrap_or_else(|| panic!("missing {marker}\n{html}"));
    &html[start..]
}

#[tokio::test]
async fn moderation_tab_with_a_decision_shows_it_and_ends_it() {
    let app = setup(decision_mock(11)).await;
    let html = page(&app, &moderation_uri()).await;
    let current = current_restriction(&html);
    assert!(
        current.contains("Suspension for Harassment or bullying. Until Jan 1, 2099, 12:00 AM UTC")
    );
    assert!(current.contains(&format!(r#"href="/decisions/{DECISION_ID}""#)));
    assert!(current.contains(&format!(r#"action="/decisions/{DECISION_ID}?action=end""#)));
    assert!(current.contains(r#"value="lifted""#));
    assert!(!current.contains("data-correction-line"));
    assert!(!html.contains("action=unban"));
    let table = section(&html, "data-decisions-table");
    assert!(table.contains(">Suspension</td>"));
    assert!(table.contains(">Active</td>"));
    assert!(html.contains("?action=suspend&amp;tab=moderation"));
    assert!(html.contains("?action=warn&amp;tab=moderation"));
}

#[tokio::test]
async fn ending_a_restricted_decision_shows_the_correction_line() {
    let app = setup(decision_mock(13)).await;
    let html = page(&app, &moderation_uri()).await;
    let current = current_restriction(&html);
    assert!(current.contains("data-correction-line"));
    assert!(current.contains("If this decision was reported to an authority, file a correction."));
}

#[tokio::test]
async fn moderation_tab_for_a_legacy_suspension_offers_the_lift_form() {
    let mut subject = suspended_subject();
    subject["enforcement_decision_id"] = Value::Null;
    let mut mock = Mock::new(subject);
    mock.audit_logs = vec![json!({
        "log_id": "1",
        "admin_user_id": ADMIN_ID,
        "admin_user": {"id": ADMIN_ID, "username": "AdminUser", "discriminator": "0001", "global_name": null},
        "action": "temp_ban",
        "target_id": USER_ID,
        "target_type": "user",
        "audit_log_reason": "Old internal note",
        "metadata": {"banned_until": SUSPENDED_UNTIL},
        "created_at": "2026-09-01T12:00:00.000Z"
    })];
    let app = setup(mock).await;
    let html = page(&app, &moderation_uri()).await;
    let current = current_restriction(&html);
    assert!(
        current.contains("Banned permanently. Recorded before decisions, so it has no decision.")
    );
    assert!(current.contains("Old internal note"));
    assert!(current.contains("?action=unban&amp;tab=moderation"));
    assert!(!current.contains("?action=end"));
    assert!(!current.contains("notify_user"));
}

#[tokio::test]
async fn moderation_tab_without_a_restriction_offers_suspend_and_warn_with_prefills() {
    let app = setup(Mock::new(subject())).await;
    let html = page(
        &app,
        &format!(
            "{}&report_id={REPORT_ID}&message={CHANNEL_ID}%2F{MESSAGE_ID}",
            moderation_uri()
        ),
    )
    .await;
    let current = current_restriction(&html);
    assert!(current.contains("No current restriction."));
    assert!(!current.contains("<form"));
    assert!(html.contains("No decisions."));
    let suspend = section(&html, "?action=suspend");
    assert!(suspend.contains(r#"name="duration_hours""#));
    assert!(suspend.contains(r#"name="remove_evidence""#));
    assert!(suspend.contains(&format!(">{REPORT_ID}</textarea>")));
    assert!(suspend.contains(&format!(">{CHANNEL_ID}/{MESSAGE_ID}</textarea>")));
    assert!(section(&html, "?action=warn").contains("Send warning"));
    let limit = between(&html, "?action=limit_features", "?action=warn");
    for expected in [
        format!(">{REPORT_ID}</textarea>"),
        format!(">{CHANNEL_ID}/{MESSAGE_ID}</textarea>"),
        r#"name="source" value="report" required checked"#.to_owned(),
        r#"Limit<span class="text-red-500 ml-0.5">*</span></legend>"#.to_owned(),
        r#"Internal note<span class="text-red-500 ml-0.5">*</span></label>"#.to_owned(),
    ] {
        assert!(limit.contains(&expected), "missing {expected}\n{limit}");
    }
}

fn between<'a>(html: &'a str, start: &str, end: &str) -> &'a str {
    let rest = section(html, start);
    rest.find(end).map_or(rest, |index| &rest[..index])
}

#[tokio::test]
async fn moderation_tab_offers_close_and_limit_features() {
    let app = setup(Mock::new(subject())).await;
    let html = page(&app, &moderation_uri()).await;
    let close = between(&html, "?action=close", "?action=limit_features");
    for expected in [
        r#"value="enforcement""#,
        r#"value="holder_request""#,
        r#"value="housekeeping""#,
        "A rule was broken",
        "The account holder asked",
        "Inactivity",
        r#"name="days_until_deletion" value="74""#,
        r#"data-show-for-basis="enforcement" hidden disabled"#,
        r#"data-show-for-basis="holder_request housekeeping" hidden disabled"#,
        r#"name="remove_evidence""#,
        "Close account",
    ] {
        assert!(close.contains(expected), "missing {expected}\n{close}");
    }
    assert!(!close.contains(r#"<option value="13""#));
    assert!(!close.contains("replace_pending_deletion_at"));
    assert!(!close.contains("notify_user"));
    let limit = between(&html, "?action=limit_features", "?action=warn");
    for expected in [
        r#"data-show-for-codes="3 6 8 18 21" hidden"#,
        r#"data-show-for-codes="29" hidden"#,
        r#"data-show-for-codes="12 14" hidden"#,
        r#"data-show-for-limit="reports" hidden disabled"#,
        r#"name="ends_on""#,
        r#"<option value="29""#,
        "Limit features",
    ] {
        assert!(limit.contains(expected), "missing {expected}\n{limit}");
    }
    assert!(!limit.contains(r#"<option value="11""#));
    assert!(!limit.contains("checked"));
}

#[tokio::test]
async fn moderation_tab_for_a_legacy_deletion_offers_the_cancel_form() {
    let mut subject = subject();
    subject["pending_deletion_at"] = json!("2026-12-01T00:00:00.000Z");
    subject["deletion_reason_code"] = json!(1);
    subject["deletion_scheduled_by"] = json!(ADMIN_ID);
    let app = setup(Mock::new(subject)).await;
    let html = page(&app, &moderation_uri()).await;
    let current = current_restriction(&html);
    assert!(
        current.contains("Scheduled for deletion. No decision owns it, so it is cancelled here.")
    );
    assert!(current.contains("?action=cancel_deletion&amp;tab=moderation"));
    assert!(current.contains("Cancel deletion"));
    assert!(!current.contains("notify_user"));
    let close = section(&html, "?action=close");
    assert!(
        close.contains(r#"name="replace_pending_deletion_at" value="2026-12-01T00:00:00.000Z""#)
    );
}

#[tokio::test]
async fn decisions_page_lists_the_review_queue() {
    let mut queued = list_item(12);
    queued["kind"] = json!("feature_limit");
    queued["limit"] = json!("purchases");
    queued["automation"] = json!("automated");
    queued["source"] = json!("own_review");
    queued["ends_at"] = Value::Null;
    queued["decided_by"] = json!("0");
    queued["payment_at"] = json!("2026-10-01T09:00:00.000Z");
    let mut mock = Mock::new(subject());
    mock.decisions = vec![queued];
    let app = setup(mock).await;
    let html = page(&app, "/decisions").await;
    let queue = section(&html, "data-review-queue");
    assert!(queue.contains(&format!(r#"href="/decisions/{DECISION_ID}""#)));
    assert!(queue.contains(&format!(r#"href="/users/{USER_ID}?tab=moderation""#)));
    assert!(queue.contains("Payment fraud"));
}

#[tokio::test]
async fn decisions_page_hides_the_review_queue_without_permission() {
    let mut mock = Mock::new(subject());
    mock.admin_acls = vec!["user:lookup"];
    let app = setup(mock).await;
    let html = page(&app, "/decisions").await;
    assert!(!html.contains("Review queue"));
}

#[tokio::test]
async fn moderation_tab_hides_decisions_and_actions_without_their_permissions() {
    let mut mock = decision_mock(11);
    mock.admin_acls = vec!["user:lookup", "user:update:flags"];
    let app = setup(mock).await;
    let html = page(&app, &moderation_uri()).await;
    let current = current_restriction(&html);
    assert!(current.contains("Banned permanently") || current.contains("Suspended until"));
    assert!(current.contains("Open the decision to see or end it."));
    assert!(!html.contains("data-decisions-table"));
    assert!(!html.contains("?action=suspend"));
    assert!(html.contains("?action=warn"));
}

#[tokio::test]
async fn overview_banner_links_the_decision_and_restriction_flags_are_read_only() {
    let app = setup(decision_mock(11)).await;
    let html = page(&app, &format!("/users/{USER_ID}")).await;
    let banner = section(&html, "data-restriction-banner");
    assert!(banner.contains(&format!(r#"href="/decisions/{DECISION_ID}""#)));
    let disabled = format!(r#"name="flags[]" value="{DISABLED}" checked disabled"#);
    assert!(html.contains(&disabled), "{html}");
    assert!(html.contains("Restriction flags are read-only. Use the moderation tab."));
}

#[tokio::test]
async fn decision_page_renders_the_full_response() {
    let app = setup(decision_mock(11)).await;
    let html = page(&app, &format!("/decisions/{DECISION_ID}")).await;
    for expected in [
        "Decision 1900000000000000001",
        "Harassment or bullying",
        "https://fluxer.com/guidelines#1-harassment-threats-and-privacy",
        "Stop sending these messages.",
        "We suspended your account.",
        "We suspended your account until Jan 1, 2099.",
        "the offending message",
        "Attachment: proof.png",
        "/messages?channel_id=1700000000000000001&amp;message_id=1700000000000000002&amp;context_limit=50",
        "/reports/1600000000000000001",
        "Ticket 42, three reports",
        "?action=add_note",
        "?action=end",
        "Network blocks",
        NETWORK_IP,
    ] {
        assert!(html.contains(expected), "missing {expected}\n{html}");
    }
}

#[tokio::test]
async fn an_expired_suspension_can_only_be_reversed() {
    let mut mock = decision_mock(11);
    for decision in [&mut mock.decision, &mut mock.decisions[0]] {
        decision["status"] = json!("ended");
        decision["end_outcome"] = json!("expired");
        decision["ended_at"] = json!(SUSPENDED_UNTIL);
        decision["ended_by"] = json!("0");
    }
    mock.decision["network_blocks"] = json!([]);
    let app = setup(mock).await;
    let html = page(&app, &format!("/decisions/{DECISION_ID}")).await;
    assert!(html.contains(">Expired<"), "{html}");
    assert!(html.contains("?action=end"));
    assert!(html.contains(r#"value="reversed""#));
    assert!(!html.contains(r#"value="lifted""#));
    let html = page(&app, &moderation_uri()).await;
    assert!(section(&html, "data-decisions-table").contains(">Expired</td>"));
}

#[tokio::test]
async fn decision_page_hides_the_end_form_without_the_ip_removal_permission() {
    let mut mock = decision_mock(11);
    mock.admin_acls = vec!["enforcement:view", "user:temp_ban"];
    let app = setup(mock).await;
    let html = page(&app, &format!("/decisions/{DECISION_ID}")).await;
    assert!(html.contains("Network blocks"));
    assert!(!html.contains("?action=end"));
}

#[tokio::test]
async fn suspend_card_offers_recent_networks_with_their_collateral() {
    let app = setup(Mock::new(subject())).await;
    let html = page(&app, &moderation_uri()).await;
    let suspend = between(&html, "?action=suspend", "?action=close");
    let networks = section(suspend, r#"data-show-for-duration_hours="0""#);
    assert!(networks.contains("hidden disabled"));
    assert!(networks.contains("Block networks this account used"));
    assert!(networks.contains("0 other unrestricted accounts last active here"));
    assert!(networks.contains("2 other unrestricted accounts last active here"));
    assert!(networks.contains("Private or special-purpose range, never blocked"));
    assert!(networks.contains(&format!(r#"value="{NETWORK_IP}" class="hidden""#)));
    assert!(networks.contains(&format!(r#"value="{SHARED_IP}" disabled"#)));
    assert!(networks.contains(r#"name="network_block_days" value="90""#));
    let close = between(&html, "?action=close", "?action=limit_features");
    assert!(close.contains("Block networks this account used"));
}

#[tokio::test]
async fn suspend_and_close_send_the_chosen_networks() {
    let app = setup(Mock::new(subject())).await;
    let captured = post(
        &app,
        &format!("/users/{USER_ID}?action=suspend&tab=moderation"),
        &format!(
            "duration_hours=0&reason_code=3&source=own_review&automation=none\
             &statement=Spam&internal_note=Ticket+7&block_networks={NETWORK_IP}&network_block_days=30"
        ),
    )
    .await;
    assert_eq!(
        captured[0].body["block_networks"],
        json!({"ips": [NETWORK_IP], "days": 30})
    );
    let captured = post(
        &app,
        &format!("/users/{USER_ID}?action=close&tab=moderation"),
        &format!(
            "basis=enforcement&days_until_deletion=74&reason_code=3&source=own_review\
             &automation=none&statement=Spam&internal_note=Ticket+7&block_networks={NETWORK_IP}"
        ),
    )
    .await;
    assert_eq!(
        captured.last().unwrap().body["block_networks"],
        json!({"ips": [NETWORK_IP], "days": 90})
    );
}

#[tokio::test]
async fn moderation_tab_skips_networks_without_the_ip_permissions() {
    let mut mock = Mock::new(subject());
    mock.admin_acls = vec!["user:temp_ban", "user:delete"];
    let app = setup(mock).await;
    let html = page(&app, &moderation_uri()).await;
    assert!(!html.contains("Block networks this account used"));
}

#[tokio::test]
async fn decision_page_renders_a_redacted_response_without_staff_forms() {
    let mut mock = decision_mock(11);
    mock.admin_acls = vec!["enforcement:view"];
    mock.decision = redacted_decision();
    let app = setup(mock).await;
    let html = page(&app, &format!("/decisions/{DECISION_ID}")).await;
    assert!(html.contains("Content is hidden. Needs the message:lookup permission."));
    assert!(html.contains("Attachments: 1"));
    assert!(!html.contains("the offending message"));
    assert!(!html.contains("/reports/1600000000000000001"));
    assert!(!html.contains("?action=add_note"));
    assert!(!html.contains("?action=end"));
}

#[tokio::test]
async fn decisions_lookup_redirects_to_the_decision() {
    let app = setup(Mock::new(subject())).await;
    let (status, headers, _) = get(&app, &format!("/decisions?id=+{DECISION_ID}+")).await;
    assert!(status.is_redirection());
    assert_eq!(
        headers
            .get(header::LOCATION)
            .and_then(|value| value.to_str().ok()),
        Some(format!("/decisions/{DECISION_ID}").as_str())
    );
    let html = page(&app, "/decisions?id=abc").await;
    assert!(html.contains("A decision ID is a number."));
}

#[tokio::test]
async fn suspend_sends_the_decision_and_the_internal_note() {
    let app = setup(Mock::new(subject())).await;
    let captured = post(
        &app,
        &format!("/users/{USER_ID}?action=suspend&tab=moderation"),
        &format!(
            "duration_hours=0&reason_code=11&source=report&automation=flagged\
             &report_ids={REPORT_ID}&evidence={CHANNEL_ID}%2F{MESSAGE_ID}\
             &statement=Stop&internal_note=Ticket+42&remove_evidence=true"
        ),
    )
    .await;
    assert_eq!(captured.len(), 1);
    assert_eq!(captured[0].route, format!("PUT /admin/users/{USER_ID}/ban"));
    assert_eq!(captured[0].audit_log_reason.as_deref(), Some("Ticket 42"));
    assert_eq!(
        captured[0].body,
        json!({
            "duration_hours": 0,
            "remove_evidence": true,
            "decision": {
                "reason_code": 11,
                "source": "report",
                "automation": "flagged",
                "report_ids": [REPORT_ID],
                "evidence": [{"channel_id": CHANNEL_ID, "message_id": MESSAGE_ID}],
                "statement": "Stop"
            }
        })
    );
}

#[tokio::test]
async fn suspend_without_a_reason_sends_nothing() {
    let app = setup(Mock::new(subject())).await;
    let captured = post(
        &app,
        &format!("/users/{USER_ID}?action=suspend&tab=moderation"),
        "duration_hours=24&source=report&automation=none&internal_note=x",
    )
    .await;
    assert!(captured.is_empty());
}

#[tokio::test]
async fn warn_sends_the_decision_with_the_rights_holder() {
    let app = setup(Mock::new(subject())).await;
    let captured = post(
        &app,
        &format!("/users/{USER_ID}?action=warn&tab=moderation"),
        "reason_code=31&source=own_review&automation=none&statement=Remove+it\
         &rights_holder=Example+Records&internal_note=DMCA+notice",
    )
    .await;
    assert_eq!(captured.len(), 1);
    assert_eq!(
        captured[0].route,
        format!("POST /admin/users/{USER_ID}/warnings")
    );
    assert_eq!(captured[0].audit_log_reason.as_deref(), Some("DMCA notice"));
    assert_eq!(
        captured[0].body,
        json!({
            "decision": {
                "reason_code": 31,
                "source": "own_review",
                "automation": "none",
                "statement": "Remove it",
                "rights_holder": "Example Records"
            }
        })
    );
}

#[tokio::test]
async fn ending_and_annotating_a_decision_send_their_requests() {
    let app = setup(decision_mock(11)).await;
    let end_uri = format!("/decisions/{DECISION_ID}?action=end");
    let captured = post(
        &app,
        &end_uri,
        "outcome=reversed&statement=We+made+a+mistake&internal_note=Appeal+upheld",
    )
    .await;
    assert_eq!(
        captured[0].route,
        format!("POST /admin/enforcement-decisions/{DECISION_ID}/end")
    );
    assert_eq!(
        captured[0].audit_log_reason.as_deref(),
        Some("Appeal upheld")
    );
    assert_eq!(
        captured[0].body,
        json!({"outcome": "reversed", "statement": "We made a mistake"})
    );
    let captured = post(
        &app,
        &format!("/decisions/{DECISION_ID}?action=add_note"),
        "body=Appeal+received",
    )
    .await;
    assert_eq!(
        captured[1].route,
        format!("POST /admin/enforcement-decisions/{DECISION_ID}/notes")
    );
    assert_eq!(captured[1].body, json!({"body": "Appeal received"}));
    let captured = post(&app, &end_uri, "statement=x&internal_note=y").await;
    assert_eq!(captured.len(), 2, "an end without an outcome sends nothing");
}

#[tokio::test]
async fn saving_flags_never_touches_restriction_flags() {
    let app = setup(Mock::new(suspended_subject())).await;
    let captured = post(
        &app,
        &format!("/users/{USER_ID}?action=update_flags&tab=overview"),
        &format!("flags[]={}&flags[]={}", 1u64 << 50, 1u64 << 6),
    )
    .await;
    assert_eq!(captured.len(), 1);
    assert_eq!(
        captured[0].route,
        format!("PATCH /admin/users/{USER_ID}/flags")
    );
    assert_eq!(
        captured[0].body,
        json!({"add_flags": [(1u64 << 50).to_string()]})
    );
}

#[tokio::test]
async fn shredder_and_delete_all_offer_a_basis_and_delete_all_refuses_restricted_reasons() {
    let app = setup(Mock::new(subject())).await;
    let html = page(
        &app,
        &format!(
            "{}&delete_all_messages_dry_run=true&delete_all_messages_channel_count=1\
             &delete_all_messages_message_count=2",
            moderation_uri()
        ),
    )
    .await;
    let delete_all = between(&html, "Delete all messages", "Message shredder");
    for expected in [
        "2 messages in 1 channel.",
        r#"name="dry_run" value="false""#,
        r#"name="basis" value="enforcement""#,
        r#"name="basis" value="holder_request""#,
        r#"name="basis" value="housekeeping""#,
        r#"name="evidence""#,
    ] {
        assert!(
            delete_all.contains(expected),
            "missing {expected}\n{delete_all}"
        );
    }
    assert!(!delete_all.contains("Restricted: no details"));
    let shredder = section(&html, "Message shredder");
    assert!(shredder.contains(r#"name="basis" value="enforcement""#));
    assert!(shredder.contains("Restricted: no details"));
    assert!(
        !shredder.contains(r#"name="evidence""#),
        "the entries are the evidence"
    );
}

#[tokio::test]
async fn content_removal_forms_send_their_basis() {
    let app = setup(Mock::new(subject())).await;
    let captured = post(
        &app,
        &format!("/users/{USER_ID}?action=message_shred&tab=moderation"),
        &format!(
            "csv_data={CHANNEL_ID}%2C{MESSAGE_ID}&basis=enforcement&reason_code=3\
             &source=own_review&automation=none&statement=Stop&internal_note=Ticket+7"
        ),
    )
    .await;
    assert_eq!(
        captured[0].route,
        format!("POST /admin/users/{USER_ID}/message-shreds")
    );
    assert_eq!(captured[0].audit_log_reason.as_deref(), Some("Ticket 7"));
    assert_eq!(
        captured[0].body,
        json!({
            "entries": [{"channel_id": CHANNEL_ID, "message_id": MESSAGE_ID}],
            "basis": {
                "type": "enforcement",
                "decision": {
                    "reason_code": 3,
                    "source": "own_review",
                    "automation": "none",
                    "statement": "Stop"
                }
            }
        })
    );
    let captured = post(
        &app,
        &format!("/users/{USER_ID}?action=delete_all_messages&tab=moderation"),
        "dry_run=false&basis=housekeeping&internal_note=Cleanup",
    )
    .await;
    assert_eq!(
        captured[1].route,
        format!("DELETE /admin/users/{USER_ID}/messages")
    );
    assert_eq!(captured[1].body, json!({"basis": {"type": "housekeeping"}}));
    assert_eq!(captured[1].audit_log_reason.as_deref(), Some("Cleanup"));
    let captured = post(
        &app,
        &format!("/users/{USER_ID}?action=clear_fields&tab=account"),
        "fields[]=bio&basis=holder_request",
    )
    .await;
    assert_eq!(
        captured[2].route,
        format!("DELETE /admin/users/{USER_ID}/profile-fields")
    );
    assert_eq!(
        captured[2].body,
        json!({"fields": ["bio"], "basis": {"type": "holder_request"}})
    );
    let captured = post(
        &app,
        &format!("/users/{USER_ID}?action=delete_all_messages&tab=moderation"),
        "dry_run=false",
    )
    .await;
    assert_eq!(
        captured.len(),
        3,
        "a deletion without a basis sends nothing"
    );
}

#[tokio::test]
async fn community_message_and_report_forms_send_their_basis_or_outcome() {
    let app = setup(Mock::new(subject())).await;
    let guild_id = "1800000000000000001";
    let captured = post(
        &app,
        &format!("/guilds/{guild_id}?action=delete_guild&tab=moderation"),
        "basis=enforcement&reason_code=26&source=report&automation=none\
         &report_ids=1600000000000000001&internal_note=Reviewed",
    )
    .await;
    assert_eq!(
        captured[0].route,
        format!("DELETE /admin/guilds/{guild_id}")
    );
    assert_eq!(captured[0].audit_log_reason.as_deref(), Some("Reviewed"));
    assert_eq!(
        captured[0].body,
        json!({
            "basis": {
                "type": "enforcement",
                "decision": {
                    "reason_code": 26,
                    "source": "report",
                    "automation": "none",
                    "report_ids": [REPORT_ID]
                }
            }
        })
    );
    let captured = post(
        &app,
        "/messages?action=delete",
        &format!("channel_id={CHANNEL_ID}&message_id={MESSAGE_ID}&basis=housekeeping"),
    )
    .await;
    assert_eq!(
        captured[1].route,
        format!("DELETE /admin/channels/{CHANNEL_ID}/messages/{MESSAGE_ID}")
    );
    assert_eq!(captured[1].body, json!({"basis": {"type": "housekeeping"}}));
    let captured = post(
        &app,
        &format!("/reports/{REPORT_ID}/resolve"),
        "resolution=actioned&public_comment=Handled",
    )
    .await;
    assert_eq!(
        captured.len(),
        2,
        "an actioned resolution without an outcome sends nothing"
    );
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
