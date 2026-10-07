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
use tokio::net::TcpListener;
use tower::ServiceExt;

const SECRET_KEY: &str = "username-tags-test-secret";
const ADMIN_ID: &str = "1500000000000000000";
const TARGET_ID: &str = "1500000000000000042";
const DISCRIMINATOR_INPUT: &str = r#"name="discriminator""#;

#[derive(Clone)]
struct MockApi {
    account_identity: &'static str,
    unique_usernames: bool,
    target: Value,
}

#[tokio::test]
async fn username_instances_show_humans_without_a_tag() {
    let page = account_page(true, "username", user(TARGET_ID, "member", 0, false)).await;
    assert!(page.contains(r#"<p class="break-words text-sm text-neutral-500">member</p>"#));
    assert!(page.contains(r#"<div class="truncate text-neutral-500 text-xs">lilith</div>"#));
    assert!(!page.contains("member#0000"));
    assert!(!page.contains("lilith#0000"));
    assert!(!page.contains(DISCRIMINATOR_INPUT));
}

#[tokio::test]
async fn email_instances_with_random_tags_show_tags_and_allow_tag_changes() {
    let page =
        account_page_with(true, "email", false, user(TARGET_ID, "member", 1234, false)).await;
    assert!(page.contains("member#1234"));
    assert!(page.contains(DISCRIMINATOR_INPUT));
}

#[tokio::test]
async fn email_instances_with_no_tags_show_humans_without_a_tag() {
    let page = account_page_with(true, "email", true, user(TARGET_ID, "member", 0, false)).await;
    assert!(page.contains(r#"<p class="break-words text-sm text-neutral-500">member</p>"#));
    assert!(!page.contains("member#0000"));
    assert!(!page.contains("lilith#0000"));
    assert!(!page.contains(DISCRIMINATOR_INPUT));
    assert!(page.contains("Send Password Reset"));
}

#[tokio::test]
async fn username_instances_never_show_tags_even_if_told_random() {
    let page =
        account_page_with(true, "username", false, user(TARGET_ID, "member", 0, false)).await;
    assert!(!page.contains("member#0000"));
    assert!(!page.contains(DISCRIMINATOR_INPUT));
}

#[tokio::test]
async fn username_instances_keep_bot_tags() {
    let page = account_page(true, "username", user(TARGET_ID, "helper", 4363, true)).await;
    assert!(page.contains("helper#4363"));
    assert!(page.contains(DISCRIMINATOR_INPUT));
}

#[tokio::test]
async fn email_instances_keep_the_zero_tag() {
    let page = account_page(true, "email", user(TARGET_ID, "member", 0, false)).await;
    assert!(page.contains("member#0000"));
    assert!(page.contains("lilith#0000"));
    assert!(page.contains(DISCRIMINATOR_INPUT));
}

#[tokio::test]
async fn hosted_admin_keeps_the_zero_tag() {
    let page = account_page(false, "username", user(TARGET_ID, "member", 0, false)).await;
    assert!(page.contains("member#0000"));
    assert!(page.contains(DISCRIMINATOR_INPUT));
}

async fn account_page(self_hosted: bool, account_identity: &'static str, target: Value) -> String {
    account_page_with(
        self_hosted,
        account_identity,
        account_identity == "username",
        target,
    )
    .await
}

async fn account_page_with(
    self_hosted: bool,
    account_identity: &'static str,
    unique_usernames: bool,
    target: Value,
) -> String {
    let api_endpoint = spawn_mock_api(MockApi {
        account_identity,
        unique_usernames,
        target,
    })
    .await;
    let router = build_router(test_config(api_endpoint, self_hosted));
    let session_value = session::create_session(ADMIN_ID, "test-token", SECRET_KEY);
    let response = router
        .oneshot(
            Request::builder()
                .method(Method::GET)
                .uri(format!("/users/{TARGET_ID}?tab=account"))
                .header(
                    header::COOKIE,
                    format!("{}={session_value}", session::SESSION_COOKIE_NAME),
                )
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(response.status(), StatusCode::OK);
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
    let target_user = format!("/admin/users/{TARGET_ID}");
    let target_sessions = format!("{target_user}/sessions");
    let target_credentials = format!("{target_user}/webauthn-credentials");
    match (method, uri.path()) {
        (Method::GET, "/admin/users/@me") => Json(json!({
            "user": user(ADMIN_ID, "lilith", 0, false)
        }))
        .into_response(),
        (Method::GET, "/.well-known/fluxer") => Json(json!({
            "features": {
                "premium_enabled": false,
                "account_identity": mock.account_identity,
                "tag_style": if mock.unique_usernames { "none" } else { "random" }
            }
        }))
        .into_response(),
        (Method::GET, p) if p == target_user => {
            Json(json!({ "users": [mock.target] })).into_response()
        }
        (Method::GET, p) if p == target_sessions => Json(json!({ "sessions": [] })).into_response(),
        (Method::GET, p) if p == target_credentials => Json(json!([])).into_response(),
        _ => (
            StatusCode::NOT_FOUND,
            Json(json!({ "message": "not found" })),
        )
            .into_response(),
    }
}

fn user(id: &str, username: &str, discriminator: u16, bot: bool) -> Value {
    json!({
        "id": id,
        "username": username,
        "discriminator": discriminator,
        "avatar": null,
        "banner": null,
        "email": null,
        "email_verified": false,
        "email_bounced": false,
        "global_name": null,
        "bio": null,
        "pronouns": null,
        "accent_color": null,
        "date_of_birth": null,
        "locale": "en-GB",
        "acls": ["*"],
        "traits": [],
        "flags": "0",
        "premium_flags": 0,
        "bot": bot,
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
