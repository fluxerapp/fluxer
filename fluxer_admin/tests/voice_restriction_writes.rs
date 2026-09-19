// SPDX-License-Identifier: AGPL-3.0-or-later

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

const SECRET_KEY: &str = "voice-restriction-writes-test-secret";
const REGION_ID: &str = "europe-north";
const SERVER_ID: &str = "europe-north-server-1";

type CapturedBodies = Arc<Mutex<Vec<(String, Value)>>>;

#[tokio::test]
async fn clearing_the_restriction_fields_reaches_the_api_as_empty_lists() {
    let app = setup().await;
    let csrf_token = csrf_token(&app).await;
    let status = post_form(
        &app,
        "/voice-servers?action=update",
        &format!(
            "_csrf={csrf_token}&region_id={REGION_ID}&server_id={SERVER_ID}\
             &endpoint=wss%3A%2F%2Fvoice.example.com&is_active=true\
             &required_guild_features=&allowed_guild_ids=&soft_connection_limit="
        ),
    )
    .await;
    assert_eq!(status, StatusCode::SEE_OTHER);

    let body = captured_body(
        &app,
        "PATCH /admin/voice/regions/europe-north/servers/europe-north-server-1",
    );
    assert_eq!(body["required_guild_features"], json!([]));
    assert_eq!(body["allowed_guild_ids"], json!([]));
    assert_eq!(body["soft_connection_limit"], Value::Null);
    assert_eq!(body["vip_only"], json!(false));
}

#[tokio::test]
async fn activating_a_server_leaves_the_restriction_fields_untouched() {
    let app = setup().await;
    let csrf_token = csrf_token(&app).await;
    let status = post_form(
        &app,
        "/voice-servers?action=update",
        &format!(
            "_csrf={csrf_token}&region_id={REGION_ID}&server_id={SERVER_ID}\
             &endpoint=wss%3A%2F%2Fvoice.example.com&is_active=false&vip_only=true"
        ),
    )
    .await;
    assert_eq!(status, StatusCode::SEE_OTHER);

    let body = captured_body(
        &app,
        "PATCH /admin/voice/regions/europe-north/servers/europe-north-server-1",
    );
    let object = body.as_object().expect("object body");
    assert!(!object.contains_key("required_guild_features"));
    assert!(!object.contains_key("allowed_guild_ids"));
    assert_eq!(body["is_active"], json!(false));
    assert_eq!(body["vip_only"], json!(true));
}

#[tokio::test]
async fn clearing_the_region_restriction_fields_reaches_the_api_as_empty_lists() {
    let app = setup().await;
    let csrf_token = csrf_token(&app).await;
    let status = post_form(
        &app,
        "/voice-regions?action=update",
        &format!(
            "_csrf={csrf_token}&id={REGION_ID}&name=Northern%20Europe&emoji=%F0%9F%87%B8%F0%9F%87%AA\
             &latitude=59.33&longitude=18.06&required_guild_features=&allowed_guild_ids="
        ),
    )
    .await;
    assert_eq!(status, StatusCode::SEE_OTHER);

    let body = captured_body(&app, "PATCH /admin/voice/regions/europe-north");
    assert_eq!(body["required_guild_features"], json!([]));
    assert_eq!(body["allowed_guild_ids"], json!([]));
}

struct TestApp {
    router: Router,
    session_cookie: String,
    captured: CapturedBodies,
}

async fn setup() -> TestApp {
    let captured: CapturedBodies = Arc::new(Mutex::new(Vec::new()));
    let api_endpoint = spawn_mock_api(Arc::clone(&captured)).await;
    let router = build_router(test_config(api_endpoint));
    let session_value = session::create_session("1500000000000000000", "test-token", SECRET_KEY);
    TestApp {
        router,
        session_cookie: format!("{}={session_value}", session::SESSION_COOKIE_NAME),
        captured,
    }
}

fn captured_body(app: &TestApp, route: &str) -> Value {
    let captured = app.captured.lock().expect("captured bodies");
    captured
        .iter()
        .find(|(seen, _)| seen == route)
        .map(|(_, body)| body.clone())
        .unwrap_or_else(|| {
            panic!(
                "no request captured for {route}, saw {:?}",
                captured.iter().map(|(seen, _)| seen).collect::<Vec<_>>()
            )
        })
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
        .expect("form carries a csrf token");
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

async fn spawn_mock_api(captured: CapturedBodies) -> String {
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
    State(captured): State<CapturedBodies>,
    method: Method,
    uri: Uri,
    request: Request<Body>,
) -> Response {
    let path = uri.path().to_owned();
    if method == Method::PATCH {
        let bytes = to_bytes(request.into_body(), usize::MAX).await.unwrap();
        let body: Value = serde_json::from_slice(&bytes).unwrap_or(Value::Null);
        captured
            .lock()
            .expect("captured bodies")
            .push((format!("PATCH {path}"), body));
    }
    match (method, path.as_str()) {
        (Method::GET, "/admin/users/@me") => Json(json!({ "user": admin_user() })).into_response(),
        (Method::PATCH, "/admin/voice/regions/europe-north") => {
            Json(json!({ "region": region() })).into_response()
        }
        (Method::PATCH, "/admin/voice/regions/europe-north/servers/europe-north-server-1") => {
            Json(json!({ "server": server() })).into_response()
        }
        (Method::GET, "/admin/voice/regions") => {
            Json(json!({ "regions": [region()] })).into_response()
        }
        _ => (
            StatusCode::NOT_FOUND,
            Json(json!({ "message": "not found" })),
        )
            .into_response(),
    }
}

fn region() -> Value {
    json!({
        "id": REGION_ID,
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

fn server() -> Value {
    json!({
        "region_id": REGION_ID,
        "server_id": SERVER_ID,
        "endpoint": "wss://voice.example.com",
        "latitude": null,
        "longitude": null,
        "is_active": true,
        "soft_connection_limit": null,
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
        "suspicious_activity_flags": 0,
        "phone_verification_deferred": false,
        "has_totp": false,
        "authenticator_types": [],
        "has_verified_phone": false,
        "temp_banned_until": null,
        "pending_deletion_at": null,
        "pending_bulk_message_deletion_at": null,
        "deletion_reason_code": null,
        "deletion_public_reason": null,
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
        admin_endpoint: "https://admin.example.test".to_owned(),
        web_app_endpoint: "https://app.example.test".to_owned(),
        kv_url: String::new(),
        oauth_client_id: "admin-client".to_owned(),
        oauth_client_secret: "admin-secret".to_owned(),
        oauth_redirect_uri: "https://admin.example.test/callback".to_owned(),
        build_version: "test".to_owned(),
        release_channel: "test".to_owned(),
        self_hosted: false,
        proxy: ProxyConfig {
            trust_client_ip_header: false,
            client_ip_header_name: "x-forwarded-for".to_owned(),
        },
    }
}
