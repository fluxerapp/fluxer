// SPDX-License-Identifier: AGPL-3.0-or-later

use axum::{
    Json, Router,
    body::{Body, to_bytes},
    http::{HeaderMap, Method, Request, StatusCode, Uri, header},
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

const SECRET_KEY: &str = "legacy-csrf-cookie-test-secret";
const ADMIN_ORIGIN: &str = "https://admin.example.test";
const LEGACY_HEX_TOKEN: &str = "8f14e45fceea167a5a36dedd4bea25438f14e45fceea167a5a36dedd4bea2543";

struct TestApp {
    router: Router,
    session_cookie: String,
}

#[tokio::test]
async fn clean_browser_can_submit_an_action() {
    let app = setup().await;
    let cookie = app.session_cookie.clone();

    let (cookie_token, page_token) = load_page(&app, &cookie).await;
    assert_eq!(cookie_token, page_token);

    let with_csrf = format!("{cookie}; __Host-csrf_token={cookie_token}");
    let status = submit_action(&app, &with_csrf, &page_token).await;
    assert_eq!(status, StatusCode::OK);
}

#[tokio::test]
async fn legacy_csrf_cookie_does_not_wedge_actions() {
    let app = setup().await;
    let stale = format!("{}; csrf_token={LEGACY_HEX_TOKEN}", app.session_cookie);

    let (cookie_token, page_token) = load_page(&app, &stale).await;
    assert_eq!(cookie_token, page_token);

    let both = format!("{stale}; __Host-csrf_token={cookie_token}");
    let status = submit_action(&app, &both, &page_token).await;
    assert_eq!(
        status,
        StatusCode::OK,
        "a leftover unsigned csrf_token cookie must not block actions"
    );
}

#[tokio::test]
async fn production_responses_expire_the_legacy_csrf_cookie() {
    let app = setup().await;
    let stale = format!("{}; csrf_token={LEGACY_HEX_TOKEN}", app.session_cookie);
    let headers = page_headers(&app, &stale).await;

    let expiry = headers
        .get_all(header::SET_COOKIE)
        .iter()
        .filter_map(|value| value.to_str().ok())
        .find(|value| value.starts_with("csrf_token=;"))
        .unwrap_or_else(|| panic!("legacy cookie was not expired: {headers:?}"));
    assert!(expiry.contains("Max-Age=0"), "{expiry}");
    assert!(expiry.contains("Path=/"), "{expiry}");
}

async fn setup() -> TestApp {
    let api_endpoint = spawn_mock_api().await;
    let router = build_router(production_config(api_endpoint));
    let session_value = session::create_session("1500000000000000000", "test-token", SECRET_KEY);
    TestApp {
        router,
        session_cookie: format!("{}={session_value}", session::SESSION_COOKIE_NAME),
    }
}

fn production_config(api_endpoint: String) -> AdminConfig {
    AdminConfig {
        env: RuntimeEnv::Production,
        host: "127.0.0.1".to_owned(),
        port: 0,
        secret_key_base: SECRET_KEY.to_owned(),
        base_path: String::new(),
        api_endpoint,
        media_endpoint: "https://media.example.test".to_owned(),
        static_cdn_endpoint: "https://static.example.test".to_owned(),
        admin_endpoint: ADMIN_ORIGIN.to_owned(),
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

async fn page_headers(app: &TestApp, cookie: &str) -> HeaderMap {
    app.router
        .clone()
        .oneshot(page_request(cookie))
        .await
        .unwrap()
        .headers()
        .clone()
}

async fn load_page(app: &TestApp, cookie: &str) -> (String, String) {
    let response = app
        .router
        .clone()
        .oneshot(page_request(cookie))
        .await
        .unwrap();
    let status = response.status();
    let headers = response.headers().clone();
    let body = to_bytes(response.into_body(), usize::MAX).await.unwrap();
    let text = String::from_utf8(body.to_vec()).unwrap();
    assert_eq!(status, StatusCode::OK, "{text}");
    let cookie_token = host_csrf_cookie(&headers)
        .unwrap_or_else(|| panic!("no __Host-csrf_token in Set-Cookie: {headers:?}"));
    let page_token = form_csrf_value(&text).expect("no _csrf hidden input rendered");
    (cookie_token, page_token)
}

fn page_request(cookie: &str) -> Request<Body> {
    Request::builder()
        .method(Method::GET)
        .uri("/admin-api-keys")
        .header(header::COOKIE, cookie)
        .header("sec-fetch-site", "same-origin")
        .body(Body::empty())
        .unwrap()
}

async fn submit_action(app: &TestApp, cookie: &str, form_token: &str) -> StatusCode {
    let response = app
        .router
        .clone()
        .oneshot(
            Request::builder()
                .method(Method::POST)
                .uri("/admin-api-keys?action=create")
                .header(header::CONTENT_TYPE, "application/x-www-form-urlencoded")
                .header(header::COOKIE, cookie)
                .header(header::ORIGIN, ADMIN_ORIGIN)
                .header("sec-fetch-site", "same-origin")
                .header("HX-Request", "true")
                .header("HX-Boosted", "true")
                .header("HX-Target", "body")
                .body(Body::from(format!(
                    "_csrf={form_token}&name=Legacy+Cookie+Key&acls=*"
                )))
                .unwrap(),
        )
        .await
        .unwrap();
    response.status()
}

fn host_csrf_cookie(headers: &HeaderMap) -> Option<String> {
    headers
        .get_all(header::SET_COOKIE)
        .iter()
        .filter_map(|value| value.to_str().ok())
        .find_map(|value| {
            value
                .split(';')
                .next()
                .and_then(|pair| pair.trim().strip_prefix("__Host-csrf_token="))
                .map(str::to_owned)
        })
}

fn form_csrf_value(body: &str) -> Option<String> {
    let marker = r#"name="_csrf" value=""#;
    body.match_indices(marker)
        .filter_map(|(index, _)| {
            let rest = &body[index + marker.len()..];
            let end = rest.find('"')?;
            Some(rest[..end].to_owned())
        })
        .find(|value| !value.is_empty())
}

async fn spawn_mock_api() -> String {
    let listener = TcpListener::bind(("127.0.0.1", 0)).await.unwrap();
    let addr = listener.local_addr().unwrap();
    tokio::spawn(async move {
        axum::serve(listener, Router::new().fallback(mock_api))
            .await
            .unwrap();
    });
    format!("http://{addr}")
}

async fn mock_api(method: Method, uri: Uri) -> Response {
    match (method, uri.path()) {
        (Method::GET, "/admin/users/me") => Json(json!({ "user": admin_user() })).into_response(),
        (Method::GET, "/admin/api-keys") => Json(json!([])).into_response(),
        (Method::POST, "/admin/api-keys") => Json(json!({
            "key_id": "1900000000000000001",
            "key": "fa_1900000000000000001_OneTimeSecretForTests",
            "name": "Legacy Cookie Key",
            "created_at": "2026-07-10T15:00:00.000Z",
            "expires_at": null,
            "acls": ["*"]
        }))
        .into_response(),
        _ => (StatusCode::NOT_FOUND, Json(json!({ "error": "not found" }))).into_response(),
    }
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
