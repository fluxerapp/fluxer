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

const SECRET_KEY: &str = "htmx-acceptance-test-secret";
const ADMIN_API_KEY_SECRET: &str = "fa_1900000000000000001_OneTimeSecretForAcceptance";

struct TestApp {
    router: Router,
    session_cookie: String,
}

#[tokio::test]
async fn admin_api_key_create_form_renders_the_one_time_secret() {
    let app = setup().await;
    let (headers, page) = get_with_headers(&app, "/admin-api-keys", &[]).await;
    let csrf_token = csrf_cookie(&headers)
        .unwrap_or_else(|| panic!("Admin API keys page did not set csrf_token cookie\n{page}"));
    assert!(page.contains(r#"data-admin-result-form="true""#), "{page}");

    let (status, _, response_body) = post_form_with_headers(
        &app,
        "/admin-api-keys?action=create",
        &[
            ("HX-Request", "true"),
            ("HX-Boosted", "true"),
            ("HX-Target", "body"),
            (
                "Cookie",
                &format!("{}; csrf_token={}", app.session_cookie, csrf_token),
            ),
        ],
        &format!("_csrf={csrf_token}&name=Acceptance+Key&acls=*"),
    )
    .await;

    assert_eq!(status, StatusCode::OK, "{response_body}");
    assert_full_layout(&response_body);
    assert!(response_body.contains(r#"hx-history="false""#));
    assert!(response_body.contains(ADMIN_API_KEY_SECRET));
}

#[tokio::test]
async fn search_routes_return_layout_or_fragments_by_hx_target() {
    let app = setup().await;
    let cases = [
        SearchCase {
            path: "/users?ids=1500000000000000001",
            result_target: "users-results",
            mismatch_target: "guilds-results",
            result_text: "SearchedUser",
        },
        SearchCase {
            path: "/guilds?ids=1600000000000000001",
            result_target: "guilds-results",
            mismatch_target: "users-results",
            result_text: "Searched Guild",
        },
        SearchCase {
            path: "/applications?owner_id=1500000000000000001",
            result_target: "applications-results",
            mismatch_target: "users-results",
            result_text: "Mock Application",
        },
    ];
    for case in cases {
        let full = get(&app, case.path, &[]).await;
        assert_full_layout(&full);
        assert!(
            full.contains(case.result_text),
            "full response for {}",
            case.path
        );
        let boosted = get(
            &app,
            case.path,
            &[
                ("HX-Request", "true"),
                ("HX-Boosted", "true"),
                ("HX-Target", "body"),
            ],
        )
        .await;
        assert_full_layout(&boosted);
        assert!(
            boosted.contains(case.result_text),
            "boosted response for {}",
            case.path
        );
        let fragment = get(
            &app,
            case.path,
            &[("HX-Request", "true"), ("HX-Target", case.result_target)],
        )
        .await;
        assert_fragment(&fragment);
        assert!(
            fragment.contains(case.result_text),
            "fragment response for {}",
            case.path
        );
        let mismatch = get(
            &app,
            case.path,
            &[("HX-Request", "true"), ("HX-Target", case.mismatch_target)],
        )
        .await;
        assert_full_layout(&mismatch);
        assert!(
            mismatch.contains(case.result_text),
            "mismatched target response for {}",
            case.path
        );
    }
}

#[tokio::test]
async fn detail_tab_routes_return_layout_or_fragments_by_route_shape() {
    let app = setup().await;
    let cases = [
        TabCase {
            full_path: "/users/1500000000000000001?tab=applications",
            fragment_path: "/users/1500000000000000001/tabs/applications",
            detail_text: "SearchedUser",
            tab_text: "Mock Application",
        },
        TabCase {
            full_path: "/guilds/1600000000000000001?tab=applications",
            fragment_path: "/guilds/1600000000000000001/tabs/applications",
            detail_text: "Searched Guild",
            tab_text: "Mock Application",
        },
    ];
    for case in cases {
        let full = get(&app, case.full_path, &[]).await;
        assert_full_layout(&full);
        assert!(
            full.contains(case.detail_text),
            "full tab URL for {}",
            case.full_path
        );
        assert!(
            full.contains(case.tab_text),
            "full tab URL for {}",
            case.full_path
        );
        let fragment = get(&app, case.fragment_path, &[]).await;
        assert_fragment(&fragment);
        assert!(
            fragment.contains(case.tab_text),
            "tab endpoint fragment for {}",
            case.fragment_path
        );
    }
}

#[tokio::test]
async fn report_routes_keep_layout_and_fragment_contract() {
    let app = setup().await;
    let reports = get(&app, "/reports?q=mock", &[]).await;
    assert_full_layout(&reports);
    assert!(reports.contains("1800000000000000001"), "{reports}");
    assert!(
        !reports.contains(r#"hx-post="/reports/bulk-resolve""#),
        "{reports}"
    );
    assert!(!reports.contains(r#"name="report_ids[]""#), "{reports}");
    let boosted = get(
        &app,
        "/reports?q=mock",
        &[
            ("HX-Request", "true"),
            ("HX-Boosted", "true"),
            ("HX-Target", "body"),
        ],
    )
    .await;
    assert_full_layout(&boosted);
    assert!(boosted.contains("1800000000000000001"), "{boosted}");
    let detail = get(&app, "/reports/1800000000000000001", &[]).await;
    assert_full_layout(&detail);
    assert!(detail.contains("Mock report details"), "{detail}");
    let fragment = get(&app, "/reports/1800000000000000001/fragment", &[]).await;
    assert_fragment(&fragment);
    assert!(fragment.contains("Mock report details"), "{fragment}");

    let message_fragment = get(&app, "/reports/1800000000000000002/fragment", &[]).await;
    assert_fragment(&message_fragment);
    assert!(
        message_fragment.contains("Message Context"),
        "{message_fragment}"
    );
    assert!(
        message_fragment.contains("Reported message in drawer"),
        "{message_fragment}"
    );
}

#[tokio::test]
async fn user_fragment_alias_returns_drawer_fragment() {
    let app = setup().await;
    let fragment = get(&app, "/users/1500000000000000001/fragment", &[]).await;

    assert_fragment(&fragment);
    assert!(fragment.contains("SearchedUser"), "{fragment}");
}

#[tokio::test]
async fn drawer_triggers_use_htmx_and_native_popover() {
    let app = setup().await;
    let body = get(&app, "/users?ids=1500000000000000001", &[]).await;

    assert!(body.contains(r#"popovertarget="user-peek""#), "{body}");
    assert!(body.contains(r##"hx-target="#user-peek-body""##), "{body}");
    assert!(!body.contains("__fluxerDrawerInit"), "{body}");
}

#[tokio::test]
async fn jobs_poll_with_htmx_fragment_instead_of_fetch_loop() {
    let app = setup().await;
    let body = get(&app, "/jobs?status=running", &[]).await;

    assert_full_layout(&body);
    assert!(body.contains(r#"id="jobs-results""#), "{body}");
    assert!(body.contains(r#"hx-trigger="every 3s""#), "{body}");
    assert!(body.contains("mockJobSync"), "{body}");
    assert!(body.contains(r##"hx-target="#main-content""##), "{body}");
    assert!(body.contains(r#"hx-swap="innerHTML""#), "{body}");
    assert!(body.contains(r#"hx-push-url="true""#), "{body}");
    assert!(!body.contains("/jobs/active.json"), "{body}");

    let fragment = get(
        &app,
        "/jobs?status=running",
        &[("HX-Request", "true"), ("HX-Target", "jobs-results")],
    )
    .await;
    assert_fragment(&fragment);
    assert!(fragment.contains(r#"id="jobs-results""#), "{fragment}");
    assert!(fragment.contains("mockJobSync"), "{fragment}");
}

#[tokio::test]
async fn detail_pages_return_fragment_for_main_content_htmx_target() {
    let app = setup().await;

    let guild_full = get(&app, "/guilds/1600000000000000001", &[]).await;
    assert_full_layout(&guild_full);
    assert!(
        guild_full.contains("Searched Guild"),
        "guild detail full: {guild_full}"
    );

    let guild_htmx = get(
        &app,
        "/guilds/1600000000000000001",
        &[("HX-Request", "true"), ("HX-Target", "main-content")],
    )
    .await;
    assert_fragment(&guild_htmx);
    assert!(
        guild_htmx.contains("Searched Guild"),
        "guild detail htmx fragment: {guild_htmx}"
    );

    let report_full = get(&app, "/reports/1800000000000000001", &[]).await;
    assert_full_layout(&report_full);

    let report_htmx = get(
        &app,
        "/reports/1800000000000000001",
        &[("HX-Request", "true"), ("HX-Target", "main-content")],
    )
    .await;
    assert_fragment(&report_htmx);

    let job_full = get(&app, "/jobs/1900000000000000001", &[]).await;
    assert_full_layout(&job_full);
    assert!(
        job_full.contains("mockJobSync"),
        "job detail full: {job_full}"
    );

    let job_htmx = get(
        &app,
        "/jobs/1900000000000000001",
        &[("HX-Request", "true"), ("HX-Target", "main-content")],
    )
    .await;
    assert_fragment(&job_htmx);
    assert!(
        job_htmx.contains("mockJobSync"),
        "job detail htmx fragment: {job_htmx}"
    );
}

#[tokio::test]
async fn user_account_actions_use_no_swap_htmx_toasts() {
    let app = setup().await;
    let account_path = "/users/1500000000000000001?tab=account";
    let (headers, body) = get_with_headers(&app, account_path, &[]).await;
    let native_confirm = format!("{}{}", "confirm", "(");
    assert_full_layout(&body);
    assert!(body.contains("__fluxerAdminActionForms"), "{body}");
    assert!(!body.contains(&native_confirm), "{body}");
    assert!(
        body.contains(r#"hx-post="/users/1500000000000000001?action=update_has_verified_phone&amp;tab=account""#),
        "{body}"
    );
    assert!(body.contains(r##"hx-target="#flash-container""##), "{body}");
    assert!(body.contains(r#"hx-swap="none""#), "{body}");
    assert!(body.contains(r#"hx-push-url="false""#), "{body}");

    let csrf_token = csrf_cookie(&headers)
        .unwrap_or_else(|| panic!("account page did not set csrf_token cookie\n{body}"));
    let (status, response_headers, response_body) = post_form_with_headers(
        &app,
        "/users/1500000000000000001?action=update_has_verified_phone&tab=account",
        &[
            ("HX-Request", "true"),
            ("HX-Target", "flash-container"),
            (
                "Cookie",
                &format!("{}; csrf_token={}", app.session_cookie, csrf_token),
            ),
        ],
        &format!("_csrf={csrf_token}&has_verified_phone=true"),
    )
    .await;
    assert_eq!(status, StatusCode::NO_CONTENT, "{response_body}");
    assert_eq!(
        response_headers
            .get("HX-Reswap")
            .and_then(|value| value.to_str().ok()),
        Some("none")
    );
    let toast = response_headers
        .get("X-Fluxer-Admin-Toast")
        .and_then(|value| value.to_str().ok())
        .unwrap_or_else(|| panic!("missing toast header\n{response_body}"));
    assert!(toast.contains("success"), "{toast}");
    assert!(
        toast.contains("Phone verification status updated successfully"),
        "{toast}"
    );
}

#[tokio::test]
async fn redirect_flash_actions_convert_to_htmx_toasts() {
    let app = setup().await;
    let report_path = "/reports/1800000000000000001";
    let (headers, body) = get_with_headers(&app, report_path, &[]).await;
    let native_confirm = format!("{}{}", "confirm", "(");
    assert_full_layout(&body);
    assert!(!body.contains(&native_confirm), "{body}");
    let csrf_token = csrf_cookie(&headers)
        .unwrap_or_else(|| panic!("report page did not set csrf_token cookie\n{body}"));

    let (status, response_headers, response_body) = post_form_with_headers(
        &app,
        "/reports/1800000000000000001/resolve",
        &[
            ("HX-Request", "true"),
            ("HX-Target", "flash-container"),
            (
                "Cookie",
                &format!("{}; csrf_token={}", app.session_cookie, csrf_token),
            ),
        ],
        &format!("_csrf={csrf_token}&resolution=done"),
    )
    .await;
    assert_eq!(status, StatusCode::NO_CONTENT, "{response_body}");
    assert_eq!(
        response_headers
            .get("HX-Reswap")
            .and_then(|value| value.to_str().ok()),
        Some("none")
    );
    let toast = response_headers
        .get("X-Fluxer-Admin-Toast")
        .and_then(|value| value.to_str().ok())
        .unwrap_or_else(|| panic!("missing toast header\n{response_body}"));
    assert!(toast.contains("success"), "{toast}");
    assert!(toast.contains("Report resolved"), "{toast}");
}

#[tokio::test]
async fn mutating_admin_pages_render_usable_csrf_tokens() {
    let app = setup().await;
    let cases = [
        ("/system-dms", &["/system-dms"][..]),
        (
            "/messages",
            &[
                "/messages?action=browse",
                "/messages?action=lookup",
                "/messages?action=lookup-by-attachment",
                "/messages?action=delete",
            ][..],
        ),
        ("/gift-codes", &["/gift-codes"][..]),
        ("/search-index", &["/search-index?action=reindex"][..]),
        ("/gateway", &["/gateway?action=reload_all"][..]),
        (
            "/instance-config",
            &[
                "/instance-config?action=update_gateway_rollout",
                "/instance-config?action=update_sso",
            ][..],
        ),
    ];

    for (path, form_actions) in cases {
        let (headers, body) = get_with_headers(&app, path, &[]).await;
        let csrf_token = csrf_cookie(&headers)
            .unwrap_or_else(|| panic!("{path}: response did not set csrf_token cookie\n{body}"));
        for form_action in form_actions {
            assert_form_has_csrf(&body, form_action, &csrf_token);
        }
    }
}

#[tokio::test]
async fn hosted_instance_config_hides_self_host_setup_controls() {
    let app = setup().await;
    let body = get(&app, "/instance-config", &[]).await;

    assert_full_layout(&body);
    assert!(body.contains("Registration Controls"), "{body}");
    assert!(body.contains("Runtime Integrations"), "{body}");
    assert!(body.contains("Gateway Rollout Configuration"), "{body}");
    assert!(!body.contains("Public App Identity"), "{body}");
    assert!(!body.contains("Setup complete"), "{body}");
    assert!(!body.contains("Community & Policy"), "{body}");
    assert!(!body.contains("Single community"), "{body}");
    assert!(!body.contains("Direct messages &amp; friends"), "{body}");
    assert!(!body.contains("Premium model"), "{body}");
    assert!(!body.contains("Optional services"), "{body}");
    assert!(!body.contains("Registration Fields"), "{body}");
    assert!(
        !body.contains("Collect date of birth during registration"),
        "{body}"
    );
    assert!(
        !body.contains("/instance-config?action=update_app_public"),
        "{body}"
    );
    assert!(
        !body.contains("/instance-config?action=update_app_registration"),
        "{body}"
    );
    assert!(
        !body.contains("/instance-config?action=update_policy"),
        "{body}"
    );
}

#[tokio::test]
async fn instance_config_registration_tables_show_copyable_urls_and_compact_pending_actions() {
    let app = setup().await;
    let body = get(&app, "/instance-config", &[]).await;

    assert_full_layout(&body);
    assert!(body.contains(r#"id="registration-url-list""#), "{body}");
    assert!(
        body.contains(
            r#"value="https://app.example.test/register?registration_url=11111111-1111-4111-8111-111111111111""#
        ),
        "{body}"
    );
    assert!(
        body.contains(
            r#"data-copy-value="https://app.example.test/register?registration_url=11111111-1111-4111-8111-111111111111""#
        ),
        "{body}"
    );
    assert!(body.contains(r#"id="pending-registration-list""#), "{body}");
    assert!(body.contains(">Applicant<"), "{body}");
    assert!(!body.contains(">User ID<"), "{body}");
    assert!(!body.contains(">Link ID<"), "{body}");
    assert!(
        body.contains(r##"hx-target="#pending-registration-list""##),
        "{body}"
    );
    assert!(body.contains(r#"hx-swap="outerHTML""#), "{body}");
}

#[tokio::test]
async fn pending_registration_actions_swap_pending_list_fragment() {
    let app = setup().await;
    let (headers, body) = get_with_headers(&app, "/instance-config", &[]).await;
    let csrf_token = csrf_cookie(&headers)
        .unwrap_or_else(|| panic!("instance config did not set csrf_token cookie\n{body}"));

    let (status, response_headers, response_body) = post_form_with_headers(
        &app,
        "/instance-config?action=approve_pending_registration",
        &[
            ("HX-Request", "true"),
            ("HX-Target", "pending-registration-list"),
            (
                "Cookie",
                &format!("{}; csrf_token={}", app.session_cookie, csrf_token),
            ),
        ],
        &format!("_csrf={csrf_token}&user_id=1500000000000000002"),
    )
    .await;

    assert_eq!(status, StatusCode::OK, "{response_body}");
    assert_fragment(&response_body);
    assert!(response_body.contains(r#"id="pending-registration-list""#));
    assert!(response_body.contains("No pending registrations."));
    assert!(!response_body.contains("PendingUser"), "{response_body}");
    let toast = response_headers
        .get("X-Fluxer-Admin-Toast")
        .and_then(|value| value.to_str().ok())
        .unwrap_or_else(|| panic!("missing toast header\n{response_body}"));
    assert!(toast.contains("success"), "{toast}");
    assert!(toast.contains("Registration approved"), "{toast}");
}

#[tokio::test]
async fn creating_registration_url_swaps_copyable_url_list_fragment() {
    let app = setup().await;
    let (headers, body) = get_with_headers(&app, "/instance-config", &[]).await;
    let csrf_token = csrf_cookie(&headers)
        .unwrap_or_else(|| panic!("instance config did not set csrf_token cookie\n{body}"));

    let (status, response_headers, response_body) = post_form_with_headers(
        &app,
        "/instance-config?action=create_registration_url",
        &[
            ("HX-Request", "true"),
            ("HX-Target", "registration-url-list"),
            (
                "Cookie",
                &format!("{}; csrf_token={}", app.session_cookie, csrf_token),
            ),
        ],
        &format!("_csrf={csrf_token}&registration_url_label=Support&registration_url_max_uses=1"),
    )
    .await;

    assert_eq!(status, StatusCode::OK, "{response_body}");
    assert_fragment(&response_body);
    assert!(response_body.contains(r#"id="registration-url-list""#));
    assert!(
        response_body.contains(
            "https://app.example.test/register?registration_url=11111111-1111-4111-8111-111111111111"
        ),
        "{response_body}"
    );
    let toast = response_headers
        .get("X-Fluxer-Admin-Toast")
        .and_then(|value| value.to_str().ok())
        .unwrap_or_else(|| panic!("missing toast header\n{response_body}"));
    assert!(toast.contains("success"), "{toast}");
    assert!(toast.contains("Registration URL created"), "{toast}");
}

struct SearchCase {
    path: &'static str,
    result_target: &'static str,
    mismatch_target: &'static str,
    result_text: &'static str,
}

struct TabCase {
    full_path: &'static str,
    fragment_path: &'static str,
    detail_text: &'static str,
    tab_text: &'static str,
}

async fn setup() -> TestApp {
    let api_endpoint = spawn_mock_api().await;
    let router = build_router(test_config(api_endpoint));
    let session_value = session::create_session("1500000000000000000", "test-token", SECRET_KEY);
    TestApp {
        router,
        session_cookie: format!("{}={session_value}", session::SESSION_COOKIE_NAME),
    }
}

async fn get(app: &TestApp, uri: &str, headers: &[(&str, &str)]) -> String {
    get_with_headers(app, uri, headers).await.1
}

async fn get_with_headers(
    app: &TestApp,
    uri: &str,
    headers: &[(&str, &str)],
) -> (HeaderMap, String) {
    let mut builder = Request::builder()
        .method(Method::GET)
        .uri(uri)
        .header(header::COOKIE, &app.session_cookie);
    for (name, value) in headers {
        builder = builder.header(*name, *value);
    }
    let response = app
        .router
        .clone()
        .oneshot(builder.body(Body::empty()).unwrap())
        .await
        .unwrap();
    let status = response.status();
    let headers = response.headers().clone();
    let body = to_bytes(response.into_body(), usize::MAX).await.unwrap();
    let text = String::from_utf8(body.to_vec()).unwrap();
    assert_eq!(status, StatusCode::OK, "{text}");
    (headers, text)
}

async fn post_form_with_headers(
    app: &TestApp,
    uri: &str,
    headers: &[(&str, &str)],
    body: &str,
) -> (StatusCode, HeaderMap, String) {
    let has_cookie_header = headers
        .iter()
        .any(|(name, _)| name.eq_ignore_ascii_case("cookie"));
    let mut builder = Request::builder()
        .method(Method::POST)
        .uri(uri)
        .header(header::CONTENT_TYPE, "application/x-www-form-urlencoded");
    if !has_cookie_header {
        builder = builder.header(header::COOKIE, &app.session_cookie);
    }
    for (name, value) in headers {
        builder = builder.header(*name, *value);
    }
    let response = app
        .router
        .clone()
        .oneshot(builder.body(Body::from(body.to_owned())).unwrap())
        .await
        .unwrap();
    let status = response.status();
    let headers = response.headers().clone();
    let body = to_bytes(response.into_body(), usize::MAX).await.unwrap();
    let text = String::from_utf8(body.to_vec()).unwrap();
    (status, headers, text)
}

fn csrf_cookie(headers: &HeaderMap) -> Option<String> {
    headers
        .get_all(header::SET_COOKIE)
        .iter()
        .filter_map(|value| value.to_str().ok())
        .find_map(|value| {
            value
                .split(';')
                .next()
                .and_then(|pair| pair.strip_prefix("csrf_token="))
                .map(str::to_owned)
        })
}

fn assert_form_has_csrf(body: &str, action: &str, csrf_token: &str) {
    let marker = format!(r#"action="{action}""#);
    let Some(action_index) = body.find(&marker) else {
        panic!("missing form action {action:?}\n{body}");
    };
    let csrf_marker = format!(r#"name="_csrf" value="{csrf_token}""#);
    let tail = &body[action_index..];
    let form = tail.find("</form>").map(|end| &tail[..end]).unwrap_or(tail);
    assert!(
        form.contains(&csrf_marker),
        "form {action:?} did not include matching CSRF token {csrf_token:?}\n{body}"
    );
}

fn assert_full_layout(body: &str) {
    assert!(body.contains(r#"id="admin-sidebar""#), "{body}");
    assert!(body.contains(r#"id="main-content""#), "{body}");
}

fn assert_fragment(body: &str) {
    assert!(!body.contains(r#"id="admin-sidebar""#), "{body}");
    assert!(!body.contains(r#"id="main-content""#), "{body}");
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
        (Method::GET, "/admin/users/me") => json_response(json!({ "user": admin_user() })),
        (Method::GET, "/admin/api-keys") => json_response(json!([])),
        (Method::POST, "/admin/api-keys") => json_response(json!({
            "key_id": "1900000000000000001",
            "key": ADMIN_API_KEY_SECRET,
            "name": "Acceptance key",
            "created_at": "2026-07-10T15:00:00.000Z",
            "expires_at": null,
            "acls": ["*"]
        })),
        (Method::POST, "/admin/users/search") => {
            json_response(json!({ "users": [searched_user()], "total": 1 }))
        }
        (Method::POST, "/admin/users/lookup") => {
            json_response(json!({ "users": [searched_user()] }))
        }
        (Method::POST, "/admin/users/update-has-verified-phone") => {
            json_response(json!({ "user": searched_user() }))
        }
        (Method::POST, "/admin/guilds/search") => {
            json_response(json!({ "guilds": [searched_guild()], "total": 1 }))
        }
        (Method::POST, "/admin/guilds/lookup") => {
            json_response(json!({ "guild": searched_guild_detail() }))
        }
        (Method::POST, "/admin/applications/lookup") => {
            json_response(json!({ "application": searched_application() }))
        }
        (Method::POST, "/admin/applications/list-by-owner") => {
            json_response(json!({ "applications": [searched_application()] }))
        }
        (Method::POST, "/admin/reports/search") => json_response(
            json!({ "reports": [searched_report()], "total": 1, "offset": 0, "limit": 25 }),
        ),
        (Method::GET, "/admin/reports/1800000000000000001") => json_response(searched_report()),
        (Method::GET, "/admin/reports/1800000000000000002") => {
            json_response(searched_message_report())
        }
        (Method::POST, "/admin/reports/resolve") => json_response(json!({
            "report_id": "1800000000000000001",
            "status": 1,
            "resolved_at": "2026-05-26T12:03:00.000Z",
            "public_comment": "done"
        })),
        (Method::POST, "/admin/jobs/list") => {
            json_response(json!({ "jobs": [searched_job()], "next_cursor": null, "cursor": null }))
        }
        (Method::POST, "/admin/jobs/get") => json_response(json!({ "job": searched_job() })),
        (Method::POST, "/admin/instance-config/get") => json_response(instance_config()),
        (Method::POST, "/admin/instance-config/registration-urls/create") => json_response(json!({
            "registration_url": registration_url_fixture(),
            "code": "11111111-1111-4111-8111-111111111111",
            "url": "https://app.example.test/register?registration_url=11111111-1111-4111-8111-111111111111"
        })),
        (Method::POST, "/admin/instance-config/registration-urls/revoke") => {
            json_response(instance_config_without_registration_urls())
        }
        (Method::POST, "/admin/instance-config/pending-registrations/approve") => {
            json_response(instance_config_without_pending_registrations())
        }
        (Method::POST, "/admin/instance-config/pending-registrations/reject") => {
            json_response(instance_config_without_pending_registrations())
        }
        (Method::POST, "/admin/limit-config/get") => json_response(limit_config()),
        _ => (StatusCode::NOT_FOUND, Json(json!({ "error": "not found" }))).into_response(),
    }
}

fn json_response(value: Value) -> Response {
    Json(value).into_response()
}

fn admin_user() -> Value {
    let mut user = user("1500000000000000000", "AdminUser");
    user["acls"] = json!(["*"]);
    user
}

fn searched_user() -> Value {
    user("1500000000000000001", "SearchedUser")
}

fn user(id: &str, username: &str) -> Value {
    json!({
        "id": id,
        "username": username,
        "discriminator": 1,
        "avatar": null,
        "banner": null,
        "email": "admin@example.com",
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
        "suspicious_activity_flags": 0,
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

fn searched_guild() -> Value {
    json!({
        "id": "1600000000000000001",
        "name": "Searched Guild",
        "icon": null,
        "banner": null,
        "owner_id": "1500000000000000001",
        "owner_username": "SearchedUser",
        "owner_global_name": "SearchedUser",
        "owner_discriminator": "0001",
        "member_count": 12,
        "features": ["COMMUNITY"],
        "nsfw_level": 0,
        "nsfw": false,
        "content_warning_level": null,
        "content_warning_text": null,
        "description": "Guild used by HTMX acceptance tests.",
        "vanity_url_code": null
    })
}

fn searched_guild_detail() -> Value {
    json!({
        "id": "1600000000000000001",
        "owner_id": "1500000000000000001",
        "owner_username": "SearchedUser",
        "owner_global_name": "SearchedUser",
        "owner_discriminator": "0001",
        "name": "Searched Guild",
        "vanity_url_code": null,
        "icon": null,
        "banner": null,
        "splash": null,
        "embed_splash": null,
        "features": ["COMMUNITY"],
        "verification_level": 1,
        "mfa_level": 0,
        "nsfw_level": 0,
        "nsfw": false,
        "content_warning_level": null,
        "content_warning_text": null,
        "explicit_content_filter": 0,
        "default_message_notifications": 0,
        "afk_channel_id": null,
        "afk_timeout": 0,
        "system_channel_id": null,
        "system_channel_flags": 0,
        "rules_channel_id": null,
        "disabled_operations": 0,
        "member_count": 12,
        "channels": [],
        "roles": [],
        "description": "Guild used by HTMX acceptance tests."
    })
}

fn searched_application() -> Value {
    json!({
        "id": "1700000000000000001",
        "name": "Mock Application",
        "owner_user_id": "1500000000000000001",
        "owner_username": "SearchedUser",
        "owner_global_name": "SearchedUser",
        "owner_discriminator": "0001",
        "bot_user_id": null,
        "bot_username": null,
        "bot_global_name": null,
        "bot_discriminator": null,
        "bot_is_public": true,
        "bot_require_code_grant": false,
        "oauth2_redirect_uris": [],
        "has_client_secret": false,
        "has_bot_token": false,
        "bot_token_preview": null,
        "bot_token_created_at": null,
        "client_secret_created_at": null,
        "version": 1
    })
}

fn searched_report() -> Value {
    json!({
        "report_id": "1800000000000000001",
        "reporter_id": "1500000000000000000",
        "reporter_tag": "AdminUser#0001",
        "reported_at": "2026-05-26T12:00:00.000Z",
        "status": 0,
        "report_type": 1,
        "category": "other",
        "additional_info": "Mock report details",
        "reported_user_id": "1500000000000000001",
        "reported_user_tag": "SearchedUser#0001"
    })
}

fn searched_message_report() -> Value {
    json!({
        "report_id": "1800000000000000002",
        "reporter_id": "1500000000000000000",
        "reporter_tag": "AdminUser#0001",
        "reported_at": "2026-05-26T12:00:00.000Z",
        "status": 0,
        "report_type": 0,
        "category": "spam",
        "additional_info": "Mock message report details",
        "reported_user_id": "1500000000000000001",
        "reported_user_tag": "SearchedUser#0001",
        "reported_message_id": "1800000000000001001",
        "reported_channel_id": "1600000000000000101",
        "reported_channel_name": "general",
        "message_context": [{
            "id": "1800000000000001001",
            "content": "Reported message in drawer",
            "timestamp": "2026-05-26T12:01:00.000Z",
            "author_id": "1500000000000000001",
            "author_username": "SearchedUser",
            "author_global_name": "SearchedUser",
            "author_discriminator": "0001",
            "author_avatar": null,
            "channel_id": "1600000000000000101",
            "attachments": []
        }]
    })
}

fn searched_job() -> Value {
    json!({
        "job_id": "1900000000000000001",
        "task_type": "mockJobSync",
        "status": "running",
        "created_at": "2026-05-26T12:00:00.000Z",
        "progress_current": 4,
        "progress_total": 10,
        "progress_message": null,
        "error_message": null,
        "started_at": "2026-05-26T12:00:05.000Z",
        "completed_at": null,
        "attempts": 1,
        "max_attempts": 3,
        "requested_by_user_id": "1500000000000000000",
        "audit_log_reason": null,
        "jet_stream_lane": null,
        "jet_stream_seq": null,
        "run_at": null,
        "cancel_requested": false,
        "context_link": null,
        "payload": null,
        "result": null
    })
}

fn instance_config() -> Value {
    json!({
        "sso": {
            "enabled": false,
            "enforced": false,
            "display_name": null,
            "issuer": null,
            "authorization_url": null,
            "token_url": null,
            "userinfo_url": null,
            "jwks_url": null,
            "client_id": null,
            "client_secret_set": false,
            "scope": null,
            "allowed_domains": [],
            "auto_provision": false,
            "redirect_uri": "https://admin.example.test/oauth2_callback"
        },
        "gateway_rollout": {
            "session_rollout_percentage": 100,
            "session_rollout_mode": "modulo",
            "guild_rollout_percentage": 100,
            "rpc_request_timeout_ms": 5000,
            "max_concurrent_session_starts": 16,
            "max_concurrent_guild_starts": 16,
            "voice_e2ee_scope": "guild_feature_only"
        },
        "registration": registration_config(),
        "self_hosted": false
    })
}

fn registration_config() -> Value {
    json!({
        "mode": "approval",
        "admin_registration_urls_enabled": true,
        "urls": [registration_url_fixture()],
        "pending_registrations": [pending_registration_fixture()]
    })
}

fn registration_url_fixture() -> Value {
    json!({
        "id": "11111111-1111-4111-8111-111111111111",
        "label": "Support batch",
        "created_by_user_id": "1500000000000000000",
        "created_at": "2026-05-26T12:00:00.000Z",
        "expires_at": null,
        "max_uses": 5,
        "use_count": 0,
        "revoked_at": null,
        "approval_required": true,
        "last_used_at": null,
        "last_used_by_user_id": null
    })
}

fn pending_registration_fixture() -> Value {
    json!({
        "user_id": "1500000000000000002",
        "username": "PendingUser",
        "discriminator": 0,
        "global_name": "Pending User",
        "email": "pending.user.with.a.long.address@example.test",
        "requested_at": "2026-05-26T12:10:00.000Z",
        "registration_url_id": "11111111-1111-4111-8111-111111111111",
        "client_ip": "203.0.113.24"
    })
}

fn instance_config_without_pending_registrations() -> Value {
    let mut config = instance_config();
    config["registration"]["pending_registrations"] = json!([]);
    config
}

fn instance_config_without_registration_urls() -> Value {
    let mut config = instance_config();
    config["registration"]["urls"] = json!([]);
    config
}

fn limit_config() -> Value {
    json!({
        "limit_config": {
            "traitDefinitions": [],
            "rules": [{
                "id": "default",
                "filters": null,
                "limits": { "maxGuilds": 100 }
            }]
        },
        "limit_config_json": "{\"traitDefinitions\":[],\"rules\":[]}",
        "self_hosted": false,
        "defaults": {
            "default": { "maxGuilds": 100 }
        },
        "metadata": {
            "maxGuilds": {
                "key": "maxGuilds",
                "label": "Max Guilds",
                "description": "Maximum guild memberships.",
                "category": "account",
                "scope": "user",
                "isToggle": false,
                "unit": null,
                "min": 0,
                "max": 1000
            }
        },
        "categories": { "account": "Account" },
        "limit_keys": ["maxGuilds"],
        "bounds": null
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
