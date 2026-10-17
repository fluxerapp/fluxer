// SPDX-License-Identifier: AGPL-3.0-or-later

#![recursion_limit = "256"]

use axum::{
    Json, Router,
    body::{Body, to_bytes},
    extract::{DefaultBodyLimit, Path},
    http::{HeaderMap, Method, Request, StatusCode, Uri, header},
    response::{IntoResponse, Response},
    routing,
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

const SECRET_KEY: &str = "htmx-acceptance-test-secret";

struct TestApp {
    router: Router,
    session_cookie: String,
}

#[tokio::test]
async fn report_legal_hold_form_places_and_clears_the_hold() {
    let received = Arc::new(Mutex::new(Vec::<Value>::new()));
    let sink = received.clone();
    let api = Router::new()
        .route(
            "/admin/reports/1800000000000000001/legal-hold",
            routing::post(move |Json(body): Json<Value>| {
                let sink = sink.clone();
                async move {
                    sink.lock().unwrap().push(body.clone());
                    if body["legal_hold_reason"] == "rejected" {
                        return (
                            StatusCode::BAD_REQUEST,
                            Json(json!({"code": "INVALID_FORM_BODY", "message": "Invalid"})),
                        )
                            .into_response();
                    }
                    json_response(json!({
                        "report_id": "1800000000000000001",
                        "legal_hold_until": body["legal_hold_until"],
                        "legal_hold_reason": body["legal_hold_reason"]
                    }))
                }
            }),
        )
        .fallback(mock_api);
    let app = setup_with_api(api).await;
    let (headers, page) = get_with_headers(&app, "/reports/1800000000000000001", &[]).await;
    let csrf_token = csrf_cookie(&headers)
        .unwrap_or_else(|| panic!("report page did not set csrf_token cookie\n{page}"));
    assert_form_has_csrf(
        &page,
        "/reports/1800000000000000001/legal-hold",
        &csrf_token,
    );
    let cookie = format!("{}; csrf_token={}", app.session_cookie, csrf_token);
    let post = |body: String| {
        let app = &app;
        let cookie = cookie.clone();
        async move {
            let (status, headers, text) = post_form_with_headers(
                app,
                "/reports/1800000000000000001/legal-hold",
                &[
                    ("HX-Request", "true"),
                    ("HX-Target", "flash-container"),
                    ("Cookie", &cookie),
                ],
                &body,
            )
            .await;
            assert_eq!(status, StatusCode::NO_CONTENT, "{text}");
            headers
                .get("X-Fluxer-Admin-Toast")
                .and_then(|value| value.to_str().ok())
                .unwrap_or_else(|| panic!("missing toast header\n{text}"))
                .to_owned()
        }
    };

    let toast = post(format!(
        "_csrf={csrf_token}&legal_hold_until=2027-01-31&legal_hold_reason=Court+order+42"
    ))
    .await;
    assert!(toast.contains("success"), "{toast}");
    assert!(
        toast.contains("Legal hold placed until Jan 31, 2027, 11:59 PM UTC"),
        "{toast}"
    );
    let toast = post(format!(
        "_csrf={csrf_token}&clear=1&legal_hold_reason=ignored"
    ))
    .await;
    assert!(toast.contains("Legal hold cleared"), "{toast}");
    let toast = post(format!(
        "_csrf={csrf_token}&legal_hold_until=2027-01-31&legal_hold_reason=rejected"
    ))
    .await;
    assert!(toast.contains("error"), "{toast}");
    assert!(toast.contains("The hold must end in the future"), "{toast}");
    assert!(!toast.contains("needs a reason"), "{toast}");
    let toast = post(format!(
        "_csrf={csrf_token}&legal_hold_until=2020-01-01&legal_hold_reason=Past+date"
    ))
    .await;
    assert!(toast.contains("error"), "{toast}");
    assert!(toast.contains("The hold must end in the future"), "{toast}");
    {
        let received = received.lock().unwrap();
        assert_eq!(
            *received,
            vec![
                json!({"legal_hold_until": "2027-01-31T23:59:59.999Z", "legal_hold_reason": "Court order 42"}),
                json!({"legal_hold_until": null, "legal_hold_reason": null}),
                json!({"legal_hold_until": "2027-01-31T23:59:59.999Z", "legal_hold_reason": "rejected"}),
            ]
        );
    }

    let toast = post(format!(
        "_csrf={csrf_token}&legal_hold_until=2027-01-31&legal_hold_reason=+"
    ))
    .await;
    assert!(toast.contains("Give a reason for the hold"), "{toast}");
    let toast = post(format!(
        "_csrf={csrf_token}&legal_hold_until=soon&legal_hold_reason=x"
    ))
    .await;
    assert!(toast.contains("Choose the date the hold ends"), "{toast}");
    assert_eq!(received.lock().unwrap().len(), 3);
}

#[tokio::test]
async fn report_notes_show_the_author_and_time_and_the_form_adds_one() {
    let received = Arc::new(Mutex::new(Vec::<Value>::new()));
    let sink = received.clone();
    let api = Router::new()
        .route(
            "/admin/reports/1800000000000000001",
            routing::get(|| async { json_response(report_with_note()) }),
        )
        .route(
            "/admin/reports/1800000000000000001/notes",
            routing::post(move |Json(body): Json<Value>| {
                let sink = sink.clone();
                async move {
                    sink.lock().unwrap().push(body.clone());
                    json_response(json!({
                        "note_id": "1950000000000000002",
                        "author_id": "1500000000000000000",
                        "body": body["body"],
                        "created_at": "2026-05-27T08:00:00.000Z"
                    }))
                }
            }),
        )
        .route(
            "/admin/users",
            routing::get(|| async {
                json_response(json!({ "users": [user("1500000000000000000", "AdminUser")] }))
            }),
        )
        .fallback(mock_api);
    let app = setup_with_api(api).await;
    let (headers, page) = get_with_headers(&app, "/reports/1800000000000000001", &[]).await;
    let note_start = page
        .find(r#"data-note="1950000000000000001""#)
        .unwrap_or_else(|| panic!("missing note\n{page}"));
    let note = &page[note_start..page[note_start..].find("</li>").unwrap() + note_start];
    assert!(
        note.contains(r#"href="/users/1500000000000000000""#),
        "{note}"
    );
    assert!(note.contains("AdminUser"), "{note}");
    assert!(note.contains("May 26, 2026, 1:30 PM UTC"), "{note}");
    assert!(
        note.contains("Reporter sent more context by email"),
        "{note}"
    );
    let csrf_token = csrf_cookie(&headers)
        .unwrap_or_else(|| panic!("report page did not set csrf_token cookie\n{page}"));
    assert_form_has_csrf(&page, "/reports/1800000000000000001/notes", &csrf_token);
    let cookie = format!("{}; csrf_token={}", app.session_cookie, csrf_token);
    let post = |body: String| {
        let app = &app;
        let cookie = cookie.clone();
        async move {
            let (status, headers, text) = post_form_with_headers(
                app,
                "/reports/1800000000000000001/notes",
                &[
                    ("HX-Request", "true"),
                    ("HX-Target", "flash-container"),
                    ("Cookie", &cookie),
                ],
                &body,
            )
            .await;
            assert_eq!(status, StatusCode::NO_CONTENT, "{text}");
            headers
                .get("X-Fluxer-Admin-Toast")
                .and_then(|value| value.to_str().ok())
                .unwrap_or_else(|| panic!("missing toast header\n{text}"))
                .to_owned()
        }
    };

    let toast = post(format!(
        "_csrf={csrf_token}&body=++Waiting+on+the+guild+owner++"
    ))
    .await;
    assert!(toast.contains("success"), "{toast}");
    assert!(toast.contains("Note added"), "{toast}");
    let toast = post(format!("_csrf={csrf_token}&body=+")).await;
    assert!(toast.contains("error"), "{toast}");
    assert!(toast.contains("Write a note first"), "{toast}");
    assert_eq!(
        *received.lock().unwrap(),
        vec![json!({"body": "Waiting on the guild owner"})]
    );
}

#[tokio::test]
async fn report_notes_hide_the_form_without_report_resolve() {
    let api = Router::new()
        .route(
            "/admin/users/@me",
            routing::get(|| async {
                let mut admin = user("1500000000000000000", "AdminUser");
                admin["acls"] = json!(["report:view"]);
                json_response(json!({ "user": admin }))
            }),
        )
        .route(
            "/admin/reports/1800000000000000001",
            routing::get(|| async { json_response(report_with_note()) }),
        )
        .fallback(mock_api);
    let app = setup_with_api(api).await;
    let (_, page) = get_with_headers(&app, "/reports/1800000000000000001", &[]).await;
    assert!(
        page.contains("Reporter sent more context by email"),
        "{page}"
    );
    assert!(
        page.contains(r#"href="/users/1500000000000000000""#),
        "{page}"
    );
    assert!(
        !page.contains(r#"action="/reports/1800000000000000001/notes""#),
        "{page}"
    );
}

#[tokio::test]
async fn report_delete_form_confirms_sends_the_reason_and_toasts_a_held_report() {
    let received = Arc::new(Mutex::new(Vec::<Option<String>>::new()));
    let sink = received.clone();
    let api = Router::new()
        .route(
            "/admin/reports/1800000000000000001",
            routing::delete(move |headers: HeaderMap| {
                let sink = sink.clone();
                async move {
                    let reason = headers
                        .get("X-Audit-Log-Reason")
                        .and_then(|value| value.to_str().ok())
                        .map(str::to_owned);
                    sink.lock().unwrap().push(reason.clone());
                    if reason.as_deref() == Some("held") {
                        return (
                            StatusCode::CONFLICT,
                            Json(json!({
                                "code": "REPORT_UNDER_LEGAL_HOLD",
                                "message": "This report is under a legal hold and can't be deleted."
                            })),
                        )
                            .into_response();
                    }
                    StatusCode::NO_CONTENT.into_response()
                }
            })
            .get(|| async { json_response(searched_report()) }),
        )
        .fallback(mock_api);
    let app = setup_with_api(api).await;
    let (headers, page) = get_with_headers(&app, "/reports/1800000000000000001", &[]).await;
    let csrf_token = csrf_cookie(&headers)
        .unwrap_or_else(|| panic!("report page did not set csrf_token cookie\n{page}"));
    assert_form_has_csrf(&page, "/reports/1800000000000000001/delete", &csrf_token);
    let cookie = format!("{}; csrf_token={}", app.session_cookie, csrf_token);
    let post = |body: String| {
        let app = &app;
        let cookie = cookie.clone();
        async move {
            post_form_with_headers(
                app,
                "/reports/1800000000000000001/delete",
                &[
                    ("HX-Request", "true"),
                    ("HX-Target", "flash-container"),
                    ("Cookie", &cookie),
                ],
                &body,
            )
            .await
        }
    };
    let toast = |headers: &HeaderMap| {
        headers
            .get("X-Fluxer-Admin-Toast")
            .and_then(|value| value.to_str().ok())
            .unwrap_or_else(|| panic!("missing toast header {headers:?}"))
            .to_owned()
    };

    let (status, headers, text) = post(format!(
        "_csrf={csrf_token}&audit_log_reason=Erasure+request"
    ))
    .await;
    assert_eq!(status, StatusCode::NO_CONTENT, "{text}");
    let message = toast(&headers);
    assert!(message.contains("error"), "{message}");
    assert!(
        message.contains("Confirm that the report should be deleted"),
        "{message}"
    );
    assert!(headers.get("HX-Redirect").is_none(), "{headers:?}");

    let (status, headers, text) = post(format!(
        "_csrf={csrf_token}&confirm=true&audit_log_reason=Erasure+request"
    ))
    .await;
    assert_eq!(status, StatusCode::NO_CONTENT, "{text}");
    assert_eq!(
        headers.get("HX-Redirect").and_then(|v| v.to_str().ok()),
        Some("/reports"),
        "{headers:?}"
    );
    let message = toast(&headers);
    assert!(message.contains("success"), "{message}");
    assert!(message.contains("Report deleted"), "{message}");
    assert!(
        headers
            .get_all(header::SET_COOKIE)
            .iter()
            .any(|value| value.to_str().is_ok_and(|v| v.starts_with("flash="))),
        "{headers:?}"
    );

    let (status, headers, text) = post(format!(
        "_csrf={csrf_token}&confirm=true&audit_log_reason=held"
    ))
    .await;
    assert_eq!(status, StatusCode::NO_CONTENT, "{text}");
    assert!(headers.get("HX-Redirect").is_none(), "{headers:?}");
    let message = toast(&headers);
    assert!(message.contains("error"), "{message}");
    assert!(
        message.contains("The report is under a legal hold. Clear the hold before deleting it."),
        "{message}"
    );

    let (status, headers, text) = post_form_with_headers(
        &app,
        "/reports/1800000000000000001/delete",
        &[("Cookie", &cookie)],
        &format!("_csrf={csrf_token}&confirm=true&audit_log_reason=+"),
    )
    .await;
    assert!(status.is_redirection(), "{status} {text}");
    assert_eq!(
        headers.get(header::LOCATION).and_then(|v| v.to_str().ok()),
        Some("/reports"),
        "{headers:?}"
    );

    assert_eq!(
        *received.lock().unwrap(),
        vec![
            Some("Erasure request".to_owned()),
            Some("held".to_owned()),
            None
        ]
    );
}

#[tokio::test]
async fn pending_registration_decision_refreshes_the_list_and_sidebar_count() {
    let received = Arc::new(Mutex::new(Vec::<(String, Value)>::new()));
    let sink = received.clone();
    let api = Router::new()
        .route(
            "/admin/users/@me",
            routing::get(|| async {
                json_response(json!({"user": admin_user(), "pending_registration_count": 2}))
            }),
        )
        .route(
            "/admin/instance/pending-registrations",
            routing::get(|| async {
                json_response(json!({"pending_registrations": [
                    pending_registration("1500000000000000002"),
                    pending_registration("1500000000000000003"),
                ]}))
            }),
        )
        .route(
            "/admin/instance/pending-registrations/{user_id}",
            routing::patch(
                move |Path(user_id): Path<String>, Json(body): Json<Value>| {
                    let sink = sink.clone();
                    async move {
                        sink.lock().unwrap().push((user_id, body));
                        json_response(json!({"pending_registrations": [
                            pending_registration("1500000000000000003"),
                        ]}))
                    }
                },
            ),
        )
        .fallback(mock_api);
    let app = setup_with_api(api).await;
    let (headers, page) = get_with_headers(&app, "/pending-registrations", &[]).await;
    assert!(
        page.contains(r#"<span id="pending-registration-count" class="tabular-nums text-neutral-400">2</span>"#),
        "{page}"
    );
    assert!(page.contains("ID: 1500000000000000002"), "{page}");
    let csrf_token = csrf_cookie(&headers).unwrap_or_else(|| {
        panic!("pending registrations page did not set csrf_token cookie\n{page}")
    });
    assert_form_has_csrf(&page, "/pending-registrations/approve", &csrf_token);
    assert_form_has_csrf(&page, "/pending-registrations/reject", &csrf_token);

    let cookie = format!("{}; csrf_token={}", app.session_cookie, csrf_token);
    let (status, headers, fragment) = post_form_with_headers(
        &app,
        "/pending-registrations/approve",
        &[
            ("HX-Request", "true"),
            ("HX-Target", "pending-registration-list"),
            ("Cookie", &cookie),
        ],
        &format!("_csrf={csrf_token}&user_id=1500000000000000002"),
    )
    .await;
    assert_eq!(status, StatusCode::OK, "{fragment}");
    assert!(
        fragment.contains(r#"id="pending-registration-list""#),
        "{fragment}"
    );
    assert!(!fragment.contains("ID: 1500000000000000002"), "{fragment}");
    assert!(fragment.contains("ID: 1500000000000000003"), "{fragment}");
    assert!(
        fragment.contains(r#"hx-swap-oob="true">1</span>"#),
        "{fragment}"
    );
    let toast = headers
        .get("X-Fluxer-Admin-Toast")
        .and_then(|value| value.to_str().ok())
        .unwrap_or_default();
    assert!(toast.contains("Registration approved"), "{toast}");
    assert_eq!(
        *received.lock().unwrap(),
        vec![(
            "1500000000000000002".to_owned(),
            json!({"status": "approved"})
        )]
    );
}

#[tokio::test]
async fn branding_asset_upload_takes_a_large_image_and_swaps_the_field() {
    let received = Arc::new(Mutex::new(Vec::<Value>::new()));
    let sink = received.clone();
    let api = Router::new()
        .route(
            "/admin/instance/config",
            routing::get(|| async {
                json_response(instance_config(Some(
                    "https://media.example.test/branding/0/old.png",
                )))
            }),
        )
        .route(
            "/admin/instance/config/branding-assets",
            routing::post(move |Json(body): Json<Value>| {
                let sink = sink.clone();
                async move {
                    sink.lock().unwrap().push(body.clone());
                    if body["image"] == "data:image/png;base64,bad" {
                        return (
                            StatusCode::BAD_REQUEST,
                            Json(json!({
                                "code": "INVALID_FORM_BODY",
                                "message": "Input form validation failed",
                                "errors": [{"path": "image", "message": "Unsupported image format"}]
                            })),
                        )
                            .into_response();
                    }
                    let logo_url = body["image"]
                        .is_string()
                        .then_some("https://media.example.test/branding/0/new.png");
                    json_response(instance_config(logo_url))
                }
            }),
        )
        .layer(DefaultBodyLimit::disable())
        .fallback(mock_api);
    let app = setup_with_api(api).await;
    let (headers, page) = get_with_headers(&app, "/instance-config", &[]).await;
    let csrf_token = csrf_cookie(&headers)
        .unwrap_or_else(|| panic!("instance config page did not set csrf_token cookie\n{page}"));
    assert_form_has_csrf(
        &page,
        "/instance-config?action=update_app_public",
        &csrf_token,
    );
    assert!(page.contains(r#"id="branding-asset-logo""#), "{page}");
    assert!(page.contains(r#"id="branding-asset-favicon""#), "{page}");
    assert!(
        page.contains(r#"src="https://media.example.test/branding/0/old.png""#),
        "{page}"
    );
    assert!(page.contains(r#"accept=".png,.jpeg,.webp"#), "{page}");

    let cookie = format!("{}; csrf_token={}", app.session_cookie, csrf_token);
    let post = |action: &'static str, fields: Vec<(&'static str, String)>| {
        let app = &app;
        let cookie = cookie.clone();
        let csrf_token = csrf_token.clone();
        async move {
            let mut body = url::form_urlencoded::Serializer::new(String::new());
            body.append_pair("_csrf", &csrf_token);
            for (name, value) in &fields {
                body.append_pair(name, value);
            }
            post_form_with_headers(
                app,
                &format!("/instance-config?action={action}"),
                &[
                    ("HX-Request", "true"),
                    ("HX-Target", "branding-asset-logo"),
                    ("X-CSRF-Token", &csrf_token),
                    ("Cookie", &cookie),
                ],
                &body.finish(),
            )
            .await
        }
    };
    let toast_of = |headers: &HeaderMap| {
        headers
            .get("X-Fluxer-Admin-Toast")
            .and_then(|value| value.to_str().ok())
            .unwrap_or_default()
            .to_owned()
    };

    let image = format!(
        "data:image/png;base64,{}",
        "iVBO+w/A".repeat(2 * 1024 * 1024 / 8)
    );
    let (status, headers, fragment) = post(
        "upload_branding_asset",
        vec![("kind", "logo".to_owned()), ("image", image.clone())],
    )
    .await;
    assert_eq!(status, StatusCode::OK, "{fragment}");
    assert!(
        fragment.starts_with(r#"<div id="branding-asset-logo""#),
        "{fragment}"
    );
    assert!(
        fragment.contains(r#"value="https://media.example.test/branding/0/new.png""#),
        "{fragment}"
    );
    assert!(
        fragment.contains("data-branding-asset-remove"),
        "{fragment}"
    );
    assert!(toast_of(&headers).contains("Logo uploaded"));

    let (status, headers, fragment) =
        post("remove_branding_asset", vec![("kind", "logo".to_owned())]).await;
    assert_eq!(status, StatusCode::OK, "{fragment}");
    assert!(
        fragment.contains(r#"name="app_logo_url" value="""#),
        "{fragment}"
    );
    assert!(!fragment.contains("<img"), "{fragment}");
    assert!(
        !fragment.contains("data-branding-asset-remove"),
        "{fragment}"
    );
    assert!(toast_of(&headers).contains("Logo removed"));

    let (status, headers, _) = post(
        "upload_branding_asset",
        vec![
            ("kind", "logo".to_owned()),
            ("image", "data:image/png;base64,bad".to_owned()),
        ],
    )
    .await;
    assert_eq!(status, StatusCode::NO_CONTENT);
    let toast = toast_of(&headers);
    assert!(toast.contains("error"), "{toast}");
    assert!(
        toast.contains("Failed to upload the logo: image: Unsupported image format"),
        "{toast}"
    );

    let (_, headers, _) = post(
        "upload_branding_asset",
        vec![("kind", "logo".to_owned()), ("image", " ".to_owned())],
    )
    .await;
    assert!(toast_of(&headers).contains("Choose an image to upload"));
    let (_, headers, _) = post(
        "upload_branding_asset",
        vec![("kind", "banner".to_owned()), ("image", image.clone())],
    )
    .await;
    assert!(toast_of(&headers).contains("Unknown branding asset"));
    let (_, headers, _) = post(
        "update_app_public",
        vec![
            ("app_product_name", "Fluxer".to_owned()),
            ("app_logo_url", image.clone()),
        ],
    )
    .await;
    assert!(toast_of(&headers).contains("Invalid form data"));

    assert_eq!(
        *received.lock().unwrap(),
        vec![
            json!({"kind": "logo", "image": image}),
            json!({"kind": "logo", "image": null}),
            json!({"kind": "logo", "image": "data:image/png;base64,bad"}),
        ]
    );
}

fn instance_config(logo_url: Option<&str>) -> Value {
    json!({
        "sso": {},
        "gateway_rollout": {
            "session_rollout_percentage": 100,
            "session_rollout_mode": "modulo",
            "guild_rollout_percentage": 100,
            "rpc_request_timeout_ms": 10000,
            "max_concurrent_session_starts": 512,
            "max_concurrent_guild_starts": 256,
            "voice_e2ee_scope": "guild_feature_only"
        },
        "self_hosted": true,
        "app_public": {"branding": {"product_name": "Fluxer", "logo_url": logo_url}}
    })
}

async fn setup_with_api(api: Router) -> TestApp {
    let api_endpoint = spawn_mock_api(api).await;
    let router = build_router(test_config(api_endpoint));
    let session_value = session::create_session("1500000000000000000", "test-token", SECRET_KEY);
    TestApp {
        router,
        session_cookie: format!("{}={session_value}", session::SESSION_COOKIE_NAME),
    }
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
            let pair = value.split(';').next()?;
            let token = pair
                .strip_prefix("__Host-csrf_token=")
                .or_else(|| pair.strip_prefix("csrf_token="))?;
            (!token.is_empty()).then(|| token.to_owned())
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

async fn spawn_mock_api(api: Router) -> String {
    let listener = TcpListener::bind(("127.0.0.1", 0)).await.unwrap();
    let addr = listener.local_addr().unwrap();
    tokio::spawn(async move {
        axum::serve(listener, api).await.unwrap();
    });
    format!("http://{addr}")
}

async fn mock_api(method: Method, uri: Uri) -> Response {
    match (method, uri.path()) {
        (Method::GET, "/admin/users/@me") => json_response(json!({ "user": admin_user() })),
        (Method::GET, "/admin/report-reasons") => json_response(report_reasons()),
        (Method::GET, "/admin/reports/1800000000000000001") => json_response(searched_report()),
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

fn pending_registration(user_id: &str) -> Value {
    json!({
        "user_id": user_id,
        "username": "applicant",
        "discriminator": 0,
        "global_name": null,
        "email": "applicant@example.com",
        "requested_at": "2026-10-14T12:00:00.000Z",
        "registration_url_id": null,
        "client_ip": "203.0.113.7"
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

fn report_with_note() -> Value {
    let mut report = searched_report();
    report["notes"] = json!([{
        "note_id": "1950000000000000001",
        "author_id": "1500000000000000000",
        "body": "Reporter sent more context by email",
        "created_at": "2026-05-26T13:30:00.000Z"
    }]);
    report
}

fn report_reasons() -> Value {
    json!({
        "reasons": [
            {
                "key": "csam",
                "label": "Child sexual abuse material",
                "highest_priority": true,
                "legacy_category_message": "child_safety",
                "legacy_category_user": "child_safety",
                "legacy_category_guild": "child_safety"
            },
            {
                "key": "harassment",
                "label": "Harassment or bullying",
                "highest_priority": false,
                "legacy_category_message": "harassment",
                "legacy_category_user": "harassment",
                "legacy_category_guild": "harassment"
            }
        ]
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
