// SPDX-License-Identifier: AGPL-3.0-or-later

#![recursion_limit = "256"]

use axum::{
    Json, Router,
    body::{Body, to_bytes},
    http::{HeaderMap, Method, Request, StatusCode, Uri, header},
    response::{IntoResponse, Response},
    routing,
};
use fluxer_admin::{
    api::{generated::types as generated_types, types::LookupGuildResponse},
    build_router,
    config::{AdminConfig, ProxyConfig, RuntimeEnv},
    session,
};
use serde_json::{Value, json};
use std::sync::{
    Arc, Mutex,
    atomic::{AtomicUsize, Ordering},
};
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
async fn target_audit_log_tabs_request_write_entries_only() {
    let app = setup().await;
    for path in [
        "/users/1500000000000000001/tabs/audit_logs",
        "/guilds/1600000000000000001/tabs/audit_logs",
    ] {
        let fragment = get(&app, path, &[]).await;
        assert!(fragment.contains("Temp ban"), "{path}\n{fragment}");
        assert!(!fragment.contains("Get user"), "{path}\n{fragment}");
    }
}

#[tokio::test]
async fn audit_log_page_forwards_the_access_filter() {
    let app = setup().await;
    let all = get(&app, "/audit-logs", &[]).await;
    assert!(all.contains("Temp ban"), "{all}");
    assert!(all.contains("Get user"), "{all}");
    assert!(
        all.contains(r#"<option value="" selected>All entries</option>"#),
        "{all}"
    );

    let reads = get(&app, "/audit-logs?access=read", &[]).await;
    assert!(reads.contains("Get user"), "{reads}");
    assert!(!reads.contains("Temp ban"), "{reads}");
    assert!(
        reads.contains(r#"<option value="read" selected>Reads only</option>"#),
        "{reads}"
    );
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
async fn report_list_reason_select_is_rendered_and_wired() {
    let app = setup().await;
    let unfiltered = get(&app, "/reports", &[]).await;
    assert!(
        unfiltered.contains(r#"<select id="reason" name="reason""#),
        "{unfiltered}"
    );
    assert!(
        unfiltered.contains(r#"<option value="" selected>All</option><option value="csam">Priority: Child sexual abuse material</option><option value="harassment">Harassment or bullying</option>"#),
        "{unfiltered}"
    );
    assert!(unfiltered.contains("1800000000000000001"), "{unfiltered}");
    assert!(!unfiltered.contains("1800000000000000003"), "{unfiltered}");

    let filtered = get(&app, "/reports?reason=csam", &[]).await;
    assert_full_layout(&filtered);
    assert!(
        filtered.contains(
            r#"<option value="csam" selected>Priority: Child sexual abuse material</option>"#
        ),
        "{filtered}"
    );
    assert!(filtered.contains("1800000000000000003"), "{filtered}");
    assert!(!filtered.contains("1800000000000000001"), "{filtered}");
    assert!(
        filtered.contains(r#"data-report-reason="csam""#),
        "{filtered}"
    );
    assert!(
        filtered.contains("Child sexual abuse material"),
        "{filtered}"
    );
    let next = pagination_href(&filtered, "Next");
    assert!(next.contains("reason=csam"), "{next}");
    assert!(next.contains("page=1"), "{next}");

    let second = get(&app, "/reports?reason=csam&page=1", &[]).await;
    let previous = pagination_href(&second, "Previous");
    assert!(previous.contains("reason=csam"), "{previous}");
    assert!(previous.contains("page=0"), "{previous}");
}

#[tokio::test]
async fn report_list_hides_the_reason_select_until_the_reason_list_loads() {
    let calls = Arc::new(AtomicUsize::new(0));
    let reasons_calls = calls.clone();
    let api = Router::new()
        .route(
            "/admin/report-reasons",
            routing::get(move || {
                let calls = reasons_calls.clone();
                async move {
                    if calls.fetch_add(1, Ordering::SeqCst) == 0 {
                        (StatusCode::NOT_FOUND, Json(json!({"code": "NOT_FOUND"}))).into_response()
                    } else {
                        json_response(report_reasons())
                    }
                }
            }),
        )
        .fallback(mock_api);
    let app = setup_with_api(api).await;

    let without = get(&app, "/reports?reason=csam", &[]).await;
    assert_full_layout(&without);
    assert!(!without.contains(r#"name="reason""#), "{without}");
    assert!(without.contains(r#"name="category""#), "{without}");
    assert!(without.contains("1800000000000000003"), "{without}");
    assert!(
        pagination_href(&without, "Next").contains("reason=csam"),
        "{without}"
    );

    for _ in 0..2 {
        let with = get(&app, "/reports", &[]).await;
        assert!(with.contains(r#"name="reason""#), "{with}");
    }
    assert_eq!(calls.load(Ordering::SeqCst), 2);
}

#[tokio::test]
async fn report_detail_shows_answers_for_flow_reports_only() {
    let app = setup().await;
    let v2 = get(&app, "/reports/1800000000000000003", &[]).await;
    assert_full_layout(&v2);
    for expected in [
        ">Reason<",
        r#"data-report-reason="csam""#,
        "Highest priority",
        "Report Answers",
        "Report message: Abusive or harmful content",
        "What private information is shared?: Email address, Phone number",
        r#"data-report-answer-step="profile_intro""#,
        "Shown to the reporter in fr",
        "Form revision",
        "b7667e8b32c98c40",
        "Surface: DSA form",
        "Good-faith statement: Confirmed",
    ] {
        assert!(v2.contains(expected), "{expected}\n{v2}");
    }
    let v2_fragment = get(&app, "/reports/1800000000000000003/fragment", &[]).await;
    assert_fragment(&v2_fragment);
    assert!(v2_fragment.contains(">Reason<"), "{v2_fragment}");
    assert!(v2_fragment.contains("Report Answers"), "{v2_fragment}");

    let legacy = get(&app, "/reports/1800000000000000001", &[]).await;
    assert!(!legacy.contains("Report Answers"), "{legacy}");
    assert!(!legacy.contains(">Reason<"), "{legacy}");
}

#[tokio::test]
async fn report_category_filter_keeps_an_unknown_value_and_labels_rows() {
    let app = setup().await;
    let unknown = get(&app, "/reports?category=future_value", &[]).await;
    assert_full_layout(&unknown);
    let select = select_markup(&unknown, "category");
    assert!(
        select.contains(r#"<option value="future_value" selected>future_value</option>"#),
        "{select}"
    );
    assert!(
        !select.contains(r#"<option value="" selected>"#),
        "{select}"
    );
    assert!(
        unknown.contains(r#"data-report-category="future_value""#),
        "{unknown}"
    );
    assert!(unknown.contains("1800000000000000006"), "{unknown}");
    let next = pagination_href(&unknown, "Next");
    assert!(next.contains("category=future_value"), "{next}");

    let known = get(&app, "/reports?category=spam", &[]).await;
    let select = select_markup(&known, "category");
    assert!(
        select.contains(r#"<option value="spam" selected>Spam</option>"#),
        "{select}"
    );
    assert!(!select.contains("Spam or Scam"), "{select}");
    assert_eq!(select.matches("<option").count(), 19, "{select}");
    assert!(known.contains(r#"data-report-category="other""#), "{known}");
    assert!(known.contains(">Other<"), "{known}");

    let detail = get(&app, "/reports/1800000000000000001", &[]).await;
    assert!(
        detail.contains(r#"data-report-category="other""#),
        "{detail}"
    );

    for path in [
        "/users/1500000000000000001/tabs/reports",
        "/guilds/1600000000000000001/tabs/reports",
    ] {
        let tab = get(&app, path, &[]).await;
        assert!(
            tab.contains(r#"data-report-category="other""#),
            "{path}\n{tab}"
        );
        assert!(tab.contains(">Other<"), "{path}\n{tab}");
    }
}

#[tokio::test]
async fn report_detail_renders_unknown_status_and_type_values() {
    let app = setup().await;
    let detail = get(&app, "/reports/1800000000000000005", &[]).await;
    assert_full_layout(&detail);
    assert!(detail.contains("Report Details"), "{detail}");
    assert!(detail.contains("1800000000000000005"), "{detail}");
    assert!(detail.contains(">Unknown<"), "{detail}");
    let fragment = get(&app, "/reports/1800000000000000005/fragment", &[]).await;
    assert_fragment(&fragment);
    assert!(!fragment.contains("Failed to load report."), "{fragment}");
    assert!(fragment.contains("1800000000000000005"), "{fragment}");
}

#[tokio::test]
async fn ncmec_submissions_forward_the_source_report_id() {
    let received = Arc::new(Mutex::new(Vec::<Value>::new()));
    let sink = received.clone();
    let api = Router::new()
        .route(
            "/admin/messages/ncmec-reports",
            routing::post(move |Json(body): Json<Value>| {
                let sink = sink.clone();
                async move {
                    sink.lock().unwrap().push(body);
                    json_response(json!({
                        "success": true,
                        "ncmec_report_id": "ncmec-1",
                        "audit_log_reason": "Submitted"
                    }))
                }
            }),
        )
        .fallback(mock_api);
    let app = setup_with_api(api).await;
    let (headers, body) = get_with_headers(&app, "/messages", &[]).await;
    let csrf_token = csrf_cookie(&headers)
        .unwrap_or_else(|| panic!("messages page did not set csrf_token cookie\n{body}"));
    let cookie = format!("{}; csrf_token={}", app.session_cookie, csrf_token);
    let fields = "channel_id=1600000000000000101&message_id=1800000000000001001\
        &attachment_id=1800000000000001002&filename=image.png\
        &reporter_full_name=Casey%20Admin&confirmed_viewed=true";
    for form in [
        format!("_csrf={csrf_token}&{fields}&source_report_id=1800000000000000009"),
        format!("_csrf={csrf_token}&{fields}"),
    ] {
        let (status, _, response) = post_form_with_headers(
            &app,
            "/messages?action=report-to-ncmec",
            &[("Cookie", &cookie), ("x-csrf-token", &csrf_token)],
            &form,
        )
        .await;
        assert_eq!(status, StatusCode::OK, "{response}");
        assert!(response.contains("ncmec-1"), "{response}");
    }
    let received = received.lock().unwrap();
    assert_eq!(received.len(), 2, "{received:?}");
    assert_eq!(received[0]["source_report_id"], "1800000000000000009");
    assert_eq!(received[0]["attachment_id"], "1800000000000001002");
    assert_eq!(received[0]["confirmed_viewed"], true);
    assert!(
        received[1]
            .get("source_report_id")
            .is_none_or(Value::is_null),
        "{received:?}"
    );
}

#[tokio::test]
async fn message_tools_offer_ncmec_only_with_every_required_acl() {
    let full = fluxer_admin::acl::NCMEC_ATTACHMENT_REPORT;
    let cases: [(&[&str], bool, bool); 4] = [
        (full, true, true),
        (&["*"], true, true),
        (&["csam:submit_ncmec", "message:lookup"], false, false),
        (
            &[
                "csam:submit_ncmec",
                "message:delete",
                "message:lookup",
                "user:delete",
            ],
            false,
            true,
        ),
    ];
    for (acls, ncmec, delete) in cases {
        let app = setup_with_api(message_tools_api(acls)).await;
        for path in [
            "/messages?channel_id=1600000000000000101&message_id=1800000000000001001",
            "/messages/browse-fragment?channel_id=1600000000000000101",
        ] {
            let body = get(&app, path, &[]).await;
            assert!(body.contains("tools-image.png"), "{acls:?} {path}\n{body}");
            assert_eq!(
                body.contains(r#"class="ncmec-report-btn"#),
                ncmec,
                "{acls:?} {path}\n{body}"
            );
            assert_eq!(
                body.contains(r#"class="delete-message-btn"#),
                delete,
                "{acls:?} {path}\n{body}"
            );
            assert!(
                !body.contains("data-source-report-id"),
                "{acls:?} {path}\n{body}"
            );
        }
    }
}

#[tokio::test]
async fn webhook_reports_show_the_webhook_and_filter_by_it() {
    let app = setup().await;
    let list = get(
        &app,
        &format!("/reports?reported_webhook_id={WEBHOOK_ID}"),
        &[],
    )
    .await;
    assert_full_layout(&list);
    assert!(
        list.contains(&format!(
            r#"id="reported_webhook_id" name="reported_webhook_id" value="{WEBHOOK_ID}""#
        )),
        "{list}"
    );
    assert!(list.contains("1800000000000000004"), "{list}");
    assert!(!list.contains("1800000000000000001"), "{list}");
    assert!(
        list.contains(&format!(r#"data-report-webhook="{WEBHOOK_ID}""#)),
        "{list}"
    );
    assert!(list.contains("Harbor Bulletin"), "{list}");
    assert!(list.contains(">Webhook<"), "{list}");
    assert!(
        list.contains(&format!("Webhook ID: {WEBHOOK_ID}")),
        "{list}"
    );
    assert!(
        list.contains("https://media.example.test/avatars/1700000000000000500/abc123"),
        "{list}"
    );
    assert!(list.contains("Channel: general"), "{list}");
    assert!(
        list.contains(r#"href="/users/1500000000000000002""#),
        "{list}"
    );
    assert!(list.contains("Webhook creator: HookOwner#0002"), "{list}");
    assert!(!list.contains(&format!("/users/{WEBHOOK_ID}")), "{list}");
    assert!(!list.contains("User unknown"), "{list}");
    let next = pagination_href(&list, "Next");
    assert!(
        next.contains(&format!("reported_webhook_id={WEBHOOK_ID}")),
        "{next}"
    );

    let unfiltered = get(&app, "/reports", &[]).await;
    assert!(
        unfiltered.contains(r#"id="reported_webhook_id" name="reported_webhook_id" value="""#),
        "{unfiltered}"
    );
    assert!(!unfiltered.contains("1800000000000000004"), "{unfiltered}");

    let detail = get(&app, "/reports/1800000000000000004", &[]).await;
    let fragment = get(&app, "/reports/1800000000000000004/fragment", &[]).await;
    assert_full_layout(&detail);
    assert_fragment(&fragment);
    for body in [&detail, &fragment] {
        for expected in [
            format!(r#"data-report-webhook="{WEBHOOK_ID}""#),
            ">Webhook ID<".to_owned(),
            format!(r#"href="/reports?reported_webhook_id={WEBHOOK_ID}""#),
            "general".to_owned(),
            format!(r#"data-message-webhook="{WEBHOOK_ID}""#),
            "Webhook spam in drawer".to_owned(),
            r#"href="/users/1500000000000000001""#.to_owned(),
            ">Webhook Creator<".to_owned(),
            r#"data-report-webhook-creator="1500000000000000002""#.to_owned(),
            r#"href="/users/1500000000000000002""#.to_owned(),
            "HookOwner#0002".to_owned(),
            ">Configured Name<".to_owned(),
            "Harbor Hook".to_owned(),
            ">Configured Avatar<".to_owned(),
            ">Webhook Type<".to_owned(),
            ">Incoming<".to_owned(),
            ">Webhook Created<".to_owned(),
            "May 20, 2026, 8:30 AM UTC".to_owned(),
        ] {
            assert!(body.contains(&expected), "{expected}\n{body}");
        }
        assert!(!body.contains(&format!("/users/{WEBHOOK_ID}")), "{body}");
        assert!(!body.contains("View Reported User"), "{body}");
        for absent in [
            ">Webhook Channel ID<",
            ">Webhook Guild ID<",
            ">Application ID<",
            ">Webhook Record<",
            "data-report-webhook-creator-deleted",
            "data-report-webhook-creator-bot",
        ] {
            assert!(!body.contains(absent), "{absent}\n{body}");
        }
    }
}

fn evidence_report() -> Value {
    json!({
        "report_id": "1800000000000000010",
        "reporter_id": "1500000000000000000",
        "reporter_tag": "AdminUser#0001",
        "reported_at": "2026-10-04T10:00:00.000Z",
        "status": 0,
        "report_type": 0,
        "category": "spam",
        "reason": "spam",
        "reason_label": "Spam",
        "reason_highest_priority": false,
        "additional_info": null,
        "reported_user_id": "1500000000000000001",
        "reported_user_tag": "SearchedUser#0001",
        "reported_user_username": "SearchedUser",
        "reported_user_discriminator": "0001",
        "reported_user_bot": true,
        "reported_message_id": "1800000000000001001",
        "reported_channel_id": "1600000000000000101",
        "reported_profile_snapshot": {
            "captured_at": "2026-10-04T10:00:00.000Z",
            "user": {
                "id": "1500000000000000001",
                "username": "SearchedUser",
                "discriminator": "1",
                "global_name": "SearchedUser",
                "bio": "Original bio",
                "pronouns": null,
                "avatar": {"hash": "avatar1", "url": "https://reports.example.test/avatar1?sig=abc"},
                "banner": null
            },
            "member": null,
            "guild": null
        },
        "message_context": [
            {
                "id": "1800000000000001001",
                "channel_id": "1600000000000000101",
                "channel_nsfw": false,
                "guild_id": null,
                "guild_nsfw_level": null,
                "content": "Buy now",
                "timestamp": "2026-10-04T10:00:00.000Z",
                "attachments": [],
                "author_id": "1500000000000000001",
                "author_username": "SearchedUser",
                "author_global_name": null,
                "author_discriminator": "0001",
                "author_avatar": null,
                "webhook_id": null,
                "author_bot": true,
                "missing_attachments": [{
                    "id": "1800000000000001002",
                    "filename": "flyer.png",
                    "nsfw": null,
                    "content_type": "image/png",
                    "width": 640,
                    "height": 480,
                    "size": 4096
                }],
                "user_prior_ncmec_report_ids": []
            },
            {
                "id": "1800000000000000999",
                "channel_id": "1600000000000000101",
                "channel_nsfw": false,
                "guild_id": null,
                "guild_nsfw_level": null,
                "content": "hello",
                "timestamp": "2026-10-04T09:59:00.000Z",
                "attachments": [],
                "author_id": "1500000000000000000",
                "author_username": "AdminUser",
                "author_global_name": null,
                "author_discriminator": "0001",
                "author_avatar": null,
                "webhook_id": null,
                "author_bot": false,
                "missing_attachments": [],
                "user_prior_ncmec_report_ids": []
            }
        ]
    })
}

fn evidence_api() -> Router {
    Router::new()
        .route(
            "/admin/reports",
            routing::get(|| async {
                json_response(json!({
                    "reports": [evidence_report(), searched_report()],
                    "total": 2,
                    "offset": 0,
                    "limit": 25
                }))
            }),
        )
        .route(
            "/admin/reports/1800000000000000010",
            routing::get(|| async { json_response(evidence_report()) }),
        )
        .fallback(mock_api)
}

#[tokio::test]
async fn report_pages_show_bot_badges_missing_attachments_and_the_profile_snapshot() {
    let app = setup_with_api(evidence_api()).await;
    let list = get(&app, "/reports", &[]).await;
    assert!(
        list.contains(r#"data-report-user-bot="1500000000000000001""#),
        "{list}"
    );
    assert_eq!(list.matches("data-report-user-bot").count(), 1, "{list}");

    let detail = get(&app, "/reports/1800000000000000010", &[]).await;
    assert_full_layout(&detail);
    for expected in [
        r#"data-report-user-bot="1500000000000000001""#,
        r#"data-message-author-bot="1500000000000000001""#,
        r#"data-missing-attachment="1800000000000001002""#,
        "flyer.png was not preserved in the report snapshot",
        "At Report Time",
        "Original bio",
        r#"src="https://reports.example.test/avatar1?sig=abc""#,
        r#"data-snapshot-field="user.bio" data-snapshot-changed"#,
        r#"data-snapshot-field="user.avatar" data-snapshot-changed"#,
        r#"data-snapshot-field="user.username">"#,
        r#"data-report-legal-hold="none""#,
        "Include the public comment in the reporter notice",
    ] {
        assert!(detail.contains(expected), "{expected}\n{detail}");
    }
    assert_eq!(
        detail.matches("data-message-author-bot").count(),
        1,
        "{detail}"
    );

    let fragment = get(&app, "/reports/1800000000000000010/fragment", &[]).await;
    assert_fragment(&fragment);
    assert!(
        fragment.contains("flyer.png was not preserved in the report snapshot"),
        "{fragment}"
    );
    assert!(!fragment.contains("At Report Time"), "{fragment}");

    let legacy = get(&app, "/reports/1800000000000000001", &[]).await;
    assert!(!legacy.contains("At Report Time"), "{legacy}");
    assert!(!legacy.contains("data-report-user-bot"), "{legacy}");
    assert!(!legacy.contains("data-missing-attachment"), "{legacy}");
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
async fn user_fragment_alias_returns_drawer_fragment() {
    let app = setup().await;
    let fragment = get(&app, "/users/1500000000000000001/fragment", &[]).await;

    assert_fragment(&fragment);
    assert!(fragment.contains("SearchedUser"), "{fragment}");
}

#[tokio::test]
async fn user_peek_alias_is_gone() {
    let app = setup().await;

    assert_eq!(
        get_status(&app, "/users/1500000000000000001/peek").await,
        StatusCode::NOT_FOUND
    );
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
        body.contains(
            r#"hx-post="/users/1500000000000000001?action=send_password_reset&amp;tab=account""#
        ),
        "{body}"
    );
    assert!(body.contains(r##"hx-target="#flash-container""##), "{body}");
    assert!(body.contains(r#"hx-swap="none""#), "{body}");
    assert!(body.contains(r#"hx-push-url="false""#), "{body}");

    let csrf_token = csrf_cookie(&headers)
        .unwrap_or_else(|| panic!("account page did not set csrf_token cookie\n{body}"));
    let (status, response_headers, response_body) = post_form_with_headers(
        &app,
        "/users/1500000000000000001?action=send_password_reset&tab=account",
        &[
            ("HX-Request", "true"),
            ("HX-Target", "flash-container"),
            (
                "Cookie",
                &format!("{}; csrf_token={}", app.session_cookie, csrf_token),
            ),
        ],
        &format!("_csrf={csrf_token}"),
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
        toast.contains("Password reset sent successfully"),
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
async fn report_actions_toast_the_server_message_and_refresh_the_detail() {
    let app = setup().await;
    let page = get(&app, "/reports/1800000000000000001", &[]).await;
    assert_full_layout(&page);
    assert!(page.contains(r#"id="report-detail""#), "{page}");
    for action in ["resolve", "legal-hold"] {
        let form = page
            .split("<form ")
            .find(|form| {
                form.contains(&format!(
                    r#"action="/reports/1800000000000000001/{action}""#
                ))
            })
            .unwrap_or_else(|| panic!("no {action} form\n{page}"));
        let tag = &form[..form.find('>').unwrap_or(form.len())];
        assert!(
            tag.contains(r##"data-admin-refresh-on-success="#report-detail""##),
            "{tag}"
        );
        assert!(!tag.contains("data-admin-result-form"), "{tag}");
    }
    let before_swap = page
        .split("htmx:beforeSwap")
        .nth(1)
        .and_then(|rest| rest.split("htmx:afterRequest").next())
        .unwrap_or_else(|| panic!("no beforeSwap handler\n{page}"));
    let header = before_swap
        .find("parseAdminToastHeader(xhr)")
        .unwrap_or_else(|| panic!("beforeSwap ignores the toast header\n{before_swap}"));
    let body = before_swap
        .find("parseFlashResponse(")
        .unwrap_or_else(|| panic!("beforeSwap has no body fallback\n{before_swap}"));
    assert!(header < body, "{before_swap}");
    assert!(
        before_swap.contains("refreshAfterSuccess("),
        "{before_swap}"
    );
    assert!(
        page.contains("if (status >= 400) level = 'error';"),
        "{page}"
    );
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
                "/instance-config?action=update_domain_migration",
                "/instance-config?action=update_experiment_delivery",
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
async fn instance_config_has_no_threads_or_plutonium_page_rollout_sections() {
    let app = setup().await;
    let body = get(&app, "/instance-config", &[]).await;
    assert_full_layout(&body);
    assert!(!body.contains("Threads and forums"), "{body}");
    assert!(!body.contains("update_channel_threads"), "{body}");
    assert!(!body.contains("update_plutonium_page"), "{body}");
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

#[tokio::test]
async fn fonts_are_served_locally_content_hashed_and_immutable() {
    let app = setup().await;

    let stylesheet_path = format!(
        "/static/fonts/{}",
        fluxer_admin::fonts::STYLESHEET_FILE_NAME
    );
    let (headers, css) = get_with_headers(&app, &stylesheet_path, &[]).await;
    assert_eq!(
        headers.get(header::CACHE_CONTROL).unwrap(),
        "public, max-age=31536000, immutable"
    );

    for fragment in css.split("url('").skip(1) {
        let file_name = fragment.split('\'').next().unwrap();
        let response = app
            .router
            .clone()
            .oneshot(
                Request::builder()
                    .uri(format!("/static/fonts/{file_name}"))
                    .body(Body::empty())
                    .unwrap(),
            )
            .await
            .unwrap();
        assert_eq!(
            response.status(),
            StatusCode::OK,
            "missing font {file_name}"
        );
        assert_eq!(
            response.headers().get(header::CONTENT_TYPE).unwrap(),
            "font/woff2"
        );
        assert_eq!(
            response.headers().get(header::CACHE_CONTROL).unwrap(),
            "public, max-age=31536000, immutable"
        );
    }

    let response = app
        .router
        .clone()
        .oneshot(
            Request::builder()
                .uri("/static/fonts/does-not-exist.woff2")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(response.status(), StatusCode::NOT_FOUND);
}

#[tokio::test]
async fn rendered_heads_never_reference_the_static_cdn_for_fonts() {
    let app = setup().await;

    let (headers, page) = get_with_headers(&app, "/users", &[]).await;
    assert!(
        !page.contains("/fonts/ibm-plex.css"),
        "the admin layout still links the CDN font stylesheets"
    );
    assert!(page.contains("/static/fonts/"), "{page}");

    let csp = headers
        .get(header::CONTENT_SECURITY_POLICY)
        .and_then(|value| value.to_str().ok())
        .expect("missing CSP");
    assert!(csp.contains("font-src 'self';"), "font-src was {csp}");
    assert!(
        csp.contains("style-src 'self' 'unsafe-inline';"),
        "style-src was {csp}"
    );
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
    setup_with_api(Router::new().fallback(mock_api)).await
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

async fn get(app: &TestApp, uri: &str, headers: &[(&str, &str)]) -> String {
    get_with_headers(app, uri, headers).await.1
}

async fn get_status(app: &TestApp, uri: &str) -> StatusCode {
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
    response.status()
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

fn assert_full_layout(body: &str) {
    assert!(body.contains(r#"id="admin-sidebar""#), "{body}");
    assert!(body.contains(r#"id="main-content""#), "{body}");
}

fn assert_fragment(body: &str) {
    assert!(!body.contains(r#"id="admin-sidebar""#), "{body}");
    assert!(!body.contains(r#"id="main-content""#), "{body}");
}

fn select_markup<'a>(body: &'a str, name: &str) -> &'a str {
    let start = body
        .find(&format!(r#"<select id="{name}""#))
        .unwrap_or_else(|| panic!("missing {name} select\n{body}"));
    let end = start + body[start..].find("</select>").unwrap();
    &body[start..end]
}

fn pagination_href(body: &str, label: &str) -> String {
    let end = body
        .find(&format!("{label} &"))
        .or_else(|| body.find(&format!("&larr; {label}")))
        .unwrap_or_else(|| panic!("missing {label} link\n{body}"));
    let start = body[..end]
        .rfind("href=\"")
        .unwrap_or_else(|| panic!("missing {label} href\n{body}"))
        + "href=\"".len();
    let href = &body[start..];
    href[..href.find('"').unwrap()].replace("&amp;", "&")
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
    let path = uri.path().to_owned();
    if method == Method::PATCH && path == "/admin/instance/config" {
        return json_response(instance_config());
    }
    match (method, path.as_str()) {
        (Method::GET, "/admin/users/@me") => json_response(json!({ "user": admin_user() })),
        (Method::GET, "/admin/api-keys") => json_response(json!([])),
        (Method::POST, "/admin/api-keys") => json_response(json!({
            "key_id": "1900000000000000001",
            "key": ADMIN_API_KEY_SECRET,
            "name": "Acceptance key",
            "created_at": "2026-07-10T15:00:00.000Z",
            "expires_at": null,
            "acls": ["*"]
        })),
        (Method::GET, "/admin/users") => {
            json_response(json!({ "users": [searched_user()], "total": 1 }))
        }
        (Method::GET, "/admin/users/1500000000000000001") => {
            json_response(json!({ "users": [searched_user()] }))
        }
        (Method::POST, "/admin/users/1500000000000000001/password-reset") => {
            StatusCode::NO_CONTENT.into_response()
        }
        (Method::GET, "/admin/guilds") => {
            json_response(json!({ "guilds": [searched_guild()], "total": 1 }))
        }
        (Method::GET, "/admin/guilds/1600000000000000001") => {
            json_response(json!({ "guild": searched_guild_detail() }))
        }
        (Method::GET, "/admin/applications") => {
            json_response(json!({ "applications": [searched_application()] }))
        }
        (Method::GET, "/admin/reports")
            if query_value(&uri, "reason").as_deref() == Some("csam") =>
        {
            json_response(json!({
                "reports": [flow_report()],
                "total": 60,
                "offset": 0,
                "limit": 25
            }))
        }
        (Method::GET, "/admin/reports")
            if query_value(&uri, "category").as_deref() == Some("future_value") =>
        {
            json_response(json!({
                "reports": [future_category_report()],
                "total": 60,
                "offset": 0,
                "limit": 25
            }))
        }
        (Method::GET, "/admin/reports")
            if query_value(&uri, "reported_webhook_id").as_deref() == Some(WEBHOOK_ID) =>
        {
            json_response(json!({
                "reports": [webhook_report()],
                "total": 60,
                "offset": 0,
                "limit": 25
            }))
        }
        (Method::GET, "/admin/reports") => json_response(
            json!({ "reports": [searched_report()], "total": 1, "offset": 0, "limit": 25 }),
        ),
        (Method::GET, "/admin/report-reasons") => json_response(report_reasons()),
        (Method::GET, "/admin/reports/1800000000000000001") => json_response(searched_report()),
        (Method::GET, "/admin/reports/1800000000000000003") => json_response(flow_report()),
        (Method::GET, "/admin/reports/1800000000000000004") => json_response(webhook_report()),
        (Method::GET, "/admin/reports/1800000000000000005") => {
            json_response(future_status_report())
        }
        (Method::GET, "/admin/reports/1800000000000000002") => {
            json_response(searched_message_report())
        }
        (Method::PATCH, "/admin/reports/1800000000000000001") => json_response(json!({
            "report_id": "1800000000000000001",
            "status": 1,
            "resolved_at": "2026-05-26T12:03:00.000Z",
            "public_comment": "done"
        })),
        (Method::GET, "/admin/jobs") => {
            json_response(json!({ "jobs": [searched_job()], "next_cursor": null, "cursor": null }))
        }
        (Method::GET, "/admin/jobs/1900000000000000001") => {
            json_response(json!({ "job": searched_job() }))
        }
        (Method::GET, "/admin/instance/config") => json_response(instance_config()),
        (Method::POST, "/admin/instance/registration-urls") => json_response(json!({
            "registration_url": registration_url_fixture(),
            "code": "11111111-1111-4111-8111-111111111111",
            "url": "https://app.example.test/register?registration_url=11111111-1111-4111-8111-111111111111"
        })),
        (Method::DELETE, path) if path.starts_with("/admin/instance/registration-urls/") => {
            json_response(instance_config_without_registration_urls())
        }
        (Method::PATCH, path) if path.starts_with("/admin/instance/pending-registrations/") => {
            json_response(instance_config_without_pending_registrations())
        }
        (Method::GET, "/admin/limit-config") => json_response(limit_config()),
        (Method::GET, "/admin/audit-logs") => {
            let access = uri.query().and_then(|query| {
                url::form_urlencoded::parse(query.as_bytes())
                    .find(|(key, _)| key == "access")
                    .map(|(_, value)| value.into_owned())
            });
            let logs = [
                audit_log_entry("1900000000000000101", "get_user", "read"),
                audit_log_entry("1900000000000000102", "temp_ban", "write"),
            ]
            .into_iter()
            .filter(|entry| {
                access
                    .as_deref()
                    .is_none_or(|access| entry.access.to_string() == access)
            })
            .collect::<Vec<_>>();
            json_response(json!({ "total": logs.len(), "logs": logs }))
        }
        _ => (StatusCode::NOT_FOUND, Json(json!({ "error": "not found" }))).into_response(),
    }
}

fn query_value(uri: &Uri, name: &str) -> Option<String> {
    uri.query().and_then(|query| {
        url::form_urlencoded::parse(query.as_bytes())
            .find(|(key, _)| key == name)
            .map(|(_, value)| value.into_owned())
    })
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

fn searched_guild() -> generated_types::GuildAdminResponse {
    serde_json::from_value(json!({
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
        "content_warning_level": 0,
        "content_warning_text": null
    }))
    .expect("guild search fixture must match the generated response contract")
}

fn searched_guild_detail() -> generated_types::LookupGuildResponseGuild {
    serde_json::from_value(json!({
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
        "content_warning_level": 0,
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
        "roles": []
    }))
    .expect("guild detail fixture must match the generated response contract")
}

#[test]
fn guild_fixtures_match_generated_response_contracts() {
    let search = searched_guild();
    assert_eq!(search.name, "Searched Guild");
    assert_eq!(*search.member_count, 12);

    let response: LookupGuildResponse =
        serde_json::from_value(json!({"guild": searched_guild_detail()})).unwrap();
    let detail = response.guild.unwrap();
    assert_eq!(detail.name, "Searched Guild");
    assert_eq!(detail.id, "1600000000000000001");
    assert_eq!(detail.member_count, 12);
}

fn audit_log_entry(
    log_id: &str,
    action: &str,
    access: &str,
) -> generated_types::AdminAuditLogResponseSchema {
    serde_json::from_value(json!({
        "log_id": log_id,
        "admin_user_id": "1500000000000000000",
        "admin_user": null,
        "target_type": "user",
        "target_id": "1500000000000000001",
        "target_user": null,
        "target_guild": null,
        "target_channel": null,
        "related_users": {},
        "related_guilds": {},
        "related_channels": {},
        "action": action,
        "access": access,
        "audit_log_reason": null,
        "metadata": {},
        "created_at": "2026-09-16T12:00:00.000Z"
    }))
    .expect("audit log fixture must match the generated response contract")
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

fn message_tools_api(acls: &[&str]) -> Router {
    let mut admin = admin_user();
    admin["acls"] = json!(acls);
    let message = json!({
        "id": "1800000000000001001",
        "channel_id": "1600000000000000101",
        "channel_name": "general",
        "channel_nsfw": false,
        "guild_id": "1600000000000000001",
        "guild_name": "Guild",
        "guild_nsfw_level": 0,
        "author_id": "1500000000000000001",
        "author_username": "SearchedUser",
        "author_global_name": null,
        "author_discriminator": "0001",
        "author_avatar": null,
        "content": "",
        "timestamp": "2026-10-04T10:00:00.000Z",
        "attachments": [{
            "id": "1800000000000001002",
            "filename": "tools-image.png",
            "url": "https://media.example.test/attachments/tools-image.png",
            "nsfw": false,
            "content_type": "image/png",
            "width": 64,
            "height": 64,
            "size": 4096,
            "ncmec_status": "not_submitted",
            "ncmec_report_id": null,
            "ncmec_failure_reason": null
        }]
    });
    let lookup = json!({"messages": [message.clone()], "message_id": "1800000000000001001"});
    let browse = json!({"messages": [message], "has_more": false});
    Router::new()
        .route(
            "/admin/users/@me",
            routing::get(move || {
                let admin = admin.clone();
                async move { json_response(json!({ "user": admin })) }
            }),
        )
        .route(
            "/admin/channels/1600000000000000101/messages/1800000000000001001",
            routing::get(move || {
                let lookup = lookup.clone();
                async move { json_response(lookup) }
            }),
        )
        .route(
            "/admin/channels/1600000000000000101/messages",
            routing::get(move || {
                let browse = browse.clone();
                async move { json_response(browse) }
            }),
        )
        .fallback(mock_api)
}

fn future_category_report() -> Value {
    let mut report = searched_report();
    report["report_id"] = json!("1800000000000000006");
    report["category"] = json!("future_value");
    report
}

fn future_status_report() -> Value {
    let mut report = searched_report();
    report["report_id"] = json!("1800000000000000005");
    report["status"] = json!(7);
    report["report_type"] = json!(9);
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

fn flow_report() -> Value {
    json!({
        "report_id": "1800000000000000003",
        "reporter_id": "1500000000000000000",
        "reporter_tag": "AdminUser#0001",
        "reported_at": "2026-05-26T12:00:00.000Z",
        "status": 0,
        "report_type": 0,
        "category": "child_safety",
        "additional_info": "Mock flow report details",
        "reported_user_id": "1500000000000000001",
        "reported_user_tag": "SearchedUser#0001",
        "reported_message_id": "1800000000000001001",
        "reported_channel_id": "1600000000000000101",
        "reason": "csam",
        "reason_label": "Child sexual abuse material",
        "reason_highest_priority": true,
        "reporter_good_faith_confirmed": true,
        "flow": {
            "revision_hash": "b7667e8b32c98c40",
            "surface": "dsa",
            "locale": "fr",
            "steps": [
                {
                    "screen_id": "root_message",
                    "screen_title": "Report message",
                    "option_id": "abuse",
                    "option_label": "Abusive or harmful content",
                    "items": []
                },
                {
                    "screen_id": "private_info",
                    "screen_title": "What private information is shared?",
                    "option_id": null,
                    "option_label": null,
                    "items": [
                        {"id": "email", "label": "Email address"},
                        {"id": "phone", "label": "Phone number"}
                    ]
                },
                {
                    "screen_id": "profile_intro",
                    "screen_title": "Report profile",
                    "option_id": null,
                    "option_label": null,
                    "items": []
                }
            ]
        }
    })
}

const WEBHOOK_ID: &str = "1700000000000000500";

fn webhook_report() -> Value {
    json!({
        "report_id": "1800000000000000004",
        "reporter_id": "1500000000000000000",
        "reporter_tag": "AdminUser#0001",
        "reported_at": "2026-05-26T12:00:00.000Z",
        "status": 0,
        "report_type": 0,
        "category": "spam",
        "additional_info": null,
        "reported_user_id": null,
        "reported_user_tag": null,
        "reported_webhook_id": WEBHOOK_ID,
        "reported_webhook_name": "Harbor Bulletin",
        "reported_webhook_avatar_hash": "abc123",
        "reported_webhook_default_name": "Harbor Hook",
        "reported_webhook_default_avatar_hash": null,
        "reported_webhook_type": 1,
        "reported_webhook_application_id": null,
        "reported_webhook_channel_id": "1600000000000000101",
        "reported_webhook_guild_id": "1600000000000000001",
        "reported_webhook_created_at": "2026-05-20T08:30:00.000Z",
        "reported_webhook_creator_id": "1500000000000000002",
        "reported_webhook_creator_tag": "HookOwner#0002",
        "reported_webhook_creator_username": "HookOwner",
        "reported_webhook_creator_global_name": null,
        "reported_webhook_creator_discriminator": "0002",
        "reported_webhook_creator_avatar_hash": null,
        "reported_message_id": "1800000000000001004",
        "reported_channel_id": "1600000000000000101",
        "reported_channel_name": "general",
        "reported_guild_id": "1600000000000000001",
        "reported_guild_name": "Searched Guild",
        "reason": "spam",
        "reason_label": "Spam",
        "reason_highest_priority": false,
        "message_context": [
            {
                "id": "1800000000000001003",
                "content": "Member message before the webhook",
                "timestamp": "2026-05-26T12:00:30.000Z",
                "author_id": "1500000000000000001",
                "author_username": "SearchedUser",
                "author_global_name": null,
                "author_discriminator": "0001",
                "author_avatar": null,
                "webhook_id": null,
                "channel_id": "1600000000000000101",
                "attachments": []
            },
            {
                "id": "1800000000000001004",
                "content": "Webhook spam in drawer",
                "timestamp": "2026-05-26T12:01:00.000Z",
                "author_id": WEBHOOK_ID,
                "author_username": "Harbor Bulletin",
                "author_global_name": null,
                "author_discriminator": "0000",
                "author_avatar": "abc123",
                "webhook_id": WEBHOOK_ID,
                "channel_id": "1600000000000000101",
                "attachments": []
            }
        ]
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
        "domain_migration": {
            "enabled": false,
            "config_version": 0,
            "rollout_basis_points": 0,
            "rollout_salt": "domain-migration-v1",
            "included_user_ids": [],
            "excluded_user_ids": [],
            "anonymous_rollout_basis_points": 0,
            "standalone_forwarding": false
        },
        "experiment_delivery": {
            "poll_interval_seconds": 300,
            "poll_jitter_percent": 15
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
