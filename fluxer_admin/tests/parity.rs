// SPDX-License-Identifier: AGPL-3.0-or-later

#[path = "parity/mod.rs"]
mod parity_support;

use axum::{
    Json, Router,
    extract::{Path, RawQuery},
    http::StatusCode,
    response::{IntoResponse, Response},
    routing::get,
};
use fluxer_admin::api::{
    generated::types::{
        AdminReportListResponse, LookupGuildResponse, ReportAdminResponseSchema,
        SearchGuildsResponse,
    },
    types::{ReportEntry, SearchReportsResponse},
};
use parity_support::{
    TEST_ACCESS_TOKEN, TEST_ADMIN_SECRET, TEST_ADMIN_USER_ID, api_fixtures, capture,
    html_normalizer, rust_server,
};
use serde_json::{Value, json};
use std::{error::Error, io};
use tokio::net::TcpListener;

const SEARCH_REPORTS_V2: &str = include_str!("parity/fixtures/api/search_reports_v2.json");
const SEARCH_REPORTS_WEBHOOK: &str =
    include_str!("parity/fixtures/api/search_reports_webhook.json");
const REPORT_WEBHOOK_DETAIL: &str = include_str!("parity/fixtures/api/report_webhook_detail.json");
const REPORT_EVIDENCE_DETAIL: &str =
    include_str!("parity/fixtures/api/report_evidence_detail.json");
const EVIDENCE_REPORT_ID: &str = "1556352159220498613";
const EVIDENCE_BOT_ID: &str = "1556352159149195428";
const EVIDENCE_WEBHOOK_ID: &str = "1556352159132418208";
const EVIDENCE_DELETED_AUTHOR_ID: &str = "1556352159216304308";
const WEBHOOK_ID: &str = "1556114449176200401";
const WEBHOOK_REPORT_ID: &str = "1556114449264280806";
const BOT_REPORT_ID: &str = "1556114449268475111";
const BOT_USER_ID: &str = "1556114449192977621";
const WEBHOOK_CREATOR_ID: &str = "1556114449125868741";

#[test]
fn html_normalizer_canonicalizes_attribute_order_and_csrf_values() {
    let left = r#"<form><input value="aaaaaaaa" name="_csrf" type="hidden"><svg><line x1="1" x2="2"></line></svg><a class="b" href="/static/app.css?v=123" id="x">Open</a></form>"#;
    let right = r#"<form><input type="hidden" name="_csrf" value="bbbbbbbb"/><svg><line x2="2" x1="1"/></svg><a id="x" href="/static/app.css?v=456" class="b">Open</a></form>"#;
    assert_eq!(
        html_normalizer::normalize_html(left),
        html_normalizer::normalize_html(right)
    );
}

#[test]
fn text_normalizer_replaces_ports_assets_query_values_and_cookie_tokens() {
    let raw = "http://127.0.0.1:31987/auth/start?state=abc&next=/static/app.css?v=dev admin_session=abcdef; csrf_token=12345; oauth_state=zz";
    let normalized = html_normalizer::normalize_text(raw);
    assert!(normalized.contains("127.0.0.1:__PORT__"));
    assert!(normalized.contains("state=__OAUTH_STATE__"));
    assert!(normalized.contains("/static/app.css"));
    assert!(normalized.contains("admin_session=__SESSION__"));
    assert!(normalized.contains("csrf_token=__CSRF_COOKIE__"));
    assert!(normalized.contains("oauth_state=__OAUTH_STATE__"));
}

#[test]
fn text_normalizer_replaces_script_csrf_values() {
    let raw = r#"<script>(function(){var csrf="aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";})()</script>"#;
    let normalized = html_normalizer::normalize_text(raw);
    assert!(normalized.contains(r#"var csrf="__CSRF_TOKEN__""#));
}

#[test]
fn html_normalizer_allows_intentional_rust_markup_fixes() {
    let ts = r#"<html><head></head><body><div class="flex flex-col gap-8 items-stretch"></div><div class="flex flex-col gap-4 items-stretch"></div><script defer>window.__fluxerDrawerInit = true;</script><aside data-drawer-panel="user-peek" aria-hidden="true"></aside></body></html>"#;
    let rust = r#"<!DOCTYPE html><html><head><script src="/static/htmx.min.js?t=parity" defer></script></head><body hx-boost="true"><div class="flex flex-col gap-8 items-center"></div><div id="users-results" class="flex flex-col gap-4 items-stretch"></div><script defer>document.body.addEventListener('showFlash', function () {});</script><script defer>window.__adminCopyToClipboard = function () {};</script><aside id="user-peek" data-drawer-panel="user-peek" popover="auto"></aside></body></html>"#;
    assert_eq!(
        html_normalizer::normalize_html(ts),
        html_normalizer::normalize_html(rust)
    );
}

#[test]
fn guild_search_fixture_matches_the_generated_response_contract() {
    let response: SearchGuildsResponse =
        serde_json::from_str(include_str!("parity/fixtures/api/search_guilds.json"))
            .expect("guild search fixture must match the generated response contract");
    assert_eq!(response.guilds.len(), 1);
    let guild = &response.guilds[0];
    assert_eq!(guild.name, "Parity Guild");
    assert_eq!(guild.nsfw, Some(false));
    assert_eq!(guild.content_warning_level.as_deref(), Some(&0));
}

#[test]
fn guild_lookup_fixture_matches_the_generated_response_contract() {
    let response: LookupGuildResponse =
        serde_json::from_str(include_str!("parity/fixtures/api/lookup_guild.json"))
            .expect("guild lookup fixture must match the generated response contract");
    let guild = response
        .guild
        .expect("guild lookup fixture must contain a guild");
    assert_eq!(String::from(guild.name), "Parity Guild");
    assert_eq!(guild.nsfw, Some(false));
    assert_eq!(guild.content_warning_level.as_deref(), Some(&0));
    assert_eq!(guild.channels.len(), 1);
    assert_eq!(guild.channels[0].content_warning_level.as_deref(), Some(&0));
}

#[tokio::test(flavor = "multi_thread")]
async fn rust_admin_fixture_routes_cover_default_protected_routes() -> Result<(), Box<dyn Error>> {
    let api_server = api_fixtures::ApiFixtureServer::start_default()
        .await
        .map_err(test_error)?;
    let rust_admin = rust_server::start(api_server.base_url())
        .await
        .map_err(test_error)?;
    let client = capture::capture_client().map_err(test_error)?;
    let session = fluxer_admin::session::create_session(
        TEST_ADMIN_USER_ID,
        TEST_ACCESS_TOKEN,
        TEST_ADMIN_SECRET,
    );
    let session_cookie = format!("{}={session}", fluxer_admin::session::SESSION_COOKIE_NAME);
    let cases = [
        ("/dashboard", 302, Some("/users"), None),
        ("/users?q=Parity", 200, None, Some("Parity User")),
        ("/guilds?q=Parity", 200, None, Some("Parity Guild")),
        (
            "/guilds/1600000000000000001",
            200,
            None,
            Some("Parity Guild"),
        ),
        ("/reports", 200, None, Some("1700000000000000001")),
        (
            "/reports/1700000000000000001",
            200,
            None,
            Some("Report Details"),
        ),
    ];
    for (route, expected_status, expected_location, expected_body) in cases {
        let response =
            capture::fetch_route(&client, rust_admin.base_url(), route, Some(&session_cookie))
                .await
                .map_err(test_error)?;
        assert_eq!(response.status, expected_status, "{route}: {response:#?}");
        if let Some(expected_location) = expected_location {
            assert_eq!(
                response.location.as_deref(),
                Some(expected_location),
                "{route}: {response:#?}"
            );
        }
        if let Some(expected_body) = expected_body {
            assert!(
                response.body.contains(expected_body),
                "{route}: expected body to contain {expected_body:?}\n{response:#?}"
            );
        }
    }
    Ok(())
}

#[test]
fn report_search_v2_fixture_matches_the_generated_and_hand_written_contracts() {
    let generated: AdminReportListResponse = serde_json::from_str(SEARCH_REPORTS_V2)
        .expect("v2 report search fixture must match the generated response contract");
    let flow = generated.reports[0]
        .flow
        .as_ref()
        .expect("the v2 message report has answers");
    assert_eq!(flow.surface, "in_app");
    assert_eq!(flow.steps.len(), 4);
    assert_eq!(generated.reports[0].reason.as_deref(), Some("csam"));
    assert_eq!(generated.reports[2].reason, None);

    let response: SearchReportsResponse = serde_json::from_str(SEARCH_REPORTS_V2)
        .expect("v2 report search fixture must match the hand-written response type");
    assert_eq!(response.total, 3);
    let [message, user, legacy] = response.reports.as_slice() else {
        panic!("expected three reports");
    };
    assert_eq!(
        message.reason_label.as_deref(),
        Some("Child sexual abuse material")
    );
    assert_eq!(message.reason_highest_priority, Some(true));
    assert_eq!(message.category.as_deref(), Some("child_safety"));
    let user_flow = user.flow.as_ref().expect("the v2 user report has answers");
    assert_eq!(user.reason.as_deref(), Some("harassment"));
    assert_eq!(user_flow.locale.as_deref(), Some("de"));
    assert_eq!(user_flow.steps[0].screen_id, "profile_intro");
    assert!(user_flow.steps[0].option_id.is_none());
    assert!(user_flow.steps[0].items.is_empty());
    assert_eq!(
        user_flow.steps[1]
            .items
            .iter()
            .map(|item| item.id.as_str())
            .collect::<Vec<_>>(),
        ["photo", "profile_text"]
    );
    assert!(legacy.reason.is_none());
    assert!(legacy.reason_label.is_none());
    assert!(legacy.flow.is_none());
    assert!(legacy.reporter_good_faith_confirmed.is_none());
}

#[tokio::test(flavor = "multi_thread")]
async fn rust_admin_renders_v2_report_fixture_with_reason_and_answers() -> Result<(), Box<dyn Error>>
{
    let api_endpoint = spawn_v2_report_api().await.map_err(test_error)?;
    let rust_admin = rust_server::start(&api_endpoint)
        .await
        .map_err(test_error)?;
    let client = capture::capture_client().map_err(test_error)?;
    let session = fluxer_admin::session::create_session(
        TEST_ADMIN_USER_ID,
        TEST_ACCESS_TOKEN,
        TEST_ADMIN_SECRET,
    );
    let session_cookie = format!("{}={session}", fluxer_admin::session::SESSION_COOKIE_NAME);

    let list = capture::fetch_route(
        &client,
        rust_admin.base_url(),
        "/reports?reason=csam",
        Some(&session_cookie),
    )
    .await
    .map_err(test_error)?;
    assert_eq!(list.status, 200, "{list:#?}");
    assert!(list.body.contains(r#"name="reason""#), "{}", list.body);
    assert!(
        list.body.contains("Priority: Child sexual abuse material"),
        "{}",
        list.body
    );
    assert!(
        list.body.contains(r#"data-report-reason="csam""#),
        "{}",
        list.body
    );
    assert!(
        !list.body.contains(r#"data-report-reason="harassment""#),
        "{}",
        list.body
    );

    let detail = capture::fetch_route(
        &client,
        rust_admin.base_url(),
        "/reports/1556008115705480394",
        Some(&session_cookie),
    )
    .await
    .map_err(test_error)?;
    assert_eq!(detail.status, 200, "{detail:#?}");
    for expected in [
        "Report Answers",
        "Report profile",
        "Which parts of their profile are a problem?: Pictures, Profile text",
        "Shown to the reporter in de",
        "Surface: In app",
        "Harassment or bullying",
    ] {
        assert!(
            detail.body.contains(expected),
            "{expected}\n{}",
            detail.body
        );
    }

    let legacy = capture::fetch_route(
        &client,
        rust_admin.base_url(),
        "/reports/1556008115709674699",
        Some(&session_cookie),
    )
    .await
    .map_err(test_error)?;
    assert_eq!(legacy.status, 200, "{legacy:#?}");
    assert!(!legacy.body.contains("Report Answers"), "{}", legacy.body);
    assert!(
        !legacy.body.contains("data-report-reason"),
        "{}",
        legacy.body
    );
    Ok(())
}

#[test]
fn report_webhook_fixtures_match_the_generated_and_hand_written_contracts() {
    let generated: AdminReportListResponse = serde_json::from_str(SEARCH_REPORTS_WEBHOOK)
        .expect("webhook report search fixture must match the generated response contract");
    assert_eq!(generated.reports.len(), 2);
    let response: SearchReportsResponse = serde_json::from_str(SEARCH_REPORTS_WEBHOOK)
        .expect("webhook report search fixture must match the hand-written response type");
    let [webhook, bot] = response.reports.as_slice() else {
        panic!("expected two reports");
    };
    assert_eq!(webhook.report_id, WEBHOOK_REPORT_ID);
    assert!(webhook.reported_user_id.is_none());
    assert_eq!(webhook.reported_webhook_id.as_deref(), Some(WEBHOOK_ID));
    assert_eq!(
        webhook.reported_webhook_name.as_deref(),
        Some("Harbor Bulletin")
    );
    assert_eq!(bot.reported_user_id.as_deref(), Some(BOT_USER_ID));
    assert!(bot.reported_webhook_id.is_none());

    let generated_detail: ReportAdminResponseSchema = serde_json::from_str(REPORT_WEBHOOK_DETAIL)
        .expect("webhook report detail fixture must match the generated response contract");
    let via_generated: ReportEntry = serde_json::from_value(
        serde_json::to_value(generated_detail).expect("serialize generated detail"),
    )
    .expect("hand-written report type from the generated detail");
    let direct: ReportEntry = serde_json::from_str(REPORT_WEBHOOK_DETAIL)
        .expect("webhook report detail fixture must match the hand-written report type");
    for detail in [&via_generated, &direct] {
        assert!(detail.reported_user_id.is_none());
        assert_eq!(detail.reported_webhook_id.as_deref(), Some(WEBHOOK_ID));
        assert_eq!(
            detail.reported_webhook_creator_id.as_deref(),
            Some(WEBHOOK_CREATOR_ID)
        );
        assert_eq!(
            detail.reported_webhook_creator_global_name.as_deref(),
            Some("Morgan Owner")
        );
        assert_eq!(detail.reported_webhook_type, Some(1));
        assert_eq!(
            detail.reported_webhook_channel_id,
            detail.reported_channel_id
        );
        assert_eq!(detail.reported_webhook_guild_id, detail.reported_guild_id);
        assert_eq!(
            detail.reported_webhook_created_at.as_deref(),
            Some("2026-10-04T01:23:15.892Z")
        );
        assert!(detail.reported_webhook_application_id.is_none());
        let context = detail.message_context.as_ref().expect("message context");
        let entry = context
            .iter()
            .find(|entry| entry["webhook_id"].as_str() == Some(WEBHOOK_ID))
            .expect("the webhook message is in the context");
        assert_eq!(entry["author_id"].as_str(), Some(WEBHOOK_ID));
        assert_eq!(entry["author_username"].as_str(), Some("Harbor Bulletin"));
    }
}

#[tokio::test(flavor = "multi_thread")]
async fn rust_admin_renders_webhook_report_fixture() -> Result<(), Box<dyn Error>> {
    let api_endpoint = spawn_v2_report_api().await.map_err(test_error)?;
    let rust_admin = rust_server::start(&api_endpoint)
        .await
        .map_err(test_error)?;
    let client = capture::capture_client().map_err(test_error)?;
    let session = fluxer_admin::session::create_session(
        TEST_ADMIN_USER_ID,
        TEST_ACCESS_TOKEN,
        TEST_ADMIN_SECRET,
    );
    let session_cookie = format!("{}={session}", fluxer_admin::session::SESSION_COOKIE_NAME);
    let webhook_marker = format!(r#"data-report-webhook="{WEBHOOK_ID}""#);
    let webhook_user_link = format!("/users/{WEBHOOK_ID}");

    let list = capture::fetch_route(
        &client,
        rust_admin.base_url(),
        &format!("/reports?reported_webhook_id={WEBHOOK_ID}"),
        Some(&session_cookie),
    )
    .await
    .map_err(test_error)?;
    assert_eq!(list.status, 200, "{list:#?}");
    assert!(list.body.contains(WEBHOOK_REPORT_ID), "{}", list.body);
    assert!(!list.body.contains(BOT_REPORT_ID), "{}", list.body);
    assert!(list.body.contains(&webhook_marker), "{}", list.body);
    assert!(list.body.contains("Harbor Bulletin"), "{}", list.body);
    assert!(list.body.contains("Channel: general"), "{}", list.body);
    assert!(!list.body.contains(&webhook_user_link), "{}", list.body);

    let both = capture::fetch_route(
        &client,
        rust_admin.base_url(),
        "/reports?reporter_id=1556114449113285826",
        Some(&session_cookie),
    )
    .await
    .map_err(test_error)?;
    assert_eq!(both.status, 200, "{both:#?}");
    assert!(
        both.body
            .contains(&format!(r#"href="/users/{BOT_USER_ID}""#)),
        "{}",
        both.body
    );

    let detail = capture::fetch_route(
        &client,
        rust_admin.base_url(),
        &format!("/reports/{WEBHOOK_REPORT_ID}"),
        Some(&session_cookie),
    )
    .await
    .map_err(test_error)?;
    assert_eq!(detail.status, 200, "{detail:#?}");
    for expected in [
        webhook_marker.clone(),
        ">Webhook ID<".to_owned(),
        format!(r#"href="/reports?reported_webhook_id={WEBHOOK_ID}""#),
        "Harbor Commons".to_owned(),
        "Message Context".to_owned(),
        format!(r#"data-message-webhook="{WEBHOOK_ID}""#),
        format!(r#"href="/users/{BOT_USER_ID}""#),
        ">Webhook Creator<".to_owned(),
        format!(r#"data-report-webhook-creator="{WEBHOOK_CREATOR_ID}""#),
        format!(r#"href="/users/{WEBHOOK_CREATOR_ID}""#),
        "Morgan Owner".to_owned(),
        ">Webhook Type<".to_owned(),
        ">Incoming<".to_owned(),
        ">Webhook Created<".to_owned(),
        "Oct 4, 2026, 1:23 AM UTC".to_owned(),
    ] {
        assert!(
            detail.body.contains(&expected),
            "{expected}\n{}",
            detail.body
        );
    }
    assert!(!detail.body.contains(&webhook_user_link), "{}", detail.body);
    assert!(
        !detail.body.contains("View Reported User"),
        "{}",
        detail.body
    );
    for absent in [
        ">Webhook Channel ID<",
        ">Webhook Guild ID<",
        ">Webhook Record<",
        "data-report-webhook-creator-deleted",
    ] {
        assert!(!detail.body.contains(absent), "{absent}\n{}", detail.body);
    }
    Ok(())
}

#[test]
fn report_evidence_fixture_matches_the_generated_and_hand_written_contracts() {
    let generated: ReportAdminResponseSchema = serde_json::from_str(REPORT_EVIDENCE_DETAIL)
        .expect("evidence report detail fixture must match the generated response contract");
    let via_generated: ReportEntry = serde_json::from_value(
        serde_json::to_value(generated).expect("serialize generated detail"),
    )
    .expect("hand-written report type from the generated detail");
    let direct: ReportEntry = serde_json::from_str(REPORT_EVIDENCE_DETAIL)
        .expect("evidence report detail fixture must match the hand-written report type");
    for detail in [&via_generated, &direct] {
        assert_eq!(detail.reported_user_id.as_deref(), Some(EVIDENCE_BOT_ID));
        assert_eq!(detail.reported_user_bot, Some(true));
        let snapshot = detail
            .reported_profile_snapshot
            .as_ref()
            .expect("the bot author has a profile snapshot");
        let user = snapshot.user.as_ref().expect("user snapshot");
        assert_eq!(user.id, EVIDENCE_BOT_ID);
        assert_eq!(user.username.as_deref(), Some("Harbor_Helper"));
        let member = snapshot.member.as_ref().expect("member snapshot");
        assert_eq!(member.guild_id, "1556352159090475158");
        assert!(member.joined_at.is_some());
        assert!(snapshot.guild.is_none());
        let context = detail.message_context.as_ref().expect("message context");
        let author_bot = |author_id: &str| {
            context
                .iter()
                .filter(|entry| entry["author_id"].as_str() == Some(author_id))
                .map(|entry| entry["author_bot"].clone())
                .collect::<Vec<_>>()
        };
        assert_eq!(author_bot(EVIDENCE_BOT_ID), [json!(true), json!(true)]);
        assert_eq!(author_bot(EVIDENCE_WEBHOOK_ID), [Value::Null]);
        assert_eq!(author_bot(EVIDENCE_DELETED_AUTHOR_ID), [Value::Null]);
        assert_eq!(author_bot("1556352159069503633"), [json!(false)]);
        assert!(context.iter().all(|entry| {
            entry["missing_attachments"]
                .as_array()
                .is_none_or(Vec::is_empty)
        }));
    }
}

#[tokio::test(flavor = "multi_thread")]
async fn rust_admin_renders_the_evidence_report_fixture() -> Result<(), Box<dyn Error>> {
    let api_endpoint = spawn_v2_report_api().await.map_err(test_error)?;
    let rust_admin = rust_server::start(&api_endpoint)
        .await
        .map_err(test_error)?;
    let client = capture::capture_client().map_err(test_error)?;
    let session = fluxer_admin::session::create_session(
        TEST_ADMIN_USER_ID,
        TEST_ACCESS_TOKEN,
        TEST_ADMIN_SECRET,
    );
    let session_cookie = format!("{}={session}", fluxer_admin::session::SESSION_COOKIE_NAME);
    let detail = capture::fetch_route(
        &client,
        rust_admin.base_url(),
        &format!("/reports/{EVIDENCE_REPORT_ID}"),
        Some(&session_cookie),
    )
    .await
    .map_err(test_error)?;
    assert_eq!(detail.status, 200, "{detail:#?}");
    for expected in [
        format!(r#"data-report-user-bot="{EVIDENCE_BOT_ID}""#),
        format!(r#"data-message-author-bot="{EVIDENCE_BOT_ID}""#),
        format!(r#"data-message-webhook="{EVIDENCE_WEBHOOK_ID}""#),
        "At Report Time".to_owned(),
        "Harbor_Helper#3966".to_owned(),
        ">Community Profile<".to_owned(),
        r#"data-report-legal-hold="none""#.to_owned(),
    ] {
        assert!(
            detail.body.contains(&expected),
            "{expected}\n{}",
            detail.body
        );
    }
    assert_eq!(
        detail.body.matches("data-message-author-bot=").count(),
        1,
        "{}",
        detail.body
    );
    assert!(
        !detail.body.contains("data-missing-attachment"),
        "{}",
        detail.body
    );
    Ok(())
}

async fn spawn_v2_report_api() -> Result<String, String> {
    let listener = TcpListener::bind("127.0.0.1:0")
        .await
        .map_err(|error| format!("failed to bind v2 report API: {error}"))?;
    let port = listener
        .local_addr()
        .map_err(|error| format!("failed to read v2 report API address: {error}"))?
        .port();
    let app = Router::new()
        .route(
            "/admin/users/@me",
            get(|| async {
                fixture_json(include_str!("parity/fixtures/api/admin_user_me.json"))
            }),
        )
        .route(
            "/admin/report-reasons",
            get(|| async {
                Json(json!({"reasons": [
                    {"key": "csam", "label": "Child sexual abuse material", "highest_priority": true, "legacy_category_message": "child_safety", "legacy_category_user": "child_safety", "legacy_category_guild": "child_safety"},
                    {"key": "harassment", "label": "Harassment or bullying", "highest_priority": false, "legacy_category_message": "harassment", "legacy_category_user": "harassment", "legacy_category_guild": "harassment"}
                ]}))
                .into_response()
            }),
        )
        .route("/admin/reports", get(v2_report_search))
        .route("/admin/reports/{report_id}", get(v2_report_detail));
    tokio::spawn(async move {
        let _ = axum::serve(listener, app).await;
    });
    Ok(format!("http://127.0.0.1:{port}"))
}

fn fixture_json(body: &'static str) -> Response {
    let value: Value = serde_json::from_str(body).expect("fixture is JSON");
    Json(value).into_response()
}

fn v2_reports() -> Vec<Value> {
    [SEARCH_REPORTS_V2, SEARCH_REPORTS_WEBHOOK]
        .into_iter()
        .flat_map(|body| {
            let value: Value = serde_json::from_str(body).expect("fixture is JSON");
            value["reports"].as_array().cloned().unwrap_or_default()
        })
        .collect()
}

async fn v2_report_search(RawQuery(query): RawQuery) -> Response {
    let query = query.unwrap_or_default();
    let filters = url::form_urlencoded::parse(query.as_bytes())
        .filter(|(key, value)| {
            matches!(
                key.as_ref(),
                "reason" | "reporter_id" | "reported_webhook_id"
            ) && !value.is_empty()
        })
        .map(|(key, value)| (key.into_owned(), value.into_owned()))
        .collect::<Vec<_>>();
    let reports = v2_reports()
        .into_iter()
        .filter(|report| {
            filters
                .iter()
                .all(|(key, value)| report[key.as_str()].as_str() == Some(value.as_str()))
        })
        .collect::<Vec<_>>();
    Json(json!({"reports": reports, "total": reports.len(), "offset": 0, "limit": 25}))
        .into_response()
}

async fn v2_report_detail(Path(report_id): Path<String>) -> Response {
    if report_id == WEBHOOK_REPORT_ID {
        return fixture_json(REPORT_WEBHOOK_DETAIL);
    }
    if report_id == EVIDENCE_REPORT_ID {
        return fixture_json(REPORT_EVIDENCE_DETAIL);
    }
    match v2_reports()
        .into_iter()
        .find(|report| report["report_id"].as_str() == Some(report_id.as_str()))
    {
        Some(report) => Json(report).into_response(),
        None => StatusCode::NOT_FOUND.into_response(),
    }
}

fn test_error(message: String) -> Box<dyn Error> {
    Box::new(io::Error::other(message))
}
