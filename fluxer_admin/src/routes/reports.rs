// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    acl,
    api::{
        client::{AdminApiClient, ApiError, ApiResultExt},
        generated::types::{UpdateReportRequestOutcome, UpdateReportRequestResolution},
        reports::SearchReportsParams,
        types::ReportEntry,
    },
    config::AdminConfig,
    middleware::{
        auth::AuthContext,
        csrf,
        flash::{self, FlashData},
        htmx,
    },
    state::AppState,
    templates,
    utils::{forms::clean_string, snowflake::is_snowflake, timestamps::format_admin_timestamp},
};
use axum::{
    Form, Router,
    extract::{FromRequest, Path, Query, Request, State},
    http::HeaderMap,
    response::{Html, IntoResponse, Redirect, Response},
    routing::get,
};
use serde::Deserialize;

const MAX_REPORT_OFFSET: u64 = 10_000;

#[derive(Deserialize)]
struct ReportsQuery {
    q: Option<String>,
    status: Option<String>,
    #[serde(rename = "type")]
    report_type: Option<String>,
    category: Option<String>,
    reason: Option<String>,
    reporter_id: Option<String>,
    reported_user_id: Option<String>,
    reported_webhook_id: Option<String>,
    reported_guild_id: Option<String>,
    reported_channel_id: Option<String>,
    guild_context_id: Option<String>,
    resolved_by_admin_id: Option<String>,
    sort: Option<String>,
    limit: Option<u32>,
    page: Option<u32>,
}

#[derive(Deserialize)]
struct LegalHoldForm {
    #[serde(default)]
    _csrf: Option<String>,
    #[serde(default)]
    legal_hold_until: Option<String>,
    #[serde(default)]
    legal_hold_reason: Option<String>,
    #[serde(default)]
    clear: Option<String>,
}

#[derive(Deserialize)]
struct NoteForm {
    #[serde(default)]
    _csrf: Option<String>,
    #[serde(default)]
    body: Option<String>,
}

#[derive(Deserialize)]
struct DeleteForm {
    #[serde(default)]
    _csrf: Option<String>,
    #[serde(default)]
    confirm: Option<String>,
    #[serde(default)]
    audit_log_reason: Option<String>,
}

#[derive(Deserialize)]
struct ResolveForm {
    #[serde(default)]
    _csrf: Option<String>,
    #[serde(default)]
    resolution: Option<String>,
    #[serde(default)]
    outcome: Option<String>,
    #[serde(default)]
    duplicate_of: Option<String>,
    #[serde(default)]
    public_comment: Option<String>,
    #[serde(default)]
    notify_reporter: Option<String>,
    #[serde(default)]
    notify_reporter_present: Option<String>,
}

pub fn router() -> Router<AppState> {
    Router::new()
        .route("/reports", get(reports_list))
        .route("/reports/{report_id}", get(report_detail))
        .route("/reports/{report_id}/fragment", get(report_fragment))
        .route(
            "/reports/{report_id}/resolve",
            axum::routing::post(report_resolve),
        )
        .route(
            "/reports/{report_id}/notes",
            axum::routing::post(report_note),
        )
        .route(
            "/reports/{report_id}/legal-hold",
            axum::routing::post(report_legal_hold),
        )
        .route(
            "/reports/{report_id}/delete",
            axum::routing::post(report_delete),
        )
}

async fn reports_list(
    State(state): State<AppState>,
    headers: HeaderMap,
    auth: axum::Extension<AuthContext>,
    Query(query): Query<ReportsQuery>,
) -> Response {
    let _is_htmx = htmx::is_htmx_request(&headers);
    let config = state.config();
    let page = query.page.unwrap_or(0);
    let limit = query.limit.unwrap_or(25).clamp(1, 200);
    let offset = u64::from(page) * u64::from(limit);
    if offset > MAX_REPORT_OFFSET {
        return reports_error_page(
            config,
            &auth.0,
            "That page is out of range. The reports search returns at most the first 10000 reports. Narrow the filters and start again.",
        );
    }
    let search_query = query.q.as_deref().and_then(clean_string);
    let (sort_by, sort_order) = decode_sort(query.sort.as_deref());
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let status_filter = status_filter(&query);
    let status = status_filter.parse::<i32>().ok();
    let report_type = query
        .report_type
        .as_deref()
        .and_then(|s| s.parse::<i32>().ok());
    let reason = query.reason.as_deref().and_then(clean_string);
    let params = SearchReportsParams {
        query: search_query.as_deref(),
        status,
        report_type,
        category: query.category.as_deref(),
        reason: reason.as_deref(),
        reporter_id: query.reporter_id.as_deref(),
        reported_user_id: query.reported_user_id.as_deref(),
        reported_webhook_id: query.reported_webhook_id.as_deref(),
        reported_guild_id: query.reported_guild_id.as_deref(),
        reported_channel_id: query.reported_channel_id.as_deref(),
        guild_context_id: query.guild_context_id.as_deref(),
        resolved_by_admin_id: query.resolved_by_admin_id.as_deref(),
        sort_by: Some(sort_by),
        sort_order: Some(sort_order),
        limit,
        offset,
    };
    let (reports, reasons) = tokio::join!(
        client.search_reports(&params),
        state.report_reasons(&client)
    );
    let reports = reports.log_error("search reports");

    let markup = templates::pages::reports_list::reports_list_page(
        config,
        &auth.0,
        reports.as_ref(),
        &templates::pages::reports_list::ReportFilters {
            query: search_query.as_deref(),
            status: Some(status_filter),
            report_type: query.report_type.as_deref(),
            category: query.category.as_deref(),
            reason: reason.as_deref(),
            reporter_id: query.reporter_id.as_deref(),
            reported_user_id: query.reported_user_id.as_deref(),
            reported_webhook_id: query.reported_webhook_id.as_deref(),
            reported_guild_id: query.reported_guild_id.as_deref(),
            reported_channel_id: query.reported_channel_id.as_deref(),
            guild_context_id: query.guild_context_id.as_deref(),
            resolved_by_admin_id: query.resolved_by_admin_id.as_deref(),
            sort: query.sort.as_deref().unwrap_or("reportedAt_desc"),
        },
        reasons.as_deref(),
        page,
        limit,
    );
    Html(markup.into_string()).into_response()
}

fn status_filter(query: &ReportsQuery) -> &str {
    match query.status.as_deref().map(str::trim) {
        Some("") | Some(STATUS_ALL) => STATUS_ALL,
        Some(status) => status,
        None if has_entity_filter(query) => STATUS_ALL,
        None => STATUS_PENDING,
    }
}

fn has_entity_filter(query: &ReportsQuery) -> bool {
    [
        &query.q,
        &query.report_type,
        &query.category,
        &query.reason,
        &query.reporter_id,
        &query.reported_user_id,
        &query.reported_webhook_id,
        &query.reported_guild_id,
        &query.reported_channel_id,
        &query.guild_context_id,
        &query.resolved_by_admin_id,
    ]
    .into_iter()
    .any(|value| {
        value
            .as_deref()
            .is_some_and(|value| !value.trim().is_empty())
    })
}

const STATUS_ALL: &str = "all";
const STATUS_PENDING: &str = "0";

fn reports_error_page(config: &AdminConfig, auth: &AuthContext, message: &str) -> Response {
    let markup = templates::layout::admin_layout(
        config,
        auth,
        "Reports",
        "reports",
        None,
        templates::components::error_display::error_alert(message),
    );
    Html(markup.into_string()).into_response()
}

async fn report_detail(
    State(state): State<AppState>,
    headers: HeaderMap,
    auth: axum::Extension<AuthContext>,
    Path(report_id): Path<String>,
    request: Request,
) -> Response {
    let config = state.config();
    let is_detail_fragment = htmx::targets(&headers, "main-content");
    let csrf_token = csrf::get_csrf_token(&request);
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let report = client
        .get_report(&report_id)
        .await
        .log_error("load report detail");
    match report {
        Some(report) => {
            let note_author_ids = note_author_ids(&report);
            let (live, note_authors) =
                tokio::join!(load_live_profile(&client, &auth.0, &report), async {
                    client
                        .lookup_users_by_ids(&note_author_ids)
                        .await
                        .log_error("load report note authors")
                        .unwrap_or_default()
                });
            let markup = templates::pages::report_detail::report_detail_page(
                config,
                &auth.0,
                &report,
                &live,
                &note_authors,
                &csrf_token,
                is_detail_fragment,
            );
            Html(markup.into_string()).into_response()
        }
        None => {
            let base = &config.base_path;
            Redirect::to(&format!("{base}/reports")).into_response()
        }
    }
}

fn note_author_ids(report: &ReportEntry) -> Vec<String> {
    let mut ids: Vec<String> = report
        .notes
        .iter()
        .map(|note| note.author_id.clone())
        .collect();
    ids.sort_unstable();
    ids.dedup();
    ids
}

async fn load_live_profile(
    client: &AdminApiClient,
    auth: &AuthContext,
    report: &ReportEntry,
) -> templates::pages::report_detail::LiveProfile {
    let can = |permission: &str| {
        auth.admin_user
            .as_ref()
            .is_some_and(|admin| acl::has_permission(&admin.acls, permission))
    };
    let snapshot = report.reported_profile_snapshot.as_ref();
    let user_id = snapshot
        .and_then(|snapshot| snapshot.user.as_ref())
        .map(|user| user.id.as_str())
        .filter(|_| can(acl::USER_LOOKUP));
    let guild_id = snapshot
        .and_then(|snapshot| snapshot.guild.as_ref())
        .map(|guild| guild.id.as_str())
        .filter(|_| can(acl::GUILD_LOOKUP));
    let (user, guild) = tokio::join!(
        async {
            match user_id {
                Some(id) => client
                    .get_user_by_id(id)
                    .await
                    .log_error("load live reported user"),
                None => None,
            }
        },
        async {
            match guild_id {
                Some(id) => client
                    .lookup_guild(id)
                    .await
                    .log_error("load live reported guild")
                    .flatten(),
                None => None,
            }
        }
    );
    templates::pages::report_detail::LiveProfile { user, guild }
}

async fn report_fragment(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    Path(report_id): Path<String>,
) -> Response {
    let config = state.config();
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let report = client
        .get_report(&report_id)
        .await
        .log_error("load report fragment");
    let markup = match report {
        Some(report) => templates::pages::report_detail::report_detail_fragment(config, &report),
        None => maud::html! {
            div data-report-fragment=""
                class="rounded-xl border border-red-200 bg-red-50 p-4 text-red-800 text-sm" {
                "Failed to load report."
            }
        },
    };
    Html(markup.into_string()).into_response()
}

async fn report_resolve(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    Path(report_id): Path<String>,
    request: Request,
) -> Response {
    let config = state.config();
    let base = &config.base_path;
    let back = format!("{base}/reports/{report_id}");
    let form: ResolveForm = match Form::from_request(request, &state).await {
        Ok(Form(f)) => f,
        Err(error) => {
            tracing::warn!(%error, report_id, "failed to parse report resolve form");
            return flash::redirect_error(config, &back, "Invalid form data");
        }
    };
    let Some((resolution, label)) =
        templates::pages::report_detail::resolution_choice(form.resolution.as_deref())
    else {
        return flash::redirect_error(config, &back, RESOLUTION_REQUIRED);
    };
    let outcome = match resolution {
        UpdateReportRequestResolution::NoViolation => Some(UpdateReportRequestOutcome::NoViolation),
        UpdateReportRequestResolution::Actioned => form
            .outcome
            .as_deref()
            .and_then(|value| UpdateReportRequestOutcome::try_from(value).ok())
            .filter(|outcome| *outcome != UpdateReportRequestOutcome::NoViolation),
        UpdateReportRequestResolution::Duplicate => None,
    };
    let duplicate_of = form
        .duplicate_of
        .as_deref()
        .and_then(duplicate_report_id)
        .map(str::to_owned);
    let missing = match resolution {
        UpdateReportRequestResolution::Actioned if outcome.is_none() => Some(OUTCOME_REQUIRED),
        UpdateReportRequestResolution::Duplicate if duplicate_of.is_none() => {
            Some(DUPLICATE_OF_REQUIRED)
        }
        _ => None,
    };
    if let Some(message) = missing {
        return flash::redirect_error(config, &back, message);
    }
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let public_comment = clean_string(form.public_comment.as_deref().unwrap_or(""));
    let notify_reporter =
        form.notify_reporter_present.is_none() || form.notify_reporter.as_deref() == Some("true");
    let flash = match client
        .resolve_report(
            &report_id,
            resolution,
            outcome,
            duplicate_of.as_deref(),
            public_comment.as_deref(),
            notify_reporter,
        )
        .await
    {
        Ok(_) => FlashData::success(format!("Report resolved: {label}")),
        Err(error) => {
            tracing::warn!(%error, report_id, "admin API request failed: resolve report");
            match (&error, duplicate_of.as_deref()) {
                (
                    ApiError::Http {
                        status: 404,
                        message,
                        ..
                    },
                    Some(target),
                ) if message.contains("UNKNOWN_REPORT") => FlashData::error(format!(
                    "No report has the ID {target}. Copy the Report ID from that report's page, not a message, user or decision ID."
                )),
                _ => FlashData::error(error.refusal_message("Failed to resolve report")),
            }
        }
    };
    flash::redirect_with_flash(&back, flash, config.secure_cookies())
}

async fn report_note(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    Path(report_id): Path<String>,
    request: Request,
) -> Response {
    let config = state.config();
    let back = format!("{}/reports/{report_id}", config.base_path);
    let form: NoteForm = match Form::from_request(request, &state).await {
        Ok(Form(f)) => f,
        Err(error) => {
            tracing::warn!(%error, report_id, "failed to parse report note form");
            return flash::redirect_error(config, &back, "Invalid form data");
        }
    };
    let Some(body) = clean_string(form.body.as_deref().unwrap_or("")) else {
        return flash::redirect_error(config, &back, "Write a note first");
    };
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let flash = match client.annotate_report(&report_id, body).await {
        Ok(_) => FlashData::success("Note added"),
        Err(error) => {
            tracing::warn!(%error, report_id, "admin API request failed: add report note");
            FlashData::error("Failed to add the note")
        }
    };
    flash::redirect_with_flash(&back, flash, config.secure_cookies())
}

fn duplicate_report_id(value: &str) -> Option<&str> {
    let value = value.trim();
    let candidate = match value.split_once("/reports/") {
        Some((_, rest)) => rest.split(['/', '?', '#']).next().unwrap_or(""),
        None => value.trim_start_matches('#'),
    };
    is_snowflake(candidate).then_some(candidate)
}

const RESOLUTION_REQUIRED: &str = "Choose a resolution";
const OUTCOME_REQUIRED: &str = "Choose what happened to the reported content or account";
const DUPLICATE_OF_REQUIRED: &str = "Enter the ID or link of the report this one repeats";

async fn report_legal_hold(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    Path(report_id): Path<String>,
    request: Request,
) -> Response {
    let config = state.config();
    let base = &config.base_path;
    let back = format!("{base}/reports/{report_id}");
    let form: LegalHoldForm = match Form::from_request(request, &state).await {
        Ok(Form(f)) => f,
        Err(error) => {
            tracing::warn!(%error, report_id, "failed to parse report legal hold form");
            return flash::redirect_error(config, &back, "Invalid form data");
        }
    };
    let clearing = form.clear.is_some();
    let until = if clearing {
        None
    } else {
        match form.legal_hold_until.as_deref().and_then(legal_hold_date) {
            Some(date) if date < time::OffsetDateTime::now_utc().date() => {
                return flash::redirect_error(config, &back, LEGAL_HOLD_IN_PAST);
            }
            Some(date) => Some(legal_hold_end_of_day(date)),
            None => {
                return flash::redirect_error(config, &back, "Choose the date the hold ends");
            }
        }
    };
    let reason = clean_string(form.legal_hold_reason.as_deref().unwrap_or(""));
    if until.is_some() && reason.is_none() {
        return flash::redirect_error(config, &back, LEGAL_HOLD_NEEDS_REASON);
    }
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let result = client
        .set_report_legal_hold(&report_id, until.as_deref(), reason.as_deref())
        .await;
    let flash = match result {
        Ok(response) => match response.legal_hold_until {
            Some(until) => FlashData::success(format!(
                "Legal hold placed until {}",
                format_admin_timestamp(&until)
            )),
            None => FlashData::success("Legal hold cleared"),
        },
        Err(error) => {
            tracing::warn!(%error, report_id, "admin API request failed: set report legal hold");
            match error {
                ApiError::Http { status: 400, .. } => FlashData::error(legal_hold_refusal(&error)),
                _ => FlashData::error("Failed to update the legal hold"),
            }
        }
    };
    flash::redirect_with_flash(&back, flash, config.secure_cookies())
}

async fn report_delete(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    Path(report_id): Path<String>,
    request: Request,
) -> Response {
    let config = state.config();
    let base = &config.base_path;
    let back = format!("{base}/reports/{report_id}");
    let navigates = htmx::is_htmx_request(request.headers())
        && htmx::targets(request.headers(), "flash-container");
    let form: DeleteForm = match Form::from_request(request, &state).await {
        Ok(Form(f)) => f,
        Err(error) => {
            tracing::warn!(%error, report_id, "failed to parse report delete form");
            return flash::redirect_error(config, &back, "Invalid form data");
        }
    };
    if form.confirm.as_deref() != Some("true") {
        return flash::redirect_error(config, &back, DELETE_NEEDS_CONFIRMATION);
    }
    let audit_log_reason = clean_string(form.audit_log_reason.as_deref().unwrap_or(""));
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    match client
        .delete_report(&report_id, audit_log_reason.as_deref())
        .await
    {
        Ok(()) => {
            let list = format!("{base}/reports");
            let flash = FlashData::success("Report deleted");
            if navigates {
                htmx::navigate_with_flash(&list, &flash, config.secure_cookies())
            } else {
                flash::redirect_with_flash(&list, flash, config.secure_cookies())
            }
        }
        Err(error) => {
            tracing::warn!(%error, report_id, "admin API request failed: delete report");
            let message = match error {
                ApiError::Http { status: 409, .. } => DELETE_REFUSED_HELD,
                ApiError::Http { status: 404, .. } => "The report no longer exists",
                _ => "Failed to delete the report",
            };
            flash::redirect_error(config, &back, message)
        }
    }
}

const DELETE_NEEDS_CONFIRMATION: &str = "Confirm that the report should be deleted";
const DELETE_REFUSED_HELD: &str =
    "The report is under a legal hold. Clear the hold before deleting it.";

const LEGAL_HOLD_IN_PAST: &str = "The hold must end in the future";
const LEGAL_HOLD_NEEDS_REASON: &str = "Give a reason for the hold";

fn legal_hold_date(date: &str) -> Option<time::Date> {
    let format = time::format_description::parse_borrowed::<2>("[year]-[month]-[day]").ok()?;
    time::Date::parse(date.trim(), &format).ok()
}

fn legal_hold_end_of_day(date: time::Date) -> String {
    format!("{date}T23:59:59.999Z")
}

fn legal_hold_refusal(error: &ApiError) -> &'static str {
    if error.has_field_error("legal_hold_reason") {
        LEGAL_HOLD_NEEDS_REASON
    } else {
        LEGAL_HOLD_IN_PAST
    }
}

fn decode_sort(sort: Option<&str>) -> (&'static str, &'static str) {
    match sort.unwrap_or("reportedAt_desc") {
        "reportedAt_asc" => ("reportedAt", "asc"),
        "createdAt_desc" => ("createdAt", "desc"),
        "createdAt_asc" => ("createdAt", "asc"),
        "resolvedAt_desc" => ("resolvedAt", "desc"),
        "resolvedAt_asc" => ("resolvedAt", "asc"),
        _ => ("reportedAt", "desc"),
    }
}
