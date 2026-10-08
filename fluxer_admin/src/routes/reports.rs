// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    acl,
    api::{
        client::{AdminApiClient, ApiError, ApiResultExt},
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
    utils::{forms::clean_string, timestamps::format_admin_timestamp},
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
    let status = query.status.as_deref().and_then(|s| s.parse::<i32>().ok());
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
            status: query.status.as_deref(),
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
            let live = load_live_profile(&client, &auth.0, &report).await;
            let markup = templates::pages::report_detail::report_detail_page(
                config,
                &auth.0,
                &report,
                &live,
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
            return flash::redirect_with_flash(
                &back,
                FlashData::error("Invalid form data"),
                config.secure_cookies(),
            );
        }
    };
    let Some((resolution, label)) =
        templates::pages::report_detail::resolution_choice(form.resolution.as_deref())
    else {
        return flash::redirect_with_flash(
            &back,
            FlashData::error(RESOLUTION_REQUIRED),
            config.secure_cookies(),
        );
    };
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let public_comment = clean_string(form.public_comment.as_deref().unwrap_or(""));
    let notify_reporter =
        form.notify_reporter_present.is_none() || form.notify_reporter.as_deref() == Some("true");
    let flash = match client
        .resolve_report(
            &report_id,
            resolution,
            public_comment.as_deref(),
            notify_reporter,
            None,
        )
        .await
    {
        Ok(_) => FlashData::success(format!("Report resolved: {label}")),
        Err(error) => {
            tracing::warn!(%error, report_id, "admin API request failed: resolve report");
            FlashData::error("Failed to resolve report")
        }
    };
    flash::redirect_with_flash(&back, flash, config.secure_cookies())
}

const RESOLUTION_REQUIRED: &str = "Choose a resolution";

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
            return flash::redirect_with_flash(
                &back,
                FlashData::error("Invalid form data"),
                config.secure_cookies(),
            );
        }
    };
    let clearing = form.clear.is_some();
    let until = if clearing {
        None
    } else {
        match form.legal_hold_until.as_deref().and_then(legal_hold_date) {
            Some(date) if date < time::OffsetDateTime::now_utc().date() => {
                return flash::redirect_with_flash(
                    &back,
                    FlashData::error(LEGAL_HOLD_IN_PAST),
                    config.secure_cookies(),
                );
            }
            Some(date) => Some(legal_hold_end_of_day(date)),
            None => {
                return flash::redirect_with_flash(
                    &back,
                    FlashData::error("Choose the date the hold ends"),
                    config.secure_cookies(),
                );
            }
        }
    };
    let reason = clean_string(form.legal_hold_reason.as_deref().unwrap_or(""));
    if until.is_some() && reason.is_none() {
        return flash::redirect_with_flash(
            &back,
            FlashData::error(LEGAL_HOLD_NEEDS_REASON),
            config.secure_cookies(),
        );
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
                ApiError::Http {
                    status: 400,
                    message,
                } => FlashData::error(legal_hold_refusal(&message)),
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
            return flash::redirect_with_flash(
                &back,
                FlashData::error("Invalid form data"),
                config.secure_cookies(),
            );
        }
    };
    if form.confirm.as_deref() != Some("true") {
        return flash::redirect_with_flash(
            &back,
            FlashData::error(DELETE_NEEDS_CONFIRMATION),
            config.secure_cookies(),
        );
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
            flash::redirect_with_flash(&back, FlashData::error(message), config.secure_cookies())
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

fn legal_hold_refusal(body: &str) -> &'static str {
    let body: serde_json::Value = serde_json::from_str(body).unwrap_or_default();
    let reason_refused = body["errors"]
        .as_array()
        .into_iter()
        .flatten()
        .any(|error| error["path"] == "legal_hold_reason");
    if reason_refused {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn legal_hold_dates_end_at_the_end_of_the_day_in_utc() {
        assert_eq!(
            legal_hold_date("2027-01-31")
                .map(legal_hold_end_of_day)
                .as_deref(),
            Some("2027-01-31T23:59:59.999Z")
        );
        assert_eq!(
            legal_hold_date(" 2027-02-01 ")
                .map(legal_hold_end_of_day)
                .as_deref(),
            Some("2027-02-01T23:59:59.999Z")
        );
        for invalid in ["", "2027-02-30", "31/01/2027", "2027-1-31", "tomorrow"] {
            assert_eq!(legal_hold_date(invalid), None, "{invalid}");
        }
    }

    #[test]
    fn a_refused_hold_names_the_field_the_api_refused() {
        let refusal = |path: &str| {
            serde_json::json!({
                "code": "INVALID_FORM_BODY",
                "message": "Input Validation Error",
                "errors": [{"path": path, "message": "refused"}]
            })
            .to_string()
        };
        assert_eq!(
            legal_hold_refusal(&refusal("legal_hold_until")),
            "The hold must end in the future"
        );
        assert_eq!(
            legal_hold_refusal(&refusal("legal_hold_reason")),
            "Give a reason for the hold"
        );
        assert_eq!(
            legal_hold_refusal("not json"),
            "The hold must end in the future"
        );
    }
}
