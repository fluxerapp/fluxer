// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::client::{AdminApiClient, ApiResultExt},
    middleware::{
        auth::AuthContext,
        csrf,
        flash::{self, FlashData},
        htmx,
    },
    state::AppState,
    templates,
    utils::forms::clean_string,
};
use axum::{
    Form, Router,
    extract::{FromRequest, Path, Query, Request, State},
    http::HeaderMap,
    http::StatusCode,
    response::{Html, IntoResponse, Redirect, Response},
    routing::get,
};
use serde::Deserialize;

#[derive(Deserialize)]
struct ReportsQuery {
    q: Option<String>,
    status: Option<String>,
    #[serde(rename = "type")]
    report_type: Option<String>,
    category: Option<String>,
    reporter_id: Option<String>,
    reported_user_id: Option<String>,
    reported_guild_id: Option<String>,
    reported_channel_id: Option<String>,
    guild_context_id: Option<String>,
    resolved_by_admin_id: Option<String>,
    sort: Option<String>,
    limit: Option<u32>,
    page: Option<u32>,
}

#[derive(Deserialize)]
struct ResolveForm {
    #[serde(default)]
    _csrf: Option<String>,
    #[serde(default)]
    resolution: Option<String>,
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
    let offset = page.saturating_mul(limit);
    let (sort_by, sort_order) = decode_sort(query.sort.as_deref());
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let status = query.status.as_deref().and_then(|s| s.parse::<i32>().ok());
    let report_type = query
        .report_type
        .as_deref()
        .and_then(|s| s.parse::<i32>().ok());
    let reports = client
        .search_reports(
            query.q.as_deref(),
            status,
            report_type,
            query.category.as_deref(),
            query.reporter_id.as_deref(),
            query.reported_user_id.as_deref(),
            query.reported_guild_id.as_deref(),
            query.reported_channel_id.as_deref(),
            query.guild_context_id.as_deref(),
            query.resolved_by_admin_id.as_deref(),
            Some(sort_by),
            Some(sort_order),
            limit,
            offset,
        )
        .await
        .log_error("search reports");

    let markup = templates::pages::reports_list::reports_list_page(
        config,
        &auth.0,
        reports.as_ref(),
        &templates::pages::reports_list::ReportFilters {
            query: query.q.as_deref(),
            status: query.status.as_deref(),
            report_type: query.report_type.as_deref(),
            category: query.category.as_deref(),
            reporter_id: query.reporter_id.as_deref(),
            reported_user_id: query.reported_user_id.as_deref(),
            reported_guild_id: query.reported_guild_id.as_deref(),
            reported_channel_id: query.reported_channel_id.as_deref(),
            guild_context_id: query.guild_context_id.as_deref(),
            resolved_by_admin_id: query.resolved_by_admin_id.as_deref(),
            sort: query.sort.as_deref().unwrap_or("reportedAt_desc"),
        },
        page,
        limit,
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
            let markup = templates::pages::report_detail::report_detail_page(
                config,
                &auth.0,
                &report,
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
    let is_background = request
        .uri()
        .query()
        .is_some_and(|query| query.split('&').any(|part| part == "background=1"));
    let form: ResolveForm = match Form::from_request(request, &state).await {
        Ok(Form(f)) => f,
        Err(error) => {
            tracing::warn!(%error, report_id, "failed to parse report resolve form");
            return flash::redirect_with_flash(
                &format!("{base}/reports/{report_id}"),
                FlashData::error("Invalid form data"),
                config.secure_cookies(),
            );
        }
    };
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let public_comment = clean_string(form.resolution.as_deref().unwrap_or(""));
    let result = client
        .resolve_report(&report_id, public_comment.as_deref(), None)
        .await;
    match result {
        Ok(_) => {
            if is_background {
                return StatusCode::NO_CONTENT.into_response();
            }
            flash::redirect_with_flash(
                &format!("{base}/reports/{report_id}"),
                FlashData::success("Report resolved"),
                config.secure_cookies(),
            )
        }
        Err(error) => {
            tracing::warn!(%error, report_id, "admin API request failed: resolve report");
            if is_background {
                return StatusCode::BAD_GATEWAY.into_response();
            }
            flash::redirect_with_flash(
                &format!("{base}/reports/{report_id}"),
                FlashData::error("Failed to resolve report"),
                config.secure_cookies(),
            )
        }
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
