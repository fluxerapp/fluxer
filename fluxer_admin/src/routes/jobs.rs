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
};
use axum::{
    Form, Json, Router,
    extract::{FromRequest, Path, Query, Request, State},
    http::HeaderMap,
    response::{Html, IntoResponse, Response},
    routing::get,
};
use serde::Deserialize;

#[derive(Deserialize)]
#[allow(dead_code)]
struct JobsQuery {
    status: Option<String>,
    task_type: Option<String>,
    requested_by_user_id: Option<String>,
    max_lookback_days: Option<String>,
    cursor_bucket_day: Option<String>,
    cursor_created_at: Option<String>,
    cursor_job_id: Option<String>,
}

#[derive(Deserialize)]
struct JobActionQuery {
    action: Option<String>,
}

#[derive(Deserialize)]
struct JobActionForm {
    audit_log_reason: Option<String>,
    #[serde(default)]
    _csrf: Option<String>,
}

pub fn router() -> Router<AppState> {
    Router::new()
        .route("/jobs", get(jobs_list))
        .route("/jobs/active.json", get(jobs_active_json))
        .route("/jobs/{job_id}", get(job_detail).post(job_detail_post))
}

fn parse_lookback_days(raw: Option<&str>) -> u32 {
    raw.and_then(|s| s.parse::<u32>().ok())
        .map(|n| n.clamp(1, 60))
        .unwrap_or(14)
}

async fn jobs_list(
    State(state): State<AppState>,
    headers: HeaderMap,
    auth: axum::Extension<AuthContext>,
    Query(query): Query<JobsQuery>,
) -> Response {
    let config = state.config();
    let status = query.status.as_deref().unwrap_or("");
    let task_type = query.task_type.as_deref().unwrap_or("");
    let requester = query.requested_by_user_id.as_deref().unwrap_or("");
    let max_lookback_days = parse_lookback_days(query.max_lookback_days.as_deref());

    let client =
        crate::api::client::AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let cursor = match (
        &query.cursor_bucket_day,
        &query.cursor_created_at,
        &query.cursor_job_id,
    ) {
        (Some(bd), Some(ca), Some(ji)) => Some(serde_json::json!({
            "bucket_day": bd,
            "created_at": ca,
            "job_id": ji,
        })),
        _ => None,
    };
    let params = crate::api::jobs::ListJobsParams {
        limit: 50,
        cursor,
        max_lookback_days,
        status: if status.is_empty() {
            None
        } else {
            Some(status.to_owned())
        },
        task_type: if task_type.is_empty() {
            None
        } else {
            Some(task_type.to_owned())
        },
        requested_by_user_id: if requester.is_empty() {
            None
        } else {
            Some(requester.to_owned())
        },
    };
    let (jobs, next_cursor) = match client.list_jobs(&params).await {
        Ok(resp) => (resp.jobs, resp.next_cursor.or(resp.cursor)),
        Err(error) => {
            tracing::warn!(%error, "admin API request failed: list jobs");
            (vec![], None)
        }
    };

    let current_url = jobs_url(config, &query);
    let list_params = templates::pages::jobs_list::JobsListParams {
        status_filter: status,
        task_type_filter: task_type,
        requester_filter: requester,
        max_lookback_days,
        current_url: &current_url,
        jobs: &jobs,
        next_cursor: next_cursor.as_ref(),
    };
    let markup = if htmx::targets(&headers, "jobs-results") {
        templates::pages::jobs_list::jobs_results(config, &list_params)
    } else {
        templates::pages::jobs_list::jobs_list_page(config, &auth.0, &list_params)
    };
    Html(markup.into_string()).into_response()
}

async fn jobs_active_json(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
) -> Response {
    let config = state.config();
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    match client.list_active_jobs().await {
        Ok(data) => Json(serde_json::json!(data)).into_response(),
        Err(error) => {
            tracing::warn!(%error, "admin API request failed: list active jobs");
            Json(serde_json::json!({ "jobs": [] })).into_response()
        }
    }
}

async fn job_detail(
    State(state): State<AppState>,
    headers: HeaderMap,
    auth: axum::Extension<AuthContext>,
    Path(job_id): Path<String>,
    request: Request,
) -> Response {
    let config = state.config();
    let is_detail_fragment = htmx::targets(&headers, "main-content");
    let csrf_token = csrf::get_csrf_token(&request);
    let client =
        crate::api::client::AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let job = client
        .get_job(&job_id)
        .await
        .log_error("load job detail")
        .map(|r| r.job);
    let markup = templates::pages::job_detail::job_detail_page(
        config,
        &auth.0,
        &job_id,
        job.as_ref(),
        &csrf_token,
        is_detail_fragment,
    );
    Html(markup.into_string()).into_response()
}

async fn job_detail_post(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    Path(job_id): Path<String>,
    Query(aq): Query<JobActionQuery>,
    request: Request,
) -> Response {
    let config = state.config();
    let base = &config.base_path;
    let form: JobActionForm = match Form::from_request(request, &state).await {
        Ok(Form(f)) => f,
        Err(_) => {
            return flash::redirect_with_flash(
                &format!("{base}/jobs/{job_id}"),
                FlashData::error("Invalid form data"),
                config.secure_cookies(),
            );
        }
    };
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let flash = if aq.action.as_deref() == Some("cancel") {
        let reason = form.audit_log_reason.as_deref().unwrap_or("").trim();
        if reason.is_empty() {
            FlashData::error("Audit reason is required")
        } else {
            match client.cancel_job(&job_id, Some(reason)).await {
                Ok(result) if result.cancelled => {
                    FlashData::success("Cancellation requested. Waiting for handler checkpoint.")
                }
                Ok(_) => FlashData::info("Job is already terminal; nothing to cancel."),
                Err(error) => {
                    tracing::warn!(%error, job_id, "admin API request failed: cancel job");
                    FlashData::error("Failed to request cancellation")
                }
            }
        }
    } else {
        FlashData::error("Unknown action")
    };
    flash::redirect_with_flash(
        &format!("{base}/jobs/{job_id}"),
        flash,
        config.secure_cookies(),
    )
}

fn jobs_url(config: &crate::config::AdminConfig, query: &JobsQuery) -> String {
    let mut params = Vec::new();
    push_query(&mut params, "status", query.status.as_deref());
    push_query(&mut params, "task_type", query.task_type.as_deref());
    push_query(
        &mut params,
        "requested_by_user_id",
        query.requested_by_user_id.as_deref(),
    );
    push_query(
        &mut params,
        "max_lookback_days",
        query.max_lookback_days.as_deref(),
    );
    push_query(
        &mut params,
        "cursor_bucket_day",
        query.cursor_bucket_day.as_deref(),
    );
    push_query(
        &mut params,
        "cursor_created_at",
        query.cursor_created_at.as_deref(),
    );
    push_query(&mut params, "cursor_job_id", query.cursor_job_id.as_deref());
    if params.is_empty() {
        return format!("{}/jobs", config.base_path);
    }
    let query = params
        .iter()
        .map(|(key, value)| {
            format!(
                "{}={}",
                urlencoding::encode(key),
                urlencoding::encode(value)
            )
        })
        .collect::<Vec<_>>()
        .join("&");
    format!("{}/jobs?{query}", config.base_path)
}

fn push_query(params: &mut Vec<(String, String)>, key: &str, value: Option<&str>) {
    if let Some(value) = value.filter(|value| !value.is_empty()) {
        params.push((key.to_owned(), value.to_owned()));
    }
}
