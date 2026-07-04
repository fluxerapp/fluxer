// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    acl,
    activity::{ActivityImageFetchError, fetch_activity_image},
    api::client::{AdminApiClient, ApiResultExt},
    middleware::{auth::AuthContext, csrf::CsrfToken, flash, htmx},
    routes::user_tabs,
    state::AppState,
    templates,
    utils::forms::MultiValueForm,
};
use axum::{
    Router,
    extract::{Path, Query, Request, State},
    http::{HeaderMap, HeaderValue, StatusCode, header},
    response::{Html, IntoResponse, Response},
    routing::get,
};
use serde::Deserialize;

#[derive(Deserialize)]
struct UserListQuery {
    q: Option<String>,
    email: Option<String>,
    ip: Option<String>,
    ids: Option<String>,
    limit: Option<u32>,
    page: Option<u32>,
}

#[derive(Deserialize)]
struct UserDetailQuery {
    tab: Option<String>,
    reports_sent_page: Option<u32>,
    reports_received_page: Option<u32>,
    reports_limit: Option<u32>,
    dm_before: Option<String>,
    dm_after: Option<String>,
    dm_limit: Option<u32>,
    audit_logs_page: Option<u32>,
    message_shred_job_id: Option<String>,
    delete_all_messages_dry_run: Option<String>,
    delete_all_messages_channel_count: Option<u64>,
    delete_all_messages_message_count: Option<u64>,
}

#[derive(Deserialize)]
struct ActionQuery {
    action: Option<String>,
    tab: Option<String>,
}

#[derive(Deserialize)]
struct ActivityImageQuery {
    url: String,
}

pub fn router() -> Router<AppState> {
    Router::new()
        .route("/users", get(users_list))
        .route("/activity-image", get(activity_image))
        .route("/users/{user_id}", get(user_detail).post(user_detail_post))
        .route(
            "/users/{user_id}/activity-fragment",
            get(user_activity_fragment),
        )
        .route("/users/{user_id}/tabs/{tab}", get(user_tab))
        .route("/users/{user_id}/peek", get(user_peek))
        .route("/users/{user_id}/fragment", get(user_peek))
}

async fn activity_image(Query(query): Query<ActivityImageQuery>) -> Response {
    match fetch_activity_image(&query.url).await {
        Ok((content_type, bytes)) => {
            let mut response = (StatusCode::OK, bytes).into_response();
            if let Ok(value) = HeaderValue::from_str(&content_type) {
                response.headers_mut().insert(header::CONTENT_TYPE, value);
            }
            response.headers_mut().insert(
                header::CACHE_CONTROL,
                HeaderValue::from_static("public, max-age=3600"),
            );
            response
        }
        Err(
            ActivityImageFetchError::InvalidUrl
            | ActivityImageFetchError::UntrustedHost
            | ActivityImageFetchError::BadContentType,
        ) => (StatusCode::BAD_REQUEST, "").into_response(),
        Err(ActivityImageFetchError::TooLarge) => {
            (StatusCode::PAYLOAD_TOO_LARGE, "").into_response()
        }
        Err(ActivityImageFetchError::BadStatus(StatusCode::NOT_FOUND)) => {
            (StatusCode::NOT_FOUND, "").into_response()
        }
        Err(ActivityImageFetchError::BadStatus(_)) | Err(ActivityImageFetchError::Upstream(_)) => {
            (StatusCode::BAD_GATEWAY, "").into_response()
        }
    }
}

async fn users_list(
    State(state): State<AppState>,
    headers: HeaderMap,
    auth: axum::Extension<AuthContext>,
    Query(query): Query<UserListQuery>,
) -> Response {
    let config = state.config();
    let params = templates::pages::users_list::UserListParams::from_query(
        query.q,
        query.email,
        query.ip,
        query.ids,
        query.limit,
        query.page,
    );
    let is_results_fragment = htmx::targets(&headers, "users-results");
    let admin_acls = auth
        .0
        .admin_user
        .as_ref()
        .map(|user| user.acls.as_slice())
        .unwrap_or(&[]);
    let can_view_email = acl::has_permission(admin_acls, acl::USER_VIEW_EMAIL);
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let results = if params.has_id_lookup() {
        let users = client
            .lookup_users_by_ids(&params.requested_ids)
            .await
            .map_err(
                |error| tracing::warn!(%error, "admin API request failed: lookup users by ids"),
            )
            .unwrap_or_default();
        Some((users, false))
    } else if params.has_search() {
        let offset = params.page.saturating_mul(params.limit);
        client
            .search_users(
                params.search_query(),
                params.email_query(),
                params.ip_query(),
                params.limit,
                offset,
            )
            .await
            .log_error("search users")
            .map(|r| {
                let has_more = u64::from(offset) + (r.users.len() as u64) < r.total;
                (r.users, has_more)
            })
    } else {
        None
    };
    let result_users = results.as_ref().map(|r| r.0.as_slice());
    let has_more = results.as_ref().is_some_and(|r| r.1);
    let markup = templates::pages::users_list::users_list_page(
        config,
        &auth.0,
        &params,
        result_users,
        has_more,
        can_view_email,
        is_results_fragment,
    );
    Html(markup.into_string()).into_response()
}

async fn user_detail(
    State(state): State<AppState>,
    headers: HeaderMap,
    auth: axum::Extension<AuthContext>,
    csrf: axum::Extension<CsrfToken>,
    Path(user_id): Path<String>,
    Query(query): Query<UserDetailQuery>,
) -> Response {
    let config = state.config();
    let is_detail_fragment = htmx::targets(&headers, "main-content");
    let active_tab = query.tab.as_deref().unwrap_or("overview");
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let user = client
        .get_user_by_id(&user_id)
        .await
        .log_error("load user detail");
    let tq = to_tab_query(&query);
    let admin_acls = auth
        .0
        .admin_user
        .as_ref()
        .map(|user| user.acls.as_slice())
        .unwrap_or(&[]);
    let tab_body = if user.is_some() {
        user_tabs::render(
            &client, config, &csrf.0.0, &user_id, active_tab, &tq, admin_acls,
        )
        .await
    } else {
        None
    };
    let markup = templates::pages::user_detail::user_detail_with_tab(
        config,
        &auth.0,
        user.as_ref(),
        &user_id,
        active_tab,
        tab_body,
        is_detail_fragment,
    );
    Html(markup.into_string()).into_response()
}

async fn user_detail_post(
    State(state): State<AppState>,
    headers: HeaderMap,
    auth: axum::Extension<AuthContext>,
    Path(user_id): Path<String>,
    Query(aq): Query<ActionQuery>,
    request: Request,
) -> Response {
    let config = state.config();
    let base = &config.base_path;
    let tab = aq.tab.as_deref().unwrap_or("");
    let form = match MultiValueForm::from_request(request).await {
        Some(form) => form,
        None => {
            let flash = flash::FlashData::error("Invalid form data");
            if htmx::is_htmx_request(&headers) && htmx::targets(&headers, "flash-container") {
                return htmx::toast_response(&flash);
            }
            return flash::redirect_with_flash(
                &format!("{base}/users/{user_id}"),
                flash,
                config.is_production(),
            );
        }
    };
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let action = aq.action.as_deref().unwrap_or("");
    let outcome = super::user_actions::dispatch(&client, &user_id, action, &form).await;
    let mut redirect = if tab.is_empty() {
        format!("{base}/users/{user_id}")
    } else {
        format!("{base}/users/{user_id}?tab={tab}")
    };
    append_query_params(&mut redirect, &outcome.redirect_params);
    if htmx::is_htmx_request(&headers)
        && htmx::targets(&headers, "flash-container")
        && outcome.redirect_params.is_empty()
    {
        return htmx::toast_response(&outcome.flash);
    }
    flash::redirect_with_flash(&redirect, outcome.flash, config.is_production())
}

async fn user_tab(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    csrf: axum::Extension<CsrfToken>,
    Path((user_id, tab)): Path<(String, String)>,
) -> Response {
    let config = state.config();
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let user = client
        .get_user_by_id(&user_id)
        .await
        .log_error("load user tab detail");
    let tq = user_tabs::TabQuery {
        reports_sent_page: None,
        reports_received_page: None,
        reports_limit: None,
        dm_before: None,
        dm_after: None,
        dm_limit: None,
        audit_logs_page: None,
        message_shred_job_id: None,
        delete_all_messages_dry_run: None,
        delete_all_messages_channel_count: None,
        delete_all_messages_message_count: None,
    };
    let admin_acls = auth
        .0
        .admin_user
        .as_ref()
        .map(|user| user.acls.as_slice())
        .unwrap_or(&[]);
    let markup = match user {
        Some(ref u) => {
            user_tabs::render(&client, config, &csrf.0.0, &user_id, &tab, &tq, admin_acls)
                .await
                .unwrap_or_else(|| {
                    templates::pages::user_detail::simple_tab_content(config, u, &tab)
                })
        }
        None => maud::html! {
            div class="p-4 text-red-600 text-sm" { "Failed to load user data." }
        },
    };
    Html(markup.into_string()).into_response()
}

async fn user_activity_fragment(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    Path(user_id): Path<String>,
) -> Response {
    let config = state.config();
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let user = client
        .get_user_by_id(&user_id)
        .await
        .log_error("load user activity fragment");
    let markup = match user {
        Some(ref u) => templates::pages::user_detail_tabs::overview::activity_detail_row(config, u),
        None => maud::html! {
            div class="flex flex-col gap-1 py-3 sm:flex-row sm:gap-4" id="user-activity-row" {
                dt class="w-48 flex-shrink-0 text-sm font-medium text-neutral-500" { "Activity" }
                dd class="min-w-0 flex-1 text-sm text-neutral-400" { "None" }
            }
        },
    };
    Html(markup.into_string()).into_response()
}

async fn user_peek(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    Path(user_id): Path<String>,
) -> Response {
    let config = state.config();
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let user = client
        .get_user_by_id(&user_id)
        .await
        .log_error("load user peek");
    let admin_acls = auth
        .0
        .admin_user
        .as_ref()
        .map(|user| user.acls.as_slice())
        .unwrap_or(&[]);
    let markup = match user {
        Some(ref u) => templates::pages::user_peek::user_peek_fragment(config, u, admin_acls),
        None => maud::html! {
            div class="p-4 text-red-600 text-sm" { "User not found." }
        },
    };
    Html(markup.into_string()).into_response()
}

fn to_tab_query(q: &UserDetailQuery) -> user_tabs::TabQuery {
    user_tabs::TabQuery {
        reports_sent_page: q.reports_sent_page,
        reports_received_page: q.reports_received_page,
        reports_limit: q.reports_limit,
        dm_before: q.dm_before.clone(),
        dm_after: q.dm_after.clone(),
        dm_limit: q.dm_limit,
        audit_logs_page: q.audit_logs_page,
        message_shred_job_id: q.message_shred_job_id.clone(),
        delete_all_messages_dry_run: q.delete_all_messages_dry_run.clone(),
        delete_all_messages_channel_count: q.delete_all_messages_channel_count,
        delete_all_messages_message_count: q.delete_all_messages_message_count,
    }
}

fn append_query_params(url: &mut String, params: &[(String, String)]) {
    for (key, value) in params {
        let separator = if url.contains('?') { '&' } else { '?' };
        url.push(separator);
        url.push_str(&urlencoding::encode(key));
        url.push('=');
        url.push_str(&urlencoding::encode(value));
    }
}
