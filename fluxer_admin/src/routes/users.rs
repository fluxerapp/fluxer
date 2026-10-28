// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    acl,
    api::{
        client::{AdminApiClient, ApiResult, ApiResultExt},
        types::AdminUser,
    },
    middleware::{auth::AuthContext, csrf::CsrfToken, flash, htmx},
    routes::user_tabs,
    state::AppState,
    templates::{
        self,
        pages::user_detail_tabs::account::{
            PASSWORD_RESET_LINK_RESULT_ID, password_reset_link_result,
        },
    },
    utils::forms::MultiValueForm,
};
use axum::{
    Router,
    extract::{Path, Query, Request, State},
    http::HeaderMap,
    response::{Html, IntoResponse, Response},
    routing::get,
};
use serde::Deserialize;

const USER_ID_LOOKUP_BATCH: usize = 100;

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
    report_id: Option<String>,
    message: Option<String>,
}

#[derive(Deserialize)]
struct ActionQuery {
    action: Option<String>,
    tab: Option<String>,
}

pub fn router() -> Router<AppState> {
    Router::new()
        .route("/users", get(users_list))
        .route("/users/{user_id}", get(user_detail).post(user_detail_post))
        .route("/users/{user_id}/tabs/{tab}", get(user_tab))
        .route("/users/{user_id}/fragment", get(user_peek))
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
    let admin_acls = auth.0.acls();
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let username_sign_in = state.account_identity(&client).await.is_username();
    let can_view_email = acl::has_permission(admin_acls, acl::USER_VIEW_EMAIL) && !username_sign_in;
    let email = match (username_sign_in, can_view_email) {
        (true, _) => templates::pages::users_list::UserEmailAccess::Hidden,
        (false, true) => templates::pages::users_list::UserEmailAccess::SearchAndView,
        (false, false) => templates::pages::users_list::UserEmailAccess::SearchOnly,
    };
    let results = if params.has_id_lookup() {
        lookup_users_in_batches(&client, &params.requested_ids)
            .await
            .log_error("lookup users by ids")
            .map(|users| (users, false, None))
    } else {
        let offset = u64::from(params.page) * u64::from(params.limit);
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
                let has_more = (r.users.len() as u64) < r.total.saturating_sub(offset);
                (r.users, has_more, Some(r.total))
            })
    };
    let view = templates::pages::users_list::UserListView {
        params: &params,
        results: results
            .as_ref()
            .map(|result| templates::pages::users_list::UserListResults {
                users: &result.0,
                has_more: result.1,
                total: result.2,
            }),
        email,
        output: if is_results_fragment {
            templates::pages::users_list::UserListOutput::Results
        } else {
            templates::pages::users_list::UserListOutput::Page
        },
    };
    let markup = templates::pages::users_list::users_list_page(config, &auth.0, view);
    Html(markup.into_string()).into_response()
}

async fn lookup_users_in_batches(
    client: &AdminApiClient,
    user_ids: &[String],
) -> ApiResult<Vec<AdminUser>> {
    let mut users = Vec::new();
    for batch in user_ids.chunks(USER_ID_LOOKUP_BATCH) {
        users.extend(client.lookup_users_by_ids(batch).await?);
    }
    Ok(users)
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
    let admin_acls = auth.0.acls();
    let tab_body = if user.is_some() {
        let account_identity = state.account_identity_settings(&client).await;
        user_tabs::render(
            &client,
            config,
            &csrf.0.0,
            &user_id,
            active_tab,
            &tq,
            admin_acls,
            account_identity,
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
    csrf: axum::Extension<CsrfToken>,
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
                config.secure_cookies(),
            );
        }
    };
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let action = aq.action.as_deref().unwrap_or("");
    if action == "create_password_reset_link" {
        return create_password_reset_link(&state, &headers, &auth.0, &csrf.0.0, &client, &user_id)
            .await;
    }
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
    flash::redirect_with_flash(&redirect, outcome.flash, config.secure_cookies())
}

async fn create_password_reset_link(
    state: &AppState,
    headers: &HeaderMap,
    auth: &AuthContext,
    csrf_token: &str,
    client: &AdminApiClient,
    user_id: &str,
) -> Response {
    let config = state.config();
    let account_url = format!("{}/users/{user_id}?tab=account", config.base_path);
    let link = match client.create_password_reset_link(user_id).await {
        Ok(link) => link,
        Err(error) => {
            tracing::warn!(%error, user_id, "admin API request failed: create password reset link");
            let flash = flash::FlashData::error("Failed to create password reset link");
            if htmx::is_htmx_request(headers)
                && (htmx::targets(headers, "flash-container")
                    || htmx::targets(headers, PASSWORD_RESET_LINK_RESULT_ID))
            {
                return htmx::toast_response(&flash);
            }
            return flash::redirect_with_flash(&account_url, flash, config.secure_cookies());
        }
    };
    if htmx::is_htmx_request(headers) && htmx::targets(headers, PASSWORD_RESET_LINK_RESULT_ID) {
        return Html(password_reset_link_result(Some(&link)).into_string()).into_response();
    }
    let admin_acls = auth.acls();
    let (user, account_identity) = tokio::join!(
        async {
            client
                .get_user_by_id(user_id)
                .await
                .log_error("load user after creating password reset link")
        },
        state.account_identity_settings(client)
    );
    let tab_body = user_tabs::render_account(
        client,
        config,
        csrf_token,
        user_id,
        &templates::pages::user_detail_tabs::account::AccountTabOptions {
            admin_acls,
            account_identity: account_identity.mode,
            username_from_sso: account_identity.sso_manages_usernames(),
            reset_link_available: account_identity.admin_reset_links_available(),
            password_reset_link: Some(&link),
        },
    )
    .await;
    let markup = templates::pages::user_detail::user_detail_with_tab(
        config,
        auth,
        user.as_ref(),
        user_id,
        "account",
        tab_body,
        htmx::targets(headers, "main-content"),
    );
    Html(markup.into_string()).into_response()
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
        report_id: None,
        message: None,
    };
    let admin_acls = auth.0.acls();
    let account_identity = state.account_identity_settings(&client).await;
    let markup = match user {
        Some(ref u) => user_tabs::render(
            &client,
            config,
            &csrf.0.0,
            &user_id,
            &tab,
            &tq,
            admin_acls,
            account_identity,
        )
        .await
        .unwrap_or_else(|| templates::pages::user_detail::simple_tab_content(config, u, &tab)),
        None => maud::html! {
            div class="p-4 text-red-600 text-sm" { "Failed to load user data." }
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
    let admin_acls = auth.0.acls();
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
        report_id: q.report_id.clone(),
        message: q.message.clone(),
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
