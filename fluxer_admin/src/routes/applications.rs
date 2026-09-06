// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    acl,
    api::client::{AdminApiClient, ApiResultExt},
    middleware::{
        auth::AuthContext,
        csrf,
        flash::{self, FlashData},
        htmx,
    },
    state::AppState,
    templates,
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

use super::ActionQuery;

#[derive(Deserialize)]
struct ApplicationsQuery {
    application_id: Option<String>,
    owner_id: Option<String>,
}

pub fn router() -> Router<AppState> {
    Router::new()
        .route("/applications", get(applications_list))
        .route(
            "/applications/{application_id}",
            get(application_detail).post(application_detail_post),
        )
}

async fn applications_list(
    State(state): State<AppState>,
    headers: HeaderMap,
    auth: axum::Extension<AuthContext>,
    Query(query): Query<ApplicationsQuery>,
) -> Response {
    let config = state.config();
    let is_results_fragment = htmx::targets(&headers, "applications-results");
    let app_id = query.application_id.as_deref().unwrap_or("");
    let owner_id = query.owner_id.as_deref().unwrap_or("");

    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);

    let mut applications = None;
    let mut error = None;
    let mut owner_count = None;

    let trimmed_app_id = app_id.trim();
    let trimmed_owner_id = owner_id.trim();

    if !trimmed_app_id.is_empty() {
        match client.lookup_application(trimmed_app_id).await {
            Ok(Some(app)) => applications = Some(vec![app]),
            Ok(None) => error = Some("Application not found".to_string()),
            Err(e) => {
                tracing::warn!(%e, application_id = trimmed_app_id, "admin API request failed: lookup application");
                error = Some(e.to_string());
            }
        }
    } else if !trimmed_owner_id.is_empty() {
        match client.list_user_applications(trimmed_owner_id).await {
            Ok(apps) => {
                owner_count = Some(apps.len());
                applications = Some(apps);
            }
            Err(e) => {
                tracing::warn!(%e, owner_id = trimmed_owner_id, "admin API request failed: list applications by owner");
                error = Some(e.to_string());
            }
        }
    }

    let markup = templates::pages::applications_list::applications_list_page(
        config,
        &auth.0,
        &templates::pages::applications_list::ApplicationsListParams {
            app_id_query: trimmed_app_id,
            owner_id_query: trimmed_owner_id,
            applications: applications.as_deref(),
            owner_count,
            error: error.as_deref(),
            is_htmx: is_results_fragment,
        },
    );
    Html(markup.into_string()).into_response()
}

async fn application_detail(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    Path(application_id): Path<String>,
    request: Request,
) -> Response {
    let config = state.config();
    let is_detail_fragment = htmx::targets(request.headers(), "main-content");
    let csrf_token = csrf::get_csrf_token(&request);
    let admin_acls = auth
        .0
        .admin_user
        .as_ref()
        .map(|user| user.acls.as_slice())
        .unwrap_or(&[]);
    let can_transfer_ownership =
        acl::has_permission(admin_acls, acl::APPLICATION_TRANSFER_OWNERSHIP);
    let can_list_by_owner = acl::has_permission(admin_acls, acl::APPLICATION_LIST_BY_OWNER);

    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let application = client
        .lookup_application(&application_id)
        .await
        .log_error("load application detail")
        .flatten();

    let detail_page = templates::pages::application_detail::ApplicationDetailPage {
        config,
        auth: &auth.0,
        application: application.as_ref(),
        application_id: &application_id,
        is_htmx: is_detail_fragment,
        csrf_token: &csrf_token,
        can_transfer_ownership,
        can_list_by_owner,
    };
    let markup = templates::pages::application_detail::application_detail_page(detail_page);
    Html(markup.into_string()).into_response()
}

async fn application_detail_post(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    Path(application_id): Path<String>,
    Query(aq): Query<ActionQuery>,
    request: Request,
) -> Response {
    let config = state.config();
    let base = &config.base_path;
    let form = match MultiValueForm::from_request(request).await {
        Some(form) => form,
        None => {
            return flash::redirect_with_flash(
                &format!("{base}/applications/{application_id}"),
                FlashData::error("Invalid form data"),
                config.secure_cookies(),
            );
        }
    };
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let action = aq.action.as_deref().unwrap_or("");
    let flash = match action {
        "transfer_ownership" => match form.clean("new_owner_id") {
            Some(new_owner_id) => {
                match client
                    .transfer_application_ownership(&application_id, &new_owner_id)
                    .await
                {
                    Ok(_) => FlashData::success("Application ownership transferred successfully"),
                    Err(error) => {
                        tracing::warn!(%error, application_id, "admin API request failed: transfer application ownership");
                        FlashData::error("Failed to transfer application ownership")
                    }
                }
            }
            None => FlashData::error("New owner user ID is required"),
        },
        _ => FlashData::error("Unknown action"),
    };
    flash::redirect_with_flash(
        &format!("{base}/applications/{application_id}"),
        flash,
        config.secure_cookies(),
    )
}
