// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::client::{AdminApiClient, ApiResult, ApiResultExt},
    config::AdminConfig,
    middleware::{
        auth::AuthContext,
        csrf::CsrfToken,
        flash::{self, FlashData},
    },
    state::AppState,
    templates,
    utils::forms::clean_string,
};
use axum::{
    Form, Router,
    extract::{FromRequest, Query, Request, State},
    response::{Html, IntoResponse, Response},
    routing::{get, post},
};
use serde::Deserialize;

#[derive(Deserialize)]
struct DiscoveryQuery {
    tab: Option<String>,
}

#[derive(Deserialize)]
struct DiscoveryActionForm {
    #[serde(default)]
    _csrf: Option<String>,
    #[serde(default)]
    guild_id: Option<String>,
    #[serde(default)]
    reason: Option<String>,
}

pub fn router() -> Router<AppState> {
    Router::new()
        .route("/discovery", get(discovery_page))
        .route("/discovery/approve", post(discovery_approve))
        .route("/discovery/reject", post(discovery_reject))
        .route("/discovery/remove", post(discovery_remove))
}

async fn discovery_page(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    csrf: axum::Extension<CsrfToken>,
    Query(query): Query<DiscoveryQuery>,
) -> Response {
    let config = state.config();
    let tab = match query.tab.as_deref() {
        Some("listed") => "listed",
        _ => "pending",
    };
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let (pending, listed, load_error) = if tab == "pending" {
        match client.list_pending_discovery_applications().await {
            Ok(pending) => (Some(pending), None, None),
            Err(error) => {
                tracing::warn!(%error, "admin API request failed: load pending discovery applications");
                (
                    None,
                    None,
                    Some("Failed to load pending discovery applications.".to_owned()),
                )
            }
        }
    } else {
        match client.list_discovery_listed_guilds().await {
            Ok(listed) => (None, Some(listed), None),
            Err(error) => {
                tracing::warn!(%error, "admin API request failed: load listed discovery guilds");
                (
                    None,
                    None,
                    Some("Failed to load listed discovery guilds.".to_owned()),
                )
            }
        }
    };
    let markup = templates::pages::discovery::discovery_page(
        config,
        &auth.0,
        tab,
        &csrf.0.0,
        pending.as_deref(),
        listed.as_deref(),
        load_error.as_deref(),
    );
    Html(markup.into_string()).into_response()
}

async fn discovery_approve(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    request: Request,
) -> Response {
    let config = state.config();
    let (form, guild_id) =
        match discovery_form(&state, request, "failed to parse discovery approve form").await {
            Ok(value) => value,
            Err(response) => return *response,
        };
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let reason = clean_string(form.reason.as_deref().unwrap_or(""));
    let result = client
        .approve_discovery_application(&guild_id, reason.as_deref())
        .await;
    finish_discovery(
        config,
        result,
        "approve discovery application",
        "Discovery application approved",
        "Failed to approve discovery application",
        "pending",
    )
}

async fn discovery_reject(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    request: Request,
) -> Response {
    let config = state.config();
    let (form, guild_id) =
        match discovery_form(&state, request, "failed to parse discovery reject form").await {
            Ok(value) => value,
            Err(response) => return *response,
        };
    let reason = form.reason.as_deref().unwrap_or("").trim();
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let result = client.reject_discovery_application(&guild_id, reason).await;
    finish_discovery(
        config,
        result,
        "reject discovery application",
        "Discovery application rejected",
        "Failed to reject discovery application",
        "pending",
    )
}

async fn discovery_remove(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    request: Request,
) -> Response {
    let config = state.config();
    let (form, guild_id) =
        match discovery_form(&state, request, "failed to parse discovery remove form").await {
            Ok(value) => value,
            Err(response) => return *response,
        };
    let reason = form.reason.as_deref().unwrap_or("").trim();
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let result = client.remove_from_discovery(&guild_id, reason).await;
    finish_discovery(
        config,
        result,
        "remove discovery guild",
        "Guild removed from discovery",
        "Failed to remove guild from discovery",
        "listed",
    )
}

async fn discovery_form(
    state: &AppState,
    request: Request,
    log_label: &str,
) -> Result<(DiscoveryActionForm, String), Box<Response>> {
    let config = state.config();
    let target = format!("{}/discovery", config.base_path);
    let form = match Form::<DiscoveryActionForm>::from_request(request, state).await {
        Ok(Form(form)) => form,
        Err(error) => {
            tracing::warn!(%error, "{log_label}");
            return Err(Box::new(flash::redirect_with_flash(
                &target,
                FlashData::error("Invalid form data"),
                config.secure_cookies(),
            )));
        }
    };
    let Some(guild_id) = clean_string(form.guild_id.as_deref().unwrap_or("")) else {
        return Err(Box::new(flash::redirect_with_flash(
            &target,
            FlashData::error("Guild ID is required"),
            config.secure_cookies(),
        )));
    };
    Ok((form, guild_id))
}

fn finish_discovery<T>(
    config: &AdminConfig,
    result: ApiResult<T>,
    log_label: &str,
    ok_msg: &str,
    err_msg: &str,
    tab: &str,
) -> Response {
    let data = if result.log_error(log_label).is_some() {
        FlashData::success(ok_msg)
    } else {
        FlashData::error(err_msg)
    };
    flash::redirect_with_flash(
        &format!("{}/discovery?tab={tab}", config.base_path),
        data,
        config.secure_cookies(),
    )
}
