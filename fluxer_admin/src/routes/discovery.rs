// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::client::{AdminApiClient, ApiResultExt},
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
    let base = &config.base_path;
    let form: DiscoveryActionForm = match Form::from_request(request, &state).await {
        Ok(Form(f)) => f,
        Err(error) => {
            tracing::warn!(%error, "failed to parse discovery approve form");
            return flash::redirect_with_flash(
                &format!("{base}/discovery"),
                FlashData::error("Invalid form data"),
                config.secure_cookies(),
            );
        }
    };
    let guild_id = match clean_string(form.guild_id.as_deref().unwrap_or("")) {
        Some(id) => id,
        None => {
            return flash::redirect_with_flash(
                &format!("{base}/discovery"),
                FlashData::error("Guild ID is required"),
                config.secure_cookies(),
            );
        }
    };
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let reason = clean_string(form.reason.as_deref().unwrap_or(""));
    let result = client
        .approve_discovery_application(&guild_id, reason.as_deref())
        .await;
    let flash = match result.log_error("approve discovery application") {
        Some(_) => FlashData::success("Discovery application approved"),
        None => FlashData::error("Failed to approve discovery application"),
    };
    flash::redirect_with_flash(
        &format!("{base}/discovery?tab=pending"),
        flash,
        config.secure_cookies(),
    )
}

async fn discovery_reject(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    request: Request,
) -> Response {
    let config = state.config();
    let base = &config.base_path;
    let form: DiscoveryActionForm = match Form::from_request(request, &state).await {
        Ok(Form(f)) => f,
        Err(error) => {
            tracing::warn!(%error, "failed to parse discovery reject form");
            return flash::redirect_with_flash(
                &format!("{base}/discovery"),
                FlashData::error("Invalid form data"),
                config.secure_cookies(),
            );
        }
    };
    let guild_id = match clean_string(form.guild_id.as_deref().unwrap_or("")) {
        Some(id) => id,
        None => {
            return flash::redirect_with_flash(
                &format!("{base}/discovery"),
                FlashData::error("Guild ID is required"),
                config.secure_cookies(),
            );
        }
    };
    let reason = form.reason.as_deref().unwrap_or("").trim();
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let result = client.reject_discovery_application(&guild_id, reason).await;
    let flash = match result.log_error("reject discovery application") {
        Some(_) => FlashData::success("Discovery application rejected"),
        None => FlashData::error("Failed to reject discovery application"),
    };
    flash::redirect_with_flash(
        &format!("{base}/discovery?tab=pending"),
        flash,
        config.secure_cookies(),
    )
}

async fn discovery_remove(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    request: Request,
) -> Response {
    let config = state.config();
    let base = &config.base_path;
    let form: DiscoveryActionForm = match Form::from_request(request, &state).await {
        Ok(Form(f)) => f,
        Err(error) => {
            tracing::warn!(%error, "failed to parse discovery remove form");
            return flash::redirect_with_flash(
                &format!("{base}/discovery"),
                FlashData::error("Invalid form data"),
                config.secure_cookies(),
            );
        }
    };
    let guild_id = match clean_string(form.guild_id.as_deref().unwrap_or("")) {
        Some(id) => id,
        None => {
            return flash::redirect_with_flash(
                &format!("{base}/discovery"),
                FlashData::error("Guild ID is required"),
                config.secure_cookies(),
            );
        }
    };
    let reason = form.reason.as_deref().unwrap_or("").trim();
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let result = client.remove_from_discovery(&guild_id, reason).await;
    let flash = match result.log_error("remove discovery guild") {
        Some(_) => FlashData::success("Guild removed from discovery"),
        None => FlashData::error("Failed to remove guild from discovery"),
    };
    flash::redirect_with_flash(
        &format!("{base}/discovery?tab=listed"),
        flash,
        config.secure_cookies(),
    )
}
