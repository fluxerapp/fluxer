// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::{client::AdminApiClient, types::PendingRegistrationStatus},
    middleware::{
        auth::AuthContext,
        csrf::CsrfToken,
        flash::{self, FlashData},
        htmx,
    },
    state::AppState,
    templates::{self, layout_sidebar::pending_registration_count_markup},
    utils::forms::clean_string,
};
use axum::{
    Form, Router,
    extract::State,
    http::HeaderMap,
    response::{Html, IntoResponse, Response},
    routing::{get, post},
};
use serde::Deserialize;

#[derive(Deserialize)]
struct PendingRegistrationForm {
    #[serde(default)]
    user_id: Option<String>,
}

pub fn router() -> Router<AppState> {
    Router::new()
        .route("/pending-registrations", get(pending_registrations_page))
        .route(
            "/pending-registrations/approve",
            post(approve_pending_registration),
        )
        .route(
            "/pending-registrations/reject",
            post(reject_pending_registration),
        )
}

async fn pending_registrations_page(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    csrf: axum::Extension<CsrfToken>,
) -> Response {
    let config = state.config();
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let pending_registrations = match client.list_pending_registrations().await {
        Ok(pending_registrations) => Some(pending_registrations),
        Err(error) => {
            tracing::warn!(%error, "admin API request failed: list pending registrations");
            None
        }
    };
    let markup = templates::pages::pending_registrations::pending_registrations_page(
        config,
        &auth.0,
        &csrf.0.0,
        pending_registrations.as_deref(),
    );
    Html(markup.into_string()).into_response()
}

async fn approve_pending_registration(
    state: State<AppState>,
    headers: HeaderMap,
    auth: axum::Extension<AuthContext>,
    csrf: axum::Extension<CsrfToken>,
    form: Form<PendingRegistrationForm>,
) -> Response {
    decide_pending_registration(
        state,
        headers,
        auth,
        csrf,
        form,
        PendingRegistrationStatus::Approved,
    )
    .await
}

async fn reject_pending_registration(
    state: State<AppState>,
    headers: HeaderMap,
    auth: axum::Extension<AuthContext>,
    csrf: axum::Extension<CsrfToken>,
    form: Form<PendingRegistrationForm>,
) -> Response {
    decide_pending_registration(
        state,
        headers,
        auth,
        csrf,
        form,
        PendingRegistrationStatus::Rejected,
    )
    .await
}

async fn decide_pending_registration(
    State(state): State<AppState>,
    headers: HeaderMap,
    auth: axum::Extension<AuthContext>,
    csrf: axum::Extension<CsrfToken>,
    Form(form): Form<PendingRegistrationForm>,
    status: PendingRegistrationStatus,
) -> Response {
    let config = state.config();
    let (success_message, error_message) = match status {
        PendingRegistrationStatus::Approved => {
            ("Registration approved", "Failed to approve registration")
        }
        PendingRegistrationStatus::Rejected => {
            ("Registration rejected", "Failed to reject registration")
        }
    };
    let flash = match clean_string(form.user_id.as_deref().unwrap_or("")) {
        None => FlashData::error("User ID is required"),
        Some(user_id) => {
            let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
            match client.decide_pending_registration(&user_id, status).await {
                Ok(pending_registrations) => {
                    let flash = FlashData::success(success_message);
                    if htmx::targets(&headers, "pending-registration-list") {
                        let markup = maud::html! {
                            (templates::pages::pending_registrations::pending_registration_list(
                                config,
                                &csrf.0.0,
                                &pending_registrations,
                            ))
                            (pending_registration_count_markup(
                                u32::try_from(pending_registrations.len()).ok(),
                                true,
                            ))
                        };
                        let mut response = Html(markup.into_string()).into_response();
                        htmx::add_toast_header(&mut response, &flash);
                        return response;
                    }
                    flash
                }
                Err(error) => {
                    tracing::warn!(%error, ?status, "admin API request failed: decide pending registration");
                    FlashData::error(error_message)
                }
            }
        }
    };
    if htmx::is_htmx_request(&headers) {
        return htmx::toast_response(&flash);
    }
    flash::redirect_with_flash(
        &format!("{}/pending-registrations", config.base_path),
        flash,
        config.secure_cookies(),
    )
}
