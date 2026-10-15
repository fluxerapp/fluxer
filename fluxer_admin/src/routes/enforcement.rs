// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    acl,
    api::{
        client::{AdminApiClient, ApiResultExt},
        enforcement::api_error_message,
        generated::types::EnforcementDecisionEndRequestOutcome,
    },
    middleware::{
        auth::AuthContext,
        csrf::CsrfToken,
        flash::{self, FlashData},
        htmx,
    },
    state::AppState,
    templates::pages::enforcement_decision,
    utils::forms::MultiValueForm,
};
use axum::{
    Router,
    extract::{Path, Query, Request, State},
    http::HeaderMap,
    response::{Html, IntoResponse, Redirect, Response},
    routing::get,
};
use serde::Deserialize;

#[derive(Deserialize)]
struct DecisionsQuery {
    id: Option<String>,
}

#[derive(Deserialize)]
struct DecisionActionQuery {
    action: Option<String>,
}

pub fn router() -> Router<AppState> {
    Router::new().route("/decisions", get(decisions)).route(
        "/decisions/{decision_id}",
        get(decision_detail).post(decision_post),
    )
}

fn is_decision_id(value: &str) -> bool {
    (1..=20).contains(&value.len()) && value.bytes().all(|byte| byte.is_ascii_digit())
}

async fn decisions(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    Query(query): Query<DecisionsQuery>,
) -> Response {
    let config = state.config();
    let lookup = query
        .id
        .as_deref()
        .map(str::trim)
        .filter(|id| !id.is_empty());
    if let Some(id) = lookup.filter(|id| is_decision_id(id)) {
        return Redirect::to(&format!("{}/decisions/{id}", config.base_path)).into_response();
    }
    let error = lookup.map(|_| "A decision ID is a number.");
    let admin_acls = auth
        .0
        .admin_user
        .as_ref()
        .map(|user| user.acls.as_slice())
        .unwrap_or_default();
    let review_queue = if acl::has_permission(admin_acls, acl::ENFORCEMENT_VIEW) {
        let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
        let result = client.list_review_queue().await;
        if let Err(error) = &result {
            tracing::warn!(%error, "admin API request failed: list the review queue");
        }
        Some(result)
    } else {
        None
    };
    let markup =
        enforcement_decision::decisions_page(config, &auth.0, error, review_queue.as_ref());
    Html(markup.into_string()).into_response()
}

async fn decision_detail(
    State(state): State<AppState>,
    headers: HeaderMap,
    auth: axum::Extension<AuthContext>,
    csrf: axum::Extension<CsrfToken>,
    Path(decision_id): Path<String>,
) -> Response {
    let config = state.config();
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let decision = if is_decision_id(&decision_id) {
        client
            .get_decision(&decision_id)
            .await
            .log_error("load enforcement decision")
    } else {
        None
    };
    let users = match &decision {
        Some(decision) => client
            .lookup_users_by_ids(&enforcement_decision::decision_user_ids(decision))
            .await
            .log_error("load users named by an enforcement decision")
            .unwrap_or_default(),
        None => Vec::new(),
    };
    let markup = enforcement_decision::enforcement_decision_page(
        config,
        &auth.0,
        &decision_id,
        decision.as_ref(),
        &users,
        &csrf.0.0,
        htmx::targets(&headers, "main-content"),
    );
    Html(markup.into_string()).into_response()
}

async fn decision_post(
    State(state): State<AppState>,
    headers: HeaderMap,
    auth: axum::Extension<AuthContext>,
    Path(decision_id): Path<String>,
    Query(query): Query<DecisionActionQuery>,
    request: Request,
) -> Response {
    let config = state.config();
    let redirect = format!("{}/decisions/{decision_id}", config.base_path);
    let flash = match MultiValueForm::from_request(request).await {
        Some(form) if is_decision_id(&decision_id) => {
            let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
            dispatch(&client, &decision_id, query.action.as_deref(), &form).await
        }
        Some(_) => FlashData::error("A decision ID is a number."),
        None => FlashData::error("Invalid form data"),
    };
    if htmx::is_htmx_request(&headers) && htmx::targets(&headers, "flash-container") {
        return htmx::toast_response(&flash);
    }
    flash::redirect_with_flash(&redirect, flash, config.secure_cookies())
}

fn failure(context: &str, error: &crate::api::client::ApiError) -> FlashData {
    tracing::warn!(%error, "admin API request failed: {context}");
    match api_error_message(error) {
        Some(message) => FlashData::error(format!("{context}: {message}")),
        None => FlashData::error(context.to_owned()),
    }
}

async fn dispatch(
    client: &AdminApiClient,
    decision_id: &str,
    action: Option<&str>,
    form: &MultiValueForm,
) -> FlashData {
    match action {
        Some("end") => {
            let Some(outcome) = form
                .first("outcome")
                .and_then(|value| EnforcementDecisionEndRequestOutcome::try_from(value).ok())
            else {
                return FlashData::error("Choose an outcome");
            };
            let Some(internal_note) = form.clean("internal_note") else {
                return FlashData::error("Add an internal note");
            };
            match client
                .end_decision(
                    decision_id,
                    outcome,
                    form.clean("statement"),
                    &internal_note,
                )
                .await
            {
                Ok(_) => FlashData::success("Decision ended"),
                Err(error) => failure("Failed to end the decision", &error),
            }
        }
        Some("add_note") => {
            let Some(body) = form.clean("body") else {
                return FlashData::error("Write a note first");
            };
            match client.annotate_decision(decision_id, body).await {
                Ok(_) => FlashData::success("Note added"),
                Err(error) => failure("Failed to add the note", &error),
            }
        }
        _ => FlashData::error("Unknown decision action"),
    }
}
