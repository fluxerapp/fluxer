// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    acl,
    api::{
        client::{AdminApiClient, ApiResultExt},
        generated::types::{
            AdminPaymentDisputeDecisionRequestDecision, AdminPaymentDisputeReviewStatusSchema,
        },
    },
    middleware::{
        auth::AuthContext,
        csrf::CsrfToken,
        flash::{self, FlashData},
    },
    state::AppState,
    templates,
};
use axum::{
    Form, Router,
    extract::{FromRequest, Path, Request, State},
    response::{Html, IntoResponse, Response},
    routing::{get, post},
};
use serde::Deserialize;

#[derive(Deserialize)]
struct DecisionForm {
    #[serde(default)]
    _csrf: Option<String>,
    decision: AdminPaymentDisputeDecisionRequestDecision,
    #[serde(default)]
    note: Option<String>,
}

#[derive(Deserialize)]
struct ReversalForm {
    #[serde(default)]
    _csrf: Option<String>,
    #[serde(default)]
    note: String,
}

pub fn router() -> Router<AppState> {
    Router::new()
        .route("/payment-disputes", get(payment_disputes_page))
        .route(
            "/payment-disputes/{dispute_id}/decision",
            post(payment_dispute_decision),
        )
        .route(
            "/payment-disputes/{dispute_id}/reversal",
            post(payment_dispute_reversal),
        )
}

async fn payment_disputes_page(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    csrf: axum::Extension<CsrfToken>,
) -> Response {
    let config = state.config();
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let reviews = client
        .list_payment_dispute_reviews()
        .await
        .log_error("load payment dispute reviews");
    let can_decide = auth
        .0
        .admin_user
        .as_ref()
        .is_some_and(|admin| acl::has_permission(&admin.acls, acl::USER_DELETE));
    let markup = templates::pages::payment_disputes::payment_disputes_page(
        config,
        &auth.0,
        &csrf.0.0,
        reviews.as_ref().map(|list| list.reviews.as_slice()),
        can_decide,
    );
    Html(markup.into_string()).into_response()
}

async fn payment_dispute_decision(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    Path(dispute_id): Path<String>,
    request: Request,
) -> Response {
    let config = state.config();
    let back = format!("{}/payment-disputes", config.base_path);
    let form: DecisionForm = match Form::from_request(request, &state).await {
        Ok(Form(form)) => form,
        Err(error) => {
            tracing::warn!(%error, "failed to parse payment dispute decision form");
            return flash::redirect_error(config, &back, "Invalid form data");
        }
    };
    let note = form
        .note
        .as_deref()
        .map(str::trim)
        .filter(|note| !note.is_empty());
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let flash = match client
        .decide_payment_dispute_review(&dispute_id, form.decision, note)
        .await
    {
        Ok(review)
            if review.status == AdminPaymentDisputeReviewStatusSchema::Unjustified
                && review.warned_at.is_none() =>
        {
            FlashData::error(format!(
                "{dispute_id} is unjustified but the warning is incomplete. Send the missing warning again."
            ))
        }
        Ok(_) => FlashData::success(format!("Decision recorded for {dispute_id}")),
        Err(error) => {
            tracing::warn!(%error, dispute_id, "admin API request failed: decide payment dispute");
            FlashData::error(error.refusal_message("Failed to record the decision"))
        }
    };
    flash::redirect_with_flash(&back, flash, config.secure_cookies())
}

async fn payment_dispute_reversal(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    Path(dispute_id): Path<String>,
    request: Request,
) -> Response {
    let config = state.config();
    let back = format!("{}/payment-disputes", config.base_path);
    let form: ReversalForm = match Form::from_request(request, &state).await {
        Ok(Form(form)) => form,
        Err(error) => {
            tracing::warn!(%error, "failed to parse payment dispute reversal form");
            return flash::redirect_error(config, &back, "Invalid form data");
        }
    };
    let note = form.note.trim();
    if note.is_empty() {
        return flash::redirect_error(config, &back, "Say what the appeal showed before reversing");
    }
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let flash = match client
        .reverse_payment_dispute_review(&dispute_id, note)
        .await
    {
        Ok(_) => FlashData::success(format!("{dispute_id} reversed to justified")),
        Err(error) => {
            tracing::warn!(%error, dispute_id, "admin API request failed: reverse payment dispute");
            FlashData::error(error.refusal_message("Failed to reverse the decision"))
        }
    };
    flash::redirect_with_flash(&back, flash, config.secure_cookies())
}
