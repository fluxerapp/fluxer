// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::client::AdminApiClient,
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
    extract::{FromRequest, Query, Request, State},
    response::{Html, IntoResponse, Redirect, Response},
    routing::get,
};
use serde::Deserialize;

#[derive(Deserialize)]
struct GiftCodesQuery {
    codes: Option<String>,
}

#[derive(Deserialize)]
struct GiftCodesForm {
    #[serde(default)]
    _csrf: Option<String>,
    #[serde(default)]
    count: Option<String>,
    #[serde(default)]
    duration_type: Option<String>,
    #[serde(default)]
    duration_quantity: Option<String>,
}

pub fn router() -> Router<AppState> {
    Router::new().route("/gift-codes", get(gift_codes_page).post(gift_codes_post))
}

async fn gift_codes_page(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    csrf: axum::Extension<CsrfToken>,
    Query(query): Query<GiftCodesQuery>,
) -> Response {
    let config = state.config();

    if config.self_hosted {
        return Redirect::to(&format!("{}/dashboard", config.base_path)).into_response();
    }

    let generated_codes: Option<Vec<String>> = query
        .codes
        .as_ref()
        .map(|c| c.split(',').map(|s| s.to_string()).collect());

    let markup = templates::pages::gift_codes::gift_codes_page(
        config,
        &auth.0,
        &csrf.0.0,
        generated_codes.as_deref(),
    );
    Html(markup.into_string()).into_response()
}

async fn gift_codes_post(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    request: Request,
) -> Response {
    let config = state.config();
    let base = &config.base_path;
    if config.self_hosted {
        return Redirect::to(&format!("{base}/dashboard")).into_response();
    }
    let form: GiftCodesForm = match Form::from_request(request, &state).await {
        Ok(Form(f)) => f,
        Err(error) => {
            tracing::warn!(%error, "failed to parse gift codes form");
            return flash::redirect_with_flash(
                &format!("{base}/gift-codes"),
                FlashData::error("Invalid form data"),
                config.secure_cookies(),
            );
        }
    };
    let count = form
        .count
        .as_deref()
        .and_then(|s| s.parse::<u32>().ok())
        .unwrap_or(1)
        .clamp(1, 100);
    let dur_type = form.duration_type.as_deref().unwrap_or("month");
    let dur_qty = form
        .duration_quantity
        .as_deref()
        .and_then(|s| s.parse::<u32>().ok())
        .unwrap_or(1);
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let secure_cookies = config.secure_cookies();
    match client.generate_gift_codes(count, dur_type, dur_qty).await {
        Ok(result) => {
            let codes = result.codes.join(",");
            flash::redirect_with_flash(
                &format!("{base}/gift-codes?codes={codes}"),
                FlashData::success(format!("{} gift code(s) generated", result.codes.len())),
                secure_cookies,
            )
        }
        Err(error) => {
            tracing::warn!(%error, "admin API request failed: generate gift codes");
            flash::redirect_with_flash(
                &format!("{base}/gift-codes"),
                FlashData::error("Failed to generate gift codes"),
                secure_cookies,
            )
        }
    }
}
