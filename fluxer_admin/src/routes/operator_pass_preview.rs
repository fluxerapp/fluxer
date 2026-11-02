// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    acl,
    api::client::{AdminApiClient, ApiResultExt},
    middleware::{
        auth::AuthContext,
        csrf::CsrfToken,
        flash::{self, FlashData},
    },
    state::AppState,
    templates::{
        self,
        pages::operator_pass_preview::{PREVIEW_PATH, PreviewPageAccess},
    },
};
use axum::{
    Form, Router,
    extract::{Path, State},
    response::{Html, IntoResponse, Response},
    routing::{get, post},
};
use chrono::{DateTime, NaiveDateTime, Utc};
use serde::Deserialize;

#[derive(Deserialize)]
struct PreviewIpForm {
    #[serde(default)]
    _csrf: Option<String>,
    #[serde(default)]
    value: String,
    #[serde(default)]
    label: String,
    #[serde(default)]
    expires_at: String,
}

#[derive(Deserialize)]
struct ConfirmForm {
    #[serde(default)]
    _csrf: Option<String>,
    #[serde(default)]
    confirm: String,
}

#[derive(Deserialize)]
struct CsrfOnlyForm {
    #[serde(default)]
    _csrf: Option<String>,
}

pub fn router() -> Router<AppState> {
    Router::new()
        .route(PREVIEW_PATH, get(operator_pass_preview_page))
        .route("/operator-pass-preview/ips", post(add_preview_ip))
        .route(
            "/operator-pass-preview/ips/{entry_id}/remove",
            post(remove_preview_ip),
        )
        .route(
            "/operator-pass-preview/purchases/{purchase_id}/cancel",
            post(cancel_preview_purchase),
        )
        .route("/operator-pass-preview/reset", post(reset_preview_data))
}

fn has_acl(auth: &AuthContext, wanted: &str) -> bool {
    auth.admin_user
        .as_ref()
        .is_some_and(|admin| acl::has_permission(&admin.acls, wanted))
}

fn parse_expiry(value: &str) -> Result<Option<DateTime<Utc>>, ()> {
    let value = value.trim();
    if value.is_empty() {
        return Ok(None);
    }
    ["%Y-%m-%dT%H:%M", "%Y-%m-%dT%H:%M:%S"]
        .iter()
        .find_map(|format| NaiveDateTime::parse_from_str(value, format).ok())
        .map(|at| Some(at.and_utc()))
        .ok_or(())
}

async fn operator_pass_preview_page(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    csrf: axum::Extension<CsrfToken>,
    flash: Option<axum::Extension<FlashData>>,
) -> Response {
    let config = state.config();
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let access = PreviewPageAccess {
        can_manage: has_acl(&auth.0, acl::OPERATOR_PASS_PREVIEW),
        can_reset: has_acl(&auth.0, acl::OPERATOR_PASS_RESET),
    };
    let preview = if access.can_manage {
        client
            .get_operator_pass_preview()
            .await
            .log_error("load the Operator Pass preview entries")
    } else {
        None
    };
    let plan = if access.can_reset {
        client
            .get_operator_pass_reset_plan()
            .await
            .log_error("load the Operator Pass reset plan")
    } else {
        None
    };
    let flash = flash.map(|flash| flash.0.to_flash_message());
    let markup = templates::pages::operator_pass_preview::operator_pass_preview_page(
        config,
        &auth.0,
        &csrf.0.0,
        flash.as_ref(),
        &access,
        preview.as_ref(),
        plan.as_ref(),
    );
    Html(markup.into_string()).into_response()
}

async fn add_preview_ip(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    Form(form): Form<PreviewIpForm>,
) -> Response {
    let config = state.config();
    let back = format!("{}{PREVIEW_PATH}", config.base_path);
    let Ok(expires_at) = parse_expiry(&form.expires_at) else {
        return flash::redirect_error(config, &back, "Give the expiry as a date and time");
    };
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let flash = match client
        .add_operator_pass_preview_ip(form.value.trim(), form.label.trim(), expires_at)
        .await
    {
        Ok(entry) => FlashData::success(format!("Added {} as a preview entry", entry.value)),
        Err(error) => {
            tracing::warn!(%error, "admin API request failed: add an Operator Pass preview entry");
            FlashData::error(error.refusal_message("The entry was not added"))
        }
    };
    flash::redirect_with_flash(&back, flash, config.secure_cookies())
}

async fn remove_preview_ip(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    Path(entry_id): Path<String>,
    Form(_form): Form<CsrfOnlyForm>,
) -> Response {
    let config = state.config();
    let back = format!("{}{PREVIEW_PATH}", config.base_path);
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let flash = match client.remove_operator_pass_preview_ip(&entry_id).await {
        Ok(()) => FlashData::success("Removed the preview entry".to_owned()),
        Err(error) => {
            tracing::warn!(%error, "admin API request failed: remove an Operator Pass preview entry");
            FlashData::error(error.refusal_message("The entry was not removed"))
        }
    };
    flash::redirect_with_flash(&back, flash, config.secure_cookies())
}

async fn cancel_preview_purchase(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    Path(purchase_id): Path<String>,
    Form(_form): Form<CsrfOnlyForm>,
) -> Response {
    let config = state.config();
    let back = format!("{}{PREVIEW_PATH}", config.base_path);
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let flash = match client
        .cancel_operator_pass_preview_purchase(&purchase_id)
        .await
    {
        Ok(purchase) => FlashData::success(format!(
            "The preview purchase is now {}",
            purchase.status.label()
        )),
        Err(error) => {
            tracing::warn!(%error, "admin API request failed: cancel an Operator Pass preview purchase");
            FlashData::error(error.refusal_message("The preview purchase was not cancelled"))
        }
    };
    flash::redirect_with_flash(&back, flash, config.secure_cookies())
}

async fn reset_preview_data(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    Form(form): Form<ConfirmForm>,
) -> Response {
    let config = state.config();
    let back = format!("{}{PREVIEW_PATH}", config.base_path);
    if form.confirm.trim() != "RESET" {
        return flash::redirect_error(config, &back, "Type RESET to confirm the reset");
    }
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let flash = match client.reset_operator_pass_preview_data().await {
        Ok(result) => FlashData::success(format!(
            "Removed {} preview purchases with {} passes and {} certificate numbers. {} preview purchases remain.",
            result.removed.preview_purchases,
            result.removed.passes,
            result.removed.certificate_numbers,
            result.remaining.preview_purchases
        )),
        Err(error) => {
            tracing::warn!(%error, "admin API request failed: reset the Operator Pass preview data");
            FlashData::error(error.refusal_message("Nothing was reset"))
        }
    };
    flash::redirect_with_flash(&back, flash, config.secure_cookies())
}
