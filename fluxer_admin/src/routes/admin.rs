// SPDX-License-Identifier: AGPL-3.0-or-later

use super::ActionQuery;
use crate::{
    acl,
    api::client::{AdminApiClient, ApiResultExt},
    config::AdminConfig,
    middleware::{
        auth::AuthContext,
        csrf::CsrfToken,
        flash::{self, FlashData},
        htmx,
    },
    state::AppState,
    templates,
    utils::forms::MultiValueForm,
};
use axum::{
    Router,
    extract::{Query, Request, State},
    response::{Html, IntoResponse, Response},
    routing::get,
};

pub fn router() -> Router<AppState> {
    Router::new().route(
        "/admin-api-keys",
        get(admin_api_keys_page).post(admin_api_keys_post),
    )
}

async fn admin_api_keys_page(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    csrf: axum::Extension<CsrfToken>,
    flash: Option<axum::Extension<FlashData>>,
) -> Response {
    let config = state.config();
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let keys = client
        .list_api_keys()
        .await
        .log_error("load admin api keys");
    let available_acls = available_acls(&auth.0);
    let flash = flash.map(|flash| flash.0.to_flash_message());
    let markup = templates::pages::admin_api_keys::admin_api_keys_page(
        config,
        &auth.0,
        &csrf.0.0,
        flash.as_ref(),
        None,
        keys.as_deref(),
        &available_acls,
    );
    Html(markup.into_string()).into_response()
}

async fn admin_api_keys_post(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    csrf: axum::Extension<CsrfToken>,
    Query(aq): Query<ActionQuery>,
    request: Request,
) -> Response {
    let config = state.config();
    let base = &config.base_path;
    let is_htmx = htmx::is_htmx_request(request.headers());
    let form = match MultiValueForm::from_request(request).await {
        Some(form) => form,
        None => {
            return admin_api_key_flash_response(
                config,
                FlashData::error("Invalid form data"),
                is_htmx,
            );
        }
    };
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let action = aq.action.as_deref().unwrap_or("");
    match action {
        "create" => {
            let name = form.clean("name").unwrap_or_default();
            let acls = form.list_values_any(&["acls[]", "acls"]);
            return match client.create_api_key(&name, &acls).await {
                Ok(created) => {
                    let keys = client
                        .list_api_keys()
                        .await
                        .log_error("reload admin api keys after create");
                    let available_acls = available_acls(&auth.0);
                    let markup = templates::pages::admin_api_keys::admin_api_keys_page(
                        config,
                        &auth.0,
                        &csrf.0.0,
                        None,
                        Some(&created),
                        keys.as_deref(),
                        &available_acls,
                    );
                    Html(markup.into_string()).into_response()
                }
                Err(error) => {
                    tracing::warn!(%error, "admin API request failed: create API key");
                    admin_api_key_flash_response(
                        config,
                        FlashData::error("Failed to create API key."),
                        is_htmx,
                    )
                }
            };
        }
        "revoke" => {
            if let Some(key_id) = form.clean("key_id") {
                let result = client.revoke_api_key(&key_id).await;
                let flash = match result.log_error("revoke API key") {
                    Some(_) => FlashData::success("API key revoked."),
                    None => FlashData::error("Failed to revoke API key"),
                };
                return flash::redirect_with_flash(
                    &format!("{base}/admin-api-keys"),
                    flash,
                    config.is_production(),
                );
            }
        }
        _ => {}
    }
    flash::redirect_with_flash(
        &format!("{base}/admin-api-keys"),
        FlashData::success(format!("API key action '{action}' completed.")),
        config.is_production(),
    )
}

fn admin_api_key_flash_response(
    config: &AdminConfig,
    flash_data: FlashData,
    is_htmx: bool,
) -> Response {
    if is_htmx {
        htmx::toast_response(&flash_data)
    } else {
        flash::redirect_with_flash(
            &format!("{}/admin-api-keys", config.base_path),
            flash_data,
            config.is_production(),
        )
    }
}

fn available_acls(auth: &AuthContext) -> Vec<&'static str> {
    let admin_acls = auth
        .admin_user
        .as_ref()
        .map(|user| user.acls.as_slice())
        .unwrap_or(&[]);
    acl::ALL_ACLS
        .iter()
        .copied()
        .filter(|item| acl::has_permission(admin_acls, item))
        .collect()
}
