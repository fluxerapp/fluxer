// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::client::ApiResultExt,
    middleware::{auth::AuthContext, csrf::CsrfToken},
    state::AppState,
    templates,
};
use axum::{
    Router,
    extract::State,
    response::{Html, IntoResponse, Response},
    routing::get,
};

pub fn router() -> Router<AppState> {
    Router::new().route("/pending-users", get(user_approvals_page))
}

async fn user_approvals_page(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    csrf: axum::Extension<CsrfToken>,
    // registration: &InstanceRegistrationResponse
) -> Response {
    let config = state.config();
    let client =
        crate::api::client::AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let instance_config = client
        .get_instance_config()
        .await
        .log_error("load instance config");
    let markup = templates::pages::pending_users::pending_guilds_users_page(
        config,
        &auth.0,
        &csrf.0.0,
        instance_config.as_ref(),
    );
    Html(markup.into_string()).into_response()
}
