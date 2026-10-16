// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::client::AdminApiClient, middleware::auth::AuthContext, state::AppState,
    utils::user_tag::with_unique_usernames,
};
use axum::{
    extract::{Request, State},
    middleware::Next,
    response::Response,
};

pub async fn scope_account_identity(
    State(state): State<AppState>,
    request: Request,
    next: Next,
) -> Response {
    let Some(auth) = request.extensions().get::<AuthContext>() else {
        return next.run(request).await;
    };
    let client = AdminApiClient::new(state.http_client(), state.config(), &auth.session);
    let settings = state.account_identity_settings(&client).await;
    let unique_usernames = settings.mode.is_username() || settings.tag_style.is_none();
    with_unique_usernames(unique_usernames, next.run(request)).await
}
