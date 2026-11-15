// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    middleware::auth::AuthContext, templates::components::media::with_neutral_default_avatars,
};
use axum::{extract::Request, middleware::Next, response::Response};

pub async fn scope_default_avatars(request: Request, next: Next) -> Response {
    let neutral = request
        .extensions()
        .get::<AuthContext>()
        .and_then(|auth| auth.branding.as_ref())
        .is_some_and(|branding| branding.neutral_default_avatars);
    with_neutral_default_avatars(neutral, next.run(request)).await
}
