// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::types::{AdminUser, AdminUserMeResponse, PremiumBranding},
    middleware::flash,
    session::{self, Session},
    state::AppState,
};
use axum::{
    extract::{Request, State},
    middleware::Next,
    response::{IntoResponse, Redirect, Response},
};

#[derive(Clone, Debug)]
pub struct AuthContext {
    pub session: Session,
    pub admin_user: Option<AdminUser>,
    pub pending_registration_count: Option<u32>,
    pub branding: Option<PremiumBranding>,
}

pub async fn require_auth(
    State(state): State<AppState>,
    mut request: Request,
    next: Next,
) -> Response {
    let config = state.config();
    let session = extract_session(&request, &config.secret_key_base);
    let Some(session) = session else {
        return Redirect::to(&format!("{}/auth/start", config.base_path)).into_response();
    };
    let (admin_user, pending_registration_count) =
        match fetch_admin_user(state.http_client(), config, &session).await {
            AdminFetchResult::Unauthorized => {
                let login_url = format!("{}/login", config.base_path);
                return clear_session_and_redirect(&login_url, config);
            }
            AdminFetchResult::Ok(me) => (Some(me.user), me.pending_registration_count),
            AdminFetchResult::None => (None, None),
        };
    request.extensions_mut().insert(AuthContext {
        session,
        admin_user,
        pending_registration_count,
        branding: state.self_hosted_branding().await,
    });

    let had_flash = flash::extract_flash(&request);
    if let Some(ref fd) = had_flash {
        request.extensions_mut().insert(fd.clone());
    }

    let mut response = next.run(request).await;

    if had_flash.is_some() {
        flash::clear_flash_cookie(&mut response);
    }

    response
}

fn clear_session_and_redirect(url: &str, config: &crate::config::AdminConfig) -> Response {
    let mut response = Redirect::to(url).into_response();
    session::append_cookies(
        &mut response,
        &session::clear_cookie_values(session::SESSION_COOKIE_NAME, config),
    );
    response
}

fn extract_session(request: &Request, secret_key: &str) -> Option<Session> {
    let cookie_header = request
        .headers()
        .get(axum::http::header::COOKIE)?
        .to_str()
        .ok()?;
    for pair in cookie_header.split(';') {
        let pair = pair.trim();
        let prefix = format!("{}=", crate::session::SESSION_COOKIE_NAME);
        if let Some(value) = pair.strip_prefix(&prefix) {
            return session::parse_session(value.trim(), secret_key);
        }
    }
    None
}

enum AdminFetchResult {
    Ok(Box<AdminUserMeResponse>),
    Unauthorized,
    None,
}

async fn fetch_admin_user(
    http_client: &reqwest::Client,
    config: &crate::config::AdminConfig,
    session: &Session,
) -> AdminFetchResult {
    let url = format!("{}/admin/users/@me", config.api_endpoint);
    let response =
        match crate::api::client::with_proxy_client_ip_header(http_client.get(&url), config)
            .header("Authorization", format!("Bearer {}", session.access_token))
            .send()
            .await
        {
            Ok(r) => r,
            Err(_) => return AdminFetchResult::None,
        };
    if response.status() == reqwest::StatusCode::UNAUTHORIZED {
        return AdminFetchResult::Unauthorized;
    }
    if !response.status().is_success() {
        return AdminFetchResult::None;
    }
    match response.json::<AdminUserMeResponse>().await {
        Ok(me) => AdminFetchResult::Ok(Box::new(me)),
        Err(_) => AdminFetchResult::None,
    }
}
