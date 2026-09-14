// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::types::AdminUser,
    middleware::flash::{self, FlashData},
    session::{self, Session},
    state::AppState,
};
use axum::{
    extract::{Request, State},
    http::StatusCode,
    middleware::Next,
    response::{IntoResponse, Redirect, Response},
};

#[derive(Clone, Debug)]
pub struct AuthContext {
    pub session: Session,
    pub admin_user: Option<AdminUser>,
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
    let admin_result = fetch_admin_user(state.http_client(), config, &session).await;
    match admin_result {
        AdminFetchResult::Unauthorized => {
            let login_url = format!("{}/login", config.base_path);
            return clear_session_and_redirect(&login_url, config);
        }
        AdminFetchResult::Ok(admin_user) => {
            let auth_context = AuthContext {
                session,
                admin_user: Some(*admin_user),
            };
            request.extensions_mut().insert(auth_context);
        }
        AdminFetchResult::None => {
            let auth_context = AuthContext {
                session,
                admin_user: None,
            };
            request.extensions_mut().insert(auth_context);
        }
    }

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

pub fn get_flash(request: &Request) -> Option<FlashData> {
    request.extensions().get::<FlashData>().cloned()
}

fn admin_cookie_path(config: &crate::config::AdminConfig) -> &str {
    if config.base_path.is_empty() {
        "/"
    } else {
        config.base_path.as_str()
    }
}

fn clear_session_and_redirect(url: &str, config: &crate::config::AdminConfig) -> Response {
    let mut response = Redirect::to(url).into_response();
    let admin_path = admin_cookie_path(config);
    for path in [admin_path, "/"] {
        if path != admin_path && admin_path == "/" {
            continue;
        }
        let clear = format!(
            "{}=; Path={}; HttpOnly; SameSite=Lax; Max-Age=0",
            crate::session::SESSION_COOKIE_NAME,
            path
        );
        if let Ok(v) = axum::http::HeaderValue::from_str(&clear) {
            response
                .headers_mut()
                .append(axum::http::header::SET_COOKIE, v);
        }
        let legacy_clear = format!(
            "{}=; Path={}; HttpOnly; SameSite=Lax; Max-Age=0",
            crate::session::LEGACY_SESSION_COOKIE_NAME,
            path
        );
        if let Ok(v) = axum::http::HeaderValue::from_str(&legacy_clear) {
            response
                .headers_mut()
                .append(axum::http::header::SET_COOKIE, v);
        }
    }
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
    Ok(Box<AdminUser>),
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
    match response
        .json::<crate::api::types::AdminUserMeResponse>()
        .await
    {
        Ok(resp) => AdminFetchResult::Ok(Box::new(resp.user)),
        Err(_) => AdminFetchResult::None,
    }
}

pub fn get_auth_context(request: &Request) -> Option<&AuthContext> {
    request.extensions().get::<AuthContext>()
}

pub fn require_auth_context(request: &Request) -> Result<&AuthContext, Box<Response>> {
    get_auth_context(request).ok_or_else(|| Box::new(StatusCode::UNAUTHORIZED.into_response()))
}
