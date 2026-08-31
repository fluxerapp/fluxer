// SPDX-License-Identifier: AGPL-3.0-or-later

use axum::{
    body::{Body, to_bytes},
    extract::{Request, State},
    http::{HeaderValue, Method, StatusCode, header},
    middleware::Next,
    response::{IntoResponse, Response},
};

use crate::middleware::auth::AuthContext;
use crate::session::{create_csrf_token, verify_csrf_token};
use crate::state::AppState;

const CSRF_COOKIE_NAME: &str = "csrf_token";
pub const CSRF_FORM_FIELD: &str = "_csrf";
const CSRF_HEADER_NAME: &str = "x-csrf-token";
const HOST_CSRF_COOKIE_NAME: &str = "__Host-csrf_token";
const MAX_CSRF_FORM_BYTES: usize = 8 * 1024 * 1024;

const IGNORED_PATH_SUFFIXES: &[&str] = &["/oauth2_callback", "/auth/start"];

pub async fn csrf_protection(
    State(state): State<AppState>,
    mut request: Request,
    next: Next,
) -> Response {
    let config = state.config();
    let secret = config.secret_key_base.clone();
    let admin_endpoint = config.admin_endpoint.clone();
    let is_production = config.is_production();

    let user_id = request
        .extensions()
        .get::<AuthContext>()
        .map(|ctx| ctx.session.user_id.clone())
        .unwrap_or_default();

    let token = extract_csrf_cookie(&request)
        .filter(|cookie| verify_csrf_token(cookie, &user_id, &secret))
        .unwrap_or_else(|| create_csrf_token(&user_id, &secret));
    request.extensions_mut().insert(CsrfToken(token.clone()));

    if matches!(
        *request.method(),
        Method::POST | Method::PATCH | Method::DELETE | Method::PUT
    ) {
        let path = request.uri().path().to_owned();
        let is_ignored = IGNORED_PATH_SUFFIXES
            .iter()
            .any(|suffix| path.ends_with(suffix));
        if !is_ignored {
            if !is_same_site_request(&request, &admin_endpoint) {
                return StatusCode::FORBIDDEN.into_response();
            }
            let header_token = extract_csrf_header(&request);
            let query_token = extract_csrf_from_query(&request);
            let mut submitted = query_token.or(header_token);
            if submitted.is_none() && is_urlencoded_form(&request) {
                let (restored_request, body_token) =
                    match extract_csrf_from_form_body(request).await {
                        Ok(result) => result,
                        Err(response) => return response,
                    };
                request = restored_request;
                submitted = body_token;
            }
            let accepted = submitted.as_deref().is_some_and(|submitted_token| {
                submitted_token == token && verify_csrf_token(submitted_token, &user_id, &secret)
            });
            if !accepted {
                return StatusCode::FORBIDDEN.into_response();
            }
        }
    }

    let mut response = next.run(request).await;

    let cookie_name = if is_production {
        HOST_CSRF_COOKIE_NAME
    } else {
        CSRF_COOKIE_NAME
    };
    let secure = if is_production { "; Secure" } else { "" };
    let cookie_value = format!("{cookie_name}={token}; Path=/; SameSite=Lax; HttpOnly{secure}");
    if let Ok(value) = HeaderValue::from_str(&cookie_value) {
        response.headers_mut().append(header::SET_COOKIE, value);
    }
    if is_production
        && let Ok(value) = HeaderValue::from_str(&format!(
            "{CSRF_COOKIE_NAME}=; Path=/; SameSite=Lax; HttpOnly; Max-Age=0"
        ))
    {
        response.headers_mut().append(header::SET_COOKIE, value);
    }

    response
}

fn extract_csrf_cookie(request: &Request) -> Option<String> {
    let cookie_header = request.headers().get(header::COOKIE)?.to_str().ok()?;
    let mut legacy = None;
    for pair in cookie_header.split(';') {
        let pair = pair.trim();
        if let Some(value) = pair.strip_prefix("__Host-csrf_token=") {
            let trimmed = value.trim();
            if !trimmed.is_empty() {
                return Some(trimmed.to_owned());
            }
        } else if let Some(value) = pair.strip_prefix("csrf_token=")
            && legacy.is_none()
        {
            let trimmed = value.trim();
            if !trimmed.is_empty() {
                legacy = Some(trimmed.to_owned());
            }
        }
    }
    legacy
}

fn extract_csrf_header(request: &Request) -> Option<String> {
    request
        .headers()
        .get(CSRF_HEADER_NAME)
        .and_then(|v| v.to_str().ok())
        .map(|s| s.to_owned())
}

fn extract_csrf_from_query(request: &Request) -> Option<String> {
    let uri = request.uri();
    let query = uri.query()?;
    for pair in query.split('&') {
        if let Some(value) = pair.strip_prefix("_csrf=") {
            return Some(urlencoding::decode(value).ok()?.into_owned());
        }
    }
    None
}

fn is_urlencoded_form(request: &Request) -> bool {
    request
        .headers()
        .get(header::CONTENT_TYPE)
        .and_then(|value| value.to_str().ok())
        .is_some_and(|value| {
            value.split(';').next().is_some_and(|media_type| {
                media_type
                    .trim()
                    .eq_ignore_ascii_case("application/x-www-form-urlencoded")
            })
        })
}

#[allow(clippy::result_large_err)]
async fn extract_csrf_from_form_body(
    request: Request,
) -> Result<(Request, Option<String>), Response> {
    let (parts, body) = request.into_parts();
    let body_bytes = match to_bytes(body, MAX_CSRF_FORM_BYTES).await {
        Ok(bytes) => bytes,
        Err(_) => return Err(StatusCode::PAYLOAD_TOO_LARGE.into_response()),
    };
    let token = url::form_urlencoded::parse(body_bytes.as_ref())
        .find_map(|(name, value)| (name == CSRF_FORM_FIELD).then(|| value.into_owned()));
    let request = Request::from_parts(parts, Body::from(body_bytes));
    Ok((request, token))
}

fn is_same_site_request(request: &Request, admin_endpoint: &str) -> bool {
    if let Some(site) = request
        .headers()
        .get("sec-fetch-site")
        .and_then(|value| value.to_str().ok())
    {
        return matches!(site, "same-origin" | "same-site" | "none");
    }
    match request
        .headers()
        .get(header::ORIGIN)
        .and_then(|value| value.to_str().ok())
    {
        Some(origin) => origin == admin_endpoint,
        None => true,
    }
}

#[derive(Clone, Debug)]
pub struct CsrfToken(pub String);

pub fn get_csrf_token(request: &Request) -> String {
    request
        .extensions()
        .get::<CsrfToken>()
        .map(|t| t.0.clone())
        .unwrap_or_default()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn oauth2_callback_is_exempt() {
        let exempt = IGNORED_PATH_SUFFIXES
            .iter()
            .any(|s| "/admin/oauth2_callback".ends_with(s));
        assert!(exempt, "oauth2_callback must be exempt");
    }

    #[test]
    fn auth_start_is_exempt() {
        let exempt = IGNORED_PATH_SUFFIXES
            .iter()
            .any(|s| "/admin/auth/start".ends_with(s));
        assert!(exempt, "auth/start must be exempt");
    }

    #[test]
    fn normal_path_not_exempt() {
        let exempt = IGNORED_PATH_SUFFIXES
            .iter()
            .any(|s| "/admin/users".ends_with(s));
        assert!(!exempt, "normal path must not be exempt");
    }
}
