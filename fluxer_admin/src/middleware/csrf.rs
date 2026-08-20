// SPDX-License-Identifier: AGPL-3.0-or-later

use axum::{
    body::{Body, to_bytes},
    extract::Request,
    http::{HeaderValue, Method, StatusCode, header},
    middleware::Next,
    response::{IntoResponse, Response},
};
use rand::RngExt;

const CSRF_COOKIE_NAME: &str = "csrf_token";
pub const CSRF_FORM_FIELD: &str = "_csrf";
const CSRF_HEADER_NAME: &str = "x-csrf-token";
const TOKEN_LENGTH: usize = 32;
const MAX_CSRF_FORM_BYTES: usize = 8 * 1024 * 1024;

const IGNORED_PATH_SUFFIXES: &[&str] = &["/oauth2_callback", "/auth/start"];

pub async fn csrf_protection(mut request: Request, next: Next) -> Response {
    let existing_token = extract_csrf_cookie(&request);
    let token = existing_token.unwrap_or_else(generate_csrf_token);
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
            match submitted {
                Some(ref submitted_token) if submitted_token == &token => {}
                _ => {
                    return StatusCode::FORBIDDEN.into_response();
                }
            }
        }
    }

    let mut response = next.run(request).await;

    let cookie_value = format!(
        "{}={}; Path=/; SameSite=Lax; HttpOnly",
        CSRF_COOKIE_NAME, token
    );
    if let Ok(value) = HeaderValue::from_str(&cookie_value) {
        response.headers_mut().append(header::SET_COOKIE, value);
    }

    response
}

fn extract_csrf_cookie(request: &Request) -> Option<String> {
    let cookie_header = request.headers().get(header::COOKIE)?.to_str().ok()?;
    for pair in cookie_header.split(';') {
        let pair = pair.trim();
        if let Some(value) = pair.strip_prefix("csrf_token=") {
            let trimmed = value.trim();
            if !trimmed.is_empty() {
                return Some(trimmed.to_owned());
            }
        }
    }
    None
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

fn generate_csrf_token() -> String {
    let mut rng = rand::rng();
    let bytes: [u8; TOKEN_LENGTH] = rng.random();
    hex_encode(&bytes)
}

fn hex_encode(bytes: &[u8]) -> String {
    let mut s = String::with_capacity(bytes.len() * 2);
    for byte in bytes {
        s.push_str(&format!("{byte:02x}"));
    }
    s
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
    fn generate_csrf_token_correct_length() {
        let token = generate_csrf_token();
        assert_eq!(
            token.len(),
            TOKEN_LENGTH * 2,
            "token must be {} hex chars",
            TOKEN_LENGTH * 2
        );
    }

    #[test]
    fn generate_csrf_token_is_valid_hex() {
        let token = generate_csrf_token();
        assert!(
            token.chars().all(|c| c.is_ascii_hexdigit()),
            "token must contain only hex chars: {token}"
        );
    }

    #[test]
    fn generate_csrf_token_is_unique() {
        let a = generate_csrf_token();
        let b = generate_csrf_token();
        assert_ne!(a, b, "consecutive tokens must differ");
    }

    #[test]
    fn hex_encode_produces_correct_output() {
        assert_eq!(hex_encode(&[0x00, 0xff, 0x0a]), "00ff0a");
        assert_eq!(hex_encode(&[]), "");
        assert_eq!(hex_encode(&[0xde, 0xad]), "dead");
    }

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
