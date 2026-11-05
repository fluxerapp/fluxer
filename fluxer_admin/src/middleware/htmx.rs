// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    middleware::flash::{self, FlashData},
    templates::components::detail_tabs::detail_page_href,
};
use axum::{
    extract::Request,
    http::{HeaderMap, HeaderName, HeaderValue, Method, StatusCode, header},
    middleware::Next,
    response::{IntoResponse, Redirect, Response},
};

const HX_RESWAP: HeaderName = HeaderName::from_static("hx-reswap");
const ADMIN_TOAST: HeaderName = HeaderName::from_static("x-fluxer-admin-toast");
const HX_REDIRECT: HeaderName = HeaderName::from_static("hx-redirect");

pub fn is_htmx_request(headers: &HeaderMap) -> bool {
    headers
        .get("HX-Request")
        .and_then(|value| value.to_str().ok())
        .is_some_and(|value| value == "true")
}

pub fn is_history_restore(headers: &HeaderMap) -> bool {
    headers
        .get("HX-History-Restore-Request")
        .and_then(|value| value.to_str().ok())
        .is_some_and(|value| value == "true")
}

pub fn tab_page_redirect(
    headers: &HeaderMap,
    query: Option<&str>,
    base_url: &str,
    entity_id: &str,
    tab: &str,
) -> Option<Response> {
    if is_htmx_request(headers) && !is_history_restore(headers) {
        return None;
    }
    let plain = |value: &str| {
        !value.is_empty()
            && value
                .bytes()
                .all(|b| b.is_ascii_alphanumeric() || b == b'_' || b == b'-')
    };
    if !plain(entity_id) {
        return None;
    }
    let tab = if plain(tab) { tab } else { "overview" };
    let mut href = detail_page_href(base_url, entity_id, tab);
    let extra: Vec<&str> = query
        .unwrap_or("")
        .split('&')
        .filter(|pair| !pair.is_empty() && pair.split('=').next().is_some_and(|key| key != "tab"))
        .collect();
    if !extra.is_empty() {
        href.push(if href.contains('?') { '&' } else { '?' });
        href.push_str(&extra.join("&"));
    }
    Some(Redirect::to(&href).into_response())
}

pub fn htmx_target(headers: &HeaderMap) -> Option<String> {
    headers
        .get("HX-Target")
        .and_then(|value| value.to_str().ok())
        .map(|s| s.to_owned())
}

pub fn targets(headers: &HeaderMap, target_id: &str) -> bool {
    htmx_target(headers).is_some_and(|target| target == target_id)
}

pub fn toast_response(flash: &FlashData) -> Response {
    let mut response = StatusCode::NO_CONTENT.into_response();
    response
        .headers_mut()
        .insert(HX_RESWAP, HeaderValue::from_static("none"));
    add_toast_header(&mut response, flash);
    response
}

pub fn navigate_with_flash(url: &str, flash: &FlashData, secure: bool) -> Response {
    let mut response = StatusCode::NO_CONTENT.into_response();
    if let Ok(value) = HeaderValue::from_str(url) {
        response.headers_mut().insert(HX_REDIRECT, value);
    }
    add_toast_header(&mut response, flash);
    flash::set_flash_cookie(&mut response, flash, secure);
    response
}

pub fn add_toast_header(response: &mut Response, flash: &FlashData) {
    let mut payload = serde_json::json!({"level": flash.flash_type});
    if flash
        .field_error
        .as_ref()
        .is_none_or(|field_error| field_error.message != flash.message)
    {
        payload["message"] = flash.message.clone().into();
    }
    if let Some(field_error) = &flash.field_error {
        payload["field"] = field_error.field.clone().into();
        payload["field_message"] = field_error.message.clone().into();
    }
    if let Ok(value) = HeaderValue::from_str(&ascii_json(&payload)) {
        response.headers_mut().insert(ADMIN_TOAST, value);
    }
}

fn ascii_json(value: &serde_json::Value) -> String {
    let mut out = String::new();
    for c in value.to_string().chars() {
        if c.is_ascii() {
            out.push(c);
        } else {
            for unit in c.encode_utf16(&mut [0; 2]) {
                out.push_str(&format!("\\u{unit:04x}"));
            }
        }
    }
    out
}

pub async fn flash_redirect_to_toast(request: Request, next: Next) -> Response {
    let should_convert = is_htmx_request(request.headers())
        && targets(request.headers(), "flash-container")
        && matches!(
            request.method(),
            &Method::POST | &Method::PUT | &Method::PATCH | &Method::DELETE
        );
    let response = next.run(request).await;
    if !should_convert || !response.status().is_redirection() {
        return response;
    }
    match flash_from_set_cookie(response.headers()) {
        Some(flash) => toast_response(&flash),
        None => response,
    }
}

fn flash_from_set_cookie(headers: &HeaderMap) -> Option<FlashData> {
    headers
        .get_all(header::SET_COOKIE)
        .iter()
        .filter_map(|value| value.to_str().ok())
        .find_map(|value| {
            value
                .split(';')
                .next()
                .and_then(|pair| pair.trim().strip_prefix("flash="))
                .filter(|encoded| !encoded.is_empty())
                .and_then(flash::parse_flash)
        })
}
