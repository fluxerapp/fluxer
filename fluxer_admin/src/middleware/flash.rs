// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::api::types::{FlashLevel, FlashMessage};
use axum::{
    extract::Request,
    http::{HeaderValue, header},
    response::{IntoResponse, Redirect, Response},
};
use base64::Engine;
use serde::{Deserialize, Serialize};

const FLASH_COOKIE_NAME: &str = "flash";

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct FlashData {
    pub message: String,
    pub flash_type: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub detail: Option<String>,
}

impl FlashData {
    pub fn success(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            flash_type: "success".to_owned(),
            detail: None,
        }
    }

    pub fn error(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            flash_type: "error".to_owned(),
            detail: None,
        }
    }

    pub fn info(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            flash_type: "info".to_owned(),
            detail: None,
        }
    }

    pub fn to_flash_message(&self) -> FlashMessage {
        FlashMessage {
            level: match self.flash_type.as_str() {
                "success" => FlashLevel::Success,
                "error" => FlashLevel::Error,
                _ => FlashLevel::Info,
            },
            message: self.message.clone(),
        }
    }
}

pub fn serialize_flash(flash: &FlashData) -> String {
    let json = serde_json::to_string(flash).unwrap_or_default();
    base64::engine::general_purpose::URL_SAFE_NO_PAD.encode(json.as_bytes())
}

pub fn parse_flash(cookie: &str) -> Option<FlashData> {
    let bytes = base64::engine::general_purpose::URL_SAFE_NO_PAD
        .decode(cookie.trim())
        .ok()?;
    serde_json::from_slice(&bytes).ok()
}

pub fn extract_flash(request: &Request) -> Option<FlashData> {
    let cookie_header = request.headers().get(header::COOKIE)?.to_str().ok()?;
    for pair in cookie_header.split(';') {
        let pair = pair.trim();
        if let Some(value) = pair.strip_prefix("flash=") {
            let trimmed = value.trim();
            if !trimmed.is_empty() {
                return parse_flash(trimmed);
            }
        }
    }
    None
}

fn delete_flash_cookie() -> String {
    format!("{FLASH_COOKIE_NAME}=; Path=/; HttpOnly; SameSite=Lax; Max-Age=0")
}

pub fn clear_flash_cookie(response: &mut Response) {
    if let Ok(v) = HeaderValue::from_str(&delete_flash_cookie()) {
        response.headers_mut().append(header::SET_COOKIE, v);
    }
}

pub fn redirect_with_flash(url: &str, flash: FlashData, secure: bool) -> Response {
    let mut response = Redirect::to(url).into_response();
    set_flash_cookie(&mut response, &flash, secure);
    response
}

pub fn set_flash_cookie(response: &mut Response, flash: &FlashData, secure: bool) {
    let encoded = serialize_flash(flash);
    let secure_flag = if secure { "; Secure" } else { "" };
    let cookie_value = format!(
        "{FLASH_COOKIE_NAME}={encoded}; Path=/; HttpOnly; SameSite=Lax; Max-Age=60{secure_flag}"
    );
    if let Ok(v) = HeaderValue::from_str(&cookie_value) {
        response.headers_mut().append(header::SET_COOKIE, v);
    }
}
