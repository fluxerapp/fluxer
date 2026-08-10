// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    config::{MarketingConfig, ReleaseChannel},
    i18n::{Locale, MarketingI18n},
};
use axum::{
    extract::State,
    http::{HeaderMap, Uri, header},
};
use base64::{Engine as _, engine::general_purpose::URL_SAFE_NO_PAD};
use cookie::Cookie;
use fluxer_common::geoip::GeoipResolver;
use hmac::{Hmac, KeyInit, Mac};
use serde::{Deserialize, Serialize};
use sha2::Sha256;
use std::{
    sync::Arc,
    time::{SystemTime, UNIX_EPOCH},
};

type HmacSha256 = Hmac<Sha256>;

const CANARY_API_ENDPOINT: &str = "https://api.canary.fluxer.app";
pub const CANARY_WEB_APP_ENDPOINT: &str = "https://web.canary.fluxer.app";
const LOCALE_COOKIE_MAX_AGE_SECONDS: u64 = 60 * 60 * 24 * 365;
const STABLE_API_ENDPOINT: &str = "https://api.fluxer.app";

#[derive(Clone)]
pub struct AppState {
    pub config: Arc<MarketingConfig>,
    pub geoip: Arc<GeoipResolver>,
    pub http_client: reqwest::Client,
    pub i18n: Arc<MarketingI18n>,
    pub swish_qr_cache: crate::swish::SwishQrCache,
    pub latest_versions_cache: crate::downloads::LatestVersionsCache,
    pub donation_rate_limiter: crate::rate_limit::RateLimiter,
}

#[derive(Clone, Debug)]
pub struct RequestContext {
    pub locale: Locale,
    pub current_path: String,
    pub base_path: String,
    pub base_url: String,
    pub api_endpoint: String,
    pub static_cdn_endpoint: String,
    pub asset_version: String,
    pub country_code: String,
    pub release_channel: ReleaseChannel,
    pub platform: Platform,
    pub architecture: Architecture,
    pub test_build: bool,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Platform {
    Windows,
    Macos,
    Linux,
    Ios,
    Android,
    Unknown,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Architecture {
    X64,
    Arm64,
    Unknown,
}

#[derive(Deserialize, Serialize)]
struct LocaleCookie {
    locale: String,
    #[serde(rename = "createdAt", default)]
    created_at: Option<u64>,
}

impl RequestContext {
    pub fn from_headers(state: &AppState, headers: &HeaderMap, uri: &Uri) -> Self {
        let locale = resolve_request_locale(state, headers);
        let user_agent = headers
            .get(header::USER_AGENT)
            .and_then(|value| value.to_str().ok())
            .unwrap_or_default();
        let platform = detect_platform(user_agent);
        let architecture = detect_architecture(user_agent, platform);
        let current_path = current_path(uri.path(), &state.config.base_path);
        Self {
            locale,
            current_path,
            base_path: state.config.base_path.clone(),
            base_url: state.config.base_url(),
            api_endpoint: state.config.api_endpoint.clone(),
            static_cdn_endpoint: state.config.static_cdn_endpoint.clone(),
            asset_version: state.config.build_version.clone(),
            country_code: state.geoip.country_code(headers),
            release_channel: state.config.release_channel,
            platform,
            architecture,
            test_build: uri
                .query()
                .map(|query| query.contains("test=1") || query.contains("test=true"))
                .unwrap_or(false),
        }
    }

    pub fn href(&self, path: &str) -> String {
        if is_passthrough_href(path) {
            return path.to_owned();
        }
        let normalized = normalize_path(path);
        self.with_base_path(&normalized)
    }

    pub fn absolute_href(&self, path: &str) -> String {
        if is_passthrough_href(path) {
            return path.to_owned();
        }
        let normalized = normalize_path(path);
        if self.base_url.ends_with('/') {
            format!("{}{}", self.base_url.trim_end_matches('/'), normalized)
        } else {
            format!("{}{}", self.base_url, normalized)
        }
    }

    fn with_base_path(&self, path: &str) -> String {
        if self.base_path.is_empty()
            || path == self.base_path
            || path.starts_with(&format!("{}/", self.base_path))
        {
            return path.to_owned();
        }
        if path == "/" {
            return self.base_path.clone();
        }
        format!("{}{}", self.base_path, path)
    }

    pub fn app_url(&self, path: &str) -> String {
        format!("{CANARY_WEB_APP_ENDPOINT}{path}")
    }

    pub fn api_url(&self, path: &str) -> String {
        let endpoint = if self.api_endpoint.trim().is_empty() {
            match self.release_channel {
                ReleaseChannel::Canary => CANARY_API_ENDPOINT,
                ReleaseChannel::Stable => STABLE_API_ENDPOINT,
            }
        } else {
            self.api_endpoint.trim_end_matches('/')
        };
        let normalized = normalize_path(path);
        format!("{endpoint}{normalized}")
    }
}

fn is_passthrough_href(path: &str) -> bool {
    path.starts_with("http://") || path.starts_with("https://") || path.starts_with('#')
}

pub fn state_context(
    State(state): State<AppState>,
    headers: HeaderMap,
    uri: Uri,
) -> (AppState, RequestContext) {
    let ctx = RequestContext::from_headers(&state, &headers, &uri);
    (state, ctx)
}

pub fn create_locale_cookie(locale: Locale, secret: &str) -> String {
    let payload = serde_json::to_string(&LocaleCookie {
        locale: locale.code().to_owned(),
        created_at: Some(
            SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap_or_default()
                .as_secs(),
        ),
    })
    .expect("locale cookie serialization should not fail");
    let encoded_payload = URL_SAFE_NO_PAD.encode(payload.as_bytes());
    let signature = sign_session_data(&encoded_payload, secret);
    format!("{encoded_payload}.{signature}")
}

pub fn parse_locale_cookie(value: &str, secret: &str) -> Option<String> {
    if let Some((encoded_payload, signature)) = value.rsplit_once('.') {
        if !verify_session_signature(encoded_payload, signature, secret) {
            return None;
        }
        let bytes = URL_SAFE_NO_PAD.decode(encoded_payload).ok()?;
        let locale_cookie: LocaleCookie = serde_json::from_slice(&bytes).ok()?;
        let created_at = locale_cookie.created_at?;
        let now = SystemTime::now().duration_since(UNIX_EPOCH).ok()?.as_secs();
        if now.saturating_sub(created_at) > LOCALE_COOKIE_MAX_AGE_SECONDS {
            return None;
        }
        return Some(locale_cookie.locale);
    }

    if is_legacy_locale_cookie(value) {
        return Some(value.to_owned());
    }

    None
}

fn resolve_request_locale(state: &AppState, headers: &HeaderMap) -> Locale {
    if let Some(cookie_header) = headers
        .get(header::COOKIE)
        .and_then(|value| value.to_str().ok())
    {
        for cookie in Cookie::split_parse(cookie_header) {
            let Ok(cookie) = cookie else {
                continue;
            };
            if cookie.name() == "locale"
                && let Some(code) =
                    parse_locale_cookie(cookie.value(), &state.config.secret_key_base)
                && let Some(locale) = state.i18n.locale_from_code(&code)
            {
                return locale;
            }
        }
    }
    if let Some(accept_language) = headers
        .get(header::ACCEPT_LANGUAGE)
        .and_then(|value| value.to_str().ok())
        && let Some(locale) = parse_accept_language(&state.i18n, accept_language)
    {
        return locale;
    }
    Locale::DEFAULT
}

fn parse_accept_language(i18n: &MarketingI18n, header: &str) -> Option<Locale> {
    let choices = accept_language::parse(header)
        .into_iter()
        .filter(|code| code != "*")
        .collect::<Vec<_>>();
    for code in &choices {
        if let Some(locale) = i18n.locale_from_code(code) {
            return Some(locale);
        }
    }
    for code in choices {
        if let Some(language) = code.split('-').next()
            && let Some(locale) = i18n.preferred_locale_for_language(language)
        {
            return Some(locale);
        }
    }
    None
}

fn sign_session_data(data: &str, secret: &str) -> String {
    let mut mac =
        HmacSha256::new_from_slice(secret.as_bytes()).expect("HMAC accepts any key length");
    mac.update(data.as_bytes());
    URL_SAFE_NO_PAD.encode(mac.finalize().into_bytes())
}

fn verify_session_signature(data: &str, signature: &str, secret: &str) -> bool {
    let Ok(signature_bytes) = URL_SAFE_NO_PAD.decode(signature) else {
        return false;
    };
    let mut mac =
        HmacSha256::new_from_slice(secret.as_bytes()).expect("HMAC accepts any key length");
    mac.update(data.as_bytes());
    mac.verify_slice(&signature_bytes).is_ok()
}

fn is_legacy_locale_cookie(value: &str) -> bool {
    !value.is_empty()
        && value.len() <= 16
        && value
            .chars()
            .all(|ch| ch.is_ascii_alphanumeric() || ch == '-' || ch == '_')
}

fn current_path(path: &str, base_path: &str) -> String {
    if base_path.is_empty() {
        return normalize_path(path);
    }
    let normalized = normalize_path(path);
    if normalized == base_path {
        return "/".to_owned();
    }
    normalized
        .strip_prefix(&format!("{base_path}/"))
        .map(|stripped| format!("/{stripped}"))
        .unwrap_or(normalized)
}

fn normalize_path(path: &str) -> String {
    if path.is_empty() {
        return "/".to_owned();
    }
    let with_slash = if path.starts_with('/') {
        path.to_owned()
    } else {
        format!("/{path}")
    };
    if with_slash.len() > 1 {
        with_slash.trim_end_matches('/').to_owned()
    } else {
        with_slash
    }
}

fn detect_platform(user_agent: &str) -> Platform {
    let ua = user_agent.to_ascii_lowercase();
    if ua.contains("iphone") || ua.contains("ipad") {
        Platform::Ios
    } else if ua.contains("android") {
        Platform::Android
    } else if ua.contains("windows") {
        Platform::Windows
    } else if ua.contains("macintosh") || ua.contains("mac os x") {
        Platform::Macos
    } else if ua.contains("linux") && !ua.contains("android") {
        Platform::Linux
    } else {
        Platform::Unknown
    }
}

fn detect_architecture(user_agent: &str, platform: Platform) -> Architecture {
    let ua = user_agent.to_ascii_lowercase();
    if ua.contains("arm64")
        || ua.contains("aarch64")
        || (platform == Platform::Macos && ua.contains("applewebkit"))
    {
        Architecture::Arm64
    } else if ua.contains("x86_64")
        || ua.contains("win64")
        || ua.contains("x64")
        || ua.contains("amd64")
    {
        Architecture::X64
    } else {
        Architecture::Unknown
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn locale_cookie_round_trips_and_rejects_tampering() {
        let cookie = create_locale_cookie(Locale::De, "test-secret");

        assert_ne!(cookie, Locale::De.code());
        assert_eq!(
            parse_locale_cookie(&cookie, "test-secret").as_deref(),
            Some(Locale::De.code())
        );
        assert_eq!(parse_locale_cookie(&cookie, "wrong-secret"), None);

        let mut tampered = cookie;
        tampered.push('x');
        assert_eq!(parse_locale_cookie(&tampered, "test-secret"), None);
    }

    #[test]
    fn locale_cookie_still_accepts_legacy_plain_codes() {
        assert_eq!(
            parse_locale_cookie("de", "test-secret").as_deref(),
            Some(Locale::De.code())
        );
    }

    #[test]
    fn visible_locale_payload_uses_url_safe_unpadded_base64() {
        assert_eq!(
            URL_SAFE_NO_PAD.encode(br#"{"locale":"de"}"#),
            "eyJsb2NhbGUiOiJkZSJ9"
        );
    }

    #[test]
    fn accept_language_prefers_exact_supported_locale_before_fallback() {
        let i18n = MarketingI18n::new().expect("marketing i18n should initialize");

        assert_eq!(
            parse_accept_language(&i18n, "fr-CA,de;q=0.9"),
            Some(Locale::De)
        );
    }
}
