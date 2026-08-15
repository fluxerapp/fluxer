// SPDX-License-Identifier: AGPL-3.0-or-later

mod android_association;
mod apple_association;
mod assets_proxy;
mod health;
mod spa_index;
mod spa_static;

use crate::state::AppState;
use axum::{
    Router,
    extract::Request,
    http::{HeaderName, HeaderValue, header},
    middleware::{Next, from_fn, from_fn_with_state},
    response::Response,
    routing::get,
};
use rand::RngExt;
use tower_http::{
    compression::{
        CompressionLayer,
        predicate::{DefaultPredicate, NotForContentType, Predicate},
    },
    trace::TraceLayer,
};

const STRICT_TRANSPORT_SECURITY_VALUE: &str = "max-age=31536000; includeSubDomains; preload";
const REFERRER_POLICY_VALUE: &str = "strict-origin-when-cross-origin";
const X_FRAME_OPTIONS_VALUE: &str = "DENY";
const PERMISSIONS_POLICY_VALUE: &str = "accelerometer=(), camera=(self), ch-dpr=(self), ch-save-data=(self), ch-viewport-width=(self), ch-width=(self), geolocation=(), gyroscope=(), magnetometer=(), microphone=(self), payment=(), usb=()";

pub fn build_router(state: AppState) -> Router {
    Router::new()
        .route("/_health", get(health::health))
        .route(
            "/.well-known/apple-app-site-association",
            get(apple_association::apple_app_site_association),
        )
        .route(
            "/.well-known/assetlinks.json",
            get(android_association::assetlinks),
        )
        .route(
            "/apple-app-site-association",
            get(apple_association::apple_app_site_association),
        )
        .route("/assets/{*path}", get(assets_proxy::proxy_assets))
        .route("/version.json", get(spa_static::version_json))
        .route("/manifest.json", get(spa_static::manifest_json))
        .route("/browserconfig.xml", get(spa_static::browserconfig_xml))
        .route("/sw.js", get(spa_static::service_worker))
        .route("/sw.js.map", get(spa_static::service_worker_map))
        .fallback(get(spa_index::spa_catch_all))
        .layer(from_fn(request_id_middleware))
        .layer(from_fn(cache_headers_middleware))
        .layer(from_fn_with_state(
            state.clone(),
            security_headers_middleware,
        ))
        .layer(
            CompressionLayer::new()
                .compress_when(DefaultPredicate::new().and(NotForContentType::const_new("font/"))),
        )
        .layer(TraceLayer::new_for_http())
        .with_state(state)
}

async fn security_headers_middleware(
    axum::extract::State(_state): axum::extract::State<AppState>,
    request: Request,
    next: Next,
) -> Response {
    let mut response = next.run(request).await;
    let headers = response.headers_mut();

    set_static_header(
        headers,
        header::STRICT_TRANSPORT_SECURITY,
        STRICT_TRANSPORT_SECURITY_VALUE,
    );
    set_static_header(headers, header::X_CONTENT_TYPE_OPTIONS, "nosniff");
    set_static_header(headers, header::REFERRER_POLICY, REFERRER_POLICY_VALUE);
    set_static_header(headers, header::X_FRAME_OPTIONS, X_FRAME_OPTIONS_VALUE);
    set_static_header(
        headers,
        HeaderName::from_static("permissions-policy"),
        PERMISSIONS_POLICY_VALUE,
    );

    response
}

async fn cache_headers_middleware(request: Request, next: Next) -> Response {
    let mut response = next.run(request).await;
    if !response.headers().contains_key(header::CACHE_CONTROL) {
        response
            .headers_mut()
            .insert(header::CACHE_CONTROL, HeaderValue::from_static("no-cache"));
    }
    response
}

async fn request_id_middleware(request: Request, next: Next) -> Response {
    let existing = request
        .headers()
        .get("x-request-id")
        .and_then(|v| v.to_str().ok())
        .filter(|s| !s.is_empty())
        .map(|s| s.to_owned());

    let id = existing.unwrap_or_else(generate_request_id);

    let mut response = next.run(request).await;
    if let Ok(v) = HeaderValue::from_str(&id) {
        response
            .headers_mut()
            .insert(HeaderName::from_static("x-request-id"), v);
    }
    response
}

fn generate_request_id() -> String {
    let bytes: [u8; 16] = rand::rng().random();
    hex::encode(bytes)
}

fn set_static_header(headers: &mut axum::http::HeaderMap, name: HeaderName, value: &'static str) {
    headers
        .entry(name)
        .or_insert(HeaderValue::from_static(value));
}
