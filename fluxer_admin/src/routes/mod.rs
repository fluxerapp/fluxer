// SPDX-License-Identifier: AGPL-3.0-or-later

pub mod admin;
pub mod applications;
pub mod auth;
pub mod bans;
mod bans_actions;
pub mod codes;
pub mod discovery;
mod guild_tabs;
pub mod guilds;
pub mod jobs;
mod message_actions;
pub mod messages;
pub mod reports;
pub mod system;
mod system_actions;
mod user_actions;
mod user_tabs;
pub mod users;
pub mod voice;
mod voice_actions;

use crate::{config::AdminConfig, middleware, state::AppState};
use serde::Deserialize;

#[derive(Deserialize)]
pub struct ActionQuery {
    pub action: Option<String>,
}
use axum::{
    Json, Router,
    extract::{Path, Request, State},
    http::{HeaderMap, HeaderName, HeaderValue, StatusCode, header},
    middleware::{Next, from_fn, from_fn_with_state},
    response::{Html, IntoResponse, Response},
    routing::get,
};
use tower_http::{
    compression::{
        CompressionLayer,
        predicate::{DefaultPredicate, NotForContentType, Predicate},
    },
    trace::TraceLayer,
};

const APP_CSS: &str = include_str!(concat!(env!("OUT_DIR"), "/static/app.css"));
const HTMX_JS: &str = include_str!("../../static/htmx.min.js");

const IMMUTABLE_CACHE_CONTROL: &str = "public, max-age=31536000, immutable";

const STRICT_TRANSPORT_SECURITY_VALUE: &str = "max-age=31536000; includeSubDomains; preload";
const REFERRER_POLICY_VALUE: &str = "strict-origin-when-cross-origin";
const X_FRAME_OPTIONS_VALUE: &str = "DENY";
const PERMISSIONS_POLICY_VALUE: &str = "accelerometer=(), camera=(), geolocation=(), \
    gyroscope=(), magnetometer=(), microphone=(), payment=(), usb=()";

pub fn build_router(config: AdminConfig) -> Router {
    let state = AppState::new(config);
    let protected = Router::new()
        .merge(users::router())
        .merge(guilds::router())
        .merge(reports::router())
        .merge(bans::router())
        .merge(applications::router())
        .merge(codes::router())
        .merge(discovery::router())
        .merge(jobs::router())
        .merge(messages::router())
        .merge(system::router())
        .merge(voice::router())
        .merge(admin::router())
        .route("/", get(dashboard))
        .route("/dashboard", get(dashboard))
        .layer(from_fn(middleware::htmx::flash_redirect_to_toast))
        .layer(from_fn(middleware::csrf::csrf_protection))
        .layer(from_fn(middleware::self_hosted::self_hosted_override))
        .layer(from_fn_with_state(
            state.clone(),
            middleware::auth::require_auth,
        ));

    Router::new()
        .route("/_health", get(health))
        .route("/robots.txt", get(robots_txt))
        .route("/static/app.css", get(app_css))
        .route("/static/fonts/{file_name}", get(font_asset))
        .route("/static/htmx.min.js", get(htmx_js))
        .merge(auth::router())
        .merge(protected)
        .fallback(not_found)
        .layer(from_fn_with_state(
            state.clone(),
            security_headers_middleware,
        ))
        .layer(from_fn(cache_headers_middleware))
        .layer(
            CompressionLayer::new()
                .compress_when(DefaultPredicate::new().and(NotForContentType::const_new("font/"))),
        )
        .layer(TraceLayer::new_for_http())
        .with_state(state)
}

async fn security_headers_middleware(
    State(state): State<AppState>,
    request: Request,
    next: Next,
) -> Response {
    let mut response = next.run(request).await;
    let headers = response.headers_mut();
    let config = state.config();

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

    let csp = build_admin_csp(config);
    if let Ok(value) = HeaderValue::from_str(&csp) {
        headers
            .entry(header::CONTENT_SECURITY_POLICY)
            .or_insert(value);
    }

    response
}

fn build_admin_csp(config: &AdminConfig) -> String {
    let static_cdn = csp_origin(&config.static_cdn_endpoint);
    let media = csp_origin(&config.media_endpoint);
    [
        "default-src 'self'".to_owned(),
        "script-src 'self' 'unsafe-inline'".to_owned(),
        "style-src 'self' 'unsafe-inline'".to_owned(),
        format!("img-src 'self' data: blob: {static_cdn} {media} https://fluxer-reports.ewr1.vultrobjects.com"),
        "font-src 'self'".to_owned(),
        "connect-src 'self'".to_owned(),
        "object-src 'none'".to_owned(),
        "frame-src 'none'".to_owned(),
        "base-uri 'self'".to_owned(),
        "form-action 'self'".to_owned(),
        "frame-ancestors 'none'".to_owned(),
    ]
    .join("; ")
}

fn csp_origin(endpoint: &str) -> String {
    match url::Url::parse(endpoint) {
        Ok(u) => u.origin().ascii_serialization(),
        Err(_) => endpoint.trim_end_matches('/').to_owned(),
    }
}

fn set_static_header(headers: &mut HeaderMap, name: HeaderName, value: &'static str) {
    headers
        .entry(name)
        .or_insert(HeaderValue::from_static(value));
}

async fn cache_headers_middleware(request: Request, next: Next) -> Response {
    let mut response = next.run(request).await;
    if response.headers().contains_key(header::CACHE_CONTROL) {
        return response;
    }
    let content_type = response
        .headers()
        .get(header::CONTENT_TYPE)
        .and_then(|value| value.to_str().ok())
        .unwrap_or("");
    let cacheable_prefixes = ["text/css", "application/javascript", "font/", "image/"];
    let should_cache = cacheable_prefixes
        .iter()
        .any(|prefix| content_type.starts_with(prefix));
    let value = if should_cache {
        HeaderValue::from_static("public, max-age=31536000, immutable")
    } else {
        HeaderValue::from_static("no-cache, no-store")
    };
    response.headers_mut().insert(header::CACHE_CONTROL, value);
    response
}

async fn health() -> Json<serde_json::Value> {
    Json(serde_json::json!({"status": "ok"}))
}

async fn robots_txt() -> impl IntoResponse {
    (
        [(header::CONTENT_TYPE, "text/plain; charset=utf-8")],
        "User-agent: *\nDisallow: /\n",
    )
}

async fn app_css() -> impl IntoResponse {
    ([(header::CONTENT_TYPE, "text/css; charset=utf-8")], APP_CSS)
}

async fn font_asset(Path(file_name): Path<String>) -> Response {
    match crate::fonts::asset(&file_name) {
        Some((content_type, bytes)) => (
            [
                (header::CONTENT_TYPE, content_type),
                (header::CACHE_CONTROL, IMMUTABLE_CACHE_CONTROL),
            ],
            bytes,
        )
            .into_response(),
        None => StatusCode::NOT_FOUND.into_response(),
    }
}

async fn htmx_js() -> impl IntoResponse {
    (
        [(
            header::CONTENT_TYPE,
            "application/javascript; charset=utf-8",
        )],
        HTMX_JS,
    )
}

async fn dashboard(State(state): State<AppState>) -> impl IntoResponse {
    let config = state.config();
    (
        StatusCode::FOUND,
        [
            (
                header::LOCATION,
                HeaderValue::from_str(&format!("{}/users", config.base_path))
                    .unwrap_or_else(|_| HeaderValue::from_static("/users")),
            ),
            (
                header::CONTENT_TYPE,
                HeaderValue::from_static("text/plain; charset=utf-8"),
            ),
        ],
        "",
    )
}

async fn not_found(State(state): State<AppState>) -> impl IntoResponse {
    let config = state.config();
    let base = &config.base_path;
    let body = maud::html! {
        (maud::DOCTYPE)
        html lang="en" {
            head {
                meta charset="utf-8";
                meta name="viewport" content="width=device-width, initial-scale=1";
                title { "404 - Not Found" }
                link rel="stylesheet" href={(base) "/static/fonts/" (crate::fonts::STYLESHEET_FILE_NAME)};
                link rel="stylesheet" href={(base) "/static/app.css"};
            }
            body class="min-h-screen bg-neutral-50 flex items-center justify-center" {
                div class="text-center" {
                    h1 class="text-6xl font-bold text-neutral-300 mb-4" { "404" }
                    h2 class="text-xl font-semibold text-neutral-900 mb-2" {
                        "Page Not Found"
                    }
                    p class="text-neutral-500 mb-6" {
                        "The page you are looking for does not exist."
                    }
                    a href={(base) "/"}
                        class="inline-flex items-center rounded-md bg-brand-primary px-4 \
                               py-2 text-sm font-medium text-white hover:bg-brand-primary-dark" {
                        "Go to Dashboard"
                    }
                }
            }
        }
    };
    (StatusCode::NOT_FOUND, Html(body.into_string()))
}
