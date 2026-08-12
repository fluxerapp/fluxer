// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    config::{DOWNLOAD_RELEASE_CHANNEL, MarketingConfig},
    content::{
        BLOG_POSTS, BlogBookmarkAsset, BlogPost, HELP_ARTICLES, HELP_CATEGORIES, JOBS, POLICIES,
        blog_tag_label, blog_tag_slug, get_blog_post, get_help_article, get_job, get_policy,
        render_blog_markdown_with_copy_label,
    },
    downloads::fetch_latest_desktop_versions_cached,
    geoip::resolver_from_marketing_config,
    i18n::{Locale, MarketingI18n, descriptors::*},
    request_context::{AppState, CANARY_WEB_APP_ENDPOINT, RequestContext, create_locale_cookie},
    swish::SwishQrCache,
    templates,
};
use axum::{
    Json, Router,
    body::Body,
    extract::{Form, Path, Query, Request, State},
    http::{HeaderMap, HeaderName, HeaderValue, StatusCode, Uri, header},
    middleware::{Next, from_fn, from_fn_with_state},
    response::{Html, IntoResponse, Redirect, Response},
    routing::{get, post},
};
use base64::{Engine as _, engine::general_purpose::STANDARD as BASE64_STANDARD};
use email_address::EmailAddress;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::{
    sync::{Arc, LazyLock},
    time::Duration,
};
use time::{
    OffsetDateTime,
    format_description::well_known::{Rfc2822, Rfc3339},
};
use tower_http::{
    compression::{
        CompressionLayer,
        predicate::{DefaultPredicate, NotForContentType, Predicate},
    },
    trace::TraceLayer,
};

const APP_CSS: &str = include_str!(concat!(env!("OUT_DIR"), "/static/app.css"));
const HTMX_JS: &str = include_str!("../static/htmx.min.js");
const WORLD_MAP_SVG: &str = include_str!("../static/world-map-equirectangular.svg");
const VOICE_REGION_FLAG_AU_SVG: &str = include_str!("../static/voice-region-flags/1f1e6-1f1fa.svg");
const VOICE_REGION_FLAG_BR_SVG: &str = include_str!("../static/voice-region-flags/1f1e7-1f1f7.svg");
const VOICE_REGION_FLAG_CL_SVG: &str = include_str!("../static/voice-region-flags/1f1e8-1f1f1.svg");
const VOICE_REGION_FLAG_DE_SVG: &str = include_str!("../static/voice-region-flags/1f1e9-1f1ea.svg");
const VOICE_REGION_FLAG_ES_SVG: &str = include_str!("../static/voice-region-flags/1f1ea-1f1f8.svg");
const VOICE_REGION_FLAG_IN_SVG: &str = include_str!("../static/voice-region-flags/1f1ee-1f1f3.svg");
const VOICE_REGION_FLAG_KR_SVG: &str = include_str!("../static/voice-region-flags/1f1f0-1f1f7.svg");
const VOICE_REGION_FLAG_PL_SVG: &str = include_str!("../static/voice-region-flags/1f1f5-1f1f1.svg");
const VOICE_REGION_FLAG_SE_SVG: &str = include_str!("../static/voice-region-flags/1f1f8-1f1ea.svg");
const VOICE_REGION_FLAG_SG_SVG: &str = include_str!("../static/voice-region-flags/1f1f8-1f1ec.svg");
const VOICE_REGION_FLAG_US_SVG: &str = include_str!("../static/voice-region-flags/1f1fa-1f1f8.svg");
const VOICE_REGION_FLAG_ZA_SVG: &str = include_str!("../static/voice-region-flags/1f1ff-1f1e6.svg");
const CANARY_ROBOTS_HEADER_VALUE: &str = "noindex, nofollow, nosnippet, noimageindex, notranslate, max-snippet:0, max-image-preview:none, max-video-preview:0";
const STRICT_TRANSPORT_SECURITY_VALUE: &str = "max-age=31536000; includeSubDomains; preload";
const REFERRER_POLICY_VALUE: &str = "strict-origin-when-cross-origin";
const PERMISSIONS_POLICY_VALUE: &str = "accelerometer=(), camera=(), geolocation=(), gyroscope=(), magnetometer=(), microphone=(), payment=(), usb=()";
const X_FRAME_OPTIONS_VALUE: &str = "DENY";
const IMMUTABLE_CACHE_CONTROL: &str = "public, max-age=31536000, immutable";

#[derive(Deserialize)]
struct LocaleForm {
    locale: String,
    redirect: Option<String>,
}

#[derive(Deserialize)]
struct DonateQuery {
    #[serde(rename = "type")]
    donation_type: Option<String>,
    error: Option<String>,
    currency: Option<String>,
    swish: Option<String>,
    swish_amount: Option<String>,
}

#[derive(Deserialize)]
struct DonateManageQuery {
    email: Option<String>,
    alert: Option<String>,
}

#[derive(Deserialize)]
struct HelpQuery {
    q: Option<String>,
}

#[derive(Deserialize)]
struct BlogQuery {
    q: Option<String>,
    tag: Option<String>,
}

#[derive(Deserialize)]
struct SwishPaymentQuery {
    swish_amount: Option<String>,
}

#[derive(Deserialize)]
struct DonationAmountsQuery {
    audience: Option<String>,
    currency: String,
}

#[derive(Deserialize)]
struct DonationCheckoutForm {
    audience: String,
    email: String,
    amount_major: String,
    custom_amount_major: Option<String>,
    interval: String,
    currency: String,
}

#[derive(Deserialize)]
struct DonationRequestLinkForm {
    email: String,
}

#[derive(Serialize)]
struct DonationCheckoutPayload<'a> {
    email: &'a str,
    amount_cents: u32,
    currency: &'a str,
    interval: Option<&'a str>,
    is_business: bool,
}

#[derive(Serialize)]
struct DonationRequestLinkPayload<'a> {
    email: &'a str,
}

fn build_http_client() -> reqwest::Client {
    reqwest::Client::builder()
        .connect_timeout(Duration::from_secs(5))
        .timeout(Duration::from_secs(15))
        .build()
        .expect("default reqwest client should build")
}

pub fn build_router(config: MarketingConfig) -> Router {
    let config = Arc::new(config);
    let geoip = resolver_from_marketing_config(&config);
    let i18n = MarketingI18n::new().expect("embedded marketing i18n catalogs must parse");
    let state = AppState {
        config,
        geoip: Arc::new(geoip),
        http_client: build_http_client(),
        i18n: Arc::new(i18n),
        swish_qr_cache: SwishQrCache::new(),
        latest_versions_cache: crate::downloads::LatestVersionsCache::new(),
        donation_rate_limiter: crate::rate_limit::RateLimiter::new(20, Duration::from_secs(60)),
    };
    Router::new()
        .route("/", get(home))
        .route("/_health", get(health))
        .route("/_locale", post(set_locale))
        .route("/_swish/qr", get(crate::swish::swish_qr))
        .route("/_swish/payment", get(swish_payment))
        .route("/_donations/amounts", get(donation_amounts))
        .route("/_donations/checkout", post(donation_checkout))
        .route("/_donations/request-link", post(donation_request_link))
        .route("/static/app.css", get(app_css))
        .route("/static/fonts/{file_name}", get(font_asset))
        .route("/static/htmx.min.js", get(htmx_js))
        .route("/static/world-map-equirectangular.svg", get(world_map_svg))
        .route(
            "/static/voice-region-flags/{flag_file}",
            get(voice_region_flag_svg),
        )
        .route("/favicon.ico", get(favicon_ico))
        .route("/robots.txt", get(robots))
        .route("/security.txt", get(security_txt))
        .route("/sitemap.xml", get(sitemap))
        .route(
            "/.well-known/org.flathub.VerifiedApps.txt",
            get(flathub_verified),
        )
        .route(
            "/.well-known/apple-app-site-association",
            get(apple_app_site_association),
        )
        .route(
            "/apple-app-site-association",
            get(apple_app_site_association),
        )
        .route("/.well-known/assetlinks.json", get(assetlinks))
        .route(
            "/regional-restrictions",
            get(redirect_regional_restrictions),
        )
        .route("/rss", get(redirect_blog_rss))
        .route("/rss/", get(redirect_blog_rss))
        .route("/feed.xml", get(redirect_blog_rss))
        .route("/atom.xml", get(blog_atom))
        .route("/blog", get(blog))
        .route("/blog/", get(blog))
        .route("/blog/rss", get(blog_rss))
        .route("/blog/rss/", get(blog_rss))
        .route("/blog/rss.xml", get(blog_rss))
        .route("/blog/feed.xml", get(blog_rss))
        .route("/blog/atom.xml", get(blog_atom))
        .route("/blog/assets/{file}", get(blog_asset))
        .route("/blog/tag/{tag}", get(redirect_blog_tag))
        .route("/blog/{*path}", get(blog_slug))
        .route("/help", get(help))
        .route("/help/{slug}", get(help_slug))
        .route("/en", get(redirect_intercom_help_home))
        .route("/en/", get(redirect_intercom_help_home))
        .route("/en/articles/{slug}", get(redirect_intercom_article))
        .route("/en/collections/{slug}", get(redirect_intercom_collection))
        .route("/download", get(download))
        .route("/careers", get(careers))
        .route("/careers/{slug}", get(job))
        .route("/donate", get(donate))
        .route("/donate/manage", get(donate_manage))
        .route("/donate/success", get(donate_success))
        .route("/plutonium", get(plutonium))
        .route("/partners", get(partners))
        .route("/press", get(press))
        .route("/press/download/{asset_id}", get(press_download))
        .route("/terms", get(policy_terms))
        .route("/privacy", get(policy_privacy))
        .route("/security", get(policy_security))
        .route("/guidelines", get(policy_guidelines))
        .route("/company-information", get(policy_company_information))
        .route("/changelog", get(policy_changelog))
        .fallback(not_found)
        .layer(from_fn_with_state(
            state.clone(),
            canonical_host_redirect_middleware,
        ))
        .layer(from_fn(cache_headers_middleware))
        .layer(from_fn_with_state(
            state.clone(),
            canary_robots_header_middleware,
        ))
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
    State(state): State<AppState>,
    request: Request,
    next: Next,
) -> Response {
    let mut response = next.run(request).await;
    apply_security_headers(response.headers_mut(), &state.config);
    response
}

fn apply_security_headers(headers: &mut HeaderMap, config: &MarketingConfig) {
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
    let content_security_policy = build_marketing_content_security_policy(config);
    if let Ok(value) = HeaderValue::from_str(&content_security_policy) {
        headers
            .entry(header::CONTENT_SECURITY_POLICY)
            .or_insert(value);
    }
}

fn set_static_header(headers: &mut HeaderMap, name: HeaderName, value: &'static str) {
    headers
        .entry(name)
        .or_insert(HeaderValue::from_static(value));
}

fn inline_script_csp_source() -> &'static str {
    static SOURCE: LazyLock<String> = LazyLock::new(|| {
        let digest = Sha256::digest(templates::HEADING_ANCHOR_SCRIPT.as_bytes());
        format!("'sha256-{}'", BASE64_STANDARD.encode(digest))
    });
    SOURCE.as_str()
}

fn build_marketing_content_security_policy(config: &MarketingConfig) -> String {
    let static_cdn = config.static_cdn_endpoint.trim_end_matches('/');
    [
        "default-src 'self'".to_owned(),
        format!("script-src 'self' {}", inline_script_csp_source()),
        "style-src 'self' 'unsafe-inline'".to_owned(),
        format!("img-src 'self' data: blob: {static_cdn}"),
        "font-src 'self'".to_owned(),
        format!("media-src 'self' {static_cdn}"),
        "connect-src 'self'".to_owned(),
        "frame-src 'none'".to_owned(),
        "object-src 'none'".to_owned(),
        "base-uri 'self'".to_owned(),
        "form-action 'self'".to_owned(),
        "frame-ancestors 'none'".to_owned(),
    ]
    .join("; ")
}

async fn canary_robots_header_middleware(
    State(state): State<AppState>,
    request: Request,
    next: Next,
) -> Response {
    let mut response = next.run(request).await;
    if state.config.is_canary() {
        response.headers_mut().insert(
            HeaderName::from_static("x-robots-tag"),
            HeaderValue::from_static(CANARY_ROBOTS_HEADER_VALUE),
        );
    }
    response
}

async fn canonical_host_redirect_middleware(
    State(state): State<AppState>,
    request: Request,
    next: Next,
) -> Response {
    match request_host(request.headers()).as_deref() {
        Some("help.fluxer.app") => {
            let target_path = help_host_redirect_path(request.uri());
            let target = absolute_marketing_url(&state.config.base_url(), &target_path);
            return Redirect::permanent(&target).into_response();
        }
        Some("blog.fluxer.app") => {
            let target_path = blog_host_redirect_path(request.uri());
            let target = absolute_marketing_url(&state.config.base_url(), &target_path);
            return Redirect::permanent(&target).into_response();
        }
        Some("www.fluxer.app" | "fluxerapp.com" | "www.fluxerapp.com") => {
            let target =
                absolute_marketing_url(&state.config.base_url(), uri_path_and_query(request.uri()));
            return Redirect::permanent(&target).into_response();
        }
        Some("fluxer.gg") => {
            let target = append_uri(&format!("{CANARY_WEB_APP_ENDPOINT}/invite"), request.uri());
            return Redirect::temporary(&target).into_response();
        }
        Some("fluxer.gift") => {
            let target = if request.uri().path() == "/" {
                state.config.base_url()
            } else {
                append_uri(&format!("{CANARY_WEB_APP_ENDPOINT}/gift"), request.uri())
            };
            return Redirect::temporary(&target).into_response();
        }
        Some("fluxer.dev" | "www.fluxer.dev") => {
            let target = append_uri("https://docs.fluxer.app", request.uri());
            return Redirect::permanent(&target).into_response();
        }
        Some("every.day.im.fluxer.ing") => {
            return Redirect::temporary("https://www.youtube.com/watch?v=KQ6zr6kCPj8")
                .into_response();
        }
        _ => {}
    }
    if let Some(response) = legacy_marketing_redirect(&state.config, request.uri()) {
        return response;
    }
    next.run(request).await
}

fn request_host(headers: &HeaderMap) -> Option<String> {
    let forwarded_host = HeaderName::from_static("x-forwarded-host");
    let raw = headers
        .get(header::HOST)
        .or_else(|| headers.get(&forwarded_host))?
        .to_str()
        .ok()?;
    let first = raw.split(',').next()?.trim();
    if first.is_empty() {
        return None;
    }
    let host = first
        .rsplit_once(':')
        .filter(|(_, port)| port.chars().all(|ch| ch.is_ascii_digit()))
        .map(|(host, _)| host)
        .unwrap_or(first)
        .trim()
        .trim_matches('.');
    if host.is_empty() {
        None
    } else {
        Some(host.to_ascii_lowercase())
    }
}

fn legacy_marketing_redirect(config: &MarketingConfig, uri: &Uri) -> Option<Response> {
    match uri.path().trim_end_matches('/') {
        "/channels" => {
            let target = append_uri(CANARY_WEB_APP_ENDPOINT, uri);
            Some(Redirect::temporary(&target).into_response())
        }
        "/delete-my-account" => Some(Redirect::temporary("/help/delete-account").into_response()),
        "/delete-my-data" => Some(Redirect::temporary("/help/data-deletion").into_response()),
        "/export-my-data" => Some(Redirect::temporary("/help/data-export").into_response()),
        "/bugs" => Some(Redirect::temporary("/help/report-bug").into_response()),
        "/.well-known/fluxer" => {
            let target = format!("{}/.well-known/fluxer", config.api_endpoint);
            Some(Redirect::permanent(&target).into_response())
        }
        _ if uri.path().starts_with("/channels/") => {
            let target = append_uri(CANARY_WEB_APP_ENDPOINT, uri);
            Some(Redirect::temporary(&target).into_response())
        }
        _ => None,
    }
}

fn help_host_redirect_path(uri: &Uri) -> String {
    let normalized_path = uri.path().trim_end_matches('/');
    let path = if normalized_path.is_empty() {
        "/"
    } else {
        normalized_path
    };
    match path {
        "/" | "/en" | "/help" => {
            let mut target = "/help".to_owned();
            if let Some(query) = uri.query().filter(|query| !query.is_empty()) {
                target.push('?');
                target.push_str(query);
            }
            target
        }
        _ => help_article_redirect_path(path)
            .or_else(|| help_collection_redirect_path(path))
            .unwrap_or_else(|| "/help".to_owned()),
    }
}

fn help_article_redirect_path(path: &str) -> Option<String> {
    let slug = path
        .strip_prefix("/en/articles/")
        .or_else(|| path.strip_prefix("/articles/"))
        .or_else(|| path.strip_prefix("/help/"))?;
    let slug = slug.split('/').next().unwrap_or_default();
    if slug.is_empty() {
        return Some("/help".to_owned());
    }
    get_help_article(slug).map(|article| format!("/help/{}", article.slug))
}

fn help_collection_redirect_path(path: &str) -> Option<String> {
    let slug = path
        .strip_prefix("/en/collections/")
        .or_else(|| path.strip_prefix("/collections/"))?;
    let slug = slug.split('/').next().unwrap_or_default();
    Some(format!("/help{}", help_collection_anchor(slug)))
}

fn help_collection_anchor(slug: &str) -> String {
    let collection_slug = if HELP_CATEGORIES.iter().any(|category| category.slug == slug) {
        slug
    } else {
        slug.split_once('-').map(|(_, tail)| tail).unwrap_or(slug)
    };
    HELP_CATEGORIES
        .iter()
        .find(|category| category.slug == collection_slug)
        .map(|category| format!("#{}", category.slug))
        .unwrap_or_default()
}

fn blog_host_redirect_path(uri: &Uri) -> String {
    let normalized_path = uri.path().trim_end_matches('/');
    let path = if normalized_path.is_empty() {
        "/"
    } else {
        normalized_path
    };
    match path {
        "/" | "/blog" => {
            let mut target = "/blog".to_owned();
            if let Some(query) = uri.query().filter(|query| !query.is_empty()) {
                target.push('?');
                target.push_str(query);
            }
            target
        }
        "/rss" | "/rss.xml" | "/feed.xml" => "/blog/rss.xml".to_owned(),
        "/atom.xml" => "/blog/atom.xml".to_owned(),
        _ => blog_asset_redirect_path(path)
            .or_else(|| blog_tag_redirect_path(path))
            .or_else(|| blog_post_redirect_path(path))
            .unwrap_or_else(|| "/blog".to_owned()),
    }
}

fn blog_asset_redirect_path(path: &str) -> Option<String> {
    match path {
        "/content/images/2026/01/roadmap-2026-feature-image-1.png" => {
            Some("/blog/assets/roadmap-2026-feature-image-1280.jpg".to_owned())
        }
        "/content/images/2026/04/cover.png" => {
            Some("/blog/assets/how-i-built-fluxer-cover-1280.jpg".to_owned())
        }
        _ => None,
    }
}

fn blog_tag_redirect_path(path: &str) -> Option<String> {
    let slug = path
        .strip_prefix("/tag/")
        .or_else(|| path.strip_prefix("/blog/tag/"))?;
    let slug = slug.split('/').next().unwrap_or_default();
    if slug.is_empty() {
        return Some("/blog".to_owned());
    }
    Some(format!("/blog?tag={}", urlencoding::encode(slug)))
}

fn blog_post_redirect_path(path: &str) -> Option<String> {
    let slug = path
        .strip_prefix("/blog/")
        .or_else(|| path.strip_prefix('/'))?;
    let slug = slug.split('/').next().unwrap_or_default();
    if slug.is_empty() {
        return Some("/blog".to_owned());
    }
    get_blog_post(slug).map(|post| format!("/blog/{}", post.slug))
}

fn absolute_marketing_url(base_url: &str, path: &str) -> String {
    let base = base_url.trim_end_matches('/');
    if path.starts_with('/') {
        format!("{base}{path}")
    } else {
        format!("{base}/{path}")
    }
}

fn append_uri(prefix: &str, uri: &Uri) -> String {
    format!(
        "{}{}",
        prefix.trim_end_matches('/'),
        uri_path_and_query(uri)
    )
}

fn uri_path_and_query(uri: &Uri) -> &str {
    uri.path_and_query()
        .map(|path_and_query| path_and_query.as_str())
        .unwrap_or("/")
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
    let cacheable_prefixes = [
        "text/css",
        "application/javascript",
        "application/json",
        "font/",
        "image/",
        "video/",
        "audio/",
        "application/font-woff2",
    ];
    let should_cache = cacheable_prefixes
        .iter()
        .any(|prefix| content_type.starts_with(prefix));
    let value = if should_cache {
        HeaderValue::from_static("public, max-age=31536000, immutable")
    } else {
        HeaderValue::from_static("no-cache")
    };
    response.headers_mut().insert(header::CACHE_CONTROL, value);
    response
}

async fn home(State(state): State<AppState>, headers: HeaderMap, uri: Uri) -> Html<String> {
    let ctx = RequestContext::from_headers(&state, &headers, &uri);
    Html(templates::home_page(&state.i18n, &ctx).into_string())
}

async fn download(State(state): State<AppState>, headers: HeaderMap, uri: Uri) -> Response {
    let ctx = RequestContext::from_headers(&state, &headers, &uri);
    let latest_versions = fetch_latest_desktop_versions_cached(
        &state.latest_versions_cache,
        &state.http_client,
        &state.config.api_endpoint,
        DOWNLOAD_RELEASE_CHANNEL.segment(),
    )
    .await;
    let mut response =
        Html(templates::download_page(&state.i18n, &ctx, &latest_versions).into_string())
            .into_response();
    response.headers_mut().insert(
        header::CACHE_CONTROL,
        HeaderValue::from_static("public, max-age=60, stale-while-revalidate=300"),
    );
    response
}

async fn help(
    State(state): State<AppState>,
    headers: HeaderMap,
    uri: Uri,
    Query(query): Query<HelpQuery>,
) -> Html<String> {
    let ctx = RequestContext::from_headers(&state, &headers, &uri);
    Html(templates::help_page(&state.i18n, &ctx, query.q.as_deref()).into_string())
}

async fn blog(
    State(state): State<AppState>,
    headers: HeaderMap,
    uri: Uri,
    Query(query): Query<BlogQuery>,
) -> Html<String> {
    let ctx = RequestContext::from_headers(&state, &headers, &uri);
    Html(
        templates::blog_page(&state.i18n, &ctx, query.q.as_deref(), query.tag.as_deref())
            .into_string(),
    )
}

async fn careers(State(state): State<AppState>, headers: HeaderMap, uri: Uri) -> Html<String> {
    let ctx = RequestContext::from_headers(&state, &headers, &uri);
    Html(templates::careers_page(&state.i18n, &ctx).into_string())
}

async fn job(
    State(state): State<AppState>,
    headers: HeaderMap,
    uri: Uri,
    Path(slug): Path<String>,
) -> Response {
    let ctx = RequestContext::from_headers(&state, &headers, &uri);
    match get_job(&slug) {
        Some(job) => {
            Html(templates::job_page(&state.i18n, &ctx, job).into_string()).into_response()
        }
        None => not_found(State(state), headers, uri).await.into_response(),
    }
}

async fn donate(
    State(state): State<AppState>,
    headers: HeaderMap,
    uri: Uri,
    Query(query): Query<DonateQuery>,
) -> Html<String> {
    let ctx = RequestContext::from_headers(&state, &headers, &uri);
    Html(
        templates::donate_page(
            &state.i18n,
            &ctx,
            templates::DonatePageOptions {
                donation_type: query.donation_type.as_deref(),
                error: query.error.as_deref(),
                currency: query.currency.as_deref(),
                swish_open: query.swish.as_deref() == Some("1"),
                swish_amount: query.swish_amount.as_deref(),
            },
        )
        .into_string(),
    )
}

async fn donate_manage(
    State(state): State<AppState>,
    headers: HeaderMap,
    uri: Uri,
    Query(query): Query<DonateManageQuery>,
) -> Html<String> {
    let ctx = RequestContext::from_headers(&state, &headers, &uri);
    Html(
        templates::donate_manage_page(
            &state.i18n,
            &ctx,
            query.email.as_deref().unwrap_or_default(),
            query.alert.as_deref(),
        )
        .into_string(),
    )
}

async fn donate_success(
    State(state): State<AppState>,
    headers: HeaderMap,
    uri: Uri,
) -> Html<String> {
    let ctx = RequestContext::from_headers(&state, &headers, &uri);
    Html(templates::donate_success_page(&state.i18n, &ctx).into_string())
}

async fn swish_payment(
    State(state): State<AppState>,
    headers: HeaderMap,
    uri: Uri,
    Query(query): Query<SwishPaymentQuery>,
) -> Html<String> {
    let ctx = RequestContext::from_headers(&state, &headers, &uri);
    Html(
        templates::swish_payment_fragment(&state.i18n, &ctx, query.swish_amount.as_deref())
            .into_string(),
    )
}

async fn donation_amounts(
    State(state): State<AppState>,
    headers: HeaderMap,
    uri: Uri,
    Query(query): Query<DonationAmountsQuery>,
) -> Response {
    let Some(currency) = templates::DonationCurrency::from_code(&query.currency) else {
        return StatusCode::BAD_REQUEST.into_response();
    };
    let ctx = RequestContext::from_headers(&state, &headers, &uri);
    let audience = templates::DonationAudience::from_query(query.audience.as_deref());
    Html(
        templates::donation_amount_fieldset_fragment(&state.i18n, &ctx, audience, currency)
            .into_string(),
    )
    .into_response()
}

async fn donation_checkout(
    State(state): State<AppState>,
    headers: HeaderMap,
    uri: Uri,
    Form(form): Form<DonationCheckoutForm>,
) -> Response {
    let ctx = RequestContext::from_headers(&state, &headers, &uri);
    if !state
        .donation_rate_limiter
        .check(&rate_limit_key(&headers, &state.config))
        .await
    {
        return too_many_requests_response();
    }
    let is_htmx = is_htmx_request(&headers);
    let audience_id = if form.audience == "business" {
        "business"
    } else {
        "individual"
    };
    let audience = templates::DonationAudience::from_query(Some(audience_id));
    if !looks_like_email(&form.email) {
        return donation_checkout_error_response(
            &state.i18n,
            &ctx,
            audience,
            audience_id,
            "invalid_email",
            None,
            is_htmx,
        );
    }
    let Some(currency) = templates::DonationCurrency::from_code(&form.currency) else {
        return donation_checkout_error_response(
            &state.i18n,
            &ctx,
            audience,
            audience_id,
            "generic",
            None,
            is_htmx,
        );
    };
    let Some(amount_major) = templates::donation_amount_from_form(
        &form.amount_major,
        form.custom_amount_major.as_deref(),
        currency,
    ) else {
        return donation_checkout_error_response(
            &state.i18n,
            &ctx,
            audience,
            audience_id,
            "invalid_amount",
            Some(currency),
            is_htmx,
        );
    };
    let interval = match form.interval.as_str() {
        "month" | "year" => Some(form.interval.as_str()),
        _ => None,
    };
    let payload = DonationCheckoutPayload {
        email: form.email.trim(),
        amount_cents: amount_major.saturating_mul(100),
        currency: currency.code(),
        interval,
        is_business: audience_id == "business",
    };
    let response = state
        .http_client
        .post(format!("{}/donations/checkout", state.config.api_endpoint))
        .json(&payload)
        .send()
        .await;
    let Ok(response) = response else {
        return donation_checkout_error_response(
            &state.i18n,
            &ctx,
            audience,
            audience_id,
            "network",
            Some(currency),
            is_htmx,
        );
    };
    if !response.status().is_success() {
        return donation_checkout_error_response(
            &state.i18n,
            &ctx,
            audience,
            audience_id,
            "generic",
            Some(currency),
            is_htmx,
        );
    }
    let Ok(body) = response.json::<serde_json::Value>().await else {
        return donation_checkout_error_response(
            &state.i18n,
            &ctx,
            audience,
            audience_id,
            "generic",
            Some(currency),
            is_htmx,
        );
    };
    let Some(url) = body.get("url").and_then(|value| value.as_str()) else {
        return donation_checkout_error_response(
            &state.i18n,
            &ctx,
            audience,
            audience_id,
            "generic",
            Some(currency),
            is_htmx,
        );
    };
    checkout_redirect_response(url, is_htmx)
}

async fn donation_request_link(
    State(state): State<AppState>,
    headers: HeaderMap,
    uri: Uri,
    Form(form): Form<DonationRequestLinkForm>,
) -> Response {
    let ctx = RequestContext::from_headers(&state, &headers, &uri);
    if !state
        .donation_rate_limiter
        .check(&rate_limit_key(&headers, &state.config))
        .await
    {
        return too_many_requests_response();
    }
    let is_htmx = is_htmx_request(&headers);
    if !looks_like_email(&form.email) {
        return donation_manage_result_response(
            &state.i18n,
            &ctx,
            &form.email,
            "invalid_email",
            is_htmx,
        );
    }
    let payload = DonationRequestLinkPayload {
        email: form.email.trim(),
    };
    let response = state
        .http_client
        .post(format!(
            "{}/donations/request-link",
            state.config.api_endpoint
        ))
        .json(&payload)
        .send()
        .await;
    let Ok(response) = response else {
        return donation_manage_result_response(&state.i18n, &ctx, &form.email, "network", is_htmx);
    };
    if !response.status().is_success() {
        return donation_manage_result_response(&state.i18n, &ctx, &form.email, "generic", is_htmx);
    }
    donation_manage_result_response(&state.i18n, &ctx, &form.email, "sent", is_htmx)
}

async fn plutonium(State(state): State<AppState>, headers: HeaderMap, uri: Uri) -> Html<String> {
    let ctx = RequestContext::from_headers(&state, &headers, &uri);
    Html(templates::plutonium_page(&state.i18n, &ctx).into_string())
}

async fn partners(State(state): State<AppState>, headers: HeaderMap, uri: Uri) -> Html<String> {
    let ctx = RequestContext::from_headers(&state, &headers, &uri);
    Html(templates::partners_page(&state.i18n, &ctx).into_string())
}

async fn press(State(state): State<AppState>, headers: HeaderMap, uri: Uri) -> Html<String> {
    let ctx = RequestContext::from_headers(&state, &headers, &uri);
    Html(templates::press_page(&state.i18n, &ctx).into_string())
}

async fn policy_terms(State(state): State<AppState>, headers: HeaderMap, uri: Uri) -> Response {
    render_policy_by_slug(state, headers, uri, "terms").await
}

async fn policy_privacy(State(state): State<AppState>, headers: HeaderMap, uri: Uri) -> Response {
    render_policy_by_slug(state, headers, uri, "privacy").await
}

async fn policy_security(State(state): State<AppState>, headers: HeaderMap, uri: Uri) -> Response {
    render_policy_by_slug(state, headers, uri, "security").await
}

async fn policy_guidelines(
    State(state): State<AppState>,
    headers: HeaderMap,
    uri: Uri,
) -> Response {
    render_policy_by_slug(state, headers, uri, "guidelines").await
}

async fn policy_company_information(
    State(state): State<AppState>,
    headers: HeaderMap,
    uri: Uri,
) -> Response {
    render_policy_by_slug(state, headers, uri, "company-information").await
}

async fn policy_changelog(State(state): State<AppState>, headers: HeaderMap, uri: Uri) -> Response {
    render_policy_by_slug(state, headers, uri, "changelog").await
}

async fn render_policy_by_slug(
    state: AppState,
    headers: HeaderMap,
    uri: Uri,
    slug: &str,
) -> Response {
    let ctx = RequestContext::from_headers(&state, &headers, &uri);
    match get_policy(slug) {
        Some(policy) => {
            Html(templates::policy_page(&state.i18n, &ctx, policy).into_string()).into_response()
        }
        None => not_found(State(state), headers, uri).await.into_response(),
    }
}

async fn not_found(
    State(state): State<AppState>,
    headers: HeaderMap,
    uri: Uri,
) -> impl IntoResponse {
    let ctx = RequestContext::from_headers(&state, &headers, &uri);
    (
        StatusCode::NOT_FOUND,
        Html(templates::not_found_page(&state.i18n, &ctx).into_string()),
    )
}

async fn set_locale(State(state): State<AppState>, Form(form): Form<LocaleForm>) -> Response {
    let Some(locale) = state.i18n.locale_from_code(&form.locale) else {
        return StatusCode::BAD_REQUEST.into_response();
    };
    let redirect = sanitize_redirect(form.redirect.as_deref().unwrap_or("/"));
    let redirect = prepend_base_path(&state.config.base_path, &redirect);
    let cookie = create_locale_cookie(locale, &state.config.secret_key_base);
    let mut response = Redirect::to(&redirect).into_response();
    let header_value = format!("locale={cookie}; Path=/; Max-Age=31536000; SameSite=Lax");
    response.headers_mut().insert(
        header::SET_COOKIE,
        HeaderValue::from_str(&header_value).expect("valid locale cookie header"),
    );
    response
}

async fn health() -> Json<serde_json::Value> {
    Json(serde_json::json!({"status": "ok"}))
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

async fn world_map_svg() -> impl IntoResponse {
    (
        [
            (header::CONTENT_TYPE, "image/svg+xml; charset=utf-8"),
            (header::CACHE_CONTROL, "public, max-age=604800, immutable"),
        ],
        WORLD_MAP_SVG,
    )
}

async fn voice_region_flag_svg(Path(flag_file): Path<String>) -> Response {
    let Some(flag_code) = flag_file.strip_suffix(".svg") else {
        return StatusCode::NOT_FOUND.into_response();
    };

    let svg = match flag_code {
        "1f1e6-1f1fa" => Some(VOICE_REGION_FLAG_AU_SVG),
        "1f1e7-1f1f7" => Some(VOICE_REGION_FLAG_BR_SVG),
        "1f1e8-1f1f1" => Some(VOICE_REGION_FLAG_CL_SVG),
        "1f1e9-1f1ea" => Some(VOICE_REGION_FLAG_DE_SVG),
        "1f1ea-1f1f8" => Some(VOICE_REGION_FLAG_ES_SVG),
        "1f1ee-1f1f3" => Some(VOICE_REGION_FLAG_IN_SVG),
        "1f1f0-1f1f7" => Some(VOICE_REGION_FLAG_KR_SVG),
        "1f1f5-1f1f1" => Some(VOICE_REGION_FLAG_PL_SVG),
        "1f1f8-1f1ea" => Some(VOICE_REGION_FLAG_SE_SVG),
        "1f1f8-1f1ec" => Some(VOICE_REGION_FLAG_SG_SVG),
        "1f1fa-1f1f8" => Some(VOICE_REGION_FLAG_US_SVG),
        "1f1ff-1f1e6" => Some(VOICE_REGION_FLAG_ZA_SVG),
        _ => None,
    };

    match svg {
        Some(svg) => (
            [
                (header::CONTENT_TYPE, "image/svg+xml; charset=utf-8"),
                (header::CACHE_CONTROL, "public, max-age=604800, immutable"),
            ],
            svg,
        )
            .into_response(),
        None => StatusCode::NOT_FOUND.into_response(),
    }
}

async fn favicon_ico(State(state): State<AppState>) -> impl IntoResponse {
    let cdn = state.config.static_cdn_endpoint.trim_end_matches('/');
    Redirect::permanent(&format!("{cdn}/web/favicon.ico"))
}

async fn robots(State(state): State<AppState>) -> impl IntoResponse {
    let base_url = state.config.base_url();
    let body = format!("User-agent: *\nAllow: /\nSitemap: {base_url}/sitemap.xml\n");
    ([(header::CONTENT_TYPE, "text/plain; charset=utf-8")], body)
}

async fn security_txt(State(state): State<AppState>) -> impl IntoResponse {
    let base_url = state.config.base_url();
    let now = OffsetDateTime::now_utc();
    let expires = now
        .replace_year(now.year() + 2)
        .unwrap_or_else(|_| now.saturating_add(time::Duration::days(730)))
        .format(&Rfc3339)
        .unwrap_or_default();
    let body = format!(
        "Contact: {base_url}/security\nContact: mailto:security@fluxer.app\nExpires: {expires}\nPreferred-Languages: en\nPolicy: {base_url}/security\n"
    );
    ([(header::CONTENT_TYPE, "text/plain; charset=utf-8")], body)
}

async fn sitemap(State(state): State<AppState>) -> impl IntoResponse {
    let base = state.config.base_url();
    let mut urls = vec![
        (base.clone(), "weekly", "1.0"),
        (format!("{base}/blog"), "weekly", "0.7"),
        (format!("{base}/careers"), "weekly", "0.6"),
        (format!("{base}/help"), "weekly", "0.7"),
        (format!("{base}/download"), "weekly", "0.9"),
        (format!("{base}/plutonium"), "weekly", "0.8"),
        (format!("{base}/partners"), "monthly", "0.6"),
        (format!("{base}/press"), "monthly", "0.5"),
    ];
    for policy in POLICIES {
        urls.push((format!("{base}/{}", policy.slug), "monthly", "0.5"));
    }
    for job in JOBS {
        urls.push((format!("{base}/careers/{}", job.slug), "weekly", "0.6"));
    }
    for article in HELP_ARTICLES {
        urls.push((format!("{base}/help/{}", article.slug), "monthly", "0.6"));
    }
    for post in BLOG_POSTS {
        urls.push((format!("{base}/blog/{}", post.slug), "monthly", "0.7"));
    }
    let entries = urls
		.into_iter()
		.map(|(loc, changefreq, priority)| {
			format!("  <url>\n    <loc>{loc}</loc>\n    <changefreq>{changefreq}</changefreq>\n    <priority>{priority}</priority>\n  </url>")
		})
		.collect::<Vec<_>>()
		.join("\n");
    let xml = format!(
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">\n{entries}\n</urlset>"
    );
    (
        [(header::CONTENT_TYPE, "application/xml; charset=utf-8")],
        xml,
    )
}

async fn flathub_verified() -> &'static str {
    "958e69ad-f965-489a-9fba-8b076da2b289\n"
}

async fn apple_app_site_association() -> impl IntoResponse {
    (
        [(header::CACHE_CONTROL, "public, max-age=1800")],
        Json(serde_json::json!({
            "webcredentials": {
                "apps": [
                    "3G5837T29K.app.fluxer",
                    "3G5837T29K.app.fluxer.canary",
                    "3G5837T29K.com.fluxer",
                    "3G5837T29K.com.fluxer.canary"
                ]
            }
        })),
    )
}

async fn assetlinks() -> impl IntoResponse {
    (
        [(header::CACHE_CONTROL, "public, max-age=1800")],
        Json(serde_json::json!([
            {
                "relation": [
                    "delegate_permission/common.handle_all_urls",
                    "delegate_permission/common.get_login_creds"
                ],
                "target": {
                    "namespace": "android_app",
                    "package_name": "com.fluxer",
                    "sha256_cert_fingerprints": ["91:E4:98:E1:B8:A6:C8:BA:99:41:5E:DB:29:78:29:6B:6C:58:BA:A5:E2:D2:A6:49:CE:C6:2D:A7:A8:29:C7:BC"]
                }
            },
            {
                "relation": [
                    "delegate_permission/common.handle_all_urls",
                    "delegate_permission/common.get_login_creds"
                ],
                "target": {
                    "namespace": "android_app",
                    "package_name": "com.fluxer.canary",
                    "sha256_cert_fingerprints": [
                        "91:E4:98:E1:B8:A6:C8:BA:99:41:5E:DB:29:78:29:6B:6C:58:BA:A5:E2:D2:A6:49:CE:C6:2D:A7:A8:29:C7:BC",
                        "CD:19:82:28:32:A8:DE:E0:97:D8:60:D9:21:28:C9:C7:C4:73:A3:72:7E:63:71:9B:A7:BB:3B:98:06:94:1F:6F"
                    ]
                }
            }
        ])),
    )
}

async fn redirect_regional_restrictions(State(state): State<AppState>) -> Redirect {
    Redirect::permanent(&prepend_base_path(
        &state.config.base_path,
        "/help/regional-restrictions",
    ))
}

async fn redirect_blog_rss(State(state): State<AppState>) -> Redirect {
    Redirect::permanent(&prepend_base_path(&state.config.base_path, "/blog/rss.xml"))
}

async fn redirect_blog_tag(State(state): State<AppState>, Path(tag): Path<String>) -> Redirect {
    let target = format!("/blog?tag={}", urlencoding::encode(&blog_tag_slug(&tag)));
    Redirect::permanent(&prepend_base_path(&state.config.base_path, &target))
}

async fn blog_slug(
    State(state): State<AppState>,
    headers: HeaderMap,
    uri: Uri,
    Path(path): Path<String>,
) -> Response {
    let slug = path.trim_matches('/').split('/').next().unwrap_or_default();
    let ctx = RequestContext::from_headers(&state, &headers, &uri);
    match get_blog_post(slug) {
        Some(post) if post.slug == slug && path.trim_matches('/') == slug => {
            Html(templates::blog_post_page(&state.i18n, &ctx, post).into_string()).into_response()
        }
        Some(post) => Redirect::permanent(&prepend_base_path(
            &state.config.base_path,
            &format!("/blog/{}", post.slug),
        ))
        .into_response(),
        None => not_found(State(state), headers, uri).await.into_response(),
    }
}

async fn blog_asset(headers: HeaderMap, Path(file): Path<String>) -> Response {
    if let Some(target) = blog_asset_alias(&file) {
        return Redirect::permanent(target).into_response();
    }
    let Some((content_type, bytes)) = blog_asset_bytes(&file) else {
        return StatusCode::NOT_FOUND.into_response();
    };
    blog_asset_response(&headers, content_type, bytes)
}

fn blog_asset_response(
    headers: &HeaderMap,
    content_type: &'static str,
    bytes: &'static [u8],
) -> Response {
    if let Some(range) = headers
        .get(header::RANGE)
        .and_then(|value| value.to_str().ok())
    {
        let Some((start, end)) = parse_byte_range(range, bytes.len()) else {
            let mut response = StatusCode::RANGE_NOT_SATISFIABLE.into_response();
            response
                .headers_mut()
                .insert(header::ACCEPT_RANGES, HeaderValue::from_static("bytes"));
            if let Ok(value) = HeaderValue::from_str(&format!("bytes */{}", bytes.len())) {
                response.headers_mut().insert(header::CONTENT_RANGE, value);
            }
            return response;
        };
        let mut response = Body::from(bytes[start..=end].to_vec()).into_response();
        *response.status_mut() = StatusCode::PARTIAL_CONTENT;
        set_blog_asset_headers(&mut response, content_type, end - start + 1);
        if let Ok(value) = HeaderValue::from_str(&format!("bytes {start}-{end}/{}", bytes.len())) {
            response.headers_mut().insert(header::CONTENT_RANGE, value);
        }
        return response;
    }
    let mut response = Body::from(bytes.to_vec()).into_response();
    set_blog_asset_headers(&mut response, content_type, bytes.len());
    response
}

fn set_blog_asset_headers(
    response: &mut Response,
    content_type: &'static str,
    content_length: usize,
) {
    response
        .headers_mut()
        .insert(header::CONTENT_TYPE, HeaderValue::from_static(content_type));
    response
        .headers_mut()
        .insert(header::ACCEPT_RANGES, HeaderValue::from_static("bytes"));
    if let Ok(value) = HeaderValue::from_str(&content_length.to_string()) {
        response.headers_mut().insert(header::CONTENT_LENGTH, value);
    }
    response.headers_mut().insert(
        header::CACHE_CONTROL,
        HeaderValue::from_static("public, max-age=31536000, immutable"),
    );
}

fn parse_byte_range(range: &str, len: usize) -> Option<(usize, usize)> {
    let range = range.strip_prefix("bytes=")?;
    let (start, end) = range.split_once('-')?;
    if range.contains(',') || len == 0 {
        return None;
    }
    if start.is_empty() {
        let suffix_len = end.parse::<usize>().ok()?;
        if suffix_len == 0 {
            return None;
        }
        let start = len.saturating_sub(suffix_len);
        return Some((start, len - 1));
    }
    let start = start.parse::<usize>().ok()?;
    let end = if end.is_empty() {
        len - 1
    } else {
        end.parse::<usize>().ok()?.min(len - 1)
    };
    (start <= end && start < len).then_some((start, end))
}

fn blog_asset_alias(file: &str) -> Option<&'static str> {
    match file {
        "how-i-built-fluxer-cover.png" => Some("/blog/assets/how-i-built-fluxer-cover-1280.jpg"),
        "mobile-clients-and-fluxer-v2-feature-image.png" => {
            Some("/blog/assets/mobile-clients-and-fluxer-v2-feature-image-1280.jpg")
        }
        "roadmap-2026-feature-image.png" => {
            Some("/blog/assets/roadmap-2026-feature-image-1280.jpg")
        }
        _ => None,
    }
}

fn blog_asset_bytes(file: &str) -> Option<(&'static str, &'static [u8])> {
    if let Some(asset) = BlogBookmarkAsset::from_file(file) {
        return Some((asset.content_type(), asset.bytes()));
    }

    match file {
        "bsky-jake-gold-avatar.jpg" => Some((
            "image/jpeg",
            include_bytes!("../static/blog/bsky-jake-gold-avatar.jpg"),
        )),
        "bsky-scylladb-link-thumb.jpg" => Some((
            "image/jpeg",
            include_bytes!("../static/blog/bsky-scylladb-link-thumb.jpg"),
        )),
        "erlang-the-movie-poster.jpg" => Some((
            "image/jpeg",
            include_bytes!("../static/blog/erlang-the-movie-poster.jpg"),
        )),
        "erlang-the-movie.mp4" => Some((
            "video/mp4",
            include_bytes!("../static/blog/erlang-the-movie.mp4"),
        )),
        "hampus-kraft-avatar.jpg" => Some((
            "image/jpeg",
            include_bytes!("../static/blog/hampus-kraft-avatar.jpg"),
        )),
        "discord-ui-revolutionary-640.avif" => Some((
            "image/avif",
            include_bytes!("../static/blog/discord-ui-revolutionary-640.avif"),
        )),
        "discord-ui-revolutionary-640.png" => Some((
            "image/png",
            include_bytes!("../static/blog/discord-ui-revolutionary-640.png"),
        )),
        "discord-ui-revolutionary-640.webp" => Some((
            "image/webp",
            include_bytes!("../static/blog/discord-ui-revolutionary-640.webp"),
        )),
        "discord-ui-revolutionary-960.avif" => Some((
            "image/avif",
            include_bytes!("../static/blog/discord-ui-revolutionary-960.avif"),
        )),
        "discord-ui-revolutionary-960.png" => Some((
            "image/png",
            include_bytes!("../static/blog/discord-ui-revolutionary-960.png"),
        )),
        "discord-ui-revolutionary-960.webp" => Some((
            "image/webp",
            include_bytes!("../static/blog/discord-ui-revolutionary-960.webp"),
        )),
        "discord-ui-revolutionary-1280.avif" => Some((
            "image/avif",
            include_bytes!("../static/blog/discord-ui-revolutionary-1280.avif"),
        )),
        "discord-ui-revolutionary-1280.png" => Some((
            "image/png",
            include_bytes!("../static/blog/discord-ui-revolutionary-1280.png"),
        )),
        "discord-ui-revolutionary-1280.webp" => Some((
            "image/webp",
            include_bytes!("../static/blog/discord-ui-revolutionary-1280.webp"),
        )),
        "discord-ui-revolutionary-1881.avif" => Some((
            "image/avif",
            include_bytes!("../static/blog/discord-ui-revolutionary-1881.avif"),
        )),
        "discord-ui-revolutionary-1881.png" => Some((
            "image/png",
            include_bytes!("../static/blog/discord-ui-revolutionary-1881.png"),
        )),
        "discord-ui-revolutionary-1881.webp" => Some((
            "image/webp",
            include_bytes!("../static/blog/discord-ui-revolutionary-1881.webp"),
        )),
        "how-i-built-fluxer-cover-640.avif" => Some((
            "image/avif",
            include_bytes!("../static/blog/how-i-built-fluxer-cover-640.avif"),
        )),
        "how-i-built-fluxer-cover-640.jpg" => Some((
            "image/jpeg",
            include_bytes!("../static/blog/how-i-built-fluxer-cover-640.jpg"),
        )),
        "how-i-built-fluxer-cover-640.webp" => Some((
            "image/webp",
            include_bytes!("../static/blog/how-i-built-fluxer-cover-640.webp"),
        )),
        "how-i-built-fluxer-cover-960.avif" => Some((
            "image/avif",
            include_bytes!("../static/blog/how-i-built-fluxer-cover-960.avif"),
        )),
        "how-i-built-fluxer-cover-960.jpg" => Some((
            "image/jpeg",
            include_bytes!("../static/blog/how-i-built-fluxer-cover-960.jpg"),
        )),
        "how-i-built-fluxer-cover-960.webp" => Some((
            "image/webp",
            include_bytes!("../static/blog/how-i-built-fluxer-cover-960.webp"),
        )),
        "how-i-built-fluxer-cover-1280.avif" => Some((
            "image/avif",
            include_bytes!("../static/blog/how-i-built-fluxer-cover-1280.avif"),
        )),
        "how-i-built-fluxer-cover-1280.jpg" => Some((
            "image/jpeg",
            include_bytes!("../static/blog/how-i-built-fluxer-cover-1280.jpg"),
        )),
        "how-i-built-fluxer-cover-1280.webp" => Some((
            "image/webp",
            include_bytes!("../static/blog/how-i-built-fluxer-cover-1280.webp"),
        )),
        "how-i-built-fluxer-cover-2000.avif" => Some((
            "image/avif",
            include_bytes!("../static/blog/how-i-built-fluxer-cover-2000.avif"),
        )),
        "how-i-built-fluxer-cover-2000.jpg" => Some((
            "image/jpeg",
            include_bytes!("../static/blog/how-i-built-fluxer-cover-2000.jpg"),
        )),
        "how-i-built-fluxer-cover-2000.webp" => Some((
            "image/webp",
            include_bytes!("../static/blog/how-i-built-fluxer-cover-2000.webp"),
        )),
        "mobile-clients-and-fluxer-v2-feature-image-640.avif" => Some((
            "image/avif",
            include_bytes!("../static/blog/mobile-clients-and-fluxer-v2-feature-image-640.avif"),
        )),
        "mobile-clients-and-fluxer-v2-feature-image-640.jpg" => Some((
            "image/jpeg",
            include_bytes!("../static/blog/mobile-clients-and-fluxer-v2-feature-image-640.jpg"),
        )),
        "mobile-clients-and-fluxer-v2-feature-image-640.webp" => Some((
            "image/webp",
            include_bytes!("../static/blog/mobile-clients-and-fluxer-v2-feature-image-640.webp"),
        )),
        "mobile-clients-and-fluxer-v2-feature-image-960.avif" => Some((
            "image/avif",
            include_bytes!("../static/blog/mobile-clients-and-fluxer-v2-feature-image-960.avif"),
        )),
        "mobile-clients-and-fluxer-v2-feature-image-960.jpg" => Some((
            "image/jpeg",
            include_bytes!("../static/blog/mobile-clients-and-fluxer-v2-feature-image-960.jpg"),
        )),
        "mobile-clients-and-fluxer-v2-feature-image-960.webp" => Some((
            "image/webp",
            include_bytes!("../static/blog/mobile-clients-and-fluxer-v2-feature-image-960.webp"),
        )),
        "mobile-clients-and-fluxer-v2-feature-image-1280.avif" => Some((
            "image/avif",
            include_bytes!("../static/blog/mobile-clients-and-fluxer-v2-feature-image-1280.avif"),
        )),
        "mobile-clients-and-fluxer-v2-feature-image-1280.jpg" => Some((
            "image/jpeg",
            include_bytes!("../static/blog/mobile-clients-and-fluxer-v2-feature-image-1280.jpg"),
        )),
        "mobile-clients-and-fluxer-v2-feature-image-1280.webp" => Some((
            "image/webp",
            include_bytes!("../static/blog/mobile-clients-and-fluxer-v2-feature-image-1280.webp"),
        )),
        "mobile-clients-and-fluxer-v2-feature-image-2000.avif" => Some((
            "image/avif",
            include_bytes!("../static/blog/mobile-clients-and-fluxer-v2-feature-image-2000.avif"),
        )),
        "mobile-clients-and-fluxer-v2-feature-image-2000.jpg" => Some((
            "image/jpeg",
            include_bytes!("../static/blog/mobile-clients-and-fluxer-v2-feature-image-2000.jpg"),
        )),
        "mobile-clients-and-fluxer-v2-feature-image-2000.webp" => Some((
            "image/webp",
            include_bytes!("../static/blog/mobile-clients-and-fluxer-v2-feature-image-2000.webp"),
        )),
        "roadmap-2026-feature-image-640.avif" => Some((
            "image/avif",
            include_bytes!("../static/blog/roadmap-2026-feature-image-640.avif"),
        )),
        "roadmap-2026-feature-image-640.jpg" => Some((
            "image/jpeg",
            include_bytes!("../static/blog/roadmap-2026-feature-image-640.jpg"),
        )),
        "roadmap-2026-feature-image-640.webp" => Some((
            "image/webp",
            include_bytes!("../static/blog/roadmap-2026-feature-image-640.webp"),
        )),
        "roadmap-2026-feature-image-960.avif" => Some((
            "image/avif",
            include_bytes!("../static/blog/roadmap-2026-feature-image-960.avif"),
        )),
        "roadmap-2026-feature-image-960.jpg" => Some((
            "image/jpeg",
            include_bytes!("../static/blog/roadmap-2026-feature-image-960.jpg"),
        )),
        "roadmap-2026-feature-image-960.webp" => Some((
            "image/webp",
            include_bytes!("../static/blog/roadmap-2026-feature-image-960.webp"),
        )),
        "roadmap-2026-feature-image-1280.avif" => Some((
            "image/avif",
            include_bytes!("../static/blog/roadmap-2026-feature-image-1280.avif"),
        )),
        "roadmap-2026-feature-image-1280.jpg" => Some((
            "image/jpeg",
            include_bytes!("../static/blog/roadmap-2026-feature-image-1280.jpg"),
        )),
        "roadmap-2026-feature-image-1280.webp" => Some((
            "image/webp",
            include_bytes!("../static/blog/roadmap-2026-feature-image-1280.webp"),
        )),
        "roadmap-2026-feature-image-2000.avif" => Some((
            "image/avif",
            include_bytes!("../static/blog/roadmap-2026-feature-image-2000.avif"),
        )),
        "roadmap-2026-feature-image-2000.jpg" => Some((
            "image/jpeg",
            include_bytes!("../static/blog/roadmap-2026-feature-image-2000.jpg"),
        )),
        "roadmap-2026-feature-image-2000.webp" => Some((
            "image/webp",
            include_bytes!("../static/blog/roadmap-2026-feature-image-2000.webp"),
        )),
        "tenor-delorean-poster.jpg" => Some((
            "image/jpeg",
            include_bytes!("../static/blog/tenor-delorean-poster.jpg"),
        )),
        "tenor-delorean.mp4" => Some((
            "video/mp4",
            include_bytes!("../static/blog/tenor-delorean.mp4"),
        )),
        "tenor-delorean.webm" => Some((
            "video/webm",
            include_bytes!("../static/blog/tenor-delorean.webm"),
        )),
        "tenor-cable-guy-well-look-who-decided-to-show-poster.jpg" => Some((
            "image/jpeg",
            include_bytes!(
                "../static/blog/tenor-cable-guy-well-look-who-decided-to-show-poster.jpg"
            ),
        )),
        "tenor-cable-guy-well-look-who-decided-to-show.mp4" => Some((
            "video/mp4",
            include_bytes!("../static/blog/tenor-cable-guy-well-look-who-decided-to-show.mp4"),
        )),
        "tenor-cable-guy-well-look-who-decided-to-show.webm" => Some((
            "video/webm",
            include_bytes!("../static/blog/tenor-cable-guy-well-look-who-decided-to-show.webm"),
        )),
        "tenor-freebie-poster.jpg" => Some((
            "image/jpeg",
            include_bytes!("../static/blog/tenor-freebie-poster.jpg"),
        )),
        "tenor-freebie.mp4" => Some((
            "video/mp4",
            include_bytes!("../static/blog/tenor-freebie.mp4"),
        )),
        "tenor-freebie.webm" => Some((
            "video/webm",
            include_bytes!("../static/blog/tenor-freebie.webm"),
        )),
        _ => None,
    }
}

async fn blog_rss(
    State(state): State<AppState>,
    headers: HeaderMap,
    uri: Uri,
) -> impl IntoResponse {
    let ctx = RequestContext::from_headers(&state, &headers, &uri);
    (
        [(header::CONTENT_TYPE, "application/rss+xml; charset=utf-8")],
        blog_rss_xml(&state.i18n, &ctx),
    )
}

async fn blog_atom(
    State(state): State<AppState>,
    headers: HeaderMap,
    uri: Uri,
) -> impl IntoResponse {
    let ctx = RequestContext::from_headers(&state, &headers, &uri);
    (
        [(header::CONTENT_TYPE, "application/atom+xml; charset=utf-8")],
        blog_atom_xml(&state.i18n, &ctx),
    )
}

fn blog_rss_xml(i18n: &MarketingI18n, ctx: &RequestContext) -> String {
    let base = ctx.base_url.trim_end_matches('/');
    let last_updated = latest_blog_updated_at();
    let items = BLOG_POSTS
        .iter()
        .map(|post| blog_rss_item(i18n, ctx.locale, base, *post))
        .collect::<Vec<_>>()
        .join("\n");
    format!(
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\
<rss version=\"2.0\" xmlns:atom=\"http://www.w3.org/2005/Atom\" xmlns:content=\"http://purl.org/rss/1.0/modules/content/\" xmlns:dc=\"http://purl.org/dc/elements/1.1/\" xmlns:media=\"http://search.yahoo.com/mrss/\">\n\
  <channel>\n\
    <title>{}</title>\n\
    <description>{}</description>\n\
    <link>{base}/blog</link>\n\
    <atom:link href=\"{base}/blog/rss.xml\" rel=\"self\" type=\"application/rss+xml\"/>\n\
    <lastBuildDate>{}</lastBuildDate>\n\
    <ttl>60</ttl>\n\
{items}\n\
  </channel>\n\
</rss>",
        xml_escape(&i18n.text(ctx.locale, BLOG_TITLE_DESCRIPTOR)),
        xml_escape(&i18n.text(ctx.locale, BLOG_DESCRIPTION_DESCRIPTOR)),
        rfc2822_date(last_updated),
    )
}

fn blog_rss_item(i18n: &MarketingI18n, locale: Locale, base: &str, post: BlogPost) -> String {
    let url = format!("{base}/blog/{}", post.slug);
    let copy_link_label = i18n.text(locale, NAVIGATION_COPY_LINK_TO_SECTION_DESCRIPTOR);
    let linked_article_label = i18n.text(locale, BLOG_LINKED_ARTICLE_DESCRIPTOR);
    let categories = post
        .tags
        .iter()
        .map(|tag| {
            format!(
                "      <category>{}</category>",
                xml_escape(&blog_tag_label(i18n, locale, tag))
            )
        })
        .collect::<Vec<_>>()
        .join("\n");
    format!(
        "    <item>\n\
      <title>{}</title>\n\
      <description>{}</description>\n\
      <link>{url}</link>\n\
      <guid isPermaLink=\"true\">{url}</guid>\n\
{categories}\n\
      <dc:creator>{}</dc:creator>\n\
      <pubDate>{}</pubDate>\n\
      <media:content url=\"{base}{}\" medium=\"image\"/>\n\
      <content:encoded><![CDATA[{}]]></content:encoded>\n\
    </item>",
        xml_escape(&i18n.text(locale, post.title)),
        xml_escape(&i18n.text(locale, post.description)),
        xml_escape(post.author),
        rfc2822_date(post.published_at),
        post.feature_image_path,
        cdata(
            render_blog_markdown_with_copy_label(
                post.body,
                base,
                &copy_link_label,
                &linked_article_label,
            )
            .into_string(),
        ),
    )
}

fn blog_atom_xml(i18n: &MarketingI18n, ctx: &RequestContext) -> String {
    let base = ctx.base_url.trim_end_matches('/');
    let last_updated = latest_blog_updated_at();
    let entries = BLOG_POSTS
        .iter()
        .map(|post| blog_atom_entry(i18n, ctx.locale, base, *post))
        .collect::<Vec<_>>()
        .join("\n");
    format!(
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\
<feed xmlns=\"http://www.w3.org/2005/Atom\">\n\
  <title>{}</title>\n\
  <subtitle>{}</subtitle>\n\
  <id>{base}/blog</id>\n\
  <link href=\"{base}/blog\"/>\n\
  <link rel=\"self\" type=\"application/atom+xml\" href=\"{base}/blog/atom.xml\"/>\n\
  <updated>{}</updated>\n\
{entries}\n\
</feed>",
        xml_escape(&i18n.text(ctx.locale, BLOG_TITLE_DESCRIPTOR)),
        xml_escape(&i18n.text(ctx.locale, BLOG_DESCRIPTION_DESCRIPTOR)),
        xml_escape(last_updated),
    )
}

fn blog_atom_entry(i18n: &MarketingI18n, locale: Locale, base: &str, post: BlogPost) -> String {
    let url = format!("{base}/blog/{}", post.slug);
    let copy_link_label = i18n.text(locale, NAVIGATION_COPY_LINK_TO_SECTION_DESCRIPTOR);
    let linked_article_label = i18n.text(locale, BLOG_LINKED_ARTICLE_DESCRIPTOR);
    let categories = post
        .tags
        .iter()
        .map(|tag| {
            format!(
                "    <category term=\"{}\" label=\"{}\"/>",
                xml_escape(&blog_tag_slug(tag)),
                xml_escape(&blog_tag_label(i18n, locale, tag))
            )
        })
        .collect::<Vec<_>>()
        .join("\n");
    let html = render_blog_markdown_with_copy_label(
        post.body,
        base,
        &copy_link_label,
        &linked_article_label,
    )
    .into_string();
    format!(
        "  <entry>\n\
    <title>{}</title>\n\
    <id>{url}</id>\n\
    <link href=\"{url}\"/>\n\
    <updated>{}</updated>\n\
    <published>{}</published>\n\
    <author><name>{}</name></author>\n\
    <summary>{}</summary>\n\
{categories}\n\
    <content type=\"html\">{}</content>\n\
  </entry>",
        xml_escape(&i18n.text(locale, post.title)),
        xml_escape(post.updated_at),
        xml_escape(post.published_at),
        xml_escape(post.author),
        xml_escape(&i18n.text(locale, post.description)),
        xml_escape(&html),
    )
}

fn latest_blog_updated_at() -> &'static str {
    BLOG_POSTS
        .iter()
        .map(|post| post.updated_at)
        .max()
        .unwrap_or("2026-01-01T00:00:00Z")
}

fn rfc2822_date(value: &str) -> String {
    let Ok(date) = OffsetDateTime::parse(value, &Rfc3339) else {
        return value.to_owned();
    };
    date.format(&Rfc2822).unwrap_or_else(|_| value.to_owned())
}

fn xml_escape(value: &str) -> String {
    let mut escaped = String::with_capacity(value.len());
    for ch in value.chars() {
        match ch {
            '&' => escaped.push_str("&amp;"),
            '<' => escaped.push_str("&lt;"),
            '>' => escaped.push_str("&gt;"),
            '"' => escaped.push_str("&quot;"),
            '\'' => escaped.push_str("&apos;"),
            _ => escaped.push(ch),
        }
    }
    escaped
}

fn cdata(value: String) -> String {
    value.replace("]]>", "]]]]><![CDATA[>")
}

async fn help_slug(
    State(state): State<AppState>,
    headers: HeaderMap,
    uri: Uri,
    Path(slug): Path<String>,
) -> Response {
    let ctx = RequestContext::from_headers(&state, &headers, &uri);
    match get_help_article(&slug) {
        Some(article) if article.slug == slug => {
            Html(templates::help_article_page(&state.i18n, &ctx, article).into_string())
                .into_response()
        }
        Some(article) => Redirect::permanent(&prepend_base_path(
            &state.config.base_path,
            &format!("/help/{}", article.slug),
        ))
        .into_response(),
        None => not_found(State(state), headers, uri).await.into_response(),
    }
}

async fn redirect_intercom_help_home(State(state): State<AppState>) -> Redirect {
    Redirect::permanent(&prepend_base_path(&state.config.base_path, "/help"))
}

async fn redirect_intercom_article(
    State(state): State<AppState>,
    headers: HeaderMap,
    uri: Uri,
    Path(slug): Path<String>,
) -> Response {
    match get_help_article(&slug) {
        Some(article) => Redirect::permanent(&prepend_base_path(
            &state.config.base_path,
            &format!("/help/{}", article.slug),
        ))
        .into_response(),
        None => not_found(State(state), headers, uri).await.into_response(),
    }
}

async fn redirect_intercom_collection(
    State(state): State<AppState>,
    Path(slug): Path<String>,
) -> Redirect {
    let anchor = help_collection_anchor(&slug);
    Redirect::permanent(&prepend_base_path(
        &state.config.base_path,
        &format!("/help{anchor}"),
    ))
}

async fn press_download(State(state): State<AppState>, Path(asset_id): Path<String>) -> Response {
    let asset = match asset_id.as_str() {
        "logo-white" => Some((
            "/marketing/branding/logo-white.svg",
            "fluxer-logo-white.svg",
        )),
        "logo-black" => Some((
            "/marketing/branding/logo-black.svg",
            "fluxer-logo-black.svg",
        )),
        "logo-color" => Some((
            "/marketing/branding/logo-color.svg",
            "fluxer-logo-color.svg",
        )),
        "symbol-white" => Some((
            "/marketing/branding/symbol-white.svg",
            "fluxer-symbol-white.svg",
        )),
        "symbol-black" => Some((
            "/marketing/branding/symbol-black.svg",
            "fluxer-symbol-black.svg",
        )),
        "symbol-color" => Some((
            "/marketing/branding/symbol-color.svg",
            "fluxer-symbol-color.svg",
        )),
        _ => None,
    };
    let Some((asset_path, filename)) = asset else {
        return StatusCode::NOT_FOUND.into_response();
    };
    let url = format!("{}{}", state.config.static_cdn_endpoint, asset_path);
    let Ok(upstream) = state.http_client.get(url).send().await else {
        return StatusCode::NOT_FOUND.into_response();
    };
    if !upstream.status().is_success() {
        return StatusCode::NOT_FOUND.into_response();
    }
    let content_type = upstream
        .headers()
        .get(reqwest::header::CONTENT_TYPE)
        .and_then(|value| value.to_str().ok())
        .map(str::to_owned);
    let content_length = upstream
        .headers()
        .get(reqwest::header::CONTENT_LENGTH)
        .and_then(|value| value.to_str().ok())
        .map(str::to_owned);
    let cache_control = upstream
        .headers()
        .get(reqwest::header::CACHE_CONTROL)
        .and_then(|value| value.to_str().ok())
        .map(str::to_owned);
    let Ok(body) = upstream.bytes().await else {
        return StatusCode::NOT_FOUND.into_response();
    };
    let mut response = body.into_response();
    if let Some(content_type) = content_type {
        if let Ok(value) = HeaderValue::from_str(&content_type) {
            response.headers_mut().insert(header::CONTENT_TYPE, value);
        }
    } else {
        response.headers_mut().insert(
            header::CONTENT_TYPE,
            HeaderValue::from_static("application/octet-stream"),
        );
    }
    if let Some(content_length) = content_length
        && let Ok(value) = HeaderValue::from_str(&content_length)
    {
        response.headers_mut().insert(header::CONTENT_LENGTH, value);
    }
    if let Some(cache_control) = cache_control
        && let Ok(value) = HeaderValue::from_str(&cache_control)
    {
        response.headers_mut().insert(header::CACHE_CONTROL, value);
    }
    response.headers_mut().insert(
        header::CONTENT_DISPOSITION,
        HeaderValue::from_str(&format!("attachment; filename=\"{filename}\""))
            .expect("valid content-disposition"),
    );
    response
}

fn looks_like_email(value: &str) -> bool {
    EmailAddress::is_valid(value.trim())
}

fn rate_limit_key(headers: &HeaderMap, config: &MarketingConfig) -> String {
    if config.trust_client_ip_header
        && let Some(ip) = headers
            .get(config.client_ip_header_name.as_str())
            .and_then(|value| value.to_str().ok())
            .and_then(|value| value.split(',').next())
            .map(str::trim)
            .filter(|value| !value.is_empty())
    {
        return ip.to_owned();
    }
    "unknown".to_owned()
}

fn too_many_requests_response() -> Response {
    let mut response = StatusCode::TOO_MANY_REQUESTS.into_response();
    response
        .headers_mut()
        .insert(header::RETRY_AFTER, HeaderValue::from_static("60"));
    response
}

fn is_htmx_request(headers: &HeaderMap) -> bool {
    headers
        .get("hx-request")
        .and_then(|value| value.to_str().ok())
        == Some("true")
}

fn donation_checkout_error_response(
    i18n: &MarketingI18n,
    ctx: &RequestContext,
    audience: templates::DonationAudience,
    audience_id: &str,
    error: &str,
    currency: Option<templates::DonationCurrency>,
    is_htmx: bool,
) -> Response {
    if is_htmx {
        return Html(
            templates::donation_checkout_error_fragment(i18n, ctx, audience, error, currency)
                .into_string(),
        )
        .into_response();
    }
    donation_error_redirect(audience_id, error, currency.map(|currency| currency.code()))
}

fn checkout_redirect_response(url: &str, is_htmx: bool) -> Response {
    if !is_htmx {
        return Redirect::to(url).into_response();
    }
    let mut response = StatusCode::OK.into_response();
    if let Ok(value) = HeaderValue::from_str(url) {
        response
            .headers_mut()
            .insert(HeaderName::from_static("hx-redirect"), value);
    }
    response
}

fn donation_error_redirect(audience: &str, error: &str, currency: Option<&str>) -> Response {
    let mut target = format!("/donate?type={audience}&error={error}");
    if let Some(currency) = currency {
        target.push_str("&currency=");
        target.push_str(&urlencoding::encode(currency));
    }
    Redirect::to(&target).into_response()
}

fn donation_manage_result_response(
    i18n: &MarketingI18n,
    ctx: &RequestContext,
    email: &str,
    alert: &str,
    is_htmx: bool,
) -> Response {
    if is_htmx {
        return Html(templates::manage_message_fragment(i18n, ctx, alert).into_string())
            .into_response();
    }
    donation_manage_redirect(email, alert)
}

fn donation_manage_redirect(email: &str, alert: &str) -> Response {
    let target = format!(
        "/donate/manage?alert={alert}&email={}",
        urlencoding::encode(email.trim())
    );
    Redirect::to(&target).into_response()
}

fn sanitize_redirect(value: &str) -> String {
    let trimmed = value.trim();
    if trimmed.is_empty()
        || trimmed.starts_with("//")
        || trimmed.contains('\n')
        || trimmed.contains('\r')
        || trimmed.contains('\\')
        || trimmed.starts_with("http://")
        || trimmed.starts_with("https://")
    {
        return "/".to_owned();
    }
    if trimmed.starts_with('/') {
        trimmed.to_owned()
    } else {
        format!("/{trimmed}")
    }
}

fn prepend_base_path(base_path: &str, path: &str) -> String {
    if base_path.is_empty() || path.starts_with(base_path) {
        path.to_owned()
    } else if path == "/" {
        base_path.to_owned()
    } else {
        format!("{}{}", base_path, path)
    }
}
