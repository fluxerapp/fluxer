// SPDX-License-Identifier: AGPL-3.0-or-later

use axum::{
    body::Body,
    http::{Request, StatusCode, header},
};
use base64::{Engine as _, engine::general_purpose::URL_SAFE_NO_PAD};
use fluxer_marketing::{
    build_router,
    config::{MarketingConfig, ReleaseChannel},
};
use http_body_util::BodyExt;
use std::collections::{BTreeMap, BTreeSet};
use tower::ServiceExt;

fn test_config() -> MarketingConfig {
    let mut config = MarketingConfig::from_env();
    config.env = fluxer_marketing::config::RuntimeEnv::Test;
    config.host = "127.0.0.1".to_owned();
    config.port = 0;
    config.secret_key_base = "test-secret".to_owned();
    config.marketing_endpoint = "https://fluxer.test".to_owned();
    config.base_path.clear();
    config.api_endpoint = "https://api.fluxer.test".to_owned();
    config.app_endpoint = "https://app.fluxer.test".to_owned();
    config.build_version = "test".to_owned();
    config.geoip_db_path.clear();
    config.trust_client_ip_header = false;
    config.client_ip_header_name = "x-real-ip".to_owned();
    config
}

#[tokio::test]
async fn canary_responses_send_robots_header_on_all_surfaces() {
    let mut config = test_config();
    config.release_channel = ReleaseChannel::Canary;
    let app = build_router(config);
    let expected = "noindex, nofollow, nosnippet, noimageindex, notranslate, max-snippet:0, max-image-preview:none, max-video-preview:0";

    for request in [
        Request::builder().uri("/").body(Body::empty()).unwrap(),
        Request::builder()
            .uri("/static/htmx.min.js")
            .body(Body::empty())
            .unwrap(),
        Request::builder()
            .uri("/")
            .header(header::HOST, "fluxer.gg")
            .body(Body::empty())
            .unwrap(),
        Request::builder()
            .uri("/missing")
            .body(Body::empty())
            .unwrap(),
    ] {
        let response = app.clone().oneshot(request).await.unwrap();
        assert_eq!(response.headers().get("x-robots-tag").unwrap(), expected);
    }
}

#[tokio::test]
async fn stable_responses_do_not_send_canary_robots_header() {
    let app = build_router(test_config());
    let response = app
        .oneshot(Request::builder().uri("/").body(Body::empty()).unwrap())
        .await
        .unwrap();
    assert!(!response.headers().contains_key("x-robots-tag"));
}

#[tokio::test]
async fn assetlinks_serves_android_login_credentials_association() {
    let app = build_router(test_config());
    let response = app
        .oneshot(
            Request::builder()
                .uri("/.well-known/assetlinks.json")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();

    assert_eq!(response.status(), StatusCode::OK);
    assert_eq!(
        response.headers().get(header::CACHE_CONTROL).unwrap(),
        "public, max-age=1800"
    );

    let body = response.into_body().collect().await.unwrap().to_bytes();
    let actual: serde_json::Value = serde_json::from_slice(&body).unwrap();
    assert_eq!(
        actual,
        serde_json::json!([
            {
                "relation": [
                    "delegate_permission/common.handle_all_urls",
                    "delegate_permission/common.get_login_creds"
                ],
                "target": {
                    "namespace": "android_app",
                    "package_name": "com.fluxer",
                    "sha256_cert_fingerprints": [
                        "91:E4:98:E1:B8:A6:C8:BA:99:41:5E:DB:29:78:29:6B:6C:58:BA:A5:E2:D2:A6:49:CE:C6:2D:A7:A8:29:C7:BC"
                    ]
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
        ])
    );
}

#[tokio::test]
async fn responses_send_security_headers_on_all_surfaces() {
    let app = build_router(test_config());
    for request in [
        Request::builder().uri("/").body(Body::empty()).unwrap(),
        Request::builder()
            .uri("/static/htmx.min.js")
            .body(Body::empty())
            .unwrap(),
        Request::builder()
            .uri("/")
            .header(header::HOST, "fluxer.gg")
            .body(Body::empty())
            .unwrap(),
        Request::builder()
            .uri("/missing")
            .body(Body::empty())
            .unwrap(),
    ] {
        let response = app.clone().oneshot(request).await.unwrap();
        assert_eq!(
            response
                .headers()
                .get(header::STRICT_TRANSPORT_SECURITY)
                .unwrap(),
            "max-age=31536000; includeSubDomains; preload"
        );
        assert_eq!(
            response
                .headers()
                .get(header::X_CONTENT_TYPE_OPTIONS)
                .unwrap(),
            "nosniff"
        );
        assert_eq!(
            response.headers().get(header::REFERRER_POLICY).unwrap(),
            "strict-origin-when-cross-origin"
        );
        assert_eq!(
            response.headers().get(header::X_FRAME_OPTIONS).unwrap(),
            "DENY"
        );
        let csp = response
            .headers()
            .get(header::CONTENT_SECURITY_POLICY)
            .unwrap()
            .to_str()
            .unwrap();
        assert!(csp.contains("script-src 'self' 'sha256-"));
        assert!(!csp.contains("script-src 'self' 'unsafe-inline'"));
        assert!(response.headers().contains_key("permissions-policy"));
    }
}

#[tokio::test]
async fn document_titles_use_marketing_title_patterns() {
    let app = build_router(test_config());

    let home = render_path(app.clone(), "/").await;
    assert_document_title(&home, "Fluxer - A chat app that puts you first");

    let download = render_path(app.clone(), "/download").await;
    assert_document_title(&download, "Download Fluxer | Fluxer");

    let response = app
        .oneshot(
            Request::builder()
                .uri("/missing")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(response.status(), StatusCode::NOT_FOUND);
    let body = response.into_body().collect().await.unwrap().to_bytes();
    let not_found = String::from_utf8(body.to_vec()).unwrap();
    assert_document_title(&not_found, "Page not found | Fluxer");
    assert!(!not_found.contains("Fluxer | Page not found"));
}

#[tokio::test]
async fn home_uses_accept_language_catalog() {
    let app = build_router(test_config());
    let response = app
        .oneshot(
            Request::builder()
                .uri("/")
                .header(header::ACCEPT_LANGUAGE, "fr-FR,fr;q=0.9")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(response.status(), StatusCode::OK);
    let body = response.into_body().collect().await.unwrap().to_bytes();
    let html = String::from_utf8(body.to_vec()).unwrap();
    assert!(html.contains("lang=\"fr\""));
    assert!(html.contains("Une appli de chat"));
    assert!(html.contains("/marketing/flags/1f1f8-1f1ea.svg"));
    assert!(!html.contains("/marketing/flags/se.svg"));
}

#[tokio::test]
async fn blog_post_under_base_path_prefixes_embedded_asset_urls() {
    let mut config = test_config();
    config.base_path = "/marketing".to_owned();
    let app = build_router(config);

    let html = render_path(app, "/blog/mobile-clients-and-fluxer-v2").await;
    assert!(
        html.contains(
            "poster=\"/marketing/blog/assets/tenor-cable-guy-well-look-who-decided-to-show-poster.jpg\""
        ),
        "video poster prefixed: {html}"
    );
    assert!(
        html.contains(
            "src=\"/marketing/blog/assets/tenor-cable-guy-well-look-who-decided-to-show.webm\""
        ),
        "video source prefixed: {html}"
    );
    assert!(
        html.contains(
            "/marketing/blog/assets/mobile-clients-and-fluxer-v2-feature-image-2000.avif 2000w"
        ),
        "feature srcset prefixed: {html}"
    );
    assert!(
        !html.contains("poster=\"/blog/assets/tenor-cable-guy"),
        "no unprefixed embed url: {html}"
    );
    assert!(
        !html.contains("/marketing/marketing"),
        "no double prefix: {html}"
    );
}

#[tokio::test]
async fn accept_language_prefers_exact_supported_locale_before_fallback() {
    let app = build_router(test_config());
    let response = app
        .oneshot(
            Request::builder()
                .uri("/")
                .header(header::ACCEPT_LANGUAGE, "fr-CA,de;q=0.9")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(response.status(), StatusCode::OK);
    let body = response.into_body().collect().await.unwrap().to_bytes();
    let html = String::from_utf8(body.to_vec()).unwrap();
    assert!(html.contains("lang=\"de\""), "{html}");
}

#[tokio::test]
async fn locale_route_sets_signed_cookie_and_redirects() {
    let app = build_router(test_config());
    let response = app
        .oneshot(
            Request::builder()
                .method("POST")
                .uri("/_locale")
                .header(header::CONTENT_TYPE, "application/x-www-form-urlencoded")
                .body(Body::from("locale=de&redirect=%2Fdownload"))
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(response.status(), StatusCode::SEE_OTHER);
    assert_eq!(
        response.headers().get(header::LOCATION).unwrap(),
        "/download"
    );
    let set_cookie = response
        .headers()
        .get(header::SET_COOKIE)
        .unwrap()
        .to_str()
        .unwrap();
    assert!(set_cookie.starts_with("locale="));
    let value = set_cookie
        .strip_prefix("locale=")
        .unwrap()
        .split(';')
        .next()
        .unwrap();
    let payload = value.split('.').next().unwrap();
    let bytes = URL_SAFE_NO_PAD.decode(payload).unwrap();
    let json: serde_json::Value = serde_json::from_slice(&bytes).unwrap();
    assert_eq!(json.get("locale").unwrap(), "de");
    assert!(
        json.get("createdAt")
            .and_then(|value| value.as_u64())
            .is_some()
    );
}

#[tokio::test]
async fn sitemap_contains_content_routes() {
    let app = build_router(test_config());
    let response = app
        .oneshot(
            Request::builder()
                .uri("/sitemap.xml")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(response.status(), StatusCode::OK);
    let body = response.into_body().collect().await.unwrap().to_bytes();
    let xml = String::from_utf8(body.to_vec()).unwrap();
    assert!(xml.contains("https://fluxer.test/privacy"));
    assert!(xml.contains("https://fluxer.test/help/report-bug"));
    assert!(xml.contains("https://fluxer.test/blog"));
    assert!(xml.contains("https://fluxer.test/blog/roadmap-2026"));
    assert!(xml.contains("https://fluxer.test/careers/product-engineer"));
}

#[tokio::test]
async fn help_center_serves_imported_articles_and_legacy_redirects() {
    let app = build_router(test_config());

    let help = render_path(app.clone(), "/help").await;
    assert!(help.contains("Help center"));
    assert!(help.contains("March 2026 Plutonium promotion"));
    assert!(help.contains("How to delete or disable your account"));
    assert!(help.contains("Legal &amp; Policy"));
    assert!(!help.contains("https://help.fluxer.app"));

    let search = render_path(app.clone(), "/help?q=regional").await;
    assert!(search.contains("Search results"));
    assert!(search.contains("Regional restrictions"));

    let article = render_path(app.clone(), "/help/report-bug").await;
    assert!(article.contains("Bug report template"));
    assert!(article.contains("heading-anchor-link"));
    assert!(article.contains("article:modified_time\" content=\"2026-03-07\""));
    assert!(!article.contains("https://help.fluxer.app"));

    let response = app
        .clone()
        .oneshot(
            Request::builder()
                .uri("/help/13984986-reporting-a-bug")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(response.status(), StatusCode::PERMANENT_REDIRECT);
    assert_eq!(
        response.headers().get(header::LOCATION).unwrap(),
        "/help/report-bug"
    );

    let response = app
        .clone()
        .oneshot(
            Request::builder()
                .uri("/en/articles/13984933-minimum-age-requirements")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(response.status(), StatusCode::PERMANENT_REDIRECT);
    assert_eq!(
        response.headers().get(header::LOCATION).unwrap(),
        "/help/minimum-age"
    );

    let response = app
        .oneshot(
            Request::builder()
                .uri("/en/collections/18821560-account")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(response.status(), StatusCode::PERMANENT_REDIRECT);
    assert_eq!(
        response.headers().get(header::LOCATION).unwrap(),
        "/help#account"
    );
}

#[tokio::test]
async fn help_host_only_redirects_to_canonical_marketing_help_routes() {
    let app = build_router(test_config());

    let response = app
        .clone()
        .oneshot(
            Request::builder()
                .uri("/")
                .header(header::HOST, "help.fluxer.app")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(response.status(), StatusCode::PERMANENT_REDIRECT);
    assert_eq!(
        response.headers().get(header::LOCATION).unwrap(),
        "https://fluxer.test/help"
    );

    let response = app
        .clone()
        .oneshot(
            Request::builder()
                .uri("/en/articles/13984933-minimum-age-requirements")
                .header(header::HOST, "help.fluxer.app")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(response.status(), StatusCode::PERMANENT_REDIRECT);
    assert_eq!(
        response.headers().get(header::LOCATION).unwrap(),
        "https://fluxer.test/help/minimum-age"
    );

    let response = app
        .clone()
        .oneshot(
            Request::builder()
                .uri("/help/13984986-reporting-a-bug")
                .header(header::HOST, "help.fluxer.app:443")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(response.status(), StatusCode::PERMANENT_REDIRECT);
    assert_eq!(
        response.headers().get(header::LOCATION).unwrap(),
        "https://fluxer.test/help/report-bug"
    );

    let response = app
        .clone()
        .oneshot(
            Request::builder()
                .uri("/en/collections/18821560-account")
                .header(header::HOST, "help.fluxer.app")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(response.status(), StatusCode::PERMANENT_REDIRECT);
    assert_eq!(
        response.headers().get(header::LOCATION).unwrap(),
        "https://fluxer.test/help#account"
    );

    let response = app
        .oneshot(
            Request::builder()
                .uri("/download")
                .header(header::HOST, "help.fluxer.app")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(response.status(), StatusCode::PERMANENT_REDIRECT);
    assert_eq!(
        response.headers().get(header::LOCATION).unwrap(),
        "https://fluxer.test/help"
    );
}

#[tokio::test]
async fn old_origin_marketing_redirects_are_preserved() {
    let app = build_router(test_config());

    let response = app
        .clone()
        .oneshot(
            Request::builder()
                .uri("/docs?ref=old")
                .header(header::HOST, "www.fluxer.app")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(response.status(), StatusCode::PERMANENT_REDIRECT);
    assert_eq!(
        response.headers().get(header::LOCATION).unwrap(),
        "https://fluxer.test/docs?ref=old"
    );

    let response = app
        .clone()
        .oneshot(
            Request::builder()
                .uri("/")
                .header(header::HOST, "fluxer.gg")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(response.status(), StatusCode::TEMPORARY_REDIRECT);
    assert_eq!(
        response.headers().get(header::LOCATION).unwrap(),
        "https://app.fluxer.test/invite/"
    );

    let response = app
        .clone()
        .oneshot(
            Request::builder()
                .uri("/guild")
                .header(header::HOST, "fluxer.gg")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(response.status(), StatusCode::TEMPORARY_REDIRECT);
    assert_eq!(
        response.headers().get(header::LOCATION).unwrap(),
        "https://app.fluxer.test/invite/guild"
    );

    let response = app
        .clone()
        .oneshot(
            Request::builder()
                .uri("/spring")
                .header(header::HOST, "fluxer.gift")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(response.status(), StatusCode::TEMPORARY_REDIRECT);
    assert_eq!(
        response.headers().get(header::LOCATION).unwrap(),
        "https://app.fluxer.test/gift/spring"
    );

    let response = app
        .clone()
        .oneshot(
            Request::builder()
                .uri("/install")
                .header(header::HOST, "fluxer.dev")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(response.status(), StatusCode::PERMANENT_REDIRECT);
    assert_eq!(
        response.headers().get(header::LOCATION).unwrap(),
        "https://docs.fluxer.app/install"
    );

    let response = app
        .clone()
        .oneshot(
            Request::builder()
                .uri("/")
                .header(header::HOST, "every.day.im.fluxer.ing")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(response.status(), StatusCode::TEMPORARY_REDIRECT);
    assert_eq!(
        response.headers().get(header::LOCATION).unwrap(),
        "https://www.youtube.com/watch?v=KQ6zr6kCPj8"
    );

    let response = app
        .clone()
        .oneshot(
            Request::builder()
                .uri("/channels/@me?source=old")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(response.status(), StatusCode::TEMPORARY_REDIRECT);
    assert_eq!(
        response.headers().get(header::LOCATION).unwrap(),
        "https://app.fluxer.test/channels/@me?source=old"
    );

    let response = app
        .clone()
        .oneshot(
            Request::builder()
                .uri("/delete-my-account")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(response.status(), StatusCode::TEMPORARY_REDIRECT);
    assert_eq!(
        response.headers().get(header::LOCATION).unwrap(),
        "/help/delete-account"
    );

    let response = app
        .oneshot(
            Request::builder()
                .uri("/.well-known/fluxer")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(response.status(), StatusCode::PERMANENT_REDIRECT);
    assert_eq!(
        response.headers().get(header::LOCATION).unwrap(),
        "https://api.fluxer.test/.well-known/fluxer"
    );
}

#[tokio::test]
async fn blog_serves_imported_posts_feeds_assets_and_legacy_redirects() {
    let app = build_router(test_config());

    let blog = render_path(app.clone(), "/blog").await;
    assert!(blog.contains("Fluxer Blog"));
    assert!(blog.contains("Mobile clients and Fluxer v2"));
    assert!(blog.contains("Roadmap 2026"));
    assert!(blog.contains("How I built Fluxer, a Discord-like chat app"));
    assert!(blog.contains("href=\"/blog/rss.xml\""));
    assert!(blog.contains("href=\"/blog/atom.xml\""));
    assert!(!blog.contains("https://blog.fluxer.app/rss/"));
    assert!(!blog.contains("min read"));

    let search = render_path(app.clone(), "/blog?q=federation").await;
    assert!(search.contains("Search results"));
    assert!(search.contains("Roadmap 2026"));
    assert!(search.contains("How I built Fluxer, a Discord-like chat app"));

    let tag = render_path(app.clone(), "/blog?tag=news").await;
    assert!(tag.contains("Filtered by News"));
    assert!(tag.contains("Mobile clients and Fluxer v2"));
    assert!(tag.contains("Roadmap 2026"));

    let mobile_article = render_path(app.clone(), "/blog/mobile-clients-and-fluxer-v2").await;
    assert!(mobile_article.contains("Hampus Kraft"));
    assert!(mobile_article.contains("/blog/assets/hampus-kraft-avatar.jpg"));
    assert!(mobile_article.contains(
        "og:image\" content=\"https://fluxer.test/blog/assets/mobile-clients-and-fluxer-v2-feature-image-1280.jpg\""
    ));
    assert!(
        mobile_article
            .contains("/blog/assets/mobile-clients-and-fluxer-v2-feature-image-2000.avif 2000w")
    );
    assert!(mobile_article.contains("Fluxer has grown to more than 300,000 users"));
    assert!(mobile_article.contains("https://github.com/fluxerapp/fluxer"));
    assert!(mobile_article.contains("https://github.com/fluxerapp/flutter_client"));
    assert!(mobile_article.contains("https://flathub.org/en/apps/app.fluxer.Fluxer"));
    assert!(
        mobile_article.contains("/blog/assets/tenor-cable-guy-well-look-who-decided-to-show.webm")
    );
    assert!(mobile_article.contains("https://docs.fluxer.app/operator/get-started/"));
    assert!(mobile_article.contains("Download Fluxer Canary"));
    assert!(mobile_article.contains("https://canary.fluxer.app/download"));
    assert!(mobile_article.contains("dozens of testers"));
    assert!(mobile_article.contains("native A/V architecture"));
    assert!(mobile_article.contains("1440p 60fps without audio or video frame drops"));
    assert!(mobile_article.contains("Fluxer Developers community"));
    assert!(mobile_article.contains("<del>Linux</del> Fluxer"));
    assert!(mobile_article.contains("instance moderation"));
    assert!(mobile_article.contains("$1,500 in developer bounties"));
    assert!(mobile_article.contains("kg-bookmark-card"));

    let article = render_path(app.clone(), "/blog/roadmap-2026").await;
    assert!(article.contains("article:published_time\" content=\"2026-01-26T12:49:48Z\""));
    assert!(article.contains("article:tag\" content=\"News\""));
    assert!(article.contains(
        "og:image\" content=\"https://fluxer.test/blog/assets/roadmap-2026-feature-image-1280.jpg\""
    ));
    assert!(article.contains("type=\"image/avif\""));
    assert!(article.contains("/blog/assets/roadmap-2026-feature-image-640.avif 640w"));
    assert!(article.contains("src=\"/blog/assets/roadmap-2026-feature-image-1280.jpg\""));
    assert!(!article.contains("src=\"https://fluxer.test/blog/assets/roadmap-2026-feature-image"));
    assert!(article.contains("href=\"/donate\""));
    assert!(!article.contains("href=\"https://fluxer.test/donate\""));
    assert!(!article.contains("this roadmap has been revised since the January beta"));
    assert!(!article.contains("min read"));
    assert!(article.contains("DeepFilterNet3"));
    assert!(article.contains("newer Rust services for users, messages, search, unfurling"));
    assert!(article.contains("custom backends through in-app account switching"));
    assert!(article.contains("simultaneous connections to multiple backends"));
    assert!(article.contains("$199 or €199"));
    assert!(article.contains("Free users get an allowance"));
    assert!(article.contains("https://fluxerstatus.com/cmpiwlw5e057vpbi74zh7ohmh"));
    assert!(article.contains("specialised tiers"));
    assert!(article.contains("/blog/assets/bookmark-fluxer-status-thumb.jpg"));
    assert!(article.contains("/blog/assets/bookmark-fluxer-status-icon.jpg"));
    assert!(article.contains("LiveKit-backed E2EE"));
    assert!(article.contains("GIFs are proxied through Fluxer"));
    assert!(article.contains("kg-bookmark-card"));
    assert!(article.contains("/blog/assets/bookmark-discord-age-verification-thumb.jpg"));
    assert!(!article.contains("blog-link-card"));
    assert!(article.contains("application/ld+json"));
    assert!(article.contains("heading-anchor-link"));

    let how_article = render_path(
        app.clone(),
        "/blog/how-i-built-fluxer-a-discord-like-chat-app",
    )
    .await;
    assert!(how_article.contains("blog-bsky-card"));
    assert!(how_article.contains("/blog/assets/bsky-jake-gold-avatar.jpg"));
    assert!(!how_article.contains("blog-bsky-brand"));
    assert!(!how_article.contains("blog-bsky-meta"));
    assert!(!how_article.contains("GitHub Sponsors"));
    assert!(!how_article.contains("github.com/sponsors"));
    assert!(how_article.contains("What the backend looks like now"));
    assert!(how_article.contains("Operator Pass"));
    assert!(how_article.contains("custom backends in the Electron desktop app"));
    assert!(how_article.contains("simultaneous connections to multiple backends"));
    assert!(how_article.contains("30 June 2026"));
    assert!(how_article.contains("/blog/assets/discord-ui-revolutionary-640.avif 640w"));
    assert!(how_article.contains("/blog/assets/discord-ui-revolutionary-1280.png"));
    assert!(how_article.contains(
        "Source: <a href=\"https://imgur.com/whenever-someone-says-discords-ui-is-revolutionary-b5kdlfM\" target=\"_blank\" rel=\"noopener noreferrer\">Imgur</a>"
    ));
    assert!(!how_article.contains("/blog/assets/discord-ui-revolutionary.png"));
    assert!(how_article.contains("/blog/assets/tenor-freebie.webm"));
    assert!(how_article.contains("/blog/assets/tenor-delorean.webm"));
    assert!(how_article.contains("/blog/assets/erlang-the-movie.mp4"));
    assert!(!how_article.contains("https://embed.bsky.app/static/embed.js"));
    assert!(!how_article.contains("https://tenor.com/embed/"));
    assert!(!how_article.contains("https://www.youtube.com/embed/"));

    let rss = app
        .clone()
        .oneshot(
            Request::builder()
                .uri("/blog/rss.xml")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(rss.status(), StatusCode::OK);
    assert_eq!(
        rss.headers().get(header::CONTENT_TYPE).unwrap(),
        "application/rss+xml; charset=utf-8"
    );
    let body = rss.into_body().collect().await.unwrap().to_bytes();
    let rss_xml = String::from_utf8(body.to_vec()).unwrap();
    assert!(rss_xml.contains("<title>Fluxer Blog</title>"));
    assert!(rss_xml.contains("<link>https://fluxer.test/blog/mobile-clients-and-fluxer-v2</link>"));
    assert!(rss_xml.contains("<link>https://fluxer.test/blog/roadmap-2026</link>"));
    assert!(rss_xml.contains("<dc:creator>Hampus Kraft</dc:creator>"));
    assert!(rss_xml.contains("<content:encoded><![CDATA["));
    assert!(rss_xml.contains("https://fluxer.test/blog/assets/bookmark-"));
    assert!(!rss_xml.contains("src=\"/blog/assets/bookmark-"));

    let atom = app
        .clone()
        .oneshot(
            Request::builder()
                .uri("/blog/atom.xml")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(atom.status(), StatusCode::OK);
    assert_eq!(
        atom.headers().get(header::CONTENT_TYPE).unwrap(),
        "application/atom+xml; charset=utf-8"
    );
    let body = atom.into_body().collect().await.unwrap().to_bytes();
    let atom_xml = String::from_utf8(body.to_vec()).unwrap();
    assert!(atom_xml.contains("<feed xmlns=\"http://www.w3.org/2005/Atom\">"));
    assert!(atom_xml.contains("<id>https://fluxer.test/blog/mobile-clients-and-fluxer-v2</id>"));
    assert!(atom_xml.contains("<id>https://fluxer.test/blog/roadmap-2026</id>"));

    let asset = app
        .clone()
        .oneshot(
            Request::builder()
                .uri("/blog/assets/roadmap-2026-feature-image-1280.avif")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(asset.status(), StatusCode::OK);
    assert_eq!(
        asset.headers().get(header::CONTENT_TYPE).unwrap(),
        "image/avif"
    );
    assert_eq!(
        asset.headers().get(header::CACHE_CONTROL).unwrap(),
        "public, max-age=31536000, immutable"
    );
    assert_eq!(asset.headers().get(header::ACCEPT_RANGES).unwrap(), "bytes");

    let mobile_asset = app
        .clone()
        .oneshot(
            Request::builder()
                .uri("/blog/assets/mobile-clients-and-fluxer-v2-feature-image-2000.avif")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(mobile_asset.status(), StatusCode::OK);
    assert_eq!(
        mobile_asset.headers().get(header::CONTENT_TYPE).unwrap(),
        "image/avif"
    );

    let mobile_video_asset = app
        .clone()
        .oneshot(
            Request::builder()
                .uri("/blog/assets/tenor-cable-guy-well-look-who-decided-to-show.webm")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(mobile_video_asset.status(), StatusCode::OK);
    assert_eq!(
        mobile_video_asset
            .headers()
            .get(header::CONTENT_TYPE)
            .unwrap(),
        "video/webm"
    );

    let mobile_video_poster_asset = app
        .clone()
        .oneshot(
            Request::builder()
                .uri("/blog/assets/tenor-cable-guy-well-look-who-decided-to-show-poster.jpg")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(mobile_video_poster_asset.status(), StatusCode::OK);
    assert_eq!(
        mobile_video_poster_asset
            .headers()
            .get(header::CONTENT_TYPE)
            .unwrap(),
        "image/jpeg"
    );

    let mobile_video_mp4_asset = app
        .clone()
        .oneshot(
            Request::builder()
                .uri("/blog/assets/tenor-cable-guy-well-look-who-decided-to-show.mp4")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(mobile_video_mp4_asset.status(), StatusCode::OK);
    assert_eq!(
        mobile_video_mp4_asset
            .headers()
            .get(header::CONTENT_TYPE)
            .unwrap(),
        "video/mp4"
    );

    let discord_ui_asset = app
        .clone()
        .oneshot(
            Request::builder()
                .uri("/blog/assets/discord-ui-revolutionary-640.avif")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(discord_ui_asset.status(), StatusCode::OK);
    assert_eq!(
        discord_ui_asset
            .headers()
            .get(header::CONTENT_TYPE)
            .unwrap(),
        "image/avif"
    );

    let legacy_asset = app
        .clone()
        .oneshot(
            Request::builder()
                .uri("/blog/assets/roadmap-2026-feature-image.png")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(legacy_asset.status(), StatusCode::PERMANENT_REDIRECT);
    assert_eq!(
        legacy_asset.headers().get(header::LOCATION).unwrap(),
        "/blog/assets/roadmap-2026-feature-image-1280.jpg"
    );

    let mobile_legacy_asset = app
        .clone()
        .oneshot(
            Request::builder()
                .uri("/blog/assets/mobile-clients-and-fluxer-v2-feature-image.png")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(mobile_legacy_asset.status(), StatusCode::PERMANENT_REDIRECT);
    assert_eq!(
        mobile_legacy_asset.headers().get(header::LOCATION).unwrap(),
        "/blog/assets/mobile-clients-and-fluxer-v2-feature-image-1280.jpg"
    );

    let range = app
        .clone()
        .oneshot(
            Request::builder()
                .uri("/blog/assets/erlang-the-movie.mp4")
                .header(header::RANGE, "bytes=0-15")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(range.status(), StatusCode::PARTIAL_CONTENT);
    assert_eq!(
        range.headers().get(header::CONTENT_TYPE).unwrap(),
        "video/mp4"
    );
    assert_eq!(range.headers().get(header::CONTENT_LENGTH).unwrap(), "16");
    assert!(
        range
            .headers()
            .get(header::CONTENT_RANGE)
            .unwrap()
            .to_str()
            .unwrap()
            .starts_with("bytes 0-15/")
    );

    let response = app
        .clone()
        .oneshot(Request::builder().uri("/rss").body(Body::empty()).unwrap())
        .await
        .unwrap();
    assert_eq!(response.status(), StatusCode::PERMANENT_REDIRECT);
    assert_eq!(
        response.headers().get(header::LOCATION).unwrap(),
        "/blog/rss.xml"
    );

    let response = app
        .clone()
        .oneshot(
            Request::builder()
                .uri("/blog/tag/news")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(response.status(), StatusCode::PERMANENT_REDIRECT);
    assert_eq!(
        response.headers().get(header::LOCATION).unwrap(),
        "/blog?tag=news"
    );
}

#[tokio::test]
async fn blog_host_only_redirects_to_canonical_marketing_blog_routes() {
    let app = build_router(test_config());

    let response = app
        .clone()
        .oneshot(
            Request::builder()
                .uri("/")
                .header(header::HOST, "blog.fluxer.app")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(response.status(), StatusCode::PERMANENT_REDIRECT);
    assert_eq!(
        response.headers().get(header::LOCATION).unwrap(),
        "https://fluxer.test/blog"
    );

    let response = app
        .clone()
        .oneshot(
            Request::builder()
                .uri("/roadmap-2026/")
                .header(header::HOST, "blog.fluxer.app")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(response.status(), StatusCode::PERMANENT_REDIRECT);
    assert_eq!(
        response.headers().get(header::LOCATION).unwrap(),
        "https://fluxer.test/blog/roadmap-2026"
    );

    let response = app
        .clone()
        .oneshot(
            Request::builder()
                .uri("/rss/")
                .header(header::HOST, "blog.fluxer.app")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(response.status(), StatusCode::PERMANENT_REDIRECT);
    assert_eq!(
        response.headers().get(header::LOCATION).unwrap(),
        "https://fluxer.test/blog/rss.xml"
    );

    let response = app
        .clone()
        .oneshot(
            Request::builder()
                .uri("/tag/news/")
                .header(header::HOST, "blog.fluxer.app")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(response.status(), StatusCode::PERMANENT_REDIRECT);
    assert_eq!(
        response.headers().get(header::LOCATION).unwrap(),
        "https://fluxer.test/blog?tag=news"
    );

    let response = app
        .oneshot(
            Request::builder()
                .uri("/content/images/2026/04/cover.png")
                .header(header::HOST, "blog.fluxer.app")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(response.status(), StatusCode::PERMANENT_REDIRECT);
    assert_eq!(
        response.headers().get(header::LOCATION).unwrap(),
        "https://fluxer.test/blog/assets/how-i-built-fluxer-cover-1280.jpg"
    );
}

#[tokio::test]
async fn download_page_renders_strips_and_cache_header() {
    let mut config = test_config();
    config.api_endpoint = "http://127.0.0.1:9".to_owned();
    let release_channel = config.release_channel;
    let expected_download_url = format!(
        "/dl/desktop/{}/win32/x64/latest/setup?test=1",
        release_channel.segment()
    );
    let expected_other_arch_url = format!(
        "/dl/desktop/{}/win32/arm64/latest/setup?test=1",
        release_channel.segment()
    );
    let app = build_router(config);
    let response = app
        .oneshot(
            Request::builder()
                .uri("/download?test=1")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(response.status(), StatusCode::OK);
    assert_eq!(
        response.headers().get(header::CACHE_CONTROL).unwrap(),
        "public, max-age=60, stale-while-revalidate=300"
    );
    let body = response.into_body().collect().await.unwrap().to_bytes();
    let html = String::from_utf8(body.to_vec()).unwrap();
    assert!(!html.contains("download-overlay"));
    assert!(!html.contains("download-button-group"));
    assert!(!html.contains("popovertarget=\"dl-"));
    assert!(!html.contains("download-card-grid"));
    assert!(!html.contains("/dl/desktop/source/latest"));
    assert!(html.contains(&expected_download_url));
    assert!(html.contains(&expected_other_arch_url));
    if release_channel.is_canary() {
        assert!(html.contains("/dl/desktop/canary/linux/x64/latest/appimage?test=1"));
    } else {
        assert!(html.contains("https://flathub.org/en/apps/app.fluxer.Fluxer"));
    }
}

#[tokio::test]
async fn download_page_uses_channel_api_endpoint_fallback_when_configured_endpoint_is_empty() {
    let mut config = test_config();
    config.api_endpoint.clear();
    config.release_channel = ReleaseChannel::Canary;
    let app = build_router(config);
    let response = app
        .oneshot(
            Request::builder()
                .uri("/download?test=1")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(response.status(), StatusCode::OK);
    let body = response.into_body().collect().await.unwrap().to_bytes();
    let html = String::from_utf8(body.to_vec()).unwrap();
    assert!(html.contains(
        "https://api.canary.fluxer.app/dl/desktop/canary/linux/x64/latest/appimage?test=1"
    ));
}

#[tokio::test]
async fn donation_page_renders_business_tab_and_swish_proxy_rejects_bad_amount() {
    let app = build_router(test_config());
    let response = app
        .clone()
        .oneshot(
            Request::builder()
                .uri("/donate?type=business")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(response.status(), StatusCode::OK);
    let body = response.into_body().collect().await.unwrap().to_bytes();
    let html = String::from_utf8(body.to_vec()).unwrap();
    assert!(html.contains("id=\"donate-content-business\" class=\"donate-content w-full\""));
    assert!(html.contains("/_donations/checkout"));
    assert!(html.contains("hx-post=\"/_donations/checkout\""));
    assert!(html.contains("hx-target=\"#donation-error-business\""));
    assert!(html.contains("<script src=\"/static/htmx.min.js?v=test\" defer>"));
    assert!(!html.contains("unpkg.com/htmx.org"));
    assert!(html.contains("hx-select=\"#donation-interaction\""));
    assert!(html.contains("hx-target=\"#donation-interaction\""));
    assert!(html.contains("hx-push-url=\"true\""));
    assert!(html.contains("id=\"donation-amount-fieldset-business\""));
    assert!(html.contains("hx-get=\"/_donations/amounts?audience=business&amp;currency=eur\""));
    assert!(html.contains("hx-target=\"#donation-amount-fieldset-business\""));
    assert!(html.contains("hx-get=\"/_swish/payment\""));
    assert!(html.contains("hx-target=\"#swish-payment-fragment\""));
    assert!(html.contains("hx-swap=\"outerHTML\""));
    assert!(html.contains("/_donations/request-link"));
    assert!(html.contains("hx-post=\"/_donations/request-link\""));
    assert!(html.contains("hx-target=\"#manage-message\""));
    assert!(html.contains("/_swish/qr"));
    assert!(!html.contains("name=\"swish\" value=\"1\""));
    assert!(html.contains("id=\"swish-donate-link\""));
    assert!(html.contains("href=\"#swish-modal-backdrop\""));
    assert!(html.contains("href=\"#_\""));
    assert!(html.contains("swish-grad-1"));

    let response = app
        .clone()
        .oneshot(
            Request::builder()
                .method("POST")
                .uri("/_donations/checkout")
                .header(header::CONTENT_TYPE, "application/x-www-form-urlencoded")
                .body(Body::from(
                    "audience=individual&email=invalid&amount_major=25&interval=once&currency=usd",
                ))
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(response.status(), StatusCode::SEE_OTHER);
    assert_eq!(
        response.headers().get(header::LOCATION).unwrap(),
        "/donate?type=individual&error=invalid_email"
    );

    let response = app
        .clone()
        .oneshot(
            Request::builder()
                .method("POST")
                .uri("/_donations/checkout")
                .header(header::CONTENT_TYPE, "application/x-www-form-urlencoded")
                .header("HX-Request", "true")
                .body(Body::from(
                    "audience=individual&email=invalid&amount_major=25&interval=once&currency=usd",
                ))
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(response.status(), StatusCode::OK);
    assert!(!response.headers().contains_key(header::LOCATION));
    let body = response.into_body().collect().await.unwrap().to_bytes();
    let fragment = String::from_utf8(body.to_vec()).unwrap();
    assert!(fragment.contains("id=\"donation-error-individual\""));
    assert!(fragment.contains("role=\"alert\""));
    assert!(fragment.contains("Please enter a valid email address"));

    let response = app
        .clone()
        .oneshot(
            Request::builder()
                .method("POST")
                .uri("/_donations/request-link")
                .header(header::CONTENT_TYPE, "application/x-www-form-urlencoded")
                .header("HX-Request", "true")
                .body(Body::from("email=invalid"))
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(response.status(), StatusCode::OK);
    assert!(!response.headers().contains_key(header::LOCATION));
    let body = response.into_body().collect().await.unwrap().to_bytes();
    let fragment = String::from_utf8(body.to_vec()).unwrap();
    assert!(fragment.contains("id=\"manage-message\""));
    assert!(fragment.contains("role=\"alert\""));
    assert!(fragment.contains("Please enter a valid email address"));

    let checkout_api = axum::Router::new().route(
        "/donations/checkout",
        axum::routing::post(|| async {
            axum::Json(serde_json::json!({
                "url": "https://checkout.fluxer.test/session"
            }))
        }),
    );
    let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
    let api_endpoint = format!("http://{}", listener.local_addr().unwrap());
    let server = tokio::spawn(async move {
        axum::serve(listener, checkout_api).await.unwrap();
    });
    let mut config = test_config();
    config.api_endpoint = api_endpoint;
    let checkout_app = build_router(config);
    let response = checkout_app
        .oneshot(
            Request::builder()
                .method("POST")
                .uri("/_donations/checkout")
                .header(header::CONTENT_TYPE, "application/x-www-form-urlencoded")
                .header("HX-Request", "true")
                .body(Body::from(
                    "audience=individual&email=test%40example.com&amount_major=25&interval=once&currency=usd",
                ))
                .unwrap(),
        )
        .await
        .unwrap();
    server.abort();
    assert_eq!(response.status(), StatusCode::OK);
    assert_eq!(
        response.headers().get("hx-redirect").unwrap(),
        "https://checkout.fluxer.test/session"
    );

    let response = app
        .oneshot(
            Request::builder()
                .uri("/_swish/qr?amount=0")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(response.status(), StatusCode::BAD_REQUEST);

    let app = build_router(test_config());
    let response = app
        .oneshot(
            Request::builder()
                .uri("/static/htmx.min.js")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(response.status(), StatusCode::OK);
    assert_eq!(
        response.headers().get(header::CONTENT_TYPE).unwrap(),
        "application/javascript; charset=utf-8"
    );

    let app = build_router(test_config());
    let response = app
        .clone()
        .oneshot(
            Request::builder()
                .uri("/_donations/amounts?audience=individual&currency=inr")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(response.status(), StatusCode::OK);
    let body = response.into_body().collect().await.unwrap().to_bytes();
    let fragment = String::from_utf8(body.to_vec()).unwrap();
    assert!(fragment.contains("id=\"donation-amount-fieldset-individual\""));
    assert!(fragment.contains("value=\"1000\" checked"));
    assert!(fragment.contains("min=\"500\" max=\"100000\""));
    assert!(fragment.contains("Minimum donation:"));

    let response = app
        .oneshot(
            Request::builder()
                .uri("/_donations/amounts?audience=individual&currency=btc")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(response.status(), StatusCode::BAD_REQUEST);
}

#[tokio::test]
async fn product_program_and_press_pages_are_not_placeholders() {
    let app = build_router(test_config());
    for (path, expected) in [
        ("/plutonium", "Free vs Plutonium"),
        ("/partners", "Partner perks"),
        ("/press", "logo-color.svg"),
    ] {
        let response = app
            .clone()
            .oneshot(Request::builder().uri(path).body(Body::empty()).unwrap())
            .await
            .unwrap();
        assert_eq!(response.status(), StatusCode::OK);
        let body = response.into_body().collect().await.unwrap().to_bytes();
        let html = String::from_utf8(body.to_vec()).unwrap();
        assert!(html.contains(expected), "{path} did not contain {expected}");
    }

    let plutonium = render_path(app, "/plutonium").await;
    for expected in [
        "<table",
        "--fluxer-premium-inner: #4641D9;",
        "Most popular",
        "Custom 4-digit username tag",
        "Message character limit",
        "Emoji and sticker packs",
        "Saved media",
        "Video quality",
        "Animated avatars and banners",
        "Custom themes",
        "25 MB",
        "500 MB",
        "Free self-hosting",
        "Operator Pass",
        "One-time purchase",
        "Everything in Free, plus:",
    ] {
        assert!(
            plutonium.contains(expected),
            "/plutonium did not contain {expected}"
        );
    }
}

#[tokio::test]
async fn rendered_pages_keep_no_js_and_accessibility_contracts() {
    let mut config = test_config();
    config.api_endpoint = "http://127.0.0.1:9".to_owned();
    let app = build_router(config);
    let paths = [
        "/",
        "/download?test=1",
        "/donate?type=business",
        "/donate?type=individual&error=invalid_amount&currency=usd",
        "/donate?swish=1&swish_amount=75",
        "/donate/manage?alert=sent&email=test%40example.com",
        "/privacy",
        "/help",
        "/help/report-bug",
        "/blog",
        "/blog/roadmap-2026",
        "/careers",
        "/partners",
        "/press",
        "/plutonium",
        "/does-not-exist",
    ];

    for path in paths {
        let response = app
            .clone()
            .oneshot(Request::builder().uri(path).body(Body::empty()).unwrap())
            .await
            .unwrap();
        assert!(
            [StatusCode::OK, StatusCode::NOT_FOUND].contains(&response.status()),
            "{path} returned {}",
            response.status()
        );
        let body = response.into_body().collect().await.unwrap().to_bytes();
        let html = String::from_utf8(body.to_vec()).unwrap();
        if path == "/privacy" || path.starts_with("/help/") {
            assert_policy_client_js(path, &html);
        } else if path.starts_with("/blog/") {
            assert_blog_article_client_js(path, &html);
        } else if path.starts_with("/donate/manage") {
            assert_donation_manage_htmx_contract(path, &html);
        } else if path.starts_with("/donate?") {
            assert_donation_htmx_contract(path, &html);
        } else {
            assert_no_client_js(path, &html);
        }
        if path == "/" {
            assert_voice_region_map_tappable_contract(&html);
        }
        assert_no_duplicate_ids(path, &html);
        assert_aria_references_exist(path, &html);
        assert_all_images_have_alt(path, &html);
        assert_blank_targets_are_safe(path, &html);
        assert!(
            html.contains(
                "role=\"dialog\" aria-modal=\"true\" aria-labelledby=\"locale-modal-title\""
            ),
            "{path} is missing the locale dialog accessibility contract"
        );
    }

    let download = render_path(app.clone(), "/download?test=1").await;
    assert!(
        download
            .contains("role=\"dialog\" aria-modal=\"true\" aria-labelledby=\"pwa-modal-title\"")
    );
    assert!(download.contains("<fieldset class=\"contents\">"));
    assert!(download.contains("aria-controls=\"pwa-panel-android\""));
    assert!(download.contains("aria-controls=\"pwa-panel-ios\""));
    assert!(download.contains("aria-controls=\"pwa-panel-desktop\""));

    let donation_error = render_path(
        app.clone(),
        "/donate?type=individual&error=invalid_amount&currency=usd",
    )
    .await;
    assert!(donation_error.matches("<fieldset").count() >= 3);
    assert!(donation_error.matches("<legend").count() >= 3);
    assert!(donation_error.contains("role=\"alert\""));
    assert!(donation_error.contains("aria-live=\"polite\""));
    assert!(donation_error.contains("aria-current=\"page\""));

    let swish_modal = render_path(app.clone(), "/donate?swish=1&swish_amount=75").await;
    assert!(!swish_modal.contains("pwa-modal-backdrop-open"));
    assert!(swish_modal.contains("href=\"#swish-modal-backdrop\""));
    assert!(swish_modal.contains("href=\"#_\""));
    assert!(swish_modal.contains("M208.49,191.51"));
    assert!(swish_modal.contains("id=\"swish-grad-1-donate-link\""));
    assert!(swish_modal.contains("/_swish/qr?amount=75"));

    let mobile_swish_modal = app
        .oneshot(
            Request::builder()
                .uri("/donate?swish_amount=75")
                .header(
                    header::USER_AGENT,
                    "Mozilla/5.0 (iPhone; CPU iPhone OS 17_0 like Mac OS X)",
                )
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    let body = mobile_swish_modal
        .into_body()
        .collect()
        .await
        .unwrap()
        .to_bytes();
    let mobile_html = String::from_utf8(body.to_vec()).unwrap();
    assert!(mobile_html.contains("Open Swish"));
    assert!(mobile_html.contains("swish://payment?data="));
    assert!(!mobile_html.contains("/_swish/qr?amount=75"));
}

async fn render_path(app: axum::Router, path: &str) -> String {
    let response = app
        .oneshot(Request::builder().uri(path).body(Body::empty()).unwrap())
        .await
        .unwrap();
    let body = response.into_body().collect().await.unwrap().to_bytes();
    String::from_utf8(body.to_vec()).unwrap()
}

fn assert_document_title(html: &str, expected: &str) {
    assert!(
        html.contains(&format!("<title>{expected}</title>")),
        "document title {expected:?} was not rendered"
    );
    assert!(
        html.contains(&format!("property=\"og:title\" content=\"{expected}\"")),
        "OpenGraph title {expected:?} was not rendered"
    );
    assert!(
        html.contains(&format!("name=\"twitter:title\" content=\"{expected}\"")),
        "Twitter title {expected:?} was not rendered"
    );
}

fn assert_no_client_js(path: &str, html: &str) {
    for needle in [
        "<script",
        "hx-",
        "javascript:",
        " onclick=",
        " onchange=",
        " oninput=",
        " onmouseover=",
        " onsubmit=",
    ] {
        assert!(
            !html.contains(needle),
            "{path} contains client-side JS marker {needle:?}"
        );
    }
}

fn assert_voice_region_map_tappable_contract(html: &str) {
    assert!(html.contains("<div class=\"voice-region-pin absolute h-6 w-6 rounded-full\""));
    assert!(
        html.contains("<button id=\"voice-region-pin-sea\" type=\"button\" class=\"voice-region-pin-target\" aria-label=\"Seattle (SEA)\" aria-describedby=\"voice-region-pin-sea-tooltip\">")
    );
    assert!(html.contains("id=\"voice-region-pin-sea-tooltip\" role=\"tooltip\""));
    assert!(html.contains("class=\"voice-region-tooltip-label font-semibold text-sm\""));
    assert!(html.contains("class=\"voice-region-pin-dot block h-full w-full"));
    assert_eq!(
        html.matches("rel=\"preload\" as=\"image\" href=\"/static/voice-region-flags/")
            .count(),
        12
    );
    assert!(html.contains(
        "rel=\"preload\" as=\"image\" href=\"/static/voice-region-flags/1f1fa-1f1f8.svg\""
    ));
    assert!(html.contains("src=\"/static/voice-region-flags/1f1fa-1f1f8.svg\" alt=\"\" aria-hidden=\"true\" loading=\"eager\" decoding=\"async\""));
    assert!(!html.contains("<details class=\"voice-region-pin"));
    assert!(!html.contains("src=\"/static/voice-region-flags/1f1fa-1f1f8.svg\" alt=\"\" aria-hidden=\"true\" loading=\"lazy\""));
}

fn assert_donation_htmx_contract(path: &str, html: &str) {
    for needle in [
        "<script src=\"/static/htmx.min.js?v=test\"",
        "hx-post=\"/_donations/checkout\"",
        "hx-target=\"#donation-error-individual\"",
        "hx-select=\"#donation-interaction\"",
        "hx-target=\"#donation-interaction\"",
        "hx-push-url=\"true\"",
        "id=\"donation-amount-fieldset-individual\"",
        "hx-get=\"/_donations/amounts?audience=individual&amp;currency=eur\"",
        "hx-trigger=\"change\"",
        "hx-target=\"#donation-amount-fieldset-individual\"",
        "hx-params=\"none\"",
        "hx-get=\"/_swish/payment\"",
        "hx-target=\"#swish-payment-fragment\"",
        "hx-swap=\"outerHTML\"",
        "hx-push-url=\"false\"",
        "hx-post=\"/_donations/request-link\"",
        "hx-target=\"#manage-message\"",
    ] {
        assert!(
            html.contains(needle),
            "{path} is missing donation HTMX contract {needle:?}"
        );
    }
    for forbidden in [
        "javascript:",
        " onclick=",
        " onchange=",
        " oninput=",
        " onmouseover=",
        " onsubmit=",
    ] {
        assert!(
            !html.contains(forbidden),
            "{path} contains unsafe JS marker {forbidden:?}"
        );
    }
}

fn assert_donation_manage_htmx_contract(path: &str, html: &str) {
    for needle in [
        "<script src=\"/static/htmx.min.js?v=test\"",
        "hx-post=\"/_donations/request-link\"",
        "hx-target=\"#manage-message\"",
        "hx-swap=\"outerHTML\"",
        "hx-push-url=\"false\"",
    ] {
        assert!(
            html.contains(needle),
            "{path} is missing manage HTMX contract {needle:?}"
        );
    }
    for forbidden in [
        "javascript:",
        " onclick=",
        " onchange=",
        " oninput=",
        " onmouseover=",
        " onsubmit=",
    ] {
        assert!(
            !html.contains(forbidden),
            "{path} contains unsafe JS marker {forbidden:?}"
        );
    }
}

fn assert_blog_article_client_js(path: &str, html: &str) {
    for needle in [
        "<script",
        "heading-anchor-link",
        "data-anchor-link=",
        "navigator.clipboard.writeText",
    ] {
        assert!(
            html.contains(needle),
            "{path} is missing blog markdown script contract {needle:?}"
        );
    }
    assert!(
        !html.contains("id=\"policy-toc\""),
        "{path} should not render a table of contents"
    );
    for forbidden in [
        "javascript:",
        " onclick=",
        " onchange=",
        " oninput=",
        " onmouseover=",
        " onsubmit=",
    ] {
        assert!(
            !html.contains(forbidden),
            "{path} contains unsafe JS marker {forbidden:?}"
        );
    }
}

fn assert_policy_client_js(path: &str, html: &str) {
    for needle in [
        "<script",
        "id=\"policy-toc\" class=\"hidden lg:block\"",
        "heading-anchor-link",
        "data-anchor-link=",
        "navigator.clipboard.writeText",
    ] {
        assert!(
            html.contains(needle),
            "{path} is missing policy markdown script contract {needle:?}"
        );
    }
    for forbidden in [
        "javascript:",
        " onclick=",
        " onchange=",
        " oninput=",
        " onmouseover=",
        " onsubmit=",
    ] {
        assert!(
            !html.contains(forbidden),
            "{path} contains unsafe JS marker {forbidden:?}"
        );
    }
}

fn assert_no_duplicate_ids(path: &str, html: &str) {
    let mut counts = BTreeMap::<String, usize>::new();
    for id in attribute_values(html, "id") {
        *counts.entry(id).or_default() += 1;
    }
    let duplicates = counts
        .into_iter()
        .filter(|(_, count)| *count > 1)
        .collect::<Vec<_>>();
    assert!(
        duplicates.is_empty(),
        "{path} has duplicate ids: {duplicates:?}"
    );
}

fn assert_aria_references_exist(path: &str, html: &str) {
    let ids = attribute_values(html, "id")
        .into_iter()
        .collect::<BTreeSet<_>>();
    for attr in ["aria-controls", "aria-describedby", "aria-labelledby"] {
        for value in attribute_values(html, attr) {
            for id in value.split_whitespace() {
                assert!(
                    ids.contains(id),
                    "{path} has {attr} reference to missing id {id:?}"
                );
            }
        }
    }
}

fn assert_all_images_have_alt(path: &str, html: &str) {
    let mut rest = html;
    while let Some(offset) = rest.find("<img") {
        let after_start = &rest[offset..];
        let Some(end) = after_start.find('>') else {
            panic!("{path} has an unterminated img tag");
        };
        let tag = &after_start[..end];
        assert!(tag.contains(" alt=\""), "{path} has img without alt: {tag}");
        rest = &after_start[end + 1..];
    }
}

fn assert_blank_targets_are_safe(path: &str, html: &str) {
    let mut rest = html;
    while let Some(offset) = rest.find("<a ") {
        let after_start = &rest[offset..];
        let Some(end) = after_start.find('>') else {
            panic!("{path} has an unterminated anchor tag");
        };
        let tag = &after_start[..end];
        if tag.contains("target=\"_blank\"") {
            assert!(
                tag.contains("rel=\"noopener noreferrer\""),
                "{path} has target=_blank without noopener noreferrer: {tag}"
            );
        }
        rest = &after_start[end + 1..];
    }
}

fn attribute_values(html: &str, name: &str) -> Vec<String> {
    let needle = format!("{name}=\"");
    let mut values = Vec::new();
    let mut rest = html;
    while let Some(offset) = rest.find(&needle) {
        let value_start = offset + needle.len();
        let after_start = &rest[value_start..];
        let Some(value_end) = after_start.find('"') else {
            break;
        };
        values.push(after_start[..value_end].to_owned());
        rest = &after_start[value_end + 1..];
    }
    values
}
