// SPDX-License-Identifier: AGPL-3.0-or-later

use super::*;
use crate::{
    config::Config,
    output_format::OutputFormat,
    server::asset_path::parse_standard_asset_path,
    storage::{Object, StorageError},
};
use axum::{body::to_bytes, http::header};
use bytes::Bytes;
use std::time::{Duration, Instant};

const ASSET_TEST_PATH: &str = "/avatars/123456789012345678/a1b2c3d4e5f6.png";

fn test_app_state(cfg: Config) -> Arc<AppState> {
    Arc::new(AppState::for_tests(cfg))
}

fn asset_test_config(storage_root: &std::path::Path) -> Config {
    Config::load_from_iter([
        (
            "FLUXER_MEDIA_PROXY_SECRET_KEY".to_owned(),
            "secret".to_owned(),
        ),
        ("FLUXER_MEDIA_PROXY_MODE".to_owned(), "mp".to_owned()),
        (
            "FLUXER_MEDIA_PROXY_STORAGE_BACKEND".to_owned(),
            "local".to_owned(),
        ),
        (
            "FLUXER_MEDIA_PROXY_STORAGE_ROOT".to_owned(),
            storage_root.display().to_string(),
        ),
    ])
    .unwrap()
}

fn asset_source_object() -> Object {
    Object {
        data: Bytes::from_static(b"\x89PNG\r\n\x1a\noriginal-asset-bytes"),
        content_type: "image/png".to_owned(),
    }
}

fn asset_transform_failure(error: CoalescerError) -> Response {
    asset_transform_failure_response(AssetTransformFailure {
        method: Method::GET,
        error,
        object: asset_source_object(),
        range_header: None,
        source_format: AssetExtension::Png,
        detail: "asset key=avatars/1/abc".to_owned(),
    })
}

async fn response_body(response: Response) -> Bytes {
    to_bytes(response.into_body(), usize::MAX)
        .await
        .expect("response body")
}

fn test_transform_cache() -> crate::transform_cache::TransformCache {
    let cfg = Config::load_from_iter([(
        "FLUXER_MEDIA_PROXY_SECRET_KEY".to_owned(),
        "secret".to_owned(),
    )])
    .expect("config loads");
    AppState::for_tests(cfg).media.transforms().cache().clone()
}

#[test]
fn asset_size_query_is_clamped_by_kind() {
    let params = HashMap::from([("size".to_owned(), "4096".to_owned())]);
    let size = asset_size::parse_image_size(params.get("size").map(String::as_str));
    let selected = output_format::select_url_variant(output_format::Input {
        kind: AssetKind::Avatar,
        original: AssetExtension::Webp,
        requested_size: Some(size),
        manual_format_override: asset_manual_format_override(&params, AssetExtension::Webp),
    });
    assert_eq!(Some(1024), selected.size);
}

#[tokio::test]
async fn a_native_allocation_failure_still_serves_the_original_image() {
    let error = test_transform_cache()
        .get_or_run(
            "asset:alloc".to_owned(),
            OutputFormat::WebP,
            None,
            || async {
                Err(anyhow::Error::new(
                    media_process::MediaError::AllocationFailed,
                ))
            },
        )
        .await
        .expect_err("the transform fails");
    assert_eq!(CoalescerError::AllocationFailed, error);
    let response = asset_transform_failure(error);
    assert_eq!(StatusCode::OK, response.status());
    assert_eq!(
        Some("image/png"),
        response
            .headers()
            .get(header::CONTENT_TYPE)
            .and_then(|value| value.to_str().ok())
    );
    assert_eq!(asset_source_object().data, response_body(response).await);
}

#[tokio::test]
async fn a_cancelled_transform_leader_still_serves_the_original_image() {
    let cache = test_transform_cache();
    let leader_cache = cache.clone();
    let leader = tokio::spawn(async move {
        let _ = leader_cache
            .get_or_run(
                "asset:cancelled".to_owned(),
                OutputFormat::WebP,
                None,
                || async {
                    tokio::time::sleep(Duration::from_secs(60)).await;
                    Ok(media_process::MediaBytes::from(vec![0u8; 4]))
                },
            )
            .await;
    });
    tokio::time::sleep(Duration::from_millis(20)).await;
    let waiter_cache = cache.clone();
    let waiter = tokio::spawn(async move {
        waiter_cache
            .get_or_run(
                "asset:cancelled".to_owned(),
                OutputFormat::WebP,
                Some(Instant::now() + Duration::from_secs(60)),
                || async { Ok(media_process::MediaBytes::from(vec![0u8; 4])) },
            )
            .await
    });
    tokio::time::sleep(Duration::from_millis(20)).await;
    leader.abort();
    let _ = leader.await;
    let error = waiter
        .await
        .expect("waiter task")
        .expect_err("the cancelled leader publishes a failure");
    assert_eq!(CoalescerError::WorkCancelled, error);
    let response = asset_transform_failure(error);
    assert_eq!(StatusCode::OK, response.status());
    assert_eq!(asset_source_object().data, response_body(response).await);
}

#[test]
fn a_coalescer_timeout_is_never_degraded_to_the_original_image() {
    let response = asset_transform_failure(CoalescerError::RequestTimeout);
    assert_eq!(StatusCode::GATEWAY_TIMEOUT, response.status());
}

#[tokio::test]
async fn an_asset_image_transform_is_served_and_then_reused_from_its_content_identity_key() {
    let tmp = tempfile::tempdir().unwrap();
    let storage_root = tmp.path().canonicalize().unwrap();
    let app = test_app_state(asset_test_config(&storage_root));
    let asset = parse_standard_asset_path(ASSET_TEST_PATH).unwrap();
    app.store
        .write_object(
            &app.cfg.storage.bucket_cdn,
            &asset.storage_key,
            &crate::test_fixtures::synthetic_png(512, 512),
            "image/png",
        )
        .await
        .unwrap();
    let params = HashMap::new();
    for _ in 0..2 {
        let response = serve_asset_image(
            &app,
            Method::GET,
            parse_standard_asset_path(ASSET_TEST_PATH).unwrap(),
            &params,
            &HeaderMap::new(),
        )
        .await;
        assert_eq!(StatusCode::OK, response.status());
        assert_eq!(
            Some("image/png"),
            response
                .headers()
                .get(header::CONTENT_TYPE)
                .and_then(|value| value.to_str().ok())
        );
        assert!(!response_body(response).await.is_empty());
    }
    let rendered = app.metrics.render();
    assert!(
        rendered.contains("fluxer_media_proxy_transform_cache_hits_total 1\n"),
        "the second request reuses the cached transform"
    );
    assert!(rendered.contains("fluxer_media_proxy_transform_cache_misses_total 1\n"));
    assert!(
        rendered.contains("fluxer_media_proxy_storage_hits_total 2\n"),
        "each asset request reads its source exactly once and never heads it first"
    );
    app.store
        .write_object(
            &app.cfg.storage.bucket_cdn,
            &asset.storage_key,
            &crate::test_fixtures::synthetic_png(256, 256),
            "image/png",
        )
        .await
        .unwrap();
    let response = serve_asset_image(
        &app,
        Method::GET,
        parse_standard_asset_path(ASSET_TEST_PATH).unwrap(),
        &params,
        &HeaderMap::new(),
    )
    .await;
    assert_eq!(StatusCode::OK, response.status());
    assert!(
        app.metrics
            .render()
            .contains("fluxer_media_proxy_transform_cache_misses_total 2\n"),
        "rewritten source bytes take a new cache key"
    );
}

#[tokio::test]
async fn an_asset_read_that_races_a_rewrite_is_refused_rather_than_cached() {
    let tmp = tempfile::tempdir().unwrap();
    let storage_root = tmp.path().canonicalize().unwrap();
    let app = test_app_state(asset_test_config(&storage_root));
    let asset = parse_standard_asset_path(ASSET_TEST_PATH).unwrap();
    app.store
        .write_object(
            &app.cfg.storage.bucket_cdn,
            &asset.storage_key,
            &crate::test_fixtures::synthetic_png(512, 512),
            "image/png",
        )
        .await
        .unwrap();
    let head = app
        .store
        .head_object(&app.cfg.storage.bucket_cdn, &asset.storage_key)
        .await
        .unwrap();
    app.store
        .write_object(
            &app.cfg.storage.bucket_cdn,
            &asset.storage_key,
            &crate::test_fixtures::synthetic_png(256, 256),
            "image/png",
        )
        .await
        .unwrap();
    let read_budget = storage::unversioned_read_budget(constants::MAX_MEDIA_PROXY_BYTES);
    let error = app
        .store
        .read_object_versioned(storage::ObjectReadRequest {
            bucket: &app.cfg.storage.bucket_cdn,
            key: &asset.storage_key,
            max_bytes: constants::MAX_MEDIA_PROXY_BYTES,
            budget: &read_budget,
            expected_identity: &head.identity,
        })
        .await
        .expect_err("the rewritten object no longer matches its head");
    assert!(matches!(error, StorageError::ObjectChanged));
}

#[test]
fn a_transform_failure_on_a_source_the_browser_cannot_show_stays_an_error() {
    let svg_failure = |error| {
        asset_transform_failure_response(AssetTransformFailure {
            method: Method::GET,
            error,
            object: Object {
                data: Bytes::from_static(b"<svg xmlns=\"http://www.w3.org/2000/svg\"/>"),
                content_type: "image/svg+xml".to_owned(),
            },
            range_header: None,
            source_format: AssetExtension::Svg,
            detail: "asset key=avatars/1/abc".to_owned(),
        })
    };
    assert_eq!(
        StatusCode::INTERNAL_SERVER_ERROR,
        svg_failure(CoalescerError::WorkFailed).status()
    );
    assert_eq!(
        StatusCode::SERVICE_UNAVAILABLE,
        svg_failure(CoalescerError::AllocationFailed).status()
    );
}

#[tokio::test]
async fn a_stored_passthrough_stream_declares_the_length_it_serves() {
    let tmp = tempfile::tempdir().unwrap();
    let storage_root = tmp.path().canonicalize().unwrap();
    let app = test_app_state(asset_test_config(&storage_root));
    app.store
        .write_object(
            &app.cfg.storage.bucket_cdn,
            "a/b.txt",
            b"hello",
            "text/plain",
        )
        .await
        .unwrap();
    let response = passthrough::serve_stored_raw(
        &app,
        Method::GET,
        &app.cfg.storage.bucket_cdn,
        "a/b.txt",
        &HeaderMap::new(),
    )
    .await;
    assert_eq!(StatusCode::OK, response.status());
    assert_eq!(
        Some("5"),
        response
            .headers()
            .get(header::CONTENT_LENGTH)
            .and_then(|value| value.to_str().ok())
    );
    assert_eq!(&b"hello"[..], &response_body(response).await[..]);
}

fn fake_asset_object(read_status: Option<u16>) -> crate::storage::tests::FakeObject {
    crate::storage::tests::FakeObject {
        body: crate::test_fixtures::synthetic_png(512, 512),
        etag: Some("\"asset-v1\"".to_owned()),
        content_type: Some("image/png".to_owned()),
        read_status,
        ..crate::storage::tests::FakeObject::default()
    }
}

#[tokio::test]
async fn an_asset_image_reads_its_source_once_and_never_pins_it() {
    let fake = crate::storage::tests::fake_s3().await;
    let tmp = tempfile::tempdir().unwrap();
    let mut cfg = fake.config(tmp.path());
    cfg.media.transform_timeout_ms = 60_000;
    let app = test_app_state(cfg);
    let asset = parse_standard_asset_path(ASSET_TEST_PATH).unwrap();
    fake.put_object(
        &format!("{}/{}", app.cfg.storage.bucket_cdn, asset.storage_key),
        fake_asset_object(None),
    );
    let response =
        serve_asset_image(&app, Method::GET, asset, &HashMap::new(), &HeaderMap::new()).await;
    assert_eq!(StatusCode::OK, response.status());
    let requests = fake.requests();
    assert_eq!(
        1,
        requests.len(),
        "the asset image path costs exactly one storage operation"
    );
    assert_eq!(Method::GET, requests[0].0);
    assert!(
        requests[0].2.get(header::IF_MATCH).is_none(),
        "an asset read is never pinned to an identity resolved by a separate request"
    );
}

#[tokio::test]
async fn an_asset_image_source_that_vanishes_mid_request_is_not_found() {
    let fake = crate::storage::tests::fake_s3().await;
    let tmp = tempfile::tempdir().unwrap();
    let app = test_app_state(fake.config(tmp.path()));
    let asset = parse_standard_asset_path(ASSET_TEST_PATH).unwrap();
    fake.put_object(
        &format!("{}/{}", app.cfg.storage.bucket_cdn, asset.storage_key),
        fake_asset_object(Some(404)),
    );
    let response =
        serve_asset_image(&app, Method::GET, asset, &HashMap::new(), &HeaderMap::new()).await;
    assert_eq!(StatusCode::NOT_FOUND, response.status());
}

fn avatar_cache_key_for_requested_size(raw: &str) -> String {
    let size = asset_size::parse_image_size(Some(raw));
    let selected = output_format::select_url_variant(output_format::Input {
        kind: AssetKind::Avatar,
        original: AssetExtension::Webp,
        requested_size: Some(size),
        manual_format_override: None,
    });
    transform_cache_key(TransformCacheKeyInput {
        route: TransformRoute::Asset,
        asset_kind: Some(AssetKind::Avatar),
        cache_identity: "avatars/852813040100737024/hash",
        width: selected.size,
        height: selected.size,
        format: selected.format,
        quality: Some(ImageQuality::High),
        animated: false,
        effort: None,
        resize_mode: Some(ResizeMode::Fit),
    })
}

#[test]
fn requested_sizes_off_the_ladder_share_the_cache_key_of_the_rung_they_snap_to() {
    let canonical = avatar_cache_key_for_requested_size("1024");
    assert_eq!(
        "asset:avatars/852813040100737024/hash|asset_kind=avatar|w=1024|h=1024|fmt=webp|q=high|anim=false|effort=none|resize=fit",
        canonical
    );
    for raw in ["641", "700", "1000", "1023", "1024"] {
        assert_eq!(
            canonical,
            avatar_cache_key_for_requested_size(raw),
            "size={raw} minted a second cache key"
        );
    }
    let floor = avatar_cache_key_for_requested_size("128");
    assert_eq!(
        "asset:avatars/852813040100737024/hash|asset_kind=avatar|w=128|h=128|fmt=webp|q=high|anim=false|effort=none|resize=fit",
        floor
    );
    for raw in ["1", "17", "20", "100", "128"] {
        assert_eq!(
            floor,
            avatar_cache_key_for_requested_size(raw),
            "size={raw} minted a second cache key"
        );
    }
}

#[test]
fn requested_sizes_on_different_rungs_keep_distinct_cache_keys() {
    assert_ne!(
        avatar_cache_key_for_requested_size("300"),
        avatar_cache_key_for_requested_size("512")
    );
    assert_ne!(
        avatar_cache_key_for_requested_size("300"),
        avatar_cache_key_for_requested_size("1000")
    );
}

#[tokio::test]
async fn asset_sources_that_share_bytes_but_not_their_stored_format_keep_distinct_entries() {
    let fake = crate::storage::tests::fake_s3().await;
    let tmp = tempfile::tempdir().unwrap();
    let mut cfg = fake.config(tmp.path());
    cfg.media.transform_timeout_ms = 60_000;
    cfg.media.transform_cache_capacity_bytes = 1 << 20;
    cfg.media.transform_cache_max_entry_bytes = 1 << 20;
    cfg.media.transform_cache_ttl_ms = 60_000;
    let app = test_app_state(cfg);
    let shared_bytes = crate::test_fixtures::synthetic_png(512, 512);
    let gif_sourced =
        parse_standard_asset_path("/avatars/123456789012345678/a1b2c3d4e5f6.webp").unwrap();
    let png_sourced =
        parse_standard_asset_path("/avatars/123456789012345679/a1b2c3d4e5f6.webp").unwrap();
    for (asset, content_type) in [(&gif_sourced, "image/gif"), (&png_sourced, "image/png")] {
        fake.put_object(
            &format!("{}/{}", app.cfg.storage.bucket_cdn, asset.storage_key),
            crate::storage::tests::FakeObject {
                body: shared_bytes.clone(),
                etag: Some("\"asset-v1\"".to_owned()),
                content_type: Some(content_type.to_owned()),
                ..crate::storage::tests::FakeObject::default()
            },
        );
    }
    let params = HashMap::from([("animated".to_owned(), "true".to_owned())]);
    let gif_response =
        serve_asset_image(&app, Method::GET, gif_sourced, &params, &HeaderMap::new()).await;
    assert_eq!(StatusCode::OK, gif_response.status());
    assert_eq!(
        Some("image/gif"),
        gif_response
            .headers()
            .get(header::CONTENT_TYPE)
            .and_then(|value| value.to_str().ok())
    );
    assert!(
        response_body(gif_response).await.starts_with(b"GIF8"),
        "an image/gif source encodes to gif once animation is asked for"
    );
    let webp_response =
        serve_asset_image(&app, Method::GET, png_sourced, &params, &HeaderMap::new()).await;
    assert_eq!(StatusCode::OK, webp_response.status());
    assert_eq!(
        Some("image/webp"),
        webp_response
            .headers()
            .get(header::CONTENT_TYPE)
            .and_then(|value| value.to_str().ok()),
        "identical source bytes stored under a different format never share one entry"
    );
    assert!(response_body(webp_response).await.starts_with(b"RIFF"));
}

#[tokio::test]
async fn an_explicit_quality_reuses_the_entry_the_resolved_default_minted() {
    let tmp = tempfile::tempdir().unwrap();
    let storage_root = tmp.path().canonicalize().unwrap();
    let app = test_app_state(asset_test_config(&storage_root));
    let asset = parse_standard_asset_path(ASSET_TEST_PATH).unwrap();
    app.store
        .write_object(
            &app.cfg.storage.bucket_cdn,
            &asset.storage_key,
            &crate::test_fixtures::synthetic_png(512, 512),
            "image/png",
        )
        .await
        .unwrap();
    for params in [
        HashMap::new(),
        HashMap::from([("quality".to_owned(), "high".to_owned())]),
    ] {
        let response = serve_asset_image(
            &app,
            Method::GET,
            parse_standard_asset_path(ASSET_TEST_PATH).unwrap(),
            &params,
            &HeaderMap::new(),
        )
        .await;
        assert_eq!(StatusCode::OK, response.status());
    }
    let rendered = app.metrics.render();
    assert!(
        rendered.contains("fluxer_media_proxy_transform_cache_hits_total 1\n"),
        "quality=high resolves to the entry the default request already minted"
    );
    assert!(rendered.contains("fluxer_media_proxy_transform_cache_misses_total 1\n"));
}

fn cache_control_of(response: &Response) -> &str {
    response
        .headers()
        .get(header::CACHE_CONTROL)
        .expect("cache-control is always set")
        .to_str()
        .expect("cache-control is ASCII")
}

#[test]
fn stored_media_responses_cache_forever() {
    let stored = media_response(MediaResponse {
        method: Method::GET,
        data: Bytes::from_static(b"stored bytes").into(),
        content_type: "image/webp",
        range_header: None,
        disposition: None,
    });
    assert_eq!("public, max-age=31536000", cache_control_of(&stored));

    let streamable = media_response(MediaResponse {
        method: Method::GET,
        data: Bytes::from_static(b"stored bytes").into(),
        content_type: "video/mp4",
        range_header: None,
        disposition: None,
    });
    assert_eq!(
        "public, max-age=31536000, no-transform",
        cache_control_of(&streamable)
    );

    let head = super::response::passthrough_head_response("image/webp", 12, None, None);
    assert_eq!("public, max-age=31536000", cache_control_of(&head));
}
