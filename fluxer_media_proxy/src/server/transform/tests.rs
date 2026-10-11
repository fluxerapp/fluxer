// SPDX-License-Identifier: AGPL-3.0-or-later

use super::*;
use crate::{
    constants::AssetExtension,
    server::format_policy::{default_transform_quality, transform_static_quality_default},
    test_fixtures::{
        JXL_64_LOSSY_ALPHA, JXL_4100_LOSSY_ALPHA, JXL_7000X4000_LOSSY_RGB, fixture_h264_mp4,
        fixture_jpeg, synthetic_png,
    },
};
use axum::body::to_bytes;

const TRANSFORM_WIDTH: u32 = 32;

fn test_runtime() -> TransformRuntime {
    let cfg = Config::load_from_iter([(
        "FLUXER_MEDIA_PROXY_SECRET_KEY".to_owned(),
        "secret".to_owned(),
    )])
    .expect("config loads");
    TransformRuntime::new(&cfg, &Arc::new(metrics::Metrics::new()))
        .expect("test transform runtime limits are valid")
}

fn attachment_cache_key(
    filename: &str,
    quality: ImageQuality,
    resize_mode: Option<ResizeMode>,
) -> String {
    transform_cache_key(TransformCacheKeyInput {
        route: TransformRoute::Attachment,
        asset_kind: None,
        cache_identity: filename,
        width: Some(TRANSFORM_WIDTH),
        height: None,
        format: OutputFormat::WebP,
        quality: Some(quality),
        animated: false,
        effort: None,
        resize_mode,
    })
}

async fn transform_qualities_reaching_the_encoder(
    fixture: Vec<u8>,
    content_type: &str,
    filename: &str,
) -> Vec<ImageQuality> {
    let runtime = test_runtime();
    let params = HashMap::from([
        ("format".to_owned(), "webp".to_owned()),
        ("width".to_owned(), TRANSFORM_WIDTH.to_string()),
    ]);
    let headers = HeaderMap::new();
    let response = serve_bytes_or_transform(
        &runtime,
        ServeBytesRequest {
            method: Method::GET,
            data: Bytes::from(fixture),
            content_type: content_type.to_owned(),
            cache_identity: filename,
            filename,
            route: TransformRoute::Attachment,
            params: &params,
            headers: &headers,
        },
    )
    .await;
    assert_eq!(
        StatusCode::OK,
        response.status(),
        "{filename} did not reach the transform path"
    );
    [
        ImageQuality::Lossless,
        ImageQuality::High,
        ImageQuality::Auto,
        ImageQuality::Low,
    ]
    .into_iter()
    .filter(|quality| {
        [None, Some(ResizeMode::Fit)]
            .into_iter()
            .any(|resize_mode| {
                runtime
                    .cache()
                    .get(&attachment_cache_key(filename, *quality, resize_mode))
                    .is_some()
            })
    })
    .collect()
}

#[test]
fn lossy_source_transform_defaults_to_lossy_output() {
    for lossy in [
        AssetExtension::Jpeg,
        AssetExtension::Heic,
        AssetExtension::Heif,
        AssetExtension::Jxl,
    ] {
        assert_eq!(
            ImageQuality::High,
            default_transform_quality(
                OutputFormat::WebP,
                false,
                transform_static_quality_default(Some(lossy))
            ),
            "{} source must not be re-encoded losslessly",
            lossy.name()
        );
    }
    for lossless in [
        AssetExtension::Png,
        AssetExtension::Apng,
        AssetExtension::Gif,
        AssetExtension::Avif,
        AssetExtension::Webp,
    ] {
        assert_eq!(
            ImageQuality::Lossless,
            default_transform_quality(
                OutputFormat::WebP,
                false,
                transform_static_quality_default(Some(lossless))
            ),
            "{} source must keep the lossless default",
            lossless.name()
        );
    }
    assert_eq!(
        ImageQuality::Lossless,
        default_transform_quality(
            OutputFormat::WebP,
            false,
            transform_static_quality_default(None)
        )
    );
    assert_eq!(
        ImageQuality::Auto,
        default_transform_quality(
            OutputFormat::WebP,
            true,
            transform_static_quality_default(Some(AssetExtension::Jpeg))
        )
    );
}

#[tokio::test]
async fn image_transform_route_sends_lossy_sources_to_a_lossy_encode() {
    assert_eq!(
        vec![ImageQuality::High],
        transform_qualities_reaching_the_encoder(fixture_jpeg(), "image/jpeg", "photo.jpg").await,
        "a jpeg attachment must not be re-encoded losslessly"
    );
    assert_eq!(
        vec![ImageQuality::High],
        transform_qualities_reaching_the_encoder(
            JXL_64_LOSSY_ALPHA.to_vec(),
            "image/jxl",
            "photo.jxl"
        )
        .await,
        "a jxl attachment must not be re-encoded losslessly"
    );
    assert_eq!(
        vec![ImageQuality::Lossless],
        transform_qualities_reaching_the_encoder(synthetic_png(48, 48), "image/png", "art.png")
            .await,
        "a png attachment must keep the lossless encode"
    );
}

#[tokio::test]
async fn video_poster_transform_route_sends_decoded_frames_to_a_lossy_encode() {
    assert_eq!(
        vec![ImageQuality::High],
        transform_qualities_reaching_the_encoder(fixture_h264_mp4(), "video/mp4", "clip.mp4").await,
        "a decoded video frame is already lossy and must not be re-encoded losslessly"
    );
}

#[tokio::test]
async fn a_redundant_format_parameter_shares_the_implicit_transform_cache_entry() {
    const KEY: &str = "attachment:art.png|asset_kind=not-applicable|w=32|h=none|fmt=png|q=lossless|anim=false|effort=none|resize=fit";
    for params in [
        HashMap::from([("width".to_owned(), TRANSFORM_WIDTH.to_string())]),
        HashMap::from([
            ("width".to_owned(), TRANSFORM_WIDTH.to_string()),
            ("format".to_owned(), "png".to_owned()),
        ]),
    ] {
        let runtime = test_runtime();
        let headers = HeaderMap::new();
        let response = serve_bytes_or_transform(
            &runtime,
            ServeBytesRequest {
                method: Method::GET,
                data: Bytes::from(synthetic_png(48, 48)),
                content_type: "image/png".to_owned(),
                cache_identity: "art.png",
                filename: "art.png",
                route: TransformRoute::Attachment,
                params: &params,
                headers: &headers,
            },
        )
        .await;
        assert_eq!(StatusCode::OK, response.status());
        assert!(
            runtime.cache().get(KEY).is_some(),
            "naming the format the url already implies must not mint a second cache key"
        );
    }
}

async fn attachment_transform(data: &[u8], filename: &str, params: &[(&str, &str)]) -> Response {
    let params = params
        .iter()
        .map(|(name, value)| ((*name).to_owned(), (*value).to_owned()))
        .collect::<HashMap<_, _>>();
    serve_bytes_or_transform(
        &test_runtime(),
        ServeBytesRequest {
            method: Method::GET,
            data: Bytes::copy_from_slice(data),
            content_type: "application/octet-stream".to_owned(),
            cache_identity: filename,
            filename,
            route: TransformRoute::Attachment,
            params: &params,
            headers: &HeaderMap::new(),
        },
    )
    .await
}

#[tokio::test]
async fn a_full_size_transform_too_large_to_process_serves_the_original() {
    let png = synthetic_png(6145, 4097);
    let response = attachment_transform(&png, "big.png", &[("format", "webp")]).await;
    assert_eq!(StatusCode::OK, response.status());
    assert_eq!(
        Some("image/png"),
        response
            .headers()
            .get(header::CONTENT_TYPE)
            .and_then(|value| value.to_str().ok())
    );
    let body = to_bytes(response.into_body(), usize::MAX)
        .await
        .expect("body");
    assert_eq!(png, body);
}

#[tokio::test]
async fn a_waiter_releases_its_source_while_the_leader_runs() {
    const KEY: &str = "attachment:art.png|asset_kind=not-applicable|w=none|h=none|fmt=webp|q=lossless|anim=false|effort=none|resize=fit";
    let runtime = test_runtime();
    let params = HashMap::from([("format".to_owned(), "webp".to_owned())]);
    let headers = HeaderMap::new();
    let source = Bytes::from(synthetic_png(48, 48));
    let probe = source.clone();
    let (release, released) = tokio::sync::oneshot::channel::<()>();
    let leader =
        runtime
            .cache()
            .get_or_run(KEY.to_owned(), OutputFormat::WebP, None, || async move {
                let _ = released.await;
                Ok(MediaBytes::from(vec![0u8; 4]))
            });
    let waiter = async {
        let response = serve_bytes_or_transform(
            &runtime,
            ServeBytesRequest {
                method: Method::GET,
                data: source,
                content_type: "application/octet-stream".to_owned(),
                cache_identity: "art.png",
                filename: "art.png",
                route: TransformRoute::Attachment,
                params: &params,
                headers: &headers,
            },
        );
        tokio::pin!(response);
        assert!(
            tokio::time::timeout(std::time::Duration::from_millis(100), response.as_mut())
                .await
                .is_err(),
            "the request waits on the running leader"
        );
        assert!(
            probe.is_unique(),
            "a waiter must not pin its source while the leader runs"
        );
        let _ = release.send(());
        response.await
    };
    let (leader, response) = tokio::join!(leader, waiter);
    leader.expect("leader publishes");
    assert_eq!(StatusCode::OK, response.status());
}

#[tokio::test]
async fn a_resized_transform_too_large_to_process_still_fails() {
    let response = attachment_transform(
        &synthetic_png(6145, 4097),
        "big.png",
        &[("format", "webp"), ("width", "6145")],
    )
    .await;
    assert_eq!(StatusCode::BAD_REQUEST, response.status());
}

#[tokio::test]
async fn a_full_size_transform_too_large_to_process_never_serves_an_undisplayable_original() {
    let response =
        attachment_transform(JXL_4100_LOSSY_ALPHA, "big.jxl", &[("format", "webp")]).await;
    assert_eq!(StatusCode::BAD_REQUEST, response.status());
}

#[tokio::test]
async fn a_full_size_jxl_above_the_static_output_cap_is_downscaled() {
    let response =
        attachment_transform(JXL_7000X4000_LOSSY_RGB, "big.jxl", &[("format", "webp")]).await;
    assert_eq!(StatusCode::OK, response.status());
    assert_eq!(
        Some("image/webp"),
        response
            .headers()
            .get(header::CONTENT_TYPE)
            .and_then(|value| value.to_str().ok())
    );
}

#[test]
fn the_budget_leaves_room_for_the_cache_and_headroom() {
    assert_eq!(
        2560 << 20,
        native_memory_budget_bytes(4096 << 20, 1024 << 20)
    );
}

#[test]
fn a_cache_larger_than_the_pod_takes_at_most_half_of_it() {
    assert_eq!(192 << 20, native_memory_budget_bytes(512 << 20, 1 << 30));
    assert_eq!(384 << 20, native_memory_budget_bytes(1 << 30, 1 << 30));
}

#[test]
fn the_budget_never_drops_below_its_floor() {
    assert_eq!(128 << 20, native_memory_budget_bytes(64 << 20, 0));
}

#[tokio::test]
async fn an_animated_transform_over_the_work_budget_serves_the_original() {
    let webp = crate::test_fixtures::ANIMATED_WEBP_1024_17_FRAMES;
    let response = attachment_transform(
        webp,
        "anim.webp",
        &[("format", "webp"), ("animated", "true"), ("width", "1000")],
    )
    .await;
    assert_eq!(StatusCode::OK, response.status());
    let body = to_bytes(response.into_body(), usize::MAX)
        .await
        .expect("body");
    assert_eq!(webp, body.as_ref());
}

#[tokio::test]
async fn an_animated_transform_within_the_work_budget_is_transformed() {
    let webp = crate::test_fixtures::ANIMATED_WEBP_1024_17_FRAMES;
    let response = attachment_transform(
        webp,
        "anim.webp",
        &[("format", "webp"), ("animated", "true"), ("width", "128")],
    )
    .await;
    assert_eq!(StatusCode::OK, response.status());
    let body = to_bytes(response.into_body(), usize::MAX)
        .await
        .expect("body");
    assert_ne!(webp, body.as_ref());
}
