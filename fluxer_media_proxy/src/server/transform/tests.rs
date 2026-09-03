// SPDX-License-Identifier: AGPL-3.0-or-later

use super::*;
use crate::{
    constants::AssetExtension,
    server::format_policy::{default_transform_quality, transform_static_quality_default},
    test_fixtures::{fixture_h264_mp4, fixture_jpeg, synthetic_png},
};

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
