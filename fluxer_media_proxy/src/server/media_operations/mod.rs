// SPDX-License-Identifier: AGPL-3.0-or-later

mod failure;
mod input;

pub(in crate::server) use failure::MediaFailure;
pub(in crate::server) use input::{MediaInput, MediaInputLimit, load_media_input};

use crate::{
    constants::AssetExtension,
    image_quality::ImageQuality,
    image_transform::AnimationMode,
    media_process, mime,
    nsfw::NSFWPolicy,
    output_format::OutputFormat,
    server::{
        format_policy::image_extension_from_filename,
        state::AppState,
        transform::execution::{run_transform, transform_error_is_timeout},
    },
};
use bytes::Bytes;
use input::LoadedMediaInput;

pub(in crate::server) struct MetadataOutput {
    pub(in crate::server) metadata: serde_json::Value,
    pub(in crate::server) data: Option<Bytes>,
}

pub(in crate::server) async fn resolve_metadata(
    app: &AppState,
    input: MediaInput,
    scan_nsfw: bool,
    include_data: bool,
) -> Result<MetadataOutput, MediaFailure> {
    let mut input = load_media_input(app, input, MediaInputLimit::INTERNAL_REQUEST).await?;
    if include_data && metadata_input_is_svg(&input) {
        input = rasterize_metadata_svg(app, input).await?;
    }
    let nsfw = if scan_nsfw {
        app.media.nsfw().policy()
    } else {
        app.media.nsfw().record_declined_scan();
        NSFWPolicy::Disabled
    };
    let json = media_process::metadata_json_with_options(
        &input.data,
        &input.filename,
        media_process::MetadataOptions {
            placeholder: true,
            nsfw,
        },
        &app.media.limits(),
        app.media.nsfw(),
        &app.metrics.transform(),
    )
    .await
    .map_err(|err| MediaFailure::MetadataExtractionFailed {
        detail: format!("filename={} err={err:?}", input.filename),
    })?;
    Ok(MetadataOutput {
        metadata: serde_json::from_str(&json).unwrap_or_else(|_| serde_json::json!({})),
        data: include_data.then_some(input.data),
    })
}

fn metadata_input_is_svg(input: &LoadedMediaInput) -> bool {
    mime::sniff(&input.data).mime == "image/svg+xml"
        || image_extension_from_filename(&input.filename) == Some(AssetExtension::Svg)
}

async fn rasterize_metadata_svg(
    app: &AppState,
    input: LoadedMediaInput,
) -> Result<LoadedMediaInput, MediaFailure> {
    let options = media_process::ImageOptions {
        format: OutputFormat::WebP,
        quality: ImageQuality::Lossless,
        animation: AnimationMode::Static,
        deadline_ms: app.media.transforms().transform_deadline_ms(),
        ..Default::default()
    };
    let filename = input.filename;
    match run_transform(app.media.transforms(), input.data, options).await {
        Ok(media) => Ok(LoadedMediaInput {
            data: media.bytes.into(),
            filename: replace_image_extension(&filename, AssetExtension::Webp),
        }),
        Err(err) if transform_error_is_timeout(&err) => {
            Err(MediaFailure::MetadataSvgRasterizeTimeout { detail: filename })
        }
        Err(err) => Err(MediaFailure::MetadataSvgRasterizeFailed {
            detail: format!("filename={filename} err={err:?}"),
        }),
    }
}

fn replace_image_extension(filename: &str, ext: AssetExtension) -> String {
    let last_slash = filename.rfind('/').map(|idx| idx + 1).unwrap_or(0);
    let last_dot = filename[last_slash..]
        .rfind('.')
        .map(|idx| last_slash + idx);
    match last_dot {
        Some(idx) => format!("{}.{}", &filename[..idx], ext.name()),
        None => format!("{}.{}", filename, ext.name()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::byte_budget::{BudgetedBytes, ByteBudget};
    use crate::config::Config;
    use std::sync::Arc;

    fn test_app_state() -> Arc<AppState> {
        let cfg = Config::load_from_iter([("FLUXER_MEDIA_PROXY_SECRET_KEY", "secret")])
            .expect("test config");
        Arc::new(AppState::for_tests(cfg))
    }

    #[test]
    fn metadata_base64_svg_detection_uses_bytes_or_filename() {
        let svg_bytes = LoadedMediaInput {
            data: Bytes::from_static(br#"<svg xmlns="http://www.w3.org/2000/svg"></svg>"#),
            filename: "upload.bin".to_owned(),
        };
        assert!(metadata_input_is_svg(&svg_bytes));

        let svg_filename = LoadedMediaInput {
            data: Bytes::from_static(b"not svg"),
            filename: "icons/logo.svg".to_owned(),
        };
        assert!(metadata_input_is_svg(&svg_filename));

        let png_filename = LoadedMediaInput {
            data: Bytes::from_static(b"not svg"),
            filename: "icons/logo.png".to_owned(),
        };
        assert!(!metadata_input_is_svg(&png_filename));
    }

    #[test]
    fn replace_image_extension_only_changes_last_path_segment() {
        assert_eq!(
            "avatars/user.icon.webp",
            replace_image_extension("avatars/user.icon.svg", AssetExtension::Webp)
        );
        assert_eq!(
            "avatars.v1/user.webp",
            replace_image_extension("avatars.v1/user", AssetExtension::Webp)
        );
    }

    #[tokio::test]
    async fn metadata_base64_svg_rasterizes_to_webp_bytes() {
        let app = test_app_state();
        let input = LoadedMediaInput {
            data: Bytes::from_static(
                br#"<svg xmlns="http://www.w3.org/2000/svg" width="8" height="8"><rect width="8" height="8" fill="red"/></svg>"#,
            ),
            filename: "icons/logo.svg".to_owned(),
        };
        let raster = match rasterize_metadata_svg(&app, input).await {
            Ok(raster) => raster,
            Err(failure) => panic!("unexpected failure {}", failure.code()),
        };

        assert_eq!("icons/logo.webp", raster.filename);
        assert_eq!("image/webp", mime::sniff(&raster.data).mime);
    }

    #[test]
    fn an_external_media_input_keeps_owing_the_shared_buffer_budget() {
        let budget = ByteBudget::new(16);
        let reservation = budget.try_reserve(16).expect("reserve at limit");
        let data = input::retained_input_bytes(BudgetedBytes::budgeted(
            Bytes::from_static(b"payload"),
            reservation,
        ));

        assert_eq!(b"payload", data.as_ref());
        assert!(
            budget.try_reserve(1).is_none(),
            "a resident external input still owes the external buffer budget"
        );

        drop(data);
        assert_eq!(
            16,
            budget
                .try_reserve(16)
                .expect("dropping the input releases its reservation")
                .amount()
        );
    }
}
