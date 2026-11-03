// SPDX-License-Identifier: AGPL-3.0-or-later

mod failure;
mod input;
mod strip;

pub(in crate::server) use failure::MediaFailure;
pub(in crate::server) use input::{
    MediaInput, MediaInputLimit, STORED_SNIFF_PREFIX_BYTES, load_media_input, read_sniff_prefix,
    spool_stored_media,
};
pub(in crate::server) use strip::{StrippedMedia, probe_audio_duration, strip_media};

use crate::{
    constants::{self, AssetExtension},
    image_quality::ImageQuality,
    image_transform::AnimationMode,
    media_process, mime,
    nsfw::NSFWPolicy,
    output_format::OutputFormat,
    server::{
        external::serve_external,
        format_policy::image_extension_from_filename,
        self_origin::{self, ServedRoute, ServedVariant},
        state::AppState,
        stored,
        transform::execution::{deadline_instant, run_transform, transform_error_is_timeout},
    },
};
use axum::{
    body::to_bytes,
    http::{HeaderMap, Method, StatusCode},
};
use bytes::Bytes;
use input::LoadedMediaInput;
use std::{sync::Arc, time::Duration};

const SERVED_VARIANT_TIMEOUT: Duration = Duration::from_secs(2);

pub(in crate::server) struct MetadataOutput {
    pub(in crate::server) metadata: serde_json::Value,
    pub(in crate::server) data: Option<Bytes>,
}

pub(in crate::server) async fn resolve_metadata(
    app: &Arc<AppState>,
    input: MediaInput,
    scan_nsfw: bool,
    include_data: bool,
) -> Result<MetadataOutput, MediaFailure> {
    let served = match &input {
        MediaInput::External { url, .. } if !include_data => self_origin::served_variant(app, url),
        _ => None,
    };
    let mut input = load_media_input(app, input, MediaInputLimit::INTERNAL_REQUEST).await?;
    if include_data && input.data.len() > constants::MAX_MEDIA_PROXY_BYTES {
        return Err(MediaFailure::MetadataDecodedTooLarge);
    }
    if include_data && metadata_input_is_svg(&input) {
        input = rasterize_metadata_svg(app, input).await?;
    }
    let nsfw = if scan_nsfw {
        app.media.nsfw().policy()
    } else {
        app.media.nsfw().record_declined_scan();
        NSFWPolicy::Disabled
    };
    let mut metadata = extract_metadata(app, &input, true, nsfw).await?;
    let is_image = metadata["content_type"]
        .as_str()
        .is_some_and(|content_type| content_type.starts_with("image/"));
    if let Some(served) = served.filter(|_| is_image)
        && let Some(mut served) = served_variant_metadata(app, served).await
    {
        for field in ["width", "height", "content_type", "format"] {
            metadata[field] = served[field].take();
        }
    }
    Ok(MetadataOutput {
        metadata,
        data: include_data.then_some(input.data),
    })
}

async fn served_variant_metadata(
    app: &Arc<AppState>,
    served: ServedVariant,
) -> Option<serde_json::Value> {
    let ServedVariant { route, params } = served;
    let headers = HeaderMap::new();
    let work = async {
        let response = match route {
            ServedRoute::Asset(asset) => {
                stored::serve_asset_image(app, Method::GET, asset, &params, &headers).await
            }
            ServedRoute::Attachment { key } => {
                stored::serve_attachment(app, Method::GET, &key, &params, &headers).await
            }
            ServedRoute::External { rest } => {
                serve_external(app, Method::GET, &rest, &params, &headers).await
            }
        };
        if response.status() != StatusCode::OK {
            return None;
        }
        let data = to_bytes(response.into_body(), constants::MAX_MEDIA_PROXY_BYTES)
            .await
            .ok()?;
        let input = LoadedMediaInput {
            data,
            filename: "served".to_owned(),
        };
        extract_metadata(app, &input, false, NSFWPolicy::Disabled)
            .await
            .ok()
    };
    tokio::time::timeout(SERVED_VARIANT_TIMEOUT, work)
        .await
        .ok()
        .flatten()
}

async fn extract_metadata(
    app: &AppState,
    input: &LoadedMediaInput,
    placeholder: bool,
    nsfw: NSFWPolicy,
) -> Result<serde_json::Value, MediaFailure> {
    let runtime = app.media.transforms();
    let options = media_process::MetadataOptions {
        placeholder,
        nsfw,
        deadline_ms: runtime.transform_deadline_ms(),
    };
    let deadline = deadline_instant(options.deadline_ms);
    let media_limits = app.media.limits();
    let transform_metrics = app.metrics.transform();
    let data = input.data.clone();
    let cost_data = input.data.clone();
    let extraction_failed = |err: &dyn std::fmt::Debug| MediaFailure::MetadataExtractionFailed {
        detail: format!("filename={} err={err:?}", input.filename),
    };
    let prepared = runtime
        .tasks()
        .run_native(
            deadline,
            move || Ok(media_process::metadata_cost(&cost_data, &media_limits)?),
            move || {
                Ok(media_process::prepare_metadata(
                    &data,
                    &options,
                    &media_limits,
                    &transform_metrics,
                )?)
            },
        )
        .await
        .map_err(|err| extraction_failed(&err))?;
    let json = media_process::finish_metadata(prepared, app.media.nsfw())
        .await
        .map_err(|err| extraction_failed(&err))?;
    Ok(serde_json::from_str(&json).unwrap_or_else(|_| serde_json::json!({})))
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
        width: Some(constants::SVG_RASTER_MAX_DIMENSION),
        height: Some(constants::SVG_RASTER_MAX_DIMENSION),
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
