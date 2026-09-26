// SPDX-License-Identifier: AGPL-3.0-or-later

use super::MediaError;
use super::av_metadata::{AVMetadata, AVProbe, NSFW_PREVIEW_MAX_DIMENSION, probe_av_metadata};
use super::image_probe::probe_image_dims;
use super::loaded_image::validate_dimensions_u32;
use super::nsfw_processing::{
    NSFWScanPreparation, NSFWScanSource, classify_nsfw_buffers, nsfw_scan_buffers,
};
use super::placeholder::{encode_thumbhash, encoded_placeholder, optional_thumbhash};
use crate::{
    constants,
    media_limits::MediaLimits,
    metrics::transform::TransformMetrics,
    mime,
    nsfw::{NSFWClassification, NSFWClient, NSFWPolicy},
};
use serde::Serialize;
use sha2::{Digest, Sha256};

#[derive(Clone, Debug)]
pub struct MetadataOptions {
    pub placeholder: bool,
    pub nsfw: NSFWPolicy,
}

impl Default for MetadataOptions {
    fn default() -> Self {
        Self {
            placeholder: true,
            nsfw: NSFWPolicy::Disabled,
        }
    }
}

#[derive(Serialize)]
struct MetadataResponse {
    content_type: String,
    size: usize,
    content_hash: String,
    format: String,
    width: Option<u32>,
    height: Option<u32>,
    animated: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    duration: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    placeholder: Option<String>,
    nsfw: bool,
    nsfw_probability: f32,
}

struct MetadataBlocking {
    response: MetadataResponse,
    nsfw_scan: Option<NSFWScanPreparation>,
}

impl MetadataBlocking {
    fn take_nsfw_scan(&mut self) -> Option<NSFWScanPreparation> {
        self.nsfw_scan.take()
    }
}

struct MetadataBlockingRequest<'a> {
    media_limits: &'a MediaLimits,
    metrics: &'a TransformMetrics,
    input: &'a [u8],
    options: &'a MetadataOptions,
}

fn metadata_content_type(sniffed_mime: &'static str, av_probe: Option<&AVProbe>) -> &'static str {
    if sniffed_mime == "video/mp4"
        && av_probe.is_some_and(|probe| probe.has_audio && !probe.has_video)
    {
        return "audio/mp4";
    }
    sniffed_mime
}

fn metadata_format(sniffed_mime: &str) -> String {
    match sniffed_mime {
        "audio/mpeg" => "mp3".to_owned(),
        "video/quicktime" => "mov".to_owned(),
        "video/x-matroska" => "mkv".to_owned(),
        "image/svg+xml" => "svg".to_owned(),
        "audio/mp4" => "m4a".to_owned(),
        "image/avif-sequence" => "avif".to_owned(),
        "image/apng" => "apng".to_owned(),
        other => other
            .rsplit_once('/')
            .map(|(_, suffix)| suffix.strip_prefix("x-").unwrap_or(suffix))
            .unwrap_or("bin")
            .to_owned(),
    }
}

fn validate_metadata_image_dimensions(
    media_limits: &MediaLimits,
    width: u32,
    height: u32,
    frames: u32,
) -> Result<(), MediaError> {
    validate_dimensions_u32(media_limits, width, height)?;
    let frame_count = frames.max(1);
    if frame_count > media_limits.animated_frames() {
        return Err(MediaError::InvalidImageDimensions);
    }
    if frame_count > 1 {
        let w = width as usize;
        let h = height as usize;
        let per_frame = w.saturating_mul(h);
        let max_total = media_limits.animated_total_pixels();
        let fc = frame_count as usize;
        if per_frame > max_total / fc.max(1) {
            return Err(MediaError::InvalidImageDimensions);
        }
    }
    Ok(())
}

fn probe_av_metadata_without_requiring_a_frame(
    input: &[u8],
    media_limits: &MediaLimits,
) -> Result<AVMetadata, MediaError> {
    match probe_av_metadata(input, NSFW_PREVIEW_MAX_DIMENSION, media_limits, None) {
        Ok(metadata) => Ok(metadata),
        Err(error) => probe_av_metadata(input, 0, media_limits, None).map_err(|_| error),
    }
}

fn metadata_blocking(request: MetadataBlockingRequest<'_>) -> Result<MetadataBlocking, MediaError> {
    let MetadataBlockingRequest {
        media_limits,
        metrics,
        input,
        options,
    } = request;
    if input.len() > constants::MAX_MEDIA_PROXY_BYTES {
        return Err(MediaError::StreamTooLong);
    }
    let sniffed = mime::sniff(input);
    if !mime::is_supported_media_mime(sniffed.mime) {
        return Err(MediaError::UnsupportedMediaType);
    }
    let initial_category = mime::category(sniffed.mime).ok_or(MediaError::UnsupportedMediaType)?;
    let is_image = initial_category == mime::Category::Image;
    let dims = if is_image {
        Some(probe_image_dims(media_limits, input)?)
    } else {
        None
    };
    let frames_count = dims.map(|d| d.pages).unwrap_or(sniffed.frames);
    let mut width = dims.map(|d| d.width).unwrap_or(sniffed.width);
    let mut height = dims.map(|d| d.height).unwrap_or(sniffed.height);
    if is_image && (width > 0 || height > 0 || frames_count > 1) {
        validate_metadata_image_dimensions(media_limits, width, height, frames_count)?;
    }

    let av_metadata = if matches!(
        initial_category,
        mime::Category::Video | mime::Category::Audio
    ) {
        Some(probe_av_metadata_without_requiring_a_frame(
            input,
            media_limits,
        )?)
    } else {
        None
    };
    let av_probe = av_metadata.as_ref().map(|metadata| metadata.probe);
    let content_type = metadata_content_type(sniffed.mime, av_probe.as_ref());
    let category = mime::category(content_type).ok_or(MediaError::UnsupportedMediaType)?;
    if let Some(probe) = av_probe.as_ref()
        && category == mime::Category::Audio
        && !probe.has_audio
    {
        return Err(MediaError::MediaDecodeFailed);
    }

    let av_frame = av_metadata
        .as_ref()
        .filter(|_| category == mime::Category::Video)
        .and_then(|metadata| metadata.frame.as_ref());
    if let Some(frame) = av_frame {
        width = frame.display_width;
        height = frame.display_height;
    }

    let animated = sniffed.animated || dims.is_some_and(|d| d.pages > 1);
    let placeholder = if options.placeholder {
        let hash = if is_image {
            optional_thumbhash(
                encode_thumbhash(media_limits, input, None),
                "image_metadata",
            )
        } else {
            match av_frame {
                Some(frame) => {
                    optional_thumbhash(frame.encode_thumbhash(media_limits, None), "video_metadata")
                }
                None => None,
            }
        };
        encoded_placeholder(hash)
    } else {
        None
    };

    let scan_eligible = is_image
        || (category == mime::Category::Video
            && av_probe.as_ref().is_some_and(|probe| probe.has_video));
    let nsfw_scan = if let Some(threshold) = options.nsfw.scan_threshold(scan_eligible) {
        nsfw_scan_buffers(NSFWScanSource {
            media_limits,
            metrics,
            threshold,
            content_type: sniffed.mime,
            animated,
            frame_count: frames_count.max(sniffed.frames),
            input,
            duration_seconds: av_probe.as_ref().and_then(|probe| probe.duration_seconds),
            deadline_ms: None,
        })?
    } else {
        None
    };

    let duration = av_probe.as_ref().and_then(|probe| {
        probe
            .duration_seconds
            .filter(|duration| duration.is_finite() && *duration > 0.0)
            .map(|duration| duration.ceil() as u32)
    });
    let (response_width, response_height) = if width > 0 && height > 0 {
        (Some(width), Some(height))
    } else {
        (None, None)
    };

    let format = metadata_format(content_type);
    let content_hash = hex::encode(Sha256::digest(input));
    Ok(MetadataBlocking {
        response: MetadataResponse {
            content_type: content_type.to_owned(),
            size: input.len(),
            content_hash,
            format,
            width: response_width,
            height: response_height,
            animated,
            duration,
            placeholder,
            nsfw: false,
            nsfw_probability: 0.0,
        },
        nsfw_scan,
    })
}

fn metadata_finalize(prepared: MetadataBlocking, verdict: NSFWClassification) -> MetadataResponse {
    let mut response = prepared.response;
    response.nsfw = verdict.is_nsfw;
    response.nsfw_probability = verdict.probability;
    response
}

pub async fn metadata_json_with_options(
    input: &[u8],
    _filename: &str,
    options: MetadataOptions,
    media_limits: &MediaLimits,
    nsfw_client: &NSFWClient,
    metrics: &TransformMetrics,
) -> Result<String, MediaError> {
    let mut prepared = metadata_blocking(MetadataBlockingRequest {
        media_limits,
        metrics,
        input,
        options: &options,
    })?;
    let scan = prepared.take_nsfw_scan();
    let verdict = classify_nsfw_buffers(nsfw_client, scan).await?;
    let response = metadata_finalize(prepared, verdict);
    serde_json::to_string(&response).map_err(|_| MediaError::MediaEncodeFailed)
}

pub async fn metadata_json(
    input: &[u8],
    filename: &str,
    media_limits: &MediaLimits,
    metrics: &TransformMetrics,
) -> Result<String, MediaError> {
    metadata_json_with_options(
        input,
        filename,
        MetadataOptions::default(),
        media_limits,
        &NSFWClient::disabled(),
        metrics,
    )
    .await
}
