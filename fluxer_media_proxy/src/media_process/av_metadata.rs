// SPDX-License-Identifier: AGPL-3.0-or-later

use super::encoding::{VipsEncodeRequest, anim_limits_from_options, encode_vips_image};
use super::loaded_image::{resize_loaded_image, validate_dimensions_u32, validate_vips_image};
use super::native_runtime::{clear_vips_error, vips_runtime};
use super::placeholder::encode_thumbhash_image;
use super::{MediaError, native_status_error};
use crate::{
    constants,
    image_quality::ResolvedImageQuality,
    image_transform::ImageOptions,
    media_limits::MediaLimits,
    native::{self, NativeStatus, VipsImageHandle, buffer::NativeBuffer},
    output_format::OutputFormat,
    thumbhash,
};
use libc::c_int;

pub const NSFW_PREVIEW_MAX_DIMENSION: u32 = 512;

#[derive(Clone, Copy, Debug, Default)]
pub struct AVProbe {
    pub has_video: bool,
    pub has_audio: bool,
    pub duration_seconds: Option<f64>,
}

pub struct AVMetadataFrame {
    buffer: NativeBuffer,
    pub display_width: u32,
    pub display_height: u32,
    width: u32,
    height: u32,
}

impl AVMetadataFrame {
    pub fn encode_thumbhash(
        &self,
        media_limits: &MediaLimits,
        deadline_ms: Option<i64>,
    ) -> Result<Vec<u8>, MediaError> {
        if self.width <= thumbhash::MAX_DIM && self.height <= thumbhash::MAX_DIM {
            return thumbhash::encode_rgba(self.buffer.as_slice(), self.width, self.height)
                .map_err(|_| MediaError::InvalidImageDimensions);
        }
        let image = self.borrow_as_vips_image(media_limits)?;
        let resized = resize_loaded_image(
            media_limits,
            image,
            &ImageOptions {
                width: Some(thumbhash::MAX_DIM),
                height: Some(thumbhash::MAX_DIM),
                deadline_ms,
                ..Default::default()
            },
        )?;
        encode_thumbhash_image(&resized, deadline_ms)
    }

    pub fn encode_nsfw_jpeg(
        &self,
        media_limits: &MediaLimits,
        deadline_ms: Option<i64>,
    ) -> Result<Vec<u8>, MediaError> {
        let image = self.borrow_as_vips_image(media_limits)?;
        encode_vips_image(
            &image,
            VipsEncodeRequest {
                format: OutputFormat::JPEG,
                quality: ResolvedImageQuality::Low,
                page_height: None,
                effort_override: None,
                animation: anim_limits_from_options(&ImageOptions {
                    deadline_ms,
                    ..Default::default()
                }),
                animation_loop_count: None,
                media_limits,
                full_canvas_animation: false,
            },
        )
    }

    fn borrow_as_vips_image(
        &self,
        media_limits: &MediaLimits,
    ) -> Result<VipsImageHandle<'_>, MediaError> {
        let buffer = self.buffer.as_slice();
        let width = c_int::try_from(self.width).map_err(|_| MediaError::InvalidImageDimensions)?;
        let height =
            c_int::try_from(self.height).map_err(|_| MediaError::InvalidImageDimensions)?;
        let raw = unsafe {
            native::fluxer_vips_image_new_from_memory(
                buffer.as_ptr().cast(),
                buffer.len(),
                width,
                height,
                4,
                native::fluxer_vips_format_uchar,
            )
        };
        let image = unsafe { VipsImageHandle::from_raw_borrowing(raw, buffer) }
            .ok_or(MediaError::MediaTransformFailed)?;
        validate_vips_image(media_limits, &image)?;
        Ok(image)
    }
}

pub struct AVMetadata {
    pub probe: AVProbe,
    pub frame: Option<AVMetadataFrame>,
}

pub fn probe_av_metadata(
    input: &[u8],
    preview_max_dimension: u32,
    media_limits: &MediaLimits,
    deadline_ms: Option<i64>,
) -> Result<AVMetadata, MediaError> {
    if input.is_empty() {
        return Err(MediaError::MediaDecodeFailed);
    }
    if input.len() > media_limits.max_media_proxy_bytes() {
        return Err(MediaError::StreamTooLong);
    }
    if preview_max_dimension > NSFW_PREVIEW_MAX_DIMENSION {
        return Err(MediaError::InvalidImageDimensions);
    }
    let runtime = vips_runtime()?;
    let max_dimension =
        c_int::try_from(preview_max_dimension).map_err(|_| MediaError::InvalidImageDimensions)?;
    let mut output = native::FluxerAVMetadataOut::empty();
    let rc = unsafe {
        native::fluxer_av_metadata(
            input.as_ptr().cast(),
            input.len(),
            runtime.config().ffmpeg_decoder_threads(),
            deadline_ms.unwrap_or(0),
            constants::MAX_VIDEO_PACKETS_FOR_THUMBNAIL as c_int,
            max_dimension,
            max_dimension,
            &mut output,
        )
    };
    let had_frame_allocation = !output.rgba.is_null();
    let frame_buffer = unsafe { NativeBuffer::from_vips_owned(output.rgba, output.rgba_size) };
    if let Some(error) =
        native_status_error(NativeStatus::from_code(rc), MediaError::MediaDecodeFailed)
    {
        clear_vips_error();
        return Err(error);
    }
    if output.has_video == 0 && output.has_audio == 0 {
        return Err(MediaError::MediaDecodeFailed);
    }
    if output.has_video != 0 {
        if output.frame_count < 0 {
            return Err(MediaError::MediaDecodeFailed);
        }
    } else if output.frame_count != 0 {
        return Err(MediaError::MediaDecodeFailed);
    }
    let probe = AVProbe {
        has_video: output.has_video != 0,
        has_audio: output.has_audio != 0,
        duration_seconds: positive_finite_duration(output.duration_seconds),
    };
    let frame = av_metadata_frame(
        &output,
        frame_buffer,
        had_frame_allocation,
        preview_max_dimension,
        media_limits,
    )?;
    Ok(AVMetadata { probe, frame })
}

fn av_metadata_frame(
    output: &native::FluxerAVMetadataOut,
    buffer: Option<NativeBuffer>,
    had_frame_allocation: bool,
    max_dimension: u32,
    media_limits: &MediaLimits,
) -> Result<Option<AVMetadataFrame>, MediaError> {
    let Some(buffer) = buffer else {
        let empty = !had_frame_allocation
            && output.rgba_size == 0
            && output.rgba_width == 0
            && output.rgba_height == 0
            && output.display_width == 0
            && output.display_height == 0;
        return if empty {
            Ok(None)
        } else {
            Err(MediaError::MediaDecodeFailed)
        };
    };
    if output.rgba_width <= 0 || output.rgba_height <= 0 {
        return Err(MediaError::MediaDecodeFailed);
    }
    if output.display_width <= 0 || output.display_height <= 0 {
        return Err(MediaError::MediaDecodeFailed);
    }
    let width = output.rgba_width as u32;
    let height = output.rgba_height as u32;
    validate_dimensions_u32(
        media_limits,
        output.display_width as u32,
        output.display_height as u32,
    )?;
    if width > max_dimension || height > max_dimension {
        return Err(MediaError::InvalidImageDimensions);
    }
    let expected_size = width as usize * height as usize * 4;
    if buffer.len() != expected_size {
        return Err(MediaError::MediaDecodeFailed);
    }
    Ok(Some(AVMetadataFrame {
        buffer,
        display_width: output.display_width as u32,
        display_height: output.display_height as u32,
        width,
        height,
    }))
}

fn positive_finite_duration(duration_seconds: f64) -> Option<f64> {
    (duration_seconds.is_finite() && duration_seconds > 0.0).then_some(duration_seconds)
}
