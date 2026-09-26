// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    byte_budget::{BudgetedBytes, ByteBudget},
    constants, metrics,
    native::{NativeStatus, buffer::NativeBuffer},
};

pub use crate::{
    image_quality::{ImageQuality, ResolvedImageQuality},
    image_transform::{AnimationLimits, AnimationMode, EncodeEffort, ImageOptions, ResizeMode},
    media_limits::MediaLimits,
};
mod animated_transform;
mod apng;
mod av_metadata;
mod encoding;
mod image_probe;
mod loaded_image;
mod metadata;
pub(crate) mod native_runtime;
mod nsfw_processing;
mod placeholder;
mod streaming_encoder;
mod transform;
mod transform_plan;
mod video_thumbnail;

#[cfg(test)]
mod tests;

pub use av_metadata::{
    AVMetadata, AVMetadataFrame, AVProbe, NSFW_PREVIEW_MAX_DIMENSION, probe_av_metadata,
};
pub use metadata::{MetadataOptions, metadata_json, metadata_json_with_options};
pub use nsfw_processing::encode_static_image_for_nsfw;
pub use transform::transform_image;
pub use video_thumbnail::{
    VideoThumbnailOptions, extract_video_thumbnail, extract_video_thumbnail_with_options,
};

use native_runtime::ensure_vips_init;

use bytes::Bytes;
use libc::{c_int, c_longlong};
use std::ops::Deref;
use thiserror::Error;

const PNG_SIGNATURE: &[u8; 8] = b"\x89PNG\r\n\x1a\n";

#[derive(Clone, Debug)]
pub struct ProcessedMedia {
    pub bytes: Vec<u8>,
    pub content_type: &'static str,
}

#[derive(Debug)]
pub struct MediaBytes {
    storage: MediaBytesStorage,
}

#[derive(Debug)]
enum MediaBytesStorage {
    Native(NativeBuffer),
    Rust(Vec<u8>),
}

impl MediaBytes {
    fn from_native(buffer: NativeBuffer) -> Self {
        Self {
            storage: MediaBytesStorage::Native(buffer),
        }
    }

    pub fn resident_bytes(&self) -> usize {
        match &self.storage {
            MediaBytesStorage::Native(buffer) => buffer.resident_bytes(),
            MediaBytesStorage::Rust(buffer) => buffer.capacity(),
        }
    }

    pub fn into_bytes(self) -> Bytes {
        match self.storage {
            MediaBytesStorage::Native(buffer) => Bytes::from_owner(buffer),
            MediaBytesStorage::Rust(buffer) => Bytes::from(buffer),
        }
    }

    pub fn try_into_budgeted(self, budget: &ByteBudget) -> Option<BudgetedBytes> {
        let reservation = budget.try_reserve(self.resident_bytes())?;
        Some(BudgetedBytes::budgeted(self.into_bytes(), reservation))
    }

    pub fn try_into_vec(self) -> Result<Vec<u8>, MediaError> {
        match self.storage {
            MediaBytesStorage::Native(buffer) => buffer
                .try_to_vec()
                .map_err(|_| MediaError::AllocationFailed),
            MediaBytesStorage::Rust(buffer) => Ok(buffer),
        }
    }
}

impl From<Vec<u8>> for MediaBytes {
    fn from(buffer: Vec<u8>) -> Self {
        Self {
            storage: MediaBytesStorage::Rust(buffer),
        }
    }
}

impl AsRef<[u8]> for MediaBytes {
    fn as_ref(&self) -> &[u8] {
        self
    }
}

impl Deref for MediaBytes {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        match &self.storage {
            MediaBytesStorage::Native(buffer) => buffer.as_slice(),
            MediaBytesStorage::Rust(buffer) => buffer.as_slice(),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ImageDimensions {
    pub width: u32,
    pub height: u32,
    pub pages: u32,
}

#[derive(Clone, Copy, Debug, Error, Eq, PartialEq)]
pub enum MediaError {
    #[error("media allocation failed")]
    AllocationFailed,
    #[error("native media init failed")]
    VipsInitFailed,
    #[error("media decode failed")]
    MediaDecodeFailed,
    #[error("media encode failed")]
    MediaEncodeFailed,
    #[error("media transform failed")]
    MediaTransformFailed,
    #[error("invalid image dimensions")]
    InvalidImageDimensions,
    #[error("unsupported media type")]
    UnsupportedMediaType,
    #[error("unsupported output format")]
    UnsupportedOutputFormat,
    #[error("stream too long")]
    StreamTooLong,
    #[error("request timed out")]
    RequestTimeout,
    #[error("nsfw scan unavailable")]
    NsfwScanUnavailable,
}

#[derive(Clone, Copy, Debug)]
struct AnimatedProbe {
    width: c_int,
    height: c_int,
    pages: c_int,
}

pub fn warmup_vips() -> Result<(), MediaError> {
    ensure_vips_init()
}

fn native_optional_deadline(deadline_monotonic_ms: Option<i64>) -> c_longlong {
    match deadline_monotonic_ms {
        Some(deadline) if deadline > 0 => deadline,
        _ => 0,
    }
}

fn ensure_deadline_pending(deadline_monotonic_ms: Option<i64>) -> Result<(), MediaError> {
    match deadline_monotonic_ms {
        Some(deadline) if deadline > 0 && metrics::now_ms() >= deadline => {
            Err(MediaError::RequestTimeout)
        }
        _ => Ok(()),
    }
}

fn native_animated_frame_limit(media_limits: &MediaLimits) -> c_int {
    media_limits.animated_frames().min(c_int::MAX as u32) as c_int
}

fn native_status_error(status: NativeStatus, operation_failure: MediaError) -> Option<MediaError> {
    match status {
        NativeStatus::Ok => None,
        NativeStatus::Unsupported | NativeStatus::CodecFailure => Some(operation_failure),
        NativeStatus::DeadlineExceeded => Some(MediaError::RequestTimeout),
        NativeStatus::WorkLimitExceeded | NativeStatus::OutputLimitExceeded => {
            Some(MediaError::StreamTooLong)
        }
        NativeStatus::InvalidDimensions => Some(MediaError::InvalidImageDimensions),
        NativeStatus::AllocationFailed => Some(MediaError::AllocationFailed),
    }
}

fn native_buffer_to_media_bytes(buffer: NativeBuffer) -> Result<MediaBytes, MediaError> {
    if buffer.len() > constants::MAX_MEDIA_PROXY_BYTES {
        return Err(MediaError::StreamTooLong);
    }
    Ok(MediaBytes::from_native(buffer))
}
