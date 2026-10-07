#![deny(clippy::all)]
// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(dead_code)]
#![allow(clippy::missing_safety_doc)]
#![allow(clippy::collapsible_if)]
#![allow(clippy::manual_is_multiple_of)]
#![allow(clippy::manual_slice_size_calculation)]
#![allow(clippy::unnecessary_cast)]
#![allow(clippy::not_unsafe_ptr_arg_deref)]
#![allow(clippy::missing_transmute_annotations)]
#![allow(clippy::missing_const_for_thread_local)]
#![allow(clippy::too_many_arguments)]

pub mod audio_pool;
pub mod config;
pub mod encoder_attach;
pub mod iosurface_pair;
pub mod os_version;
pub mod permission_probe;

pub use audio_pool::{
    MAC_AUDIO_POOL_CAP, MAX_FRAME_BYTES_PER_SLOT, MacAudioError, MacAudioFramePool,
    MacAudioPoolStats, PooledMacAudioFrame,
};
pub use config::{
    AUDIO_CHANNEL_COUNT_DEFAULT, AUDIO_CHANNEL_COUNT_MAX, AUDIO_CHANNEL_COUNT_MIN,
    AUDIO_SAMPLE_RATE_DEFAULT_HZ, AUDIO_SAMPLE_RATE_MAX_HZ, AUDIO_SAMPLE_RATE_MIN_HZ,
    AudioSampleFormat, CaptureFailureSurface, FPS_DEFAULT, FPS_MAX, FPS_MIN,
    MacScreenShareAudioFrame, MacScreenShareAudioFrameWithBytes, QUEUE_DEPTH_DEFAULT,
    QUEUE_DEPTH_MAX, QUEUE_DEPTH_MIN, SckCaptureConfig, SckCaptureConfigBuilder, SckCaptureFailure,
    SckColorSpace, SckError, SckPixelFormat,
};
pub use encoder_attach::{EncoderAttachError, EncoderAttachStats, EncoderAttachment};
pub use iosurface_pair::{IoSurfacePair, IoSurfaceRaw};

#[cfg(target_os = "macos")]
pub mod foundation;
#[cfg(target_os = "macos")]
pub mod sck;

#[cfg(target_os = "macos")]
mod napi_surface_macos;

#[cfg(not(target_os = "macos"))]
mod napi_surface_stub;
