// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::constants::{
    MAX_ANIMATED_FRAMES_DEFAULT, MAX_ANIMATED_TOTAL_PIXELS_DEFAULT,
    MAX_MEDIA_IMAGE_DIMENSION_DEFAULT, MAX_MEDIA_IMAGE_PIXELS_DEFAULT, MAX_MEDIA_PROXY_BYTES,
};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct MediaLimits {
    image_dimension: u32,
    image_pixels: usize,
    animated_frames: u32,
    animated_total_pixels: usize,
    max_media_proxy_bytes: usize,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) struct MediaLimitConfiguration {
    pub image_dimension: u32,
    pub image_pixels: usize,
    pub animated_frames: u32,
    pub animated_total_pixels: usize,
    pub max_media_proxy_bytes: usize,
}

impl MediaLimits {
    pub(crate) fn new(configuration: MediaLimitConfiguration) -> anyhow::Result<Self> {
        let MediaLimitConfiguration {
            image_dimension,
            image_pixels,
            animated_frames,
            animated_total_pixels,
            max_media_proxy_bytes,
        } = configuration;
        anyhow::ensure!(
            (16..=MAX_MEDIA_IMAGE_DIMENSION_DEFAULT).contains(&image_dimension),
            "media image dimension must be between 16 and {MAX_MEDIA_IMAGE_DIMENSION_DEFAULT}"
        );
        anyhow::ensure!(
            (256..=MAX_MEDIA_IMAGE_PIXELS_DEFAULT).contains(&image_pixels),
            "media image pixels must be between 256 and {MAX_MEDIA_IMAGE_PIXELS_DEFAULT}"
        );
        anyhow::ensure!(animated_frames >= 1, "animated frame limit must be nonzero");
        anyhow::ensure!(
            i32::try_from(animated_frames).is_ok(),
            "animated frame limit must fit the native codec boundary"
        );
        anyhow::ensure!(
            (1024..=MAX_ANIMATED_TOTAL_PIXELS_DEFAULT).contains(&animated_total_pixels),
            "animated total pixel limit must be between 1024 and {MAX_ANIMATED_TOTAL_PIXELS_DEFAULT}"
        );
        anyhow::ensure!(
            (1..=MAX_MEDIA_PROXY_BYTES).contains(&max_media_proxy_bytes),
            "media proxy byte limit must be between 1 and {MAX_MEDIA_PROXY_BYTES}"
        );
        Ok(Self {
            image_dimension,
            image_pixels,
            animated_frames,
            animated_total_pixels,
            max_media_proxy_bytes,
        })
    }

    pub fn default_from_config() -> Self {
        Self::new(MediaLimitConfiguration {
            image_dimension: MAX_MEDIA_IMAGE_DIMENSION_DEFAULT,
            image_pixels: MAX_MEDIA_IMAGE_PIXELS_DEFAULT,
            animated_frames: MAX_ANIMATED_FRAMES_DEFAULT,
            animated_total_pixels: MAX_ANIMATED_TOTAL_PIXELS_DEFAULT,
            max_media_proxy_bytes: MAX_MEDIA_PROXY_BYTES,
        })
        .expect("compiled-in media limit defaults are always within their own bounds")
    }

    pub fn image_dimension(&self) -> u32 {
        self.image_dimension
    }

    pub fn image_pixels(&self) -> usize {
        self.image_pixels
    }

    pub fn animated_frames(&self) -> u32 {
        self.animated_frames
    }

    pub fn animated_total_pixels(&self) -> usize {
        self.animated_total_pixels
    }

    pub fn max_media_proxy_bytes(&self) -> usize {
        self.max_media_proxy_bytes
    }
}
