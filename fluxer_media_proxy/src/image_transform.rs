// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{image_quality::ImageQuality, output_format::OutputFormat};
use std::num::NonZeroU32;

pub const MAX_ENCODE_EFFORT: u8 = 9;

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct EncodeEffort(u8);

impl EncodeEffort {
    pub fn parse_lenient(raw: &str) -> Option<Self> {
        if raw.is_empty() {
            return None;
        }
        raw.parse::<u8>()
            .ok()
            .map(|value| Self(value.min(MAX_ENCODE_EFFORT)))
    }

    pub const fn minimum() -> Self {
        Self(0)
    }

    pub const fn get(self) -> u8 {
        self.0
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ResizeMode {
    Fit,
    Cover,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct AnimationLimits {
    max_frames: NonZeroU32,
    max_duration_ms: NonZeroU32,
}

impl AnimationLimits {
    pub fn new(max_frames: u32, max_duration_ms: u32) -> anyhow::Result<Self> {
        let max_frames = NonZeroU32::new(max_frames)
            .ok_or_else(|| anyhow::anyhow!("animation encode frame limit must be nonzero"))?;
        let max_duration_ms = NonZeroU32::new(max_duration_ms)
            .ok_or_else(|| anyhow::anyhow!("animation encode duration limit must be nonzero"))?;
        anyhow::ensure!(
            max_frames.get() <= i32::MAX as u32,
            "animation encode frame limit must fit the native codec boundary"
        );
        anyhow::ensure!(
            max_duration_ms.get() <= i32::MAX as u32,
            "animation encode duration limit must fit the native codec boundary"
        );
        Ok(Self {
            max_frames,
            max_duration_ms,
        })
    }

    pub const fn single_frame() -> Self {
        Self {
            max_frames: NonZeroU32::MIN,
            max_duration_ms: NonZeroU32::MIN,
        }
    }

    pub const fn max_frames(self) -> NonZeroU32 {
        self.max_frames
    }

    pub const fn max_duration_ms(self) -> NonZeroU32 {
        self.max_duration_ms
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum AnimationMode {
    Static,
    Animated(AnimationLimits),
}

impl AnimationMode {
    pub const fn new(animated: bool, limits: AnimationLimits) -> Self {
        if animated {
            Self::Animated(limits)
        } else {
            Self::Static
        }
    }

    pub const fn is_animated(self) -> bool {
        matches!(self, Self::Animated(_))
    }

    pub const fn encode_limits(self) -> AnimationLimits {
        match self {
            Self::Static => AnimationLimits::single_frame(),
            Self::Animated(limits) => limits,
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct ImageOptions {
    pub width: Option<u32>,
    pub height: Option<u32>,
    pub format: OutputFormat,
    pub quality: ImageQuality,
    pub animation: AnimationMode,
    pub effort_override: Option<EncodeEffort>,
    pub resize_mode: ResizeMode,
    pub deadline_ms: Option<i64>,
}

impl ImageOptions {
    pub const fn is_animated(&self) -> bool {
        self.animation.is_animated()
    }

    pub const fn wants_cover_crop(&self) -> bool {
        matches!(self.resize_mode, ResizeMode::Cover)
    }
}

impl Default for ImageOptions {
    fn default() -> Self {
        Self {
            width: None,
            height: None,
            format: OutputFormat::WebP,
            quality: ImageQuality::High,
            animation: AnimationMode::Static,
            effort_override: None,
            resize_mode: ResizeMode::Fit,
            deadline_ms: None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn effort_parsing_stays_lenient_and_clamps_at_nine() {
        assert_eq!(None, EncodeEffort::parse_lenient(""));
        assert_eq!(None, EncodeEffort::parse_lenient("not-a-number"));
        assert_eq!(None, EncodeEffort::parse_lenient("-1"));
        assert_eq!(
            Some(0),
            EncodeEffort::parse_lenient("0").map(EncodeEffort::get)
        );
        assert_eq!(
            Some(6),
            EncodeEffort::parse_lenient("6").map(EncodeEffort::get)
        );
        assert_eq!(
            Some(9),
            EncodeEffort::parse_lenient("9").map(EncodeEffort::get)
        );
        assert_eq!(
            Some(9),
            EncodeEffort::parse_lenient("250").map(EncodeEffort::get)
        );
        assert_eq!(None, EncodeEffort::parse_lenient("256"));
        assert_eq!(0, EncodeEffort::minimum().get());
    }

    #[test]
    fn animation_limits_reject_zero_and_out_of_range_bounds() {
        assert!(AnimationLimits::new(0, 30_000).is_err());
        assert!(AnimationLimits::new(4096, 0).is_err());
        assert!(AnimationLimits::new(u32::MAX, 30_000).is_err());
        assert!(AnimationLimits::new(4096, u32::MAX).is_err());
        let limits = AnimationLimits::new(4096, 30_000).expect("valid animation limits");
        assert_eq!(4096, limits.max_frames().get());
        assert_eq!(30_000, limits.max_duration_ms().get());
    }

    #[test]
    fn static_mode_collapses_to_a_single_frame_budget() {
        let limits = AnimationLimits::new(4096, 30_000).expect("valid animation limits");
        assert!(!AnimationMode::new(false, limits).is_animated());
        assert!(AnimationMode::new(true, limits).is_animated());
        assert_eq!(
            AnimationLimits::single_frame(),
            AnimationMode::Static.encode_limits()
        );
        assert_eq!(1, AnimationLimits::single_frame().max_frames().get());
        assert_eq!(1, AnimationLimits::single_frame().max_duration_ms().get());
        assert_eq!(limits, AnimationMode::Animated(limits).encode_limits());
    }

    #[test]
    fn default_options_render_a_static_high_quality_webp() {
        let options = ImageOptions::default();
        assert_eq!(OutputFormat::WebP, options.format);
        assert_eq!(ImageQuality::High, options.quality);
        assert!(!options.is_animated());
        assert!(!options.wants_cover_crop());
        assert_eq!(None, options.effort_override);
        assert_eq!(None, options.deadline_ms);
    }
}
