// SPDX-License-Identifier: AGPL-3.0-or-later

use std::fmt;

pub const FPS_MIN: u32 = 1;
pub const FPS_MAX: u32 = 120;
pub const MAX_OUTPUT_WIDTH_DEFAULT: u32 = 3840;
pub const MAX_OUTPUT_HEIGHT_DEFAULT: u32 = 2160;
pub const OUTPUT_DIMENSION_MIN: u32 = 2;
pub const QUEUE_DEPTH_MIN: u32 = 1;
pub const QUEUE_DEPTH_MAX: u32 = 16;
pub const QUEUE_DEPTH_DEFAULT: u32 = 8;
pub const FPS_DEFAULT: u32 = 30;
pub const FRAME_INTERVAL_FACTOR_NUM: u64 = 9;
pub const FRAME_INTERVAL_FACTOR_DEN: u64 = 10;

pub const PIXEL_FORMAT_BGRA_FOURCC: u32 = u32::from_be_bytes(*b"BGRA");
pub const PIXEL_FORMAT_L10R_FOURCC: u32 = u32::from_be_bytes(*b"l10r");
pub const PIXEL_FORMAT_420V_FOURCC: u32 = u32::from_be_bytes(*b"420v");
pub const PIXEL_FORMAT_420F_FOURCC: u32 = u32::from_be_bytes(*b"420f");

pub const AUDIO_SAMPLE_RATE_DEFAULT_HZ: u32 = 48_000;
pub const AUDIO_CHANNEL_COUNT_DEFAULT: u32 = 2;
pub const AUDIO_SAMPLE_RATE_MIN_HZ: u32 = 8_000;
pub const AUDIO_SAMPLE_RATE_MAX_HZ: u32 = 192_000;
pub const AUDIO_CHANNEL_COUNT_MIN: u32 = 1;
pub const AUDIO_CHANNEL_COUNT_MAX: u32 = 8;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SckPixelFormat {
    Bgra8,
    L10rHdr,
    Nv12VideoRange,
    Nv12FullRange,
}

impl SckPixelFormat {
    pub fn as_fourcc(self) -> u32 {
        let value = match self {
            SckPixelFormat::Bgra8 => PIXEL_FORMAT_BGRA_FOURCC,
            SckPixelFormat::L10rHdr => PIXEL_FORMAT_L10R_FOURCC,
            SckPixelFormat::Nv12VideoRange => PIXEL_FORMAT_420V_FOURCC,
            SckPixelFormat::Nv12FullRange => PIXEL_FORMAT_420F_FOURCC,
        };
        assert!(value != 0);
        value
    }

    pub fn is_hdr(self) -> bool {
        matches!(self, SckPixelFormat::L10rHdr)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SckColorSpace {
    DisplayP3,
    SrgbBt709,
}

impl SckColorSpace {
    pub fn as_cf_name(self) -> &'static str {
        let name = match self {
            SckColorSpace::DisplayP3 => "kCGColorSpaceDisplayP3",
            SckColorSpace::SrgbBt709 => "kCGColorSpaceSRGB",
        };
        assert!(!name.is_empty());
        name
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SckError {
    InvalidFps(u32),
    InvalidQueueDepth(u32),
    HdrRequiresWideColorSpace,
    InvalidAudioSampleRate(u32),
    InvalidAudioChannelCount(u32),
}

impl fmt::Display for SckError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SckError::InvalidFps(v) => write!(
                f,
                "SckCaptureConfig: target_fps={v} out of range [{FPS_MIN}..={FPS_MAX}]"
            ),
            SckError::InvalidQueueDepth(v) => write!(
                f,
                "SckCaptureConfig: queue_depth={v} out of range [{QUEUE_DEPTH_MIN}..={QUEUE_DEPTH_MAX}]"
            ),
            SckError::HdrRequiresWideColorSpace => write!(
                f,
                "SckCaptureConfig: l10r HDR pixel format requires DisplayP3 color space"
            ),
            SckError::InvalidAudioSampleRate(v) => write!(
                f,
                "SckCaptureConfig: audio_sample_rate_hz={v} out of range [{AUDIO_SAMPLE_RATE_MIN_HZ}..={AUDIO_SAMPLE_RATE_MAX_HZ}]"
            ),
            SckError::InvalidAudioChannelCount(v) => write!(
                f,
                "SckCaptureConfig: audio_channels={v} out of range [{AUDIO_CHANNEL_COUNT_MIN}..={AUDIO_CHANNEL_COUNT_MAX}]"
            ),
        }
    }
}

impl std::error::Error for SckError {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SckCaptureConfig {
    target_fps: u32,
    queue_depth: u32,
    pixel_format: SckPixelFormat,
    color_space: SckColorSpace,
    captures_audio: bool,
    audio_sample_rate_hz: u32,
    audio_channels: u32,
}

impl SckCaptureConfig {
    pub fn new(
        target_fps: u32,
        queue_depth: u32,
        pixel_format: SckPixelFormat,
        color_space: SckColorSpace,
    ) -> Result<Self, SckError> {
        Self::new_with_audio(
            target_fps,
            queue_depth,
            pixel_format,
            color_space,
            false,
            AUDIO_SAMPLE_RATE_DEFAULT_HZ,
            AUDIO_CHANNEL_COUNT_DEFAULT,
        )
    }

    pub fn new_with_audio(
        target_fps: u32,
        queue_depth: u32,
        pixel_format: SckPixelFormat,
        color_space: SckColorSpace,
        captures_audio: bool,
        audio_sample_rate_hz: u32,
        audio_channels: u32,
    ) -> Result<Self, SckError> {
        if !(FPS_MIN..=FPS_MAX).contains(&target_fps) {
            return Err(SckError::InvalidFps(target_fps));
        }
        if !(QUEUE_DEPTH_MIN..=QUEUE_DEPTH_MAX).contains(&queue_depth) {
            return Err(SckError::InvalidQueueDepth(queue_depth));
        }
        if pixel_format.is_hdr() && color_space != SckColorSpace::DisplayP3 {
            return Err(SckError::HdrRequiresWideColorSpace);
        }
        if !(AUDIO_SAMPLE_RATE_MIN_HZ..=AUDIO_SAMPLE_RATE_MAX_HZ).contains(&audio_sample_rate_hz) {
            return Err(SckError::InvalidAudioSampleRate(audio_sample_rate_hz));
        }
        if !(AUDIO_CHANNEL_COUNT_MIN..=AUDIO_CHANNEL_COUNT_MAX).contains(&audio_channels) {
            return Err(SckError::InvalidAudioChannelCount(audio_channels));
        }
        let cfg = Self {
            target_fps,
            queue_depth,
            pixel_format,
            color_space,
            captures_audio,
            audio_sample_rate_hz,
            audio_channels,
        };
        assert!(cfg.target_fps >= FPS_MIN);
        assert!(cfg.target_fps <= FPS_MAX);
        assert!(cfg.queue_depth >= QUEUE_DEPTH_MIN);
        assert!(cfg.queue_depth <= QUEUE_DEPTH_MAX);
        assert!(cfg.audio_sample_rate_hz >= AUDIO_SAMPLE_RATE_MIN_HZ);
        assert!(cfg.audio_channels >= AUDIO_CHANNEL_COUNT_MIN);
        Ok(cfg)
    }

    pub fn builder() -> SckCaptureConfigBuilder {
        SckCaptureConfigBuilder::default()
    }

    pub fn target_fps(&self) -> u32 {
        assert!(self.target_fps >= FPS_MIN);
        assert!(self.target_fps <= FPS_MAX);
        self.target_fps
    }

    pub fn queue_depth(&self) -> u32 {
        assert!(self.queue_depth >= QUEUE_DEPTH_MIN);
        assert!(self.queue_depth <= QUEUE_DEPTH_MAX);
        self.queue_depth
    }

    pub fn pixel_format(&self) -> SckPixelFormat {
        let pf = self.pixel_format;
        assert!(pf.as_fourcc() != 0);
        pf
    }

    pub fn color_space(&self) -> SckColorSpace {
        let cs = self.color_space;
        assert!(!cs.as_cf_name().is_empty());
        cs
    }

    pub fn minimum_frame_interval_ns(&self) -> u64 {
        assert!(self.target_fps >= FPS_MIN);
        assert!(self.target_fps <= FPS_MAX);
        let base_ns: u64 = 1_000_000_000 / (self.target_fps as u64);
        let scaled = base_ns * FRAME_INTERVAL_FACTOR_NUM / FRAME_INTERVAL_FACTOR_DEN;
        assert!(scaled > 0);
        assert!(scaled <= 1_000_000_000);
        scaled
    }

    pub fn captures_audio(&self) -> bool {
        self.captures_audio
    }

    pub fn audio_sample_rate_hz(&self) -> u32 {
        assert!(self.audio_sample_rate_hz >= AUDIO_SAMPLE_RATE_MIN_HZ);
        assert!(self.audio_sample_rate_hz <= AUDIO_SAMPLE_RATE_MAX_HZ);
        self.audio_sample_rate_hz
    }

    pub fn audio_channels(&self) -> u32 {
        assert!(self.audio_channels >= AUDIO_CHANNEL_COUNT_MIN);
        assert!(self.audio_channels <= AUDIO_CHANNEL_COUNT_MAX);
        self.audio_channels
    }
}

impl Default for SckCaptureConfig {
    fn default() -> Self {
        let cfg = Self::new_with_audio(
            FPS_DEFAULT,
            QUEUE_DEPTH_DEFAULT,
            SckPixelFormat::Nv12VideoRange,
            SckColorSpace::SrgbBt709,
            false,
            AUDIO_SAMPLE_RATE_DEFAULT_HZ,
            AUDIO_CHANNEL_COUNT_DEFAULT,
        );
        match cfg {
            Ok(c) => c,
            Err(_) => unreachable!("default SckCaptureConfig must validate"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct SckCaptureConfigBuilder {
    target_fps: u32,
    queue_depth: u32,
    pixel_format: SckPixelFormat,
    color_space: SckColorSpace,
    captures_audio: bool,
    audio_sample_rate_hz: u32,
    audio_channels: u32,
}

impl Default for SckCaptureConfigBuilder {
    fn default() -> Self {
        Self {
            target_fps: FPS_DEFAULT,
            queue_depth: QUEUE_DEPTH_DEFAULT,
            pixel_format: SckPixelFormat::Nv12VideoRange,
            color_space: SckColorSpace::SrgbBt709,
            captures_audio: false,
            audio_sample_rate_hz: AUDIO_SAMPLE_RATE_DEFAULT_HZ,
            audio_channels: AUDIO_CHANNEL_COUNT_DEFAULT,
        }
    }
}

impl SckCaptureConfigBuilder {
    pub fn target_fps(mut self, target_fps: u32) -> Self {
        self.target_fps = target_fps;
        self
    }

    pub fn queue_depth(mut self, queue_depth: u32) -> Self {
        self.queue_depth = queue_depth;
        self
    }

    pub fn pixel_format(mut self, pixel_format: SckPixelFormat) -> Self {
        self.pixel_format = pixel_format;
        self
    }

    pub fn color_space(mut self, color_space: SckColorSpace) -> Self {
        self.color_space = color_space;
        self
    }

    pub fn captures_audio(mut self, captures_audio: bool) -> Self {
        self.captures_audio = captures_audio;
        self
    }

    pub fn audio_sample_rate_hz(mut self, audio_sample_rate_hz: u32) -> Self {
        self.audio_sample_rate_hz = audio_sample_rate_hz;
        self
    }

    pub fn audio_channels(mut self, audio_channels: u32) -> Self {
        self.audio_channels = audio_channels;
        self
    }

    pub fn build(self) -> Result<SckCaptureConfig, SckError> {
        SckCaptureConfig::new_with_audio(
            self.target_fps,
            self.queue_depth,
            self.pixel_format,
            self.color_space,
            self.captures_audio,
            self.audio_sample_rate_hz,
            self.audio_channels,
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SckCaptureFailure {
    StreamStoppedWithError(String),
    StreamStartFailed(String),
    SystemDeniedAccess,
    DisplayDisconnected,
    Unknown(String),
}

impl SckCaptureFailure {
    pub fn reason(&self) -> &str {
        match self {
            SckCaptureFailure::StreamStoppedWithError(m) => m.as_str(),
            SckCaptureFailure::StreamStartFailed(m) => m.as_str(),
            SckCaptureFailure::SystemDeniedAccess => "screen recording permission denied",
            SckCaptureFailure::DisplayDisconnected => "captured display was disconnected",
            SckCaptureFailure::Unknown(m) => m.as_str(),
        }
    }
}

pub trait CaptureFailureSurface: Send + Sync {
    fn on_failure(&self, reason: SckCaptureFailure);
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum AudioSampleFormat {
    F32Planar = 0,
    F32Interleaved = 1,
    I16Interleaved = 2,
    Unknown = 3,
}

pub const AUDIO_SAMPLE_FORMAT_CODE_MAX: u32 = 3;

impl AudioSampleFormat {
    pub fn code(self) -> u32 {
        let code = match self {
            AudioSampleFormat::F32Planar => 0,
            AudioSampleFormat::F32Interleaved => 1,
            AudioSampleFormat::I16Interleaved => 2,
            AudioSampleFormat::Unknown => 3,
        };
        assert!(code <= AUDIO_SAMPLE_FORMAT_CODE_MAX);
        assert_eq!(code, self as u32);
        code
    }

    pub fn as_str(self) -> &'static str {
        let s = match self {
            AudioSampleFormat::F32Planar => "f32_planar",
            AudioSampleFormat::F32Interleaved => "f32_interleaved",
            AudioSampleFormat::I16Interleaved => "i16_interleaved",
            AudioSampleFormat::Unknown => "unknown",
        };
        assert!(!s.is_empty());
        s
    }

    pub fn bytes_per_sample(self) -> u32 {
        let bytes = match self {
            AudioSampleFormat::F32Planar => 4,
            AudioSampleFormat::F32Interleaved => 4,
            AudioSampleFormat::I16Interleaved => 2,
            AudioSampleFormat::Unknown => 0,
        };
        assert!(bytes <= 4);
        bytes
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct MacScreenShareAudioFrame {
    pub sample_rate_hz: u32,
    pub channels: u32,
    pub num_samples_per_channel: u32,
    pub pts_us: i64,
}

impl MacScreenShareAudioFrame {
    pub fn new(
        sample_rate_hz: u32,
        channels: u32,
        num_samples_per_channel: u32,
        pts_us: i64,
    ) -> Result<Self, SckError> {
        if !(AUDIO_SAMPLE_RATE_MIN_HZ..=AUDIO_SAMPLE_RATE_MAX_HZ).contains(&sample_rate_hz) {
            return Err(SckError::InvalidAudioSampleRate(sample_rate_hz));
        }
        if !(AUDIO_CHANNEL_COUNT_MIN..=AUDIO_CHANNEL_COUNT_MAX).contains(&channels) {
            return Err(SckError::InvalidAudioChannelCount(channels));
        }
        assert!(sample_rate_hz >= AUDIO_SAMPLE_RATE_MIN_HZ);
        assert!(channels >= AUDIO_CHANNEL_COUNT_MIN);
        Ok(Self {
            sample_rate_hz,
            channels,
            num_samples_per_channel,
            pts_us,
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct MacScreenShareAudioFrameWithBytes {
    pub sample_rate_hz: u32,
    pub channels: u32,
    pub num_samples_per_channel: u32,
    pub pts_us: i64,
    pub format: AudioSampleFormat,
    pub samples: Vec<u8>,
}

impl MacScreenShareAudioFrameWithBytes {
    pub fn new(
        sample_rate_hz: u32,
        channels: u32,
        num_samples_per_channel: u32,
        pts_us: i64,
        format: AudioSampleFormat,
        samples: Vec<u8>,
    ) -> Result<Self, SckError> {
        if !(AUDIO_SAMPLE_RATE_MIN_HZ..=AUDIO_SAMPLE_RATE_MAX_HZ).contains(&sample_rate_hz) {
            return Err(SckError::InvalidAudioSampleRate(sample_rate_hz));
        }
        if !(AUDIO_CHANNEL_COUNT_MIN..=AUDIO_CHANNEL_COUNT_MAX).contains(&channels) {
            return Err(SckError::InvalidAudioChannelCount(channels));
        }
        assert!(sample_rate_hz >= AUDIO_SAMPLE_RATE_MIN_HZ);
        assert!(channels >= AUDIO_CHANNEL_COUNT_MIN);
        Ok(Self {
            sample_rate_hz,
            channels,
            num_samples_per_channel,
            pts_us,
            format,
            samples,
        })
    }
}
