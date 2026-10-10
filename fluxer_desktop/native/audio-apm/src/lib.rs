// SPDX-License-Identifier: AGPL-3.0-or-later

#![deny(clippy::too_many_lines)]
#![deny(clippy::unwrap_used)]

pub const APM_FRAME_MS: u32 = 10;
pub const APM_MAX_SAMPLE_RATE: u32 = 48_000;
pub const APM_MAX_CHANNELS: u16 = 2;
pub const APM_MAX_FRAME_SAMPLES: usize =
    (APM_FRAME_MS as usize) * (APM_MAX_SAMPLE_RATE as usize) / 1000;

pub const APM_MIN_SAMPLE_RATE: u32 = 8_000;
pub const APM_MIN_CHANNELS: u16 = 1;

const _: () = assert!(APM_MAX_FRAME_SAMPLES == 480);
const _: () = assert!(APM_MIN_SAMPLE_RATE <= APM_MAX_SAMPLE_RATE);
const _: () = assert!(APM_MIN_CHANNELS <= APM_MAX_CHANNELS);

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ApmError {
    SampleRateOutOfRange {
        sample_rate_hz: u32,
    },
    SampleRateMismatch {
        expected_hz: u32,
        observed_hz: u32,
    },
    ChannelsOutOfRange {
        channels: u16,
    },
    ChannelsMismatch {
        expected: u16,
        observed: u16,
    },
    FrameLengthMismatch {
        expected_samples: usize,
        observed_samples: usize,
    },
    NotInitialized,
}

impl core::fmt::Display for ApmError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            ApmError::SampleRateOutOfRange { sample_rate_hz } => write!(
                f,
                "sample rate {sample_rate_hz} hz outside [{APM_MIN_SAMPLE_RATE}, {APM_MAX_SAMPLE_RATE}]",
            ),
            ApmError::SampleRateMismatch {
                expected_hz,
                observed_hz,
            } => write!(
                f,
                "sample rate mismatch: expected={expected_hz} observed={observed_hz}",
            ),
            ApmError::ChannelsOutOfRange { channels } => write!(
                f,
                "channel count {channels} outside [{APM_MIN_CHANNELS}, {APM_MAX_CHANNELS}]",
            ),
            ApmError::ChannelsMismatch { expected, observed } => write!(
                f,
                "channels mismatch: expected={expected} observed={observed}",
            ),
            ApmError::FrameLengthMismatch {
                expected_samples,
                observed_samples,
            } => write!(
                f,
                "frame length mismatch: expected={expected_samples} observed={observed_samples}",
            ),
            ApmError::NotInitialized => write!(f, "audio processor not initialized"),
        }
    }
}

impl std::error::Error for ApmError {}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct AecMetrics {
    pub echo_return_loss_db: f32,
    pub echo_return_loss_enhancement_db: f32,
    pub delay_ms: i32,
}

impl AecMetrics {
    pub const NEUTRAL: AecMetrics = AecMetrics {
        echo_return_loss_db: 0.0,
        echo_return_loss_enhancement_db: 0.0,
        delay_ms: 0,
    };
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ApmReport {
    pub aec_metrics: AecMetrics,
    pub voice_detected: bool,
    pub level_dbfs: f32,
}

impl ApmReport {
    pub const NEUTRAL: ApmReport = ApmReport {
        aec_metrics: AecMetrics::NEUTRAL,
        voice_detected: false,
        level_dbfs: -120.0,
    };
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ApmConfig {
    pub aec_enabled: bool,
    pub ns_enabled: bool,
    pub agc_enabled: bool,
    pub aec_mobile_mode: bool,
    pub target_level_dbfs: i32,
}

impl Default for ApmConfig {
    fn default() -> Self {
        ApmConfig {
            aec_enabled: true,
            ns_enabled: true,
            agc_enabled: true,
            aec_mobile_mode: false,
            target_level_dbfs: -3,
        }
    }
}

pub struct ApmConfigBuilder {
    config: ApmConfig,
}

impl ApmConfigBuilder {
    pub fn new() -> Self {
        ApmConfigBuilder {
            config: ApmConfig::default(),
        }
    }

    pub fn aec(mut self, enabled: bool) -> Self {
        self.config.aec_enabled = enabled;
        self
    }

    pub fn ns(mut self, enabled: bool) -> Self {
        self.config.ns_enabled = enabled;
        self
    }

    pub fn agc(mut self, enabled: bool) -> Self {
        self.config.agc_enabled = enabled;
        self
    }

    pub fn aec_mobile_mode(mut self, enabled: bool) -> Self {
        self.config.aec_mobile_mode = enabled;
        self
    }

    pub fn target_level_dbfs(mut self, target: i32) -> Self {
        self.config.target_level_dbfs = target;
        self
    }

    pub fn build(self) -> ApmConfig {
        assert!(self.config.target_level_dbfs <= 0);
        assert!(self.config.target_level_dbfs >= -60);
        self.config
    }
}

impl Default for ApmConfigBuilder {
    fn default() -> Self {
        ApmConfigBuilder::new()
    }
}

pub trait AudioProcessor: Send {
    fn process_capture_frame(
        &mut self,
        samples: &mut [i16],
        sample_rate_hz: u32,
        channels: u16,
    ) -> Result<ApmReport, ApmError>;

    fn process_render_frame(
        &mut self,
        samples: &[i16],
        sample_rate_hz: u32,
        channels: u16,
    ) -> Result<(), ApmError>;

    fn reset(&mut self) -> Result<(), ApmError>;
}

pub fn expected_frame_samples(sample_rate_hz: u32, channels: u16) -> usize {
    assert!(sample_rate_hz >= APM_MIN_SAMPLE_RATE);
    assert!(sample_rate_hz <= APM_MAX_SAMPLE_RATE);
    assert!(channels >= APM_MIN_CHANNELS);
    assert!(channels <= APM_MAX_CHANNELS);
    let per_channel = (APM_FRAME_MS as usize) * (sample_rate_hz as usize) / 1000;
    per_channel * (channels as usize)
}

pub(crate) fn validate_frame_shape(
    samples_len: usize,
    sample_rate_hz: u32,
    channels: u16,
    expected_sample_rate_hz: u32,
    expected_channels: u16,
) -> Result<(), ApmError> {
    assert!(expected_sample_rate_hz >= APM_MIN_SAMPLE_RATE);
    assert!(expected_sample_rate_hz <= APM_MAX_SAMPLE_RATE);
    if !(APM_MIN_SAMPLE_RATE..=APM_MAX_SAMPLE_RATE).contains(&sample_rate_hz) {
        return Err(ApmError::SampleRateOutOfRange { sample_rate_hz });
    }
    if !(APM_MIN_CHANNELS..=APM_MAX_CHANNELS).contains(&channels) {
        return Err(ApmError::ChannelsOutOfRange { channels });
    }
    if sample_rate_hz != expected_sample_rate_hz {
        return Err(ApmError::SampleRateMismatch {
            expected_hz: expected_sample_rate_hz,
            observed_hz: sample_rate_hz,
        });
    }
    if channels != expected_channels {
        return Err(ApmError::ChannelsMismatch {
            expected: expected_channels,
            observed: channels,
        });
    }
    let expected_samples = expected_frame_samples(sample_rate_hz, channels);
    if samples_len != expected_samples {
        return Err(ApmError::FrameLengthMismatch {
            expected_samples,
            observed_samples: samples_len,
        });
    }
    Ok(())
}

#[derive(Debug)]
pub struct StubAudioProcessor {
    config: ApmConfig,
    expected_sample_rate_hz: u32,
    expected_channels: u16,
    capture_frames_processed: u64,
    render_frames_processed: u64,
}

impl StubAudioProcessor {
    pub fn new(config: ApmConfig, sample_rate_hz: u32, channels: u16) -> Result<Self, ApmError> {
        if !(APM_MIN_SAMPLE_RATE..=APM_MAX_SAMPLE_RATE).contains(&sample_rate_hz) {
            return Err(ApmError::SampleRateOutOfRange { sample_rate_hz });
        }
        if !(APM_MIN_CHANNELS..=APM_MAX_CHANNELS).contains(&channels) {
            return Err(ApmError::ChannelsOutOfRange { channels });
        }
        assert!(sample_rate_hz >= APM_MIN_SAMPLE_RATE);
        assert!(channels >= APM_MIN_CHANNELS);
        Ok(StubAudioProcessor {
            config,
            expected_sample_rate_hz: sample_rate_hz,
            expected_channels: channels,
            capture_frames_processed: 0,
            render_frames_processed: 0,
        })
    }

    pub fn config(&self) -> ApmConfig {
        self.config
    }

    pub fn capture_frames_processed(&self) -> u64 {
        self.capture_frames_processed
    }

    pub fn render_frames_processed(&self) -> u64 {
        self.render_frames_processed
    }
}

impl AudioProcessor for StubAudioProcessor {
    fn process_capture_frame(
        &mut self,
        samples: &mut [i16],
        sample_rate_hz: u32,
        channels: u16,
    ) -> Result<ApmReport, ApmError> {
        assert!(self.expected_sample_rate_hz >= APM_MIN_SAMPLE_RATE);
        assert!(self.expected_channels >= APM_MIN_CHANNELS);
        validate_frame_shape(
            samples.len(),
            sample_rate_hz,
            channels,
            self.expected_sample_rate_hz,
            self.expected_channels,
        )?;
        assert!(self.capture_frames_processed < u64::MAX);
        self.capture_frames_processed = self.capture_frames_processed.saturating_add(1);
        Ok(ApmReport::NEUTRAL)
    }

    fn process_render_frame(
        &mut self,
        samples: &[i16],
        sample_rate_hz: u32,
        channels: u16,
    ) -> Result<(), ApmError> {
        assert!(self.expected_sample_rate_hz >= APM_MIN_SAMPLE_RATE);
        assert!(self.expected_channels >= APM_MIN_CHANNELS);
        validate_frame_shape(
            samples.len(),
            sample_rate_hz,
            channels,
            self.expected_sample_rate_hz,
            self.expected_channels,
        )?;
        assert!(self.render_frames_processed < u64::MAX);
        self.render_frames_processed = self.render_frames_processed.saturating_add(1);
        Ok(())
    }

    fn reset(&mut self) -> Result<(), ApmError> {
        assert!(self.expected_sample_rate_hz >= APM_MIN_SAMPLE_RATE);
        assert!(self.expected_channels >= APM_MIN_CHANNELS);
        self.capture_frames_processed = 0;
        self.render_frames_processed = 0;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_frame(sample_rate_hz: u32, channels: u16) -> Vec<i16> {
        let n = expected_frame_samples(sample_rate_hz, channels);
        (0..n).map(|i| (i as i16).wrapping_mul(7)).collect()
    }

    #[test]
    fn rejects_mismatched_sample_rate() {
        let mut stub = StubAudioProcessor::new(ApmConfig::default(), 48_000, 1).expect("ctor");
        let mut samples = make_frame(16_000, 1);
        let err = stub
            .process_capture_frame(&mut samples, 16_000, 1)
            .expect_err("err");
        assert!(matches!(err, ApmError::SampleRateMismatch { .. }));
    }

    #[test]
    fn rejects_mismatched_channels() {
        let mut stub = StubAudioProcessor::new(ApmConfig::default(), 48_000, 1).expect("ctor");
        let mut samples = make_frame(48_000, 2);
        let err = stub
            .process_capture_frame(&mut samples, 48_000, 2)
            .expect_err("err");
        assert!(matches!(err, ApmError::ChannelsMismatch { .. }));
    }

    #[test]
    fn rejects_wrong_frame_length() {
        let mut stub = StubAudioProcessor::new(ApmConfig::default(), 48_000, 1).expect("ctor");
        let mut samples = vec![0i16; 100];
        let err = stub
            .process_capture_frame(&mut samples, 48_000, 1)
            .expect_err("err");
        assert!(matches!(err, ApmError::FrameLengthMismatch { .. }));
    }

    #[test]
    fn rejects_render_frame_length() {
        let mut stub = StubAudioProcessor::new(ApmConfig::default(), 48_000, 1).expect("ctor");
        let samples = vec![0i16; 99];
        let err = stub
            .process_render_frame(&samples, 48_000, 1)
            .expect_err("err");
        assert!(matches!(err, ApmError::FrameLengthMismatch { .. }));
    }

    #[test]
    fn rejects_sample_rate_out_of_range_on_construct() {
        let err = StubAudioProcessor::new(ApmConfig::default(), 4_000, 1).expect_err("err");
        assert!(matches!(err, ApmError::SampleRateOutOfRange { .. }));
    }

    #[test]
    fn rejects_channels_out_of_range_on_construct() {
        let err = StubAudioProcessor::new(ApmConfig::default(), 48_000, 4).expect_err("err");
        assert!(matches!(err, ApmError::ChannelsOutOfRange { .. }));
    }

    #[test]
    fn state_preserved_across_many_frames() {
        let mut stub = StubAudioProcessor::new(ApmConfig::default(), 48_000, 1).expect("ctor");
        let mut samples = make_frame(48_000, 1);
        const N: u64 = 100;
        for _ in 0..N {
            stub.process_capture_frame(&mut samples, 48_000, 1)
                .expect("ok");
        }
        assert_eq!(stub.capture_frames_processed(), N);
        let render = vec![0i16; expected_frame_samples(48_000, 1)];
        for _ in 0..N {
            stub.process_render_frame(&render, 48_000, 1).expect("ok");
        }
        assert_eq!(stub.render_frames_processed(), N);
    }

    #[test]
    fn stereo_round_trip_capture_succeeds() {
        let mut stub = StubAudioProcessor::new(ApmConfig::default(), 48_000, 2).expect("ctor");
        let mut samples = make_frame(48_000, 2);
        let original = samples.clone();
        let report = stub
            .process_capture_frame(&mut samples, 48_000, 2)
            .expect("ok");
        assert_eq!(samples, original);
        assert_eq!(report, ApmReport::NEUTRAL);
    }

    #[test]
    fn validate_frame_shape_accepts_canonical_48k_mono() {
        validate_frame_shape(480, 48_000, 1, 48_000, 1).expect("ok");
    }
}
