#![allow(dead_code)]

// SPDX-License-Identifier: AGPL-3.0-or-later

pub const DIRECT_CAPTURE_SAMPLE_RATE: u32 = 48_000;
pub const DIRECT_CAPTURE_CHANNELS: u32 = 2;
pub const DIRECT_CAPTURE_LATENCY_FRAMES: u32 = 4_096;
pub const DIRECT_CAPTURE_READ_CHUNK_US: u32 = 20_000;
pub const DIRECT_CAPTURE_MAX_SAMPLES: usize =
    DIRECT_CAPTURE_SAMPLE_RATE as usize * DIRECT_CAPTURE_CHANNELS as usize * 2;
pub const DIRECT_CAPTURE_MAX_READ_SAMPLES: usize =
    DIRECT_CAPTURE_SAMPLE_RATE as usize * DIRECT_CAPTURE_CHANNELS as usize / 50;
pub const MAX_ROUTING_RULE_PATTERNS: u32 = 64;
pub const MAX_ROUTING_RULE_KEYS_PER_PATTERN: u32 = 32;
pub const MAX_ROUTING_RULE_KEY_LENGTH: usize = 128;
pub const MAX_ROUTING_RULE_VALUE_LENGTH: usize = 512;
pub const MAX_INVENTORY_FIELDS: u32 = 32;
pub const MAX_INVENTORY_FIELD_LENGTH: usize = 128;

pub fn whole_frame_sample_count(sample_count: usize, channels: u32) -> usize {
    if channels == 0 {
        return 0;
    }
    let channel_count = channels as usize;
    sample_count - (sample_count % channel_count)
}

pub fn sample_count_for_duration_us(sample_rate: u32, channels: u32, duration_us: u32) -> usize {
    if sample_rate == 0 || channels == 0 || duration_us == 0 {
        return 0;
    }
    let frames = ((u128::from(sample_rate) * u128::from(duration_us)) / 1_000_000).max(1) as usize;
    frames.saturating_mul(channels as usize)
}

pub fn duration_us_for_sample_count(sample_count: usize, sample_rate: u32, channels: u32) -> i64 {
    if sample_rate == 0 || channels == 0 {
        return 0;
    }
    let frames = sample_count / channels as usize;
    ((frames as u128 * 1_000_000) / u128::from(sample_rate)).min(i64::MAX as u128) as i64
}

pub fn bounded_direct_read_sample_count_for_format(
    available: usize,
    sample_rate: u32,
    channels: u32,
) -> usize {
    let max = sample_count_for_duration_us(sample_rate, channels, DIRECT_CAPTURE_READ_CHUNK_US);
    whole_frame_sample_count(available.min(max), channels)
}

pub fn direct_capture_latency_fraction() -> String {
    format!("{DIRECT_CAPTURE_LATENCY_FRAMES}/{DIRECT_CAPTURE_SAMPLE_RATE}")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn whole_frame_sample_count_trims_incomplete_channel_frames() {
        assert_eq!(0, whole_frame_sample_count(1, 2));
        assert_eq!(2, whole_frame_sample_count(2, 2));
        assert_eq!(4, whole_frame_sample_count(5, 2));
        assert_eq!(6, whole_frame_sample_count(7, 3));
        assert_eq!(0, whole_frame_sample_count(7, 0));
    }

    #[test]
    fn direct_read_count_uses_stable_twenty_ms_chunks() {
        assert_eq!(1_920, DIRECT_CAPTURE_MAX_READ_SAMPLES);
        assert_eq!(
            1_920,
            bounded_direct_read_sample_count_for_format(9_600, 48_000, 2)
        );
        assert_eq!(
            1_764,
            bounded_direct_read_sample_count_for_format(9_600, 44_100, 2)
        );
    }
}
