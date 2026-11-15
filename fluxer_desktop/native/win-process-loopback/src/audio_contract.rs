// SPDX-License-Identifier: AGPL-3.0-or-later

pub const TARGET_SAMPLE_RATE: u32 = 48_000;
pub const TARGET_CHANNELS: u16 = 2;

pub fn validate_sample_rate(value: u32) -> bool {
    value == TARGET_SAMPLE_RATE
}

pub fn validate_channels(value: u32) -> bool {
    value == TARGET_CHANNELS as u32
}

#[cfg(target_os = "windows")]
pub fn sample_count_for_frames(frames: u32) -> Option<usize> {
    (frames as usize).checked_mul(TARGET_CHANNELS as usize)
}

#[cfg(target_os = "windows")]
pub fn qpc_100ns_to_timestamp_us(qpc_100ns: u64) -> i64 {
    (qpc_100ns / 10).min(i64::MAX as u64) as i64
}

#[cfg(target_os = "windows")]
pub fn pcm16_to_float32(sample: i16) -> f32 {
    sample as f32 / 32768.0
}
