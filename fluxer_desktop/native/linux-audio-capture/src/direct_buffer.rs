// SPDX-License-Identifier: AGPL-3.0-or-later

#![allow(dead_code)]

use std::collections::VecDeque;

use crate::audio_contract::{
    self, DIRECT_CAPTURE_CHANNELS, DIRECT_CAPTURE_MAX_SAMPLES, DIRECT_CAPTURE_SAMPLE_RATE,
};
pub struct CapturedFrame {
    pub samples: Vec<f32>,
    pub sample_rate: u32,
    pub channels: u32,
    pub timestamp_us: i64,
}

pub struct DirectReadMeta {
    pub sample_rate: u32,
    pub channels: u32,
    pub timestamp_us: i64,
}

pub struct DirectAudioBuffer {
    samples: VecDeque<f32>,
    queue_start_us: i64,
    sample_rate: u32,
    channels: u32,
}

impl DirectAudioBuffer {
    pub fn new(sample_rate: u32, channels: u32) -> Self {
        Self {
            samples: VecDeque::with_capacity(DIRECT_CAPTURE_MAX_SAMPLES),
            queue_start_us: 0,
            sample_rate: sample_rate.max(1),
            channels: channels.max(1),
        }
    }

    pub fn default_format() -> Self {
        Self::new(DIRECT_CAPTURE_SAMPLE_RATE, DIRECT_CAPTURE_CHANNELS)
    }

    #[cfg(test)]
    pub fn len(&self) -> usize {
        self.samples.len()
    }

    #[cfg(test)]
    pub fn is_empty(&self) -> bool {
        self.samples.is_empty()
    }

    #[cfg(test)]
    pub fn queue_start_us(&self) -> i64 {
        self.queue_start_us
    }

    pub fn set_format(&mut self, sample_rate: u32, channels: u32) {
        let sample_rate = sample_rate.max(1);
        let channels = channels.max(1);
        if self.sample_rate != sample_rate || self.channels != channels {
            self.clear();
        }
        self.sample_rate = sample_rate;
        self.channels = channels;
    }

    pub fn clear(&mut self) {
        self.samples.clear();
        self.queue_start_us = 0;
    }

    pub fn push(&mut self, input: &[f32], end_timestamp_us: i64) {
        let whole = audio_contract::whole_frame_sample_count(input.len(), self.channels);
        if whole == 0 {
            return;
        }
        let mut frame = &input[..whole];
        if frame.len() > DIRECT_CAPTURE_MAX_SAMPLES {
            let keep =
                audio_contract::whole_frame_sample_count(DIRECT_CAPTURE_MAX_SAMPLES, self.channels);
            frame = &frame[frame.len() - keep..];
            self.clear();
        }
        if self.samples.is_empty() {
            let duration_us = audio_contract::duration_us_for_sample_count(
                frame.len(),
                self.sample_rate,
                self.channels,
            );
            self.queue_start_us = end_timestamp_us.saturating_sub(duration_us);
        }
        self.drop_for_incoming(frame.len());
        self.samples.extend(frame.iter().copied());
    }

    pub fn read(&mut self) -> Option<CapturedFrame> {
        let mut out = Vec::with_capacity(audio_contract::DIRECT_CAPTURE_MAX_READ_SAMPLES);
        let meta = self.read_into(&mut out)?;
        Some(CapturedFrame {
            samples: out,
            sample_rate: meta.sample_rate,
            channels: meta.channels,
            timestamp_us: meta.timestamp_us,
        })
    }

    pub fn read_into(&mut self, out: &mut Vec<f32>) -> Option<DirectReadMeta> {
        assert!(self.sample_rate >= 1);
        assert!(self.channels >= 1);
        out.clear();
        if self.samples.is_empty() {
            return None;
        }
        let take = audio_contract::bounded_direct_read_sample_count_for_format(
            self.samples.len(),
            self.sample_rate,
            self.channels,
        );
        if take == 0 {
            return None;
        }
        assert!(take <= self.samples.len());
        assert!(take.is_multiple_of(self.channels as usize));
        let timestamp_us = self.queue_start_us.max(0);
        let (front, back) = self.samples.as_slices();
        let front_take = take.min(front.len());
        out.extend_from_slice(&front[..front_take]);
        out.extend_from_slice(&back[..take - front_take]);
        self.samples.drain(..take);
        self.queue_start_us =
            self.queue_start_us
                .saturating_add(audio_contract::duration_us_for_sample_count(
                    take,
                    self.sample_rate,
                    self.channels,
                ));
        if self.samples.is_empty() {
            self.queue_start_us = 0;
        }
        Some(DirectReadMeta {
            sample_rate: self.sample_rate,
            channels: self.channels,
            timestamp_us,
        })
    }

    fn drop_for_incoming(&mut self, incoming: usize) {
        assert!(incoming >= 1);
        assert!(incoming <= DIRECT_CAPTURE_MAX_SAMPLES);
        assert!(self.channels >= 1);
        let total = self.samples.len() + incoming;
        if total <= DIRECT_CAPTURE_MAX_SAMPLES {
            return;
        }
        let overflow = total - DIRECT_CAPTURE_MAX_SAMPLES;
        let channels = self.channels as usize;
        let remainder = overflow % channels;
        let drop = if remainder == 0 {
            overflow
        } else {
            overflow + (channels - remainder)
        }
        .min(self.samples.len());
        self.samples.drain(..drop);
        self.queue_start_us =
            self.queue_start_us
                .saturating_add(audio_contract::duration_us_for_sample_count(
                    drop,
                    self.sample_rate,
                    self.channels,
                ));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn read_emits_stable_twenty_ms_chunks_with_continuous_timestamps() {
        let mut buffer = DirectAudioBuffer::default_format();
        buffer.push(&vec![0.5; 4_800], 1_000_000);

        let first = buffer.read().expect("first chunk");
        assert_eq!(1_920, first.samples.len());
        assert_eq!(950_000, first.timestamp_us);

        let second = buffer.read().expect("second chunk");
        assert_eq!(1_920, second.samples.len());
        assert_eq!(970_000, second.timestamp_us);
    }

    #[test]
    fn format_changes_clear_queued_samples() {
        let mut buffer = DirectAudioBuffer::default_format();
        buffer.push(&vec![0.5; 1_920], 100_000);
        assert!(!buffer.is_empty());

        buffer.set_format(44_100, 2);

        assert!(buffer.is_empty());
        assert_eq!(0, buffer.queue_start_us());
        buffer.push(&vec![0.25; 1_764], 200_000);
        let frame = buffer.read().expect("chunk");
        assert_eq!(1_764, frame.samples.len());
        assert_eq!(180_000, frame.timestamp_us);
        assert_eq!(44_100, frame.sample_rate);
    }

    #[test]
    fn overflow_drops_from_front_and_advances_timestamp() {
        let mut buffer = DirectAudioBuffer::default_format();
        buffer.push(&vec![0.5; DIRECT_CAPTURE_MAX_SAMPLES + 1_920], 3_000_000);

        assert_eq!(DIRECT_CAPTURE_MAX_SAMPLES, buffer.len());
        let frame = buffer.read().expect("chunk");
        assert_eq!(1_000_000, frame.timestamp_us);
    }
}
