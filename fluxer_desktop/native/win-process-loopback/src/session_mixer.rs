// SPDX-License-Identifier: AGPL-3.0-or-later

use std::collections::VecDeque;

#[derive(Debug, Clone, PartialEq)]
pub struct MixerPacket {
    pub timestamp_us: i64,
    pub samples: Vec<f32>,
}

pub fn timestamp_delta_to_frames(
    base_timestamp_us: i64,
    timestamp_us: i64,
    sample_rate: u32,
) -> usize {
    if sample_rate == 0 {
        return 0;
    }
    let delta_us = timestamp_us.saturating_sub(base_timestamp_us).max(0) as u128;
    ((delta_us * u128::from(sample_rate)) / 1_000_000) as usize
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SessionCandidate {
    pub root_pid: u32,
    pub active: bool,
}

pub fn select_capture_pids(candidates: &[SessionCandidate], recently_active: &[u32]) -> Vec<u32> {
    let mut selected: Vec<u32> = candidates
        .iter()
        .filter(|candidate| candidate.root_pid != 0)
        .filter(|candidate| candidate.active || recently_active.contains(&candidate.root_pid))
        .map(|candidate| candidate.root_pid)
        .collect();
    selected.sort_unstable();
    selected.dedup();
    selected
}

pub const SESSION_INACTIVE_GRACE_US: i64 = 5_000_000;

#[derive(Debug, Default)]
pub struct SessionActivity {
    last_active_us: Vec<(u32, i64)>,
}

impl SessionActivity {
    pub fn clear(&mut self) {
        self.last_active_us.clear();
    }

    pub fn select(&mut self, candidates: &[SessionCandidate], now_us: i64) -> Vec<u32> {
        for candidate in candidates
            .iter()
            .filter(|candidate| candidate.active && candidate.root_pid != 0)
        {
            match self
                .last_active_us
                .iter_mut()
                .find(|(pid, _)| *pid == candidate.root_pid)
            {
                Some(entry) => entry.1 = now_us,
                None => self.last_active_us.push((candidate.root_pid, now_us)),
            }
        }
        self.last_active_us
            .retain(|(_, at_us)| now_us.saturating_sub(*at_us) < SESSION_INACTIVE_GRACE_US);
        let recently_active: Vec<u32> = self.last_active_us.iter().map(|(pid, _)| *pid).collect();
        select_capture_pids(candidates, &recently_active)
    }
}

pub const MIX_TIMELINE_MAX_AHEAD_US: i64 = 500_000;
pub const MIX_TIMELINE_IDLE_RESET_US: i64 = 1_000_000;

#[derive(Debug, Default)]
pub struct MixTimeline {
    origin_us: Option<i64>,
    next_frame: i64,
    last_input_us: i64,
    acc: VecDeque<f32>,
}

fn frame_position(origin_us: i64, timestamp_us: i64, sample_rate: u32) -> i64 {
    if timestamp_us >= origin_us {
        timestamp_delta_to_frames(origin_us, timestamp_us, sample_rate) as i64
    } else {
        -(timestamp_delta_to_frames(timestamp_us, origin_us, sample_rate) as i64)
    }
}

fn frame_timestamp_us(origin_us: i64, frame: i64, sample_rate: u32) -> i64 {
    origin_us
        .saturating_add(((i128::from(frame) * 1_000_000) / i128::from(sample_rate.max(1))) as i64)
}

fn max_ahead_frames(sample_rate: u32) -> i64 {
    timestamp_delta_to_frames(0, MIX_TIMELINE_MAX_AHEAD_US, sample_rate) as i64
}

impl MixTimeline {
    fn reset(&mut self) {
        self.origin_us = None;
        self.next_frame = 0;
        self.acc.clear();
    }

    pub fn push(&mut self, packet: MixerPacket, sample_rate: u32, channels: usize) {
        if sample_rate == 0 || channels == 0 {
            return;
        }
        let packet_frames = (packet.samples.len() / channels) as i64;
        if packet_frames == 0 {
            return;
        }
        let origin_us = *self.origin_us.get_or_insert(packet.timestamp_us);
        self.last_input_us = self.last_input_us.max(packet.timestamp_us);
        let start_frame = frame_position(origin_us, packet.timestamp_us, sample_rate);
        let skip_frames = (self.next_frame - start_frame).max(0);
        if skip_frames >= packet_frames {
            return;
        }
        let offset_frames = (start_frame + skip_frames) - self.next_frame;
        let limit_frames = max_ahead_frames(sample_rate);
        if offset_frames >= limit_frames {
            return;
        }
        let copy_frames = (packet_frames - skip_frames).min(limit_frames - offset_frames);
        let offset = offset_frames as usize * channels;
        let needed = offset + copy_frames as usize * channels;
        if self.acc.len() < needed {
            self.acc.resize(needed, 0.0);
        }
        let source_start = skip_frames as usize * channels;
        let source = &packet.samples[source_start..source_start + copy_frames as usize * channels];
        for (index, sample) in source.iter().copied().enumerate() {
            self.acc[offset + index] += sample;
        }
    }

    pub fn drain_ready(
        &mut self,
        now_us: i64,
        hold_us: i64,
        sample_rate: u32,
        channels: usize,
    ) -> Option<MixerPacket> {
        if sample_rate == 0 || channels == 0 {
            return None;
        }
        let origin_us = self.origin_us?;
        if now_us.saturating_sub(self.last_input_us) > MIX_TIMELINE_IDLE_RESET_US {
            self.reset();
            return None;
        }
        let end_frame = frame_position(origin_us, now_us.saturating_sub(hold_us), sample_rate);
        if end_frame <= self.next_frame {
            return None;
        }
        let limit_frames = max_ahead_frames(sample_rate);
        if end_frame - self.next_frame > limit_frames {
            let dropped_frames = end_frame - limit_frames - self.next_frame;
            let dropped_samples = (dropped_frames as usize * channels).min(self.acc.len());
            self.acc.drain(..dropped_samples);
            self.next_frame += dropped_frames;
        }
        let frames = (end_frame - self.next_frame) as usize;
        let sample_count = frames * channels;
        let available = sample_count.min(self.acc.len());
        let mut samples: Vec<f32> = self
            .acc
            .drain(..available)
            .map(|sample| sample.clamp(-1.0, 1.0))
            .collect();
        samples.resize(sample_count, 0.0);
        let timestamp_us = frame_timestamp_us(origin_us, self.next_frame, sample_rate);
        self.next_frame = end_frame;
        Some(MixerPacket {
            timestamp_us,
            samples,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const RATE: u32 = 48_000;
    const CHANNELS: usize = 2;

    fn packet(timestamp_us: i64, frames: usize, value: f32) -> MixerPacket {
        MixerPacket {
            timestamp_us,
            samples: vec![value; frames * CHANNELS],
        }
    }

    #[test]
    fn timestamp_delta_to_frames_uses_audio_rate() {
        assert_eq!(0, timestamp_delta_to_frames(1_000, 1_000, 48_000));
        assert_eq!(48, timestamp_delta_to_frames(1_000, 2_000, 48_000));
        assert_eq!(0, timestamp_delta_to_frames(2_000, 1_000, 48_000));
        assert_eq!(0, timestamp_delta_to_frames(1_000, 2_000, 0));
    }

    #[test]
    fn select_capture_pids_ignores_inactive_only_sessions() {
        let candidates = [SessionCandidate {
            root_pid: 10,
            active: false,
        }];
        assert!(select_capture_pids(&candidates, &[]).is_empty());
    }

    #[test]
    fn select_capture_pids_admits_active_sessions_sorted() {
        let candidates = [
            SessionCandidate {
                root_pid: 30,
                active: true,
            },
            SessionCandidate {
                root_pid: 10,
                active: true,
            },
            SessionCandidate {
                root_pid: 10,
                active: false,
            },
        ];
        assert_eq!(select_capture_pids(&candidates, &[]), vec![10, 30]);
    }

    #[test]
    fn select_capture_pids_keeps_recently_active_root_that_went_inactive() {
        let candidates = [SessionCandidate {
            root_pid: 10,
            active: false,
        }];
        assert_eq!(select_capture_pids(&candidates, &[10]), vec![10]);
    }

    #[test]
    fn select_capture_pids_drops_recently_active_root_without_sessions() {
        let candidates = [SessionCandidate {
            root_pid: 20,
            active: true,
        }];
        assert_eq!(select_capture_pids(&candidates, &[10]), vec![20]);
    }

    #[test]
    fn session_activity_keeps_paused_root_only_within_grace() {
        let mut activity = SessionActivity::default();
        let playing = [SessionCandidate {
            root_pid: 10,
            active: true,
        }];
        let paused = [SessionCandidate {
            root_pid: 10,
            active: false,
        }];
        assert_eq!(activity.select(&playing, 0), vec![10]);
        assert_eq!(
            activity.select(&paused, SESSION_INACTIVE_GRACE_US - 1),
            vec![10]
        );
        assert!(
            activity
                .select(&paused, SESSION_INACTIVE_GRACE_US)
                .is_empty()
        );
        assert!(
            activity
                .select(&paused, SESSION_INACTIVE_GRACE_US + 1)
                .is_empty()
        );
    }

    #[test]
    fn session_activity_clear_drops_paused_roots() {
        let mut activity = SessionActivity::default();
        let playing = [SessionCandidate {
            root_pid: 10,
            active: true,
        }];
        let paused = [SessionCandidate {
            root_pid: 10,
            active: false,
        }];
        assert_eq!(activity.select(&playing, 0), vec![10]);
        activity.clear();
        assert!(activity.select(&paused, 1).is_empty());
    }

    #[test]
    fn timeline_sums_interleaved_sources_into_continuous_output() {
        let mut timeline = MixTimeline::default();
        let start = 1_000_000;
        let mut emitted_frames = 0usize;
        let mut first_timestamp = None;
        for step in 0..100i64 {
            let ts = start + step * 10_000;
            timeline.push(packet(ts, 480, 0.25), RATE, CHANNELS);
            timeline.push(packet(ts + 5_000, 480, 0.25), RATE, CHANNELS);
            if let Some(out) = timeline.drain_ready(ts + 10_000 + 40_000, 40_000, RATE, CHANNELS) {
                assert_eq!(out.samples.len() % CHANNELS, 0);
                if step > 2 {
                    assert!(
                        out.samples
                            .iter()
                            .all(|sample| (*sample - 0.5).abs() < 1e-6)
                    );
                }
                first_timestamp.get_or_insert(out.timestamp_us);
                emitted_frames += out.samples.len() / CHANNELS;
            }
        }
        assert_eq!(first_timestamp, Some(start));
        assert_eq!(emitted_frames, 100 * 480);
    }

    #[test]
    fn timeline_trims_frames_that_arrive_after_their_slot_was_emitted() {
        let mut timeline = MixTimeline::default();
        timeline.push(packet(0, 480, 0.5), RATE, CHANNELS);
        let first = timeline
            .drain_ready(10_000, 0, RATE, CHANNELS)
            .expect("first block");
        assert_eq!(first.samples.len(), 480 * CHANNELS);
        timeline.push(packet(5_000, 480, 0.25), RATE, CHANNELS);
        let second = timeline
            .drain_ready(15_000, 0, RATE, CHANNELS)
            .expect("second block");
        assert_eq!(second.timestamp_us, 10_000);
        assert_eq!(second.samples.len(), 240 * CHANNELS);
        assert!(second.samples.iter().all(|sample| *sample == 0.25));
    }

    #[test]
    fn timeline_zero_fills_gaps_between_packets() {
        let mut timeline = MixTimeline::default();
        timeline.push(packet(0, 480, 0.5), RATE, CHANNELS);
        timeline.push(packet(20_000, 480, 0.5), RATE, CHANNELS);
        let out = timeline
            .drain_ready(30_000, 0, RATE, CHANNELS)
            .expect("block");
        assert_eq!(out.samples.len(), 1_440 * CHANNELS);
        assert!(out.samples[..480 * CHANNELS].iter().all(|s| *s == 0.5));
        assert!(
            out.samples[480 * CHANNELS..960 * CHANNELS]
                .iter()
                .all(|s| *s == 0.0)
        );
        assert!(out.samples[960 * CHANNELS..].iter().all(|s| *s == 0.5));
    }

    #[test]
    fn timeline_keeps_emitting_silence_while_idle_and_resets_after_a_second() {
        let mut timeline = MixTimeline::default();
        timeline.push(packet(0, 480, 0.5), RATE, CHANNELS);
        let first = timeline
            .drain_ready(500_000, 0, RATE, CHANNELS)
            .expect("block");
        assert_eq!(first.samples.len(), 24_000 * CHANNELS);
        assert!(timeline.drain_ready(1_000_001, 0, RATE, CHANNELS).is_none());
        assert!(timeline.drain_ready(1_100_000, 0, RATE, CHANNELS).is_none());
        timeline.push(packet(2_000_000, 480, 0.5), RATE, CHANNELS);
        let restarted = timeline
            .drain_ready(2_010_000, 0, RATE, CHANNELS)
            .expect("re-anchored block");
        assert_eq!(restarted.timestamp_us, 2_000_000);
        assert_eq!(restarted.samples.len(), 480 * CHANNELS);
    }

    #[test]
    fn timeline_clamps_summed_output() {
        let mut timeline = MixTimeline::default();
        timeline.push(packet(0, 480, 0.75), RATE, CHANNELS);
        timeline.push(packet(0, 480, 0.75), RATE, CHANNELS);
        timeline.push(packet(10_000, 480, -0.75), RATE, CHANNELS);
        timeline.push(packet(10_000, 480, -0.75), RATE, CHANNELS);
        let out = timeline
            .drain_ready(20_000, 0, RATE, CHANNELS)
            .expect("block");
        assert!(out.samples[..480 * CHANNELS].iter().all(|s| *s == 1.0));
        assert!(out.samples[480 * CHANNELS..].iter().all(|s| *s == -1.0));
    }

    #[test]
    fn timeline_truncates_partial_trailing_frames() {
        let mut timeline = MixTimeline::default();
        timeline.push(
            MixerPacket {
                timestamp_us: 0,
                samples: vec![0.5, 0.5, 0.5],
            },
            RATE,
            CHANNELS,
        );
        timeline.push(
            MixerPacket {
                timestamp_us: 1_000,
                samples: vec![0.25],
            },
            RATE,
            CHANNELS,
        );
        let out = timeline
            .drain_ready(1_000, 0, RATE, CHANNELS)
            .expect("block");
        assert_eq!(out.samples.len(), 48 * CHANNELS);
        assert_eq!(&out.samples[..2], &[0.5, 0.5]);
        assert!(out.samples[2..].iter().all(|s| *s == 0.0));
    }

    #[test]
    fn timeline_drops_data_beyond_the_lookahead_window() {
        let mut timeline = MixTimeline::default();
        timeline.push(packet(0, 480, 0.5), RATE, CHANNELS);
        timeline.push(packet(600_000, 480, 0.5), RATE, CHANNELS);
        let out = timeline
            .drain_ready(610_000, 0, RATE, CHANNELS)
            .expect("block");
        assert_eq!(out.samples.len(), 24_000 * CHANNELS);
        assert!(out.samples.iter().all(|s| *s == 0.0));
    }
}
