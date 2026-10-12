// SPDX-License-Identifier: AGPL-3.0-or-later

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

fn normalize_packet_to_whole_frames(
    mut packet: MixerPacket,
    channel_count: usize,
) -> Option<MixerPacket> {
    if channel_count == 0 {
        return None;
    }
    let whole = (packet.samples.len() / channel_count) * channel_count;
    if whole == 0 {
        return None;
    }
    packet.samples.truncate(whole);
    for sample in &mut packet.samples {
        *sample = (*sample).clamp(-1.0, 1.0);
    }
    Some(packet)
}

pub fn mix_packets(
    mut packets: Vec<MixerPacket>,
    channels: u16,
    sample_rate: u32,
    max_emit_frames: usize,
) -> Vec<MixerPacket> {
    let channel_count = usize::from(channels);
    if channel_count == 0 || sample_rate == 0 {
        return Vec::new();
    }
    packets.retain(|packet| packet.samples.len() >= channel_count);
    if packets.is_empty() {
        return Vec::new();
    }
    packets.sort_by_key(|packet| packet.timestamp_us);
    let base_timestamp = packets[0].timestamp_us;
    let mut output_frames = 0usize;
    for packet in &packets {
        let offset_frames =
            timestamp_delta_to_frames(base_timestamp, packet.timestamp_us, sample_rate);
        let frames = packet.samples.len() / channel_count;
        output_frames = output_frames.max(offset_frames.saturating_add(frames));
    }
    if output_frames == 0 {
        return Vec::new();
    }
    if output_frames > max_emit_frames {
        return packets
            .into_iter()
            .filter_map(|packet| normalize_packet_to_whole_frames(packet, channel_count))
            .collect();
    }
    let mut mixed = vec![0.0f32; output_frames * channel_count];
    for packet in packets {
        let offset = timestamp_delta_to_frames(base_timestamp, packet.timestamp_us, sample_rate)
            * channel_count;
        let full_frame_samples = (packet.samples.len() / channel_count) * channel_count;
        for (idx, sample) in packet
            .samples
            .iter()
            .copied()
            .take(full_frame_samples)
            .enumerate()
        {
            if let Some(slot) = mixed.get_mut(offset + idx) {
                *slot += sample;
            }
        }
    }
    for sample in &mut mixed {
        *sample = (*sample).clamp(-1.0, 1.0);
    }
    vec![MixerPacket {
        timestamp_us: base_timestamp,
        samples: mixed,
    }]
}

#[cfg(test)]
mod tests {
    use super::*;

    fn packet(timestamp_us: i64, samples: &[f32]) -> MixerPacket {
        MixerPacket {
            timestamp_us,
            samples: samples.to_vec(),
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
    fn mix_packets_ignores_empty_input() {
        assert!(mix_packets(Vec::new(), 2, 48_000, 24_000).is_empty());
        assert!(mix_packets(vec![packet(0, &[])], 2, 48_000, 24_000).is_empty());
    }

    #[test]
    fn mix_packets_rejects_invalid_format_shape() {
        assert!(mix_packets(vec![packet(0, &[1.0, 1.0])], 0, 48_000, 24_000).is_empty());
        assert!(mix_packets(vec![packet(0, &[1.0, 1.0])], 2, 0, 24_000).is_empty());
    }

    #[test]
    fn mix_packets_sums_packets_with_matching_timestamps() {
        let mixed = mix_packets(
            vec![packet(10, &[0.25, 0.5]), packet(10, &[0.25, -0.25])],
            2,
            48_000,
            24_000,
        );
        assert_eq!(mixed, vec![packet(10, &[0.5, 0.25])]);
    }

    #[test]
    fn mix_packets_clamps_overlapping_samples() {
        let mixed = mix_packets(
            vec![packet(10, &[0.75, -0.75]), packet(10, &[0.75, -0.75])],
            2,
            48_000,
            24_000,
        );
        assert_eq!(mixed, vec![packet(10, &[1.0, -1.0])]);
    }

    #[test]
    fn mix_packets_offsets_by_timestamp() {
        let mixed = mix_packets(
            vec![packet(1_000, &[1.0, 0.0]), packet(2_000, &[0.0, 1.0])],
            2,
            48_000,
            24_000,
        );
        assert_eq!(mixed.len(), 1);
        assert_eq!(mixed[0].timestamp_us, 1_000);
        assert_eq!(&mixed[0].samples[0..2], &[1.0, 0.0]);
        assert_eq!(&mixed[0].samples[96..98], &[0.0, 1.0]);
    }

    #[test]
    fn mix_packets_sorts_packets_before_mixing() {
        let mixed = mix_packets(
            vec![packet(2_000, &[0.0, 1.0]), packet(1_000, &[1.0, 0.0])],
            2,
            48_000,
            24_000,
        );
        assert_eq!(mixed.len(), 1);
        assert_eq!(mixed[0].timestamp_us, 1_000);
        assert_eq!(&mixed[0].samples[0..2], &[1.0, 0.0]);
        assert_eq!(&mixed[0].samples[96..98], &[0.0, 1.0]);
    }

    #[test]
    fn mix_packets_falls_back_to_sorted_packets_when_span_is_too_large() {
        let packets = vec![packet(2_000, &[0.0, 1.0]), packet(1_000, &[1.0, 0.0])];
        let mixed = mix_packets(packets, 2, 48_000, 1);
        assert_eq!(
            mixed,
            vec![packet(1_000, &[1.0, 0.0]), packet(2_000, &[0.0, 1.0])]
        );
    }

    #[test]
    fn mix_packets_fallback_truncates_partial_frames_to_whole_stereo() {
        let packets = vec![
            packet(2_000, &[0.0, 1.0, 0.5]),
            packet(1_000, &[1.0, 0.0, 0.25, 0.75]),
        ];
        let mixed = mix_packets(packets, 2, 48_000, 1);
        assert_eq!(
            mixed,
            vec![
                packet(1_000, &[1.0, 0.0, 0.25, 0.75]),
                packet(2_000, &[0.0, 1.0]),
            ]
        );
        for emitted in &mixed {
            assert_eq!(emitted.samples.len() % 2, 0);
        }
    }

    #[test]
    fn mix_packets_fallback_clamps_out_of_range_samples() {
        let packets = vec![
            packet(2_000, &[0.0, 1.0]),
            packet(1_000, &[4.0, -4.0, 0.5, -0.5]),
        ];
        let mixed = mix_packets(packets, 2, 48_000, 1);
        assert_eq!(
            mixed,
            vec![
                packet(1_000, &[1.0, -1.0, 0.5, -0.5]),
                packet(2_000, &[0.0, 1.0]),
            ]
        );
    }

    #[test]
    fn mix_packets_fallback_drops_subframe_packets() {
        let packets = vec![packet(2_000, &[0.0, 1.0]), packet(1_000, &[0.5])];
        let mixed = mix_packets(packets, 2, 48_000, 1);
        assert_eq!(mixed, vec![packet(2_000, &[0.0, 1.0])]);
        for emitted in &mixed {
            assert_eq!(emitted.samples.len() % 2, 0);
        }
    }

    #[test]
    fn mix_packets_truncates_partial_trailing_frames() {
        let mixed = mix_packets(vec![packet(10, &[0.25, 0.5, 0.75])], 2, 48_000, 24_000);
        assert_eq!(mixed, vec![packet(10, &[0.25, 0.5])]);
    }
}
