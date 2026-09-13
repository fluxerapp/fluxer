// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	asVoiceTrackSource,
	isScreenShareAudioPublicationLike,
	VoiceTrackSource,
} from '@app/features/voice/engine/VoiceTrackSource';
import {describe, expect, it} from 'vitest';

describe('VoiceTrackSource', () => {
	it('normalizes native screen-share audio source aliases', () => {
		expect(asVoiceTrackSource('screenshareAudio')).toBe(VoiceTrackSource.ScreenShareAudio);
		expect(asVoiceTrackSource('screenshare_audio')).toBe(VoiceTrackSource.ScreenShareAudio);
		expect(asVoiceTrackSource('screen_audio')).toBe(VoiceTrackSource.ScreenShareAudio);
		expect(asVoiceTrackSource('system_audio')).toBe(VoiceTrackSource.ScreenShareAudio);
	});

	it('recognizes native screen-share audio publications by track name', () => {
		expect(isScreenShareAudioPublicationLike({source: VoiceTrackSource.Microphone, trackName: 'screen-audio'})).toBe(
			true,
		);
		expect(
			isScreenShareAudioPublicationLike({
				source: VoiceTrackSource.Microphone,
				audioTrack: {mediaStreamTrack: {label: 'screenshareAudio'}},
			}),
		).toBe(true);
		expect(isScreenShareAudioPublicationLike({source: VoiceTrackSource.Microphone, trackName: 'regular-mic'})).toBe(
			false,
		);
	});
});
