// SPDX-License-Identifier: AGPL-3.0-or-later

import {resolveVoiceChannelBitrate} from '@fluxer/constants/src/GuildConstants';
import {describe, expect, it} from 'vitest';
import {
	buildMicrophonePublishOptions,
	normaliseAudioBitrateBps,
	OPUS_MAX_AUDIO_BITRATE_BPS,
	SCREEN_SHARE_AUDIO_PUBLISH_OPTIONS,
} from './AudioPublishOptions';

describe('normaliseAudioBitrateBps', () => {
	it('keeps stored channel bitrates in bits per second', () => {
		expect(normaliseAudioBitrateBps(64000)).toBe(64000);
		expect(normaliseAudioBitrateBps(320000)).toBe(320000);
		expect(normaliseAudioBitrateBps(384000)).toBe(384000);
	});
	it('accepts legacy kilobit-style values defensively', () => {
		expect(normaliseAudioBitrateBps(64)).toBe(64000);
		expect(normaliseAudioBitrateBps(384)).toBe(384000);
	});
	it('bounds audio bitrates to the supported Opus range used by WebRTC', () => {
		expect(normaliseAudioBitrateBps(1)).toBe(8000);
		expect(normaliseAudioBitrateBps(999999)).toBe(OPUS_MAX_AUDIO_BITRATE_BPS);
	});
	it('ignores missing or invalid values', () => {
		expect(normaliseAudioBitrateBps(null)).toBeUndefined();
		expect(normaliseAudioBitrateBps(undefined)).toBeUndefined();
		expect(normaliseAudioBitrateBps(0)).toBeUndefined();
		expect(normaliseAudioBitrateBps(Number.NaN)).toBeUndefined();
	});
});

describe('buildMicrophonePublishOptions', () => {
	it('uses the channel bitrate for normal voice tracks', () => {
		expect(buildMicrophonePublishOptions(96000, 'voice')).toEqual({
			audioPreset: {
				maxBitrate: 96000,
				priority: 'high',
			},
			red: true,
		});
	});
	it('keeps studio microphone tracks high fidelity when the channel can carry stereo', () => {
		expect(buildMicrophonePublishOptions(320000, 'studio')).toEqual({
			audioPreset: {
				maxBitrate: 320000,
				priority: 'high',
			},
			dtx: false,
			forceStereo: true,
			red: true,
		});
	});
	it('pins a call outside a guild to 64 kbps', () => {
		expect(buildMicrophonePublishOptions(resolveVoiceChannelBitrate(null, null), 'voice')).toEqual({
			audioPreset: {
				maxBitrate: 64000,
				priority: 'high',
			},
			red: true,
		});
	});
	it('does not force stereo below the stereo voice threshold', () => {
		expect(buildMicrophonePublishOptions(96000, 'studio')).toEqual({
			audioPreset: {
				maxBitrate: 96000,
				priority: 'high',
			},
			dtx: false,
			forceStereo: false,
			red: true,
		});
	});
});

describe('SCREEN_SHARE_AUDIO_PUBLISH_OPTIONS', () => {
	it('uses maximum stereo Opus settings for screen-share audio', () => {
		expect(SCREEN_SHARE_AUDIO_PUBLISH_OPTIONS.audioPreset?.maxBitrate).toBe(OPUS_MAX_AUDIO_BITRATE_BPS);
		expect(SCREEN_SHARE_AUDIO_PUBLISH_OPTIONS.audioPreset?.priority).toBe('high');
		expect(SCREEN_SHARE_AUDIO_PUBLISH_OPTIONS.dtx).toBe(false);
		expect(SCREEN_SHARE_AUDIO_PUBLISH_OPTIONS.forceStereo).toBe(true);
		expect(SCREEN_SHARE_AUDIO_PUBLISH_OPTIONS.red).toBe(true);
		expect(SCREEN_SHARE_AUDIO_PUBLISH_OPTIONS.source).toBe('screen_share_audio');
		expect(SCREEN_SHARE_AUDIO_PUBLISH_OPTIONS.stream).toBe('screen_share');
	});
});
