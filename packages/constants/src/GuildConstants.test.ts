// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it} from 'vitest';
import {
	clampVoiceChannelBitrate,
	GuildFeatures,
	getMaxVoiceChannelBitrate,
	resolveVoiceChannelBitrate,
} from './GuildConstants';

describe('getMaxVoiceChannelBitrate', () => {
	it('caps a guild holding no audio bitrate feature at 96 kbps', () => {
		expect(getMaxVoiceChannelBitrate([])).toBe(96000);
		expect(getMaxVoiceChannelBitrate([GuildFeatures.VIP_VOICE])).toBe(96000);
	});
	it('caps a call outside a guild at 96 kbps', () => {
		expect(getMaxVoiceChannelBitrate(null)).toBe(96000);
		expect(getMaxVoiceChannelBitrate(undefined)).toBe(96000);
	});
	it('raises the cap to the feature the guild holds', () => {
		expect(getMaxVoiceChannelBitrate([GuildFeatures.AUDIO_BITRATE_128_KBPS])).toBe(128000);
		expect(getMaxVoiceChannelBitrate([GuildFeatures.AUDIO_BITRATE_256_KBPS])).toBe(256000);
		expect(getMaxVoiceChannelBitrate([GuildFeatures.AUDIO_BITRATE_384_KBPS])).toBe(384000);
	});
	it('takes the highest feature when a guild holds several', () => {
		expect(
			getMaxVoiceChannelBitrate([GuildFeatures.AUDIO_BITRATE_384_KBPS, GuildFeatures.AUDIO_BITRATE_128_KBPS]),
		).toBe(384000);
		expect(
			getMaxVoiceChannelBitrate(new Set([GuildFeatures.AUDIO_BITRATE_128_KBPS, GuildFeatures.AUDIO_BITRATE_256_KBPS])),
		).toBe(256000);
	});
});

describe('clampVoiceChannelBitrate', () => {
	it('clamps above the cap the guild holds', () => {
		expect(clampVoiceChannelBitrate(384000, [])).toBe(96000);
		expect(clampVoiceChannelBitrate(384000, [GuildFeatures.AUDIO_BITRATE_128_KBPS])).toBe(128000);
		expect(clampVoiceChannelBitrate(384000, [GuildFeatures.AUDIO_BITRATE_384_KBPS])).toBe(384000);
	});
	it('leaves a value under the cap alone', () => {
		expect(clampVoiceChannelBitrate(64000, [])).toBe(64000);
		expect(clampVoiceChannelBitrate(8000, [])).toBe(8000);
	});
	it('raises a value under the minimum', () => {
		expect(clampVoiceChannelBitrate(1000, [])).toBe(8000);
	});
});

describe('resolveVoiceChannelBitrate', () => {
	it('falls back to 64 kbps when the channel stores no bitrate', () => {
		expect(resolveVoiceChannelBitrate(null, null)).toBe(64000);
		expect(resolveVoiceChannelBitrate(undefined, [])).toBe(64000);
		expect(resolveVoiceChannelBitrate(0, [])).toBe(64000);
		expect(resolveVoiceChannelBitrate(Number.NaN, [])).toBe(64000);
	});
	it('clamps a stored bitrate to what the guild currently holds', () => {
		expect(resolveVoiceChannelBitrate(384000, [])).toBe(96000);
		expect(resolveVoiceChannelBitrate(384000, [GuildFeatures.AUDIO_BITRATE_256_KBPS])).toBe(256000);
	});
});
