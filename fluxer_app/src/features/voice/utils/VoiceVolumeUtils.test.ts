// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	boostedVoiceVolumePercentToTrackVolume,
	clampVoiceTrackTotalGain,
	composeVoiceVolumeGain,
	recalibrateStoredVoiceVolumePercent,
	recalibrateStoredVoiceVolumes,
	VOICE_TRACK_MAX_TOTAL_GAIN,
	VOICE_VOLUME_MAX_GAIN,
	VOICE_VOLUME_MAX_PERCENT,
	VOICE_VOLUME_MAX_SLIDER_VOLUME,
} from '@app/features/voice/utils/VoiceVolumeUtils';
import {describe, expect, it} from 'vitest';

describe('boostedVoiceVolumePercentToTrackVolume', () => {
	it('leaves every setting at or below unity untouched', () => {
		const cases: Array<[number, number]> = [
			[0, 0],
			[1, 10 ** -1.188],
			[10, 10 ** -1.08],
			[25, 10 ** -0.9],
			[50, 10 ** -0.6],
			[75, 10 ** -0.3],
			[99, 10 ** -0.012],
			[100, 1],
		];
		for (const [percent, expected] of cases) {
			expect(boostedVoiceVolumePercentToTrackVolume(percent)).toBeCloseTo(expected, 15);
		}
		expect(boostedVoiceVolumePercentToTrackVolume(0)).toBe(0);
		expect(boostedVoiceVolumePercentToTrackVolume(100)).toBe(1);
	});

	it('widens the boost leg to +12.04 dB at 200%', () => {
		expect(boostedVoiceVolumePercentToTrackVolume(200)).toBeCloseTo(4, 12);
		expect(boostedVoiceVolumePercentToTrackVolume(150)).toBe(2);
		expect(boostedVoiceVolumePercentToTrackVolume(125)).toBe(Math.SQRT2);
		expect(boostedVoiceVolumePercentToTrackVolume(100)).toBe(1);
	});
});

describe('voice volume ceilings', () => {
	it('keeps the slider ceiling and the gain ceiling as separate quantities', () => {
		expect(VOICE_VOLUME_MAX_SLIDER_VOLUME).toBe(2);
		expect(VOICE_VOLUME_MAX_SLIDER_VOLUME).toBe(VOICE_VOLUME_MAX_PERCENT / 100);
		expect(VOICE_VOLUME_MAX_GAIN).toBe(4);
		expect(boostedVoiceVolumePercentToTrackVolume(VOICE_VOLUME_MAX_PERCENT)).toBeCloseTo(VOICE_VOLUME_MAX_GAIN, 12);
		expect(VOICE_TRACK_MAX_TOTAL_GAIN).toBe(12);
	});
});

describe('composeVoiceVolumeGain', () => {
	it('composes faders in linear gain instead of collapsing them', () => {
		expect(composeVoiceVolumeGain(200, 100, 200)).toBeCloseTo(16, 12);
		expect(composeVoiceVolumeGain(200, 100, 200)).toBeGreaterThan(composeVoiceVolumeGain(200, 100, 100));
		expect(composeVoiceVolumeGain(50, 100, 100)).toBe(10 ** -0.6);
		expect(composeVoiceVolumeGain()).toBe(1);
	});

	it('clamps each part, not the product', () => {
		expect(composeVoiceVolumeGain(400)).toBe(composeVoiceVolumeGain(200));
		expect(composeVoiceVolumeGain(-50)).toBe(0);
	});
});

describe('clampVoiceTrackTotalGain', () => {
	it('fails to unity, not silence', () => {
		expect(clampVoiceTrackTotalGain(Number.NaN)).toBe(1);
		expect(clampVoiceTrackTotalGain(Number.POSITIVE_INFINITY)).toBe(1);
		expect(clampVoiceTrackTotalGain(48)).toBe(12);
		expect(clampVoiceTrackTotalGain(-1)).toBe(0);
	});
});

describe('recalibrateStoredVoiceVolumePercent', () => {
	it('pulls every boosted stored value back to unity', () => {
		expect(recalibrateStoredVoiceVolumePercent(200)).toBe(100);
		expect(recalibrateStoredVoiceVolumePercent(150)).toBe(100);
		expect(recalibrateStoredVoiceVolumePercent(101)).toBe(100);
	});

	it('leaves unity and quieter values untouched', () => {
		expect(recalibrateStoredVoiceVolumePercent(100)).toBe(100);
		expect(recalibrateStoredVoiceVolumePercent(40)).toBe(40);
		expect(recalibrateStoredVoiceVolumePercent(0)).toBe(0);
	});

	it('keeps the same object when nothing needs recalibrating', () => {
		const volumes = {alice: 100, bob: 40};
		expect(recalibrateStoredVoiceVolumes(volumes)).toBe(volumes);
	});

	it('caps only the boosted entries in a stored map', () => {
		expect(recalibrateStoredVoiceVolumes({alice: 200, bob: 60, carol: 150})).toEqual({
			alice: 100,
			bob: 60,
			carol: 100,
		});
	});
});
