// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	CALL_VOLUME_DEFAULT_PERCENT,
	callVolumePercentToSliderVolume,
	resolveCallVolumeMuteToggle,
	resolveLastNonZeroCallVolume,
	sliderVolumeToCallVolumePercent,
} from '@app/features/voice/components/CallVolumeState';
import {describe, expect, it} from 'vitest';

describe('callVolumePercentToSliderVolume', () => {
	it('maps percent to the slider scale', () => {
		expect(callVolumePercentToSliderVolume(0)).toBe(0);
		expect(callVolumePercentToSliderVolume(50)).toBe(0.5);
		expect(callVolumePercentToSliderVolume(100)).toBe(1);
	});

	it('falls back to the default for non-finite input', () => {
		expect(callVolumePercentToSliderVolume(Number.NaN)).toBe(1);
	});

	it('clamps negative percent to zero', () => {
		expect(callVolumePercentToSliderVolume(-10)).toBe(0);
	});

	it('reaches the boosted ceiling and clamps past it', () => {
		expect(callVolumePercentToSliderVolume(200)).toBe(2);
		expect(callVolumePercentToSliderVolume(250)).toBe(2);
	});
});

describe('sliderVolumeToCallVolumePercent', () => {
	it('maps slider values to whole percent', () => {
		expect(sliderVolumeToCallVolumePercent(0)).toBe(0);
		expect(sliderVolumeToCallVolumePercent(0.337)).toBe(34);
		expect(sliderVolumeToCallVolumePercent(1)).toBe(100);
	});

	it('clamps out-of-range slider values', () => {
		expect(sliderVolumeToCallVolumePercent(-0.5)).toBe(0);
		expect(sliderVolumeToCallVolumePercent(2.5)).toBe(200);
	});

	it('maps the boosted range above unity', () => {
		expect(sliderVolumeToCallVolumePercent(1.5)).toBe(150);
		expect(sliderVolumeToCallVolumePercent(2)).toBe(200);
	});

	it('falls back to the default for non-finite input', () => {
		expect(sliderVolumeToCallVolumePercent(Number.POSITIVE_INFINITY)).toBe(100);
		expect(sliderVolumeToCallVolumePercent(Number.NaN)).toBe(CALL_VOLUME_DEFAULT_PERCENT);
	});
});

describe('resolveLastNonZeroCallVolume', () => {
	it('tracks the latest non-zero volume', () => {
		expect(resolveLastNonZeroCallVolume(60, 30)).toBe(60);
	});

	it('keeps the previous value while muted', () => {
		expect(resolveLastNonZeroCallVolume(0, 30)).toBe(30);
	});

	it('falls back to the default when nothing was remembered', () => {
		expect(resolveLastNonZeroCallVolume(0, 0)).toBe(CALL_VOLUME_DEFAULT_PERCENT);
	});
});

describe('resolveCallVolumeMuteToggle', () => {
	it('mutes when the volume is above zero', () => {
		expect(resolveCallVolumeMuteToggle(75, 75)).toBe(0);
	});

	it('restores the remembered volume when muted', () => {
		expect(resolveCallVolumeMuteToggle(0, 40)).toBe(40);
	});

	it('restores the default when no volume was remembered', () => {
		expect(resolveCallVolumeMuteToggle(0, 0)).toBe(CALL_VOLUME_DEFAULT_PERCENT);
	});
});

describe('call volume round trip', () => {
	it('preserves a stored boosted volume through the slider', () => {
		expect(sliderVolumeToCallVolumePercent(callVolumePercentToSliderVolume(200))).toBe(200);
	});

	it('steps a boosted volume down by one keyboard step instead of collapsing to unity', () => {
		expect(sliderVolumeToCallVolumePercent(Math.max(0, callVolumePercentToSliderVolume(200) - 0.1))).toBe(190);
	});

	it('restores a boosted volume after a mute toggle', () => {
		expect(resolveCallVolumeMuteToggle(200, 200)).toBe(0);
		expect(resolveCallVolumeMuteToggle(0, resolveLastNonZeroCallVolume(0, 200))).toBe(200);
	});
});
