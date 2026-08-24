// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it} from 'vitest';
import {
	getEffectiveVideoLayoutDimensions,
	hasDifferentAspectRatio,
	isInlinePlayableVideoSize,
	resolveVideoLayout,
} from './VideoDimensionUtils';

describe('getEffectiveVideoLayoutDimensions', () => {
	it('prefers decoded dimensions over declared upload dimensions', () => {
		expect(getEffectiveVideoLayoutDimensions({width: 1280, height: 700}, {width: 1920, height: 700})).toEqual({
			width: 1920,
			height: 700,
		});
	});

	it('prefers decoded dimensions for rotated portrait video', () => {
		expect(getEffectiveVideoLayoutDimensions({width: 1920, height: 1080}, {width: 1080, height: 1920})).toEqual({
			width: 1080,
			height: 1920,
		});
	});

	it('falls back to declared dimensions until metadata decodes', () => {
		expect(getEffectiveVideoLayoutDimensions({width: 640, height: 360}, null)).toEqual({width: 640, height: 360});
	});

	it('falls back to a 16:9 default when nothing is known', () => {
		expect(getEffectiveVideoLayoutDimensions(null, null)).toEqual({width: 16, height: 9});
	});

	it('ignores invalid decoded dimensions', () => {
		expect(getEffectiveVideoLayoutDimensions({width: 640, height: 360}, {width: 0, height: 0})).toEqual({
			width: 640,
			height: 360,
		});
	});
});

describe('poster aspect-ratio mismatch detection', () => {
	it('flags an anamorphic thumbnail as mismatched against the decoded video', () => {
		expect(hasDifferentAspectRatio({width: 1920, height: 700}, {width: 1280, height: 700}, 0.05)).toBe(true);
	});

	it('flags a rotated (inverted) thumbnail as mismatched', () => {
		expect(hasDifferentAspectRatio({width: 1080, height: 1920}, {width: 1920, height: 1080}, 0.05)).toBe(true);
	});

	it('treats a matching thumbnail as consistent within tolerance', () => {
		expect(hasDifferentAspectRatio({width: 1920, height: 1080}, {width: 1280, height: 720}, 0.05)).toBe(false);
	});
});

describe('resolveVideoLayout', () => {
	it('produces an aspect ratio matching the decoded display dimensions', () => {
		const {aspectRatio} = resolveVideoLayout({width: 1920, height: 700}, {maxWidth: 400, maxHeight: 400});
		const [w, h] = aspectRatio.split(' / ').map(Number);
		expect(w / h).toBeCloseTo(1920 / 700, 1);
	});
});

describe('isInlinePlayableVideoSize', () => {
	it('accepts a landscape source sitting exactly on both edges', () => {
		expect(isInlinePlayableVideoSize({width: 6016, height: 3384})).toBe(true);
	});

	it('accepts a portrait source sitting exactly on the swapped edges', () => {
		expect(isInlinePlayableVideoSize({width: 3384, height: 6016})).toBe(true);
	});

	it('rejects one pixel past the landscape long edge', () => {
		expect(isInlinePlayableVideoSize({width: 6017, height: 3384})).toBe(false);
	});

	it('rejects one pixel past the landscape short edge', () => {
		expect(isInlinePlayableVideoSize({width: 6016, height: 3385})).toBe(false);
	});

	it('rejects one pixel past the portrait long edge', () => {
		expect(isInlinePlayableVideoSize({width: 3384, height: 6017})).toBe(false);
	});

	it('rejects one pixel past the portrait short edge', () => {
		expect(isInlinePlayableVideoSize({width: 3385, height: 6016})).toBe(false);
	});

	it('accepts a source that only fits the swapped clause', () => {
		expect(isInlinePlayableVideoSize({width: 3000, height: 5000})).toBe(true);
	});

	it('rejects a square source larger than the short edge', () => {
		expect(isInlinePlayableVideoSize({width: 4000, height: 4000})).toBe(false);
	});

	it('accepts a square source on the short edge', () => {
		expect(isInlinePlayableVideoSize({width: 3384, height: 3384})).toBe(true);
	});

	it('accepts unknown dimensions so metadata gaps never silence playback', () => {
		expect(isInlinePlayableVideoSize(null)).toBe(true);
		expect(isInlinePlayableVideoSize({width: 0, height: 0})).toBe(true);
		expect(isInlinePlayableVideoSize({width: Number.NaN, height: Number.NaN})).toBe(true);
	});
});
