// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it} from 'vitest';
import {createCalculator, fitMediaWithinBounds, mediaAspectRatioValue} from './DimensionUtils';

const ATTACHMENT_CAPS = {maxWidth: 550, maxHeight: 350};

describe('fitMediaWithinBounds', () => {
	it('fits the maintainer 797x118 case to 550x81', () => {
		expect(fitMediaWithinBounds({width: 797, height: 118, ...ATTACHMENT_CAPS})).toEqual({width: 550, height: 81});
	});

	it('binds on height in the second pass for a tall image', () => {
		expect(fitMediaWithinBounds({width: 400, height: 1200, ...ATTACHMENT_CAPS})).toEqual({width: 117, height: 350});
		expect(fitMediaWithinBounds({width: 1000, height: 3000, ...ATTACHMENT_CAPS})).toEqual({width: 117, height: 350});
	});

	it('never upscales media that is already under both caps', () => {
		expect(fitMediaWithinBounds({width: 200, height: 100, ...ATTACHMENT_CAPS})).toEqual({width: 200, height: 100});
		expect(fitMediaWithinBounds({width: 43, height: 48, maxWidth: 400, maxHeight: 300})).toEqual({
			width: 43,
			height: 48,
		});
	});

	it('rounds twice, once per pass, rather than applying a single combined scale', () => {
		const combined = (width: number, height: number, maxWidth: number, maxHeight: number) => {
			const scale = Math.min(1, maxWidth / width, maxHeight / height);
			return {width: Math.round(width * scale), height: Math.round(height * scale)};
		};
		for (const [width, height, twoPassWidth, combinedWidth] of [
			[551, 351, 550, 549],
			[800, 600, 466, 467],
			[1200, 900, 466, 467],
		] as const) {
			expect(fitMediaWithinBounds({width, height, ...ATTACHMENT_CAPS})).toEqual({
				width: twoPassWidth,
				height: 350,
			});
			expect(combined(width, height, 550, 350)).toEqual({width: combinedWidth, height: 350});
			expect(twoPassWidth).not.toBe(combinedWidth);
		}
		expect(fitMediaWithinBounds({width: 797, height: 600, ...ATTACHMENT_CAPS})).toEqual({width: 465, height: 350});
		expect(fitMediaWithinBounds({width: 550, height: 350, ...ATTACHMENT_CAPS})).toEqual({width: 550, height: 350});
	});
});

describe('MediaDimensionCalculator', () => {
	it('caps the wrapper at the fitted width so small media is not stretched', () => {
		const calculator = createCalculator({maxWidth: 400, maxHeight: 300});

		const {dimensions, style} = calculator.calculate({width: 43, height: 48});

		expect(dimensions).toEqual({width: 43, height: 48});
		expect(style).toMatchObject({
			maxWidth: 'min(100%, 2.6875rem)',
			width: '100%',
			display: 'block',
			aspectRatio: 43 / 48,
		});
	});

	it('carries the aspect ratio as a unitless number for the maintainer case', () => {
		const calculator = createCalculator(ATTACHMENT_CAPS);

		const {dimensions, style} = calculator.calculate({width: 797, height: 118});

		expect(dimensions).toEqual({width: 550, height: 81});
		expect(style.aspectRatio).toBe(550 / 81);
		expect(style.aspectRatio).toBeCloseTo(6.79012, 5);
		expect(typeof style.aspectRatio).toBe('number');
	});

	it('scales tall media down to the height cap without an off-by-one', () => {
		const calculator = createCalculator(ATTACHMENT_CAPS);

		const {dimensions, style} = calculator.calculate({width: 400, height: 1200});

		expect(dimensions).toEqual({width: 117, height: 350});
		expect(style.maxHeight).toBe('21.875rem');
		expect(mediaAspectRatioValue(dimensions)).toBeCloseTo(0.334286, 6);
	});

	it('preserves an already fitted pair and still emits the numeric aspect ratio', () => {
		const calculator = createCalculator(ATTACHMENT_CAPS);

		const {dimensions, style} = calculator.calculate({width: 550, height: 81}, {preserve: true});

		expect(dimensions).toEqual({width: 550, height: 81});
		expect(style).toMatchObject({width: '34.375rem', maxWidth: '100%', aspectRatio: 550 / 81});
	});
});
