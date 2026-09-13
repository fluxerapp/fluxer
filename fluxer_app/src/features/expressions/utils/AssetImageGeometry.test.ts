// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	clampWideAssetAspectRatio,
	getAspectRatioRange,
	isAspectRatioInRange,
	isOriginalImageWithinAssetBounds,
	WIDE_ASSET_ASPECT_RATIO_RANGE,
} from '@app/features/expressions/utils/AssetImageGeometry';
import {describe, expect, it} from 'vitest';

describe('AssetImageGeometry', () => {
	it('builds the flexible wide asset range from the crop height limits', () => {
		expect(WIDE_ASSET_ASPECT_RATIO_RANGE.min).toBeCloseTo(16 / 9);
		expect(WIDE_ASSET_ASPECT_RATIO_RANGE.max).toBeCloseTo(32 / 9);
	});

	it('rejects original images that would bypass crop bounds', () => {
		expect(
			isOriginalImageWithinAssetBounds({width: 540, height: 4320}, WIDE_ASSET_ASPECT_RATIO_RANGE, 2048, 1152),
		).toBe(false);
		expect(
			isOriginalImageWithinAssetBounds({width: 2048, height: 1152}, WIDE_ASSET_ASPECT_RATIO_RANGE, 2048, 1152),
		).toBe(true);
		expect(
			isOriginalImageWithinAssetBounds({width: 4096, height: 2304}, WIDE_ASSET_ASPECT_RATIO_RANGE, 2048, 1152),
		).toBe(false);
	});

	it('allows a small tolerance for strict square assets', () => {
		const range = getAspectRatioRange(1);
		expect(isAspectRatioInRange(1, range)).toBe(true);
		expect(isAspectRatioInRange(1.005, range)).toBe(true);
		expect(isAspectRatioInRange(1.05, range)).toBe(false);
	});

	it('clamps legacy wide asset ratios into the renderable range', () => {
		expect(clampWideAssetAspectRatio(540 / 4320)).toBeCloseTo(16 / 9);
		expect(clampWideAssetAspectRatio(10)).toBeCloseTo(32 / 9);
	});
});
