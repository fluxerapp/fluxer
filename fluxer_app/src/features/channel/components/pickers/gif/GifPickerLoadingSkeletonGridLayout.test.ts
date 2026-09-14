// SPDX-License-Identifier: AGPL-3.0-or-later

import {buildGifPickerLoadingSkeletonLayout} from '@app/features/channel/components/pickers/gif/GifPickerLoadingSkeletonGridLayout';
import {describe, expect, it} from 'vitest';

describe('buildGifPickerLoadingSkeletonLayout', () => {
	it('lays out GIF-like placeholders with the same masonry constraints as the real grid', () => {
		const layout = buildGifPickerLoadingSkeletonLayout({
			viewportWidth: 436,
			viewportHeight: 1200,
		});
		expect(layout.length).toBeGreaterThan(12);
		expect(layout[0]).toMatchObject({
			key: 'square-255',
			column: 0,
			left: 12,
			top: 12,
			renderedWidth: 202,
			renderedHeight: 202,
		});
		const wideItem = layout.find((item) => item.key === 'wide-640-358');
		const portraitItem = layout.find((item) => item.key === 'portrait-325-498');
		expect(wideItem).toBeDefined();
		expect(portraitItem).toBeDefined();
		expect(wideItem!.renderedHeight / wideItem!.renderedWidth).toBeCloseTo(358 / 640, 4);
		expect(portraitItem!.renderedHeight / portraitItem!.renderedWidth).toBeCloseTo(498 / 325, 4);
		for (const item of layout) {
			expect(item.left).toBeGreaterThanOrEqual(12);
			expect(item.left + item.renderedWidth).toBeLessThanOrEqual(436 - 12);
			expect(item.top).toBeGreaterThanOrEqual(12);
		}
	});

	it('returns no placeholders before the skeleton viewport has measurable dimensions', () => {
		expect(buildGifPickerLoadingSkeletonLayout({viewportWidth: 0, viewportHeight: 600})).toEqual([]);
		expect(buildGifPickerLoadingSkeletonLayout({viewportWidth: 436, viewportHeight: 0})).toEqual([]);
	});
});
