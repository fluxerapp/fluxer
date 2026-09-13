// SPDX-License-Identifier: AGPL-3.0-or-later

import {buildGifPickerLoadingSkeletonLayout} from '@app/features/channel/components/pickers/gif/GifPickerLoadingSkeletonGridLayout';
import {bench, describe} from 'vitest';

const VIEWPORTS = [
	{viewportWidth: 360, viewportHeight: 480},
	{viewportWidth: 436, viewportHeight: 640},
	{viewportWidth: 720, viewportHeight: 720},
	{viewportWidth: 960, viewportHeight: 840},
] as const;

describe('GifPickerLoadingSkeletonGridLayout benchmarks', () => {
	bench('builds deterministic masonry skeleton layouts for common picker sizes', () => {
		for (const viewport of VIEWPORTS) {
			buildGifPickerLoadingSkeletonLayout(viewport);
		}
	});
});
