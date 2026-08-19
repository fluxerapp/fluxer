// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import * as ImageCacheUtils from '@app/features/messaging/utils/ImageCacheUtils';
import {afterEach, describe, expect, it, vi} from 'vitest';

afterEach(() => {
	ImageCacheUtils._clearForTests();
});

describe('loadImage rejection path', () => {
	it('reports an unusable source through onError so callers can fall back', () => {
		const onLoad = vi.fn();
		const onError = vi.fn();
		ImageCacheUtils.loadImage(null, onLoad, onError);
		expect(onLoad).not.toHaveBeenCalled();
		expect(onError).toHaveBeenCalledTimes(1);
	});

	it('reports an oversized source through onError rather than queueing it', () => {
		const onLoad = vi.fn();
		const onError = vi.fn();
		ImageCacheUtils.loadImage(`https://cdn.test/${'a'.repeat(32 * 1024)}.png`, onLoad, onError);
		expect(onLoad).not.toHaveBeenCalled();
		expect(onError).toHaveBeenCalledTimes(1);
	});

	it('returns a no-op disposer when it rejects, so cleanup stays safe', () => {
		const dispose = ImageCacheUtils.loadImage(null, vi.fn(), vi.fn());
		expect(() => dispose()).not.toThrow();
	});
});
