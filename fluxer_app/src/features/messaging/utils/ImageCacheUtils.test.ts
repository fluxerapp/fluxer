// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import * as ImageCacheUtils from '@app/features/messaging/utils/ImageCacheUtils';
import {afterEach, describe, expect, it, vi} from 'vitest';

const IMAGE_RETRY_CEILING_MS = 20_000;

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

describe('image load retry', () => {
	class StubImage {
		static instances: Array<StubImage> = [];
		onload: (() => void) | null = null;
		onerror: (() => void) | null = null;
		decoding = 'auto';
		naturalWidth = 0;
		naturalHeight = 0;
		#src = '';
		constructor() {
			StubImage.instances.push(this);
		}
		get src(): string {
			return this.#src;
		}
		set src(value: string) {
			this.#src = value;
		}
		fail(): void {
			this.onerror?.();
		}
	}

	const withStubImage = (run: () => void): void => {
		const original = globalThis.Image;
		StubImage.instances = [];
		globalThis.Image = StubImage as unknown as typeof Image;
		try {
			run();
		} finally {
			globalThis.Image = original;
		}
	};

	it('retries a failed load instead of reporting failure immediately', () => {
		vi.useFakeTimers();
		withStubImage(() => {
			const onError = vi.fn();
			ImageCacheUtils.loadImage('https://cdn.test/a.png', vi.fn(), onError);
			const first = StubImage.instances.at(-1);
			expect(first).toBeDefined();
			first?.fail();
			expect(onError).not.toHaveBeenCalled();
			vi.advanceTimersByTime(600);
			expect(StubImage.instances.length).toBeGreaterThan(1);
		});
		vi.useRealTimers();
	});

	it('gives up after a bounded number of attempts and reports the failure once', () => {
		vi.useFakeTimers();
		withStubImage(() => {
			const onError = vi.fn();
			ImageCacheUtils.loadImage('https://cdn.test/b.png', vi.fn(), onError);
			for (let attempt = 0; attempt < 12; attempt++) {
				StubImage.instances.at(-1)?.fail();
				vi.advanceTimersByTime(IMAGE_RETRY_CEILING_MS);
			}
			expect(StubImage.instances.length).toBeGreaterThan(1);
			expect(StubImage.instances.length).toBeLessThanOrEqual(6);
			expect(onError).toHaveBeenCalledTimes(1);
		});
		vi.useRealTimers();
	});
});
