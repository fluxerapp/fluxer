// SPDX-License-Identifier: AGPL-3.0-or-later

import * as ImageCacheUtils from '@app/features/messaging/utils/ImageCacheUtils';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

const AVATAR_URL = 'https://fluxerusercontent.com/avatars/1471779547901222947/93e5e9b9.webp?size=160';

class FailingImage {
	static created: Array<FailingImage> = [];
	decoding = '';
	complete = false;
	naturalWidth = 0;
	naturalHeight = 0;
	onload: (() => void) | null = null;
	onerror: (() => void) | null = null;
	private source = '';

	constructor() {
		FailingImage.created.push(this);
	}

	get src(): string {
		return this.source;
	}

	set src(value: string) {
		this.source = value;
		this.onerror?.();
	}
}

function failImageLoad(): void {
	const onError = vi.fn();
	ImageCacheUtils.loadImage(AVATAR_URL, () => {}, onError);
	vi.runAllTimers();
	expect(onError).toHaveBeenCalledTimes(1);
}

describe('ImageCacheUtils failure cooldown', () => {
	beforeEach(() => {
		FailingImage.created = [];
		vi.useFakeTimers();
		vi.stubGlobal('window', globalThis);
		vi.stubGlobal('Image', FailingImage);
		ImageCacheUtils._clearForTests();
	});

	afterEach(() => {
		ImageCacheUtils._clearForTests();
		vi.unstubAllGlobals();
		vi.useRealTimers();
	});

	it('gives up on a failing image after two retries', () => {
		failImageLoad();
		expect(FailingImage.created).toHaveLength(3);
		expect(ImageCacheUtils.hasFailedImage(AVATAR_URL)).toBe(true);
	});

	it('rejects new loads of a failed image without requesting it again during the cooldown', () => {
		failImageLoad();
		const onError = vi.fn();
		ImageCacheUtils.loadImage(AVATAR_URL, () => {}, onError);
		vi.advanceTimersByTime(59_000);
		ImageCacheUtils.loadImage(AVATAR_URL, () => {}, onError);
		expect(onError).toHaveBeenCalledTimes(2);
		expect(FailingImage.created).toHaveLength(3);
	});

	it('requests the image again once the cooldown has passed', () => {
		failImageLoad();
		vi.advanceTimersByTime(60_000);
		expect(ImageCacheUtils.hasFailedImage(AVATAR_URL)).toBe(false);
		failImageLoad();
		expect(FailingImage.created).toHaveLength(6);
	});

	it('requests the image again straight away after it is forgotten', () => {
		failImageLoad();
		ImageCacheUtils.forgetImage(AVATAR_URL);
		expect(ImageCacheUtils.hasFailedImage(AVATAR_URL)).toBe(false);
		ImageCacheUtils.loadImage(AVATAR_URL, () => {});
		expect(FailingImage.created).toHaveLength(4);
	});
});
