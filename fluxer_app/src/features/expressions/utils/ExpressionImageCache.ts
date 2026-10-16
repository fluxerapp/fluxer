// SPDX-License-Identifier: AGPL-3.0-or-later

import {hasImage, loadImage} from '@app/features/messaging/utils/ImageCacheUtils';
import {useEffect} from 'react';

const IDLE_PRELOAD_BATCH_SIZE = 12;
const IMAGE_PRELOAD_ERROR_RETRY_DELAY_MS = 5000;
const MAX_TRACKED_FAILURES = 512;

const failedAt = new Map<string, number>();

function scheduleImagePreload(callback: () => void): void {
	if (typeof window === 'undefined') {
		return;
	}
	if (typeof window.requestIdleCallback === 'function') {
		window.requestIdleCallback(callback, {timeout: 250});
		return;
	}
	window.setTimeout(callback, 0);
}

function recordFailure(url: string): void {
	if (failedAt.size >= MAX_TRACKED_FAILURES) {
		failedAt.clear();
	}
	failedAt.set(url, Date.now());
}

function isInFailureBackoff(url: string): boolean {
	const failedTime = failedAt.get(url);
	if (failedTime === undefined) {
		return false;
	}
	if (Date.now() - failedTime < IMAGE_PRELOAD_ERROR_RETRY_DELAY_MS) {
		return true;
	}
	failedAt.delete(url);
	return false;
}

function preloadExpressionImage(url: string): void {
	if (hasImage(url) || isInFailureBackoff(url)) {
		return;
	}
	loadImage(
		url,
		() => {
			failedAt.delete(url);
		},
		() => {
			recordFailure(url);
		},
	);
}

function preloadExpressionImages(urls: ReadonlyArray<string | null | undefined>): void {
	if (urls.length === 0) {
		return;
	}
	const uniqueUrls = Array.from(new Set(urls.filter((url): url is string => Boolean(url))));
	if (uniqueUrls.length === 0) {
		return;
	}
	let index = 0;
	const preloadNextBatch = () => {
		const end = Math.min(index + IDLE_PRELOAD_BATCH_SIZE, uniqueUrls.length);
		for (; index < end; index++) {
			preloadExpressionImage(uniqueUrls[index]);
		}
		if (index < uniqueUrls.length) {
			scheduleImagePreload(preloadNextBatch);
		}
	};
	scheduleImagePreload(preloadNextBatch);
}

export function useExpressionImagesPreload(urls: ReadonlyArray<string | null | undefined>): void {
	useEffect(() => {
		preloadExpressionImages(urls);
	}, [urls]);
}
