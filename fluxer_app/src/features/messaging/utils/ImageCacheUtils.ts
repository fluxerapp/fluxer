// SPDX-License-Identifier: AGPL-3.0-or-later

import {LRUCache} from 'lru-cache';

export interface CachedImageSize {
	width: number;
	height: number;
}

interface ImageSubscriber {
	onLoad: () => void;
	onError: (() => void) | undefined;
}

interface ImageCacheEntry {
	src: string;
	loaded: boolean;
	width: number;
	height: number;
	image: HTMLImageElement | null;
	subscribers: Set<ImageSubscriber>;
	failedAttempts: number;
	retryDelayMs: number;
	loadTimeoutId: number;
	retryTimeoutId: number;
	connectivityListener: (() => void) | null;
}

const MAX_CACHE_ENTRIES = 1000;
const MAX_IMAGE_SOURCE_LENGTH = 16 * 1024;
const MAX_PENDING_IMAGE_CALLBACKS_PER_LOAD = 256;
const IMAGE_LOAD_TIMEOUT_MS = 30_000;
const IMAGE_RETRY_ATTEMPT_LIMIT = 5;
const IMAGE_RETRY_INITIAL_DELAY_MS = 500;
const IMAGE_RETRY_MAX_DELAY_MS = IMAGE_RETRY_INITIAL_DELAY_MS * 10;

const imageCache = new LRUCache<string, ImageCacheEntry>({
	max: MAX_CACHE_ENTRIES,
	disposeAfter: (entry: ImageCacheEntry) => {
		abandonEntry(entry);
	},
});

const imageSourceEncoder = new TextEncoder();

const acceptsImageSource = (src: string | null | undefined): src is string =>
	typeof src === 'string' &&
	src.length > 0 &&
	src.length <= MAX_IMAGE_SOURCE_LENGTH &&
	imageSourceEncoder.encode(src).byteLength <= MAX_IMAGE_SOURCE_LENGTH;

const isLoadedImage = (image: HTMLImageElement | null | undefined): image is HTMLImageElement =>
	image?.complete === true && image.naturalWidth > 0;

const imageHasSource = (image: HTMLImageElement, src: string): boolean => {
	if (image.currentSrc.length > 0) {
		try {
			return image.currentSrc === new URL(src, image.ownerDocument.baseURI).href;
		} catch {
			return image.currentSrc === src;
		}
	}
	if (image.getAttribute('src') === src) return true;
	return image.src === src;
};

const ownsCacheKey = (entry: ImageCacheEntry): boolean => imageCache.peek(entry.src) === entry;

function clearRetryState(entry: ImageCacheEntry): void {
	if (entry.retryTimeoutId !== 0) {
		window.clearTimeout(entry.retryTimeoutId);
		entry.retryTimeoutId = 0;
	}
	if (entry.connectivityListener != null) {
		window.removeEventListener('online', entry.connectivityListener);
		entry.connectivityListener = null;
	}
}

function detachImageLoad(entry: ImageCacheEntry): void {
	if (entry.loadTimeoutId !== 0) {
		window.clearTimeout(entry.loadTimeoutId);
		entry.loadTimeoutId = 0;
	}
	if (entry.image != null) {
		entry.image.onload = null;
		entry.image.onerror = null;
		entry.image = null;
	}
}

function notifySubscribers(entry: ImageCacheEntry, loaded: boolean): void {
	const subscribers = [...entry.subscribers];
	entry.subscribers.clear();
	const failures: Array<unknown> = [];
	for (const subscriber of subscribers) {
		try {
			if (loaded) subscriber.onLoad();
			else if (subscriber.onError) subscriber.onError();
		} catch (error) {
			failures.push(error);
		}
	}
	if (failures.length > 0) throw new AggregateError(failures, 'Image load callbacks failed');
}

function abandonEntry(entry: ImageCacheEntry): void {
	clearRetryState(entry);
	detachImageLoad(entry);
	notifySubscribers(entry, false);
}

function failEntry(entry: ImageCacheEntry): void {
	if (ownsCacheKey(entry)) imageCache.delete(entry.src);
	abandonEntry(entry);
}

function settleLoaded(entry: ImageCacheEntry, image: HTMLImageElement): void {
	clearRetryState(entry);
	detachImageLoad(entry);
	entry.loaded = true;
	entry.width = image.naturalWidth;
	entry.height = image.naturalHeight;
	entry.failedAttempts = 0;
	entry.retryDelayMs = IMAGE_RETRY_INITIAL_DELAY_MS;
	notifySubscribers(entry, true);
}

function retryDelayForAttempt(entry: ImageCacheEntry): number {
	const growth = 2 * entry.retryDelayMs * Math.random();
	entry.retryDelayMs = Math.min(entry.retryDelayMs + growth, IMAGE_RETRY_MAX_DELAY_MS);
	return entry.retryDelayMs;
}

function startImageLoad(entry: ImageCacheEntry): void {
	const image = new Image();
	image.decoding = 'async';
	entry.image = image;
	entry.loadTimeoutId = window.setTimeout(() => {
		entry.loadTimeoutId = 0;
		failEntry(entry);
	}, IMAGE_LOAD_TIMEOUT_MS);
	image.onload = () => {
		detachImageLoad(entry);
		if (!isLoadedImage(image)) {
			scheduleRetryOrFail(entry);
			return;
		}
		settleLoaded(entry, image);
	};
	image.onerror = () => {
		detachImageLoad(entry);
		scheduleRetryOrFail(entry);
	};
	image.src = entry.src;
}

function scheduleRetryOrFail(entry: ImageCacheEntry): void {
	if (entry.failedAttempts >= IMAGE_RETRY_ATTEMPT_LIMIT) {
		failEntry(entry);
		return;
	}
	entry.failedAttempts += 1;
	const resume = (): void => {
		clearRetryState(entry);
		if (!ownsCacheKey(entry)) {
			abandonEntry(entry);
			return;
		}
		startImageLoad(entry);
	};
	if (typeof navigator !== 'undefined' && navigator.onLine === false) {
		entry.connectivityListener = resume;
		window.addEventListener('online', resume, {once: true});
		return;
	}
	entry.retryTimeoutId = window.setTimeout(resume, retryDelayForAttempt(entry));
}

function createEntry(src: string): ImageCacheEntry {
	const entry: ImageCacheEntry = {
		src,
		loaded: false,
		width: 0,
		height: 0,
		image: null,
		subscribers: new Set(),
		failedAttempts: 0,
		retryDelayMs: IMAGE_RETRY_INITIAL_DELAY_MS,
		loadTimeoutId: 0,
		retryTimeoutId: 0,
		connectivityListener: null,
	};
	imageCache.set(src, entry);
	return entry;
}

function rejectImageLoad(onError: (() => void) | undefined): () => void {
	if (onError) onError();
	return () => {};
}

export function hasImage(src: string | null | undefined): boolean {
	if (!acceptsImageSource(src)) return false;
	return imageCache.get(src)?.loaded === true;
}

export function getImageSize(src: string | null | undefined): CachedImageSize | undefined {
	if (!acceptsImageSource(src)) return undefined;
	const entry = imageCache.get(src);
	if (entry == null || !entry.loaded || entry.width <= 0 || entry.height <= 0) return undefined;
	return {width: entry.width, height: entry.height};
}

export function rememberImage(src: string | null | undefined, image: HTMLImageElement): void {
	if (!acceptsImageSource(src) || !imageHasSource(image, src) || !isLoadedImage(image)) return;
	const entry = imageCache.get(src) ?? createEntry(src);
	if (entry.loaded) return;
	settleLoaded(entry, image);
}

export function forgetImage(src: string | null | undefined): void {
	if (!acceptsImageSource(src)) return;
	const entry = imageCache.get(src);
	if (entry == null) return;
	failEntry(entry);
}

export function loadImage(src: string | null | undefined, onLoad: () => void, onError?: () => void): () => void {
	if (!acceptsImageSource(src)) return rejectImageLoad(onError);
	const cached = imageCache.get(src);
	if (cached?.loaded === true) {
		onLoad();
		return () => {};
	}
	if (cached != null && cached.subscribers.size >= MAX_PENDING_IMAGE_CALLBACKS_PER_LOAD) {
		return rejectImageLoad(onError);
	}
	const entry = cached ?? createEntry(src);
	const subscriber: ImageSubscriber = {onLoad, onError};
	entry.subscribers.add(subscriber);
	if (cached == null) startImageLoad(entry);
	return () => {
		entry.subscribers.delete(subscriber);
	};
}

export function pinImage(src: string | null | undefined): () => void {
	return loadImage(src, () => {});
}

export function warmImage(src: string | null | undefined): void {
	loadImage(src, () => {});
}

export function _clearForTests(): void {
	imageCache.clear();
}
