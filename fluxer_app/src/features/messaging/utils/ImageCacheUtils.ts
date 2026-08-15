// SPDX-License-Identifier: AGPL-3.0-or-later

import {LRUCache} from 'lru-cache';

interface ImageCacheEntry {
	image: HTMLImageElement;
	src: string;
}

interface PendingImageSubscriber {
	onLoad: () => void;
	onError: (() => void) | undefined;
}

interface PendingImageLoad {
	image: HTMLImageElement | null;
	subscribers: Set<PendingImageSubscriber>;
	active: boolean;
	timeoutId: number;
}

const MAX_CACHE_ENTRIES = 500;
const MAX_CACHE_BYTES = 64 * 1024 * 1024;
const MAX_CACHE_ENTRY_BYTES = 16 * 1024 * 1024;
const FALLBACK_IMAGE_BYTES = 256 * 1024;
const MAX_PENDING_IMAGE_LOADS = 64;
const MAX_ACTIVE_IMAGE_LOADS = 4;
const MAX_PENDING_IMAGE_CALLBACKS = 1024;
const MAX_PENDING_IMAGE_CALLBACKS_PER_LOAD = 256;
const MAX_IMAGE_SOURCE_LENGTH = 16 * 1024;
const IMAGE_LOAD_TIMEOUT_MS = 30_000;
const IMAGE_LOAD_ACTIVATION_TIMEOUT_MS = 10_000;

const getImageByteSize = (image: HTMLImageElement): number | null => {
	const width = image.naturalWidth;
	const height = image.naturalHeight;
	if (!Number.isSafeInteger(width) || !Number.isSafeInteger(height) || width <= 0 || height <= 0) {
		return null;
	}
	const byteSize = width * height * 4;
	if (!Number.isSafeInteger(byteSize) || byteSize <= 0) return null;
	return byteSize;
};

const estimateImageBytes = (entry: ImageCacheEntry): number => {
	const byteSize = getImageByteSize(entry.image);
	if (byteSize == null) return MAX_CACHE_ENTRY_BYTES + FALLBACK_IMAGE_BYTES;
	return byteSize;
};

const imageCache = new LRUCache<string, ImageCacheEntry>({
	max: MAX_CACHE_ENTRIES,
	maxSize: MAX_CACHE_BYTES,
	maxEntrySize: MAX_CACHE_ENTRY_BYTES,
	sizeCalculation: estimateImageBytes,
});
const pendingImageLoads = new Map<string, PendingImageLoad>();
let activeImageLoadCount = 0;
let pendingImageCallbackCount = 0;

const isLoadedImage = (image?: HTMLImageElement): image is HTMLImageElement => {
	if (!image) return false;
	return image.complete && image.naturalWidth > 0;
};
const isCacheableImage = (image: HTMLImageElement): boolean => {
	const byteSize = getImageByteSize(image);
	return byteSize != null && byteSize <= MAX_CACHE_ENTRY_BYTES;
};
const imageSourceEncoder = new TextEncoder();
const imageHasSource = (image: HTMLImageElement, src: string): boolean => {
	if (image.currentSrc.length > 0) {
		try {
			return image.currentSrc === new URL(src, image.ownerDocument.baseURI).href;
		} catch {
			return image.currentSrc === src;
		}
	}
	const attributeSource = image.getAttribute('src');
	if (attributeSource === src) return true;
	return image.src === src;
};
const acceptsImageSource = (src: string | null): src is string => {
	return (
		typeof src === 'string' &&
		src.length > 0 &&
		src.length <= MAX_IMAGE_SOURCE_LENGTH &&
		imageSourceEncoder.encode(src).byteLength <= MAX_IMAGE_SOURCE_LENGTH
	);
};
const isCached = (src: string | null): boolean => {
	if (!acceptsImageSource(src)) return false;
	const entry = imageCache.get(src);
	if (!entry) return false;
	if (entry.src === src && imageHasSource(entry.image, src) && isLoadedImage(entry.image)) return true;
	imageCache.delete(src);
	return false;
};

export function hasImage(src: string | null): boolean {
	return isCached(src);
}

export function getImage(src: string | null): HTMLImageElement | undefined {
	if (!acceptsImageSource(src)) return undefined;
	const entry = imageCache.get(src);
	if (!entry) return undefined;
	const image = entry.image;
	if (entry.src === src && imageHasSource(image, src) && isLoadedImage(image)) return image;
	imageCache.delete(src);
	return undefined;
}

export function rememberImage(src: string | null, image: HTMLImageElement): void {
	const currentPendingLoad = acceptsImageSource(src) ? pendingImageLoads.get(src) : undefined;
	if (!acceptsImageSource(src) || !imageHasSource(image, src) || !isLoadedImage(image)) {
		if (typeof src === 'string' && currentPendingLoad != null && currentPendingLoad.image === image) {
			settlePendingImageLoad(src, currentPendingLoad, false, null);
		}
		return;
	}
	if (isCacheableImage(image)) imageCache.set(src, {image, src});
	if (!currentPendingLoad) return;
	settlePendingImageLoad(src, currentPendingLoad, true, image);
}

export function forgetImage(src: string | null): void {
	if (!acceptsImageSource(src)) return;
	imageCache.delete(src);
	const currentPendingLoad = pendingImageLoads.get(src);
	if (!currentPendingLoad) return;
	settlePendingImageLoad(src, currentPendingLoad, false, null);
}

function retainPendingImageCallback(): void {
	pendingImageCallbackCount += 1;
}

function releasePendingImageCallbacks(count: number): void {
	pendingImageCallbackCount -= count;
	if (pendingImageCallbackCount < 0) throw new Error('Pending image callback count became negative');
}

function releaseActiveImageLoad(): void {
	activeImageLoadCount -= 1;
	if (activeImageLoadCount < 0) throw new Error('Active image load count became negative');
}

function detachPendingImageLoad(src: string, pendingLoad: PendingImageLoad): boolean {
	if (pendingImageLoads.get(src) !== pendingLoad) return false;
	pendingImageLoads.delete(src);
	window.clearTimeout(pendingLoad.timeoutId);
	if (pendingLoad.image != null) {
		pendingLoad.image.onload = null;
		pendingLoad.image.onerror = null;
	}
	if (pendingLoad.active) releaseActiveImageLoad();
	return true;
}

function notifyPendingImageSubscribers(pendingLoad: PendingImageLoad, loaded: boolean): void {
	const subscribers = [...pendingLoad.subscribers];
	pendingLoad.subscribers.clear();
	releasePendingImageCallbacks(subscribers.length);
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

function settlePendingImageLoad(
	src: string,
	pendingLoad: PendingImageLoad,
	loaded: boolean,
	loadedImage: HTMLImageElement | null,
): void {
	if (!detachPendingImageLoad(src, pendingLoad)) return;
	if (pendingLoad.image != null && pendingLoad.image !== loadedImage) pendingLoad.image.src = 'data:,';
	try {
		notifyPendingImageSubscribers(pendingLoad, loaded);
	} finally {
		pumpPendingImageLoads();
	}
}

function discardPendingImageLoad(src: string, pendingLoad: PendingImageLoad): void {
	if (!detachPendingImageLoad(src, pendingLoad)) return;
	if (pendingLoad.image != null) pendingLoad.image.src = 'data:,';
	const subscriberCount = pendingLoad.subscribers.size;
	pendingLoad.subscribers.clear();
	releasePendingImageCallbacks(subscriberCount);
	pumpPendingImageLoads();
}

function activatePendingImageLoad(src: string, pendingLoad: PendingImageLoad): void {
	if (pendingLoad.active || pendingImageLoads.get(src) !== pendingLoad) return;
	pendingLoad.active = true;
	activeImageLoadCount += 1;
	if (activeImageLoadCount > MAX_ACTIVE_IMAGE_LOADS) throw new Error('Active image load count exceeds its limit');
	window.clearTimeout(pendingLoad.timeoutId);
	pendingLoad.timeoutId = window.setTimeout(() => {
		settlePendingImageLoad(src, pendingLoad, false, null);
	}, IMAGE_LOAD_TIMEOUT_MS);
	const image = new Image();
	image.decoding = 'async';
	pendingLoad.image = image;
	image.onload = () => {
		if (pendingImageLoads.get(src) !== pendingLoad) return;
		rememberImage(src, image);
	};
	image.onerror = () => {
		if (pendingImageLoads.get(src) !== pendingLoad) return;
		imageCache.delete(src);
		settlePendingImageLoad(src, pendingLoad, false, null);
	};
	image.src = src;
}

function pumpPendingImageLoads(): void {
	if (activeImageLoadCount >= MAX_ACTIVE_IMAGE_LOADS) return;
	for (const [src, pendingLoad] of pendingImageLoads) {
		if (activeImageLoadCount >= MAX_ACTIVE_IMAGE_LOADS) return;
		if (!pendingLoad.active) activatePendingImageLoad(src, pendingLoad);
	}
}

function createPendingImageLoad(src: string): PendingImageLoad {
	const pendingLoad: PendingImageLoad = {
		image: null,
		subscribers: new Set(),
		active: false,
		timeoutId: 0,
	};
	pendingLoad.timeoutId = window.setTimeout(() => {
		settlePendingImageLoad(src, pendingLoad, false, null);
	}, IMAGE_LOAD_ACTIVATION_TIMEOUT_MS);
	pendingImageLoads.set(src, pendingLoad);
	return pendingLoad;
}

function rejectImageLoad(onError: (() => void) | undefined): () => void {
	if (onError) onError();
	return () => {};
}

export function loadImage(src: string | null, onLoad: () => void, onError?: () => void): () => void {
	if (!acceptsImageSource(src)) return rejectImageLoad(onError);
	if (isCached(src)) {
		onLoad();
		return () => {};
	}
	let pendingLoad = pendingImageLoads.get(src);
	if (!pendingLoad) {
		if (pendingImageLoads.size >= MAX_PENDING_IMAGE_LOADS) return rejectImageLoad(onError);
		pendingLoad = createPendingImageLoad(src);
	}
	if (
		pendingLoad.subscribers.size >= MAX_PENDING_IMAGE_CALLBACKS_PER_LOAD ||
		pendingImageCallbackCount >= MAX_PENDING_IMAGE_CALLBACKS
	) {
		if (pendingLoad.subscribers.size === 0) discardPendingImageLoad(src, pendingLoad);
		return rejectImageLoad(onError);
	}
	const subscriber: PendingImageSubscriber = {onLoad, onError};
	pendingLoad.subscribers.add(subscriber);
	retainPendingImageCallback();
	pumpPendingImageLoads();
	return () => {
		if (!pendingLoad.subscribers.delete(subscriber)) return;
		releasePendingImageCallbacks(1);
		if (pendingImageLoads.get(src) === pendingLoad && pendingLoad.subscribers.size === 0) {
			discardPendingImageLoad(src, pendingLoad);
		}
	};
}

export function pinImage(src: string | null): () => void {
	return loadImage(src, () => {});
}

export function _clearForTests(): void {
	for (const [src, pendingLoad] of pendingImageLoads) {
		discardPendingImageLoad(src, pendingLoad);
	}
	imageCache.clear();
	if (pendingImageLoads.size !== 0) throw new Error('Image cache retained pending loads after clear');
	if (activeImageLoadCount !== 0) throw new Error('Image cache retained active loads after clear');
	if (pendingImageCallbackCount !== 0) throw new Error('Image cache retained callbacks after clear');
}
