// SPDX-License-Identifier: AGPL-3.0-or-later

import {retainImage} from '@app/features/messaging/utils/ImageRetention';
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
	failedCycles: number;
	retryDelayMs: number;
	failedUntil: number;
	loadTimeoutId: number;
	retryTimeoutId: number;
	connectivityListener: (() => void) | null;
}

const MAX_CACHE_ENTRIES = 1000;
const MAX_IMAGE_SOURCE_LENGTH = 16 * 1024;
const MAX_PENDING_IMAGE_CALLBACKS_PER_LOAD = 256;
const IMAGE_LOAD_TIMEOUT_MS = 30_000;
const IMAGE_RETRY_ATTEMPT_LIMIT = 2;
const IMAGE_RETRY_INITIAL_DELAY_MS = 1000;
const IMAGE_RETRY_MAX_DELAY_MS = IMAGE_RETRY_INITIAL_DELAY_MS * 10;
const IMAGE_FAILURE_INITIAL_COOLDOWN_MS = 2000;
const IMAGE_FAILURE_COOLDOWN_MS = 60_000;

interface ImageRecovery {
	readonly src: string;
	readonly waiters: Set<() => void>;
	timeoutId: number;
	cancelLoad: (() => void) | null;
}

const recoveries = new Map<string, ImageRecovery>();
let recoveryWakeListenersInstalled = false;

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

const isCoolingDown = (entry: ImageCacheEntry): boolean => entry.failedUntil > Date.now();

const isLoadInFlight = (entry: ImageCacheEntry): boolean =>
	entry.image != null || entry.retryTimeoutId !== 0 || entry.connectivityListener != null;

const failureCooldownMs = (failedCycles: number): number =>
	Math.min(IMAGE_FAILURE_COOLDOWN_MS, IMAGE_FAILURE_INITIAL_COOLDOWN_MS * 2 ** Math.max(0, failedCycles - 1));

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

function runCallbacks(callbacks: ReadonlyArray<() => void>): void {
	const failures: Array<unknown> = [];
	for (const callback of callbacks) {
		try {
			callback();
		} catch (error) {
			failures.push(error);
		}
	}
	if (failures.length > 0) throw new AggregateError(failures, 'Image load callbacks failed');
}

function notifySubscribers(entry: ImageCacheEntry, loaded: boolean): void {
	const subscribers = [...entry.subscribers];
	entry.subscribers.clear();
	runCallbacks(subscribers.map((subscriber) => (loaded ? subscriber.onLoad : (subscriber.onError ?? (() => {})))));
}

function abandonEntry(entry: ImageCacheEntry): void {
	clearRetryState(entry);
	detachImageLoad(entry);
	notifySubscribers(entry, false);
}

function dropEntry(entry: ImageCacheEntry): void {
	if (ownsCacheKey(entry)) imageCache.delete(entry.src);
	abandonEntry(entry);
}

function failEntry(entry: ImageCacheEntry): void {
	entry.failedAttempts = 0;
	entry.failedCycles += 1;
	entry.retryDelayMs = IMAGE_RETRY_INITIAL_DELAY_MS;
	entry.failedUntil = Date.now() + failureCooldownMs(entry.failedCycles);
	abandonEntry(entry);
}

function settleLoaded(entry: ImageCacheEntry, image: HTMLImageElement): void {
	retainImage(image);
	clearRetryState(entry);
	detachImageLoad(entry);
	entry.loaded = true;
	entry.width = image.naturalWidth;
	entry.height = image.naturalHeight;
	entry.failedAttempts = 0;
	entry.failedCycles = 0;
	entry.retryDelayMs = IMAGE_RETRY_INITIAL_DELAY_MS;
	entry.failedUntil = 0;
	try {
		notifySubscribers(entry, true);
	} finally {
		resolveRecovery(entry.src);
	}
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
		failedCycles: 0,
		retryDelayMs: IMAGE_RETRY_INITIAL_DELAY_MS,
		failedUntil: 0,
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

export function hasFailedImage(src: string | null | undefined): boolean {
	if (!acceptsImageSource(src)) return false;
	const entry = imageCache.peek(src);
	return entry != null && isCoolingDown(entry);
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
	dropEntry(entry);
}

export function loadImage(src: string | null | undefined, onLoad: () => void, onError?: () => void): () => void {
	if (!acceptsImageSource(src)) return rejectImageLoad(onError);
	const cached = imageCache.get(src);
	if (cached?.loaded === true) {
		onLoad();
		return () => {};
	}
	if (cached != null && (isCoolingDown(cached) || cached.subscribers.size >= MAX_PENDING_IMAGE_CALLBACKS_PER_LOAD)) {
		return rejectImageLoad(onError);
	}
	const entry = cached ?? createEntry(src);
	const subscriber: ImageSubscriber = {onLoad, onError};
	entry.subscribers.add(subscriber);
	if (!isLoadInFlight(entry)) {
		entry.failedUntil = 0;
		startImageLoad(entry);
	}
	return () => {
		entry.subscribers.delete(subscriber);
	};
}

export function reportImageError(src: string | null | undefined): void {
	if (!acceptsImageSource(src)) return;
	const entry = imageCache.get(src) ?? createEntry(src);
	if (isLoadInFlight(entry) || isCoolingDown(entry)) return;
	entry.loaded = false;
	entry.width = 0;
	entry.height = 0;
	failEntry(entry);
}

export function awaitImage(src: string | null | undefined, onLoad: () => void): () => void {
	if (!acceptsImageSource(src)) return () => {};
	if (hasImage(src)) {
		onLoad();
		return () => {};
	}
	const recovery = recoveries.get(src) ?? createRecovery(src);
	const waiter = (): void => onLoad();
	recovery.waiters.add(waiter);
	if (recovery.timeoutId === 0 && recovery.cancelLoad == null) attemptRecovery(recovery);
	return () => {
		recovery.waiters.delete(waiter);
		if (recovery.waiters.size === 0) stopRecovery(recovery);
	};
}

function createRecovery(src: string): ImageRecovery {
	installRecoveryWakeListeners();
	const recovery: ImageRecovery = {src, waiters: new Set(), timeoutId: 0, cancelLoad: null};
	recoveries.set(src, recovery);
	return recovery;
}

function clearRecoveryWork(recovery: ImageRecovery): void {
	if (recovery.timeoutId !== 0) {
		window.clearTimeout(recovery.timeoutId);
		recovery.timeoutId = 0;
	}
	const cancelLoad = recovery.cancelLoad;
	recovery.cancelLoad = null;
	cancelLoad?.();
}

function stopRecovery(recovery: ImageRecovery): void {
	clearRecoveryWork(recovery);
	if (recoveries.get(recovery.src) === recovery) recoveries.delete(recovery.src);
}

function resolveRecovery(src: string): void {
	const recovery = recoveries.get(src);
	if (recovery == null) return;
	const waiters = [...recovery.waiters];
	recovery.waiters.clear();
	stopRecovery(recovery);
	runCallbacks(waiters);
}

function scheduleRecovery(recovery: ImageRecovery): void {
	if (recoveries.get(recovery.src) !== recovery) return;
	const entry = imageCache.peek(recovery.src);
	const delay = entry != null && isCoolingDown(entry) ? entry.failedUntil - Date.now() : IMAGE_RETRY_INITIAL_DELAY_MS;
	recovery.timeoutId = window.setTimeout(() => {
		recovery.timeoutId = 0;
		attemptRecovery(recovery);
	}, delay);
}

function attemptRecovery(recovery: ImageRecovery): void {
	clearRecoveryWork(recovery);
	if (hasImage(recovery.src)) {
		resolveRecovery(recovery.src);
		return;
	}
	const entry = imageCache.peek(recovery.src);
	if (entry != null && isCoolingDown(entry)) {
		scheduleRecovery(recovery);
		return;
	}
	let settled = false;
	const cancelLoad = loadImage(
		recovery.src,
		() => {
			settled = true;
		},
		() => {
			settled = true;
			recovery.cancelLoad = null;
			scheduleRecovery(recovery);
		},
	);
	if (!settled) recovery.cancelLoad = cancelLoad;
}

function wakeRecoveries(): void {
	for (const recovery of [...recoveries.values()]) {
		if (recovery.cancelLoad != null) continue;
		const entry = imageCache.peek(recovery.src);
		if (entry != null && !isLoadInFlight(entry)) entry.failedUntil = 0;
		attemptRecovery(recovery);
	}
}

function installRecoveryWakeListeners(): void {
	if (recoveryWakeListenersInstalled || typeof window === 'undefined') return;
	recoveryWakeListenersInstalled = true;
	window.addEventListener('online', wakeRecoveries);
	window.addEventListener('focus', wakeRecoveries);
	document.addEventListener('visibilitychange', () => {
		if (document.visibilityState === 'visible') wakeRecoveries();
	});
}

export function pinImage(src: string | null | undefined): () => void {
	return loadImage(src, () => {});
}

export function warmImage(src: string | null | undefined): void {
	loadImage(src, () => {});
}

export function _clearForTests(): void {
	for (const recovery of [...recoveries.values()]) stopRecovery(recovery);
	imageCache.clear();
}
