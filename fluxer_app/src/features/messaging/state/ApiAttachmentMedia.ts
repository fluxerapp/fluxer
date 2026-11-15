// SPDX-License-Identifier: AGPL-3.0-or-later

import {ResettableStates} from '@app/features/app/state/ResettableStates';
import {
	apiAttachmentRequestPath,
	isApiAttachmentUrl,
	withoutAttachmentToken,
} from '@app/features/messaging/utils/ApiAttachmentUrl';
import {unwrapDesktopLocalResourceURL} from '@app/features/messaging/utils/DesktopLocalResourceTarget';
import {http} from '@app/features/platform/transport/RestTransport';
import {HttpError} from '@app/features/platform/types/EndpointError';
import {Logger} from '@app/features/platform/utils/AppLogger';

const logger = new Logger('ApiAttachmentMedia');

const MAX_CONCURRENT_LOADS = 6;
const MAX_CACHED_BYTES = 192 * 1024 * 1024;
const MAX_CACHED_ENTRIES = 400;
const LOAD_RETRIES = 2;
const FAILURE_BACKOFF_MS = 5_000;
const MAX_FAILURE_BACKOFF_MS = 60_000;

type ApiAttachmentMediaState = {status: 'loading'} | {status: 'ready'; objectUrl: string} | {status: 'failed'};

const LOADING: ApiAttachmentMediaState = Object.freeze({status: 'loading'});
const FAILED: ApiAttachmentMediaState = Object.freeze({status: 'failed'});

interface MediaEntry {
	readonly url: string;
	state: ApiAttachmentMediaState;
	queued: boolean;
	controller: AbortController | null;
	refs: number;
	size: number;
	failures: number;
	retryAtMs: number;
	retryTimer: ReturnType<typeof setTimeout> | null;
}

class NotAnApiAttachmentError extends Error {
	constructor() {
		super('The link is not an attachment link on the API');
		this.name = 'NotAnApiAttachmentError';
	}
}

export async function fetchApiAttachmentBlob(url: string, signal?: AbortSignal): Promise<Blob> {
	const path = apiAttachmentRequestPath(unwrapDesktopLocalResourceURL(url));
	if (path === null) throw new NotAnApiAttachmentError();
	const response = await http.get<Blob>(path, {parse: 'binary', mode: 'auto-retry', retries: LOAD_RETRIES, signal});
	if (!(response.body instanceof Blob)) throw new Error('The attachment response had no body');
	return response.body;
}

function isPermanentFailure(error: unknown): boolean {
	if (error instanceof NotAnApiAttachmentError) return true;
	return error instanceof HttpError && (error.status === 400 || error.status === 403 || error.status === 404);
}

class ApiAttachmentMedia {
	private entries = new Map<string, MediaEntry>();
	private readonly waiting: Array<MediaEntry> = [];
	private readonly listeners = new Set<() => void>();
	private active = 0;
	private cachedBytes = 0;
	private generation = 0;
	private headerMode = false;

	subscribe = (listener: () => void): (() => void) => {
		this.listeners.add(listener);
		return () => {
			this.listeners.delete(listener);
		};
	};

	isHeaderMode = (): boolean => this.headerMode;

	currentGeneration = (): number => this.generation;

	setHeaderMode(headerMode: boolean): void {
		if (this.headerMode === headerMode) return;
		this.headerMode = headerMode;
		if (headerMode) this.reset();
		else this.notify();
	}

	read(url: string): ApiAttachmentMediaState {
		return this.entries.get(url)?.state ?? LOADING;
	}

	retain(url: string): () => void {
		const entry = this.entries.get(url) ?? this.create(url);
		entry.refs += 1;
		this.touch(entry);
		if (entry.state.status === 'failed' && Date.now() >= entry.retryAtMs) this.restart(entry);
		let released = false;
		return () => {
			if (released) return;
			released = true;
			this.release(entry);
		};
	}

	warm(url: string): void {
		if (!this.entries.has(url)) this.create(url);
	}

	retry(url: string): void {
		const entry = this.entries.get(url);
		if (entry === undefined || entry.state.status !== 'failed') return;
		entry.retryAtMs = 0;
		if (entry.refs > 0) this.restart(entry);
	}

	reset(): void {
		this.generation += 1;
		for (const entry of this.entries.values()) this.discard(entry);
		this.entries = new Map();
		this.waiting.length = 0;
		this.active = 0;
		this.cachedBytes = 0;
		this.notify();
	}

	private create(url: string): MediaEntry {
		const entry: MediaEntry = {
			url,
			state: LOADING,
			queued: false,
			controller: null,
			refs: 0,
			size: 0,
			failures: 0,
			retryAtMs: 0,
			retryTimer: null,
		};
		this.entries.set(url, entry);
		this.enqueue(entry);
		return entry;
	}

	private touch(entry: MediaEntry): void {
		this.entries.delete(entry.url);
		this.entries.set(entry.url, entry);
	}

	private restart(entry: MediaEntry): void {
		this.cancelRetry(entry);
		entry.state = LOADING;
		this.enqueue(entry);
		this.notify();
	}

	private enqueue(entry: MediaEntry): void {
		if (entry.queued || entry.controller !== null) return;
		entry.queued = true;
		this.waiting.push(entry);
		this.pump();
	}

	private release(entry: MediaEntry): void {
		entry.refs = Math.max(0, entry.refs - 1);
		if (entry.refs > 0 || this.entries.get(entry.url) !== entry) return;
		if (entry.state.status !== 'loading') {
			this.evict();
			return;
		}
		this.discard(entry);
		this.entries.delete(entry.url);
	}

	private discard(entry: MediaEntry): void {
		if (entry.queued) {
			entry.queued = false;
			const index = this.waiting.indexOf(entry);
			if (index >= 0) this.waiting.splice(index, 1);
		}
		entry.controller?.abort();
		entry.controller = null;
		this.cancelRetry(entry);
		if (entry.state.status === 'ready') {
			URL.revokeObjectURL(entry.state.objectUrl);
			this.cachedBytes -= entry.size;
		}
		entry.state = LOADING;
	}

	private evict(): void {
		for (const entry of this.entries.values()) {
			if (this.cachedBytes <= MAX_CACHED_BYTES && this.entries.size <= MAX_CACHED_ENTRIES) return;
			if (entry.refs > 0 || entry.state.status === 'loading') continue;
			this.discard(entry);
			this.entries.delete(entry.url);
		}
	}

	private pump(): void {
		while (this.active < MAX_CONCURRENT_LOADS && this.waiting.length > 0) {
			const entry = this.waiting.shift()!;
			entry.queued = false;
			void this.load(entry);
		}
	}

	private async load(entry: MediaEntry): Promise<void> {
		const generation = this.generation;
		const controller = new AbortController();
		entry.controller = controller;
		this.active += 1;
		let retryNow = false;
		try {
			const blob = await fetchApiAttachmentBlob(entry.url, controller.signal);
			if (generation !== this.generation || controller.signal.aborted) return;
			entry.state = {status: 'ready', objectUrl: URL.createObjectURL(blob)};
			entry.size = blob.size;
			entry.failures = 0;
			this.cachedBytes += entry.size;
			this.evict();
			this.notify();
		} catch (error) {
			if (generation !== this.generation || controller.signal.aborted) return;
			if (error instanceof HttpError && error.status === 401 && entry.failures === 0) {
				entry.failures = 1;
				retryNow = true;
				return;
			}
			logger.warn('Could not load an attachment', error);
			this.fail(entry, isPermanentFailure(error));
		} finally {
			if (generation === this.generation) {
				if (entry.controller === controller) entry.controller = null;
				this.active -= 1;
				if (retryNow && entry.refs > 0) this.enqueue(entry);
				else if (retryNow) this.entries.delete(entry.url);
				this.pump();
			}
		}
	}

	private cancelRetry(entry: MediaEntry): void {
		if (entry.retryTimer === null) return;
		clearTimeout(entry.retryTimer);
		entry.retryTimer = null;
	}

	private fail(entry: MediaEntry, permanent: boolean): void {
		entry.failures += 1;
		const delayMs = Math.min(FAILURE_BACKOFF_MS * 2 ** (entry.failures - 1), MAX_FAILURE_BACKOFF_MS);
		entry.retryAtMs = permanent ? Number.POSITIVE_INFINITY : Date.now() + delayMs;
		entry.state = FAILED;
		if (!permanent) {
			entry.retryTimer = setTimeout(() => {
				entry.retryTimer = null;
				if (entry.refs > 0 && entry.state === FAILED && this.entries.get(entry.url) === entry) this.restart(entry);
			}, delayMs);
		}
		this.notify();
	}

	private notify(): void {
		for (const listener of this.listeners) listener();
	}
}

const apiAttachmentMedia = new ApiAttachmentMedia();
ResettableStates.register(apiAttachmentMedia, () => apiAttachmentMedia.reset());

export function fetchedApiAttachmentKey(url: string): string | null {
	if (apiAttachmentMedia.isHeaderMode()) return null;
	const target = unwrapDesktopLocalResourceURL(url);
	return isApiAttachmentUrl(target) ? withoutAttachmentToken(target) : null;
}

export default apiAttachmentMedia;
