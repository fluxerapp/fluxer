// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import {ResettableStates} from '@app/features/app/state/ResettableStates';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {
	ATTACHMENT_URL_REFRESH_MARGIN_MS,
	type AttachmentUrlSignature,
	applyAttachmentSignature,
	attachmentCacheKey,
	attachmentUrlNeedsRefresh,
	isAttachmentCdnUrl,
	readAttachmentSignatureFields,
	readAttachmentUrlSignature,
	stripAttachmentSignature,
} from '@app/features/messaging/utils/AttachmentCdnUrl';
import {unwrapDesktopLocalResourceURL} from '@app/features/messaging/utils/DesktopResourceUrl';
import {resolveRetryAfterMs} from '@app/features/messaging/utils/RetryAfterUtils';
import {http} from '@app/features/platform/transport/RestTransport';
import {HttpError} from '@app/features/platform/types/EndpointError';
import {Logger} from '@app/features/platform/utils/AppLogger';
import type {RefreshAttachmentUrlsResponse} from '@fluxer/schema/src/domains/message/AttachmentSchemas';
import {makeAutoObservable, runInAction, untracked} from 'mobx';

const logger = new Logger('AttachmentUrlRefresher');

const FLUSH_WINDOW_MS = 16;
const MAX_URLS_PER_REQUEST = 50;
const UNSIGNED_RESULT_COOLDOWN_MS = 600_000;
const REQUEST_FAILURE_COOLDOWN_MS = 30_000;
const REQUEST_FAILURE_MAX_BACKOFF_STEPS = 4;
const RATE_LIMIT_FALLBACK_COOLDOWN_MS = 10_000;
const MAX_REQUEST_URL_LENGTH = 2048;
const MIN_SIGNATURE_LIFETIME_MS = 43_200_000;
const MAX_TIMER_DELAY_MS = 2_147_483_647;

type CacheEntry =
	| {kind: 'signature'; fields: string; expiresAtMs: number; goodUntilMs: number; usableUntilMs: number}
	| {kind: 'blocked'; untilMs: number};

interface QueuedRequest {
	url: string;
	resolvers: Array<(fields: string | null) => void>;
}

export function sameAttachmentSource(first: string, second: string): boolean {
	if (first === second) return true;
	return (
		stripAttachmentSignature(unwrapDesktopLocalResourceURL(first)) ===
		stripAttachmentSignature(unwrapDesktopLocalResourceURL(second))
	);
}

function isOwnAttachmentUrl(url: string): boolean {
	const mediaEndpoint = RuntimeConfig.getSnapshotOrNull()?.mediaEndpoint;
	return mediaEndpoint !== undefined && isAttachmentCdnUrl(url, mediaEndpoint);
}

function isNewerThanUrl(entry: {expiresAtMs: number}, signature: AttachmentUrlSignature | null): boolean {
	return signature === null || entry.expiresAtMs > signature.expiresAtMs;
}

class AttachmentUrlRefresher {
	entries = new Map<string, CacheEntry>();
	readonly queued = new Map<string, QueuedRequest>();
	readonly inFlight = new Map<string, Promise<string | null>>();
	readonly deferred = new Map<string, string>();
	flushTimer: ReturnType<typeof setTimeout> | null = null;
	wakeTimer: ReturnType<typeof setTimeout> | null = null;
	wakeAtMs = 0;
	retryAtMs = 0;
	requestFailures = 0;
	cooldownUntilMs = 0;
	generation = 0;
	revision = 0;
	wakeups = 0;

	constructor() {
		makeAutoObservable(
			this,
			{
				queued: false,
				inFlight: false,
				deferred: false,
				flushTimer: false,
				wakeTimer: false,
				wakeAtMs: false,
				retryAtMs: false,
				requestFailures: false,
				cooldownUntilMs: false,
				generation: false,
			},
			{autoBind: true},
		);
		if (typeof window !== 'undefined') {
			window.addEventListener('focus', this.wakeIfDue);
			window.addEventListener('online', this.retryNow);
			document.addEventListener('visibilitychange', this.wakeIfDue);
		}
	}

	fresh(url: string, options: {refreshUnsigned?: boolean} = {}): string {
		if (!url || !isOwnAttachmentUrl(url)) return url;
		const key = attachmentCacheKey(url);
		if (key === null) return url;
		void this.wakeups;
		const entry = this.entries.get(key);
		const signature = readAttachmentUrlSignature(url);
		const nowMs = Date.now();
		if (entry?.kind === 'signature' && entry.usableUntilMs > nowMs) {
			const renewing = entry.goodUntilMs <= nowMs;
			this.wakeAt(renewing ? entry.usableUntilMs : entry.goodUntilMs);
			if (renewing && attachmentUrlNeedsRefresh(url, nowMs)) this.enqueue(key, url, nowMs);
			return isNewerThanUrl(entry, signature) ? applyAttachmentSignature(url, entry.fields) : url;
		}
		if (signature !== null && signature.expiresAtMs > nowMs + ATTACHMENT_URL_REFRESH_MARGIN_MS) {
			this.wakeAt(signature.expiresAtMs - ATTACHMENT_URL_REFRESH_MARGIN_MS);
			return url;
		}
		if (signature === null && options.refreshUnsigned !== true) return url;
		if (entry?.kind === 'blocked' && entry.untilMs > nowMs) {
			this.wakeAt(entry.untilMs);
			return url;
		}
		this.enqueue(key, url, nowMs);
		return url;
	}

	freshWhenReady(url: string, options: {refreshUnsigned?: boolean} = {}): string | null {
		const current = this.fresh(url, options);
		if (!current || !isOwnAttachmentUrl(current)) return current;
		const nowMs = Date.now();
		if (nowMs < this.cooldownUntilMs) return current;
		const signature = readAttachmentUrlSignature(current);
		if (signature === null ? options.refreshUnsigned !== true : signature.expiresAtMs > nowMs) return current;
		const key = attachmentCacheKey(current);
		if (key === null) return current;
		const entry = this.entries.get(key);
		if (entry?.kind === 'blocked' || (entry?.kind === 'signature' && entry.usableUntilMs > nowMs)) return current;
		return this.queued.has(key) || this.inFlight.has(key) ? null : current;
	}

	warm(url: string): void {
		if (!url || !isOwnAttachmentUrl(url)) return;
		const nowMs = Date.now();
		if (!attachmentUrlNeedsRefresh(url, nowMs)) return;
		const key = attachmentCacheKey(url);
		if (key === null) return;
		const entry = untracked(() => this.entries.get(key));
		if (entry?.kind === 'signature' && entry.goodUntilMs > nowMs) return;
		if (entry?.kind === 'blocked' && entry.untilMs > nowMs) return;
		this.enqueue(key, url, nowMs);
	}

	async refresh(url: string, options: {force?: boolean} = {}): Promise<string> {
		if (!url) return url;
		const target = unwrapDesktopLocalResourceURL(url);
		if (!isOwnAttachmentUrl(target)) return url;
		const key = attachmentCacheKey(target);
		if (key === null) return url;
		const nowMs = Date.now();
		const entry = untracked(() => this.entries.get(key));
		if (entry?.kind === 'signature' && entry.goodUntilMs > nowMs) {
			if (isNewerThanUrl(entry, readAttachmentUrlSignature(target))) {
				return applyAttachmentSignature(target, entry.fields);
			}
			if (options.force !== true) return target;
		}
		if (options.force !== true) {
			if (entry?.kind === 'blocked' && entry.untilMs > nowMs) return target;
			if (!attachmentUrlNeedsRefresh(target, nowMs)) return target;
		}
		const fields = (await this.request(key, target, nowMs)) ?? this.usableFields(key);
		return fields === null ? target : applyAttachmentSignature(target, fields);
	}

	reset(): void {
		this.generation += 1;
		this.revision += 1;
		this.entries = new Map();
		for (const request of this.queued.values()) {
			for (const resolve of request.resolvers) resolve(null);
		}
		this.queued.clear();
		this.inFlight.clear();
		this.deferred.clear();
		this.cooldownUntilMs = 0;
		this.retryAtMs = 0;
		this.requestFailures = 0;
		if (this.flushTimer !== null) {
			clearTimeout(this.flushTimer);
			this.flushTimer = null;
		}
		if (this.wakeTimer !== null) {
			clearTimeout(this.wakeTimer);
			this.wakeTimer = null;
		}
	}

	private forgetExpired(nowMs: number): void {
		for (const [key, entry] of this.entries) {
			const untilMs = entry.kind === 'signature' ? entry.usableUntilMs : entry.untilMs;
			if (untilMs <= nowMs) this.entries.delete(key);
		}
	}

	private usableFields(key: string): string | null {
		const entry = untracked(() => this.entries.get(key));
		return entry?.kind === 'signature' && entry.usableUntilMs > Date.now() ? entry.fields : null;
	}

	private blockedEntry(key: string, untilMs: number, nowMs: number): CacheEntry {
		const entry = untracked(() => this.entries.get(key));
		if (entry?.kind !== 'signature' || entry.usableUntilMs <= nowMs) return {kind: 'blocked', untilMs};
		return {...entry, goodUntilMs: Math.min(entry.usableUntilMs, Math.max(entry.goodUntilMs, untilMs))};
	}

	private wakeAt(atMs: number): void {
		if (!Number.isFinite(atMs)) return;
		if (this.wakeTimer !== null) {
			if (atMs >= this.wakeAtMs) return;
			clearTimeout(this.wakeTimer);
		}
		this.wakeAtMs = atMs;
		this.wakeTimer = setTimeout(this.wake, Math.min(Math.max(atMs - Date.now(), 0), MAX_TIMER_DELAY_MS));
	}

	private wake(): void {
		this.wakeTimer = null;
		const nowMs = Date.now();
		if (this.wakeAtMs > nowMs) {
			this.wakeAt(this.wakeAtMs);
			return;
		}
		if (this.deferred.size > 0) {
			if (nowMs < this.retryAtMs) this.wakeAt(this.retryAtMs);
			else this.requeueDeferred();
		}
		runInAction(() => {
			this.forgetExpired(nowMs);
			this.notify();
		});
	}

	private wakeIfDue(): void {
		if (document.visibilityState === 'hidden') return;
		if (this.wakeTimer === null || this.wakeAtMs > Date.now()) return;
		clearTimeout(this.wakeTimer);
		this.wake();
	}

	private retryNow(): void {
		this.requestFailures = 0;
		this.retryAtMs = this.cooldownUntilMs;
		if (this.deferred.size === 0 || Date.now() < this.cooldownUntilMs) return;
		this.requeueDeferred();
	}

	private notify(): void {
		this.wakeups += 1;
		this.revision += 1;
	}

	private requeueDeferred(): void {
		for (const [key, url] of this.deferred) {
			if (this.inFlight.has(key) || this.queued.has(key)) continue;
			this.queued.set(key, {url, resolvers: []});
		}
		this.deferred.clear();
		if (this.queued.size > 0) this.scheduleFlush();
	}

	private defer(batch: Array<[string, QueuedRequest]>, retryAtMs: number): void {
		for (const [key, request] of batch) this.deferred.set(key, request.url);
		this.retryAtMs = Math.max(this.retryAtMs, retryAtMs);
		this.wakeAt(this.retryAtMs);
	}

	private enqueue(key: string, url: string, nowMs: number): void {
		if (this.inFlight.has(key)) return;
		if (this.queued.has(key)) return;
		if (nowMs < this.cooldownUntilMs) {
			this.deferred.set(key, stripAttachmentSignature(url));
			return;
		}
		this.queued.set(key, {url: stripAttachmentSignature(url), resolvers: []});
		this.scheduleFlush();
	}

	private request(key: string, url: string, nowMs: number): Promise<string | null> {
		const existing = this.inFlight.get(key);
		if (existing) return existing;
		if (nowMs < this.cooldownUntilMs) {
			this.deferred.set(key, stripAttachmentSignature(url));
			return Promise.resolve(null);
		}
		const queued = this.queued.get(key) ?? {url: stripAttachmentSignature(url), resolvers: []};
		this.queued.set(key, queued);
		this.scheduleFlush();
		return new Promise<string | null>((resolve) => {
			queued.resolvers.push(resolve);
		});
	}

	private scheduleFlush(): void {
		if (this.flushTimer !== null) return;
		this.flushTimer = setTimeout(() => {
			this.flushTimer = null;
			void this.flush();
		}, FLUSH_WINDOW_MS);
	}

	private async flush(): Promise<void> {
		if (this.queued.size === 0) return;
		const nowMs = Date.now();
		if (nowMs < this.cooldownUntilMs) {
			this.deferQueue();
			return;
		}
		const batch: Array<[string, QueuedRequest]> = [];
		const oversized: Array<[string, QueuedRequest]> = [];
		for (const item of this.queued) {
			if (batch.length === MAX_URLS_PER_REQUEST) break;
			this.queued.delete(item[0]);
			(item[1].url.length > MAX_REQUEST_URL_LENGTH ? oversized : batch).push(item);
		}
		if (this.queued.size > 0) this.scheduleFlush();
		if (oversized.length > 0) this.blockOversized(oversized, nowMs);
		if (batch.length === 0) return;
		const generation = this.generation;
		const send = this.send(batch, generation);
		for (const [key] of batch) {
			this.inFlight.set(
				key,
				send.then((outcome) => outcome.get(key) ?? null),
			);
		}
		const outcome = await send;
		for (const [key, request] of batch) {
			if (generation === this.generation) this.inFlight.delete(key);
			const fields = outcome.get(key) ?? null;
			for (const resolve of request.resolvers) resolve(fields);
		}
	}

	private async send(batch: Array<[string, QueuedRequest]>, generation: number): Promise<Map<string, string>> {
		const resolved = new Map<string, string>();
		try {
			const response = await http.post<RefreshAttachmentUrlsResponse>(Endpoints.ATTACHMENTS_REFRESH_URLS, {
				body: {attachment_urls: batch.map(([, request]) => request.url)},
			});
			if (generation !== this.generation) return resolved;
			const byOriginal = new Map(
				(response.body?.refreshed_urls ?? []).map((entry) => [entry.original, entry.refreshed]),
			);
			const nowMs = Date.now();
			const updates: Array<[string, CacheEntry]> = [];
			for (const [key, request] of batch) {
				const refreshed = byOriginal.get(request.url);
				const fields = refreshed === undefined ? null : readAttachmentSignatureFields(refreshed);
				const expiry = refreshed === undefined ? null : readAttachmentUrlSignature(refreshed);
				if (fields === null || expiry === null) {
					updates.push([key, this.blockedEntry(key, nowMs + UNSIGNED_RESULT_COOLDOWN_MS, nowMs)]);
					continue;
				}
				resolved.set(key, fields);
				const usableUntilMs = Math.max(expiry.expiresAtMs, nowMs + MIN_SIGNATURE_LIFETIME_MS);
				updates.push([
					key,
					{
						kind: 'signature',
						fields,
						expiresAtMs: expiry.expiresAtMs,
						goodUntilMs: usableUntilMs - ATTACHMENT_URL_REFRESH_MARGIN_MS,
						usableUntilMs,
					},
				]);
			}
			this.requestFailures = 0;
			runInAction(() => {
				this.forgetExpired(nowMs);
				for (const [key, entry] of updates) this.entries.set(key, entry);
				this.revision += 1;
			});
			return resolved;
		} catch (error) {
			if (generation !== this.generation) return resolved;
			const nowMs = Date.now();
			if (error instanceof HttpError && error.status === 429) {
				this.cooldownUntilMs = nowMs + (resolveRetryAfterMs(error) ?? RATE_LIMIT_FALLBACK_COOLDOWN_MS);
				this.defer(batch, this.cooldownUntilMs);
				runInAction(this.notify);
				return resolved;
			}
			logger.warn('Failed to refresh attachment URLs', error);
			const rejected = error instanceof HttpError && error.status >= 400 && error.status < 500;
			const untilMs = nowMs + this.nextFailureBackoffMs();
			if (!rejected) this.defer(batch, untilMs);
			runInAction(() => {
				this.forgetExpired(nowMs);
				for (const [key] of batch) this.entries.set(key, this.blockedEntry(key, untilMs, nowMs));
				this.revision += 1;
			});
			return resolved;
		}
	}

	private nextFailureBackoffMs(): number {
		const step = Math.min(this.requestFailures, REQUEST_FAILURE_MAX_BACKOFF_STEPS);
		this.requestFailures += 1;
		return REQUEST_FAILURE_COOLDOWN_MS * 2 ** step;
	}

	private blockOversized(oversized: Array<[string, QueuedRequest]>, nowMs: number): void {
		runInAction(() => {
			for (const [key] of oversized) {
				this.entries.set(key, this.blockedEntry(key, nowMs + UNSIGNED_RESULT_COOLDOWN_MS, nowMs));
			}
			this.revision += 1;
		});
		for (const [, request] of oversized) {
			for (const resolve of request.resolvers) resolve(null);
		}
	}

	private deferQueue(): void {
		const pending = Array.from(this.queued.entries());
		this.queued.clear();
		this.defer(pending, this.cooldownUntilMs);
		for (const [, request] of pending) {
			for (const resolve of request.resolvers) resolve(null);
		}
	}
}

const attachmentUrlRefresher = new AttachmentUrlRefresher();

ResettableStates.register(attachmentUrlRefresher, () => attachmentUrlRefresher.reset());

export default attachmentUrlRefresher;
