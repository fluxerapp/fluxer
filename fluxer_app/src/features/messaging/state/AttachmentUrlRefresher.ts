// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {
	ATTACHMENT_URL_REFRESH_MARGIN_MS,
	applyAttachmentSignature,
	attachmentCacheKey,
	attachmentUrlNeedsRefresh,
	isAttachmentCdnUrl,
	readAttachmentSignatureFields,
	readAttachmentUrlSignature,
	stripAttachmentSignature,
} from '@app/features/messaging/utils/AttachmentCdnUrl';
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
const TRANSPORT_ERROR_COOLDOWN_MS = 30_000;
const RATE_LIMIT_FALLBACK_COOLDOWN_MS = 10_000;

type CacheEntry = {kind: 'signature'; fields: string; goodUntilMs: number} | {kind: 'blocked'; untilMs: number};

interface QueuedRequest {
	url: string;
	resolvers: Array<(fields: string | null) => void>;
}

class AttachmentUrlRefresher {
	entries = new Map<string, CacheEntry>();
	readonly queued = new Map<string, QueuedRequest>();
	readonly inFlight = new Map<string, Promise<string | null>>();
	flushTimer: ReturnType<typeof setTimeout> | null = null;
	cooldownUntilMs = 0;
	generation = 0;
	revision = 0;

	constructor() {
		makeAutoObservable(
			this,
			{
				queued: false,
				inFlight: false,
				flushTimer: false,
				cooldownUntilMs: false,
				generation: false,
			},
			{autoBind: true},
		);
	}

	fresh(url: string, options: {refreshUnsigned?: boolean} = {}): string {
		if (!url || !isAttachmentCdnUrl(url, RuntimeConfig.mediaEndpoint)) return url;
		const signature = readAttachmentUrlSignature(url);
		const nowMs = Date.now();
		if (signature !== null && signature.expiresAtMs > nowMs + ATTACHMENT_URL_REFRESH_MARGIN_MS) return url;
		const key = attachmentCacheKey(url);
		if (key === null) return url;
		const entry = this.entries.get(key);
		if (entry?.kind === 'signature' && entry.goodUntilMs > nowMs) {
			return applyAttachmentSignature(url, entry.fields);
		}
		if (signature === null && options.refreshUnsigned !== true) return url;
		if (entry?.kind === 'blocked' && entry.untilMs > nowMs) return url;
		this.enqueue(key, url, nowMs);
		return url;
	}

	warm(url: string): void {
		if (!url || !isAttachmentCdnUrl(url, RuntimeConfig.mediaEndpoint)) return;
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
		if (!url || !isAttachmentCdnUrl(url, RuntimeConfig.mediaEndpoint)) return url;
		const key = attachmentCacheKey(url);
		if (key === null) return url;
		const nowMs = Date.now();
		const entry = untracked(() => this.entries.get(key));
		if (entry?.kind === 'signature' && entry.goodUntilMs > nowMs) {
			return applyAttachmentSignature(url, entry.fields);
		}
		if (options.force !== true) {
			if (entry?.kind === 'blocked' && entry.untilMs > nowMs) return url;
			if (!attachmentUrlNeedsRefresh(url, nowMs)) return url;
		}
		const fields = await this.request(key, url, nowMs);
		return fields === null ? url : applyAttachmentSignature(url, fields);
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
		this.cooldownUntilMs = 0;
		if (this.flushTimer !== null) {
			clearTimeout(this.flushTimer);
			this.flushTimer = null;
		}
	}

	private forgetExpired(nowMs: number): void {
		for (const [key, entry] of this.entries) {
			const untilMs = entry.kind === 'signature' ? entry.goodUntilMs : entry.untilMs;
			if (untilMs <= nowMs) this.entries.delete(key);
		}
	}

	private enqueue(key: string, url: string, nowMs: number): void {
		if (this.inFlight.has(key)) return;
		if (nowMs < this.cooldownUntilMs) return;
		if (this.queued.has(key)) return;
		this.queued.set(key, {url: stripAttachmentSignature(url), resolvers: []});
		this.scheduleFlush();
	}

	private request(key: string, url: string, nowMs: number): Promise<string | null> {
		const existing = this.inFlight.get(key);
		if (existing) return existing;
		if (nowMs < this.cooldownUntilMs) return Promise.resolve(null);
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
			this.drainQueue();
			return;
		}
		const batch = Array.from(this.queued.entries()).slice(0, MAX_URLS_PER_REQUEST);
		for (const [key] of batch) this.queued.delete(key);
		if (this.queued.size > 0) this.scheduleFlush();
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
					updates.push([key, {kind: 'blocked', untilMs: nowMs + UNSIGNED_RESULT_COOLDOWN_MS}]);
					continue;
				}
				resolved.set(key, fields);
				updates.push([
					key,
					{kind: 'signature', fields, goodUntilMs: expiry.expiresAtMs - ATTACHMENT_URL_REFRESH_MARGIN_MS},
				]);
			}
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
				return resolved;
			}
			logger.warn('Failed to refresh attachment URLs', error);
			runInAction(() => {
				this.forgetExpired(nowMs);
				for (const [key] of batch) {
					this.entries.set(key, {kind: 'blocked', untilMs: nowMs + TRANSPORT_ERROR_COOLDOWN_MS});
				}
				this.revision += 1;
			});
			return resolved;
		}
	}

	private drainQueue(): void {
		for (const request of this.queued.values()) {
			for (const resolve of request.resolvers) resolve(null);
		}
		this.queued.clear();
	}
}

export default new AttachmentUrlRefresher();
