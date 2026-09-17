// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/api/Logger';
import {DiscardPolicy, JetStreamApiError, RetentionPolicy, StorageType} from '@nats-io/jetstream';
import {nanos} from '@nats-io/transport-node';
import {JetStreamConnectionManager} from '@pkgs/nats/src/JetStreamConnectionManager';

export type StorageChangeOp = 'put' | 'delete';

export interface StorageChangeEvent {
	bucket: string;
	key: string;
	op: StorageChangeOp;
	size: number | null;
	etag: string | null;
	contentType: string | null;
	at: string;
}

export interface StorageChangeSink {
	publish(subject: string, body: string, msgID: string): Promise<void>;
	close(): Promise<void>;
}

interface StorageChangeFeedStats {
	queued: number;
	published: number;
	dropped: number;
	failed: number;
}

interface StorageChangeFeedOptions {
	capacity?: number;
	concurrency?: number;
	maxAttempts?: number;
	retryDelayMs?: number;
	reportIntervalMs?: number;
}

interface QueuedChange {
	subject: string;
	body: string;
	msgID: string;
	attempts: number;
}

const DEFAULT_CAPACITY = 20_000;
const DEFAULT_CONCURRENCY = 64;
const DEFAULT_MAX_ATTEMPTS = 5;
const DEFAULT_RETRY_DELAY_MS = 1000;
const DEFAULT_REPORT_INTERVAL_MS = 60_000;

const STREAM_SUBJECTS = 'storage.>';
const STREAM_MAX_AGE_MS = 14 * 24 * 60 * 60 * 1000;
const STREAM_MAX_BYTES = 1024 * 1024 * 1024;
const STREAM_DUPLICATE_WINDOW_MS = 2 * 60 * 1000;
const PUBLISH_TIMEOUT_MS = 5000;
const STREAM_NAME_IN_USE_ERR_CODE = 10058;
const STREAM_NOT_FOUND_ERR_CODE = 10059;

function delay(ms: number): Promise<void> {
	return new Promise((resolve) => {
		setTimeout(resolve, ms).unref();
	});
}

function subjectToken(bucket: string): string {
	return bucket.replace(/[.*>\s]/gu, '_');
}

function jsErrorCode(error: unknown): number | null {
	return error instanceof JetStreamApiError ? error.code : null;
}

export class StorageChangeFeed {
	private readonly sink: StorageChangeSink;
	private readonly capacity: number;
	private readonly concurrency: number;
	private readonly maxAttempts: number;
	private readonly retryDelayMs: number;
	private readonly reportIntervalMs: number;
	private readonly queue: Array<QueuedChange> = [];
	private draining: Promise<void> | null = null;
	private stopped = false;
	private published = 0;
	private dropped = 0;
	private failed = 0;
	private lastReportAt = Number.NEGATIVE_INFINITY;
	private reportedDropped = 0;
	private reportedFailed = 0;

	constructor(sink: StorageChangeSink, options: StorageChangeFeedOptions = {}) {
		this.sink = sink;
		this.capacity = options.capacity ?? DEFAULT_CAPACITY;
		this.concurrency = options.concurrency ?? DEFAULT_CONCURRENCY;
		this.maxAttempts = options.maxAttempts ?? DEFAULT_MAX_ATTEMPTS;
		this.retryDelayMs = options.retryDelayMs ?? DEFAULT_RETRY_DELAY_MS;
		this.reportIntervalMs = options.reportIntervalMs ?? DEFAULT_REPORT_INTERVAL_MS;
	}

	record(event: StorageChangeEvent): void {
		try {
			if (this.stopped || this.queue.length >= this.capacity) {
				this.dropped++;
				this.report(null);
				return;
			}
			this.queue.push({
				subject: `storage.${subjectToken(event.bucket)}.${event.op}`,
				body: JSON.stringify(event),
				msgID: `${event.bucket}:${event.key}:${event.etag ?? event.at}`,
				attempts: 0,
			});
			this.draining ??= this.scheduleDrain();
		} catch (error) {
			this.dropped++;
			this.report(error);
		}
	}

	stats(): StorageChangeFeedStats {
		return {queued: this.queue.length, published: this.published, dropped: this.dropped, failed: this.failed};
	}

	async idle(): Promise<void> {
		while (this.draining !== null) {
			await this.draining;
		}
	}

	async close(timeoutMs: number): Promise<void> {
		const deadline = delay(timeoutMs).then(() => false);
		while (this.draining !== null) {
			const drained = await Promise.race([this.draining.then(() => true), deadline]);
			if (!drained) break;
		}
		this.stopped = true;
		this.dropped += this.queue.length;
		this.queue.length = 0;
		Logger.info(this.stats(), 'Storage change feed stopped');
		try {
			await this.sink.close();
		} catch (error) {
			Logger.warn({err: error}, 'Storage change feed connection did not close cleanly');
		}
	}

	private scheduleDrain(): Promise<void> {
		return new Promise<void>((resolve) => setImmediate(resolve))
			.then(() => this.drain())
			.catch((error: unknown) => {
				this.report(error);
			})
			.finally(() => {
				this.draining = null;
				if (this.queue.length > 0 && !this.stopped) {
					this.draining = this.scheduleDrain();
				}
			});
	}

	private async drain(): Promise<void> {
		while (this.queue.length > 0 && !this.stopped) {
			const batch = this.queue.splice(0, this.concurrency);
			const results = await Promise.allSettled(
				batch.map(async (change) => {
					await this.sink.publish(change.subject, change.body, change.msgID);
				}),
			);
			const retry: Array<QueuedChange> = [];
			let failure: PromiseRejectedResult | null = null;
			for (const [index, result] of results.entries()) {
				if (result.status === 'fulfilled') {
					this.published++;
					continue;
				}
				failure = result;
				const change = batch[index];
				change.attempts++;
				if (this.stopped) {
					this.dropped++;
				} else if (change.attempts >= this.maxAttempts) {
					this.failed++;
				} else {
					retry.push(change);
				}
			}
			if (failure === null) continue;
			this.queue.unshift(...retry);
			this.report(failure.reason);
			await delay(this.retryDelayMs);
		}
	}

	private report(error: unknown): void {
		const now = Date.now();
		if (now - this.lastReportAt < this.reportIntervalMs) return;
		if (error === null && this.dropped === this.reportedDropped && this.failed === this.reportedFailed) return;
		this.lastReportAt = now;
		this.reportedDropped = this.dropped;
		this.reportedFailed = this.failed;
		Logger.warn({err: error ?? undefined, ...this.stats()}, 'Storage change feed is dropping or failing events');
	}
}

export class JetStreamStorageChangeSink implements StorageChangeSink {
	private readonly connection: JetStreamConnectionManager;
	private readonly stream: string;
	private ready: Promise<void> | null = null;

	constructor(options: {url: string; token: string; stream: string}) {
		this.connection = new JetStreamConnectionManager({
			url: options.url,
			token: options.token || undefined,
			name: 'storage-change-feed',
		});
		this.stream = options.stream;
	}

	async publish(subject: string, body: string, msgID: string): Promise<void> {
		this.ready ??= this.open().catch((error: unknown) => {
			this.ready = null;
			throw error;
		});
		await this.ready;
		if (this.connection.isClosed()) {
			this.ready = null;
			throw new Error('Storage change feed connection is closed');
		}
		await this.connection.getJetStreamClient().publish(subject, body, {msgID, timeout: PUBLISH_TIMEOUT_MS});
	}

	async close(): Promise<void> {
		await this.connection.drain();
	}

	private async open(): Promise<void> {
		await this.connection.connect();
		const jsm = await this.connection.getJetStreamManager();
		try {
			await jsm.streams.info(this.stream);
			return;
		} catch (error) {
			if (jsErrorCode(error) !== STREAM_NOT_FOUND_ERR_CODE) throw error;
		}
		try {
			await jsm.streams.add({
				name: this.stream,
				subjects: [STREAM_SUBJECTS],
				retention: RetentionPolicy.Limits,
				storage: StorageType.File,
				max_age: nanos(STREAM_MAX_AGE_MS),
				max_bytes: STREAM_MAX_BYTES,
				duplicate_window: nanos(STREAM_DUPLICATE_WINDOW_MS),
				discard: DiscardPolicy.Old,
				num_replicas: 1,
			});
			Logger.info({stream: this.stream}, 'Created storage change feed stream');
		} catch (error) {
			if (jsErrorCode(error) !== STREAM_NAME_IN_USE_ERR_CODE) throw error;
		}
	}
}
