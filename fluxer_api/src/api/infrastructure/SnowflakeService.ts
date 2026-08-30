// SPDX-License-Identifier: AGPL-3.0-or-later

import type {INatsConnectionManager} from '@pkgs/nats/src/INatsConnectionManager';
import {StringCodec} from 'nats';
import {Logger} from '../Logger';
import {isJsonRecord, parseJsonWithGuard} from '../utils/JsonBoundaryUtils';
import type {ISnowflakeService} from './ISnowflakeService';

const DEFAULT_REMOTE_SUBJECT = 'svc.snowflakes';
const DEFAULT_REMOTE_BATCH_SIZE = 128;
const DEFAULT_REMOTE_LOW_WATERMARK = 32;
const DEFAULT_REMOTE_TIMEOUT_MS = 6000;
const DEFAULT_REMOTE_MAX_BUFFER_AGE_MS = 5000;
const MAX_REMOTE_BATCH_SIZE = 512;

interface SnowflakeServiceOptions {
	connectionManager: INatsConnectionManager;
	subject?: string;
	batchSize?: number;
	lowWatermark?: number;
	requestTimeoutMs?: number;
	maxBufferAgeMs?: number;
}

interface RemoteSnowflakeResponse {
	ids?: Array<string>;
	error?: string;
}

interface RemoteSnowflakeRequest {
	op: 'GenerateBatch';
	count: number;
	routing_key?: string;
}

function isRemoteSnowflakeResponse(value: unknown): value is RemoteSnowflakeResponse {
	if (!isJsonRecord(value)) return false;
	return (
		(value.ids === undefined || (Array.isArray(value.ids) && value.ids.every((id) => typeof id === 'string'))) &&
		(value.error === undefined || typeof value.error === 'string')
	);
}

function resolveIntegerOption(value: number | undefined, fallback: number, min: number, max: number): number {
	if (typeof value !== 'number' || !Number.isInteger(value)) {
		return fallback;
	}
	return Math.min(max, Math.max(min, value));
}

export class SnowflakeService implements ISnowflakeService {
	private readonly connectionManager: INatsConnectionManager;
	private readonly subject: string;
	private readonly batchSize: number;
	private readonly lowWatermark: number;
	private readonly requestTimeoutMs: number;
	private readonly maxBufferAgeMs: number;
	private readonly codec = StringCodec();
	private initialized = false;
	private shutdownRequested = false;
	private buffer: Array<bigint> = [];
	private bufferOffset = 0;
	private bufferFetchedAtMs: number | null = null;
	private initializationPromise: Promise<void> | null = null;
	private refillPromise: Promise<void> | null = null;

	constructor(options: SnowflakeServiceOptions) {
		this.connectionManager = options.connectionManager;
		this.subject = options.subject ?? DEFAULT_REMOTE_SUBJECT;
		this.batchSize = resolveIntegerOption(options.batchSize, DEFAULT_REMOTE_BATCH_SIZE, 1, MAX_REMOTE_BATCH_SIZE);
		this.lowWatermark = Math.min(
			resolveIntegerOption(options.lowWatermark, DEFAULT_REMOTE_LOW_WATERMARK, 0, MAX_REMOTE_BATCH_SIZE),
			Math.max(0, this.batchSize - 1),
		);
		this.requestTimeoutMs = resolveIntegerOption(options.requestTimeoutMs, DEFAULT_REMOTE_TIMEOUT_MS, 1, 60000);
		this.maxBufferAgeMs = resolveIntegerOption(options.maxBufferAgeMs, DEFAULT_REMOTE_MAX_BUFFER_AGE_MS, 1, 60000);
	}

	async initialize(): Promise<void> {
		if (this.shutdownRequested) {
			return;
		}
		if (this.initialized) {
			return;
		}
		if (!this.initializationPromise) {
			this.initializationPromise = (async () => {
				await this.ensureConnected();
				await this.refillBuffer();
				this.initialized = true;
			})().finally(() => {
				this.initializationPromise = null;
			});
		}
		await this.initializationPromise;
	}

	async reinitialize(): Promise<void> {
		this.shutdownRequested = false;
		this.initialized = false;
		this.buffer = [];
		this.bufferOffset = 0;
		this.bufferFetchedAtMs = null;
		this.initializationPromise = null;
		await this.initialize();
	}

	async shutdown(): Promise<void> {
		this.shutdownRequested = true;
		this.initialized = false;
		this.buffer = [];
		this.bufferOffset = 0;
		this.bufferFetchedAtMs = null;
		const refillPromise = this.refillPromise;
		this.refillPromise = null;
		if (refillPromise) {
			await refillPromise.catch(() => undefined);
		}
		await this.connectionManager.drain();
	}

	async generate(): Promise<bigint> {
		await this.ensureInitialized();
		this.discardExpiredBuffer();
		const bufferedId = this.takeBufferedId();
		if (bufferedId != null) {
			this.scheduleRefillIfNeeded();
			return bufferedId;
		}
		await this.refillBuffer();
		const refilledId = this.takeBufferedId();
		if (refilledId == null) {
			throw new Error('Snowflake service returned no IDs');
		}
		this.scheduleRefillIfNeeded();
		return refilledId;
	}

	async generateForChannel(channelId: string | bigint): Promise<bigint> {
		await this.ensureInitialized();
		const routingKey = `channel:${channelId.toString()}`;
		const ids = await this.requestBatch(1, routingKey);
		const id = ids[0];
		if (id == null) {
			throw new Error('Snowflake service returned no IDs');
		}
		return id;
	}

	private async ensureInitialized(): Promise<void> {
		if (this.shutdownRequested) {
			throw new Error('SnowflakeService is shut down');
		}
		if (!this.initialized) {
			await this.initialize();
		}
		if (!this.initialized) {
			throw new Error('SnowflakeService not initialized');
		}
	}

	private async refillBuffer(): Promise<void> {
		if (this.refillPromise) {
			await this.refillPromise;
			return;
		}
		this.refillPromise = (async () => {
			await this.ensureConnected();
			const ids = await this.requestBatch(this.batchSize);
			if (ids.length === 0) {
				throw new Error('Snowflake service returned an empty batch');
			}
			this.discardExpiredBuffer();
			this.compactBuffer();
			if (this.buffer.length === 0) {
				this.bufferFetchedAtMs = Date.now();
			}
			this.buffer.push(...ids);
		})().finally(() => {
			this.refillPromise = null;
		});
		await this.refillPromise;
	}

	private async requestBatch(count: number, routingKey?: string): Promise<Array<bigint>> {
		const connection = this.connectionManager.getConnection();
		const request: RemoteSnowflakeRequest = {
			op: 'GenerateBatch',
			count,
		};
		if (routingKey) {
			request.routing_key = routingKey;
		}
		const responseMessage = await connection.request(this.subject, this.codec.encode(JSON.stringify(request)), {
			timeout: this.requestTimeoutMs,
		});
		const response = parseJsonWithGuard(this.codec.decode(responseMessage.data), isRemoteSnowflakeResponse);
		if (!response) {
			throw new Error('Snowflake service returned an invalid response');
		}
		if (response.error) {
			throw new Error(`Snowflake service error: ${response.error}`);
		}
		if (!Array.isArray(response.ids)) {
			throw new Error('Snowflake service returned an invalid response');
		}
		return response.ids.map((id) => BigInt(id));
	}

	private async ensureConnected(): Promise<void> {
		if (this.connectionManager.isClosed()) {
			await this.connectionManager.connect();
		}
	}

	private takeBufferedId(): bigint | null {
		if (this.bufferOffset >= this.buffer.length) {
			this.buffer = [];
			this.bufferOffset = 0;
			this.bufferFetchedAtMs = null;
			return null;
		}
		const id = this.buffer[this.bufferOffset];
		this.bufferOffset += 1;
		if (this.bufferOffset > 1024) {
			this.compactBuffer();
		}
		return id;
	}

	private availableIds(): number {
		return this.buffer.length - this.bufferOffset;
	}

	private compactBuffer(): void {
		if (this.bufferOffset === 0) {
			return;
		}
		this.buffer = this.buffer.slice(this.bufferOffset);
		this.bufferOffset = 0;
		if (this.buffer.length === 0) {
			this.bufferFetchedAtMs = null;
		}
	}

	private discardExpiredBuffer(): void {
		if (this.bufferFetchedAtMs == null) {
			return;
		}
		if (Date.now() - this.bufferFetchedAtMs <= this.maxBufferAgeMs) {
			return;
		}
		this.buffer = [];
		this.bufferOffset = 0;
		this.bufferFetchedAtMs = null;
	}

	private scheduleRefillIfNeeded(): void {
		if (this.shutdownRequested || this.availableIds() > this.lowWatermark || this.refillPromise) {
			return;
		}
		void this.refillBuffer().catch((error) => {
			Logger.error({error}, 'Failed to refill snowflake buffer');
		});
	}
}
