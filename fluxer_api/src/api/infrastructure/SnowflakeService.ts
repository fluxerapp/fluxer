// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import type {ISnowflakeService} from '@app/api/infrastructure/ISnowflakeService';
import {Logger} from '@app/api/Logger';
import {requireIntegerInRange} from '@app/api/utils/IntegerOptions';
import {isJsonRecord, parseJsonWithGuard} from '@app/api/utils/JsonBoundaryUtils';
import {ServiceUnavailableError} from '@fluxer/errors/src/domains/core/ServiceUnavailableError';
import type {INatsConnectionManager} from '@pkgs/nats/src/INatsConnectionManager';
import {StringCodec} from 'nats';

const DEFAULT_REMOTE_SUBJECT = 'svc.snowflakes';
const DEFAULT_REMOTE_BATCH_SIZE = 128;
const DEFAULT_REMOTE_LOW_WATERMARK = 32;
const DEFAULT_REMOTE_TIMEOUT_MS = 6000;
const DEFAULT_REMOTE_MAX_BUFFER_AGE_MS = 5000;
const MAX_REMOTE_BATCH_SIZE = 512;
const MAX_PENDING_GENERATIONS = 1024;
const MAX_REMOTE_RESPONSE_BYTES = 16 * 1024;
const MAX_REMOTE_ID = 0xffffffffffffffffn;

class SnowflakeServiceStoppedError extends Error {
	constructor() {
		super('SnowflakeService is shut down');
		this.name = 'SnowflakeServiceStoppedError';
	}
}

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

function parseRemoteSnowflakeId(value: string): bigint {
	if (value.length === 0 || value.length > 20 || /\D/.test(value)) {
		throw new Error('Snowflake ID must be a canonical unsigned 64-bit integer');
	}
	const id = BigInt(value);
	if (id > MAX_REMOTE_ID || id.toString() !== value) {
		throw new Error('Snowflake ID must be a canonical unsigned 64-bit integer');
	}
	return id;
}

export class SnowflakeService implements ISnowflakeService {
	private readonly connectionManager: INatsConnectionManager;
	private readonly subject: string;
	private readonly batchSize: number;
	private readonly lowWatermark: number;
	private readonly requestTimeoutMs: number;
	private readonly maxBufferAgeMs: number;
	private readonly codec = StringCodec();
	private phase: 'idle' | 'starting' | 'ready' | 'stopping' | 'stopped' = 'idle';
	private buffer: Array<bigint> = [];
	private bufferOffset = 0;
	private bufferFetchedAtMs: number | null = null;
	private initializationPromise: Promise<void> | null = null;
	private refillPromise: Promise<void> | null = null;
	private shutdownPromise: Promise<void> | null = null;
	private pendingGenerations = 0;
	private resolveGenerationsDrained: (() => void) | null = null;

	constructor({
		connectionManager,
		subject = DEFAULT_REMOTE_SUBJECT,
		batchSize = DEFAULT_REMOTE_BATCH_SIZE,
		lowWatermark = Math.min(DEFAULT_REMOTE_LOW_WATERMARK, batchSize - 1),
		requestTimeoutMs = DEFAULT_REMOTE_TIMEOUT_MS,
		maxBufferAgeMs = DEFAULT_REMOTE_MAX_BUFFER_AGE_MS,
	}: SnowflakeServiceOptions) {
		this.connectionManager = connectionManager;
		this.subject = subject;
		this.batchSize = requireIntegerInRange('FLUXER_SNOWFLAKE_SERVICE_BATCH_SIZE', batchSize, 1, MAX_REMOTE_BATCH_SIZE);
		this.lowWatermark = requireIntegerInRange(
			'FLUXER_SNOWFLAKE_SERVICE_LOW_WATERMARK',
			lowWatermark,
			0,
			this.batchSize - 1,
		);
		this.requestTimeoutMs = requireIntegerInRange(
			'FLUXER_SNOWFLAKE_SERVICE_REQUEST_TIMEOUT_MS',
			requestTimeoutMs,
			1,
			60_000,
		);
		this.maxBufferAgeMs = requireIntegerInRange(
			'FLUXER_SNOWFLAKE_SERVICE_MAX_BUFFER_AGE_MS',
			maxBufferAgeMs,
			1,
			60_000,
		);
	}

	async initialize(): Promise<void> {
		this.assertActive();
		if (this.phase === 'ready') return;
		if (!this.initializationPromise) {
			this.phase = 'starting';
			this.initializationPromise = this.start().finally(() => {
				this.initializationPromise = null;
			});
		}
		await this.initializationPromise;
	}

	private async start(): Promise<void> {
		try {
			await this.refillBuffer();
			this.assertActive();
			this.phase = 'ready';
		} catch (error) {
			if (this.phase === 'starting') this.phase = 'idle';
			throw error;
		}
	}

	shutdown(): Promise<void> {
		if (this.shutdownPromise) return this.shutdownPromise;
		this.phase = 'stopping';
		this.clearBuffer();
		this.shutdownPromise = this.close();
		return this.shutdownPromise;
	}

	private async close(): Promise<void> {
		try {
			await Promise.allSettled([this.initializationPromise, this.refillPromise]);
			if (this.pendingGenerations > 0) {
				await new Promise<void>((resolve) => {
					this.resolveGenerationsDrained = resolve;
				});
			}
			await this.connectionManager.drain();
		} finally {
			this.phase = 'stopped';
			this.clearBuffer();
		}
	}

	generate(): Promise<bigint> {
		return this.withGeneration(async (deadline) => {
			await this.ensureInitialized(deadline);
			for (;;) {
				this.assertActive();
				this.remainingTime(deadline);
				this.discardExpiredBuffer();
				const id = this.takeBufferedId();
				if (id !== null) {
					this.scheduleRefillIfNeeded();
					return id;
				}
				await this.waitFor(this.refillBuffer(), deadline);
			}
		});
	}

	generateForChannel(channelId: string | bigint): Promise<bigint> {
		return this.withGeneration(async (deadline) => {
			const routingKey = `channel:${parseRemoteSnowflakeId(channelId.toString())}`;
			await this.ensureInitialized(deadline);
			const ids = await this.requestBatch(1, routingKey);
			return ids[0]!;
		});
	}

	private async withGeneration(operation: (deadline: number) => Promise<bigint>): Promise<bigint> {
		this.assertActive();
		if (this.pendingGenerations >= MAX_PENDING_GENERATIONS) {
			Logger.warn(
				{pendingGenerations: this.pendingGenerations, limit: MAX_PENDING_GENERATIONS},
				'Snowflake generation capacity exceeded',
			);
			throw new ServiceUnavailableError({headers: {'Retry-After': '1'}});
		}
		this.pendingGenerations++;
		try {
			const id = await operation(performance.now() + this.requestTimeoutMs);
			this.assertActive();
			return id;
		} finally {
			this.pendingGenerations--;
			if (this.pendingGenerations === 0) {
				this.resolveGenerationsDrained?.();
				this.resolveGenerationsDrained = null;
			}
		}
	}

	private assertActive(): void {
		if (this.phase === 'stopping' || this.phase === 'stopped') throw new SnowflakeServiceStoppedError();
	}

	private remainingTime(deadline: number): number {
		const remaining = deadline - performance.now();
		if (remaining <= 0) throw new Error('Snowflake generation timed out');
		return remaining;
	}

	private async waitFor(task: Promise<void>, deadline: number): Promise<void> {
		let timer: NodeJS.Timeout | undefined;
		const timeout = new Promise<never>((_, reject) => {
			timer = setTimeout(() => reject(new Error('Snowflake generation timed out')), this.remainingTime(deadline));
			timer.unref();
		});
		try {
			await Promise.race([task, timeout]);
		} finally {
			clearTimeout(timer);
		}
	}

	private async ensureInitialized(deadline: number): Promise<void> {
		this.assertActive();
		if (this.phase !== 'ready') await this.waitFor(this.initialize(), deadline);
		this.assertActive();
		assert(this.phase === 'ready', 'Snowflake initialization completed without becoming ready');
	}

	private async refillBuffer(): Promise<void> {
		this.assertActive();
		if (this.refillPromise) {
			await this.refillPromise;
			return;
		}
		this.refillPromise = (async () => {
			await this.ensureConnected();
			const ids = await this.requestBatch(this.batchSize);
			this.assertActive();
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
		this.assertActive();
		assert(Number.isInteger(count) && count > 0 && count <= MAX_REMOTE_BATCH_SIZE, 'Invalid snowflake batch size');
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
		this.assertActive();
		if (responseMessage.data.byteLength > MAX_REMOTE_RESPONSE_BYTES) {
			throw new Error('Snowflake service response exceeds the byte limit');
		}
		const response = parseJsonWithGuard(this.codec.decode(responseMessage.data), isRemoteSnowflakeResponse);
		if (!response) {
			throw new Error('Snowflake service returned an invalid response');
		}
		if (response.error !== undefined) {
			throw new Error(`Snowflake service error: ${response.error}`);
		}
		if (!response.ids || response.ids.length !== count) {
			throw new Error('Snowflake service returned an incorrect batch size');
		}
		let previous = -1n;
		return response.ids.map((value) => {
			const id = parseRemoteSnowflakeId(value);
			if (id <= previous) throw new Error('Snowflake service returned duplicate or unordered IDs');
			previous = id;
			return id;
		});
	}

	private async ensureConnected(): Promise<void> {
		this.assertActive();
		if (this.connectionManager.isClosed()) {
			await this.connectionManager.connect();
		}
		this.assertActive();
	}

	private clearBuffer(): void {
		this.buffer = [];
		this.bufferOffset = 0;
		this.bufferFetchedAtMs = null;
	}

	private takeBufferedId(): bigint | null {
		if (this.bufferOffset >= this.buffer.length) {
			this.clearBuffer();
			return null;
		}
		const id = this.buffer[this.bufferOffset];
		this.bufferOffset += 1;
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
		this.clearBuffer();
	}

	private scheduleRefillIfNeeded(): void {
		if (this.phase !== 'ready' || this.availableIds() > this.lowWatermark || this.refillPromise) {
			return;
		}
		void this.refillBuffer().catch((error) => {
			if (error instanceof SnowflakeServiceStoppedError) return;
			Logger.error({error}, 'Failed to refill snowflake buffer');
		});
	}
}
