// SPDX-License-Identifier: AGPL-3.0-or-later

import type {INatsConnectionManager} from '@pkgs/nats/src/INatsConnectionManager';
import {type NatsConnection, StringCodec} from 'nats';
import {afterEach, describe, expect, it} from 'vitest';
import {SnowflakeService} from './SnowflakeService';

interface FakeRequest {
	subject: string;
	body: {
		op: string;
		count: number;
		routing_key?: string;
	};
	timeout: number | undefined;
}

type FakeBatch = Array<string> | {error: string};

class FakeNatsConnectionManager implements INatsConnectionManager {
	private readonly codec = StringCodec();
	private closed = true;
	private readonly batches: Array<FakeBatch>;
	readonly requests: Array<FakeRequest> = [];
	drained = false;

	constructor(batches: Array<FakeBatch>) {
		this.batches = [...batches];
	}

	async connect(): Promise<void> {
		this.closed = false;
	}

	getConnection(): NatsConnection {
		if (this.closed) {
			throw new Error('not connected');
		}
		return {
			request: async (subject: string, data: Uint8Array, options?: {timeout?: number}) => {
				const body = JSON.parse(this.codec.decode(data)) as FakeRequest['body'];
				this.requests.push({subject, body, timeout: options?.timeout});
				const batch = this.batches.shift() ?? [];
				const response = Array.isArray(batch) ? {ids: batch} : batch;
				return {
					data: this.codec.encode(JSON.stringify(response)),
				};
			},
		} as unknown as NatsConnection;
	}

	async drain(): Promise<void> {
		this.drained = true;
		this.closed = true;
	}

	isClosed(): boolean {
		return this.closed;
	}
}

describe('SnowflakeService', () => {
	let services: Array<SnowflakeService> = [];
	afterEach(async () => {
		await Promise.all(services.map((service) => service.shutdown().catch(() => undefined)));
		services = [];
	});
	function track(service: SnowflakeService): SnowflakeService {
		services.push(service);
		return service;
	}
	it('uses buffered remote IDs before requesting another batch', async () => {
		const manager = new FakeNatsConnectionManager([
			['101', '102', '103', '104'],
			['105', '106', '107', '108'],
		]);
		const service = track(
			new SnowflakeService({
				connectionManager: manager,
				batchSize: 4,
				lowWatermark: 0,
				requestTimeoutMs: 1234,
			}),
		);
		await service.initialize();
		expect(manager.requests).toHaveLength(1);
		expect(await service.generate()).toBe(101n);
		expect(await service.generate()).toBe(102n);
		expect(await service.generate()).toBe(103n);
		expect(manager.requests).toHaveLength(1);
		expect(manager.requests[0]).toEqual({
			subject: 'svc.snowflakes',
			body: {
				op: 'GenerateBatch',
				count: 4,
			},
			timeout: 1234,
		});
	});
	it('routes channel-scoped requests without consuming the buffered batch', async () => {
		const manager = new FakeNatsConnectionManager([['201', '202', '203', '204'], ['301']]);
		const service = track(
			new SnowflakeService({
				connectionManager: manager,
				batchSize: 4,
				lowWatermark: 0,
			}),
		);
		await service.initialize();
		expect(await service.generateForChannel('1510189013330296832')).toBe(301n);
		expect(manager.requests).toHaveLength(2);
		expect(manager.requests[1]).toEqual({
			subject: 'svc.snowflakes',
			body: {
				op: 'GenerateBatch',
				count: 1,
				routing_key: 'channel:1510189013330296832',
			},
			timeout: 6000,
		});
		expect(await service.generate()).toBe(201n);
		expect(manager.requests).toHaveLength(2);
	});
	it('coalesces concurrent remote initialization and refill', async () => {
		const manager = new FakeNatsConnectionManager([['201', '202', '203', '204']]);
		const service = track(
			new SnowflakeService({
				connectionManager: manager,
				batchSize: 4,
				lowWatermark: 0,
			}),
		);
		const ids = await Promise.all([service.generate(), service.generate(), service.generate()]);
		expect(ids).toEqual([201n, 202n, 203n]);
		expect(manager.requests).toHaveLength(1);
	});
	it('surfaces remote service errors', async () => {
		const manager = new FakeNatsConnectionManager([{error: 'shard_unavailable'}]);
		const service = track(
			new SnowflakeService({
				connectionManager: manager,
				batchSize: 4,
				lowWatermark: 0,
			}),
		);
		await expect(service.initialize()).rejects.toThrow('Snowflake service error: shard_unavailable');
	});
	it('discards stale buffered remote IDs after idle periods', async () => {
		const manager = new FakeNatsConnectionManager([
			['401', '402', '403', '404'],
			['501', '502', '503', '504'],
		]);
		const service = track(
			new SnowflakeService({
				connectionManager: manager,
				batchSize: 4,
				lowWatermark: 0,
				maxBufferAgeMs: 1,
			}),
		);
		await service.initialize();
		await new Promise((resolve) => setTimeout(resolve, 5));
		expect(await service.generate()).toBe(501n);
		expect(manager.requests).toHaveLength(2);
	});
	it('drains the remote NATS connection on shutdown', async () => {
		const manager = new FakeNatsConnectionManager([['301', '302', '303', '304']]);
		const service = track(
			new SnowflakeService({
				connectionManager: manager,
				batchSize: 4,
			}),
		);
		await service.initialize();
		await service.shutdown();
		expect(manager.drained).toBe(true);
	});
});
