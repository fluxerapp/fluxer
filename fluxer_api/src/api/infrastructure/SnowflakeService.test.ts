// SPDX-License-Identifier: AGPL-3.0-or-later

import {SnowflakeService} from '@app/api/infrastructure/SnowflakeService';
import {FakeNatsConnectionManager} from '@app/api/test/mocks/FakeNatsConnectionManager';
import {afterEach, describe, expect, it} from 'vitest';

type FakeBatch = Array<string> | {error: string};

function createConnectionManager(batches: Array<FakeBatch>): FakeNatsConnectionManager {
	const pending = [...batches];
	return new FakeNatsConnectionManager(() => {
		const batch = pending.shift() ?? [];
		return JSON.stringify(Array.isArray(batch) ? {ids: batch} : batch);
	});
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
		const manager = createConnectionManager([
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
		const manager = createConnectionManager([['201', '202', '203', '204'], ['301']]);
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
		const manager = createConnectionManager([['201', '202', '203', '204']]);
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
		const manager = createConnectionManager([{error: 'shard_unavailable'}]);
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
		const manager = createConnectionManager([
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
		const manager = createConnectionManager([['301', '302', '303', '304']]);
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
