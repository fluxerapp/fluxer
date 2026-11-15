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
});
