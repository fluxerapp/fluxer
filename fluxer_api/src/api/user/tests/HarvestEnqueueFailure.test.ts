// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {resetApiServicesForTesting} from '@app/api/CreateApiContext';
import {setInjectedWorkerService} from '@app/api/middleware/ServiceRegistry';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopWorkerService} from '@app/api/test/NoopWorkerService';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {UserHarvestRepository} from '@app/api/user/UserHarvestRepository';
import type {HarvestStatusResponse} from '@fluxer/schema/src/domains/user/UserHarvestSchemas';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

class UnavailableWorkerService extends NoopWorkerService {
	override async addJob(): Promise<bigint> {
		throw new Error('jobs stream unavailable');
	}
}

describe('Harvest request when the job cannot be queued', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
		setInjectedWorkerService(new UnavailableWorkerService());
		resetApiServicesForTesting();
	});
	afterEach(() => {
		setInjectedWorkerService(new NoopWorkerService());
		resetApiServicesForTesting();
	});

	test('leaves a terminal failed harvest instead of one that stays pending', async () => {
		const account = await createTestAccount(harness);
		await createBuilder(harness, account.token).post('/users/@me/harvest').expect(500).execute();

		const latest = await createBuilder<HarvestStatusResponse | null>(harness, account.token)
			.get('/users/@me/harvest/latest')
			.execute();
		expect(latest?.status).toBe('failed');

		const stored = await new UserHarvestRepository().findLatestByUserId(createUserID(BigInt(account.userId)));
		expect(stored?.terminalFailedAt).not.toBeNull();
		expect(stored?.attemptId ?? null).toBeNull();
	});
});
