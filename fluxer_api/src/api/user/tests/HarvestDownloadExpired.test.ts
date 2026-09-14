// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {
	expectHarvestDownloadFailsWithError,
	markHarvestCompleted,
	requestHarvest,
} from '@app/api/user/tests/HarvestTestUtils';
import {beforeEach, describe, test} from 'vitest';

describe('Harvest Download Expired', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	test('download fails when harvest has expired', async () => {
		const account = await createTestAccount(harness);
		const {harvest_id} = await requestHarvest(harness, account.token);
		const expiredTime = new Date(Date.now() - 60 * 60 * 1000);
		await markHarvestCompleted(account.userId, harvest_id, expiredTime);
		await expectHarvestDownloadFailsWithError(harness, account.token, harvest_id, 'HARVEST_EXPIRED');
	});
});
