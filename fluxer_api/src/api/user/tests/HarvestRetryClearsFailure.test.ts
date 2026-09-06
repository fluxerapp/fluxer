// SPDX-License-Identifier: AGPL-3.0-or-later

import {beforeEach, describe, expect, test} from 'vitest';
import {createTestAccount} from '../../auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {
	expectHarvestDownloadFailsWithError,
	fetchHarvestDownload,
	findHarvest,
	markHarvestCompleted,
	markHarvestFailed,
	markHarvestStarted,
	requestHarvest,
} from './HarvestTestUtils';

describe('Harvest Retry Clears Failure', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	test('a retry clears the previous failure and a completed retry reports completed', async () => {
		const account = await createTestAccount(harness);
		const {harvest_id} = await requestHarvest(harness, account.token);
		await markHarvestFailed(account.userId, harvest_id, 'harvest exploded');
		const failed = await findHarvest(account.userId, harvest_id);
		expect(failed?.getStatus()).toBe('failed');
		expect(failed?.errorMessage).toBe('harvest exploded');
		await markHarvestStarted(account.userId, harvest_id);
		const retrying = await findHarvest(account.userId, harvest_id);
		expect(retrying?.getStatus()).toBe('processing');
		expect(retrying?.failedAt).toBeNull();
		expect(retrying?.errorMessage).toBeNull();
		const validTime = new Date(Date.now() + 6 * 24 * 60 * 60 * 1000);
		await markHarvestCompleted(account.userId, harvest_id, validTime);
		const completed = await findHarvest(account.userId, harvest_id);
		expect(completed?.getStatus()).toBe('completed');
		expect(completed?.failedAt).toBeNull();
		expect(completed?.errorMessage).toBeNull();
		const download = await fetchHarvestDownload(harness, account.token, harvest_id);
		expect(download.download_url).not.toBe('');
	});
	test('download reports the failure rather than unreadiness when the latest attempt failed', async () => {
		const account = await createTestAccount(harness);
		const {harvest_id} = await requestHarvest(harness, account.token);
		await markHarvestFailed(account.userId, harvest_id, 'harvest exploded');
		await expectHarvestDownloadFailsWithError(harness, account.token, harvest_id, 'HARVEST_FAILED');
	});
});
