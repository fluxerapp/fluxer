// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS, TEST_IDS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {afterAll, beforeAll, beforeEach, describe, it} from 'vitest';

describe('Guild Operation Validation', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	it('should reject getting nonexistent guild', async () => {
		const account = await createTestAccount(harness);
		await createBuilder(harness, account.token)
			.get(`/guilds/${TEST_IDS.NONEXISTENT_GUILD}`)
			.expect(HTTP_STATUS.NOT_FOUND)
			.execute();
	});
	it('should reject updating nonexistent guild', async () => {
		const account = await createTestAccount(harness);
		await createBuilder(harness, account.token)
			.patch(`/guilds/${TEST_IDS.NONEXISTENT_GUILD}`)
			.body({name: 'New Name'})
			.expect(HTTP_STATUS.NOT_FOUND)
			.execute();
	});
	it('should reject leaving nonexistent guild', async () => {
		const account = await createTestAccount(harness);
		await createBuilder(harness, account.token)
			.delete(`/users/@me/guilds/${TEST_IDS.NONEXISTENT_GUILD}`)
			.expect(HTTP_STATUS.NOT_FOUND)
			.execute();
	});
});
