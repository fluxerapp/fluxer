// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {afterEach, beforeEach, describe, test} from 'vitest';
import {createTestAccount, setUserACLs, type TestAccount} from '../../auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';

async function createAdminWithAcls(harness: ApiTestHarness, acls: Array<string>): Promise<TestAccount> {
	const account = await createTestAccount(harness);
	return await setUserACLs(harness, account, [AdminACLs.AUTHENTICATE, ...acls]);
}

describe('GatewayAdminController', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	test('rejects a guild memory statistics limit below the minimum', async () => {
		const admin = await createAdminWithAcls(harness, [AdminACLs.GATEWAY_MEMORY_STATS]);
		await createBuilder(harness, `${admin.token}`)
			.get('/admin/gateway/memory-stats?limit=50')
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
	test('requires the gateway memory stats acl for guild memory statistics', async () => {
		const admin = await createAdminWithAcls(harness, []);
		await createBuilder(harness, `${admin.token}`)
			.get('/admin/gateway/memory-stats')
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});
	test('requires the gateway reload acl for reloads', async () => {
		const admin = await createAdminWithAcls(harness, [AdminACLs.GATEWAY_MEMORY_STATS]);
		await createBuilder(harness, `${admin.token}`)
			.post('/admin/gateway/reloads')
			.body({guild_ids: []})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});
});
