// SPDX-License-Identifier: AGPL-3.0-or-later

import {createAdminWithAcls} from '@app/api/admin/tests/AdminTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {afterEach, beforeEach, describe, test} from 'vitest';

describe('GatewayAdminController', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
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
