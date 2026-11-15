// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {afterEach, beforeEach, describe, test} from 'vitest';

describe('Admin Search Endpoints', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness({search: 'enabled'});
	});
	afterEach(async () => {
		await harness.shutdown();
	});
	describe('GET /admin/users/{user_id}/dm-channels', () => {
		test('requires user:list:dm_channels ACL', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate']);
			await createBuilder(harness, `${admin.token}`)
				.get(`/admin/users/${admin.userId}/dm-channels?limit=10`)
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
	});
	describe('/admin/audit-logs/{log_id}', () => {
		test('requires audit_log:view ACL', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate']);
			await createBuilder(harness, `${admin.token}`)
				.get('/admin/audit-logs/999999999999999999')
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
	});
});
