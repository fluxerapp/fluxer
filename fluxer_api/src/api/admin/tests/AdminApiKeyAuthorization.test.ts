// SPDX-License-Identifier: AGPL-3.0-or-later

import {createAdminApiKey} from '@app/api/admin/tests/AdminTestUtils';
import {createTestAccount, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {beforeEach, describe, test} from 'vitest';

describe('Admin API Key Authorization', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	test('key fails if owner loses admin access', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'admin_api_key:manage', 'user:lookup']);
		const apiKey = await createAdminApiKey(harness, admin, 'Owner Check', ['user:lookup'], null);
		await setUserACLs(harness, admin, ['admin_api_key:manage', 'user:lookup']);
		await createBuilder(harness, apiKey.token)
			.get(`/admin/users/${admin.userId}`)
			.expect(HTTP_STATUS.FORBIDDEN, 'MISSING_PERMISSIONS')
			.execute();
	});
	test('key fails if owner loses required ACL', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'admin_api_key:manage', 'user:lookup']);
		const apiKey = await createAdminApiKey(harness, admin, 'Owner ACL Check', ['user:lookup'], null);
		await setUserACLs(harness, admin, ['admin:authenticate', 'admin_api_key:manage']);
		await createBuilder(harness, apiKey.token)
			.get(`/admin/users/${admin.userId}`)
			.expect(HTTP_STATUS.FORBIDDEN, 'MISSING_ACL')
			.execute();
	});
	test('key cannot access endpoints without required ACLs', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'admin_api_key:manage', 'audit_log:view', 'user:lookup']);
		const apiKey = await createAdminApiKey(harness, admin, 'Limited Key', ['audit_log:view'], null);
		await createBuilder(harness, apiKey.token)
			.get(`/admin/users/${admin.userId}`)
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});
});
