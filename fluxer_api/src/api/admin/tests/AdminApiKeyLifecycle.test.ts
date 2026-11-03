// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createAdminApiKey,
	createAdminApiKeyWithDefaultACLs,
	listAdminApiKeys,
} from '@app/api/admin/tests/AdminTestUtils';
import {createTestAccount, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

describe('Admin API Key Lifecycle', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	test('list does not include secret key', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, [
			'admin:authenticate',
			'admin_api_key:manage',
			'audit_log:view',
			'user:lookup',
			'guild:lookup',
		]);
		await createAdminApiKeyWithDefaultACLs(harness, admin, 'Test Key');
		const keys = await listAdminApiKeys(harness, admin.token);
		expect(keys).toHaveLength(1);
		expect('key' in keys[0]!).toBe(false);
	});
	test('update API key cannot grant acls the admin does not hold', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'admin_api_key:manage', 'audit_log:view']);
		const apiKey = await createAdminApiKey(harness, admin, 'Escalation Key', ['audit_log:view'], null);
		await createBuilder(harness, `${admin.token}`)
			.patch(`/admin/api-keys/${apiKey.keyId}`)
			.body({acls: ['guild:delete']})
			.expect(HTTP_STATUS.FORBIDDEN)
			.executeWithResponse();
	});
	test('only keys created by user are listed', async () => {
		const admin1 = await createTestAccount(harness);
		const admin2 = await createTestAccount(harness);
		await setUserACLs(harness, admin1, ['admin:authenticate', 'admin_api_key:manage', 'audit_log:view']);
		await setUserACLs(harness, admin2, ['admin:authenticate', 'admin_api_key:manage', 'audit_log:view']);
		await createAdminApiKey(harness, admin1, 'Admin 1 Key', ['audit_log:view'], null);
		await createAdminApiKey(harness, admin2, 'Admin 2 Key', ['audit_log:view'], null);
		const keys1 = await listAdminApiKeys(harness, admin1.token);
		expect(keys1).toHaveLength(1);
		expect(keys1[0]!.name).toBe('Admin 1 Key');
		const keys2 = await listAdminApiKeys(harness, admin2.token);
		expect(keys2).toHaveLength(1);
		expect(keys2[0]!.name).toBe('Admin 2 Key');
	});
});
