// SPDX-License-Identifier: AGPL-3.0-or-later

import {createAdminApiKeyWithDefaultACLs, listAdminApiKeys} from '@app/api/admin/tests/AdminTestUtils';
import {createTestAccount, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {beforeEach, describe, expect, test} from 'vitest';

async function hasAdminAPIKeyId(harness: ApiTestHarness, token: string, keyId: string): Promise<boolean> {
	const keys = await listAdminApiKeys(harness, token);
	return keys.some((k) => k.key_id === keyId);
}

describe('Admin API Key Revocation', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	test('revocation requires admin_api_key:manage ACL', async () => {
		const admin1 = await createTestAccount(harness);
		const admin2 = await createTestAccount(harness);
		await setUserACLs(harness, admin1, [
			'admin:authenticate',
			'admin_api_key:manage',
			'audit_log:view',
			'user:lookup',
			'guild:lookup',
		]);
		await setUserACLs(harness, admin2, ['admin:authenticate']);
		const apiKey = await createAdminApiKeyWithDefaultACLs(harness, admin1, 'Admin1 Key');
		await createBuilder(harness, `${admin2.token}`)
			.delete(`/admin/api-keys/${apiKey.keyId}`)
			.body(null)
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
		expect(await hasAdminAPIKeyId(harness, admin1.token, apiKey.keyId)).toBe(true);
	});
	test('cannot revoke other users keys', async () => {
		const admin1 = await createTestAccount(harness);
		const admin2 = await createTestAccount(harness);
		await setUserACLs(harness, admin1, [
			'admin:authenticate',
			'admin_api_key:manage',
			'audit_log:view',
			'user:lookup',
			'guild:lookup',
		]);
		await setUserACLs(harness, admin2, ['admin:authenticate', 'admin_api_key:manage', 'audit_log:view']);
		const apiKey = await createAdminApiKeyWithDefaultACLs(harness, admin1, 'Admin1 Key');
		await createBuilder(harness, `${admin2.token}`)
			.delete(`/admin/api-keys/${apiKey.keyId}`)
			.body(null)
			.expect(HTTP_STATUS.NOT_FOUND)
			.execute();
		expect(await hasAdminAPIKeyId(harness, admin1.token, apiKey.keyId)).toBe(true);
	});
});
