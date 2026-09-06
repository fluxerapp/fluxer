// SPDX-License-Identifier: AGPL-3.0-or-later

import {beforeEach, describe, expect, test} from 'vitest';
import {createTestAccount, setUserACLs} from '../../auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';
import {createAdminApiKeyWithDefaultACLs, listAdminApiKeys} from './AdminTestUtils';

async function hasAdminAPIKeyId(harness: ApiTestHarness, token: string, keyId: string): Promise<boolean> {
	const keys = await listAdminApiKeys(harness, token);
	return keys.some((k) => k.key_id === keyId);
}

describe('Admin API Key Revocation', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	test('basic revocation', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, [
			'admin:authenticate',
			'admin_api_key:manage',
			'audit_log:view',
			'user:lookup',
			'guild:lookup',
		]);
		const apiKey = await createAdminApiKeyWithDefaultACLs(harness, admin, 'Revoke Test');
		expect(await hasAdminAPIKeyId(harness, admin.token, apiKey.keyId)).toBe(true);
		await createBuilder(harness, `${admin.token}`)
			.delete(`/admin/api-keys/${apiKey.keyId}`)
			.body(null)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(await hasAdminAPIKeyId(harness, admin.token, apiKey.keyId)).toBe(false);
	});
	test('revocation by ID', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, [
			'admin:authenticate',
			'admin_api_key:manage',
			'audit_log:view',
			'user:lookup',
			'guild:lookup',
		]);
		const apiKey = await createAdminApiKeyWithDefaultACLs(harness, admin, 'ID Test');
		await createBuilder(harness, `${admin.token}`)
			.delete(`/admin/api-keys/${apiKey.keyId}`)
			.body(null)
			.expect(HTTP_STATUS.OK)
			.execute();
		const keys = await listAdminApiKeys(harness, admin.token);
		expect(keys).toHaveLength(0);
	});
	test('revocation of non-existent key returns 404', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, [
			'admin:authenticate',
			'admin_api_key:manage',
			'audit_log:view',
			'user:lookup',
			'guild:lookup',
		]);
		await createBuilder(harness, `${admin.token}`)
			.delete('/admin/api-keys/999999999999999999')
			.body(null)
			.expect(HTTP_STATUS.NOT_FOUND)
			.execute();
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
