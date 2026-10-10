// SPDX-License-Identifier: AGPL-3.0-or-later

import {createAdminApiKeyWithDefaultACLs, revokeAdminApiKey} from '@app/api/admin/tests/AdminTestUtils';
import {createTestAccount, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {beforeEach, describe, test} from 'vitest';

describe('Admin API Key Authentication', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	test('valid key authenticates successfully', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, [
			'admin:authenticate',
			'admin_api_key:manage',
			'audit_log:view',
			'user:lookup',
			'guild:lookup',
		]);
		const apiKey = await createAdminApiKeyWithDefaultACLs(harness, admin, 'Auth Test Key');
		await createBuilder(harness, apiKey.token).get(`/admin/users/${admin.userId}`).expect(HTTP_STATUS.OK).execute();
	});
	test('invalid key is rejected', async () => {
		await createTestAccount(harness);
		await createBuilder(harness, 'Admin invalid_key_12345')
			.get('/admin/users/123456789')
			.expect(HTTP_STATUS.UNAUTHORIZED)
			.execute();
	});
	test('revoked key is rejected', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, [
			'admin:authenticate',
			'admin_api_key:manage',
			'audit_log:view',
			'user:lookup',
			'guild:lookup',
		]);
		const apiKey = await createAdminApiKeyWithDefaultACLs(harness, admin, 'Revoke Test Key');
		await revokeAdminApiKey(harness, admin.token, apiKey.keyId);
		await createBuilder(harness, apiKey.token)
			.get(`/admin/users/${admin.userId}`)
			.expect(HTTP_STATUS.UNAUTHORIZED)
			.execute();
	});
	test('wrong prefix is rejected', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, [
			'admin:authenticate',
			'admin_api_key:manage',
			'audit_log:view',
			'user:lookup',
			'guild:lookup',
		]);
		const apiKey = await createAdminApiKeyWithDefaultACLs(harness, admin, 'Prefix Test Key');
		await createBuilder(harness, `Bearer ${apiKey.key}`)
			.get(`/admin/users/${admin.userId}`)
			.expect(HTTP_STATUS.UNAUTHORIZED)
			.execute();
	});
	test('missing prefix is rejected', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, [
			'admin:authenticate',
			'admin_api_key:manage',
			'audit_log:view',
			'user:lookup',
			'guild:lookup',
		]);
		const apiKey = await createAdminApiKeyWithDefaultACLs(harness, admin, 'No Prefix Test Key');
		await createBuilder(harness, apiKey.key)
			.get(`/admin/users/${admin.userId}`)
			.expect(HTTP_STATUS.UNAUTHORIZED)
			.execute();
	});
	test('case sensitive prefix', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, [
			'admin:authenticate',
			'admin_api_key:manage',
			'audit_log:view',
			'user:lookup',
			'guild:lookup',
		]);
		const apiKey = await createAdminApiKeyWithDefaultACLs(harness, admin, 'Case Test Key');
		await createBuilder(harness, `admin ${apiKey.key}`)
			.get(`/admin/users/${admin.userId}`)
			.expect(HTTP_STATUS.UNAUTHORIZED)
			.execute();
	});
	test('empty key is rejected', async () => {
		await createTestAccount(harness);
		await createBuilder(harness, 'Admin ').get('/admin/users/123456789').expect(HTTP_STATUS.UNAUTHORIZED).execute();
	});
	test('cannot authenticate to user endpoints', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, [
			'admin:authenticate',
			'admin_api_key:manage',
			'audit_log:view',
			'user:lookup',
			'guild:lookup',
		]);
		const apiKey = await createAdminApiKeyWithDefaultACLs(harness, admin, 'User Endpoint Test Key');
		await createBuilder(harness, apiKey.token).get('/users/@me').expect(HTTP_STATUS.UNAUTHORIZED).execute();
	});
	test('cannot authenticate to bot endpoints', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, [
			'admin:authenticate',
			'admin_api_key:manage',
			'audit_log:view',
			'user:lookup',
			'guild:lookup',
		]);
		const apiKey = await createAdminApiKeyWithDefaultACLs(harness, admin, 'Bot Endpoint Test Key');
		await createBuilder(harness, `Bot ${apiKey.key}`).get('/users/@me').expect(HTTP_STATUS.UNAUTHORIZED).execute();
	});
});
