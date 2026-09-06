// SPDX-License-Identifier: AGPL-3.0-or-later

import {beforeEach, describe, expect, test} from 'vitest';
import {createTestAccount, setUserACLs} from '../../auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';
import {createAdminApiKey} from './AdminTestUtils';

describe('Admin API Key Authorization', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	test('key can access endpoints with granted ACLs', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'admin_api_key:manage', 'audit_log:view', 'user:lookup']);
		const apiKey = await createAdminApiKey(harness, admin, 'Test Key', ['audit_log:view', 'user:lookup'], null);
		await createBuilder(harness, apiKey.token).get('/admin/audit-logs?limit=10').expect(HTTP_STATUS.OK).execute();
		await createBuilder(harness, apiKey.token).get(`/admin/users/${admin.userId}`).expect(HTTP_STATUS.OK).execute();
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
	test('key with wildcard can access all endpoints', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['*']);
		const apiKey = await createAdminApiKey(harness, admin, 'Wildcard Key', ['*'], null);
		await createBuilder(harness, apiKey.token).get(`/admin/users/${admin.userId}`).expect(HTTP_STATUS.OK).execute();
	});
	test('multiple keys with different ACLs work independently', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, [
			'admin:authenticate',
			'admin_api_key:manage',
			'audit_log:view',
			'user:lookup',
			'guild:lookup',
		]);
		const key1 = await createAdminApiKey(harness, admin, 'Audit Log Key', ['audit_log:view'], null);
		const key2 = await createAdminApiKey(harness, admin, 'Users Key', ['user:lookup'], null);
		const key3 = await createAdminApiKey(harness, admin, 'Guilds Key', ['guild:lookup'], null);
		await createBuilder(harness, key1.token).get('/admin/audit-logs?limit=10').expect(HTTP_STATUS.OK).execute();
		await createBuilder(harness, key2.token).get(`/admin/users/${admin.userId}`).expect(HTTP_STATUS.OK).execute();
		const guildJson = await createBuilder<{
			guild: unknown;
		}>(harness, key3.token)
			.get('/admin/guilds/123')
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(guildJson.guild).toBeNull();
		await createBuilder(harness, key1.token)
			.get(`/admin/users/${admin.userId}`)
			.expect(HTTP_STATUS.FORBIDDEN, 'MISSING_ACL')
			.execute();
	});
	test('GET /admin/api-keys requires admin_api_key:manage', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'admin_api_key:manage']);
		const apiKey = await createAdminApiKey(harness, admin, 'List Test', ['admin_api_key:manage'], null);
		await createBuilder(harness, apiKey.token).get('/admin/api-keys').expect(HTTP_STATUS.OK).execute();
	});
	test('GET /admin/api-keys fails without admin_api_key:manage', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'admin_api_key:manage']);
		const apiKey = await createAdminApiKey(harness, admin, 'List Test No ACL', [], null);
		await createBuilder(harness, apiKey.token).get('/admin/api-keys').expect(HTTP_STATUS.FORBIDDEN).execute();
	});
	test('DELETE /admin/api-keys/:key_id requires admin_api_key:manage', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'admin_api_key:manage']);
		const keyToDelete = await createAdminApiKey(harness, admin, 'To Delete', [], null);
		const deleterKey = await createAdminApiKey(harness, admin, 'Deleter', ['admin_api_key:manage'], null);
		await createBuilder(harness, deleterKey.token)
			.delete(`/admin/api-keys/${keyToDelete.keyId}`)
			.body(null)
			.expect(HTTP_STATUS.OK)
			.execute();
	});
	test('DELETE /admin/api-keys/:key_id fails without admin_api_key:manage', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'admin_api_key:manage']);
		const keyToDelete = await createAdminApiKey(harness, admin, 'To Delete 2', [], null);
		const deleterKey = await createAdminApiKey(harness, admin, 'Deleter No ACL', [], null);
		await createBuilder(harness, deleterKey.token)
			.delete(`/admin/api-keys/${keyToDelete.keyId}`)
			.body(null)
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});
});
