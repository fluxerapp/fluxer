// SPDX-License-Identifier: AGPL-3.0-or-later

import {beforeEach, describe, expect, test} from 'vitest';
import type {TestAccount} from '../../auth/tests/AuthTestUtils';
import {createTestAccount, setUserACLs} from '../../auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';

interface AdminApiKey {
	keyId: string;
	key: string;
	name: string;
	acls: Array<string>;
	token: string;
}

async function createAdminApiKey(
	harness: ApiTestHarness,
	account: TestAccount,
	name: string,
	acls: Array<string>,
	expiresInDays: number | null,
): Promise<AdminApiKey> {
	const data = await createBuilder<{
		key_id: string;
		key: string;
		name: string;
		acls: Array<string>;
	}>(harness, `${account.token}`)
		.post('/admin/api-keys')
		.body({
			name,
			acls,
			...(expiresInDays !== null ? {expires_in_days: expiresInDays} : {}),
		})
		.expect(HTTP_STATUS.OK)
		.execute();
	return {
		keyId: data.key_id,
		key: data.key,
		name: data.name,
		acls: data.acls,
		token: `Admin ${data.key}`,
	};
}

async function createAdminApiKeyWithDefaultACLs(
	harness: ApiTestHarness,
	account: TestAccount,
	name: string,
): Promise<AdminApiKey> {
	return await createAdminApiKey(harness, account, name, ['audit_log:view', 'user:lookup', 'guild:lookup'], null);
}

async function listAdminApiKeys(harness: ApiTestHarness, token: string): Promise<Array<Record<string, unknown>>> {
	return await createBuilder<Array<Record<string, unknown>>>(harness, `${token}`)
		.get('/admin/api-keys')
		.expect(HTTP_STATUS.OK)
		.execute();
}

async function revokeAdminApiKey(harness: ApiTestHarness, token: string, keyId: string): Promise<void> {
	await createBuilder(harness, `${token}`)
		.delete(`/admin/api-keys/${keyId}`)
		.body(null)
		.expect(HTTP_STATUS.OK)
		.execute();
}

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
	test('updates last_used_at timestamp', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, [
			'admin:authenticate',
			'admin_api_key:manage',
			'audit_log:view',
			'user:lookup',
			'guild:lookup',
		]);
		const apiKey = await createAdminApiKeyWithDefaultACLs(harness, admin, 'Last Used Test Key');
		const keys = await listAdminApiKeys(harness, admin.token);
		expect(keys).toHaveLength(1);
		expect(keys[0]!.last_used_at).toBeNull();
		await createBuilder(harness, apiKey.token).get(`/admin/users/${admin.userId}`).execute();
		const keysAfter = await listAdminApiKeys(harness, admin.token);
		expect(keysAfter).toHaveLength(1);
		expect(keysAfter[0]!.last_used_at).not.toBeNull();
	});
	test('multiple keys for same user all work', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'admin_api_key:manage', 'user:lookup']);
		const key1 = await createAdminApiKey(harness, admin, 'Key 1', ['admin:authenticate', 'user:lookup'], null);
		const key2 = await createAdminApiKey(harness, admin, 'Key 2', ['admin:authenticate', 'user:lookup'], null);
		const key3 = await createAdminApiKey(harness, admin, 'Key 3', ['admin:authenticate', 'user:lookup'], null);
		for (const key of [key1, key2, key3]) {
			await createBuilder(harness, key.token).get(`/admin/users/${admin.userId}`).expect(HTTP_STATUS.OK).execute();
		}
		const keys = await listAdminApiKeys(harness, admin.token);
		expect(keys).toHaveLength(3);
	});
	test('different users can use same key if creator has permissions', async () => {
		const admin1 = await createTestAccount(harness);
		const admin2 = await createTestAccount(harness);
		await setUserACLs(harness, admin1, ['admin:authenticate', 'admin_api_key:manage', 'user:lookup']);
		await setUserACLs(harness, admin2, ['admin:authenticate', 'user:lookup']);
		const key1 = await createAdminApiKey(harness, admin1, 'Admin 1 Key', ['admin:authenticate', 'user:lookup'], null);
		await createBuilder(harness, key1.token).get(`/admin/users/${admin2.userId}`).expect(HTTP_STATUS.OK).execute();
	});
});
