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

interface ApiKeyResponse {
	key_id: string;
	key: string;
	name: string;
	acls: Array<string>;
	created_at: string;
	expires_at: string | null;
}

describe('Admin API Key Lifecycle', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	test('create basic API key', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, [
			'admin:authenticate',
			'admin_api_key:manage',
			'audit_log:view',
			'user:lookup',
			'guild:lookup',
		]);
		const data = await createBuilder<ApiKeyResponse>(harness, `${admin.token}`)
			.post('/admin/api-keys')
			.body({
				name: 'Test API Key',
				acls: ['audit_log:view'],
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(data.key_id).toBeTruthy();
		expect(data.key).toBeTruthy();
		expect(data.key).toMatch(/^fa_/);
		expect(data.name).toBe('Test API Key');
		expect(data.acls).toEqual(['audit_log:view']);
		expect(data.created_at).toBeTruthy();
		expect(data.expires_at).toBeNull();
	});
	test('create API key with expiration', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'admin_api_key:manage', 'audit_log:view']);
		const data = await createBuilder<ApiKeyResponse>(harness, `${admin.token}`)
			.post('/admin/api-keys')
			.body({
				name: 'Expiring API Key',
				expires_in_days: 30,
				acls: ['audit_log:view'],
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(data.expires_at).not.toBeNull();
		expect(data.expires_at).toBeTruthy();
	});
	test('expiration validation - zero days rejected', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'admin_api_key:manage', 'audit_log:view']);
		await createBuilder(harness, `${admin.token}`)
			.post('/admin/api-keys')
			.body({
				name: 'Test Key',
				expires_in_days: 0,
				acls: ['audit_log:view'],
			})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.executeWithResponse();
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
	test('get API key by id', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'admin_api_key:manage', 'audit_log:view']);
		const apiKey = await createAdminApiKey(harness, admin, 'Readable Key', ['audit_log:view'], null);
		const key = await createBuilder<Record<string, unknown>>(harness, `${admin.token}`)
			.get(`/admin/api-keys/${apiKey.keyId}`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(key.key_id).toBe(apiKey.keyId);
		expect(key.name).toBe('Readable Key');
		expect(key.acls).toEqual(['audit_log:view']);
		expect('key' in key).toBe(false);
	});
	test('get non-existent API key', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'admin_api_key:manage']);
		await createBuilder(harness, `${admin.token}`)
			.get('/admin/api-keys/999999999999999999')
			.expect(HTTP_STATUS.NOT_FOUND)
			.executeWithResponse();
	});
	test('update API key name and acls', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'admin_api_key:manage', 'audit_log:view', 'guild:lookup']);
		const apiKey = await createAdminApiKey(harness, admin, 'Old Name', ['audit_log:view'], null);
		const updated = await createBuilder<Record<string, unknown>>(harness, `${admin.token}`)
			.patch(`/admin/api-keys/${apiKey.keyId}`)
			.body({name: 'New Name', acls: ['guild:lookup']})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(updated.name).toBe('New Name');
		expect(updated.acls).toEqual(['guild:lookup']);
		const keys = await listAdminApiKeys(harness, admin.token);
		expect(keys[0]!.name).toBe('New Name');
		expect(keys[0]!.acls).toEqual(['guild:lookup']);
	});
	test('update API key leaves omitted fields unchanged', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'admin_api_key:manage', 'audit_log:view']);
		const apiKey = await createAdminApiKey(harness, admin, 'Stable Name', ['audit_log:view'], null);
		const updated = await createBuilder<Record<string, unknown>>(harness, `${admin.token}`)
			.patch(`/admin/api-keys/${apiKey.keyId}`)
			.body({name: 'Renamed'})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(updated.name).toBe('Renamed');
		expect(updated.acls).toEqual(['audit_log:view']);
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
