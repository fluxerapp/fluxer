// SPDX-License-Identifier: AGPL-3.0-or-later

import {afterEach, beforeEach, describe, expect, test} from 'vitest';
import {createTestAccount, setUserACLs} from '../../auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';
import {
	createAdminApiKey,
	createAdminApiKeyWithDefaultACLs,
	listAdminApiKeys,
	revokeAdminApiKey,
} from './AdminTestUtils';

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
	test('create API key with multiple ACLs', async () => {
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
				name: 'Multi-ACL API Key',
				acls: ['audit_log:view', 'user:lookup', 'guild:lookup'],
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(data.acls).toHaveLength(3);
	});
	test('name validation - empty name rejected', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'admin_api_key:manage', 'audit_log:view']);
		await createBuilder(harness, `${admin.token}`)
			.post('/admin/api-keys')
			.body({
				name: '',
				acls: ['audit_log:view'],
			})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.executeWithResponse();
	});
	test('name validation - spaces only rejected', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'admin_api_key:manage', 'audit_log:view']);
		await createBuilder(harness, `${admin.token}`)
			.post('/admin/api-keys')
			.body({
				name: '   ',
				acls: ['audit_log:view'],
			})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.executeWithResponse();
	});
	test('name validation - valid name accepted', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'admin_api_key:manage', 'audit_log:view']);
		await createBuilder(harness, `${admin.token}`)
			.post('/admin/api-keys')
			.body({
				name: 'My API Key',
				acls: ['audit_log:view'],
			})
			.expect(HTTP_STATUS.OK)
			.executeWithResponse();
	});
	test('name validation - special characters accepted', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'admin_api_key:manage', 'audit_log:view']);
		await createBuilder(harness, `${admin.token}`)
			.post('/admin/api-keys')
			.body({
				name: 'Key-123_Test!@#',
				acls: ['audit_log:view'],
			})
			.expect(HTTP_STATUS.OK)
			.executeWithResponse();
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
	test('expiration validation - negative days rejected', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'admin_api_key:manage', 'audit_log:view']);
		await createBuilder(harness, `${admin.token}`)
			.post('/admin/api-keys')
			.body({
				name: 'Test Key',
				expires_in_days: -1,
				acls: ['audit_log:view'],
			})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.executeWithResponse();
	});
	test('expiration validation - too many days rejected', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'admin_api_key:manage', 'audit_log:view']);
		await createBuilder(harness, `${admin.token}`)
			.post('/admin/api-keys')
			.body({
				name: 'Test Key',
				expires_in_days: 366,
				acls: ['audit_log:view'],
			})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.executeWithResponse();
	});
	test('expiration validation - valid minimum accepted', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'admin_api_key:manage', 'audit_log:view']);
		await createBuilder(harness, `${admin.token}`)
			.post('/admin/api-keys')
			.body({
				name: 'Test Key',
				expires_in_days: 1,
				acls: ['audit_log:view'],
			})
			.expect(HTTP_STATUS.OK)
			.executeWithResponse();
	});
	test('expiration validation - valid maximum accepted', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'admin_api_key:manage', 'audit_log:view']);
		await createBuilder(harness, `${admin.token}`)
			.post('/admin/api-keys')
			.body({
				name: 'Test Key',
				expires_in_days: 365,
				acls: ['audit_log:view'],
			})
			.expect(HTTP_STATUS.OK)
			.executeWithResponse();
	});
	test('list empty API keys', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'admin_api_key:manage', 'audit_log:view']);
		const keys = await listAdminApiKeys(harness, admin.token);
		expect(keys).toHaveLength(0);
	});
	test('list multiple API keys', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, [
			'admin:authenticate',
			'admin_api_key:manage',
			'audit_log:view',
			'user:lookup',
			'guild:lookup',
		]);
		await createAdminApiKeyWithDefaultACLs(harness, admin, 'First Key');
		await createAdminApiKey(harness, admin, 'Second Key', ['user:lookup'], null);
		const keys = await listAdminApiKeys(harness, admin.token);
		expect(keys).toHaveLength(2);
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
	test('revoke API key', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, [
			'admin:authenticate',
			'admin_api_key:manage',
			'audit_log:view',
			'user:lookup',
			'guild:lookup',
		]);
		const apiKey = await createAdminApiKeyWithDefaultACLs(harness, admin, 'Key to Revoke');
		let keys = await listAdminApiKeys(harness, admin.token);
		expect(keys).toHaveLength(1);
		await revokeAdminApiKey(harness, admin.token, apiKey.keyId);
		keys = await listAdminApiKeys(harness, admin.token);
		expect(keys).toHaveLength(0);
	});
	test('revoke non-existent key', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'admin_api_key:manage']);
		await createBuilder(harness, `${admin.token}`)
			.delete('/admin/api-keys/999999999999999999')
			.body(null)
			.expect(HTTP_STATUS.NOT_FOUND)
			.executeWithResponse();
	});
	test('revoked key cannot be used', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'admin_api_key:manage', 'user:lookup']);
		const apiKey = await createAdminApiKey(harness, admin, 'Key to Test', ['user:lookup'], null);
		await createBuilder(harness, apiKey.token)
			.get(`/admin/users/${admin.userId}`)
			.expect(HTTP_STATUS.OK)
			.executeWithResponse();
		await revokeAdminApiKey(harness, admin.token, apiKey.keyId);
		await createBuilder(harness, apiKey.token)
			.get(`/admin/users/${admin.userId}`)
			.expect(HTTP_STATUS.UNAUTHORIZED)
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
