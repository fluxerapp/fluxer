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

interface ValidationErrorResponse {
	errors: Array<{
		path: string;
		code: string;
		message: string;
	}>;
}

function expectInvalidKeyIdFormat(json: ValidationErrorResponse): void {
	const keyIdError = json.errors.find((error) => error.path === 'key_id');
	expect(keyIdError).toBeDefined();
	expect(keyIdError?.code).toBe('INVALID_SNOWFLAKE_FORMAT');
}

describe('Admin API Key Management', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	describe('API Key ACL Validation', () => {
		test('user can only grant ACLs they possess', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'admin_api_key:manage', 'audit_log:view']);
			await createBuilder(harness, `${admin.token}`)
				.post('/admin/api-keys')
				.body({
					name: 'Test Key',
					acls: ['audit_log:view'],
				})
				.expect(HTTP_STATUS.OK)
				.execute();
		});
		test('user cannot grant ACLs they do not possess', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'admin_api_key:manage', 'audit_log:view']);
			await createBuilder(harness, `${admin.token}`)
				.post('/admin/api-keys')
				.body({
					name: 'Test Key',
					acls: ['audit_log:view', 'user:lookup'],
				})
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
		test('user with wildcard ACL can grant any ACL', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['*']);
			await createBuilder(harness, `${admin.token}`)
				.post('/admin/api-keys')
				.body({
					name: 'Wildcard Test Key',
					acls: ['audit_log:view', 'user:lookup', 'guild:lookup', 'archive:trigger:user'],
				})
				.expect(HTTP_STATUS.OK)
				.execute();
		});
		test('must have admin_api_key:manage ACL to create keys', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'audit_log:view']);
			await createBuilder(harness, `${admin.token}`)
				.post('/admin/api-keys')
				.body({
					name: 'Test Key',
					acls: ['audit_log:view'],
				})
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
		test('ACLs are stored and retrievable correctly', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, [
				'admin:authenticate',
				'admin_api_key:manage',
				'audit_log:view',
				'user:lookup',
				'guild:lookup',
			]);
			const requestedACLs = ['audit_log:view', 'user:lookup', 'guild:lookup'];
			await createAdminApiKey(harness, admin, 'ACL Storage Test', requestedACLs, null);
			const keys = await listAdminApiKeys(harness, admin.token);
			expect(keys).toHaveLength(1);
			const keyACLs = keys[0]!.acls as Array<string>;
			expect(keyACLs).toHaveLength(requestedACLs.length);
			expect(keyACLs).toEqual(expect.arrayContaining(requestedACLs));
		});
		test('empty ACL list is valid', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'admin_api_key:manage', 'audit_log:view']);
			await createBuilder(harness, `${admin.token}`)
				.post('/admin/api-keys')
				.body({
					name: 'Test Key',
					acls: [],
				})
				.expect(HTTP_STATUS.OK)
				.execute();
		});
	});
	describe('API Key Authentication', () => {
		test('valid API key authenticates successfully', async () => {
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
		test('invalid API key is rejected', async () => {
			await createTestAccount(harness);
			await createBuilder(harness, 'Admin invalid_key_12345')
				.get('/admin/users/123456789')
				.expect(HTTP_STATUS.UNAUTHORIZED)
				.execute();
		});
		test('API key requires Admin prefix', async () => {
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
		test('Admin prefix is case sensitive', async () => {
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
		test('API key cannot authenticate to user endpoints', async () => {
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
		test('updates last_used_at timestamp on use', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, [
				'admin:authenticate',
				'admin_api_key:manage',
				'audit_log:view',
				'user:lookup',
				'guild:lookup',
			]);
			const apiKey = await createAdminApiKeyWithDefaultACLs(harness, admin, 'Last Used Test Key');
			const keysBefore = await listAdminApiKeys(harness, admin.token);
			expect(keysBefore).toHaveLength(1);
			expect(keysBefore[0]!.last_used_at).toBeNull();
			await createBuilder(harness, apiKey.token).get(`/admin/users/${admin.userId}`).execute();
			const keysAfter = await listAdminApiKeys(harness, admin.token);
			expect(keysAfter).toHaveLength(1);
			expect(keysAfter[0]!.last_used_at).not.toBeNull();
		});
	});
	describe('API Key Authorization (ACL Restrictions)', () => {
		test('key can access endpoints with granted ACLs', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, [
				'admin:authenticate',
				'admin_api_key:manage',
				'audit_log:view',
				'user:lookup',
			]);
			const apiKey = await createAdminApiKey(harness, admin, 'Test Key', ['audit_log:view', 'user:lookup'], null);
			await createBuilder(harness, apiKey.token).get(`/admin/users/${admin.userId}`).expect(HTTP_STATUS.OK).execute();
		});
		test('key cannot access endpoints without required ACLs', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, [
				'admin:authenticate',
				'admin_api_key:manage',
				'audit_log:view',
				'user:lookup',
			]);
			const apiKey = await createAdminApiKey(harness, admin, 'Limited Key', ['audit_log:view'], null);
			await createBuilder(harness, apiKey.token)
				.get(`/admin/users/${admin.userId}`)
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
		test('key with wildcard ACL can access all endpoints', async () => {
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
			const auditKey = await createAdminApiKey(harness, admin, 'Audit Log Key', ['audit_log:view'], null);
			const userKey = await createAdminApiKey(harness, admin, 'Users Key', ['user:lookup'], null);
			await createBuilder(harness, userKey.token).get(`/admin/users/${admin.userId}`).expect(HTTP_STATUS.OK).execute();
			await createBuilder(harness, auditKey.token)
				.get(`/admin/users/${admin.userId}`)
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
		test('list API keys requires admin_api_key:manage ACL', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'admin_api_key:manage']);
			const apiKeyWithACL = await createAdminApiKey(harness, admin, 'List Test', ['admin_api_key:manage'], null);
			await createBuilder(harness, apiKeyWithACL.token).get('/admin/api-keys').expect(HTTP_STATUS.OK).execute();
			const apiKeyWithoutACL = await createAdminApiKey(harness, admin, 'List Test No ACL', [], null);
			await createBuilder(harness, apiKeyWithoutACL.token)
				.get('/admin/api-keys')
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
		test('delete API key requires admin_api_key:manage ACL', async () => {
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
		test('delete API key fails without admin_api_key:manage ACL', async () => {
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
	describe('API Key Revocation', () => {
		test('basic revocation removes key from list', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, [
				'admin:authenticate',
				'admin_api_key:manage',
				'audit_log:view',
				'user:lookup',
				'guild:lookup',
			]);
			const apiKey = await createAdminApiKeyWithDefaultACLs(harness, admin, 'Revoke Test');
			let keys = await listAdminApiKeys(harness, admin.token);
			expect(keys).toHaveLength(1);
			expect(keys.some((k) => k.key_id === apiKey.keyId)).toBe(true);
			await revokeAdminApiKey(harness, admin.token, apiKey.keyId);
			keys = await listAdminApiKeys(harness, admin.token);
			expect(keys).toHaveLength(0);
		});
		test('revoked key cannot be used for authentication', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, [
				'admin:authenticate',
				'admin_api_key:manage',
				'audit_log:view',
				'user:lookup',
				'guild:lookup',
			]);
			const apiKey = await createAdminApiKeyWithDefaultACLs(harness, admin, 'Revoke Auth Test');
			await createBuilder(harness, apiKey.token).get(`/admin/users/${admin.userId}`).expect(HTTP_STATUS.OK).execute();
			await revokeAdminApiKey(harness, admin.token, apiKey.keyId);
			await createBuilder(harness, apiKey.token)
				.get(`/admin/users/${admin.userId}`)
				.expect(HTTP_STATUS.UNAUTHORIZED)
				.execute();
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
		test('get rejects a non-snowflake key id', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'admin_api_key:manage']);
			const {json} = await createBuilder<ValidationErrorResponse>(harness, `${admin.token}`)
				.get('/admin/api-keys/nonexistent-id')
				.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
				.executeWithResponse();
			expectInvalidKeyIdFormat(json);
		});
		test('update rejects a non-snowflake key id', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'admin_api_key:manage']);
			const {json} = await createBuilder<ValidationErrorResponse>(harness, `${admin.token}`)
				.patch('/admin/api-keys/nonexistent-id')
				.body({name: 'Renamed'})
				.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
				.executeWithResponse();
			expectInvalidKeyIdFormat(json);
		});
		test('revocation rejects a non-snowflake key id', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'admin_api_key:manage']);
			const {json} = await createBuilder<ValidationErrorResponse>(harness, `${admin.token}`)
				.delete('/admin/api-keys/nonexistent-id')
				.body(null)
				.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
				.executeWithResponse();
			expectInvalidKeyIdFormat(json);
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
			const keys = await listAdminApiKeys(harness, admin1.token);
			expect(keys.some((k) => k.key_id === apiKey.keyId)).toBe(true);
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
			const keys = await listAdminApiKeys(harness, admin1.token);
			expect(keys.some((k) => k.key_id === apiKey.keyId)).toBe(true);
		});
	});
	describe('Setting User ACLs Requires Proper ACL', () => {
		test('setting user ACLs requires acl:set:user ACL', async () => {
			const admin = await createTestAccount(harness);
			const targetUser = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'acl:set:user']);
			await createBuilder(harness, `${admin.token}`)
				.put(`/admin/users/${targetUser.userId}/acls`)
				.body({acls: ['admin:authenticate']})
				.expect(HTTP_STATUS.OK)
				.execute();
		});
		test('setting user ACLs fails without acl:set:user ACL', async () => {
			const admin = await createTestAccount(harness);
			const targetUser = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'user:lookup']);
			await createBuilder(harness, `${admin.token}`)
				.put(`/admin/users/${targetUser.userId}/acls`)
				.body({acls: ['admin:authenticate']})
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
		test('API key can set user ACLs with acl:set:user', async () => {
			const admin = await createTestAccount(harness);
			const targetUser = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'admin_api_key:manage', 'acl:set:user']);
			const apiKey = await createAdminApiKey(
				harness,
				admin,
				'ACL Setter Key',
				['admin:authenticate', 'acl:set:user'],
				null,
			);
			await createBuilder(harness, apiKey.token)
				.put(`/admin/users/${targetUser.userId}/acls`)
				.body({acls: ['admin:authenticate']})
				.expect(HTTP_STATUS.OK)
				.execute();
		});
		test('API key cannot set user ACLs without acl:set:user', async () => {
			const admin = await createTestAccount(harness);
			const targetUser = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'admin_api_key:manage', 'user:lookup']);
			const apiKey = await createAdminApiKey(harness, admin, 'No ACL Setter Key', ['user:lookup'], null);
			await createBuilder(harness, apiKey.token)
				.put(`/admin/users/${targetUser.userId}/acls`)
				.body({acls: ['admin:authenticate']})
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
		test('setting ACLs on non-existent user fails', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'acl:set:user']);
			await createBuilder(harness, `${admin.token}`)
				.put('/admin/users/999999999999999999/acls')
				.body({acls: ['admin:authenticate']})
				.expect(HTTP_STATUS.NOT_FOUND)
				.execute();
		});
	});
	describe('Deletion Schedule Minimum Validation', () => {
		test('schedule deletion requires user:delete ACL', async () => {
			const admin = await createTestAccount(harness);
			const targetUser = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'user:delete']);
			await createBuilder(harness, `${admin.token}`)
				.put(`/admin/users/${targetUser.userId}/deletion`)
				.body({reason_code: 1, days_until_deletion: 60})
				.expect(HTTP_STATUS.OK)
				.execute();
		});
		test('schedule deletion fails without user:delete ACL', async () => {
			const admin = await createTestAccount(harness);
			const targetUser = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'user:lookup']);
			await createBuilder(harness, `${admin.token}`)
				.put(`/admin/users/${targetUser.userId}/deletion`)
				.body({reason_code: 1, days_until_deletion: 60})
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
		test('deletion schedule enforces minimum days for user requested deletion', async () => {
			const admin = await createTestAccount(harness);
			const targetUser = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'user:delete']);
			await createBuilder(harness, `${admin.token}`)
				.put(`/admin/users/${targetUser.userId}/deletion`)
				.body({reason_code: 1, days_until_deletion: 1})
				.expect(HTTP_STATUS.OK)
				.executeWithResponse();
		});
		test('deletion schedule enforces minimum days for standard deletion', async () => {
			const admin = await createTestAccount(harness);
			const targetUser = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'user:delete']);
			await createBuilder(harness, `${admin.token}`)
				.put(`/admin/users/${targetUser.userId}/deletion`)
				.body({reason_code: 2, days_until_deletion: 1})
				.expect(HTTP_STATUS.OK)
				.executeWithResponse();
		});
		test('API key can schedule deletion with user:delete ACL', async () => {
			const admin = await createTestAccount(harness);
			const targetUser = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'admin_api_key:manage', 'user:delete']);
			const apiKey = await createAdminApiKey(harness, admin, 'Deletion Key', ['user:delete'], null);
			await createBuilder(harness, apiKey.token)
				.put(`/admin/users/${targetUser.userId}/deletion`)
				.body({reason_code: 1, days_until_deletion: 60})
				.expect(HTTP_STATUS.OK)
				.execute();
		});
		test('API key cannot schedule deletion without user:delete ACL', async () => {
			const admin = await createTestAccount(harness);
			const targetUser = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'admin_api_key:manage', 'user:lookup']);
			const apiKey = await createAdminApiKey(harness, admin, 'No Deletion Key', ['user:lookup'], null);
			await createBuilder(harness, apiKey.token)
				.put(`/admin/users/${targetUser.userId}/deletion`)
				.body({reason_code: 1, days_until_deletion: 60})
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
		test('schedule deletion on non-existent user fails', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'user:delete']);
			await createBuilder(harness, `${admin.token}`)
				.put('/admin/users/999999999999999999/deletion')
				.body({reason_code: 1, days_until_deletion: 60})
				.expect(HTTP_STATUS.NOT_FOUND)
				.execute();
		});
	});
});
