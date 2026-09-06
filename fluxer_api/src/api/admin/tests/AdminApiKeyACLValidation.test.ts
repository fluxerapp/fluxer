// SPDX-License-Identifier: AGPL-3.0-or-later

import {beforeEach, describe, expect, test} from 'vitest';
import {createTestAccount, setUserACLs} from '../../auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';
import {createAdminApiKey, listAdminApiKeys} from './AdminTestUtils';

describe('Admin API Key ACL Validation', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	test('user cannot grant ACLs they do not have - grant only ACLs user has', async () => {
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
	test('user cannot grant ACLs they do not have - grant ACL user does not have', async () => {
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
	test('user with wildcard can grant any ACL', async () => {
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
	test('ACLs are stored correctly', async () => {
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
	test('cannot grant admin_api_key:manage without having it', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'audit_log:view', 'user:lookup']);
		await createBuilder(harness, `${admin.token}`)
			.post('/admin/api-keys')
			.body({
				name: 'Test Key',
				acls: ['audit_log:view', 'admin_api_key:manage'],
			})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});
	test('user with limited ACLs can create key with subset', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'admin_api_key:manage', 'audit_log:view', 'user:lookup']);
		await createBuilder(harness, `${admin.token}`)
			.post('/admin/api-keys')
			.body({
				name: 'Test Key',
				acls: ['audit_log:view'],
			})
			.expect(HTTP_STATUS.OK)
			.execute();
	});
	test('user with limited ACLs cannot create key with broader ACLs', async () => {
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
	test('rejects an ACL outside the registry with 400', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['*']);
		await createBuilder(harness, `${admin.token}`)
			.post('/admin/api-keys')
			.body({
				name: 'Typo Key',
				acls: ['user:veiw'],
			})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.executeWithResponse();
		const keys = await listAdminApiKeys(harness, admin.token);
		expect(keys).toHaveLength(0);
	});
	test('update rejects an ACL outside the registry with 400 and leaves the stored set unchanged', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['*']);
		const apiKey = await createAdminApiKey(harness, admin, 'Typo Update Key', ['audit_log:view'], null);
		await createBuilder(harness, `${admin.token}`)
			.patch(`/admin/api-keys/${apiKey.keyId}`)
			.body({acls: ['user:veiw']})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.executeWithResponse();
		const keys = await listAdminApiKeys(harness, admin.token);
		expect(keys[0]!.acls).toEqual(['audit_log:view']);
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
