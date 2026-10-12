// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth, type TestRequestBuilder} from '@app/api/test/TestRequestBuilder';
import {beforeEach, describe, test} from 'vitest';

function withMethod(builder: TestRequestBuilder, method: string, path: string, body: unknown = {}): TestRequestBuilder {
	switch (method) {
		case 'POST':
			return builder.post(path).body(body);
		case 'PATCH':
			return builder.patch(path).body(body);
		case 'PUT':
			return builder.put(path).body(body);
		case 'DELETE':
			return builder.delete(path);
		default:
			return builder.get(path);
	}
}

interface AdminEndpointCase {
	method: string;
	path: string;
	body?: unknown;
}

const adminEndpoints: Array<AdminEndpointCase> = [
	{method: 'GET', path: '/admin/reports'},
	{method: 'GET', path: '/admin/reports/1'},
	{method: 'PATCH', path: '/admin/reports/1'},
	{method: 'DELETE', path: '/admin/reports/1'},
	{
		method: 'POST',
		path: '/admin/bulk-jobs',
		body: {task: 'update_user_flags', user_ids: ['1']},
	},
	{
		method: 'POST',
		path: '/admin/bulk-jobs',
		body: {task: 'update_guild_features', guild_ids: ['1']},
	},
	{
		method: 'POST',
		path: '/admin/bulk-jobs',
		body: {task: 'add_guild_members', guild_id: '1', user_ids: ['2']},
	},
	{method: 'GET', path: '/admin/guilds'},
	{method: 'GET', path: '/admin/users'},
	{method: 'GET', path: '/admin/messages?channel_id=1'},
];

describe('Admin Endpoints Authorization', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness({search: 'enabled'});
	});
	test('admin endpoints require authentication', async () => {
		for (const endpoint of adminEndpoints.slice(0, 5)) {
			await withMethod(createBuilderWithoutAuth(harness), endpoint.method, endpoint.path, endpoint.body)
				.expect(HTTP_STATUS.UNAUTHORIZED)
				.execute();
		}
	});
	test('admin endpoints require proper ACLs', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate']);
		for (const endpoint of adminEndpoints.slice(0, 10)) {
			await withMethod(createBuilder(harness, `${admin.token}`), endpoint.method, endpoint.path, endpoint.body)
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		}
	});
	test('user lookup endpoint requires user:lookup ACL', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'admin_api_key:manage', 'audit_log:view']);
		await createBuilder(harness, `${admin.token}`)
			.get(`/admin/users/${admin.userId}`)
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});
	test('guild lookup endpoint requires guild:lookup ACL', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'admin_api_key:manage', 'audit_log:view']);
		await createBuilder(harness, `${admin.token}`)
			.get('/admin/guilds/123456789')
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});
	test('audit logs endpoint requires audit_log:view ACL', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'admin_api_key:manage', 'user:lookup']);
		await createBuilder(harness, `${admin.token}`)
			.get('/admin/audit-logs?limit=10')
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});
});
