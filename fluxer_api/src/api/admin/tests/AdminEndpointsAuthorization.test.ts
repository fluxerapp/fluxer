// SPDX-License-Identifier: AGPL-3.0-or-later

import {beforeEach, describe, test} from 'vitest';
import {createTestAccount, setUserACLs} from '../../auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder, createBuilderWithoutAuth, type TestRequestBuilder} from '../../test/TestRequestBuilder';

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
	requiredACL: string;
	body?: unknown;
}

const adminEndpoints: Array<AdminEndpointCase> = [
	{method: 'GET', path: '/admin/reports', requiredACL: 'report:view'},
	{method: 'GET', path: '/admin/reports/1', requiredACL: 'report:view'},
	{method: 'PATCH', path: '/admin/reports/1', requiredACL: 'report:resolve'},
	{
		method: 'POST',
		path: '/admin/bulk-jobs',
		requiredACL: 'bulk:update:user_flags',
		body: {task: 'update_user_flags', user_ids: ['1']},
	},
	{
		method: 'POST',
		path: '/admin/bulk-jobs',
		requiredACL: 'bulk:update:guild_features',
		body: {task: 'update_guild_features', guild_ids: ['1']},
	},
	{
		method: 'POST',
		path: '/admin/bulk-jobs',
		requiredACL: 'bulk:add:guild_members',
		body: {task: 'add_guild_members', guild_id: '1', user_ids: ['2']},
	},
	{method: 'GET', path: '/admin/guilds', requiredACL: 'guild:lookup'},
	{method: 'GET', path: '/admin/users', requiredACL: 'user:lookup'},
	{method: 'GET', path: '/admin/messages?channel_id=1', requiredACL: 'message:lookup'},
	{method: 'DELETE', path: '/admin/channels/1/messages/1', requiredACL: 'message:delete'},
	{method: 'GET', path: '/admin/gateway/memory-stats', requiredACL: 'gateway:manage'},
	{method: 'POST', path: '/admin/gateway/reloads', requiredACL: 'gateway:manage'},
	{method: 'GET', path: '/admin/gateway/stats', requiredACL: 'gateway:manage'},
	{method: 'GET', path: '/admin/gateway/voice-state-counts', requiredACL: 'gateway:manage'},
	{method: 'GET', path: '/admin/audit-logs', requiredACL: 'audit_log:view'},
	{method: 'GET', path: '/admin/audit-logs?q=example', requiredACL: 'audit_log:view'},
	{method: 'GET', path: '/admin/guilds/1', requiredACL: 'guild:lookup'},
	{method: 'GET', path: '/admin/guilds/1/members', requiredACL: 'guild:list:members'},
	{method: 'PATCH', path: '/admin/guilds/1', requiredACL: 'guild:update'},
	{method: 'DELETE', path: '/admin/guilds/1', requiredACL: 'guild:delete'},
	{method: 'PUT', path: '/admin/guilds/1/members/2', requiredACL: 'guild:force_add_member'},
	{method: 'DELETE', path: '/admin/guilds/1/members/2', requiredACL: 'guild:kick_member'},
	{method: 'PUT', path: '/admin/guilds/1/bans/2', requiredACL: 'guild:ban_member'},
	{method: 'GET', path: '/admin/guilds/1/audit-logs', requiredACL: 'guild:audit_log:view'},
	{method: 'DELETE', path: '/admin/guilds/1/assets', requiredACL: 'asset:purge'},
	{method: 'POST', path: '/admin/guilds/1/reloads', requiredACL: 'guild:reload'},
	{method: 'POST', path: '/admin/guilds/1/shutdowns', requiredACL: 'guild:shutdown'},
	{method: 'GET', path: '/admin/users/1', requiredACL: 'user:lookup'},
	{method: 'GET', path: '/admin/users/1/guilds', requiredACL: 'user:list:guilds'},
	{method: 'GET', path: '/admin/users/1/dm-channels', requiredACL: 'user:list:dm_channels'},
	{method: 'DELETE', path: '/admin/users/1/mfa', requiredACL: 'user:update:mfa'},
	{method: 'GET', path: '/admin/users/1/webauthn-credentials', requiredACL: 'user:update:mfa'},
	{method: 'DELETE', path: '/admin/users/1/webauthn-credentials/credential', requiredACL: 'user:update:mfa'},
	{method: 'DELETE', path: '/admin/users/1/profile-fields', requiredACL: 'user:update:profile'},
	{method: 'PUT', path: '/admin/users/1/bot-status', requiredACL: 'user:update:bot_status'},
	{method: 'PUT', path: '/admin/users/1/acls', requiredACL: 'acl:set:user'},
	{method: 'PUT', path: '/admin/users/1/deletion', requiredACL: 'user:delete'},
	{method: 'POST', path: '/admin/users/1/avatar-block', requiredACL: 'ban:avatar_hash:add'},
	{method: 'GET', path: '/admin/guilds/1/emojis', requiredACL: 'asset:purge'},
	{method: 'GET', path: '/admin/guilds/1/stickers', requiredACL: 'asset:purge'},
	{method: 'GET', path: '/admin/blocklists', requiredACL: 'ban:ip:check'},
	{method: 'GET', path: '/admin/blocklists/ip/entries', requiredACL: 'ban:ip:check'},
	{method: 'POST', path: '/admin/blocklists/ip/entries', requiredACL: 'ban:ip:add', body: {ip: '198.51.100.9'}},
	{method: 'GET', path: '/admin/discovery/applications', requiredACL: 'discovery:review'},
	{method: 'GET', path: '/admin/discovery/listings', requiredACL: 'discovery:review'},
	{method: 'PATCH', path: '/admin/discovery/listings/1', requiredACL: 'discovery:review'},
	{method: 'DELETE', path: '/admin/discovery/listings/1', requiredACL: 'discovery:remove'},
	{method: 'GET', path: '/admin/search/index-refreshes/1', requiredACL: 'guild:lookup'},
	{
		method: 'POST',
		path: '/admin/search/indexes/users/refreshes',
		requiredACL: 'guild:lookup',
		body: {},
	},
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
	test('admin endpoints succeed with proper ACLs', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'user:lookup', 'guild:lookup']);
		const endpointsToTest = ['/admin/users/123'];
		for (const path of endpointsToTest) {
			await createBuilder(harness, `${admin.token}`).get(path).expect(HTTP_STATUS.OK).execute();
		}
		await createBuilder(harness, `${admin.token}`).get('/admin/guilds/123').expect(HTTP_STATUS.OK).execute();
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
