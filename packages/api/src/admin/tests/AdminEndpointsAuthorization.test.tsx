/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {createTestAccount, setUserACLs} from '@fluxer/api/src/auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@fluxer/api/src/test/ApiTestHarness';
import {HTTP_STATUS} from '@fluxer/api/src/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@fluxer/api/src/test/TestRequestBuilder';
import {beforeEach, describe, test} from 'vitest';

const adminEndpoints = [
	{method: 'POST', path: '/admin/reports/list', requiredACL: 'report:manage'},
	{method: 'GET', path: '/admin/reports/1', requiredACL: 'report:manage'},
	{method: 'POST', path: '/admin/reports/resolve', requiredACL: 'report:manage'},
	{method: 'POST', path: '/admin/bulk/update-user-flags', requiredACL: 'bulk:update'},
	{method: 'POST', path: '/admin/bulk/update-guild-features', requiredACL: 'bulk:update'},
	{method: 'POST', path: '/admin/bulk/add-guild-members', requiredACL: 'bulk:update'},
	{method: 'POST', path: '/admin/guilds/search', requiredACL: 'guild:lookup'},
	{method: 'POST', path: '/admin/users/search', requiredACL: 'user:lookup'},
	{method: 'POST', path: '/admin/messages/lookup', requiredACL: 'message:lookup'},
	{method: 'POST', path: '/admin/messages/delete', requiredACL: 'message:delete'},
	{method: 'POST', path: '/admin/gateway/memory-stats', requiredACL: 'gateway:manage'},
	{method: 'POST', path: '/admin/gateway/reload-all', requiredACL: 'gateway:manage'},
	{method: 'GET', path: '/admin/gateway/stats', requiredACL: 'gateway:manage'},
	{method: 'POST', path: '/admin/audit-logs', requiredACL: 'audit_log:view'},
	{method: 'POST', path: '/admin/audit-logs/search', requiredACL: 'audit_log:view'},
	{method: 'POST', path: '/admin/guilds/lookup', requiredACL: 'guild:lookup'},
	{method: 'POST', path: '/admin/guilds/list-members', requiredACL: 'guild:lookup'},
	{method: 'POST', path: '/admin/guilds/update-features', requiredACL: 'guild:update'},
	{method: 'POST', path: '/admin/guilds/update-name', requiredACL: 'guild:update'},
	{method: 'POST', path: '/admin/guilds/update-settings', requiredACL: 'guild:update'},
	{method: 'POST', path: '/admin/guilds/transfer-ownership', requiredACL: 'guild:update'},
	{method: 'POST', path: '/admin/guilds/update-vanity', requiredACL: 'guild:update'},
	{method: 'POST', path: '/admin/guilds/force-add-user', requiredACL: 'guild:update'},
	{method: 'POST', path: '/admin/guilds/reload', requiredACL: 'guild:update'},
	{method: 'POST', path: '/admin/guilds/shutdown', requiredACL: 'guild:update'},
	{method: 'POST', path: '/admin/users/lookup', requiredACL: 'user:lookup'},
	{method: 'POST', path: '/admin/users/list-guilds', requiredACL: 'user:lookup'},
	{method: 'POST', path: '/admin/users/list-dm-channels', requiredACL: 'user:list:dm_channels'},
	{method: 'POST', path: '/admin/users/disable-mfa', requiredACL: 'user:update'},
	{method: 'POST', path: '/admin/users/list-webauthn-credentials', requiredACL: 'user:update:mfa'},
	{method: 'POST', path: '/admin/users/delete-webauthn-credential', requiredACL: 'user:update:mfa'},
	{method: 'POST', path: '/admin/users/clear-fields', requiredACL: 'user:update'},
	{method: 'POST', path: '/admin/users/set-bot-status', requiredACL: 'user:update'},
	{method: 'POST', path: '/admin/users/set-acls', requiredACL: 'acl:set:user'},
	{method: 'POST', path: '/admin/users/schedule-deletion', requiredACL: 'user:delete'},
];

describe('Admin Endpoints Authorization', () => {
	let harness: ApiTestHarness;

	beforeEach(async () => {
		harness = await createApiTestHarness({search: 'meilisearch'});
	});

	test('admin endpoints require authentication', async () => {
		for (const endpoint of adminEndpoints.slice(0, 5)) {
			if (endpoint.method === 'POST') {
				await createBuilderWithoutAuth(harness).post(endpoint.path).expect(HTTP_STATUS.UNAUTHORIZED).execute();
			} else {
				await createBuilderWithoutAuth(harness).get(endpoint.path).expect(HTTP_STATUS.UNAUTHORIZED).execute();
			}
		}
	});

	test('admin endpoints require proper ACLs', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate']);

		for (const endpoint of adminEndpoints.slice(0, 10)) {
			if (endpoint.method === 'POST') {
				await createBuilder(harness, `Bearer ${admin.token}`)
					.post(endpoint.path)
					.body({})
					.expect(HTTP_STATUS.FORBIDDEN)
					.execute();
			} else {
				await createBuilder(harness, `Bearer ${admin.token}`)
					.get(endpoint.path)
					.expect(HTTP_STATUS.FORBIDDEN)
					.execute();
			}
		}
	});

	test('admin endpoints succeed with proper ACLs', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'user:lookup', 'guild:lookup']);

		const endpointsToTest = [
			{path: '/admin/users/lookup', body: {user_ids: ['123']}},
			{path: '/admin/guilds/lookup', body: {guild_id: '123'}},
		];

		for (const endpoint of endpointsToTest) {
			await createBuilder(harness, `Bearer ${admin.token}`)
				.post(endpoint.path)
				.body(endpoint.body)
				.expect(HTTP_STATUS.OK)
				.execute();
		}
	});

	test('user lookup endpoint requires user:lookup ACL', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'admin_api_key:manage', 'audit_log:view']);

		await createBuilder(harness, `Bearer ${admin.token}`)
			.post('/admin/users/lookup')
			.body({
				user_ids: [admin.userId],
			})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});

	test('guild lookup endpoint requires guild:lookup ACL', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'admin_api_key:manage', 'audit_log:view']);

		await createBuilder(harness, `Bearer ${admin.token}`)
			.post('/admin/guilds/lookup')
			.body({
				guild_ids: ['123456789'],
			})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});

	test('audit logs endpoint requires audit_log:view ACL', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'admin_api_key:manage', 'user:lookup']);

		await createBuilder(harness, `Bearer ${admin.token}`)
			.post('/admin/audit-logs')
			.body({
				limit: 10,
			})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});
});
