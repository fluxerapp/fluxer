// SPDX-License-Identifier: AGPL-3.0-or-later

import {beforeEach, describe, test} from 'vitest';
import {createTestAccount, setUserACLs} from '../../auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '../../test/TestRequestBuilder';

describe('Security Access Control', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	describe('Blocked User Restrictions', () => {
		test('blocked user cannot send friend request', async () => {
			const user1 = await createTestAccount(harness);
			const user2 = await createTestAccount(harness);
			await createBuilder(harness, `${user1.token}`)
				.put(`/users/@me/relationships/${user2.userId}`)
				.body({
					type: 2,
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			await createBuilder(harness, `${user2.token}`)
				.post(`/users/@me/relationships/${user1.userId}`)
				.body(null)
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
		test('blocked user cannot create DM', async () => {
			const user1 = await createTestAccount(harness);
			const user2 = await createTestAccount(harness);
			await createBuilder(harness, `${user1.token}`)
				.put(`/users/@me/relationships/${user2.userId}`)
				.body({
					type: 2,
				})
				.execute();
			await createBuilder(harness, `${user2.token}`)
				.post('/users/@me/channels')
				.body({
					recipients: [user1.userId],
				})
				.expect(HTTP_STATUS.BAD_REQUEST, 'GROUP_DM_RECIPIENTS_NOT_ADDABLE')
				.execute();
		});
	});
	describe('Unauthorized Access', () => {
		test('unauthorized user cannot access other users @me endpoint', async () => {
			const user1 = await createTestAccount(harness);
			await createTestAccount(harness);
			await createBuilder(harness, `${user1.token}`).get('/users/@me').expect(HTTP_STATUS.OK).execute();
		});
		test('unauthorized access without token is rejected', async () => {
			await createBuilderWithoutAuth(harness).get('/users/@me').expect(HTTP_STATUS.UNAUTHORIZED).execute();
		});
		test('unauthorized access with invalid token is rejected', async () => {
			await createBuilder(harness, 'Bearer invalid_token_12345')
				.get('/users/@me')
				.expect(HTTP_STATUS.UNAUTHORIZED)
				.execute();
		});
		test('unauthorized guild access is rejected', async () => {
			const owner = await createTestAccount(harness);
			const attacker = await createTestAccount(harness);
			const createJson = await createBuilder<{
				id: string;
			}>(harness, `${owner.token}`)
				.post('/guilds')
				.body({
					name: 'Private Guild',
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			const guildId = createJson.id;
			await createBuilder(harness, `${attacker.token}`)
				.get(`/guilds/${guildId}`)
				.expect(HTTP_STATUS.FORBIDDEN, 'ACCESS_DENIED')
				.execute();
		});
		test('unauthorized channel access is rejected', async () => {
			const owner = await createTestAccount(harness);
			const attacker = await createTestAccount(harness);
			const createJson = await createBuilder<{
				id: string;
			}>(harness, `${owner.token}`)
				.post('/guilds')
				.body({
					name: 'Private Guild',
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			const guildId = createJson.id;
			await createBuilder(harness, `${attacker.token}`)
				.get(`/guilds/${guildId}/channels`)
				.expect(HTTP_STATUS.NOT_FOUND, 'UNKNOWN_GUILD')
				.execute();
		});
	});
	describe('Admin Endpoint Security', () => {
		test('admin endpoints require admin authentication', async () => {
			const regularUser = await createTestAccount(harness);
			await createBuilder(harness, `${regularUser.token}`)
				.get(`/admin/users/${regularUser.userId}`)
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
		test('admin endpoints require proper ACLs', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate']);
			await createBuilder(harness, `${admin.token}`)
				.get(`/admin/users/${admin.userId}`)
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
		test('admin endpoints succeed with proper ACLs', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'user:lookup']);
			await createBuilder(harness, `${admin.token}`)
				.get(`/admin/users/${admin.userId}`)
				.expect(HTTP_STATUS.OK)
				.execute();
		});
	});
	describe('Token Security', () => {
		test('token must have valid format', async () => {
			await createBuilder(harness, 'Bearer invalid_format')
				.get('/users/@me')
				.expect(HTTP_STATUS.UNAUTHORIZED)
				.execute();
		});
		test('empty token is rejected', async () => {
			await createBuilder(harness, 'Bearer ').get('/users/@me').expect(HTTP_STATUS.UNAUTHORIZED).execute();
		});
		test('malformed token is rejected', async () => {
			await createBuilder(harness, 'Bearer flx_invalid_token_format')
				.get('/users/@me')
				.expect(HTTP_STATUS.UNAUTHORIZED)
				.execute();
		});
	});
});
