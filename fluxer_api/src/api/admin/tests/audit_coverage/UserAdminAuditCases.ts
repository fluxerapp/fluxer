// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AdminAuditCoverageCase} from '@app/api/admin/tests/audit_coverage/AdminAuditCoverage';
import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {
	createDmChannel,
	createFriendship,
	createGroupDmChannel,
	createGuild,
} from '@app/api/channel/tests/ChannelTestUtils';
import {getUserActivityBuffer} from '@app/api/middleware/ServiceSingletons';
import {HTTP_STATUS, TEST_CREDENTIALS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';

const MISSING_USER_ID = '999999999999999999';
const LAST_ACTIVE_IP = '198.51.100.91';

export const UserAdminAuditCases: ReadonlyArray<AdminAuditCoverageCase> = [
	{
		method: 'GET',
		route: '/admin/acls',
		async prepare() {
			return {
				request: {path: '/admin/acls'},
				expected: {
					action: 'list_admin_acls',
					targetType: 'admin_acl',
					targetId: '0',
					metadata: {acl_count: String(Object.values(AdminACLs).length)},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/users/@me',
		exempt: true,
		async prepare() {
			return {request: {path: '/admin/users/@me'}};
		},
	},
	{
		method: 'GET',
		route: '/admin/users',
		name: 'user_id',
		async prepare({harness}) {
			const target = await createTestAccount(harness);
			return {
				request: {path: `/admin/users?user_id=${target.userId}`},
				expected: {
					action: 'search_users',
					targetType: 'user',
					targetId: '0',
					metadata: {
						selector: 'user_id',
						user_id: target.userId,
						user_id_count: '1',
						result_count: '1',
						total: '1',
					},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/users',
		name: 'several user_id',
		async prepare({harness}) {
			const first = await createTestAccount(harness);
			const second = await createTestAccount(harness);
			return {
				request: {path: `/admin/users?user_id=${first.userId}&user_id=${second.userId}&user_id=${MISSING_USER_ID}`},
				expected: {
					action: 'search_users',
					targetType: 'user',
					targetId: '0',
					metadata: {
						selector: 'user_id',
						user_id_count: '3',
						result_count: '2',
						total: '2',
					},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/users',
		name: 'resolve',
		async prepare({harness}) {
			const target = await createTestAccount(harness);
			return {
				request: {path: `/admin/users?resolve=${encodeURIComponent(target.email)}`},
				expected: {
					action: 'search_users',
					targetType: 'user',
					targetId: '0',
					metadata: {
						selector: 'resolve',
						result_count: '1',
						total: '1',
					},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/users',
		name: 'email',
		async prepare({harness}) {
			const target = await createTestAccount(harness);
			return {
				request: {path: `/admin/users?email=${encodeURIComponent(target.email)}&limit=10`},
				expected: {
					action: 'search_users',
					targetType: 'user',
					targetId: '0',
					metadata: {
						selector: 'email',
						result_count: '1',
						total: '1',
					},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/users',
		name: 'last_active_ip',
		async prepare({harness}) {
			const target = await createTestAccount(harness);
			await createBuilder(harness, target.token)
				.get('/users/@me')
				.header('x-forwarded-for', LAST_ACTIVE_IP)
				.expect(HTTP_STATUS.OK)
				.execute();
			await getUserActivityBuffer().drainAndFlush();
			return {
				request: {path: `/admin/users?last_active_ip=${encodeURIComponent(LAST_ACTIVE_IP)}&limit=10&offset=0`},
				expected: {
					action: 'search_users',
					targetType: 'user',
					targetId: '0',
					metadata: {
						selector: 'last_active_ip',
						limit: '10',
						offset: '0',
						result_count: '1',
						total: '1',
					},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/users',
		name: 'search',
		search: 'enabled',
		async prepare() {
			return {
				request: {path: '/admin/users?q=nobody-matches-this-query&limit=25'},
				expected: {
					action: 'search_users',
					targetType: 'user',
					targetId: '0',
					metadata: {
						selector: 'search',
						has_query: 'true',
						limit: '25',
						offset: '0',
						result_count: '0',
						total: '0',
					},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/users/:user_id',
		name: 'found',
		async prepare({harness}) {
			const target = await createTestAccount(harness);
			return {
				request: {path: `/admin/users/${target.userId}`},
				expected: {
					action: 'get_user',
					targetType: 'user',
					targetId: target.userId,
					metadata: {found: 'true'},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/users/:user_id',
		name: 'missing',
		async prepare() {
			return {
				request: {path: `/admin/users/${MISSING_USER_ID}`},
				expected: {
					action: 'get_user',
					targetType: 'user',
					targetId: MISSING_USER_ID,
					metadata: {found: 'false'},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/users/:user_id/guilds',
		async prepare({harness}) {
			const target = await createTestAccount(harness);
			await createGuild(harness, target.token, 'Audit Guild One');
			const second = await createGuild(harness, target.token, 'Audit Guild Two');
			return {
				request: {path: `/admin/users/${target.userId}/guilds?before=${second.id}&limit=50`},
				expected: {
					action: 'list_user_guilds',
					targetType: 'user',
					targetId: target.userId,
					metadata: {
						before_guild_id: second.id,
						limit: '50',
						with_counts: 'false',
						guild_count: '1',
					},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/users/:user_id/dm-channels',
		name: 'dm',
		async prepare({harness}) {
			const target = await createTestAccount(harness);
			const first = await createTestAccount(harness);
			const second = await createTestAccount(harness);
			await createFriendship(harness, target, first);
			await createFriendship(harness, target, second);
			const firstDm = await createDmChannel(harness, target.token, first.userId);
			await createDmChannel(harness, target.token, second.userId);
			return {
				request: {path: `/admin/users/${target.userId}/dm-channels?after=${firstDm.id}&limit=10`},
				expected: {
					action: 'list_user_dm_channels',
					targetType: 'user',
					targetId: target.userId,
					metadata: {
						type: 'dm',
						after_channel_id: firstDm.id,
						limit: '10',
						channel_count: '1',
					},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/users/:user_id/dm-channels',
		name: 'group_dm',
		async prepare({harness}) {
			const target = await createTestAccount(harness);
			const first = await createTestAccount(harness);
			const second = await createTestAccount(harness);
			await createFriendship(harness, target, first);
			await createFriendship(harness, target, second);
			await createGroupDmChannel(harness, target.token, [first.userId, second.userId]);
			return {
				request: {path: `/admin/users/${target.userId}/dm-channels?type=group_dm`},
				expected: {
					action: 'list_user_dm_channels',
					targetType: 'user',
					targetId: target.userId,
					metadata: {type: 'group_dm', channel_count: '1'},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/users/:user_id/change-log',
		async prepare({harness}) {
			const target = await createTestAccount(harness);
			for (const username of ['auditfirstname', 'auditsecondname']) {
				await createBuilder(harness, target.token)
					.patch('/users/@me')
					.body({username, password: TEST_CREDENTIALS.STRONG_PASSWORD})
					.expect(HTTP_STATUS.OK)
					.execute();
			}
			return {
				request: {path: `/admin/users/${target.userId}/change-log?limit=50`},
				expected: {
					action: 'list_user_change_log',
					targetType: 'user',
					targetId: target.userId,
					metadata: {
						limit: '50',
						entry_count: '2',
					},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/users/:user_id/relationships',
		async prepare({harness}) {
			const target = await createTestAccount(harness);
			const friend = await createTestAccount(harness);
			const requested = await createTestAccount(harness);
			await createFriendship(harness, target, friend);
			await createBuilder(harness, target.token)
				.post(`/users/@me/relationships/${requested.userId}`)
				.body({})
				.execute();
			return {
				request: {path: `/admin/users/${target.userId}/relationships`},
				expected: {
					action: 'list_user_relationships',
					targetType: 'user',
					targetId: target.userId,
					metadata: {
						friend_count: '1',
						incoming_request_count: '0',
						outgoing_request_count: '1',
						blocked_count: '0',
					},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/users/:user_id/sessions',
		async prepare({harness}) {
			const target = await createTestAccount(harness);
			return {
				request: {path: `/admin/users/${target.userId}/sessions`},
				expected: {
					action: 'list_user_sessions',
					targetType: 'user',
					targetId: target.userId,
					metadata: {session_count: '1'},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/users/:user_id/webauthn-credentials',
		async prepare({harness}) {
			const target = await createTestAccount(harness);
			return {
				request: {path: `/admin/users/${target.userId}/webauthn-credentials`},
				expected: {
					action: 'list_webauthn_credentials',
					targetType: 'user',
					targetId: target.userId,
					metadata: {credential_count: '0'},
				},
			};
		},
	},
];
