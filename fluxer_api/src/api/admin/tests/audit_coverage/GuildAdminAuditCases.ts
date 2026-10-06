// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AdminAuditCoverageCase} from '@app/api/admin/tests/audit_coverage/AdminAuditCoverage';
import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createGuild} from '@app/api/message/tests/MessageTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';

const MISSING_GUILD_ID = '900000000000000001';

async function addMember(
	harness: ApiTestHarness,
	admin: TestAccount,
	guildId: string,
	member: TestAccount,
): Promise<void> {
	await createBuilder(harness, admin.token)
		.put(`/admin/guilds/${guildId}/members/${member.userId}`)
		.body(null)
		.expect(200)
		.execute();
}

export const GuildAdminAuditCases: ReadonlyArray<AdminAuditCoverageCase> = [
	{
		method: 'GET',
		route: '/admin/guilds',
		search: 'enabled',
		async prepare({harness, admin}) {
			const name = `Audit Search Guild ${Date.now()}`;
			await createGuild(harness, admin.token, name);
			return {
				request: {path: `/admin/guilds?q=${encodeURIComponent(name)}&limit=10`},
				expected: {
					action: 'search_guilds',
					targetType: 'guild',
					targetId: '0',
					metadata: {
						has_query: 'true',
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
		route: '/admin/guilds/:guild_id',
		name: 'found',
		async prepare({harness, admin}) {
			const guild = await createGuild(harness, admin.token, 'Audit Detail Guild');
			return {
				request: {path: `/admin/guilds/${guild.id}`},
				expected: {
					action: 'get_guild',
					targetType: 'guild',
					targetId: guild.id,
					metadata: {found: 'true'},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/guilds/:guild_id',
		name: 'missing',
		async prepare() {
			return {
				request: {path: `/admin/guilds/${MISSING_GUILD_ID}`},
				expected: {
					action: 'get_guild',
					targetType: 'guild',
					targetId: MISSING_GUILD_ID,
					metadata: {found: 'false'},
				},
			};
		},
	},
	{
		method: 'PATCH',
		route: '/admin/guilds/:guild_id',
		async prepare({harness, admin}) {
			const guild = await createGuild(harness, admin.token, 'Audit Patch Guild');
			return {
				request: {path: `/admin/guilds/${guild.id}`, body: {name: 'Audit Patched Guild'}},
				expected: {
					action: 'update_name',
					targetType: 'guild',
					targetId: guild.id,
					metadata: {old_name: 'Audit Patch Guild', new_name: 'Audit Patched Guild'},
				},
			};
		},
	},
	{
		method: 'PATCH',
		route: '/admin/guilds/:guild_id',
		name: 'empty body',
		async prepare({harness, admin}) {
			const guild = await createGuild(harness, admin.token, 'Audit Empty Patch Guild');
			return {
				request: {path: `/admin/guilds/${guild.id}`, body: {}},
				expected: {
					action: 'update_guild',
					targetType: 'guild',
					targetId: guild.id,
					metadata: {},
				},
			};
		},
	},
	{
		method: 'DELETE',
		route: '/admin/guilds/:guild_id',
		async prepare({harness, admin}) {
			const guild = await createGuild(harness, admin.token, 'Audit Deleted Guild');
			return {
				request: {path: `/admin/guilds/${guild.id}`},
				expected: {
					action: 'delete_guild',
					targetType: 'guild',
					targetId: guild.id,
					metadata: {guild_id: guild.id},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/guilds/:guild_id/members',
		async prepare({harness, admin}) {
			const guild = await createGuild(harness, admin.token, 'Audit Members Guild');
			return {
				request: {path: `/admin/guilds/${guild.id}/members?limit=10&offset=5`},
				expected: {
					action: 'list_guild_members',
					targetType: 'guild',
					targetId: guild.id,
					metadata: {limit: '10', offset: '5', result_count: '0', total: '0'},
				},
			};
		},
	},
	{
		method: 'PUT',
		route: '/admin/guilds/:guild_id/members/:user_id',
		async prepare({harness, admin}) {
			const guild = await createGuild(harness, admin.token, 'Audit Force Add Guild');
			const target = await createTestAccount(harness);
			return {
				request: {path: `/admin/guilds/${guild.id}/members/${target.userId}`},
				expected: {
					action: 'force_add_to_guild',
					targetType: 'user',
					targetId: target.userId,
					metadata: {guild_id: guild.id},
				},
			};
		},
	},
	{
		method: 'DELETE',
		route: '/admin/guilds/:guild_id/members/:user_id',
		async prepare({harness, admin}) {
			const guild = await createGuild(harness, admin.token, 'Audit Kick Guild');
			const target = await createTestAccount(harness);
			await addMember(harness, admin, guild.id, target);
			return {
				request: {path: `/admin/guilds/${guild.id}/members/${target.userId}`, expectStatus: 204},
				expected: {
					action: 'kick_member',
					targetType: 'guild_member',
					targetId: target.userId,
					metadata: {guild_id: guild.id, user_id: target.userId},
				},
			};
		},
	},
	{
		method: 'PUT',
		route: '/admin/guilds/:guild_id/bans/:user_id',
		async prepare({harness, admin}) {
			const guild = await createGuild(harness, admin.token, 'Audit Ban Guild');
			const target = await createTestAccount(harness);
			return {
				request: {path: `/admin/guilds/${guild.id}/bans/${target.userId}`, body: {}, expectStatus: 204},
				expected: {
					action: 'ban_member',
					targetType: 'guild_member',
					targetId: target.userId,
					metadata: {guild_id: guild.id, user_id: target.userId, delete_message_days: '0'},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/guilds/:guild_id/emojis',
		async prepare({harness, admin}) {
			const guild = await createGuild(harness, admin.token, 'Audit Emoji Guild');
			return {
				request: {path: `/admin/guilds/${guild.id}/emojis`},
				expected: {
					action: 'list_guild_emojis',
					targetType: 'guild',
					targetId: guild.id,
					metadata: {result_count: '0'},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/guilds/:guild_id/stickers',
		async prepare({harness, admin}) {
			const guild = await createGuild(harness, admin.token, 'Audit Sticker Guild');
			return {
				request: {path: `/admin/guilds/${guild.id}/stickers`},
				expected: {
					action: 'list_guild_stickers',
					targetType: 'guild',
					targetId: guild.id,
					metadata: {result_count: '0'},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/guilds/:guild_id/audit-logs',
		async prepare({harness, admin}) {
			const guild = await createGuild(harness, admin.token, 'Audit Log Guild');
			await createBuilder(harness, admin.token)
				.patch(`/guilds/${guild.id}`)
				.body({name: 'Audit Log Guild Renamed'})
				.expect(200)
				.execute();
			return {
				request: {
					path: `/admin/guilds/${guild.id}/audit-logs?limit=10&user_id=${admin.userId}&action_type=${AuditLogActionType.GUILD_UPDATE}`,
				},
				expected: {
					action: 'list_guild_audit_logs',
					targetType: 'guild',
					targetId: guild.id,
					metadata: {
						limit: '10',
						filter_user_id: admin.userId,
						action_type: String(AuditLogActionType.GUILD_UPDATE),
						result_count: '1',
					},
				},
			};
		},
	},
	{
		method: 'POST',
		route: '/admin/guilds/:guild_id/reloads',
		async prepare({harness, admin}) {
			const guild = await createGuild(harness, admin.token, 'Audit Reload Guild');
			return {
				request: {path: `/admin/guilds/${guild.id}/reloads`},
				expected: {
					action: 'reload_guild',
					targetType: 'guild',
					targetId: guild.id,
					metadata: {guild_id: guild.id},
				},
			};
		},
	},
	{
		method: 'POST',
		route: '/admin/guilds/:guild_id/shutdowns',
		async prepare({harness, admin}) {
			const guild = await createGuild(harness, admin.token, 'Audit Shutdown Guild');
			return {
				request: {path: `/admin/guilds/${guild.id}/shutdowns`},
				expected: {
					action: 'shutdown_guild',
					targetType: 'guild',
					targetId: guild.id,
					metadata: {guild_id: guild.id},
				},
			};
		},
	},
];
