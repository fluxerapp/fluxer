// SPDX-License-Identifier: AGPL-3.0-or-later

import {afterEach, beforeEach, describe, expect, test} from 'vitest';
import {createTestAccount, setUserACLs} from '../../auth/tests/AuthTestUtils';
import {createDmChannel, createFriendship, createGuild} from '../../channel/tests/ChannelTestUtils';
import {getUserActivityBuffer} from '../../middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';

async function setLastActiveIp(harness: ApiTestHarness, token: string, ip: string): Promise<void> {
	await createBuilder(harness, `${token}`)
		.get('/users/@me')
		.header('x-forwarded-for', ip)
		.expect(HTTP_STATUS.OK)
		.execute();
	await getUserActivityBuffer().drainAndFlush();
}

describe('Admin Search Endpoints', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness({search: 'enabled'});
	});
	afterEach(async () => {
		await harness.shutdown();
	});
	describe('GET /admin/users', () => {
		test('requires user:lookup ACL', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate']);
			await createBuilder(harness, `${admin.token}`)
				.get(`/admin/users?q=${encodeURIComponent('test')}&limit=10&offset=0`)
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
		test('returns empty results for non-matching query', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'user:lookup']);
			const result = await createBuilder<{
				users: Array<unknown>;
				total: number;
			}>(harness, `${admin.token}`)
				.get(`/admin/users?q=${encodeURIComponent('nonexistent-user-query-xyz')}&limit=10&offset=0`)
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.users).toEqual([]);
			expect(result.total).toBe(0);
		});
		test('returns matching users when query matches username', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'user:lookup']);
			const targetUser = await createTestAccount(harness, {
				username: `searchable_user_${Date.now()}`,
			});
			const result = await createBuilder<{
				users: Array<{
					id: string;
					username: string;
				}>;
				total: number;
			}>(harness, `${admin.token}`)
				.get(`/admin/users?q=${encodeURIComponent(targetUser.username ?? '')}&limit=10&offset=0`)
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.total).toBeGreaterThanOrEqual(1);
			const foundUser = result.users.find((u) => u.id === targetUser.userId);
			expect(foundUser).toBeDefined();
			expect(foundUser?.username).toBe(targetUser.username);
		});
		test('respects limit and offset parameters', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'user:lookup']);
			const result = await createBuilder<{
				users: Array<unknown>;
				total: number;
			}>(harness, `${admin.token}`)
				.get(`/admin/users?limit=1&offset=0`)
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.users.length).toBeLessThanOrEqual(1);
		});
		test('supports searching by last active IP', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'user:lookup', 'user:view:ip']);
			const targetUser = await createTestAccount(harness);
			await setLastActiveIp(harness, targetUser.token, '198.51.100.91');
			const result = await createBuilder<{
				users: Array<{
					id: string;
				}>;
				total: number;
			}>(harness, `${admin.token}`)
				.get(`/admin/users?last_active_ip=${encodeURIComponent('198.51.100.91')}&limit=10&offset=0`)
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.total).toBeGreaterThanOrEqual(1);
			expect(result.users.find((user) => user.id === targetUser.userId)).toBeDefined();
		});
	});
	describe('GET /admin/users/{user_id}/dm-channels', () => {
		test('requires user:list:dm_channels ACL', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate']);
			await createBuilder(harness, `${admin.token}`)
				.get(`/admin/users/${admin.userId}/dm-channels?limit=10`)
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
		test('returns paginated historical DM channels', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'user:list:dm_channels']);
			const subjectUser = await createTestAccount(harness);
			const recipientA = await createTestAccount(harness);
			const recipientB = await createTestAccount(harness);
			const recipientC = await createTestAccount(harness);
			await createFriendship(harness, subjectUser, recipientA);
			await createFriendship(harness, subjectUser, recipientB);
			await createFriendship(harness, subjectUser, recipientC);
			const dmA = await createDmChannel(harness, subjectUser.token, recipientA.userId);
			const dmB = await createDmChannel(harness, subjectUser.token, recipientB.userId);
			const dmC = await createDmChannel(harness, subjectUser.token, recipientC.userId);
			const firstPage = await createBuilder<{
				channels: Array<{
					channel_id: string;
					channel_type: number | null;
					recipient_ids: Array<string>;
					last_message_id: string | null;
					is_open: boolean;
				}>;
			}>(harness, `${admin.token}`)
				.get(`/admin/users/${subjectUser.userId}/dm-channels?limit=2`)
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(firstPage.channels).toHaveLength(2);
			expect(BigInt(firstPage.channels[0]!.channel_id)).toBeGreaterThan(BigInt(firstPage.channels[1]!.channel_id));
			for (const channel of firstPage.channels) {
				expect(channel.channel_type).toBe(1);
				expect(channel.recipient_ids).toContain(subjectUser.userId);
				expect(channel.is_open).toBe(true);
			}
			const secondPage = await createBuilder<{
				channels: Array<{
					channel_id: string;
					channel_type: number | null;
					recipient_ids: Array<string>;
					last_message_id: string | null;
					is_open: boolean;
				}>;
			}>(harness, `${admin.token}`)
				.get(`/admin/users/${subjectUser.userId}/dm-channels?limit=2&before=${firstPage.channels[1]!.channel_id}`)
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(secondPage.channels).toHaveLength(1);
			expect(secondPage.channels[0]!.channel_id).not.toBe(firstPage.channels[0]!.channel_id);
			expect(secondPage.channels[0]!.channel_id).not.toBe(firstPage.channels[1]!.channel_id);
			const previousPage = await createBuilder<{
				channels: Array<{
					channel_id: string;
					channel_type: number | null;
					recipient_ids: Array<string>;
					last_message_id: string | null;
					is_open: boolean;
				}>;
			}>(harness, `${admin.token}`)
				.get(`/admin/users/${subjectUser.userId}/dm-channels?limit=2&after=${secondPage.channels[0]!.channel_id}`)
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(previousPage.channels.map((channel) => channel.channel_id)).toEqual(
				firstPage.channels.map((channel) => channel.channel_id),
			);
			expect(new Set([dmA.id, dmB.id, dmC.id]).size).toBe(3);
		});
		test('rejects requests that specify both before and after cursors', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'user:list:dm_channels']);
			await createBuilder(harness, `${admin.token}`)
				.get(`/admin/users/${admin.userId}/dm-channels?limit=10&before=1&after=2`)
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
	});
	describe('GET /admin/guilds', () => {
		test('requires guild:lookup ACL', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate']);
			await createBuilder(harness, `${admin.token}`)
				.get('/admin/guilds?q=test&limit=10&offset=0')
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
		test('returns empty results for non-matching query', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'guild:lookup']);
			const result = await createBuilder<{
				guilds: Array<unknown>;
				total: number;
			}>(harness, `${admin.token}`)
				.get('/admin/guilds?q=nonexistent-guild-query-xyz&limit=10&offset=0')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.guilds).toEqual([]);
			expect(result.total).toBe(0);
		});
		test('returns matching guilds when query matches guild name', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'guild:lookup']);
			const guildName = `searchable-guild-${Date.now()}`;
			const guild = await createGuild(harness, admin.token, guildName);
			const result = await createBuilder<{
				guilds: Array<{
					id: string;
					name: string;
				}>;
				total: number;
			}>(harness, `${admin.token}`)
				.get(`/admin/guilds?q=${encodeURIComponent(guildName)}&limit=10&offset=0`)
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.total).toBeGreaterThanOrEqual(1);
			const foundGuild = result.guilds.find((g) => g.id === guild.id);
			expect(foundGuild).toBeDefined();
			expect(foundGuild?.name).toBe(guildName);
		});
	});
	describe('/admin/reports', () => {
		test('requires report:view ACL', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate']);
			await createBuilder(harness, `${admin.token}`)
				.get('/admin/reports?q=example&limit=10&offset=0')
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
		test('returns report list response when searching the report index', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'report:view']);
			const result = await createBuilder<{
				reports: Array<unknown>;
				total: number;
			}>(harness, `${admin.token}`)
				.get('/admin/reports?q=example&limit=10&offset=0')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(Array.isArray(result.reports)).toBe(true);
			expect(result.total).toBeGreaterThanOrEqual(result.reports.length);
		});
		test('returns report list response without search filters', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'report:view']);
			const result = await createBuilder<{
				reports: Array<unknown>;
			}>(harness, `${admin.token}`)
				.get('/admin/reports?status=pending&limit=10&offset=0')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(Array.isArray(result.reports)).toBe(true);
		});
	});
	describe('/admin/audit-logs (search)', () => {
		test('requires audit_log:view ACL', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate']);
			await createBuilder(harness, `${admin.token}`)
				.get('/admin/audit-logs?q=set_acls&limit=10&offset=0')
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
		test('returns results with proper structure', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'audit_log:view']);
			const result = await createBuilder<{
				logs: Array<unknown>;
				total: number;
			}>(harness, `${admin.token}`)
				.get('/admin/audit-logs?q=set_acls&limit=10&offset=0')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result).toHaveProperty('logs');
			expect(result).toHaveProperty('total');
			expect(Array.isArray(result.logs)).toBe(true);
			expect(typeof result.total).toBe('number');
		});
		test('supports filtering by admin_user_id', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'audit_log:view', 'user:update_acls', 'acl:set:user']);
			const targetUser = await createTestAccount(harness);
			await createBuilder(harness, `${admin.token}`)
				.put(`/admin/users/${targetUser.userId}/acls`)
				.body({acls: ['admin:authenticate']})
				.expect(HTTP_STATUS.OK)
				.execute();
			const result = await createBuilder<{
				logs: Array<{
					admin_user_id: string;
					action: string;
				}>;
				total: number;
			}>(harness, `${admin.token}`)
				.get(`/admin/audit-logs?admin_user_id=${admin.userId}&limit=50&offset=0`)
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.total).toBeGreaterThanOrEqual(1);
			for (const log of result.logs) {
				expect(log.admin_user_id).toBe(admin.userId);
			}
		});
		test('supports filtering by target_id', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'audit_log:view', 'user:update_acls', 'acl:set:user']);
			const targetUser = await createTestAccount(harness);
			await createBuilder(harness, `${admin.token}`)
				.put(`/admin/users/${targetUser.userId}/acls`)
				.body({acls: ['admin:authenticate']})
				.expect(HTTP_STATUS.OK)
				.execute();
			const result = await createBuilder<{
				logs: Array<{
					target_id: string;
					action: string;
				}>;
				total: number;
			}>(harness, `${admin.token}`)
				.get(`/admin/audit-logs?target_id=${targetUser.userId}&limit=50&offset=0`)
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.total).toBeGreaterThanOrEqual(1);
			for (const log of result.logs) {
				expect(log.target_id).toBe(targetUser.userId);
			}
		});
		test('supports full-text search by query', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'audit_log:view', 'user:update_acls', 'acl:set:user']);
			const targetUser = await createTestAccount(harness);
			await createBuilder(harness, `${admin.token}`)
				.put(`/admin/users/${targetUser.userId}/acls`)
				.body({acls: ['admin:authenticate']})
				.header('X-Audit-Log-Reason', 'unique-test-reason-xyz')
				.expect(HTTP_STATUS.OK)
				.execute();
			const result = await createBuilder<{
				logs: Array<{
					audit_log_reason: string | null;
				}>;
				total: number;
			}>(harness, `${admin.token}`)
				.get('/admin/audit-logs?q=set_acls&limit=50&offset=0')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result).toHaveProperty('logs');
			expect(result).toHaveProperty('total');
		});
		test('supports sort_by and sort_order parameters', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'audit_log:view']);
			const resultDesc = await createBuilder<{
				logs: Array<{
					created_at: string;
				}>;
				total: number;
			}>(harness, `${admin.token}`)
				.get('/admin/audit-logs?q=set_acls&limit=10&offset=0&sort_by=createdAt&sort_order=desc')
				.expect(HTTP_STATUS.OK)
				.execute();
			const resultAsc = await createBuilder<{
				logs: Array<{
					created_at: string;
				}>;
				total: number;
			}>(harness, `${admin.token}`)
				.get('/admin/audit-logs?q=set_acls&limit=10&offset=0&sort_by=createdAt&sort_order=asc')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(resultDesc).toHaveProperty('logs');
			expect(resultAsc).toHaveProperty('logs');
		});
	});
	describe('/admin/audit-logs/{log_id}', () => {
		test('requires audit_log:view ACL', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate']);
			await createBuilder(harness, `${admin.token}`)
				.get('/admin/audit-logs/999999999999999999')
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
		test('returns 404 for an unknown entry', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'audit_log:view']);
			await createBuilder(harness, `${admin.token}`)
				.get('/admin/audit-logs/999999999999999999')
				.expect(HTTP_STATUS.NOT_FOUND)
				.execute();
		});
	});
	describe('/admin/audit-logs (list)', () => {
		test('requires audit_log:view ACL', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate']);
			await createBuilder(harness, `${admin.token}`)
				.get('/admin/audit-logs?limit=10&offset=0')
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
		test('returns results with proper structure', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'audit_log:view']);
			const result = await createBuilder<{
				logs: Array<unknown>;
				total: number;
			}>(harness, `${admin.token}`)
				.get('/admin/audit-logs?limit=10&offset=0')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result).toHaveProperty('logs');
			expect(result).toHaveProperty('total');
			expect(Array.isArray(result.logs)).toBe(true);
		});
	});
	describe('POST /admin/search/indexes/{index_name}/refreshes', () => {
		test('rejects a channel_messages refresh without guild_id', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'guild:lookup']);
			const response = await createBuilder<{
				code: string;
				errors: Array<{
					path: string;
				}>;
			}>(harness, `${admin.token}`)
				.post('/admin/search/indexes/channel_messages/refreshes')
				.body({})
				.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
				.execute();
			expect(response.errors[0]?.path).toBe('guild_id');
		});
		test('rejects a guild_members refresh without guild_id', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'guild:lookup']);
			const response = await createBuilder<{
				code: string;
				errors: Array<{
					path: string;
				}>;
			}>(harness, `${admin.token}`)
				.post('/admin/search/indexes/guild_members/refreshes')
				.body({})
				.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
				.execute();
			expect(response.errors[0]?.path).toBe('guild_id');
		});
		test('rejects a favorite_memes refresh without user_id', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'guild:lookup']);
			const response = await createBuilder<{
				code: string;
				errors: Array<{
					path: string;
				}>;
			}>(harness, `${admin.token}`)
				.post('/admin/search/indexes/favorite_memes/refreshes')
				.body({})
				.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
				.execute();
			expect(response.errors[0]?.path).toBe('user_id');
		});
	});
});

describe('Admin Search Endpoints without a search backend', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness({search: 'disabled'});
	});
	afterEach(async () => {
		await harness.shutdown();
	});
	test('GET /admin/guilds answers 403 FEATURE_TEMPORARILY_DISABLED', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'guild:lookup']);
		await createBuilder(harness, `${admin.token}`)
			.get('/admin/guilds?q=test&limit=10&offset=0')
			.expect(HTTP_STATUS.FORBIDDEN, 'FEATURE_TEMPORARILY_DISABLED')
			.execute();
	});
	test('GET /admin/users with q answers 403 FEATURE_TEMPORARILY_DISABLED', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'user:lookup']);
		await createBuilder(harness, `${admin.token}`)
			.get('/admin/users?q=test&limit=10&offset=0')
			.expect(HTTP_STATUS.FORBIDDEN, 'FEATURE_TEMPORARILY_DISABLED')
			.execute();
	});
	test('GET /admin/reports answers 403 FEATURE_TEMPORARILY_DISABLED on the status branch', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'report:view']);
		await createBuilder(harness, `${admin.token}`)
			.get('/admin/reports?status=pending&limit=10&offset=0')
			.expect(HTTP_STATUS.FORBIDDEN, 'FEATURE_TEMPORARILY_DISABLED')
			.execute();
	});
	test('GET /admin/reports answers 403 FEATURE_TEMPORARILY_DISABLED on the search branch', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'report:view']);
		await createBuilder(harness, `${admin.token}`)
			.get('/admin/reports?q=example&limit=10&offset=0')
			.expect(HTTP_STATUS.FORBIDDEN, 'FEATURE_TEMPORARILY_DISABLED')
			.execute();
	});
});
