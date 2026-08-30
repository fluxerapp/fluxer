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
	describe('/admin/users/search', () => {
		test('requires user:lookup ACL', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate']);
			await createBuilder(harness, `${admin.token}`)
				.post('/admin/users/search')
				.body({query: 'test', limit: 10, offset: 0})
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
				.post('/admin/users/search')
				.body({query: 'nonexistent-user-query-xyz', limit: 10, offset: 0})
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
				.post('/admin/users/search')
				.body({query: targetUser.username, limit: 10, offset: 0})
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
				.post('/admin/users/search')
				.body({limit: 1, offset: 0})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.users.length).toBeLessThanOrEqual(1);
		});
		test('supports searching by last active IP', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'user:lookup']);
			const targetUser = await createTestAccount(harness);
			await setLastActiveIp(harness, targetUser.token, '198.51.100.91');
			const result = await createBuilder<{
				users: Array<{
					id: string;
				}>;
				total: number;
			}>(harness, `${admin.token}`)
				.post('/admin/users/search')
				.body({last_active_ip: '198.51.100.91', limit: 10, offset: 0})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.total).toBeGreaterThanOrEqual(1);
			expect(result.users.find((user) => user.id === targetUser.userId)).toBeDefined();
		});
	});
	describe('/admin/users/list-dm-channels', () => {
		test('requires user:list:dm_channels ACL', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate']);
			await createBuilder(harness, `${admin.token}`)
				.post('/admin/users/list-dm-channels')
				.body({user_id: admin.userId, limit: 10})
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
				.post('/admin/users/list-dm-channels')
				.body({user_id: subjectUser.userId, limit: 2})
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
				.post('/admin/users/list-dm-channels')
				.body({user_id: subjectUser.userId, limit: 2, before: firstPage.channels[1]!.channel_id})
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
				.post('/admin/users/list-dm-channels')
				.body({user_id: subjectUser.userId, limit: 2, after: secondPage.channels[0]!.channel_id})
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
				.post('/admin/users/list-dm-channels')
				.body({user_id: admin.userId, limit: 10, before: '1', after: '2'})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
	});
	describe('/admin/guilds/search', () => {
		test('requires guild:lookup ACL', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate']);
			await createBuilder(harness, `${admin.token}`)
				.post('/admin/guilds/search')
				.body({query: 'test', limit: 10, offset: 0})
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
				.post('/admin/guilds/search')
				.body({query: 'nonexistent-guild-query-xyz', limit: 10, offset: 0})
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
				.post('/admin/guilds/search')
				.body({query: guildName, limit: 10, offset: 0})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.total).toBeGreaterThanOrEqual(1);
			const foundGuild = result.guilds.find((g) => g.id === guild.id);
			expect(foundGuild).toBeDefined();
			expect(foundGuild?.name).toBe(guildName);
		});
	});
	describe('/admin/reports/search', () => {
		test('requires report:view ACL', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate']);
			await createBuilder(harness, `${admin.token}`)
				.post('/admin/reports/search')
				.body({limit: 10, offset: 0})
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
		test('returns report list response', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate', 'report:view']);
			const result = await createBuilder<{
				reports: Array<unknown>;
				total: number;
			}>(harness, `${admin.token}`)
				.post('/admin/reports/search')
				.body({limit: 10, offset: 0})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(Array.isArray(result.reports)).toBe(true);
			expect(result.total).toBeGreaterThanOrEqual(result.reports.length);
		});
	});
	describe('/admin/audit-logs/search', () => {
		test('requires audit_log:view ACL', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate']);
			await createBuilder(harness, `${admin.token}`)
				.post('/admin/audit-logs/search')
				.body({limit: 10, offset: 0})
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
				.post('/admin/audit-logs/search')
				.body({limit: 10, offset: 0})
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
				.post('/admin/users/set-acls')
				.body({user_id: targetUser.userId, acls: ['admin:authenticate']})
				.expect(HTTP_STATUS.OK)
				.execute();
			const result = await createBuilder<{
				logs: Array<{
					admin_user_id: string;
					action: string;
				}>;
				total: number;
			}>(harness, `${admin.token}`)
				.post('/admin/audit-logs/search')
				.body({admin_user_id: admin.userId, limit: 50, offset: 0})
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
				.post('/admin/users/set-acls')
				.body({user_id: targetUser.userId, acls: ['admin:authenticate']})
				.expect(HTTP_STATUS.OK)
				.execute();
			const result = await createBuilder<{
				logs: Array<{
					target_id: string;
					action: string;
				}>;
				total: number;
			}>(harness, `${admin.token}`)
				.post('/admin/audit-logs/search')
				.body({target_id: targetUser.userId, limit: 50, offset: 0})
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
				.post('/admin/users/set-acls')
				.body({user_id: targetUser.userId, acls: ['admin:authenticate']})
				.header('X-Audit-Log-Reason', 'unique-test-reason-xyz')
				.expect(HTTP_STATUS.OK)
				.execute();
			const result = await createBuilder<{
				logs: Array<{
					audit_log_reason: string | null;
				}>;
				total: number;
			}>(harness, `${admin.token}`)
				.post('/admin/audit-logs/search')
				.body({query: 'set_acls', limit: 50, offset: 0})
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
				.post('/admin/audit-logs/search')
				.body({limit: 10, offset: 0, sort_by: 'createdAt', sort_order: 'desc'})
				.expect(HTTP_STATUS.OK)
				.execute();
			const resultAsc = await createBuilder<{
				logs: Array<{
					created_at: string;
				}>;
				total: number;
			}>(harness, `${admin.token}`)
				.post('/admin/audit-logs/search')
				.body({limit: 10, offset: 0, sort_by: 'createdAt', sort_order: 'asc'})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(resultDesc).toHaveProperty('logs');
			expect(resultAsc).toHaveProperty('logs');
		});
	});
	describe('/admin/audit-logs (list)', () => {
		test('requires audit_log:view ACL', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, ['admin:authenticate']);
			await createBuilder(harness, `${admin.token}`)
				.post('/admin/audit-logs')
				.body({limit: 10, offset: 0})
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
				.post('/admin/audit-logs')
				.body({limit: 10, offset: 0})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result).toHaveProperty('logs');
			expect(result).toHaveProperty('total');
			expect(Array.isArray(result.logs)).toBe(true);
		});
	});
});
