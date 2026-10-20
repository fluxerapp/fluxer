// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {setupTestGuildWithMembers} from '@app/api/guild/tests/GuildTestUtils';
import {getAdminRepository} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopGatewayService} from '@app/api/test/NoopGatewayService';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {afterEach, beforeEach, describe, expect, test, vi} from 'vitest';

const SYSTEM_USER_ID = '0';
const STAFF_REASON = 'Staff ticket 4410: member is under 18';

interface GuildAuditLogResponse {
	audit_log_entries: Array<{action_type: number; user_id: string | null; target_id: string | null; reason?: string}>;
	users: Array<{id: string}>;
}

interface GuildBanEntry {
	user: {id: string};
	reason: string | null;
	moderator_id: string;
}

async function createStaff(harness: ApiTestHarness, acls: Array<string>): Promise<TestAccount> {
	return setUserACLs(harness, await createTestAccount(harness), [AdminACLs.AUTHENTICATE, ...acls]);
}

async function listGuildAuditLog(
	harness: ApiTestHarness,
	owner: TestAccount,
	guildId: string,
	actionType: AuditLogActionType,
): Promise<GuildAuditLogResponse> {
	return createBuilder<GuildAuditLogResponse>(harness, owner.token)
		.get(`/guilds/${guildId}/audit-logs?action_type=${actionType}`)
		.expect(HTTP_STATUS.OK)
		.execute();
}

async function listBans(harness: ApiTestHarness, owner: TestAccount, guildId: string): Promise<Array<GuildBanEntry>> {
	return createBuilder<Array<GuildBanEntry>>(harness, owner.token)
		.get(`/guilds/${guildId}/bans`)
		.expect(HTTP_STATUS.OK)
		.execute();
}

async function latestAdminAuditLog(action: string, targetId: string) {
	const logs = await getAdminRepository().listAllAuditLogsPaginated(100000);
	return logs.find((log) => log.action === action && log.targetId.toString() === targetId);
}

function captureGuildDispatches() {
	const events: Array<{event: string; data: unknown}> = [];
	vi.spyOn(NoopGatewayService.prototype, 'dispatchGuild').mockImplementation(async (params) => {
		events.push({event: params.event, data: params.data});
	});
	return events;
}

describe('Admin guild moderation as the system user', () => {
	let harness: ApiTestHarness;

	beforeEach(async () => {
		harness = await createApiTestHarness();
	});

	afterEach(async () => {
		vi.restoreAllMocks();
		await harness?.shutdown();
	});

	test('kick works without guild membership and is attributed to the system user', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const target = members[0]!;
		const staff = await createStaff(harness, [AdminACLs.GUILD_KICK_MEMBER]);
		const dispatches = captureGuildDispatches();

		await createBuilder(harness, staff.token)
			.delete(`/admin/guilds/${guild.id}/members/${target.userId}`)
			.header('X-Audit-Log-Reason', STAFF_REASON)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();

		await createBuilder(harness, owner.token)
			.get(`/guilds/${guild.id}/members/${target.userId}`)
			.expect(HTTP_STATUS.NOT_FOUND)
			.execute();

		const auditLog = await listGuildAuditLog(harness, owner, guild.id, AuditLogActionType.MEMBER_KICK);
		const entry = auditLog.audit_log_entries.find((candidate) => candidate.target_id === target.userId);
		expect(entry?.user_id).toBe(SYSTEM_USER_ID);
		expect(entry?.reason).toBeUndefined();
		expect(auditLog.users.map((user) => user.id)).toContain(SYSTEM_USER_ID);
		expect(JSON.stringify(auditLog)).not.toContain(staff.userId);

		expect(dispatches.map((dispatch) => dispatch.event)).toContain('GUILD_MEMBER_REMOVE');
		expect(dispatches.find((dispatch) => dispatch.event === 'GUILD_AUDIT_LOG_ENTRY_CREATE')?.data).toMatchObject({
			user_id: SYSTEM_USER_ID,
		});
		expect(JSON.stringify(dispatches)).not.toContain(staff.userId);
		expect(JSON.stringify(dispatches)).not.toContain(STAFF_REASON);

		const adminLog = await latestAdminAuditLog('kick_member', target.userId);
		expect(adminLog?.adminUserId.toString()).toBe(staff.userId);
		expect(adminLog?.auditLogReason).toBe(STAFF_REASON);
		expect(Object.fromEntries(adminLog?.metadata ?? [])).toEqual({
			guild_id: guild.id,
			user_id: target.userId,
			acl: AdminACLs.GUILD_KICK_MEMBER,
		});
	});

	test('ban works without guild membership and the ban record names the system user', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const target = members[0]!;
		const staff = await createStaff(harness, [AdminACLs.GUILD_BAN_MEMBER]);
		const dispatches = captureGuildDispatches();

		await createBuilder(harness, staff.token)
			.put(`/admin/guilds/${guild.id}/bans/${target.userId}`)
			.header('X-Audit-Log-Reason', STAFF_REASON)
			.body({reason: 'Underage member'})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();

		const bans = await listBans(harness, owner, guild.id);
		expect(bans).toHaveLength(1);
		expect(bans[0]).toMatchObject({user: {id: target.userId}, moderator_id: SYSTEM_USER_ID, reason: 'Underage member'});

		const auditLog = await listGuildAuditLog(harness, owner, guild.id, AuditLogActionType.MEMBER_BAN_ADD);
		const entry = auditLog.audit_log_entries.find((candidate) => candidate.target_id === target.userId);
		expect(entry?.user_id).toBe(SYSTEM_USER_ID);
		expect(entry?.reason).toBe('Underage member');
		expect(JSON.stringify(auditLog)).not.toContain(staff.userId);

		expect(dispatches.map((dispatch) => dispatch.event)).toEqual(
			expect.arrayContaining(['GUILD_BAN_ADD', 'GUILD_MEMBER_REMOVE', 'GUILD_AUDIT_LOG_ENTRY_CREATE']),
		);
		expect(JSON.stringify(dispatches)).not.toContain(staff.userId);
		expect(JSON.stringify(dispatches)).not.toContain(STAFF_REASON);

		const adminLog = await latestAdminAuditLog('ban_member', target.userId);
		expect(adminLog?.adminUserId.toString()).toBe(staff.userId);
		expect(adminLog?.auditLogReason).toBe(STAFF_REASON);
		expect(adminLog?.metadata.get('acl')).toBe(AdminACLs.GUILD_BAN_MEMBER);
		expect(adminLog?.metadata.get('reason')).toBe('Underage member');
	});

	test('ban without a reason stores no reason', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const target = members[0]!;
		const staff = await createStaff(harness, [AdminACLs.GUILD_BAN_MEMBER]);

		await createBuilder(harness, staff.token)
			.put(`/admin/guilds/${guild.id}/bans/${target.userId}`)
			.header('X-Audit-Log-Reason', STAFF_REASON)
			.body({})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();

		const bans = await listBans(harness, owner, guild.id);
		expect(bans[0]).toMatchObject({moderator_id: SYSTEM_USER_ID, reason: null});
		const auditLog = await listGuildAuditLog(harness, owner, guild.id, AuditLogActionType.MEMBER_BAN_ADD);
		expect(auditLog.audit_log_entries[0]?.reason).toBeUndefined();
	});

	test('unban removes the ban as the system user', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const target = members[0]!;
		const staff = await createStaff(harness, [AdminACLs.GUILD_BAN_MEMBER]);
		await createBuilder(harness, staff.token)
			.put(`/admin/guilds/${guild.id}/bans/${target.userId}`)
			.body({})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		const dispatches = captureGuildDispatches();

		await createBuilder(harness, staff.token)
			.delete(`/admin/guilds/${guild.id}/bans/${target.userId}`)
			.header('X-Audit-Log-Reason', STAFF_REASON)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();

		expect(await listBans(harness, owner, guild.id)).toEqual([]);
		const auditLog = await listGuildAuditLog(harness, owner, guild.id, AuditLogActionType.MEMBER_BAN_REMOVE);
		expect(auditLog.audit_log_entries[0]).toMatchObject({user_id: SYSTEM_USER_ID, target_id: target.userId});
		expect(auditLog.audit_log_entries[0]?.reason).toBeUndefined();
		expect(dispatches.map((dispatch) => dispatch.event)).toContain('GUILD_BAN_REMOVE');
		expect(JSON.stringify(dispatches)).not.toContain(staff.userId);

		const adminLog = await latestAdminAuditLog('unban_member', target.userId);
		expect(adminLog?.adminUserId.toString()).toBe(staff.userId);
		expect(adminLog?.auditLogReason).toBe(STAFF_REASON);
		expect(adminLog?.metadata.get('acl')).toBe(AdminACLs.GUILD_BAN_MEMBER);

		await createBuilder(harness, staff.token)
			.delete(`/admin/guilds/${guild.id}/bans/${target.userId}`)
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});

	test('kicking or banning the guild owner is refused', async () => {
		const {owner, guild} = await setupTestGuildWithMembers(harness, 0);
		const staff = await createStaff(harness, [AdminACLs.GUILD_KICK_MEMBER, AdminACLs.GUILD_BAN_MEMBER]);

		await createBuilder(harness, staff.token)
			.delete(`/admin/guilds/${guild.id}/members/${owner.userId}`)
			.expect(HTTP_STATUS.FORBIDDEN, 'MISSING_PERMISSIONS')
			.execute();
		await createBuilder(harness, staff.token)
			.put(`/admin/guilds/${guild.id}/bans/${owner.userId}`)
			.body({})
			.expect(HTTP_STATUS.FORBIDDEN, 'MISSING_PERMISSIONS')
			.execute();

		expect(await listBans(harness, owner, guild.id)).toEqual([]);
		await createBuilder(harness, owner.token)
			.get(`/guilds/${guild.id}/members/${owner.userId}`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(await latestAdminAuditLog('kick_member', owner.userId)).toBeUndefined();
		expect(await latestAdminAuditLog('ban_member', owner.userId)).toBeUndefined();
	});

	test('unknown guilds are reported as unknown', async () => {
		const target = await createTestAccount(harness);
		const staff = await createStaff(harness, [AdminACLs.GUILD_KICK_MEMBER, AdminACLs.GUILD_BAN_MEMBER]);
		const missingGuildId = '900000000000000001';

		await createBuilder(harness, staff.token)
			.delete(`/admin/guilds/${missingGuildId}/members/${target.userId}`)
			.expect(HTTP_STATUS.NOT_FOUND, 'UNKNOWN_GUILD')
			.execute();
		await createBuilder(harness, staff.token)
			.put(`/admin/guilds/${missingGuildId}/bans/${target.userId}`)
			.body({})
			.expect(HTTP_STATUS.NOT_FOUND, 'UNKNOWN_GUILD')
			.execute();
		await createBuilder(harness, staff.token)
			.delete(`/admin/guilds/${missingGuildId}/bans/${target.userId}`)
			.expect(HTTP_STATUS.NOT_FOUND, 'UNKNOWN_GUILD')
			.execute();
	});

	test('each route still requires its Admin ACL', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const target = members[0]!;
		const kicker = await createStaff(harness, [AdminACLs.GUILD_KICK_MEMBER]);
		const banner = await createStaff(harness, [AdminACLs.GUILD_BAN_MEMBER]);

		await createBuilder(harness, banner.token)
			.delete(`/admin/guilds/${guild.id}/members/${target.userId}`)
			.expect(HTTP_STATUS.FORBIDDEN, 'MISSING_ACL')
			.execute();
		await createBuilder(harness, kicker.token)
			.put(`/admin/guilds/${guild.id}/bans/${target.userId}`)
			.body({})
			.expect(HTTP_STATUS.FORBIDDEN, 'MISSING_ACL')
			.execute();
		await createBuilder(harness, kicker.token)
			.delete(`/admin/guilds/${guild.id}/bans/${target.userId}`)
			.expect(HTTP_STATUS.FORBIDDEN, 'MISSING_ACL')
			.execute();

		expect(await listBans(harness, owner, guild.id)).toEqual([]);
		await createBuilder(harness, owner.token)
			.get(`/guilds/${guild.id}/members/${target.userId}`)
			.expect(HTTP_STATUS.OK)
			.execute();
	});
});
