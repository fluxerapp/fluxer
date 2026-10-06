// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	addMemberRole,
	createChannel,
	createRole,
	removeMemberRole,
	setupTestGuildWithMembers,
} from '@app/api/guild/tests/GuildTestUtils';
import {getGatewayService} from '@app/api/middleware/ServiceRegistry';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {MentionReplyPreferences} from '@fluxer/constants/src/UserConstants';
import {afterEach, beforeEach, describe, expect, test, vi} from 'vitest';

interface AuditLogChange {
	key: string;
	old_value?: unknown;
	new_value?: unknown;
}

interface AuditLogEntry {
	id: string;
	action_type: number;
	user_id: string | null;
	target_id: string | null;
	reason?: string;
	options?: Record<string, unknown>;
	changes?: Array<AuditLogChange>;
}

interface AuditLogResponse {
	audit_log_entries: Array<AuditLogEntry>;
	users: Array<{
		id: string;
	}>;
}

async function listAuditLogs(
	harness: ApiTestHarness,
	token: string,
	guildId: string,
	actionType: AuditLogActionType,
): Promise<AuditLogResponse> {
	return createBuilder<AuditLogResponse>(harness, token)
		.get(`/guilds/${guildId}/audit-logs?action_type=${actionType}`)
		.expect(HTTP_STATUS.OK)
		.execute();
}

async function listTargetEntries(
	harness: ApiTestHarness,
	token: string,
	guildId: string,
	actionType: AuditLogActionType,
	targetId: string,
): Promise<Array<AuditLogEntry>> {
	const response = await listAuditLogs(harness, token, guildId, actionType);
	return response.audit_log_entries.filter((entry) => entry.target_id === targetId);
}

function findChange(entry: AuditLogEntry | undefined, key: string): AuditLogChange | undefined {
	return entry?.changes?.find((change) => change.key === key);
}

async function banMember(
	harness: ApiTestHarness,
	token: string,
	guildId: string,
	userId: string,
	body: Record<string, unknown>,
	headerReason?: string,
): Promise<void> {
	const builder = createBuilder(harness, token).put(`/guilds/${guildId}/bans/${userId}`).body(body);
	if (headerReason !== undefined) {
		builder.header('X-Audit-Log-Reason', headerReason);
	}
	await builder.expect(HTTP_STATUS.NO_CONTENT).execute();
}

async function patchMember(
	harness: ApiTestHarness,
	token: string,
	guildId: string,
	userId: string,
	body: Record<string, unknown>,
	headerReason?: string,
): Promise<void> {
	const builder = createBuilder(harness, token).patch(`/guilds/${guildId}/members/${userId}`).body(body);
	if (headerReason !== undefined) {
		builder.header('X-Audit-Log-Reason', headerReason);
	}
	await builder.expect(HTTP_STATUS.OK).execute();
}

function oneHourFromNow(): string {
	return new Date(Date.now() + 60 * 60 * 1000).toISOString();
}

describe('Guild audit log member and moderation writers', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});

	test('records a kick with the header reason', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0];
		await createBuilder(harness, owner.token)
			.delete(`/guilds/${guild.id}/members/${member.userId}`)
			.header('X-Audit-Log-Reason', 'Kick header reason')
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		const entries = await listTargetEntries(
			harness,
			owner.token,
			guild.id,
			AuditLogActionType.MEMBER_KICK,
			member.userId,
		);
		expect(entries).toHaveLength(1);
		expect(entries[0]?.user_id).toBe(owner.userId);
		expect(entries[0]?.reason).toBe('Kick header reason');
	});

	test('uses the body reason for a ban without a header and records no options', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0];
		await banMember(harness, owner.token, guild.id, member.userId, {reason: 'Body ban reason'});
		const entries = await listTargetEntries(
			harness,
			owner.token,
			guild.id,
			AuditLogActionType.MEMBER_BAN_ADD,
			member.userId,
		);
		expect(entries).toHaveLength(1);
		expect(entries[0]?.reason).toBe('Body ban reason');
		expect(entries[0]?.options).toBeUndefined();
		expect(findChange(entries[0], 'reason')?.new_value).toBe('Body ban reason');
	});

	test('records delete_message_seconds for a ban that deletes messages', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0];
		await banMember(harness, owner.token, guild.id, member.userId, {delete_message_seconds: 3600});
		const entries = await listTargetEntries(
			harness,
			owner.token,
			guild.id,
			AuditLogActionType.MEMBER_BAN_ADD,
			member.userId,
		);
		expect(entries).toHaveLength(1);
		expect(entries[0]?.options).toEqual({delete_message_seconds: 3600});
	});

	test('records delete_message_days as delete_message_seconds', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0];
		await banMember(harness, owner.token, guild.id, member.userId, {delete_message_days: 2});
		const entries = await listTargetEntries(
			harness,
			owner.token,
			guild.id,
			AuditLogActionType.MEMBER_BAN_ADD,
			member.userId,
		);
		expect(entries).toHaveLength(1);
		expect(entries[0]?.options).toEqual({delete_message_seconds: 172800});
	});

	test('keeps the header as the ban entry reason and the body reason in changes', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0];
		await banMember(harness, owner.token, guild.id, member.userId, {reason: 'Body ban reason'}, 'Header ban reason');
		const entries = await listTargetEntries(
			harness,
			owner.token,
			guild.id,
			AuditLogActionType.MEMBER_BAN_ADD,
			member.userId,
		);
		expect(entries).toHaveLength(1);
		expect(entries[0]?.reason).toBe('Header ban reason');
		expect(findChange(entries[0], 'reason')?.new_value).toBe('Body ban reason');
	});

	test('records expires_at for a temporary ban', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0];
		const bannedAfter = Date.now();
		await banMember(harness, owner.token, guild.id, member.userId, {ban_duration_seconds: 3600});
		const entries = await listTargetEntries(
			harness,
			owner.token,
			guild.id,
			AuditLogActionType.MEMBER_BAN_ADD,
			member.userId,
		);
		expect(entries).toHaveLength(1);
		const expiresAt = findChange(entries[0], 'expires_at')?.new_value;
		expect(typeof expiresAt).toBe('string');
		expect(Date.parse(expiresAt as string)).toBeGreaterThanOrEqual(bannedAfter + 3600 * 1000);
	});

	test('lists the original moderator when a different moderator unbans', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 2);
		const [moderator, target] = members;
		const moderatorRole = await createRole(harness, owner.token, guild.id, {
			name: 'Ban moderator',
			permissions: Permissions.BAN_MEMBERS.toString(),
		});
		await addMemberRole(harness, owner.token, guild.id, moderator.userId, moderatorRole.id);
		await banMember(harness, moderator.token, guild.id, target.userId, {});
		await createBuilder(harness, owner.token)
			.delete(`/guilds/${guild.id}/bans/${target.userId}`)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		const response = await listAuditLogs(harness, owner.token, guild.id, AuditLogActionType.MEMBER_BAN_REMOVE);
		const entry = response.audit_log_entries.find((log) => log.target_id === target.userId);
		expect(entry?.user_id).toBe(owner.userId);
		expect(findChange(entry, 'moderator_id')?.old_value).toBe(moderator.userId);
		expect(response.users.map((user) => user.id)).toContain(moderator.userId);
	});

	test('uses timeout_reason as the entry reason unless a header reason is sent', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 2);
		const [bodyOnlyTarget, headerTarget] = members;
		await patchMember(harness, owner.token, guild.id, bodyOnlyTarget.userId, {
			communication_disabled_until: oneHourFromNow(),
			timeout_reason: 'Timeout body reason',
		});
		await patchMember(
			harness,
			owner.token,
			guild.id,
			headerTarget.userId,
			{
				communication_disabled_until: oneHourFromNow(),
				timeout_reason: 'Timeout body reason',
			},
			'Timeout header reason',
		);
		const bodyOnlyEntries = await listTargetEntries(
			harness,
			owner.token,
			guild.id,
			AuditLogActionType.MEMBER_UPDATE,
			bodyOnlyTarget.userId,
		);
		expect(bodyOnlyEntries).toHaveLength(1);
		expect(bodyOnlyEntries[0]?.reason).toBe('Timeout body reason');
		expect(bodyOnlyEntries[0]?.options).toBeUndefined();
		expect(typeof findChange(bodyOnlyEntries[0], 'communication_disabled_until')?.new_value).toBe('string');
		const headerEntries = await listTargetEntries(
			harness,
			owner.token,
			guild.id,
			AuditLogActionType.MEMBER_UPDATE,
			headerTarget.userId,
		);
		expect(headerEntries).toHaveLength(1);
		expect(headerEntries[0]?.reason).toBe('Timeout header reason');
		expect(headerEntries[0]?.options).toBeUndefined();
	});

	test('writes nothing when clearing a timeout on a member without one', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0];
		await patchMember(harness, owner.token, guild.id, member.userId, {
			communication_disabled_until: null,
			timeout_reason: 'Nothing to clear',
		});
		const entries = await listTargetEntries(
			harness,
			owner.token,
			guild.id,
			AuditLogActionType.MEMBER_UPDATE,
			member.userId,
		);
		expect(entries).toHaveLength(0);
	});

	test('writes nothing for a mention_flags only update of the current member', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0];
		await createBuilder(harness, member.token)
			.patch(`/guilds/${guild.id}/members/@me`)
			.body({mention_flags: MentionReplyPreferences.PREFER_NO_MENTION})
			.expect(HTTP_STATUS.OK)
			.execute();
		const entries = await listTargetEntries(
			harness,
			owner.token,
			guild.id,
			AuditLogActionType.MEMBER_UPDATE,
			member.userId,
		);
		expect(entries).toHaveLength(0);
	});

	test('records a repeated nickname only once', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0];
		await patchMember(harness, owner.token, guild.id, member.userId, {nick: 'Audit Nick'});
		await patchMember(harness, owner.token, guild.id, member.userId, {nick: 'Audit Nick'});
		const entries = await listTargetEntries(
			harness,
			owner.token,
			guild.id,
			AuditLogActionType.MEMBER_UPDATE,
			member.userId,
		);
		expect(entries).toHaveLength(1);
		expect(findChange(entries[0], 'nick')).toEqual({key: 'nick', old_value: null, new_value: 'Audit Nick'});
	});

	test('records role_name for member role changes and skips adding a role the member has', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0];
		const role = await createRole(harness, owner.token, guild.id, {
			name: 'Audit Member Role',
			permissions: Permissions.VIEW_CHANNEL.toString(),
		});
		await addMemberRole(harness, owner.token, guild.id, member.userId, role.id);
		await addMemberRole(harness, owner.token, guild.id, member.userId, role.id);
		await removeMemberRole(harness, owner.token, guild.id, member.userId, role.id);
		const entries = await listTargetEntries(
			harness,
			owner.token,
			guild.id,
			AuditLogActionType.MEMBER_ROLE_UPDATE,
			member.userId,
		);
		expect(entries).toHaveLength(2);
		const [removeEntry, addEntry] = entries;
		expect(addEntry?.options).toEqual({role_name: 'Audit Member Role'});
		expect(findChange(addEntry, 'roles')).toEqual({key: 'roles', old_value: [], new_value: [role.id]});
		expect(removeEntry?.options).toEqual({role_name: 'Audit Member Role'});
		expect(findChange(removeEntry, 'roles')).toEqual({key: 'roles', old_value: [role.id], new_value: []});
	});

	test('writes nothing for a voice move into the channel the member is already in', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0];
		const firstChannel = await createChannel(harness, owner.token, guild.id, 'First voice', ChannelTypes.GUILD_VOICE);
		const secondChannel = await createChannel(harness, owner.token, guild.id, 'Second voice', ChannelTypes.GUILD_VOICE);
		const getVoiceState = vi.spyOn(getGatewayService(), 'getVoiceState');
		try {
			getVoiceState.mockResolvedValue({channel_id: firstChannel.id});
			await patchMember(harness, owner.token, guild.id, member.userId, {channel_id: secondChannel.id});
			getVoiceState.mockResolvedValue({channel_id: secondChannel.id});
			await patchMember(harness, owner.token, guild.id, member.userId, {channel_id: secondChannel.id});
		} finally {
			getVoiceState.mockRestore();
		}
		const moves = await listTargetEntries(
			harness,
			owner.token,
			guild.id,
			AuditLogActionType.MEMBER_MOVE,
			member.userId,
		);
		expect(moves).toHaveLength(1);
		expect(findChange(moves[0], 'channel_id')).toEqual({
			key: 'channel_id',
			old_value: firstChannel.id,
			new_value: secondChannel.id,
		});
		const updates = await listTargetEntries(
			harness,
			owner.token,
			guild.id,
			AuditLogActionType.MEMBER_UPDATE,
			member.userId,
		);
		expect(updates).toHaveLength(0);
	});
});
