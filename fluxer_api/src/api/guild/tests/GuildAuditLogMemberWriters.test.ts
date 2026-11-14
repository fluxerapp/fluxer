// SPDX-License-Identifier: AGPL-3.0-or-later

import {addMemberRole, createRole, setupTestGuildWithMembers} from '@app/api/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

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
});
