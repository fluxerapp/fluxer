// SPDX-License-Identifier: AGPL-3.0-or-later

import {createChannel, createRole, setupTestGuildWithMembers} from '@app/api/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import type {ChannelResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

interface AuditLogChange {
	key: string;
	old_value?: unknown;
	new_value?: unknown;
}

interface AuditLogOptions {
	channel_id?: string;
	id?: string;
	role_name?: string;
	type?: number;
}

interface AuditLogEntry {
	id: string;
	action_type: number;
	user_id: string | null;
	target_id: string | null;
	reason?: string;
	options?: AuditLogOptions;
	changes?: Array<AuditLogChange>;
}

interface AuditLogResponse {
	audit_log_entries: Array<AuditLogEntry>;
	users: Array<{
		id: string;
	}>;
}

async function fetchAuditLog(
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

function requireEntry(entries: Array<AuditLogEntry>, predicate: (entry: AuditLogEntry) => boolean): AuditLogEntry {
	const entry = entries.find(predicate);
	if (!entry) {
		throw new Error('Expected audit log entry was not recorded');
	}
	return entry;
}

function changeKeys(entry: AuditLogEntry): Array<string> {
	return (entry.changes ?? []).map((change) => change.key);
}

describe('Guild audit log channel writers', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	test('records a permissions update as overwrite entries with the header reason and no channel update', async () => {
		const {owner, guild} = await setupTestGuildWithMembers(harness, 0);
		const channel = await createChannel(harness, owner.token, guild.id, 'announcements');
		const role = await createRole(harness, owner.token, guild.id, {name: 'Posters'});
		const reason = 'Restrict posting';
		await createBuilder<ChannelResponse>(harness, owner.token)
			.patch(`/channels/${channel.id}`)
			.header('X-Audit-Log-Reason', reason)
			.body({
				permission_overwrites: [
					{id: role.id, type: 0, allow: Permissions.SEND_MESSAGES.toString(), deny: '0'},
					{id: guild.id, type: 0, allow: '0', deny: Permissions.SEND_MESSAGES.toString()},
				],
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		const updateLog = await fetchAuditLog(harness, owner.token, guild.id, AuditLogActionType.CHANNEL_UPDATE);
		expect(updateLog.audit_log_entries.filter((entry) => entry.target_id === channel.id)).toHaveLength(0);
		const overwriteLog = await fetchAuditLog(
			harness,
			owner.token,
			guild.id,
			AuditLogActionType.CHANNEL_OVERWRITE_CREATE,
		);
		const channelEntries = overwriteLog.audit_log_entries.filter((entry) => entry.options?.channel_id === channel.id);
		expect(channelEntries.map((entry) => entry.target_id).sort()).toEqual([guild.id, role.id].sort());
		for (const entry of channelEntries) {
			expect(entry.reason).toBe(reason);
		}
	});
	test('records a channel delete with the header reason', async () => {
		const {owner, guild} = await setupTestGuildWithMembers(harness, 0);
		const channel = await createChannel(harness, owner.token, guild.id, 'to-delete');
		const reason = 'No longer needed';
		await createBuilder(harness, owner.token)
			.delete(`/channels/${channel.id}`)
			.header('X-Audit-Log-Reason', reason)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		const log = await fetchAuditLog(harness, owner.token, guild.id, AuditLogActionType.CHANNEL_DELETE);
		const entry = requireEntry(log.audit_log_entries, (candidate) => candidate.target_id === channel.id);
		expect(entry.reason).toBe(reason);
		expect(entry.options?.type).toBe(ChannelTypes.GUILD_TEXT);
		expect(changeKeys(entry)).not.toContain('permission_overwrite_count');
	});
	test('records overwrite create, update and delete on the permissions route with the reason and role name', async () => {
		const {owner, guild} = await setupTestGuildWithMembers(harness, 0);
		const channel = await createChannel(harness, owner.token, guild.id, 'overrides');
		const role = await createRole(harness, owner.token, guild.id, {name: 'Moderators'});
		const sendOnly = Permissions.SEND_MESSAGES;
		const sendAndAttach = Permissions.SEND_MESSAGES | Permissions.ATTACH_FILES;
		const putOverwrite = async (allow: bigint, reason: string) => {
			await createBuilder(harness, owner.token)
				.put(`/channels/${channel.id}/permissions/${role.id}`)
				.header('X-Audit-Log-Reason', reason)
				.body({type: 0, allow: allow.toString(), deny: '0'})
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
		};
		await putOverwrite(sendOnly, 'Grant sending');
		await putOverwrite(sendAndAttach, 'Grant uploads');
		await putOverwrite(sendAndAttach, 'Repeat uploads');
		await createBuilder(harness, owner.token)
			.delete(`/channels/${channel.id}/permissions/${role.id}`)
			.header('X-Audit-Log-Reason', 'Drop override')
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		const isRoleOverwrite = (entry: AuditLogEntry) =>
			entry.target_id === role.id && entry.options?.channel_id === channel.id;
		const createEntries = (
			await fetchAuditLog(harness, owner.token, guild.id, AuditLogActionType.CHANNEL_OVERWRITE_CREATE)
		).audit_log_entries.filter(isRoleOverwrite);
		const updateEntries = (
			await fetchAuditLog(harness, owner.token, guild.id, AuditLogActionType.CHANNEL_OVERWRITE_UPDATE)
		).audit_log_entries.filter(isRoleOverwrite);
		const deleteEntries = (
			await fetchAuditLog(harness, owner.token, guild.id, AuditLogActionType.CHANNEL_OVERWRITE_DELETE)
		).audit_log_entries.filter(isRoleOverwrite);
		expect(createEntries).toHaveLength(1);
		expect(updateEntries).toHaveLength(1);
		expect(deleteEntries).toHaveLength(1);
		expect(createEntries[0]?.reason).toBe('Grant sending');
		expect(updateEntries[0]?.reason).toBe('Grant uploads');
		expect(deleteEntries[0]?.reason).toBe('Drop override');
		expect(updateEntries[0]?.changes).toEqual([
			{key: 'allow', old_value: sendOnly.toString(), new_value: sendAndAttach.toString()},
		]);
		for (const entry of [...createEntries, ...updateEntries, ...deleteEntries]) {
			expect(entry.user_id).toBe(owner.userId);
			expect(entry.options?.type).toBe(0);
			expect(entry.options?.role_name).toBe('Moderators');
		}
	});
});
