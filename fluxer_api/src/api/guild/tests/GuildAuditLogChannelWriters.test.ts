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
	test('records a voice channel created under a category and then its overwrite with the header reason', async () => {
		const {owner, guild} = await setupTestGuildWithMembers(harness, 0);
		const role = await createRole(harness, owner.token, guild.id, {name: 'Speakers'});
		const category = await createChannel(harness, owner.token, guild.id, 'Voice', ChannelTypes.GUILD_CATEGORY);
		const reason = 'Set up the lounge';
		const voice = await createBuilder<ChannelResponse>(harness, owner.token)
			.post(`/guilds/${guild.id}/channels`)
			.header('X-Audit-Log-Reason', reason)
			.body({
				name: 'Lounge',
				type: ChannelTypes.GUILD_VOICE,
				parent_id: category.id,
				permission_overwrites: [{id: role.id, type: 0, allow: Permissions.CONNECT.toString(), deny: '0'}],
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		const createLog = await fetchAuditLog(harness, owner.token, guild.id, AuditLogActionType.CHANNEL_CREATE);
		const createEntry = requireEntry(createLog.audit_log_entries, (entry) => entry.target_id === voice.id);
		expect(createEntry.reason).toBe(reason);
		expect(createEntry.changes?.find((change) => change.key === 'parent_id')?.new_value).toBe(category.id);
		expect(changeKeys(createEntry)).not.toContain('permission_overwrite_count');
		const overwriteLog = await fetchAuditLog(
			harness,
			owner.token,
			guild.id,
			AuditLogActionType.CHANNEL_OVERWRITE_CREATE,
		);
		const overwriteEntry = requireEntry(
			overwriteLog.audit_log_entries,
			(entry) => entry.target_id === role.id && entry.options?.channel_id === voice.id,
		);
		expect(overwriteEntry.reason).toBe(reason);
		expect(BigInt(overwriteEntry.id) > BigInt(createEntry.id)).toBe(true);
	});
	test('records the url of a link channel on create and on a url-only update', async () => {
		const {owner, guild} = await setupTestGuildWithMembers(harness, 0);
		const link = await createBuilder<ChannelResponse>(harness, owner.token)
			.post(`/guilds/${guild.id}/channels`)
			.body({name: 'docs', type: ChannelTypes.GUILD_LINK, url: 'https://fluxer.app/docs'})
			.expect(HTTP_STATUS.OK)
			.execute();
		const createLog = await fetchAuditLog(harness, owner.token, guild.id, AuditLogActionType.CHANNEL_CREATE);
		const createEntry = requireEntry(createLog.audit_log_entries, (entry) => entry.target_id === link.id);
		expect(createEntry.changes?.find((change) => change.key === 'url')?.new_value).toBe('https://fluxer.app/docs');
		await createBuilder<ChannelResponse>(harness, owner.token)
			.patch(`/channels/${link.id}`)
			.body({url: 'https://fluxer.app/help'})
			.expect(HTTP_STATUS.OK)
			.execute();
		const updateLog = await fetchAuditLog(harness, owner.token, guild.id, AuditLogActionType.CHANNEL_UPDATE);
		const updateEntry = requireEntry(updateLog.audit_log_entries, (entry) => entry.target_id === link.id);
		expect(updateEntry.options?.type).toBe(ChannelTypes.GUILD_LINK);
		expect(updateEntry.changes).toEqual([
			{key: 'url', old_value: 'https://fluxer.app/docs', new_value: 'https://fluxer.app/help'},
		]);
	});
	test('records a channel rename with the header reason', async () => {
		const {owner, guild} = await setupTestGuildWithMembers(harness, 0);
		const channel = await createChannel(harness, owner.token, guild.id, 'before-rename');
		const reason = 'Clearer name';
		await createBuilder<ChannelResponse>(harness, owner.token)
			.patch(`/channels/${channel.id}`)
			.header('X-Audit-Log-Reason', reason)
			.body({name: 'after-rename'})
			.expect(HTTP_STATUS.OK)
			.execute();
		const log = await fetchAuditLog(harness, owner.token, guild.id, AuditLogActionType.CHANNEL_UPDATE);
		const entry = requireEntry(log.audit_log_entries, (candidate) => candidate.target_id === channel.id);
		expect(entry.user_id).toBe(owner.userId);
		expect(entry.reason).toBe(reason);
		expect(entry.changes).toEqual([{key: 'name', old_value: 'before-rename', new_value: 'after-rename'}]);
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
	test('lists a member overwrite target in users', async () => {
		const {owner, members, guild, channels} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0];
		const channel = channels[0];
		await createBuilder(harness, owner.token)
			.put(`/channels/${channel.id}/permissions/${member.userId}`)
			.body({type: 1, allow: Permissions.ATTACH_FILES.toString(), deny: '0'})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		const log = await fetchAuditLog(harness, owner.token, guild.id, AuditLogActionType.CHANNEL_OVERWRITE_CREATE);
		const entry = requireEntry(log.audit_log_entries, (candidate) => candidate.target_id === member.userId);
		expect(entry.options?.type).toBe(1);
		expect(entry.options?.role_name).toBeUndefined();
		expect(log.users.map((user) => user.id)).toContain(member.userId);
	});
	test('writes nothing for a channel update without an effective change', async () => {
		const {owner, guild} = await setupTestGuildWithMembers(harness, 0);
		const channel = await createChannel(harness, owner.token, guild.id, 'steady-name');
		await createBuilder<ChannelResponse>(harness, owner.token)
			.patch(`/channels/${channel.id}`)
			.header('X-Audit-Log-Reason', 'Nothing to change')
			.body({name: 'steady-name', topic: null})
			.expect(HTTP_STATUS.OK)
			.execute();
		const log = await fetchAuditLog(harness, owner.token, guild.id, AuditLogActionType.CHANNEL_UPDATE);
		expect(log.audit_log_entries.filter((entry) => entry.target_id === channel.id)).toHaveLength(0);
	});
});
