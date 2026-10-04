// SPDX-License-Identifier: AGPL-3.0-or-later

import type {TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {addMemberRole, createRole, setupTestGuildWithMembers, updateRole} from '@app/api/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {DEFAULT_PERMISSIONS, Permissions} from '@fluxer/constants/src/ChannelConstants';
import type {GuildAuditLogListResponse} from '@fluxer/schema/src/domains/guild/GuildAuditLogSchemas';
import type {GuildEvent, GuildEventCreate} from '@fluxer/schema/src/domains/guild/GuildEventSchemas';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

const EVENT_ACTIONS = new Set<AuditLogActionType>([
	AuditLogActionType.GUILD_EVENT_CREATE,
	AuditLogActionType.GUILD_EVENT_UPDATE,
	AuditLogActionType.GUILD_EVENT_DELETE,
]);

function eventData(): GuildEventCreate {
	return {
		name: 'Community game night',
		description: 'Bring your favourite game',
		location: 'Gaming voice channel',
		starts_at: '2030-06-10T18:00:00.000Z',
		ends_at: '2030-06-10T20:00:00.000Z',
	};
}

async function createEvent(
	harness: ApiTestHarness,
	token: string,
	guildId: string,
	reason?: string,
): Promise<GuildEvent> {
	const builder = createBuilder<GuildEvent>(harness, token).post(`/guilds/${guildId}/events`).body(eventData());
	if (reason !== undefined) builder.header('X-Audit-Log-Reason', reason);
	return builder.expect(HTTP_STATUS.OK).execute();
}

async function listAuditLogs(
	harness: ApiTestHarness,
	token: string,
	guildId: string,
	actionType?: AuditLogActionType,
): Promise<GuildAuditLogListResponse> {
	const query = actionType === undefined ? '' : `?action_type=${actionType}`;
	return createBuilder<GuildAuditLogListResponse>(harness, token)
		.get(`/guilds/${guildId}/audit-logs${query}`)
		.expect(HTTP_STATUS.OK)
		.execute();
}

async function listEvents(harness: ApiTestHarness, token: string, guildId: string): Promise<Array<GuildEvent>> {
	return createBuilder<Array<GuildEvent>>(harness, token)
		.get(`/guilds/${guildId}/events`)
		.expect(HTTP_STATUS.OK)
		.execute();
}

async function grantManageEvents(
	harness: ApiTestHarness,
	owner: TestAccount,
	guildId: string,
	member: TestAccount,
): Promise<void> {
	const role = await createRole(harness, owner.token, guildId, {
		name: 'Event moderator',
		permissions: (Permissions.VIEW_CHANNEL | Permissions.MANAGE_EVENTS).toString(),
	});
	await addMemberRole(harness, owner.token, guildId, member.userId, role.id);
}

function eventEntries(response: GuildAuditLogListResponse) {
	return response.audit_log_entries.filter((entry) => EVENT_ACTIONS.has(entry.action_type));
}

describe('Guild audit log event writers', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});

	afterEach(async () => {
		await harness?.shutdown();
	});

	test('records the creator, event fields and decoded audit reason on create', async () => {
		const {owner, guild} = await setupTestGuildWithMembers(harness, 0);
		const reason = 'Café night approved';
		const headerReason = Buffer.from(reason, 'utf8').toString('latin1');
		const event = await createEvent(harness, owner.token, guild.id, headerReason);
		const response = await listAuditLogs(harness, owner.token, guild.id, AuditLogActionType.GUILD_EVENT_CREATE);
		const entries = response.audit_log_entries.filter((entry) => entry.target_id === event.id);
		expect(entries).toHaveLength(1);
		expect(entries[0]).toMatchObject({
			action_type: AuditLogActionType.GUILD_EVENT_CREATE,
			user_id: owner.userId,
			target_id: event.id,
			reason,
		});
		expect(entries[0]?.changes).toEqual(
			expect.arrayContaining([
				{key: 'name', new_value: event.name},
				{key: 'starts_at', new_value: event.starts_at},
			]),
		);
	});

	test('attributes a manager edit to the editor while preserving the event creator', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const manager = members[0]!;
		await grantManageEvents(harness, owner, guild.id, manager);
		const event = await createEvent(harness, owner.token, guild.id);
		const reason = 'Move the session later';
		const updated = await createBuilder<GuildEvent>(harness, manager.token)
			.patch(`/guilds/${guild.id}/events/${event.id}`)
			.header('X-Audit-Log-Reason', reason)
			.body({name: 'Late game night', starts_at: '2030-06-10T19:00:00.000Z'})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(updated.creator_id).toBe(owner.userId);
		const response = await listAuditLogs(harness, owner.token, guild.id, AuditLogActionType.GUILD_EVENT_UPDATE);
		const entries = response.audit_log_entries.filter((entry) => entry.target_id === event.id);
		expect(entries).toHaveLength(1);
		expect(entries[0]).toMatchObject({user_id: manager.userId, target_id: event.id, reason});
		expect(entries[0]?.changes).toEqual(
			expect.arrayContaining([
				{key: 'name', old_value: event.name, new_value: updated.name},
				{key: 'starts_at', old_value: event.starts_at, new_value: updated.starts_at},
			]),
		);
		expect(entries[0]?.changes).toHaveLength(2);
	});

	test('does not record an update that leaves all supplied event fields unchanged', async () => {
		const {owner, guild} = await setupTestGuildWithMembers(harness, 0);
		const event = await createEvent(harness, owner.token, guild.id);
		await createBuilder<GuildEvent>(harness, owner.token)
			.patch(`/guilds/${guild.id}/events/${event.id}`)
			.header('X-Audit-Log-Reason', 'Nothing changed')
			.body({
				name: event.name,
				description: event.description,
				location: event.location,
				starts_at: event.starts_at,
				ends_at: event.ends_at,
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		const response = await listAuditLogs(harness, owner.token, guild.id, AuditLogActionType.GUILD_EVENT_UPDATE);
		expect(response.audit_log_entries.filter((entry) => entry.target_id === event.id)).toHaveLength(0);
	});

	test('retains the event creator in a deletion audit when a different user deletes it', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const creator = members[0]!;
		const event = await createEvent(harness, creator.token, guild.id);
		await createBuilder(harness, owner.token)
			.delete(`/guilds/${guild.id}/events/${event.id}`)
			.header('X-Audit-Log-Reason', 'The event was cancelled')
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		expect((await listEvents(harness, owner.token, guild.id)).some((entry) => entry.id === event.id)).toBe(false);
		const response = await listAuditLogs(harness, owner.token, guild.id, AuditLogActionType.GUILD_EVENT_DELETE);
		const entries = response.audit_log_entries.filter((entry) => entry.target_id === event.id);
		expect(entries).toHaveLength(1);
		expect(entries[0]).toMatchObject({
			user_id: owner.userId,
			target_id: event.id,
			reason: 'The event was cancelled',
		});
		expect(entries[0]?.changes).toEqual(expect.arrayContaining([{key: 'name', old_value: event.name}]));
		expect(response.users.map((user) => user.id)).toContain(creator.userId);
	});

	test('records no event audit and changes no event for unauthorized mutations', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0]!;
		await updateRole(harness, owner.token, guild.id, guild.id, {
			permissions: (DEFAULT_PERMISSIONS & ~Permissions.CREATE_EVENTS & ~Permissions.MANAGE_EVENTS).toString(),
		});
		const event = await createEvent(harness, owner.token, guild.id);
		const before = eventEntries(await listAuditLogs(harness, owner.token, guild.id));
		await createBuilder(harness, member.token)
			.post(`/guilds/${guild.id}/events`)
			.body(eventData())
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
		await createBuilder(harness, member.token)
			.patch(`/guilds/${guild.id}/events/${event.id}`)
			.body({name: 'Unauthorized rename'})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
		await createBuilder(harness, member.token)
			.delete(`/guilds/${guild.id}/events/${event.id}`)
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
		expect(eventEntries(await listAuditLogs(harness, owner.token, guild.id))).toEqual(before);
		expect(await listEvents(harness, owner.token, guild.id)).toEqual([event]);
	});

	test('records no event audit for invalid create dates or an invalid partial date update', async () => {
		const {owner, guild} = await setupTestGuildWithMembers(harness, 0);
		const event = await createEvent(harness, owner.token, guild.id);
		const before = eventEntries(await listAuditLogs(harness, owner.token, guild.id));
		await createBuilder(harness, owner.token)
			.post(`/guilds/${guild.id}/events`)
			.body({...eventData(), ends_at: event.starts_at})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
		await createBuilder(harness, owner.token)
			.patch(`/guilds/${guild.id}/events/${event.id}`)
			.body({starts_at: '2030-06-10T21:00:00.000Z'})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
		expect(eventEntries(await listAuditLogs(harness, owner.token, guild.id))).toEqual(before);
		expect(await listEvents(harness, owner.token, guild.id)).toEqual([event]);
	});
});
