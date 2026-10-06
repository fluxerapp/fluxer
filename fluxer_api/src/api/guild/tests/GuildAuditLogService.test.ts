// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createChannelID,
	createGuildID,
	createRoleID,
	createUserID,
	type RoleID,
	type UserID,
} from '@app/api/BrandedTypes';
import type {GuildAuditLogRow} from '@app/api/database/types/GuildTypes';
import {mapGuildAuditLogEntry} from '@app/api/guild/GuildAuditLogEntryMapper';
import {GuildAuditLogService} from '@app/api/guild/GuildAuditLogService';
import type {IGuildRepositoryAggregate} from '@app/api/guild/repositories/IGuildRepositoryAggregate';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {ISnowflakeService} from '@app/api/infrastructure/ISnowflakeService';
import {ChannelPermissionOverwrite} from '@app/api/models/ChannelPermissionOverwrite';
import {GuildAuditLog} from '@app/api/models/GuildAuditLog';
import type {GuildRole} from '@app/api/models/GuildRole';
import type {WorkerTaskName} from '@app/api/worker/WorkerLaneConfig';
import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import type {IWorkerService} from '@pkgs/worker/src/contracts/IWorkerService';
import {describe, expect, it, vi} from 'vitest';

const GUILD_ID = createGuildID(1420000000000000000n);
const EVERYONE_ROLE_ID = createRoleID(1420000000000000000n);
const ACTOR_ID = createUserID(1420000000000000001n);
const MEMBER_ID = createUserID(1420000000000000002n);
const ROLE_ID = createRoleID(1420000000000000003n);
const CHANNEL_ID = createChannelID(1420000000000000004n);

const SKIPPABLE_ACTIONS = [
	AuditLogActionType.GUILD_UPDATE,
	AuditLogActionType.CHANNEL_UPDATE,
	AuditLogActionType.CHANNEL_OVERWRITE_UPDATE,
	AuditLogActionType.MEMBER_UPDATE,
	AuditLogActionType.MEMBER_ROLE_UPDATE,
	AuditLogActionType.MEMBER_MOVE,
	AuditLogActionType.ROLE_UPDATE,
	AuditLogActionType.WEBHOOK_UPDATE,
	AuditLogActionType.EMOJI_UPDATE,
	AuditLogActionType.STICKER_UPDATE,
];

function createService(roleNames: Map<string, string> = new Map()) {
	let nextLogId = 1420000000000001000n;
	const createAuditLog = vi.fn(async (row: GuildAuditLogRow) => new GuildAuditLog(row));
	const batchDeleteAndCreateAuditLogs = vi.fn(
		async (_guildId: unknown, _logs: Array<GuildAuditLog>, row: GuildAuditLogRow) => new GuildAuditLog(row),
	);
	const getRole = vi.fn(async (roleId: RoleID) => {
		const name = roleNames.get(roleId.toString());
		return name === undefined ? null : ({id: roleId, name} as unknown as GuildRole);
	});
	const dispatchGuild = vi.fn().mockResolvedValue(undefined);
	const addJob = vi.fn().mockResolvedValue(undefined);
	const service = new GuildAuditLogService(
		{createAuditLog, batchDeleteAndCreateAuditLogs, getRole} as unknown as IGuildRepositoryAggregate,
		{generate: vi.fn(async () => nextLogId++)} as unknown as ISnowflakeService,
		{addJob} as unknown as IWorkerService<WorkerTaskName>,
		{dispatchGuild} as unknown as IGatewayService,
	);
	return {service, createAuditLog, batchDeleteAndCreateAuditLogs, getRole, dispatchGuild, addJob};
}

function overwrites(
	entries: Array<[RoleID | UserID, {type: number; allow: bigint; deny: bigint}]>,
): Map<RoleID | UserID, ChannelPermissionOverwrite> {
	return new Map(
		entries.map(([id, overwrite]) => [
			id,
			new ChannelPermissionOverwrite({type: overwrite.type, allow_: overwrite.allow, deny_: overwrite.deny}),
		]),
	);
}

function writtenRow(createAuditLog: ReturnType<typeof createService>['createAuditLog'], index = 0): GuildAuditLogRow {
	const call = createAuditLog.mock.calls[index];
	if (!call) {
		throw new Error(`expected audit log write ${index}`);
	}
	return call[0];
}

function writtenOptions(row: GuildAuditLogRow): Record<string, string> {
	return Object.fromEntries(row.options ?? []);
}

describe('GuildAuditLogService dispatch', () => {
	it('dispatches the REST entry shape plus guild_id for an invite', async () => {
		const {service, dispatchGuild} = createService();
		const log = await service
			.createBuilder(GUILD_ID, ACTOR_ID)
			.withAction(AuditLogActionType.INVITE_CREATE, 'abcdef')
			.withMetadata({
				max_uses: '0',
				max_age: '604800',
				temporary: 'false',
				channel_id: CHANNEL_ID.toString(),
				inviter_id: ACTOR_ID.toString(),
			})
			.withChanges(service.computeChanges(null, {code: 'abcdef', uses: 0}))
			.commit();
		expect(log).not.toBeNull();
		expect(dispatchGuild).toHaveBeenCalledTimes(1);
		expect(dispatchGuild).toHaveBeenCalledWith({
			guildId: GUILD_ID,
			event: 'GUILD_AUDIT_LOG_ENTRY_CREATE',
			data: {...mapGuildAuditLogEntry(log!), guild_id: GUILD_ID.toString()},
		});
		const data = dispatchGuild.mock.calls[0]![0].data;
		expect(data.options).toEqual({
			max_uses: 0,
			max_age: 604800,
			temporary: false,
			channel_id: CHANNEL_ID.toString(),
			inviter_id: ACTOR_ID.toString(),
		});
	});

	it('dispatches the resolved reason for a legacy ban row', async () => {
		const {service, dispatchGuild} = createService();
		const log = await service
			.createBuilder(GUILD_ID, ACTOR_ID)
			.withAction(AuditLogActionType.MEMBER_BAN_ADD, MEMBER_ID.toString())
			.withMetadata({delete_member_days: '0'})
			.withChanges([
				{key: 'user_id', new_value: MEMBER_ID.toString()},
				{key: 'reason', new_value: 'spam'},
			])
			.commit();
		const data = dispatchGuild.mock.calls[0]![0].data;
		expect(data).toEqual({...mapGuildAuditLogEntry(log!), guild_id: GUILD_ID.toString()});
		expect(data.reason).toBe('spam');
		expect(data.options).toEqual({delete_member_days: '0'});
	});

	it('does not dispatch raw timeout options', async () => {
		const {service, dispatchGuild} = createService();
		await service
			.createBuilder(GUILD_ID, ACTOR_ID)
			.withAction(AuditLogActionType.MEMBER_UPDATE, MEMBER_ID.toString())
			.withMetadata({timeout_reason: 'cool off', communication_disabled_until: '2026-09-13T00:00:00.000Z'})
			.withChanges([{key: 'communication_disabled_until', new_value: '2026-09-13T00:00:00.000Z'}])
			.commit();
		const data = dispatchGuild.mock.calls[0]![0].data;
		expect(data.options).toBeUndefined();
		expect(data.reason).toBe('cool off');
	});

	it('dispatches a batched bulk delete with a numeric count', async () => {
		const {service, dispatchGuild, batchDeleteAndCreateAuditLogs} = createService();
		const logs = [1420000000000000200n, 1420000000000000199n].map(
			(logId) =>
				new GuildAuditLog({
					guild_id: GUILD_ID,
					log_id: logId,
					user_id: ACTOR_ID,
					target_id: null,
					action_type: AuditLogActionType.MESSAGE_DELETE,
					reason: null,
					options: new Map([['channel_id', CHANNEL_ID.toString()]]),
					changes: null,
				}),
		);
		const result = await service.batchConsecutiveMessageDeleteLogs(GUILD_ID, logs);
		expect(batchDeleteAndCreateAuditLogs).toHaveBeenCalledTimes(1);
		expect(result.createdLogs).toHaveLength(1);
		expect(dispatchGuild).toHaveBeenCalledTimes(1);
		const data = dispatchGuild.mock.calls[0]![0].data;
		expect(data.action_type).toBe(AuditLogActionType.MESSAGE_BULK_DELETE);
		expect(data.guild_id).toBe(GUILD_ID.toString());
		expect(data.options).toEqual({channel_id: CHANNEL_ID.toString(), count: 2});
	});
});

describe('GuildAuditLogBuilder.commit', () => {
	it.each(SKIPPABLE_ACTIONS)('skips action %i with empty changes', async (actionType) => {
		const {service, createAuditLog, dispatchGuild} = createService();
		const result = await service
			.createBuilder(GUILD_ID, ACTOR_ID)
			.withAction(actionType, MEMBER_ID.toString())
			.withReason('ignored reason')
			.withMetadata({role_name: 'Moderators'})
			.withChanges([])
			.commit();
		expect(result).toBeNull();
		expect(createAuditLog).not.toHaveBeenCalled();
		expect(dispatchGuild).not.toHaveBeenCalled();
	});

	it.each(SKIPPABLE_ACTIONS)('skips action %i without changes', async (actionType) => {
		const {service, createAuditLog, dispatchGuild} = createService();
		const result = await service
			.createBuilder(GUILD_ID, ACTOR_ID)
			.withAction(actionType, MEMBER_ID.toString())
			.commit();
		expect(result).toBeNull();
		expect(createAuditLog).not.toHaveBeenCalled();
		expect(dispatchGuild).not.toHaveBeenCalled();
	});

	it('skips a guild update that only changes internal keys', async () => {
		const {service, createAuditLog} = createService();
		const result = await service
			.createBuilder(GUILD_ID, ACTOR_ID)
			.withAction(AuditLogActionType.GUILD_UPDATE, GUILD_ID.toString())
			.withChanges([
				{key: 'member_count', old_value: 1, new_value: 2},
				{key: 'splash_width', old_value: null, new_value: 1920},
			])
			.commit();
		expect(result).toBeNull();
		expect(createAuditLog).not.toHaveBeenCalled();
	});

	it('records a kick without changes', async () => {
		const {service, createAuditLog, dispatchGuild} = createService();
		const result = await service
			.createBuilder(GUILD_ID, ACTOR_ID)
			.withAction(AuditLogActionType.MEMBER_KICK, MEMBER_ID.toString())
			.withReason('rule 1')
			.commit();
		expect(result).not.toBeNull();
		expect(createAuditLog).toHaveBeenCalledTimes(1);
		expect(writtenRow(createAuditLog).reason).toBe('rule 1');
		expect(writtenRow(createAuditLog).changes).toBeNull();
		expect(dispatchGuild).toHaveBeenCalledTimes(1);
	});

	it('records a webhook create with empty changes', async () => {
		const {service, createAuditLog, dispatchGuild} = createService();
		const result = await service
			.createBuilder(GUILD_ID, ACTOR_ID)
			.withAction(AuditLogActionType.WEBHOOK_CREATE, '1420000000000000005')
			.withChanges([])
			.commit();
		expect(result).not.toBeNull();
		expect(createAuditLog).toHaveBeenCalledTimes(1);
		expect(dispatchGuild).toHaveBeenCalledTimes(1);
	});
});

describe('GuildAuditLogService.recordPermissionOverwriteDiff', () => {
	it('records role_name for a role overwrite', async () => {
		const {service, createAuditLog, getRole} = createService(new Map([[ROLE_ID.toString(), 'Moderators']]));
		await service.recordPermissionOverwriteDiff({
			guildId: GUILD_ID,
			userId: ACTOR_ID,
			channelId: CHANNEL_ID,
			previous: null,
			next: overwrites([[ROLE_ID, {type: 0, allow: 1024n, deny: 0n}]]),
		});
		expect(getRole).toHaveBeenCalledTimes(1);
		expect(getRole).toHaveBeenCalledWith(ROLE_ID, GUILD_ID);
		const row = writtenRow(createAuditLog);
		expect(row.action_type).toBe(AuditLogActionType.CHANNEL_OVERWRITE_CREATE);
		expect(writtenOptions(row)).toEqual({
			id: ROLE_ID.toString(),
			type: '0',
			channel_id: CHANNEL_ID.toString(),
			role_name: 'Moderators',
		});
	});

	it('records role_name on update and delete', async () => {
		const {service, createAuditLog} = createService(new Map([[ROLE_ID.toString(), 'Moderators']]));
		await service.recordPermissionOverwriteDiff({
			guildId: GUILD_ID,
			userId: ACTOR_ID,
			channelId: CHANNEL_ID,
			previous: overwrites([[ROLE_ID, {type: 0, allow: 1024n, deny: 0n}]]),
			next: overwrites([[ROLE_ID, {type: 0, allow: 0n, deny: 1024n}]]),
		});
		await service.recordPermissionOverwriteDiff({
			guildId: GUILD_ID,
			userId: ACTOR_ID,
			channelId: CHANNEL_ID,
			previous: overwrites([[ROLE_ID, {type: 0, allow: 0n, deny: 1024n}]]),
			next: null,
		});
		expect(writtenRow(createAuditLog, 0).action_type).toBe(AuditLogActionType.CHANNEL_OVERWRITE_UPDATE);
		expect(writtenOptions(writtenRow(createAuditLog, 0)).role_name).toBe('Moderators');
		expect(writtenRow(createAuditLog, 1).action_type).toBe(AuditLogActionType.CHANNEL_OVERWRITE_DELETE);
		expect(writtenOptions(writtenRow(createAuditLog, 1)).role_name).toBe('Moderators');
	});

	it('records the entry without role_name when the role lookup fails', async () => {
		const {service, createAuditLog, getRole} = createService(new Map([[ROLE_ID.toString(), 'Moderators']]));
		getRole.mockRejectedValueOnce(new Error('no host available'));
		await service.recordPermissionOverwriteDiff({
			guildId: GUILD_ID,
			userId: ACTOR_ID,
			channelId: CHANNEL_ID,
			previous: null,
			next: overwrites([[ROLE_ID, {type: 0, allow: 1024n, deny: 0n}]]),
		});
		expect(createAuditLog).toHaveBeenCalledTimes(1);
		expect(writtenOptions(writtenRow(createAuditLog))).toEqual({
			id: ROLE_ID.toString(),
			type: '0',
			channel_id: CHANNEL_ID.toString(),
		});
	});

	it('omits role_name for a member overwrite', async () => {
		const {service, createAuditLog, getRole} = createService(new Map([[MEMBER_ID.toString(), 'Not a role']]));
		await service.recordPermissionOverwriteDiff({
			guildId: GUILD_ID,
			userId: ACTOR_ID,
			channelId: CHANNEL_ID,
			previous: null,
			next: overwrites([[MEMBER_ID, {type: 1, allow: 1024n, deny: 0n}]]),
		});
		expect(getRole).not.toHaveBeenCalled();
		expect(writtenOptions(writtenRow(createAuditLog))).toEqual({
			id: MEMBER_ID.toString(),
			type: '1',
			channel_id: CHANNEL_ID.toString(),
		});
	});

	it('omits role_name for @everyone', async () => {
		const {service, createAuditLog, getRole} = createService(new Map([[GUILD_ID.toString(), '@everyone']]));
		await service.recordPermissionOverwriteDiff({
			guildId: GUILD_ID,
			userId: ACTOR_ID,
			channelId: CHANNEL_ID,
			previous: null,
			next: overwrites([[EVERYONE_ROLE_ID, {type: 0, allow: 0n, deny: 1024n}]]),
		});
		expect(getRole).not.toHaveBeenCalled();
		expect(writtenOptions(writtenRow(createAuditLog))).not.toHaveProperty('role_name');
	});

	it('omits role_name when the role no longer exists', async () => {
		const {service, createAuditLog, getRole} = createService();
		await service.recordPermissionOverwriteDiff({
			guildId: GUILD_ID,
			userId: ACTOR_ID,
			channelId: CHANNEL_ID,
			previous: overwrites([[ROLE_ID, {type: 0, allow: 1024n, deny: 0n}]]),
			next: null,
		});
		expect(getRole).toHaveBeenCalledTimes(1);
		expect(writtenOptions(writtenRow(createAuditLog))).not.toHaveProperty('role_name');
	});

	it('passes the reason through', async () => {
		const {service, createAuditLog} = createService(new Map([[ROLE_ID.toString(), 'Moderators']]));
		await service.recordPermissionOverwriteDiff({
			guildId: GUILD_ID,
			userId: ACTOR_ID,
			channelId: CHANNEL_ID,
			previous: null,
			next: overwrites([[ROLE_ID, {type: 0, allow: 1024n, deny: 0n}]]),
			reason: 'channel cleanup',
		});
		expect(writtenRow(createAuditLog).reason).toBe('channel cleanup');
	});

	it('writes nothing for an unchanged overwrite', async () => {
		const {service, createAuditLog, getRole, dispatchGuild} = createService(
			new Map([[ROLE_ID.toString(), 'Moderators']]),
		);
		await service.recordPermissionOverwriteDiff({
			guildId: GUILD_ID,
			userId: ACTOR_ID,
			channelId: CHANNEL_ID,
			previous: overwrites([[ROLE_ID, {type: 0, allow: 1024n, deny: 0n}]]),
			next: overwrites([[ROLE_ID, {type: 0, allow: 1024n, deny: 0n}]]),
			reason: 'no change',
		});
		expect(getRole).not.toHaveBeenCalled();
		expect(createAuditLog).not.toHaveBeenCalled();
		expect(dispatchGuild).not.toHaveBeenCalled();
	});
});

describe('GuildAuditLogService.scheduleMessageDeleteBatchJob', () => {
	it('gives every delete in one 30 second window a single batch job that runs after the window closes, without a ledger row', async () => {
		vi.useFakeTimers({toFake: ['Date']});
		try {
			const {service, addJob} = createService();
			for (const at of ['2026-09-21T12:00:00.000Z', '2026-09-21T12:00:29.999Z', '2026-09-21T12:00:40.000Z']) {
				vi.setSystemTime(new Date(at));
				await service.scheduleMessageDeleteBatchJob(GUILD_ID);
			}
			const options = addJob.mock.calls.map((call) => call[2] as {jobKey: string; runAt: Date; skipLedger: boolean});
			expect(options.every((option) => option.skipLedger)).toBe(true);
			expect(options[0]!.jobKey).toBe(options[1]!.jobKey);
			expect(options[2]!.jobKey).not.toBe(options[1]!.jobKey);
			expect(options[0]!.runAt.getTime()).toBeGreaterThan(new Date('2026-09-21T12:00:29.999Z').getTime());
			expect(options[1]!.runAt).toEqual(options[0]!.runAt);
			expect(options[2]!.runAt).toEqual(new Date('2026-09-21T12:01:30.000Z'));
			expect(options[2]!.runAt.getTime() - options[0]!.runAt.getTime()).toBe(30_000);
		} finally {
			vi.useRealTimers();
		}
	});
});
