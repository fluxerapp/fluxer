// SPDX-License-Identifier: AGPL-3.0-or-later

import {type ChannelID, createRoleID, type GuildID, type RoleID, type UserID} from '@app/api/BrandedTypes';
import type {GuildAuditLogRow} from '@app/api/database/types/GuildTypes';
import {isNoopGuildAuditLog, mapGuildAuditLogEntry} from '@app/api/guild/GuildAuditLogEntryMapper';
import type {AuditLogChange, GuildAuditLogChange} from '@app/api/guild/GuildAuditLogTypes';
import type {IGuildRepositoryAggregate} from '@app/api/guild/repositories/IGuildRepositoryAggregate';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {ISnowflakeService} from '@app/api/infrastructure/ISnowflakeService';
import {Logger} from '@app/api/Logger';
import type {ChannelPermissionOverwrite} from '@app/api/models/ChannelPermissionOverwrite';
import type {GuildAuditLog} from '@app/api/models/GuildAuditLog';
import type {WorkerTaskName} from '@app/api/worker/WorkerLaneConfig';
import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import type {IWorkerService} from '@pkgs/worker/src/contracts/IWorkerService';
import {ms} from 'itty-time';

const MESSAGE_DELETE_BATCH_DELAY_MS = ms('30 seconds');

interface MessageDeleteBatchGroup {
	logs: Array<GuildAuditLog>;
	userId: UserID;
	channelId: string;
}

interface BatchResult {
	processedLogs: Array<GuildAuditLog>;
	deletedLogIds: Array<bigint>;
	createdLogs: Array<GuildAuditLog>;
}

interface CreateGuildAuditLogParams {
	guildId: GuildID;
	userId: UserID;
	actionType: AuditLogActionType;
	targetId?: string | null;
	auditLogReason?: string | null;
	metadata?: Map<string, string> | Record<string, string> | Array<[string, string]>;
	changes?: GuildAuditLogChange | null;
	createdAt?: Date;
}

function normalizeAuditLogMetadata(metadata?: CreateGuildAuditLogParams['metadata']): Map<string, string> {
	if (!metadata) {
		return new Map();
	}
	if (metadata instanceof Map) {
		return metadata;
	}
	if (Array.isArray(metadata)) {
		return new Map(metadata);
	}
	return new Map(Object.entries(metadata));
}

export class GuildAuditLogService {
	constructor(
		private readonly guildRepository: IGuildRepositoryAggregate,
		private readonly snowflakeService: ISnowflakeService,
		private readonly workerService: IWorkerService<WorkerTaskName>,
		private readonly gatewayService: IGatewayService,
	) {}

	async createLog(params: CreateGuildAuditLogParams): Promise<GuildAuditLog> {
		const logId = await this.snowflakeService.generate();
		const metadataMap = normalizeAuditLogMetadata(params.metadata);
		const row: GuildAuditLogRow = {
			guild_id: params.guildId,
			log_id: logId,
			user_id: params.userId,
			target_id: params.targetId ?? null,
			action_type: params.actionType,
			reason: params.auditLogReason ?? null,
			options: metadataMap.size > 0 ? metadataMap : null,
			changes: params.changes ? JSON.stringify(params.changes) : null,
		};
		const log = await this.guildRepository.createAuditLog(row);
		await this.dispatchAuditLogEntryCreate(log);
		if (params.actionType === AuditLogActionType.MESSAGE_DELETE) {
			await this.scheduleMessageDeleteBatchJob(params.guildId);
		}
		return log;
	}

	async scheduleMessageDeleteBatchJob(guildId: GuildID): Promise<void> {
		const batchWindow = Math.floor(Date.now() / MESSAGE_DELETE_BATCH_DELAY_MS);
		await this.workerService.addJob(
			'batchGuildAuditLogMessageDeletes',
			{guildId: guildId.toString()},
			{
				jobKey: `batch-audit-log-message-deletes:${guildId}:${batchWindow}`,
				runAt: new Date((batchWindow + 2) * MESSAGE_DELETE_BATCH_DELAY_MS),
				maxAttempts: 3,
				skipLedger: true,
			},
		);
	}

	async batchConsecutiveMessageDeleteLogs(guildId: GuildID, logs: Array<GuildAuditLog>): Promise<BatchResult> {
		const groups = this.findConsecutiveMessageDeleteGroups(logs);
		const deletedLogIds: Array<bigint> = [];
		const createdLogs: Array<GuildAuditLog> = [];
		const processedLogs: Array<GuildAuditLog> = [];
		for (const log of logs) {
			const group = groups.find((g) => g.logs.includes(log));
			if (group && group.logs.length >= 2) {
				if (log === group.logs[0]) {
					for (const groupLog of group.logs) {
						deletedLogIds.push(groupLog.logId);
					}
					const batchedLog = await this.createBatchedMessageDeleteLog(guildId, group);
					createdLogs.push(batchedLog);
					processedLogs.push(batchedLog);
				}
			} else {
				processedLogs.push(log);
			}
		}
		return {processedLogs, deletedLogIds, createdLogs};
	}

	async batchRecentMessageDeleteLogs(guildId: GuildID, limit: number = 250): Promise<BatchResult> {
		const logs = await this.guildRepository.listAuditLogs({
			guildId,
			limit,
			actionType: AuditLogActionType.MESSAGE_DELETE,
		});
		if (logs.length < 2) {
			return {processedLogs: logs, deletedLogIds: [], createdLogs: []};
		}
		const allLogs = await this.guildRepository.listAuditLogs({
			guildId,
			limit,
		});
		return this.batchConsecutiveMessageDeleteLogs(guildId, allLogs);
	}

	private findConsecutiveMessageDeleteGroups(logs: Array<GuildAuditLog>): Array<MessageDeleteBatchGroup> {
		const groups: Array<MessageDeleteBatchGroup> = [];
		let currentGroup: MessageDeleteBatchGroup | null = null;
		for (const log of logs) {
			if (log.actionType !== AuditLogActionType.MESSAGE_DELETE) {
				if (currentGroup && currentGroup.logs.length >= 2) {
					groups.push(currentGroup);
				}
				currentGroup = null;
				continue;
			}
			const channelId = log.options.get('channel_id');
			if (!channelId) {
				if (currentGroup && currentGroup.logs.length >= 2) {
					groups.push(currentGroup);
				}
				currentGroup = null;
				continue;
			}
			if (currentGroup && currentGroup.userId === log.userId && currentGroup.channelId === channelId) {
				currentGroup.logs.push(log);
			} else {
				if (currentGroup && currentGroup.logs.length >= 2) {
					groups.push(currentGroup);
				}
				currentGroup = {
					logs: [log],
					userId: log.userId,
					channelId,
				};
			}
		}
		if (currentGroup && currentGroup.logs.length >= 2) {
			groups.push(currentGroup);
		}
		return groups;
	}

	private async createBatchedMessageDeleteLog(
		guildId: GuildID,
		group: MessageDeleteBatchGroup,
	): Promise<GuildAuditLog> {
		// Must be a fresh id: reusing one of the replaced logs would put a delete and an insert for the same
		// primary key in one batch, and Cassandra resolves that tie in favour of the tombstone, so the entry
		// would silently vanish from every table whose key does not include action_type.
		const logId = await this.snowflakeService.generate();
		const row: GuildAuditLogRow = {
			guild_id: guildId,
			log_id: logId,
			user_id: group.userId,
			target_id: null,
			action_type: AuditLogActionType.MESSAGE_BULK_DELETE,
			reason: null,
			options: new Map([
				['channel_id', group.channelId],
				['count', group.logs.length.toString()],
			]),
			changes: null,
		};
		const log = await this.guildRepository.batchDeleteAndCreateAuditLogs(guildId, group.logs, row);
		await this.dispatchAuditLogEntryCreate(log);
		return log;
	}

	createBuilder(guildId: GuildID, userId: UserID): GuildAuditLogBuilder {
		return new GuildAuditLogBuilder(this, guildId, userId);
	}

	async recordPermissionOverwriteDiff(params: {
		guildId: GuildID;
		userId: UserID;
		channelId: ChannelID;
		previous: Map<RoleID | UserID, ChannelPermissionOverwrite> | null | undefined;
		next: Map<RoleID | UserID, ChannelPermissionOverwrite> | null | undefined;
		reason?: string | null;
	}): Promise<void> {
		const previousEntries = params.previous ?? new Map<RoleID | UserID, ChannelPermissionOverwrite>();
		const nextEntries = params.next ?? new Map<RoleID | UserID, ChannelPermissionOverwrite>();
		const targetIds = new Set<RoleID | UserID>([...previousEntries.keys(), ...nextEntries.keys()]);
		for (const targetId of targetIds) {
			const before = previousEntries.get(targetId) ?? null;
			const after = nextEntries.get(targetId) ?? null;
			if (before && after && before.type === after.type && before.allow === after.allow && before.deny === after.deny) {
				continue;
			}
			const overwriteType = (after ?? before)?.type ?? 0;
			const previousSnapshot = before
				? {
						id: targetId.toString(),
						type: before.type.toString(),
						allow: before.allow.toString(),
						deny: before.deny.toString(),
					}
				: null;
			const nextSnapshot = after
				? {
						id: targetId.toString(),
						type: after.type.toString(),
						allow: after.allow.toString(),
						deny: after.deny.toString(),
					}
				: null;
			const changes = this.computeChanges(previousSnapshot, nextSnapshot);
			if (changes.length === 0) continue;
			const action =
				!before && after
					? AuditLogActionType.CHANNEL_OVERWRITE_CREATE
					: before && !after
						? AuditLogActionType.CHANNEL_OVERWRITE_DELETE
						: AuditLogActionType.CHANNEL_OVERWRITE_UPDATE;
			let roleName: string | null = null;
			if (overwriteType === 0 && targetId.toString() !== params.guildId.toString()) {
				try {
					const role = await this.guildRepository.getRole(createRoleID(BigInt(targetId)), params.guildId);
					roleName = role?.name ?? null;
				} catch (error) {
					Logger.warn(
						{
							error,
							guildId: params.guildId.toString(),
							roleId: targetId.toString(),
						},
						'Failed to resolve role name for guild audit log',
					);
				}
			}
			try {
				const metadata: Record<string, string> = {
					id: targetId.toString(),
					type: overwriteType.toString(),
					channel_id: params.channelId.toString(),
				};
				if (roleName !== null) {
					metadata['role_name'] = roleName;
				}
				await this.createBuilder(params.guildId, params.userId)
					.withAction(action, targetId.toString())
					.withReason(params.reason ?? null)
					.withMetadata(metadata)
					.withChanges(changes)
					.commit();
			} catch (error) {
				Logger.error(
					{
						error,
						guildId: params.guildId.toString(),
						userId: params.userId.toString(),
						action,
						targetId: targetId.toString(),
						channelId: params.channelId.toString(),
					},
					'Failed to record guild audit log',
				);
			}
		}
	}

	computeChanges(
		previous: Record<string, unknown> | null | undefined,
		next: Record<string, unknown> | null | undefined,
	): GuildAuditLogChange {
		const changes: Array<AuditLogChange> = [];
		if (!previous && next) {
			for (const [key, value] of Object.entries(next)) {
				changes.push({key, new_value: value});
			}
			return changes;
		}
		if (previous && !next) {
			for (const [key, value] of Object.entries(previous)) {
				changes.push({key, old_value: value});
			}
			return changes;
		}
		if (previous && next) {
			const allKeys = new Set([...Object.keys(previous), ...Object.keys(next)]);
			for (const key of allKeys) {
				const oldValue = previous[key];
				const newValue = next[key];
				if (this.areValuesEqual(oldValue, newValue)) {
					continue;
				}
				const change: AuditLogChange = {key};
				if (oldValue === undefined && newValue !== undefined) {
					change.new_value = newValue;
				} else if (oldValue !== undefined && newValue === undefined) {
					change.old_value = oldValue;
				} else {
					change.old_value = oldValue;
					change.new_value = newValue;
				}
				changes.push(change);
			}
		}
		return changes;
	}

	computeArrayChange<T>(
		previous: Array<T> | null | undefined,
		next: Array<T> | null | undefined,
		key: string,
	): AuditLogChange | null {
		if (!previous?.length && !next?.length) {
			return null;
		}
		if (!this.areArraysEqual(previous, next)) {
			const change: AuditLogChange = {key};
			if (previous !== undefined && previous !== null) {
				change.old_value = previous;
			}
			if (next !== undefined && next !== null) {
				change.new_value = next;
			}
			return change;
		}
		return null;
	}

	private areValuesEqual(a: unknown, b: unknown): boolean {
		if (a === b) return true;
		if (a == null || b == null) return false;
		if (typeof a !== typeof b) return false;
		if (typeof a === 'object' && typeof b === 'object') {
			return JSON.stringify(a) === JSON.stringify(b);
		}
		return false;
	}

	private areArraysEqual(a: Array<unknown> | null | undefined, b: Array<unknown> | null | undefined): boolean {
		if (a === b) return true;
		if (!a || !b) return false;
		if (a.length !== b.length) return false;
		return JSON.stringify(a) === JSON.stringify(b);
	}

	private async dispatchAuditLogEntryCreate(log: GuildAuditLog): Promise<void> {
		try {
			await this.gatewayService.dispatchGuild({
				guildId: log.guildId,
				event: 'GUILD_AUDIT_LOG_ENTRY_CREATE',
				data: {...mapGuildAuditLogEntry(log), guild_id: log.guildId.toString()},
			});
		} catch (error) {
			Logger.error(
				{
					error,
					guildId: log.guildId.toString(),
					logId: log.logId.toString(),
				},
				'Failed to dispatch guild audit log entry create event',
			);
		}
	}
}

class GuildAuditLogBuilder {
	private readonly params: Partial<CreateGuildAuditLogParams>;
	private metadataMap: Map<string, string> | null = null;

	constructor(
		private readonly service: GuildAuditLogService,
		guildId: GuildID,
		userId: UserID,
	) {
		this.params = {guildId, userId};
	}

	withAction(actionType: AuditLogActionType, targetId?: string | null): this {
		this.params.actionType = actionType;
		this.params.targetId = targetId ?? null;
		return this;
	}

	withReason(reason?: string | null): this {
		this.params.auditLogReason = reason ?? null;
		return this;
	}

	withMetadata(metadata?: CreateGuildAuditLogParams['metadata']): this {
		this.metadataMap = normalizeAuditLogMetadata(metadata);
		return this;
	}

	withMetadataEntry(key: string, value: string): this {
		if (!this.metadataMap) {
			this.metadataMap = new Map();
		}
		this.metadataMap.set(key, value);
		return this;
	}

	withChanges(changes: GuildAuditLogChange | null): this {
		this.params.changes = changes;
		return this;
	}

	withComputedChanges(
		previous: Record<string, unknown> | null | undefined,
		next: Record<string, unknown> | null | undefined,
	): this {
		this.params.changes = this.service.computeChanges(previous, next);
		return this;
	}

	withCreatedAt(createdAt?: Date): this {
		this.params.createdAt = createdAt;
		return this;
	}

	async commit(): Promise<GuildAuditLog | null> {
		if (this.params.actionType === undefined) {
			throw new Error('Audit log action type must be set before committing');
		}
		if (isNoopGuildAuditLog(this.params.actionType, this.params.changes)) {
			return null;
		}
		return this.service.createLog({
			guildId: this.params.guildId!,
			userId: this.params.userId!,
			actionType: this.params.actionType,
			targetId: this.params.targetId ?? null,
			auditLogReason: this.params.auditLogReason ?? null,
			metadata: this.metadataMap ?? undefined,
			changes: this.params.changes ?? null,
			createdAt: this.params.createdAt,
		});
	}
}
