/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {GuildID, UserID} from '~/BrandedTypes';
import {AuditLogActionType} from '~/constants/AuditLogActionType';
import type {GuildAuditLogRow} from '~/database/CassandraTypes';
import type {SnowflakeService} from '~/infrastructure/SnowflakeService';
import type {GuildAuditLog} from '~/Models';
import type {IWorkerService} from '~/worker/IWorkerService';
import type {AuditLogChange, GuildAuditLogChange} from './GuildAuditLogTypes';
import type {IGuildRepository} from './IGuildRepository';

export type {GuildAuditLogChange};

const BATCH_AUDIT_LOG_JOB_DELAY_MS = 30_000;

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

const normalizeAuditLogMetadata = (metadata?: CreateGuildAuditLogParams['metadata']): Map<string, string> => {
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
};

export class GuildAuditLogService {
	constructor(
		private readonly guildRepository: IGuildRepository,
		private readonly snowflakeService: SnowflakeService,
		private readonly workerService?: IWorkerService,
	) {}

	async createLog(params: CreateGuildAuditLogParams): Promise<GuildAuditLog> {
		const logId = this.snowflakeService.generate();
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

		if (params.actionType === AuditLogActionType.MESSAGE_DELETE && this.workerService) {
			await this.scheduleMessageDeleteBatchJob(params.guildId);
		}

		return log;
	}

	async scheduleMessageDeleteBatchJob(guildId: GuildID): Promise<void> {
		if (!this.workerService) {
			return;
		}

		const runAt = new Date(Date.now() + BATCH_AUDIT_LOG_JOB_DELAY_MS);
		await this.workerService.addJob(
			'batchGuildAuditLogMessageDeletes',
			{guildId: guildId.toString()},
			{
				jobKey: `batch-audit-log-message-deletes:${guildId}`,
				runAt,
				maxAttempts: 3,
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
					const newestLogId = group.logs.reduce((max, l) => (l.logId > max ? l.logId : max), group.logs[0].logId);

					for (const groupLog of group.logs) {
						deletedLogIds.push(groupLog.logId);
					}
					await this.guildRepository.deleteAuditLogs(guildId, group.logs);

					const batchedLog = await this.createBatchedMessageDeleteLog(guildId, group, newestLogId);
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
		logId: bigint,
	): Promise<GuildAuditLog> {
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

		return this.guildRepository.createAuditLog(row);
	}

	createBuilder(guildId: GuildID, userId: UserID): GuildAuditLogBuilder {
		return new GuildAuditLogBuilder(this, guildId, userId);
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
}

export class GuildAuditLogBuilder {
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

	async commit(): Promise<GuildAuditLog> {
		if (this.params.actionType === undefined) {
			throw new Error('Audit log action type must be set before committing');
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
