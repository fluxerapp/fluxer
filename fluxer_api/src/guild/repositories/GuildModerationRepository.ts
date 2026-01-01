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
import type {AuditLogActionType} from '~/constants/AuditLogActionType';
import {
	BatchBuilder,
	Db,
	deleteOneOrMany,
	executeVersionedUpdate,
	fetchMany,
	fetchOne,
	upsertOne,
} from '~/database/Cassandra';
import type {GuildAuditLogRow, GuildBanRow, GuildRow} from '~/database/CassandraTypes';
import {GuildAuditLog, GuildBan} from '~/Models';
import {
	GuildAuditLogs,
	GuildAuditLogsByAction,
	GuildAuditLogsByUser,
	GuildAuditLogsByUserAction,
	GuildBans,
	Guilds,
} from '~/Tables';
import {IGuildModerationRepository} from './IGuildModerationRepository';

const FETCH_GUILD_BAN_BY_GUILD_AND_USER_ID_QUERY = GuildBans.selectCql({
	where: [GuildBans.where.eq('guild_id'), GuildBans.where.eq('user_id')],
	limit: 1,
});

const FETCH_GUILD_BANS_BY_GUILD_ID_QUERY = GuildBans.selectCql({
	where: GuildBans.where.eq('guild_id'),
});

const AUDIT_LOG_TTL_SECONDS = 45 * 24 * 60 * 60;

const FETCH_GUILD_BY_ID_QUERY = Guilds.selectCql({
	where: Guilds.where.eq('guild_id'),
	limit: 1,
});

const FETCH_GUILD_AUDIT_LOG_QUERY = GuildAuditLogs.selectCql({
	where: [GuildAuditLogs.where.eq('guild_id'), GuildAuditLogs.where.eq('log_id')],
	limit: 1,
});

const FETCH_GUILD_AUDIT_LOGS_BY_IDS_QUERY = GuildAuditLogs.selectCql({
	where: [GuildAuditLogs.where.eq('guild_id'), GuildAuditLogs.where.in('log_id', 'log_ids')],
});

export class GuildModerationRepository extends IGuildModerationRepository {
	async getBan(guildId: GuildID, userId: UserID): Promise<GuildBan | null> {
		const ban = await fetchOne<GuildBanRow>(FETCH_GUILD_BAN_BY_GUILD_AND_USER_ID_QUERY, {
			guild_id: guildId,
			user_id: userId,
		});
		return ban ? new GuildBan(ban) : null;
	}

	async listBans(guildId: GuildID): Promise<Array<GuildBan>> {
		const bans = await fetchMany<GuildBanRow>(FETCH_GUILD_BANS_BY_GUILD_ID_QUERY, {
			guild_id: guildId,
		});
		return bans.map((ban) => new GuildBan(ban));
	}

	async upsertBan(data: GuildBanRow): Promise<GuildBan> {
		if (data.expires_at) {
			const now = Date.now();
			const expiryTime = data.expires_at.getTime();
			const ttl = Math.max(0, Math.ceil((expiryTime - now) / 1000));
			await upsertOne(GuildBans.insertWithTtl(data, ttl));
		} else {
			await upsertOne(GuildBans.insert(data));
		}
		return new GuildBan(data);
	}

	async deleteBan(guildId: GuildID, userId: UserID): Promise<void> {
		await deleteOneOrMany(
			GuildBans.deleteByPk({
				guild_id: guildId,
				user_id: userId,
			}),
		);
	}

	async createAuditLog(data: GuildAuditLogRow): Promise<GuildAuditLog> {
		const payload = {
			...data,
			options: data.options ?? null,
			changes: data.changes ?? null,
		};

		const batch = new BatchBuilder();
		batch.addPrepared(GuildAuditLogs.insertWithTtl(payload, AUDIT_LOG_TTL_SECONDS));
		batch.addPrepared(GuildAuditLogsByUser.insertWithTtl(payload, AUDIT_LOG_TTL_SECONDS));
		batch.addPrepared(GuildAuditLogsByAction.insertWithTtl(payload, AUDIT_LOG_TTL_SECONDS));
		batch.addPrepared(GuildAuditLogsByUserAction.insertWithTtl(payload, AUDIT_LOG_TTL_SECONDS));
		await batch.execute();

		return this.mapRowToGuildAuditLog(data);
	}

	async getAuditLog(guildId: GuildID, logId: bigint): Promise<GuildAuditLog | null> {
		const row = await fetchOne<GuildAuditLogRow>(FETCH_GUILD_AUDIT_LOG_QUERY, {
			guild_id: guildId,
			log_id: logId,
		});
		return row ? this.mapRowToGuildAuditLog(row) : null;
	}

	async listAuditLogs(params: {
		guildId: GuildID;
		limit: number;
		afterLogId?: bigint;
		beforeLogId?: bigint;
		userId?: UserID;
		actionType?: AuditLogActionType;
	}): Promise<Array<GuildAuditLog>> {
		const {guildId, limit, afterLogId, beforeLogId, userId, actionType} = params;
		const table = this.selectAuditLogTable(userId, actionType);
		const query = this.buildAuditLogSelectQuery(table, limit, beforeLogId, afterLogId, userId, actionType);

		const values: {
			guild_id: GuildID;
			user_id?: UserID;
			action_type?: AuditLogActionType;
			before_log_id?: bigint;
			after_log_id?: bigint;
		} = {guild_id: guildId};

		if (userId) {
			values.user_id = userId;
		}
		if (actionType !== undefined) {
			values.action_type = actionType;
		}
		if (beforeLogId) {
			values.before_log_id = beforeLogId;
		} else if (afterLogId) {
			values.after_log_id = afterLogId;
		}

		const rows = await fetchMany<GuildAuditLogRow>(query, values);
		return rows.map((row) => this.mapRowToGuildAuditLog(row));
	}

	async listAuditLogsByIds(guildId: GuildID, logIds: Array<bigint>): Promise<Array<GuildAuditLog>> {
		if (logIds.length === 0) {
			return [];
		}

		const rows = await fetchMany<GuildAuditLogRow>(FETCH_GUILD_AUDIT_LOGS_BY_IDS_QUERY, {
			guild_id: guildId,
			log_ids: logIds,
		});
		return rows.map((row) => this.mapRowToGuildAuditLog(row));
	}

	async deleteAuditLogs(guildId: GuildID, logs: Array<GuildAuditLog>): Promise<void> {
		if (logs.length === 0) {
			return;
		}

		const batch = new BatchBuilder();
		for (const log of logs) {
			batch.addPrepared(
				GuildAuditLogs.deleteByPk({
					guild_id: guildId,
					log_id: log.logId,
				}),
			);
			batch.addPrepared(
				GuildAuditLogsByUser.deleteByPk({
					guild_id: guildId,
					user_id: log.userId,
					log_id: log.logId,
				}),
			);
			batch.addPrepared(
				GuildAuditLogsByAction.deleteByPk({
					guild_id: guildId,
					action_type: log.actionType,
					log_id: log.logId,
				}),
			);
			batch.addPrepared(
				GuildAuditLogsByUserAction.deleteByPk({
					guild_id: guildId,
					user_id: log.userId,
					action_type: log.actionType,
					log_id: log.logId,
				}),
			);
		}
		await batch.execute();
	}

	async updateAuditLogsIndexedAt(guildId: GuildID, indexedAt: Date | null): Promise<void> {
		await executeVersionedUpdate<GuildRow, 'guild_id'>(
			() => fetchOne<GuildRow>(FETCH_GUILD_BY_ID_QUERY, {guild_id: guildId}),
			(current) => {
				const patch: Record<string, ReturnType<typeof Db.set> | ReturnType<typeof Db.clear>> = {};

				if (indexedAt !== null) {
					patch.audit_logs_indexed_at = Db.set(indexedAt);
				} else if (current?.audit_logs_indexed_at !== null && current?.audit_logs_indexed_at !== undefined) {
					patch.audit_logs_indexed_at = Db.clear();
				}

				return {
					pk: {guild_id: guildId},
					patch,
				};
			},
			Guilds,
			{onFailure: 'log'},
		);
	}

	private mapRowToGuildAuditLog(row: GuildAuditLogRow): GuildAuditLog {
		return new GuildAuditLog(row);
	}

	private buildAuditLogSelectQuery(
		table:
			| typeof GuildAuditLogs
			| typeof GuildAuditLogsByUser
			| typeof GuildAuditLogsByAction
			| typeof GuildAuditLogsByUserAction,
		limit: number,
		beforeLogId?: bigint,
		afterLogId?: bigint,
		userId?: UserID,
		actionType?: AuditLogActionType,
	): string {
		const where: Array<ReturnType<typeof table.where.eq | typeof table.where.lt | typeof table.where.gt>> = [
			table.where.eq('guild_id'),
		];

		if (userId) {
			where.push(table.where.eq('user_id'));
		}
		if (actionType !== undefined) {
			where.push(table.where.eq('action_type'));
		}
		if (beforeLogId) {
			where.push(table.where.lt('log_id', 'before_log_id'));
		} else if (afterLogId) {
			where.push(table.where.gt('log_id', 'after_log_id'));
		}

		return table.selectCql({where, limit});
	}

	private selectAuditLogTable(
		userId?: UserID,
		actionType?: AuditLogActionType,
	):
		| typeof GuildAuditLogs
		| typeof GuildAuditLogsByUser
		| typeof GuildAuditLogsByAction
		| typeof GuildAuditLogsByUserAction {
		if (userId && actionType !== undefined) {
			return GuildAuditLogsByUserAction;
		}
		if (userId) {
			return GuildAuditLogsByUser;
		}
		if (actionType !== undefined) {
			return GuildAuditLogsByAction;
		}
		return GuildAuditLogs;
	}
}
