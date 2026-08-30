// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {seconds} from 'itty-time';
import type {GuildID, UserID} from '../../BrandedTypes';
import {BatchBuilder, fetchMany, fetchOne} from '../../database/CassandraQueryExecution';
import {Db, type DbOp, type QueryTemplate, type WhereExpr} from '../../database/CassandraTypes';
import {executeVersionedUpdate} from '../../database/CassandraVersionedUpdate';
import type {
	GuildAuditLogRow,
	GuildBanByEmailRow,
	GuildBanByUserIdRow,
	GuildBanRow,
	GuildRow,
} from '../../database/types/GuildTypes';
import type {RequestCache} from '../../middleware/RequestCacheMiddleware';
import {GuildAuditLog} from '../../models/GuildAuditLog';
import {GuildBan} from '../../models/GuildBan';
import {
	GuildAuditLogs,
	GuildAuditLogsByAction,
	GuildAuditLogsByUser,
	GuildAuditLogsByUserAction,
	GuildBans,
	GuildBansByEmail,
	GuildBansByUserId,
	Guilds,
} from '../../Tables';
import {IGuildModerationRepository} from './IGuildModerationRepository';

const FETCH_GUILD_BAN_BY_GUILD_AND_USER_ID_QUERY = GuildBans.selectCql({
	where: [GuildBans.where.eq('guild_id'), GuildBans.where.eq('user_id')],
	limit: 1,
});
const FETCH_GUILD_BANS_BY_GUILD_ID_QUERY = GuildBans.selectCql({
	where: GuildBans.where.eq('guild_id'),
});
const FETCH_GUILD_BAN_BY_EMAIL_QUERY = GuildBansByEmail.selectCql({
	where: [GuildBansByEmail.where.eq('guild_id'), GuildBansByEmail.where.eq('email')],
	limit: 1,
});
const FETCH_GUILD_BANS_BY_USER_ID_QUERY = GuildBansByUserId.selectCql({
	where: GuildBansByUserId.where.eq('user_id'),
});
const AUDIT_LOG_TTL_SECONDS = seconds('45 days');
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
	constructor(private readonly requestCache?: RequestCache) {
		super();
	}

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
		const batch = new BatchBuilder();
		const byUserRow: GuildBanByUserIdRow = {
			user_id: data.user_id,
			guild_id: data.guild_id,
			email: data.email,
		};
		if (data.expires_at) {
			const now = Date.now();
			const ttl = Math.max(0, Math.ceil((data.expires_at.getTime() - now) / 1000));
			batch.addPrepared(GuildBans.insertWithTtl(data, ttl));
			batch.addPrepared(GuildBansByUserId.insertWithTtl(byUserRow, ttl));
			if (data.email)
				batch.addPrepared(
					GuildBansByEmail.insertWithTtl({guild_id: data.guild_id, email: data.email, user_id: data.user_id}, ttl),
				);
		} else {
			batch.addPrepared(GuildBans.insert(data));
			batch.addPrepared(GuildBansByUserId.insert(byUserRow));
			if (data.email)
				batch.addPrepared(GuildBansByEmail.insert({guild_id: data.guild_id, email: data.email, user_id: data.user_id}));
		}
		await batch.execute();
		return new GuildBan(data);
	}

	async deleteBan(guildId: GuildID, userId: UserID): Promise<void> {
		const ban = await this.getBan(guildId, userId);
		const batch = new BatchBuilder();
		batch.addPrepared(GuildBans.deleteByPk({guild_id: guildId, user_id: userId}));
		batch.addPrepared(GuildBansByUserId.deleteByPk({user_id: userId, guild_id: guildId}));
		if (ban?.email) batch.addPrepared(GuildBansByEmail.deleteByPk({guild_id: guildId, email: ban.email}));
		await batch.execute();
	}

	async deleteAllBansForUser(userId: UserID): Promise<void> {
		const bans = await fetchMany<GuildBanByUserIdRow>(FETCH_GUILD_BANS_BY_USER_ID_QUERY, {user_id: userId});
		const batch = new BatchBuilder();
		for (const ban of bans) {
			batch.addPrepared(GuildBans.deleteByPk({guild_id: ban.guild_id, user_id: userId}));
			batch.addPrepared(GuildBansByUserId.deleteByPk({user_id: userId, guild_id: ban.guild_id}));
			if (ban.email) {
				batch.addPrepared(GuildBansByEmail.deleteByPk({guild_id: ban.guild_id, email: ban.email}));
			}
		}
		await batch.execute();
	}

	async getBanByEmail(guildId: GuildID, email: string): Promise<GuildBan | null> {
		const row = await fetchOne<GuildBanByEmailRow>(FETCH_GUILD_BAN_BY_EMAIL_QUERY, {guild_id: guildId, email});
		if (!row) return null;
		return this.getBan(guildId, row.user_id);
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
		await batch.execute(false);
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
		const rows = await fetchMany<GuildAuditLogRow>(query.bind(values));
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

	async batchDeleteAndCreateAuditLogs(
		guildId: GuildID,
		logsToDelete: Array<GuildAuditLog>,
		logToCreate: GuildAuditLogRow,
	): Promise<GuildAuditLog> {
		const payload = {
			...logToCreate,
			options: logToCreate.options ?? null,
			changes: logToCreate.changes ?? null,
		};
		const batch = new BatchBuilder();
		for (const log of logsToDelete) {
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
		batch.addPrepared(GuildAuditLogs.insertWithTtl(payload, AUDIT_LOG_TTL_SECONDS));
		batch.addPrepared(GuildAuditLogsByUser.insertWithTtl(payload, AUDIT_LOG_TTL_SECONDS));
		batch.addPrepared(GuildAuditLogsByAction.insertWithTtl(payload, AUDIT_LOG_TTL_SECONDS));
		batch.addPrepared(GuildAuditLogsByUserAction.insertWithTtl(payload, AUDIT_LOG_TTL_SECONDS));
		await batch.execute();
		return this.mapRowToGuildAuditLog(payload);
	}

	async updateAuditLogsIndexedAt(guildId: GuildID, indexedAt: Date | null): Promise<void> {
		this.requestCache?.guilds.delete(guildId);
		await executeVersionedUpdate<GuildRow, 'guild_id'>(
			() => fetchOne<GuildRow>(FETCH_GUILD_BY_ID_QUERY, {guild_id: guildId}),
			(current) => {
				const patch: Record<string, DbOp<unknown>> = {};
				if (indexedAt !== null) {
					patch['audit_logs_indexed_at'] = Db.set(indexedAt);
				} else if (current?.audit_logs_indexed_at !== null && current?.audit_logs_indexed_at !== undefined) {
					patch['audit_logs_indexed_at'] = Db.clear();
				}
				return {
					pk: {guild_id: guildId},
					patch,
				};
			},
			Guilds,
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
	): QueryTemplate {
		const where: Array<WhereExpr<GuildAuditLogRow>> = [table.where.eq('guild_id')];
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
		return table.select({
			where,
			limit,
			orderBy: {col: 'log_id', direction: 'DESC'},
		});
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
