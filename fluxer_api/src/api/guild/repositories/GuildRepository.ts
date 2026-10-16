// SPDX-License-Identifier: AGPL-3.0-or-later

import type {EmojiID, GuildID, RoleID, StickerID, UserID} from '@app/api/BrandedTypes';
import {enqueueRemoveThreadMemberships} from '@app/api/channel/threads/ThreadJobs';
import {
	BatchBuilder,
	deleteOneOrMany,
	executeGroupedBatches,
	fetchMany,
	fetchOne,
	upsertOne,
} from '@app/api/database/CassandraQueryExecution';
import type {QueryTemplate, WhereExpr} from '@app/api/database/CassandraTypes';
import {buildPatchFromData, executeVersionedUpdate} from '@app/api/database/CassandraVersionedUpdate';
import {
	GUILD_COLUMNS,
	GUILD_EMOJI_COLUMNS,
	GUILD_MEMBER_COLUMNS,
	GUILD_ROLE_COLUMNS,
	GUILD_STICKER_COLUMNS,
	type GuildAuditLogRow,
	type GuildBanByEmailRow,
	type GuildBanByUserIdRow,
	type GuildBanRow,
	type GuildEmojiRow,
	type GuildMemberByUserIdRow,
	type GuildMemberRow,
	type GuildMembershipMetadataRow,
	type GuildRoleRow,
	type GuildRow,
	type GuildStickerRow,
} from '@app/api/database/types/GuildTypes';
import {isTainted} from '@app/api/experiment/ChannelThreadsGate';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import {Guild} from '@app/api/models/Guild';
import {GuildAuditLog} from '@app/api/models/GuildAuditLog';
import {GuildBan} from '@app/api/models/GuildBan';
import {GuildEmoji} from '@app/api/models/GuildEmoji';
import {GuildMember} from '@app/api/models/GuildMember';
import {GuildRole} from '@app/api/models/GuildRole';
import {GuildSticker} from '@app/api/models/GuildSticker';
import {
	GuildAuditLogs,
	GuildAuditLogsByAction,
	GuildAuditLogsByUser,
	GuildAuditLogsByUserAction,
	GuildBans,
	GuildBansByEmail,
	GuildBansByUserId,
	GuildEmojis,
	GuildEmojisByEmojiId,
	GuildMembers,
	GuildMembersByUserId,
	GuildMembershipMetadata,
	GuildRoles,
	GuildStickers,
	GuildStickersByStickerId,
	Guilds,
} from '@app/api/Tables';
import type {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {seconds} from 'itty-time';

const FETCH_GUILD_BY_ID_QUERY = Guilds.selectCql({
	where: Guilds.where.eq('guild_id'),
	limit: 1,
});

const FETCH_GUILDS_BY_IDS_QUERY = Guilds.selectCql({
	where: Guilds.where.in('guild_id', 'guild_ids'),
});

const createFetchAllGuildsPaginatedQuery = (limit: number) =>
	Guilds.select({
		where: Guilds.where.tokenGt('guild_id', 'last_guild_id'),
		limit,
	});

function createFetchAllGuildsFirstPageQuery(limit: number) {
	return Guilds.select({limit});
}

const FETCH_GUILD_MEMBER_BY_GUILD_AND_USER_ID_QUERY = GuildMembers.selectCql({
	where: [GuildMembers.where.eq('guild_id'), GuildMembers.where.eq('user_id')],
	limit: 1,
});

const FETCH_GUILD_MEMBERS_BY_GUILD_ID_QUERY = GuildMembers.selectCql({
	where: GuildMembers.where.eq('guild_id'),
});

const COUNT_GUILD_MEMBERS_BY_GUILD_ID_QUERY = GuildMembers.selectCountCql({
	where: GuildMembers.where.eq('guild_id'),
});

const FETCH_MEMBERSHIP_METADATA_QUERY = GuildMembershipMetadata.selectCql({
	where: [GuildMembershipMetadata.where.eq('guild_id'), GuildMembershipMetadata.where.eq('user_id')],
	limit: 1,
});

function createPaginatedFirstPageQuery(limit: number) {
	return GuildMembers.select({
		where: GuildMembers.where.eq('guild_id'),
		limit,
	});
}

function createPaginatedQuery(limit: number) {
	return GuildMembers.select({
		where: [GuildMembers.where.eq('guild_id'), GuildMembers.where.gt('user_id')],
		limit,
	});
}

const FETCH_GUILD_ROLE_BY_ID_QUERY = GuildRoles.selectCql({
	where: [GuildRoles.where.eq('guild_id'), GuildRoles.where.eq('role_id')],
	limit: 1,
});

const FETCH_GUILD_ROLES_BY_GUILD_ID_QUERY = GuildRoles.selectCql({
	where: GuildRoles.where.eq('guild_id'),
});

const FETCH_ROLES_BY_IDS_QUERY = GuildRoles.selectCql({
	where: [GuildRoles.where.eq('guild_id'), GuildRoles.where.in('role_id', 'role_ids')],
});

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

const FETCH_GUILD_EMOJIS_BY_GUILD_ID_QUERY = GuildEmojis.selectCql({
	where: GuildEmojis.where.eq('guild_id'),
});

const FETCH_GUILD_EMOJI_BY_ID_QUERY = GuildEmojis.selectCql({
	where: [GuildEmojis.where.eq('guild_id'), GuildEmojis.where.eq('emoji_id')],
	limit: 1,
});

const FETCH_GUILD_EMOJI_BY_EMOJI_ID_ONLY_QUERY = GuildEmojisByEmojiId.selectCql({
	where: GuildEmojisByEmojiId.where.eq('emoji_id'),
	limit: 1,
});

const FETCH_GUILD_STICKERS_BY_GUILD_ID_QUERY = GuildStickers.selectCql({
	where: GuildStickers.where.eq('guild_id'),
});

const FETCH_GUILD_STICKER_BY_ID_QUERY = GuildStickers.selectCql({
	where: [GuildStickers.where.eq('guild_id'), GuildStickers.where.eq('sticker_id')],
	limit: 1,
});

const FETCH_GUILD_STICKER_BY_STICKER_ID_ONLY_QUERY = GuildStickersByStickerId.selectCql({
	where: GuildStickersByStickerId.where.eq('sticker_id'),
	limit: 1,
});

export class GuildRepository {
	constructor(private readonly requestCache?: RequestCache) {}
	async findUnique(guildId: GuildID): Promise<Guild | null> {
		const prefetched = this.requestCache?.takeGuild(guildId);
		if (prefetched !== undefined) {
			return prefetched;
		}
		const guild = await fetchOne<GuildRow>(FETCH_GUILD_BY_ID_QUERY, {
			guild_id: guildId,
		});
		return guild ? new Guild(guild) : null;
	}

	async listGuilds(guildIds: Array<GuildID>): Promise<Array<Guild>> {
		if (guildIds.length === 0) {
			return [];
		}
		const guilds = await fetchMany<GuildRow>(FETCH_GUILDS_BY_IDS_QUERY, {guild_ids: guildIds});
		return guilds.map((guild) => new Guild(guild));
	}

	async listAllGuildsPaginated(limit: number, lastGuildId?: GuildID): Promise<Array<Guild>> {
		let guilds: Array<GuildRow>;
		if (lastGuildId) {
			const query = createFetchAllGuildsPaginatedQuery(limit);
			guilds = await fetchMany<GuildRow>(
				query.bind({
					last_guild_id: lastGuildId,
				}),
			);
		} else {
			const query = createFetchAllGuildsFirstPageQuery(limit);
			guilds = await fetchMany<GuildRow>(query.bind({}));
		}
		return guilds.map((guild) => new Guild(guild));
	}

	async listUserGuilds(userId: UserID): Promise<Array<Guild>> {
		const query = GuildMembersByUserId.select({
			columns: ['guild_id'],
			where: GuildMembersByUserId.where.eq('user_id'),
		});
		const guildMemberships = await fetchMany<Pick<GuildMemberByUserIdRow, 'guild_id'>>(query.bind({user_id: userId}));
		if (guildMemberships.length === 0) {
			return [];
		}
		const guildIds = guildMemberships.map((m) => m.guild_id);
		const guilds = await fetchMany<GuildRow>(FETCH_GUILDS_BY_IDS_QUERY, {guild_ids: guildIds});
		return guilds.map((guild) => new Guild(guild));
	}

	async countUserGuilds(userId: UserID): Promise<number> {
		const query = GuildMembersByUserId.select({
			columns: ['guild_id'],
			where: GuildMembersByUserId.where.eq('user_id'),
		});
		const guildMemberships = await fetchMany<Pick<GuildMemberByUserIdRow, 'guild_id'>>(query.bind({user_id: userId}));
		return guildMemberships.length;
	}

	async listOwnedGuildIds(userId: UserID): Promise<Array<GuildID>> {
		const userGuilds = await this.listUserGuilds(userId);
		return userGuilds.filter((guild) => guild.ownerId === userId).map((guild) => guild.id);
	}

	async upsertPartial(
		guildId: GuildID,
		patch: Partial<GuildRow>,
		oldData?: GuildRow | null,
		_previousOwnerId?: UserID,
	): Promise<Guild> {
		this.requestCache?.guilds.delete(guildId);
		const result = await executeVersionedUpdate<GuildRow, 'guild_id'>(
			async () => fetchOne<GuildRow>(FETCH_GUILD_BY_ID_QUERY, {guild_id: guildId}),
			(current) => ({
				pk: {guild_id: guildId},
				patch: buildPatchFromData(patch, current, GUILD_COLUMNS, ['guild_id']),
			}),
			Guilds,
			{initialData: oldData},
		);
		const latest = await fetchOne<GuildRow>(FETCH_GUILD_BY_ID_QUERY, {guild_id: guildId});
		if (!latest) {
			throw new Error('Guild row vanished after partial upsert');
		}
		return new Guild({...latest, version: result.finalVersion ?? latest.version});
	}

	private async deleteGuildRow(guildId: GuildID, _ownerId?: UserID): Promise<void> {
		this.requestCache?.guilds.delete(guildId);
		const guild = await fetchOne<GuildRow>(FETCH_GUILD_BY_ID_QUERY, {guild_id: guildId});
		if (!guild) {
			return;
		}
		const batch = new BatchBuilder();
		batch.addPrepared(Guilds.deleteByPk({guild_id: guildId}));
		await batch.execute();
	}

	async getMember(guildId: GuildID, userId: UserID): Promise<GuildMember | null> {
		const member = await fetchOne<GuildMemberRow>(FETCH_GUILD_MEMBER_BY_GUILD_AND_USER_ID_QUERY, {
			guild_id: guildId,
			user_id: userId,
		});
		return member ? new GuildMember(member) : null;
	}

	async listMembers(guildId: GuildID): Promise<Array<GuildMember>> {
		const members = await fetchMany<GuildMemberRow>(FETCH_GUILD_MEMBERS_BY_GUILD_ID_QUERY, {
			guild_id: guildId,
		});
		return members.map((member) => new GuildMember(member));
	}

	async countMembers(guildId: GuildID): Promise<number> {
		const result = await fetchOne<{
			count: bigint;
		}>(COUNT_GUILD_MEMBERS_BY_GUILD_ID_QUERY, {
			guild_id: guildId,
		});
		return result ? Number(result.count) : 0;
	}

	async upsertMember(data: GuildMemberRow, oldData?: GuildMemberRow | null): Promise<GuildMember> {
		const guildId = data.guild_id;
		const userId = data.user_id;
		const result = await executeVersionedUpdate<GuildMemberRow, 'guild_id' | 'user_id'>(
			async () =>
				fetchOne<GuildMemberRow>(FETCH_GUILD_MEMBER_BY_GUILD_AND_USER_ID_QUERY, {
					guild_id: guildId,
					user_id: userId,
				}),
			(current) => ({
				pk: {guild_id: guildId, user_id: userId},
				patch: buildPatchFromData(data, current, GUILD_MEMBER_COLUMNS, ['guild_id', 'user_id']),
			}),
			GuildMembers,
			{initialData: oldData},
		);
		await upsertOne(
			GuildMembersByUserId.insert({
				user_id: userId,
				guild_id: guildId,
			}),
		);
		return new GuildMember({...data, version: result.finalVersion ?? 1});
	}

	async listMembersPaginated(guildId: GuildID, limit: number, afterUserId?: UserID): Promise<Array<GuildMember>> {
		let rows: Array<GuildMemberRow>;
		if (afterUserId) {
			rows = await fetchMany<GuildMemberRow>(
				createPaginatedQuery(limit).bind({
					guild_id: guildId,
					user_id: afterUserId,
				}),
			);
		} else {
			rows = await fetchMany<GuildMemberRow>(
				createPaginatedFirstPageQuery(limit).bind({
					guild_id: guildId,
				}),
			);
		}
		return rows.map((row) => new GuildMember(row));
	}

	private async deleteMemberRows(guildId: GuildID, userId: UserID): Promise<void> {
		const batch = new BatchBuilder();
		batch.addPrepared(
			GuildMembers.deleteByPk({
				guild_id: guildId,
				user_id: userId,
			}),
		);
		batch.addPrepared(
			GuildMembersByUserId.deleteByPk({
				user_id: userId,
				guild_id: guildId,
			}),
		);
		await batch.execute();
	}

	async getMembershipMetadata(guildId: GuildID, userId: UserID): Promise<GuildMembershipMetadataRow | null> {
		return await fetchOne<GuildMembershipMetadataRow>(FETCH_MEMBERSHIP_METADATA_QUERY, {
			guild_id: guildId,
			user_id: userId,
		});
	}

	async upsertMembershipMetadata(data: GuildMembershipMetadataRow, ttlSeconds: number): Promise<void> {
		await upsertOne(GuildMembershipMetadata.insertWithTtl(data, ttlSeconds));
	}

	async getRole(roleId: RoleID, guildId: GuildID): Promise<GuildRole | null> {
		const role = await fetchOne<GuildRoleRow>(FETCH_GUILD_ROLE_BY_ID_QUERY, {
			guild_id: guildId,
			role_id: roleId,
		});
		return role ? new GuildRole(role) : null;
	}

	async listRoles(guildId: GuildID): Promise<Array<GuildRole>> {
		const roles = await fetchMany<GuildRoleRow>(FETCH_GUILD_ROLES_BY_GUILD_ID_QUERY, {
			guild_id: guildId,
		});
		return roles.map((role) => new GuildRole(role));
	}

	async listRolesByIds(roleIds: Array<RoleID>, guildId: GuildID): Promise<Array<GuildRole>> {
		if (roleIds.length === 0) return [];
		const roles = await fetchMany<GuildRoleRow>(FETCH_ROLES_BY_IDS_QUERY, {
			guild_id: guildId,
			role_ids: roleIds,
		});
		return roles.map((role) => new GuildRole(role));
	}

	async countRoles(guildId: GuildID): Promise<number> {
		const roles = await fetchMany<GuildRoleRow>(FETCH_GUILD_ROLES_BY_GUILD_ID_QUERY, {
			guild_id: guildId,
		});
		return roles.length;
	}

	async upsertRole(data: GuildRoleRow, oldData?: GuildRoleRow | null): Promise<GuildRole> {
		const guildId = data.guild_id;
		const roleId = data.role_id;
		const result = await executeVersionedUpdate<GuildRoleRow, 'guild_id' | 'role_id'>(
			async () =>
				fetchOne<GuildRoleRow>(FETCH_GUILD_ROLE_BY_ID_QUERY, {
					guild_id: guildId,
					role_id: roleId,
				}),
			(current) => ({
				pk: {guild_id: guildId, role_id: roleId},
				patch: buildPatchFromData(data, oldData ?? current, GUILD_ROLE_COLUMNS, ['guild_id', 'role_id']),
			}),
			GuildRoles,
			{initialData: oldData},
		);
		return new GuildRole({...data, version: result.finalVersion ?? 1});
	}

	async deleteRole(guildId: GuildID, roleId: RoleID): Promise<void> {
		await deleteOneOrMany(
			GuildRoles.deleteByPk({
				guild_id: guildId,
				role_id: roleId,
			}),
		);
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
		await executeGroupedBatches(
			bans.map((ban) => [
				GuildBans.deleteByPk({guild_id: ban.guild_id, user_id: userId}),
				GuildBansByUserId.deleteByPk({user_id: userId, guild_id: ban.guild_id}),
				...(ban.email ? [GuildBansByEmail.deleteByPk({guild_id: ban.guild_id, email: ban.email})] : []),
			]),
		);
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

	async getEmoji(emojiId: EmojiID, guildId: GuildID): Promise<GuildEmoji | null> {
		const emoji = await fetchOne<GuildEmojiRow>(FETCH_GUILD_EMOJI_BY_ID_QUERY, {
			guild_id: guildId,
			emoji_id: emojiId,
		});
		return emoji ? new GuildEmoji(emoji) : null;
	}

	async getEmojiById(emojiId: EmojiID): Promise<GuildEmoji | null> {
		const emoji = await fetchOne<GuildEmojiRow>(FETCH_GUILD_EMOJI_BY_EMOJI_ID_ONLY_QUERY, {
			emoji_id: emojiId,
		});
		return emoji ? new GuildEmoji(emoji) : null;
	}

	async listEmojis(guildId: GuildID): Promise<Array<GuildEmoji>> {
		const emojis = await fetchMany<GuildEmojiRow>(FETCH_GUILD_EMOJIS_BY_GUILD_ID_QUERY, {
			guild_id: guildId,
		});
		return emojis.map((emoji) => new GuildEmoji(emoji));
	}

	async upsertEmoji(data: GuildEmojiRow, oldData?: GuildEmojiRow | null): Promise<GuildEmoji> {
		const guildId = data.guild_id;
		const emojiId = data.emoji_id;
		const result = await executeVersionedUpdate<GuildEmojiRow, 'guild_id' | 'emoji_id'>(
			async () =>
				fetchOne<GuildEmojiRow>(FETCH_GUILD_EMOJI_BY_ID_QUERY, {
					guild_id: guildId,
					emoji_id: emojiId,
				}),
			(current) => ({
				pk: {guild_id: guildId, emoji_id: emojiId},
				patch: buildPatchFromData(data, current, GUILD_EMOJI_COLUMNS, ['guild_id', 'emoji_id']),
			}),
			GuildEmojis,
			{initialData: oldData},
		);
		await upsertOne(GuildEmojisByEmojiId.insert(data));
		return new GuildEmoji({...data, version: result.finalVersion ?? 1});
	}

	async deleteEmoji(guildId: GuildID, emojiId: EmojiID): Promise<void> {
		const batch = new BatchBuilder();
		batch.addPrepared(
			GuildEmojis.deleteByPk({
				guild_id: guildId,
				emoji_id: emojiId,
			}),
		);
		batch.addPrepared(GuildEmojisByEmojiId.deleteByPk({emoji_id: emojiId}));
		await batch.execute();
	}

	async getSticker(stickerId: StickerID, guildId: GuildID): Promise<GuildSticker | null> {
		const sticker = await fetchOne<GuildStickerRow>(FETCH_GUILD_STICKER_BY_ID_QUERY, {
			guild_id: guildId,
			sticker_id: stickerId,
		});
		return sticker ? new GuildSticker(sticker) : null;
	}

	async getStickerById(stickerId: StickerID): Promise<GuildSticker | null> {
		const sticker = await fetchOne<GuildStickerRow>(FETCH_GUILD_STICKER_BY_STICKER_ID_ONLY_QUERY, {
			sticker_id: stickerId,
		});
		return sticker ? new GuildSticker(sticker) : null;
	}

	async listStickers(guildId: GuildID): Promise<Array<GuildSticker>> {
		const stickers = await fetchMany<GuildStickerRow>(FETCH_GUILD_STICKERS_BY_GUILD_ID_QUERY, {
			guild_id: guildId,
		});
		return stickers.map((sticker) => new GuildSticker(sticker));
	}

	async upsertSticker(data: GuildStickerRow, oldData?: GuildStickerRow | null): Promise<GuildSticker> {
		const guildId = data.guild_id;
		const stickerId = data.sticker_id;
		const result = await executeVersionedUpdate<GuildStickerRow, 'guild_id' | 'sticker_id'>(
			async () =>
				fetchOne<GuildStickerRow>(FETCH_GUILD_STICKER_BY_ID_QUERY, {
					guild_id: guildId,
					sticker_id: stickerId,
				}),
			(current) => ({
				pk: {guild_id: guildId, sticker_id: stickerId},
				patch: buildPatchFromData(data, current, GUILD_STICKER_COLUMNS, ['guild_id', 'sticker_id']),
			}),
			GuildStickers,
			{initialData: oldData},
		);
		await upsertOne(GuildStickersByStickerId.insert(data));
		return new GuildSticker({...data, version: result.finalVersion ?? 1});
	}

	async deleteSticker(guildId: GuildID, stickerId: StickerID): Promise<void> {
		const batch = new BatchBuilder();
		batch.addPrepared(
			GuildStickers.deleteByPk({
				guild_id: guildId,
				sticker_id: stickerId,
			}),
		);
		batch.addPrepared(GuildStickersByStickerId.deleteByPk({sticker_id: stickerId}));
		await batch.execute();
	}

	async delete(guildId: GuildID, ownerId?: UserID): Promise<void> {
		const guild = await this.findUnique(guildId);
		if (!guild) {
			return;
		}
		const actualOwnerId = ownerId ?? guild.ownerId;
		const [members, roles, emojis] = await Promise.all([
			this.listMembers(guildId),
			this.listRoles(guildId),
			this.listEmojis(guildId),
		]);
		const BATCH_SIZE = 50;
		for (let i = 0; i < members.length; i += BATCH_SIZE) {
			const memberBatch = members.slice(i, i + BATCH_SIZE);
			await Promise.all(memberBatch.map((member) => this.deleteMemberRows(guildId, member.userId)));
		}
		for (let i = 0; i < roles.length; i += BATCH_SIZE) {
			const roleBatch = roles.slice(i, i + BATCH_SIZE);
			await Promise.all(roleBatch.map((role) => this.deleteRole(guildId, role.id)));
		}
		for (let i = 0; i < emojis.length; i += BATCH_SIZE) {
			const emojiBatch = emojis.slice(i, i + BATCH_SIZE);
			await Promise.all(emojiBatch.map((emoji) => this.deleteEmoji(guildId, emoji.id)));
		}
		await this.deleteGuildRow(guildId, actualOwnerId);
	}

	async deleteMember(guildId: GuildID, userId: UserID): Promise<void> {
		await this.deleteMemberRows(guildId, userId);
		if (await isTainted(guildId, {fresh: true})) {
			await enqueueRemoveThreadMemberships(guildId, userId);
		}
	}
}
