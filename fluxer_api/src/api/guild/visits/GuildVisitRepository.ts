// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildID, UserID} from '@app/api/BrandedTypes';
import {BatchBuilder, fetchMany, fetchOne} from '@app/api/database/CassandraQueryExecution';
import type {GuildVisitRow} from '@app/api/database/types/GuildVisitTypes';
import {GuildVisitsByGuild, GuildVisitsByUser} from '@app/api/Tables';
import {GUILD_VISIT_TTL_SECONDS} from '@fluxer/constants/src/GuildVisitConstants';

const MAX_GUILD_PAGE = 5000;
const USER_PARTITION_LIMIT = 200;

const GET_CQL = GuildVisitsByUser.selectCql({
	where: [GuildVisitsByUser.where.eq('user_id'), GuildVisitsByUser.where.eq('guild_id')],
	limit: 1,
});
const LIST_CQL = GuildVisitsByUser.selectCql({
	where: GuildVisitsByUser.where.eq('user_id'),
	limit: USER_PARTITION_LIMIT,
});
const pageByGuildQuery = (limit: number) =>
	GuildVisitsByGuild.select({
		where: [GuildVisitsByGuild.where.eq('guild_id'), GuildVisitsByGuild.where.gt('user_id', 'after_user_id')],
		limit,
	});
const firstPageByGuildQuery = (limit: number) =>
	GuildVisitsByGuild.select({where: GuildVisitsByGuild.where.eq('guild_id'), limit});
const pageAllQuery = (limit: number) =>
	GuildVisitsByUser.select({where: GuildVisitsByUser.where.tokenGt('user_id', 'after_user_id'), limit});
const firstPageAllQuery = (limit: number) => GuildVisitsByUser.select({limit});

export class GuildVisitRepository {
	async get(userId: UserID, guildId: GuildID): Promise<GuildVisitRow | null> {
		return fetchOne<GuildVisitRow>(GET_CQL, {user_id: userId, guild_id: guildId});
	}

	async list(userId: UserID): Promise<Array<GuildVisitRow>> {
		return fetchMany<GuildVisitRow>(LIST_CQL, {user_id: userId});
	}

	async upsert(row: GuildVisitRow, now: Date = new Date()): Promise<boolean> {
		let ttl: number;
		if (row.invite_expires_at === null) {
			ttl = GUILD_VISIT_TTL_SECONDS;
		} else {
			const remaining = Math.floor((row.invite_expires_at.getTime() - now.getTime()) / 1000);
			ttl = Math.max(0, Math.min(GUILD_VISIT_TTL_SECONDS, remaining));
		}
		if (ttl <= 0) return false;
		const batch = new BatchBuilder();
		batch.addPrepared(GuildVisitsByUser.insertWithTtl(row, ttl));
		batch.addPrepared(GuildVisitsByGuild.insertWithTtl(row, ttl));
		await batch.execute();
		return true;
	}

	async touch(row: GuildVisitRow, now: Date = new Date()): Promise<GuildVisitRow> {
		const touched = {...row, opened_at: now};
		await this.upsert(touched, now);
		return touched;
	}

	async delete(userId: UserID, guildId: GuildID): Promise<void> {
		const batch = new BatchBuilder();
		batch.addPrepared(GuildVisitsByUser.deleteByPk({user_id: userId, guild_id: guildId}));
		batch.addPrepared(GuildVisitsByGuild.deleteByPk({guild_id: guildId, user_id: userId}));
		await batch.execute();
	}

	async deleteAllForUser(userId: UserID): Promise<Array<GuildID>> {
		const rows = await this.list(userId);
		for (const row of rows) {
			await this.delete(userId, row.guild_id);
		}
		return rows.map((row) => row.guild_id);
	}

	async pageByGuild(
		guildId: GuildID,
		afterUserId: UserID | null,
		limit: number,
	): Promise<{rows: Array<GuildVisitRow>; next: UserID | null}> {
		const bounded = Math.max(1, Math.min(MAX_GUILD_PAGE, Math.floor(limit)));
		const rows =
			afterUserId === null
				? await fetchMany<GuildVisitRow>(firstPageByGuildQuery(bounded).bind({guild_id: guildId}))
				: await fetchMany<GuildVisitRow>(
						pageByGuildQuery(bounded).bind({guild_id: guildId, after_user_id: afterUserId}),
					);
		const next = rows.length === bounded ? rows[rows.length - 1]!.user_id : null;
		return {rows, next};
	}

	async pageVisitorIds(
		afterUserId: UserID | null,
		limit: number,
	): Promise<{userIds: Array<UserID>; next: UserID | null}> {
		const bounded = Math.max(1, Math.min(MAX_GUILD_PAGE, Math.floor(limit)));
		const rows =
			afterUserId === null
				? await fetchMany<GuildVisitRow>(firstPageAllQuery(bounded).bind({}))
				: await fetchMany<GuildVisitRow>(pageAllQuery(bounded).bind({after_user_id: afterUserId}));
		const userIds = [...new Set(rows.map((row) => row.user_id))];
		const next = rows.length === bounded ? rows[rows.length - 1]!.user_id : null;
		return {userIds, next};
	}
}
