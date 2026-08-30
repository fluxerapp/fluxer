// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildID, UserID} from '../../BrandedTypes';
import {BatchBuilder, fetchMany, fetchOne} from '../../database/CassandraQueryExecution';
import {buildPatchFromData, executeVersionedUpdate} from '../../database/CassandraVersionedUpdate';
import {GUILD_COLUMNS, type GuildMemberByUserIdRow, type GuildRow} from '../../database/types/GuildTypes';
import type {RequestCache} from '../../middleware/RequestCacheMiddleware';
import {Guild} from '../../models/Guild';
import {GuildMembersByUserId, Guilds} from '../../Tables';
import {IGuildDataRepository} from './IGuildDataRepository';

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

export class GuildDataRepository extends IGuildDataRepository {
	constructor(private readonly requestCache?: RequestCache) {
		super();
	}

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

	async upsert(data: GuildRow, oldData?: GuildRow | null, _previousOwnerId?: UserID): Promise<Guild> {
		const guildId = data.guild_id;
		this.requestCache?.guilds.delete(guildId);
		const result = await executeVersionedUpdate<GuildRow, 'guild_id'>(
			async () => fetchOne<GuildRow>(FETCH_GUILD_BY_ID_QUERY, {guild_id: guildId}),
			(current) => ({
				pk: {guild_id: guildId},
				patch: buildPatchFromData(data, current, GUILD_COLUMNS, ['guild_id']),
			}),
			Guilds,
			{initialData: oldData},
		);
		return new Guild({...data, version: result.finalVersion ?? 0});
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

	async delete(guildId: GuildID, _ownerId?: UserID): Promise<void> {
		this.requestCache?.guilds.delete(guildId);
		const guild = await fetchOne<GuildRow>(FETCH_GUILD_BY_ID_QUERY, {guild_id: guildId});
		if (!guild) {
			return;
		}
		const batch = new BatchBuilder();
		batch.addPrepared(Guilds.deleteByPk({guild_id: guildId}));
		await batch.execute();
	}
}
