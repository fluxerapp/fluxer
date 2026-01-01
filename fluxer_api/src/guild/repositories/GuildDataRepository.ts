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
import {BatchBuilder, buildPatchFromData, executeVersionedUpdate, fetchMany, fetchOne} from '~/database/Cassandra';
import {
	GUILD_COLUMNS,
	type GuildByOwnerIdRow,
	type GuildMemberByUserIdRow,
	type GuildRow,
} from '~/database/CassandraTypes';
import {Guild} from '~/Models';
import {GuildMembersByUserId, Guilds, GuildsByOwnerId} from '~/Tables';
import {IGuildDataRepository} from './IGuildDataRepository';

const FETCH_GUILD_BY_ID_QUERY = Guilds.selectCql({
	where: Guilds.where.eq('guild_id'),
	limit: 1,
});

const FETCH_GUILD_MEMBERS_BY_USER_QUERY = GuildMembersByUserId.select({
	columns: ['guild_id'],
	where: GuildMembersByUserId.where.eq('user_id'),
});

const FETCH_GUILDS_BY_IDS_QUERY = Guilds.selectCql({
	where: Guilds.where.in('guild_id', 'guild_ids'),
});

const createFetchAllGuildsPaginatedQuery = (limit: number) =>
	Guilds.selectCql({
		where: Guilds.where.tokenGt('guild_id', 'last_guild_id'),
		limit,
	});

const createFetchAllGuildsFirstPageQuery = (limit: number) => Guilds.selectCql({limit});

const FETCH_GUILD_IDS_BY_OWNER_QUERY = GuildsByOwnerId.select({
	columns: ['guild_id'],
	where: GuildsByOwnerId.where.eq('owner_id'),
});

export class GuildDataRepository extends IGuildDataRepository {
	async findUnique(guildId: GuildID): Promise<Guild | null> {
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
			guilds = await fetchMany<GuildRow>(query, {
				last_guild_id: lastGuildId,
			});
		} else {
			const query = createFetchAllGuildsFirstPageQuery(limit);
			guilds = await fetchMany<GuildRow>(query, {});
		}

		return guilds.map((guild) => new Guild(guild));
	}

	async listUserGuilds(userId: UserID): Promise<Array<Guild>> {
		const guildMemberships = await fetchMany<Pick<GuildMemberByUserIdRow, 'guild_id'>>(
			FETCH_GUILD_MEMBERS_BY_USER_QUERY.bind({user_id: userId}),
		);

		if (guildMemberships.length === 0) {
			return [];
		}

		const guildIds = guildMemberships.map((m) => m.guild_id);
		const guilds = await fetchMany<GuildRow>(FETCH_GUILDS_BY_IDS_QUERY, {guild_ids: guildIds});
		return guilds.map((guild) => new Guild(guild));
	}

	async countUserGuilds(userId: UserID): Promise<number> {
		const guildMemberships = await fetchMany<Pick<GuildMemberByUserIdRow, 'guild_id'>>(
			FETCH_GUILD_MEMBERS_BY_USER_QUERY.bind({user_id: userId}),
		);
		return guildMemberships.length;
	}

	async listOwnedGuildIds(userId: UserID): Promise<Array<GuildID>> {
		const guilds = await fetchMany<Pick<GuildByOwnerIdRow, 'guild_id'>>(
			FETCH_GUILD_IDS_BY_OWNER_QUERY.bind({owner_id: userId}),
		);
		return guilds.map((g) => g.guild_id as GuildID);
	}

	async upsert(data: GuildRow, oldData?: GuildRow | null): Promise<Guild> {
		const guildId = data.guild_id;

		const result = await executeVersionedUpdate<GuildRow, 'guild_id'>(
			async () => {
				if (oldData !== undefined) return oldData;
				return await fetchOne<GuildRow>(FETCH_GUILD_BY_ID_QUERY, {guild_id: guildId});
			},
			(current) => ({
				pk: {guild_id: guildId},
				patch: buildPatchFromData(data, current, GUILD_COLUMNS, ['guild_id']),
			}),
			Guilds,
			{onFailure: 'log'},
		);

		const previousOwnerId = oldData?.owner_id ?? (await this.findUnique(guildId))?.ownerId;
		if (previousOwnerId && previousOwnerId !== data.owner_id) {
			const batch = new BatchBuilder();
			batch.addPrepared(
				GuildsByOwnerId.deleteByPk({
					guild_id: guildId,
					owner_id: previousOwnerId,
				}),
			);
			batch.addPrepared(
				GuildsByOwnerId.insert({
					owner_id: data.owner_id,
					guild_id: guildId,
				}),
			);
			await batch.execute();
		} else if (!previousOwnerId) {
			await fetchOne(
				GuildsByOwnerId.insert({
					owner_id: data.owner_id,
					guild_id: guildId,
				}),
			);
		}

		return new Guild({...data, version: result.finalVersion ?? 0});
	}

	async delete(guildId: GuildID): Promise<void> {
		const guild = await this.findUnique(guildId);
		if (!guild) {
			return;
		}

		const finalBatch = new BatchBuilder();
		finalBatch.addPrepared(Guilds.deleteByPk({guild_id: guildId}));
		finalBatch.addPrepared(
			GuildsByOwnerId.deleteByPk({
				guild_id: guildId,
				owner_id: guild.ownerId,
			}),
		);
		await finalBatch.execute();
	}
}
