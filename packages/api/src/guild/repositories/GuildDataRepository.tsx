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

import type {GuildID, UserID} from '@fluxer/api/src/BrandedTypes';
import {
	BatchBuilder,
	buildPatchFromData,
	executeVersionedUpdate,
	fetchMany,
	fetchOne,
} from '@fluxer/api/src/database/Cassandra';
import {GUILD_COLUMNS, type GuildMemberByUserIdRow, type GuildRow} from '@fluxer/api/src/database/types/GuildTypes';
import {IGuildDataRepository} from '@fluxer/api/src/guild/repositories/IGuildDataRepository';
import {Guild} from '@fluxer/api/src/models/Guild';
import {GuildMembersByUserId, Guilds} from '@fluxer/api/src/Tables';

const FETCH_GUILD_BY_ID_QUERY = Guilds.selectCql({
	where: Guilds.where.eq('guild_id'),
	limit: 1,
});

const FETCH_GUILDS_BY_IDS_QUERY = Guilds.selectCql({
	where: Guilds.where.in('guild_id', 'guild_ids'),
});

const createFetchAllGuildsPaginatedQuery = (limit: number) =>
	Guilds.selectCql({
		where: Guilds.where.tokenGt('guild_id', 'last_guild_id'),
		limit,
	});

function createFetchAllGuildsFirstPageQuery(limit: number) {
	return Guilds.selectCql({limit});
}

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

	async delete(guildId: GuildID, _ownerId?: UserID): Promise<void> {
		const guild = await this.findUnique(guildId);
		if (!guild) {
			return;
		}

		const batch = new BatchBuilder();
		batch.addPrepared(Guilds.deleteByPk({guild_id: guildId}));
		await batch.execute();
	}
}
