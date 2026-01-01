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
import {BatchBuilder, fetchMany} from '~/database/Cassandra';
import type {GuildMemberByUserIdRow} from '~/database/CassandraTypes';
import {GuildMembers, GuildMembersByUserId} from '~/Tables';

const FETCH_GUILD_MEMBERS_BY_USER_CQL = GuildMembersByUserId.selectCql({
	where: GuildMembersByUserId.where.eq('user_id'),
});

export class UserGuildRepository {
	async getUserGuildIds(userId: UserID): Promise<Array<GuildID>> {
		const guilds = await fetchMany<GuildMemberByUserIdRow>(FETCH_GUILD_MEMBERS_BY_USER_CQL, {
			user_id: userId,
		});
		return guilds.map((g) => g.guild_id);
	}

	async removeFromAllGuilds(userId: UserID): Promise<void> {
		const guilds = await fetchMany<GuildMemberByUserIdRow>(FETCH_GUILD_MEMBERS_BY_USER_CQL, {
			user_id: userId,
		});

		const batch = new BatchBuilder();
		for (const guild of guilds) {
			batch.addPrepared(
				GuildMembers.deleteByPk({
					guild_id: guild.guild_id,
					user_id: userId,
				}),
			);
			batch.addPrepared(
				GuildMembersByUserId.deleteByPk({
					user_id: userId,
					guild_id: guild.guild_id,
				}),
			);
		}

		await batch.execute();
	}
}
