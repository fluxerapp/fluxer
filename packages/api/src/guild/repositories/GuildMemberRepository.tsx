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
import type {GuildMemberRow} from '@fluxer/api/src/database/types/GuildTypes';
import {GUILD_MEMBER_COLUMNS} from '@fluxer/api/src/database/types/GuildTypes';
import {IGuildMemberRepository} from '@fluxer/api/src/guild/repositories/IGuildMemberRepository';
import {GuildMember} from '@fluxer/api/src/models/GuildMember';
import {GuildMembers, GuildMembersByUserId} from '@fluxer/api/src/Tables';

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

function createPaginatedFirstPageQuery(limit: number) {
	return GuildMembers.selectCql({
		where: GuildMembers.where.eq('guild_id'),
		limit,
	});
}

function createPaginatedQuery(limit: number) {
	return GuildMembers.selectCql({
		where: [GuildMembers.where.eq('guild_id'), GuildMembers.where.gt('user_id')],
		limit,
	});
}

export class GuildMemberRepository extends IGuildMemberRepository {
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
		const result = await fetchOne<{count: bigint}>(COUNT_GUILD_MEMBERS_BY_GUILD_ID_QUERY, {
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

		await fetchOne(
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
			rows = await fetchMany<GuildMemberRow>(createPaginatedQuery(limit), {
				guild_id: guildId,
				user_id: afterUserId,
			});
		} else {
			rows = await fetchMany<GuildMemberRow>(createPaginatedFirstPageQuery(limit), {
				guild_id: guildId,
			});
		}
		return rows.map((row) => new GuildMember(row));
	}

	async deleteMember(guildId: GuildID, userId: UserID): Promise<void> {
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
}
