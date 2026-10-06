// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildID, UserID} from '@app/api/BrandedTypes';
import {BatchBuilder, fetchMany, fetchOne, upsertOne} from '@app/api/database/CassandraQueryExecution';
import {buildPatchFromData, executeVersionedUpdate} from '@app/api/database/CassandraVersionedUpdate';
import type {GuildMemberRow, GuildMembershipMetadataRow} from '@app/api/database/types/GuildTypes';
import {GUILD_MEMBER_COLUMNS} from '@app/api/database/types/GuildTypes';
import {IGuildMemberRepository} from '@app/api/guild/repositories/IGuildMemberRepository';
import {GuildMember} from '@app/api/models/GuildMember';
import {GuildMembers, GuildMembersByUserId, GuildMembershipMetadata} from '@app/api/Tables';

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

	async getMembershipMetadata(guildId: GuildID, userId: UserID): Promise<GuildMembershipMetadataRow | null> {
		return await fetchOne<GuildMembershipMetadataRow>(FETCH_MEMBERSHIP_METADATA_QUERY, {
			guild_id: guildId,
			user_id: userId,
		});
	}

	async upsertMembershipMetadata(data: GuildMembershipMetadataRow, ttlSeconds: number): Promise<void> {
		await upsertOne(GuildMembershipMetadata.insertWithTtl(data, ttlSeconds));
	}
}
