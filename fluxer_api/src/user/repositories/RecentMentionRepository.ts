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

import {createMessageID, type MessageID, type UserID} from '~/BrandedTypes';
import {BatchBuilder, fetchMany, fetchOne, prepared} from '~/database/Cassandra';
import type {RecentMentionRow} from '~/database/CassandraTypes';
import {RecentMention} from '~/Models';
import {RecentMentions, RecentMentionsByGuild} from '~/Tables';
import * as SnowflakeUtils from '~/utils/SnowflakeUtils';

const FETCH_RECENT_MENTION_CQL = RecentMentions.selectCql({
	where: [RecentMentions.where.eq('user_id'), RecentMentions.where.eq('message_id')],
	limit: 1,
});

const createFetchRecentMentionsQuery = (limit: number) =>
	RecentMentions.selectCql({
		where: [RecentMentions.where.eq('user_id'), RecentMentions.where.lt('message_id', 'before_message_id')],
		limit,
	});

export class RecentMentionRepository {
	async getRecentMention(userId: UserID, messageId: MessageID): Promise<RecentMention | null> {
		const mention = await fetchOne<RecentMentionRow>(FETCH_RECENT_MENTION_CQL, {
			user_id: userId,
			message_id: messageId,
		});
		return mention ? new RecentMention(mention) : null;
	}

	async listRecentMentions(
		userId: UserID,
		includeEveryone: boolean = true,
		includeRole: boolean = true,
		includeGuilds: boolean = true,
		limit: number = 25,
		before?: MessageID,
	): Promise<Array<RecentMention>> {
		const fetchLimit = Math.max(limit * 2, 50);
		const query = createFetchRecentMentionsQuery(fetchLimit);
		const params: {user_id: UserID; before_message_id: MessageID} = {
			user_id: userId,
			before_message_id: before || createMessageID(SnowflakeUtils.getSnowflake()),
		};
		const allMentions = await fetchMany<RecentMentionRow>(query, params);
		const filteredMentions = allMentions.filter((mention) => {
			if (!includeEveryone && mention.is_everyone) return false;
			if (!includeRole && mention.is_role) return false;
			if (!includeGuilds && mention.guild_id != null) return false;
			return true;
		});
		return filteredMentions.slice(0, limit).map((mention) => new RecentMention(mention));
	}

	async createRecentMention(mention: RecentMentionRow): Promise<RecentMention> {
		const batch = new BatchBuilder();
		batch.addPrepared(RecentMentions.upsertAll(mention));
		batch.addPrepared(
			RecentMentionsByGuild.insert({
				user_id: mention.user_id,
				guild_id: mention.guild_id,
				message_id: mention.message_id,
				channel_id: mention.channel_id,
				is_everyone: mention.is_everyone,
				is_role: mention.is_role,
			}),
		);
		await batch.execute();
		return new RecentMention(mention);
	}

	async createRecentMentions(mentions: Array<RecentMentionRow>): Promise<void> {
		if (mentions.length === 0) {
			return;
		}
		const batch = new BatchBuilder();
		for (const mention of mentions) {
			batch.addPrepared(RecentMentions.upsertAll(mention));
			batch.addPrepared(
				RecentMentionsByGuild.insert({
					user_id: mention.user_id,
					guild_id: mention.guild_id,
					message_id: mention.message_id,
					channel_id: mention.channel_id,
					is_everyone: mention.is_everyone,
					is_role: mention.is_role,
				}),
			);
		}
		await batch.execute();
	}

	async deleteRecentMention(mention: RecentMention): Promise<void> {
		const batch = new BatchBuilder();
		batch.addPrepared(RecentMentions.deleteByPk({user_id: mention.userId, message_id: mention.messageId}));
		batch.addPrepared(
			RecentMentionsByGuild.deleteByPk({
				user_id: mention.userId,
				guild_id: mention.guildId,
				message_id: mention.messageId,
			}),
		);
		await batch.execute();
	}

	async deleteAllRecentMentions(userId: UserID): Promise<void> {
		const mentions = await fetchMany<{guild_id: bigint; message_id: bigint}>(
			RecentMentions.selectCql({
				columns: ['guild_id', 'message_id'],
				where: RecentMentions.where.eq('user_id'),
			}),
			{
				user_id: userId,
			},
		);

		const batch = new BatchBuilder();

		batch.addPrepared(
			prepared(RecentMentions.deleteCql({where: RecentMentions.where.eq('user_id')}), {user_id: userId}),
		);

		for (const mention of mentions) {
			batch.addPrepared(
				RecentMentionsByGuild.deleteByPk({
					guild_id: mention.guild_id,
					user_id: userId,
					message_id: mention.message_id,
				}),
			);
		}

		if (batch) {
			await batch.execute();
		}
	}
}
