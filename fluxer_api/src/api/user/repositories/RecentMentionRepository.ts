// SPDX-License-Identifier: AGPL-3.0-or-later

import {createMessageID, type MessageID, type UserID} from '@app/api/BrandedTypes';
import {BatchBuilder, fetchMany, fetchOne} from '@app/api/database/CassandraQueryExecution';
import type {RecentMentionRow} from '@app/api/database/types/UserTypes';
import {RecentMention} from '@app/api/models/RecentMention';
import {RecentMentions, RecentMentionsByGuild} from '@app/api/Tables';
import {generateSnowflake} from '@fluxer/snowflake/src/Snowflake';

const FETCH_RECENT_MENTION_CQL = RecentMentions.selectCql({
	where: [RecentMentions.where.eq('user_id'), RecentMentions.where.eq('message_id')],
	limit: 1,
});
const createFetchRecentMentionsQuery = (limit: number) =>
	RecentMentions.select({
		where: [RecentMentions.where.eq('user_id'), RecentMentions.where.lt('message_id', 'before_message_id')],
		limit,
	});
const BULK_RECENT_MENTION_BATCH_QUERY_LIMIT = 50;

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
		let beforeMessageId = before || createMessageID(generateSnowflake());
		const filteredMentions: Array<RecentMentionRow> = [];
		while (filteredMentions.length < limit) {
			const allMentions = await fetchMany<RecentMentionRow>(
				query.bind({
					user_id: userId,
					before_message_id: beforeMessageId,
				}),
			);
			if (allMentions.length === 0) break;
			filteredMentions.push(
				...allMentions.filter((mention) => this.matchesFilters(mention, includeEveryone, includeRole, includeGuilds)),
			);
			beforeMessageId = allMentions[allMentions.length - 1].message_id;
			if (allMentions.length < fetchLimit) break;
		}
		return filteredMentions.slice(0, limit).map((mention) => new RecentMention(mention));
	}

	private matchesFilters(
		mention: RecentMentionRow,
		includeEveryone: boolean,
		includeRole: boolean,
		includeGuilds: boolean,
	): boolean {
		if (!includeEveryone && mention.is_everyone) return false;
		if (!includeRole && mention.is_role) return false;
		if (!includeGuilds && mention.guild_id != null) return false;
		return true;
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
		await batch.executeChunked(BULK_RECENT_MENTION_BATCH_QUERY_LIMIT, false);
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

	async deleteRecentMentions(mentions: Array<RecentMention>): Promise<void> {
		if (mentions.length === 0) {
			return;
		}
		const batch = new BatchBuilder();
		for (const mention of mentions) {
			batch.addPrepared(RecentMentions.deleteByPk({user_id: mention.userId, message_id: mention.messageId}));
			batch.addPrepared(
				RecentMentionsByGuild.deleteByPk({
					user_id: mention.userId,
					guild_id: mention.guildId,
					message_id: mention.messageId,
				}),
			);
		}
		await batch.executeChunked(BULK_RECENT_MENTION_BATCH_QUERY_LIMIT, false);
	}

	async deleteAllRecentMentions(userId: UserID): Promise<void> {
		const mentions = await fetchMany<{
			guild_id: bigint;
			message_id: bigint;
		}>(
			RecentMentions.selectCql({
				columns: ['guild_id', 'message_id'],
				where: RecentMentions.where.eq('user_id'),
			}),
			{
				user_id: userId,
			},
		);
		const batch = new BatchBuilder();
		batch.addPrepared(RecentMentions.delete({where: RecentMentions.where.eq('user_id')}).bind({user_id: userId}));
		for (const mention of mentions) {
			batch.addPrepared(
				RecentMentionsByGuild.deleteByPk({
					guild_id: mention.guild_id,
					user_id: userId,
					message_id: mention.message_id,
				}),
			);
		}
		await batch.executeChunked(BULK_RECENT_MENTION_BATCH_QUERY_LIMIT, false);
	}
}
