// SPDX-License-Identifier: AGPL-3.0-or-later

import {generateSnowflake} from '@fluxer/snowflake/src/Snowflake';
import {type ChannelID, createMessageID, type MessageID, type UserID} from '../../BrandedTypes';
import {deleteOneOrMany, fetchMany, fetchOne, upsertOne} from '../../database/CassandraQueryExecution';
import type {SavedMessageRow} from '../../database/types/UserTypes';
import {SavedMessage} from '../../models/SavedMessage';
import {SavedMessages} from '../../Tables';

const createFetchSavedMessagesQuery = (limit: number) =>
	SavedMessages.select({
		where: [SavedMessages.where.eq('user_id'), SavedMessages.where.lt('message_id', 'before_message_id')],
		limit,
	});

const COUNT_SAVED_MESSAGES_CQL = SavedMessages.selectCountCql({
	where: SavedMessages.where.eq('user_id'),
});

export class SavedMessageRepository {
	async countSavedMessages(userId: UserID): Promise<number> {
		const result = await fetchOne<{
			count: bigint;
		}>(COUNT_SAVED_MESSAGES_CQL, {user_id: userId});
		return result ? Number(result.count) : 0;
	}

	async listSavedMessages(
		userId: UserID,
		limit: number = 25,
		before: MessageID = createMessageID(generateSnowflake()),
	): Promise<Array<SavedMessage>> {
		const fetchLimit = Math.max(limit * 2, 50);
		const savedMessageRows = await fetchMany<SavedMessageRow>(
			createFetchSavedMessagesQuery(fetchLimit).bind({
				user_id: userId,
				before_message_id: before,
			}),
		);
		const savedMessages: Array<SavedMessage> = [];
		for (const savedMessageRow of savedMessageRows) {
			if (savedMessages.length >= limit) break;
			savedMessages.push(new SavedMessage(savedMessageRow));
		}
		return savedMessages;
	}

	async createSavedMessage(userId: UserID, channelId: ChannelID, messageId: MessageID): Promise<SavedMessage> {
		const savedMessageRow: SavedMessageRow = {
			user_id: userId,
			channel_id: channelId,
			message_id: messageId,
			saved_at: new Date(),
		};
		await upsertOne(SavedMessages.upsertAll(savedMessageRow));
		return new SavedMessage(savedMessageRow);
	}

	async deleteSavedMessage(userId: UserID, messageId: MessageID): Promise<void> {
		await deleteOneOrMany(SavedMessages.deleteByPk({user_id: userId, message_id: messageId}));
	}

	async deleteAllSavedMessages(userId: UserID): Promise<void> {
		await deleteOneOrMany(SavedMessages.delete({where: SavedMessages.where.eq('user_id')}).bind({user_id: userId}));
	}
}
