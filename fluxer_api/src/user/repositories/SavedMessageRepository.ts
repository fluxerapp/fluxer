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

import {type ChannelID, createMessageID, type MessageID, type UserID} from '~/BrandedTypes';
import {deleteOneOrMany, fetchMany, prepared, upsertOne} from '~/database/Cassandra';
import type {SavedMessageRow} from '~/database/CassandraTypes';
import {SavedMessage} from '~/Models';
import {SavedMessages} from '~/Tables';
import * as SnowflakeUtils from '~/utils/SnowflakeUtils';

const createFetchSavedMessagesQuery = (limit: number) =>
	SavedMessages.selectCql({
		where: [SavedMessages.where.eq('user_id'), SavedMessages.where.lt('message_id', 'before_message_id')],
		limit,
	});

export class SavedMessageRepository {
	async listSavedMessages(
		userId: UserID,
		limit: number = 25,
		before: MessageID = createMessageID(SnowflakeUtils.getSnowflake()),
	): Promise<Array<SavedMessage>> {
		const fetchLimit = Math.max(limit * 2, 50);
		const savedMessageRows = await fetchMany<SavedMessageRow>(createFetchSavedMessagesQuery(fetchLimit), {
			user_id: userId,
			before_message_id: before,
		});
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
		await deleteOneOrMany(
			prepared(SavedMessages.deleteCql({where: SavedMessages.where.eq('user_id')}), {user_id: userId}),
		);
	}
}
