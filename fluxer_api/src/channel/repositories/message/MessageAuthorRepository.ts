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

import type {ChannelID, MessageID, UserID} from '~/BrandedTypes';
import {createChannelID, createMessageID} from '~/BrandedTypes';
import {Db, deleteOneOrMany, fetchMany, fetchOne, upsertOne} from '~/database/Cassandra';
import {Messages, MessagesByAuthor} from '~/Tables';
import * as BucketUtils from '~/utils/BucketUtils';
import type {MessageDataRepository} from './MessageDataRepository';
import type {MessageDeletionRepository} from './MessageDeletionRepository';

const SELECT_MESSAGE_BY_AUTHOR = MessagesByAuthor.select({
	where: [
		MessagesByAuthor.where.eq('author_id'),
		MessagesByAuthor.where.eq('channel_id'),
		MessagesByAuthor.where.eq('message_id'),
	],
	limit: 1,
});

function listMessagesByAuthorQuery(limit: number, usePagination: boolean) {
	return MessagesByAuthor.select({
		columns: ['channel_id', 'message_id'],
		where: usePagination
			? [
					MessagesByAuthor.where.eq('author_id'),
					MessagesByAuthor.where.tupleGt(['channel_id', 'message_id'], ['last_channel_id', 'last_message_id']),
				]
			: [MessagesByAuthor.where.eq('author_id')],
		limit,
	});
}

export class MessageAuthorRepository {
	constructor(
		private messageDataRepo: MessageDataRepository,
		private messageDeletionRepo: MessageDeletionRepository,
	) {}

	async listMessagesByAuthor(
		authorId: UserID,
		limit: number = 1000,
		lastChannelId?: ChannelID,
		lastMessageId?: MessageID,
	): Promise<Array<{channelId: ChannelID; messageId: MessageID}>> {
		const usePagination = Boolean(lastChannelId && lastMessageId);

		const q = listMessagesByAuthorQuery(limit, usePagination);

		const results = await fetchMany<{channel_id: bigint; message_id: bigint}>(
			usePagination
				? q.bind({
						author_id: authorId,
						last_channel_id: lastChannelId!,
						last_message_id: lastMessageId!,
					})
				: q.bind({
						author_id: authorId,
					}),
		);

		let filteredResults = results;
		if (lastChannelId && lastMessageId) {
			filteredResults = results.filter((r) => {
				const channelId = createChannelID(r.channel_id);
				const messageId = createMessageID(r.message_id);
				return channelId > lastChannelId || (channelId === lastChannelId && messageId > lastMessageId);
			});
		}

		return filteredResults.map((r) => ({
			channelId: createChannelID(r.channel_id),
			messageId: createMessageID(r.message_id),
		}));
	}

	async deleteMessagesByAuthor(
		authorId: UserID,
		channelIds?: Array<ChannelID>,
		messageIds?: Array<MessageID>,
	): Promise<void> {
		const messagesToDelete = await this.listMessagesByAuthor(authorId);

		for (const {channelId, messageId} of messagesToDelete) {
			if (channelIds && !channelIds.includes(channelId)) continue;
			if (messageIds && !messageIds.includes(messageId)) continue;

			const message = await this.messageDataRepo.getMessage(channelId, messageId);
			if (message && message.authorId === authorId) {
				await this.messageDeletionRepo.deleteMessage(
					channelId,
					messageId,
					authorId,
					message.pinnedTimestamp || undefined,
				);
			}
		}
	}

	async hasMessageByAuthor(authorId: UserID, channelId: ChannelID, messageId: MessageID): Promise<boolean> {
		const result = await fetchOne<{channel_id: bigint; message_id: bigint}>(
			SELECT_MESSAGE_BY_AUTHOR.bind({
				author_id: authorId,
				channel_id: channelId,
				message_id: messageId,
			}),
		);

		return result !== null;
	}

	async anonymizeMessage(channelId: ChannelID, messageId: MessageID, newAuthorId: UserID): Promise<void> {
		const bucket = BucketUtils.makeBucket(messageId);

		const message = await this.messageDataRepo.getMessage(channelId, messageId);
		if (!message) return;

		if (message.authorId) {
			await deleteOneOrMany(
				MessagesByAuthor.deleteByPk({
					author_id: message.authorId,
					channel_id: channelId,
					message_id: messageId,
				}),
			);
		}

		await upsertOne(
			MessagesByAuthor.upsertAll({
				author_id: newAuthorId,
				channel_id: channelId,
				message_id: messageId,
			}),
		);

		await upsertOne(
			Messages.patchByPk(
				{
					channel_id: channelId,
					bucket,
					message_id: messageId,
				},
				{
					author_id: Db.set(newAuthorId),
				},
			),
		);
	}
}
