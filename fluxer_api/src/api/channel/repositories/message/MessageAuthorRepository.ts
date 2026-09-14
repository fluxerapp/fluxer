// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, MessageID, UserID} from '@app/api/BrandedTypes';
import {createChannelID, createMessageID} from '@app/api/BrandedTypes';
import type {MessageDataRepository} from '@app/api/channel/repositories/message/MessageDataRepository';
import type {MessageDeletionRepository} from '@app/api/channel/repositories/message/MessageDeletionRepository';
import {deleteOneOrMany, fetchMany, fetchOne, upsertOne} from '@app/api/database/CassandraQueryExecution';
import {Db} from '@app/api/database/CassandraTypes';
import {Messages, MessagesByAuthorV2} from '@app/api/Tables';
import * as BucketUtils from '@fluxer/snowflake/src/SnowflakeBuckets';

const SELECT_MESSAGE_BY_AUTHOR = MessagesByAuthorV2.select({
	where: [MessagesByAuthorV2.where.eq('author_id'), MessagesByAuthorV2.where.eq('message_id')],
	limit: 1,
});

function listMessagesByAuthorQuery(limit: number, usePagination: boolean) {
	return MessagesByAuthorV2.select({
		columns: ['channel_id', 'message_id'],
		where: usePagination
			? [MessagesByAuthorV2.where.eq('author_id'), MessagesByAuthorV2.where.lt('message_id', 'last_message_id')]
			: [MessagesByAuthorV2.where.eq('author_id')],
		orderBy: {col: 'message_id', direction: 'DESC'},
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
		lastMessageId?: MessageID,
	): Promise<
		Array<{
			channelId: ChannelID;
			messageId: MessageID;
		}>
	> {
		const usePagination = Boolean(lastMessageId);
		const q = listMessagesByAuthorQuery(limit, usePagination);
		const results = await fetchMany<{
			channel_id: bigint;
			message_id: bigint;
		}>(
			usePagination
				? q.bind({
						author_id: authorId,
						last_message_id: lastMessageId!,
					})
				: q.bind({
						author_id: authorId,
					}),
		);
		return results.map((r) => ({
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

	async hasMessageByAuthor(authorId: UserID, _channelId: ChannelID, messageId: MessageID): Promise<boolean> {
		const result = await fetchOne<{
			channel_id: bigint;
			message_id: bigint;
		}>(
			SELECT_MESSAGE_BY_AUTHOR.bind({
				author_id: authorId,
				message_id: messageId,
			}),
		);
		return result !== null;
	}

	async anonymizeMessage(channelId: ChannelID, messageId: MessageID, newAuthorId: UserID): Promise<void> {
		const bucket = BucketUtils.makeBucket(messageId);
		const message = await this.messageDataRepo.getMessage(channelId, messageId);
		if (!message) return;
		if (message.authorId != null) {
			await deleteOneOrMany(
				MessagesByAuthorV2.deleteByPk({
					author_id: message.authorId,
					message_id: messageId,
				}),
			);
		}
		await upsertOne(
			MessagesByAuthorV2.upsertAll({
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
