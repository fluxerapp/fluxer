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
import {BatchBuilder, Db, deleteOneOrMany, fetchMany, fetchOne, upsertOne} from '~/database/Cassandra';
import type {ChannelMessageBucketRow, ChannelStateRow} from '~/database/CassandraTypes';
import type {Message} from '~/Models';
import {
	AttachmentLookup,
	ChannelEmptyBuckets,
	ChannelMessageBuckets,
	ChannelPins,
	ChannelState,
	MessageReactions,
	Messages,
	MessagesByAuthor,
} from '~/Tables';
import * as BucketUtils from '~/utils/BucketUtils';
import type {MessageDataRepository} from './MessageDataRepository';

const BULK_DELETE_BATCH_SIZE = 100;
const POST_DELETE_BUCKET_CHECK_LIMIT = 25;

const HAS_ANY_MESSAGE_IN_BUCKET = Messages.select({
	columns: ['message_id'],
	where: [Messages.where.eq('channel_id'), Messages.where.eq('bucket')],
	limit: 1,
});

const FETCH_CHANNEL_STATE = ChannelState.select({
	where: ChannelState.where.eq('channel_id'),
	limit: 1,
});

const LIST_BUCKETS_DESC = ChannelMessageBuckets.select({
	columns: ['bucket'],
	where: ChannelMessageBuckets.where.eq('channel_id'),
	orderBy: {col: 'bucket', direction: 'DESC'},
	limit: POST_DELETE_BUCKET_CHECK_LIMIT,
});

const FETCH_LATEST_MESSAGE_ID_IN_BUCKET = Messages.select({
	columns: ['message_id'],
	where: [Messages.where.eq('channel_id'), Messages.where.eq('bucket')],
	limit: 1,
});

export class MessageDeletionRepository {
	constructor(private messageDataRepo: MessageDataRepository) {}

	private addMessageDeletionBatchQueries(
		batch: BatchBuilder,
		channelId: ChannelID,
		messageId: MessageID,
		bucket: number,
		message: Message | null,
		authorId?: UserID,
		pinnedTimestamp?: Date,
	): void {
		batch.addPrepared(
			Messages.deleteByPk({
				channel_id: channelId,
				bucket,
				message_id: messageId,
			}),
		);

		const effectiveAuthorId = authorId ?? message?.authorId ?? null;
		if (effectiveAuthorId) {
			batch.addPrepared(
				MessagesByAuthor.deleteByPk({
					author_id: effectiveAuthorId,
					channel_id: channelId,
					message_id: messageId,
				}),
			);
		}

		const effectivePinned = pinnedTimestamp ?? message?.pinnedTimestamp ?? null;
		if (effectivePinned) {
			batch.addPrepared(
				ChannelPins.deleteByPk({
					channel_id: channelId,
					message_id: messageId,
					pinned_timestamp: effectivePinned,
				}),
			);
		}

		batch.addPrepared(
			MessageReactions.deletePartition({
				channel_id: channelId,
				bucket,
				message_id: messageId,
			}),
		);

		if (message?.attachments) {
			for (const attachment of message.attachments) {
				batch.addPrepared(
					AttachmentLookup.deleteByPk({
						channel_id: channelId,
						attachment_id: attachment.id,
						filename: attachment.filename,
					}),
				);
			}
		}
	}

	private async markBucketEmpty(channelId: ChannelID, bucket: number): Promise<void> {
		const batch = new BatchBuilder();

		batch.addPrepared(
			ChannelMessageBuckets.deleteByPk({
				channel_id: channelId,
				bucket,
			}),
		);

		batch.addPrepared(
			ChannelEmptyBuckets.upsertAll({
				channel_id: channelId,
				bucket,
				updated_at: new Date(),
			}),
		);

		await batch.execute(true);
	}

	private async isBucketEmpty(channelId: ChannelID, bucket: number): Promise<boolean> {
		const row = await fetchOne<{message_id: bigint}>(
			HAS_ANY_MESSAGE_IN_BUCKET.bind({
				channel_id: channelId,
				bucket,
			}),
		);
		return row == null;
	}

	private async reconcileChannelStateIfNeeded(
		channelId: ChannelID,
		deletedMessageIds: Array<MessageID>,
		emptiedBuckets: Set<number>,
	): Promise<void> {
		const state = await fetchOne<ChannelStateRow>(FETCH_CHANNEL_STATE.bind({channel_id: channelId}));
		if (!state) return;

		const lastBucket = state.last_message_bucket as number | null | undefined;
		const lastId = state.last_message_id as MessageID | null | undefined;

		const touchedLast =
			(lastBucket != null && emptiedBuckets.has(lastBucket)) || (lastId != null && deletedMessageIds.includes(lastId));

		if (!touchedLast) return;

		const bucketRows = await fetchMany<Pick<ChannelMessageBucketRow, 'bucket'>>(
			LIST_BUCKETS_DESC.bind({channel_id: channelId}),
		);

		for (const {bucket} of bucketRows) {
			const latest = await fetchOne<{message_id: bigint}>(
				FETCH_LATEST_MESSAGE_ID_IN_BUCKET.bind({channel_id: channelId, bucket}),
			);

			if (!latest) {
				await this.markBucketEmpty(channelId, bucket);
				continue;
			}

			await upsertOne(
				ChannelState.patchByPk(
					{channel_id: channelId},
					{
						has_messages: Db.set(true),
						last_message_bucket: Db.set(bucket),
						last_message_id: Db.set(latest.message_id as MessageID),
						updated_at: Db.set(new Date()),
					},
				),
			);
			return;
		}

		await upsertOne(
			ChannelState.patchByPk(
				{channel_id: channelId},
				{
					has_messages: Db.set(false),
					last_message_bucket: Db.clear(),
					last_message_id: Db.clear(),
					updated_at: Db.set(new Date()),
				},
			),
		);
	}

	private async postDeleteMaintenance(
		channelId: ChannelID,
		affectedBuckets: Set<number>,
		deletedMessageIds: Array<MessageID>,
	): Promise<void> {
		const emptiedBuckets = new Set<number>();

		for (const bucket of affectedBuckets) {
			const empty = await this.isBucketEmpty(channelId, bucket);
			if (!empty) continue;

			emptiedBuckets.add(bucket);
			await this.markBucketEmpty(channelId, bucket);
		}

		if (emptiedBuckets.size > 0 || deletedMessageIds.length > 0) {
			await this.reconcileChannelStateIfNeeded(channelId, deletedMessageIds, emptiedBuckets);
		}
	}

	async deleteMessage(
		channelId: ChannelID,
		messageId: MessageID,
		authorId: UserID,
		pinnedTimestamp?: Date,
	): Promise<void> {
		const bucket = BucketUtils.makeBucket(messageId);
		const message = await this.messageDataRepo.getMessage(channelId, messageId);

		const batch = new BatchBuilder();
		this.addMessageDeletionBatchQueries(batch, channelId, messageId, bucket, message, authorId, pinnedTimestamp);
		await batch.execute();

		await this.postDeleteMaintenance(channelId, new Set([bucket]), [messageId]);
	}

	async bulkDeleteMessages(channelId: ChannelID, messageIds: Array<MessageID>): Promise<void> {
		if (messageIds.length === 0) return;

		for (let i = 0; i < messageIds.length; i += BULK_DELETE_BATCH_SIZE) {
			const chunk = messageIds.slice(i, i + BULK_DELETE_BATCH_SIZE);
			const messages = await Promise.all(chunk.map((id) => this.messageDataRepo.getMessage(channelId, id)));

			const affectedBuckets = new Set<number>();
			const batch = new BatchBuilder();

			for (let j = 0; j < chunk.length; j++) {
				const messageId = chunk[j];
				const message = messages[j];
				const bucket = BucketUtils.makeBucket(messageId);
				affectedBuckets.add(bucket);
				this.addMessageDeletionBatchQueries(batch, channelId, messageId, bucket, message);
			}

			await batch.execute();
			await this.postDeleteMaintenance(channelId, affectedBuckets, chunk);
		}
	}

	async deleteAllChannelMessages(channelId: ChannelID): Promise<void> {
		const BATCH_SIZE = 50;
		let hasMore = true;
		let beforeMessageId: MessageID | undefined;

		const allDeleted: Array<MessageID> = [];
		const affectedBuckets = new Set<number>();

		while (hasMore) {
			const messages = await this.messageDataRepo.listMessages(channelId, beforeMessageId, 100);

			if (messages.length === 0) {
				hasMore = false;
				break;
			}

			for (let i = 0; i < messages.length; i += BATCH_SIZE) {
				const batch = new BatchBuilder();
				const messageBatch = messages.slice(i, i + BATCH_SIZE);

				for (const message of messageBatch) {
					const bucket = BucketUtils.makeBucket(message.id);
					affectedBuckets.add(bucket);
					allDeleted.push(message.id);

					this.addMessageDeletionBatchQueries(
						batch,
						channelId,
						message.id,
						bucket,
						message,
						message.authorId ?? undefined,
						message.pinnedTimestamp || undefined,
					);
				}

				await batch.execute();
			}

			if (messages.length < 100) {
				hasMore = false;
			} else {
				beforeMessageId = messages[messages.length - 1].id;
			}
		}

		await this.postDeleteMaintenance(channelId, affectedBuckets, allDeleted);

		await deleteOneOrMany(
			ChannelMessageBuckets.deletePartition({
				channel_id: channelId,
			}),
		);

		await deleteOneOrMany(
			ChannelEmptyBuckets.deletePartition({
				channel_id: channelId,
			}),
		);
	}
}
