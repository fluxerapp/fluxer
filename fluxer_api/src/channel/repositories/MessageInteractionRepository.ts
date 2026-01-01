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

import type {ChannelID, EmojiID, MessageID, UserID} from '~/BrandedTypes';
import {createEmojiID} from '~/BrandedTypes';
import {MAX_USERS_PER_MESSAGE_REACTION} from '~/Constants';
import {Db, deleteOneOrMany, fetchMany, fetchOne, upsertOne} from '~/database/Cassandra';
import type {ChannelPinRow, MessageReactionRow} from '~/database/CassandraTypes';
import {type Message, MessageReaction} from '~/Models';
import {ChannelPins, MessageReactions, Messages} from '~/Tables';
import * as BucketUtils from '~/utils/BucketUtils';
import {IMessageInteractionRepository} from './IMessageInteractionRepository';
import type {MessageRepository} from './MessageRepository';

const createFetchChannelPinsQuery = (limit: number) =>
	ChannelPins.selectCql({
		where: [ChannelPins.where.eq('channel_id'), ChannelPins.where.lt('pinned_timestamp', 'before_pinned_timestamp')],
		limit,
	});

const FETCH_MESSAGE_REACTIONS_BY_CHANNEL_AND_MESSAGE_QUERY = MessageReactions.selectCql({
	where: [
		MessageReactions.where.eq('channel_id'),
		MessageReactions.where.eq('bucket'),
		MessageReactions.where.eq('message_id'),
	],
});

const CHECK_MESSAGE_HAS_REACTIONS_QUERY = MessageReactions.selectCql({
	columns: ['channel_id'],
	where: [
		MessageReactions.where.eq('channel_id'),
		MessageReactions.where.eq('bucket'),
		MessageReactions.where.eq('message_id'),
	],
	limit: 1,
});

const createFetchReactionUsersByEmojiQuery = (limit: number, hasAfter: boolean = false) =>
	MessageReactions.selectCql({
		where: hasAfter
			? [
					MessageReactions.where.eq('channel_id'),
					MessageReactions.where.eq('bucket'),
					MessageReactions.where.eq('message_id'),
					MessageReactions.where.eq('emoji_id'),
					MessageReactions.where.eq('emoji_name'),
					MessageReactions.where.gt('user_id', 'after_user_id'),
				]
			: [
					MessageReactions.where.eq('channel_id'),
					MessageReactions.where.eq('bucket'),
					MessageReactions.where.eq('message_id'),
					MessageReactions.where.eq('emoji_id'),
					MessageReactions.where.eq('emoji_name'),
				],
		limit,
	});

const CHECK_USER_REACTION_EXISTS_QUERY = MessageReactions.selectCql({
	columns: ['channel_id', 'bucket', 'message_id', 'user_id', 'emoji_id', 'emoji_name'],
	where: [
		MessageReactions.where.eq('channel_id'),
		MessageReactions.where.eq('bucket'),
		MessageReactions.where.eq('message_id'),
		MessageReactions.where.eq('user_id'),
		MessageReactions.where.eq('emoji_id'),
		MessageReactions.where.eq('emoji_name'),
	],
	limit: 1,
});

export class MessageInteractionRepository extends IMessageInteractionRepository {
	private messageRepository: MessageRepository;

	constructor(messageRepository: MessageRepository) {
		super();
		this.messageRepository = messageRepository;
	}

	async listChannelPins(
		channelId: ChannelID,
		beforePinnedTimestamp: Date,
		limit: number = 50,
	): Promise<Array<Message>> {
		const pins = await fetchMany<ChannelPinRow>(createFetchChannelPinsQuery(limit), {
			channel_id: channelId,
			before_pinned_timestamp: beforePinnedTimestamp,
		});

		const messages: Array<Message> = [];
		for (const pin of pins) {
			const message = await this.messageRepository.getMessage(channelId, pin.message_id);
			if (message) {
				messages.push(message);
			}
		}

		return messages;
	}

	async addChannelPin(channelId: ChannelID, messageId: MessageID, pinnedTimestamp: Date): Promise<void> {
		await upsertOne(
			ChannelPins.upsertAll({
				channel_id: channelId,
				message_id: messageId,
				pinned_timestamp: pinnedTimestamp,
			}),
		);
	}

	async removeChannelPin(channelId: ChannelID, messageId: MessageID): Promise<void> {
		const message = await this.messageRepository.getMessage(channelId, messageId);

		if (!message || !message.pinnedTimestamp) {
			return;
		}

		await deleteOneOrMany(
			ChannelPins.deleteByPk({
				channel_id: channelId,
				pinned_timestamp: message.pinnedTimestamp,
				message_id: messageId,
			}),
		);
	}

	async listMessageReactions(channelId: ChannelID, messageId: MessageID): Promise<Array<MessageReaction>> {
		const bucket = BucketUtils.makeBucket(messageId);
		const reactions = await fetchMany<MessageReactionRow>(FETCH_MESSAGE_REACTIONS_BY_CHANNEL_AND_MESSAGE_QUERY, {
			channel_id: channelId,
			bucket,
			message_id: messageId,
		});
		return reactions.map((reaction) => new MessageReaction(reaction));
	}

	async listReactionUsers(
		channelId: ChannelID,
		messageId: MessageID,
		emojiName: string,
		limit: number = 25,
		after?: UserID,
		emojiId?: EmojiID,
	): Promise<Array<MessageReaction>> {
		const bucket = BucketUtils.makeBucket(messageId);
		const normalizedEmojiId = emojiId ?? createEmojiID(0n);
		const hasAfter = !!after;

		const reactions = hasAfter
			? await fetchMany<MessageReactionRow>(
					createFetchReactionUsersByEmojiQuery(Math.min(limit, MAX_USERS_PER_MESSAGE_REACTION), true),
					{
						channel_id: channelId,
						bucket,
						message_id: messageId,
						emoji_id: normalizedEmojiId,
						emoji_name: emojiName,
						after_user_id: after!,
					},
				)
			: await fetchMany<MessageReactionRow>(
					createFetchReactionUsersByEmojiQuery(Math.min(limit, MAX_USERS_PER_MESSAGE_REACTION), false),
					{
						channel_id: channelId,
						bucket,
						message_id: messageId,
						emoji_id: normalizedEmojiId,
						emoji_name: emojiName,
					},
				);

		return reactions.map((reaction) => new MessageReaction(reaction));
	}

	async addReaction(
		channelId: ChannelID,
		messageId: MessageID,
		userId: UserID,
		emojiName: string,
		emojiId?: EmojiID,
		emojiAnimated: boolean = false,
	): Promise<MessageReaction> {
		const bucket = BucketUtils.makeBucket(messageId);
		const normalizedEmojiId = emojiId ? emojiId : createEmojiID(0n);

		const reactionData: MessageReactionRow = {
			channel_id: channelId,
			bucket,
			message_id: messageId,
			user_id: userId,
			emoji_id: normalizedEmojiId,
			emoji_name: emojiName,
			emoji_animated: emojiAnimated,
		};

		await upsertOne(MessageReactions.upsertAll(reactionData));
		await this.setHasReaction(channelId, messageId, true);
		return new MessageReaction(reactionData);
	}

	async removeReaction(
		channelId: ChannelID,
		messageId: MessageID,
		userId: UserID,
		emojiName: string,
		emojiId?: EmojiID,
	): Promise<void> {
		const bucket = BucketUtils.makeBucket(messageId);
		const normalizedEmojiId = emojiId ?? createEmojiID(0n);

		await deleteOneOrMany(
			MessageReactions.deleteByPk({
				channel_id: channelId,
				bucket,
				message_id: messageId,
				user_id: userId,
				emoji_id: normalizedEmojiId,
				emoji_name: emojiName,
			}),
		);

		const hasReactions = await this.messageHasAnyReactions(channelId, messageId);
		await this.setHasReaction(channelId, messageId, hasReactions);
	}

	async removeAllReactions(channelId: ChannelID, messageId: MessageID): Promise<void> {
		const bucket = BucketUtils.makeBucket(messageId);
		const deleteQuery = MessageReactions.deleteCql({
			where: [
				MessageReactions.where.eq('channel_id'),
				MessageReactions.where.eq('bucket'),
				MessageReactions.where.eq('message_id'),
			],
		});
		await deleteOneOrMany(deleteQuery, {
			channel_id: channelId,
			bucket,
			message_id: messageId,
		});

		const hasReactions = await this.messageHasAnyReactions(channelId, messageId);
		await this.setHasReaction(channelId, messageId, hasReactions);
	}

	async removeAllReactionsForEmoji(
		channelId: ChannelID,
		messageId: MessageID,
		emojiName: string,
		emojiId?: EmojiID,
	): Promise<void> {
		const bucket = BucketUtils.makeBucket(messageId);
		const normalizedEmojiId = emojiId ?? createEmojiID(0n);

		const deleteQuery = MessageReactions.deleteCql({
			where: [
				MessageReactions.where.eq('channel_id'),
				MessageReactions.where.eq('bucket'),
				MessageReactions.where.eq('message_id'),
				MessageReactions.where.eq('emoji_id'),
				MessageReactions.where.eq('emoji_name'),
			],
		});
		await deleteOneOrMany(deleteQuery, {
			channel_id: channelId,
			bucket,
			message_id: messageId,
			emoji_id: normalizedEmojiId,
			emoji_name: emojiName,
		});

		const hasReactions = await this.messageHasAnyReactions(channelId, messageId);
		await this.setHasReaction(channelId, messageId, hasReactions);
	}

	async countReactionUsers(
		channelId: ChannelID,
		messageId: MessageID,
		emojiName: string,
		emojiId?: EmojiID,
	): Promise<number> {
		const reactions = await this.listReactionUsers(channelId, messageId, emojiName, undefined, undefined, emojiId);
		return reactions.length;
	}

	async countUniqueReactions(channelId: ChannelID, messageId: MessageID): Promise<number> {
		const reactions = await this.listMessageReactions(channelId, messageId);
		const uniqueEmojis = new Set<string>();

		for (const reaction of reactions) {
			const emojiKey = `${reaction.emojiId}:${reaction.emojiName}`;
			uniqueEmojis.add(emojiKey);
		}

		return uniqueEmojis.size;
	}

	async checkUserReactionExists(
		channelId: ChannelID,
		messageId: MessageID,
		userId: UserID,
		emojiName: string,
		emojiId?: EmojiID,
	): Promise<boolean> {
		const bucket = BucketUtils.makeBucket(messageId);
		const normalizedEmojiId = emojiId ?? createEmojiID(0n);

		const reaction = await fetchOne<MessageReactionRow>(CHECK_USER_REACTION_EXISTS_QUERY, {
			channel_id: channelId,
			bucket,
			message_id: messageId,
			user_id: userId,
			emoji_id: normalizedEmojiId,
			emoji_name: emojiName,
		});

		return !!reaction;
	}

	async setHasReaction(channelId: ChannelID, messageId: MessageID, hasReaction: boolean): Promise<void> {
		const bucket = BucketUtils.makeBucket(messageId);

		await upsertOne(
			Messages.patchByPk(
				{
					channel_id: channelId,
					bucket,
					message_id: messageId,
				},
				{
					has_reaction: Db.set(hasReaction),
				},
			),
		);
	}

	private async messageHasAnyReactions(channelId: ChannelID, messageId: MessageID): Promise<boolean> {
		const bucket = BucketUtils.makeBucket(messageId);
		const row = await fetchOne<Pick<MessageReactionRow, 'channel_id'>>(CHECK_MESSAGE_HAS_REACTIONS_QUERY, {
			channel_id: channelId,
			bucket,
			message_id: messageId,
		});
		return Boolean(row);
	}
}
