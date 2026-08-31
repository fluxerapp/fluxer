// SPDX-License-Identifier: AGPL-3.0-or-later

import type {MessagePollAnswerVotersPage, MessagePollSelectedAnswer, MessagePollVoteRow} from '@app/api/database/types/PollTypes';
import * as BucketUtils from '@fluxer/snowflake/src/SnowflakeBuckets';
import type {ChannelID, EmojiID, MessageID, UserID} from '../../BrandedTypes';
import {createEmojiID} from '../../BrandedTypes';
import {BatchBuilder, deleteOneOrMany, fetchMany, fetchOne, upsertOne} from '../../database/CassandraQueryExecution';
import {Db} from '../../database/CassandraTypes';
import type {ChannelPinRow, MessageReactionRow} from '../../database/types/MessageTypes';
import type {Message} from '../../models/Message';
import {MessageReaction} from '../../models/MessageReaction';
import {ChannelPins, MessagePollVotes, MessageReactions, Messages} from '../../Tables';
import {IMessageInteractionRepository} from './IMessageInteractionRepository';
import type {MessageRepository} from './MessageRepository';

const createFetchChannelPinsQuery = (limit: number) =>
	ChannelPins.select({
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
	MessageReactions.select({
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
const CHECK_USER_VOTE_ANSWERS_QUERY = MessagePollVotes.select({
	columns: ['answer_id'],
	where: [
		MessagePollVotes.where.eq('bucket'),
		MessagePollVotes.where.eq('channel_id'),
		MessagePollVotes.where.eq('message_id'),
		MessagePollVotes.where.eq('user_id'),
	],
});
const createCheckAnswerVotesQuery = (limit: number, hasAfter: boolean = false) =>
	MessagePollVotes.select({
		columns: ['user_id'],
		where: hasAfter
			? [
					MessagePollVotes.where.eq('bucket'),
					MessagePollVotes.where.eq('channel_id'),
					MessagePollVotes.where.eq('message_id'),
					MessagePollVotes.where.eq('answer_id'),
					MessagePollVotes.where.gt('user_id', 'after_user_id'),
				]
			: [
					MessagePollVotes.where.eq('bucket'),
					MessagePollVotes.where.eq('channel_id'),
					MessagePollVotes.where.eq('message_id'),
					MessagePollVotes.where.eq('answer_id'),
				],
		limit,
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
		const pins = await fetchMany<ChannelPinRow>(
			createFetchChannelPinsQuery(limit).bind({
				channel_id: channelId,
				before_pinned_timestamp: beforePinnedTimestamp,
			}),
		);
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
					createFetchReactionUsersByEmojiQuery(limit, true).bind({
						channel_id: channelId,
						bucket,
						message_id: messageId,
						emoji_id: normalizedEmojiId,
						emoji_name: emojiName,
						after_user_id: after!,
					}),
				)
			: await fetchMany<MessageReactionRow>(
					createFetchReactionUsersByEmojiQuery(limit, false).bind({
						channel_id: channelId,
						bucket,
						message_id: messageId,
						emoji_id: normalizedEmojiId,
						emoji_name: emojiName,
					}),
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
		knownHasReaction?: boolean | null,
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
			created_at: new Date(),
		};
		await upsertOne(MessageReactions.upsertAll(reactionData));
		if (knownHasReaction !== true) {
			await this.setHasReaction(channelId, messageId, true);
		}
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

	async getVoteAnswers(
		channelId: ChannelID,
		messageId: MessageID,
		userId: UserID,
	): Promise<Array<MessagePollSelectedAnswer>> {
		const bucket = BucketUtils.makeBucket(messageId);
		const rows = await fetchMany<Pick<MessagePollVoteRow, 'answer_id'>>(
			CHECK_USER_VOTE_ANSWERS_QUERY.bind({
				channel_id: channelId,
				bucket,
				message_id: messageId,
				user_id: userId,
			}),
		);
		return rows?.map((row) => ({id: row.answer_id})) ?? [];
	}

	async getVotesForAnswer(
		channelId: ChannelID,
		messageId: MessageID,
		answerId: number,
		limit: number = 25,
		after?: UserID,
	): Promise<MessagePollAnswerVotersPage> {
		const requestedLimit = limit !== undefined && Number.isFinite(limit) ? Math.floor(limit) : 25;
		const validatedLimit = Math.min(Math.max(requestedLimit, 1), 100);
		const fetchLimit = validatedLimit + 1;

		const bucket = BucketUtils.makeBucket(messageId);
		const hasAfter = !!after;
		const rows = hasAfter
			? await fetchMany<Pick<MessagePollVoteRow, 'user_id'>>(
					createCheckAnswerVotesQuery(fetchLimit, true).bind({
						channel_id: channelId,
						bucket,
						message_id: messageId,
						answer_id: answerId,
						after_user_id: after,
					}),
				)
			: await fetchMany<Pick<MessagePollVoteRow, 'user_id'>>(
					createCheckAnswerVotesQuery(fetchLimit, false).bind({
						channel_id: channelId,
						bucket,
						message_id: messageId,
						answer_id: answerId,
					}),
				);
		const hasMore = rows.length > validatedLimit;
		const pageRows = hasMore ? rows.slice(0, validatedLimit) : rows;
		if (!pageRows.length) return {userIds: [], hasMore: false, nextAfter: null};
		const nextAfter = hasMore ? pageRows[pageRows.length - 1].user_id.toString() : null;
		return {
			userIds: rows?.map((row) => row.user_id) ?? [],
			hasMore,
			nextAfter,
		};
	}

	async addVote(channelId: ChannelID, messageId: MessageID, userId: UserID, answerId: number): Promise<void> {
		const votes: MessagePollVoteRow = {
			channel_id: channelId,
			bucket: BucketUtils.makeBucket(messageId),
			message_id: messageId,
			user_id: userId,
			answer_id: answerId,
		};
		await upsertOne(MessagePollVotes.upsertAll(votes));
	}

	async removeVote(channelId: ChannelID, messageId: MessageID, userId: UserID, answerId: number): Promise<void> {
		const bucket = BucketUtils.makeBucket(messageId);
		const voteData: MessagePollVoteRow = {
			channel_id: channelId,
			bucket,
			message_id: messageId,
			user_id: userId,
			answer_id: answerId,
		};
		await deleteOneOrMany(MessagePollVotes.deleteByPk(voteData));
	}

	async removeAllVotes(channelId: ChannelID, messageId: MessageID): Promise<void> {
		const bucket = BucketUtils.makeBucket(messageId);
		await deleteOneOrMany(
			MessagePollVotes.delete({
				where: [
					MessagePollVotes.where.eq('channel_id'),
					MessagePollVotes.where.eq('bucket'),
					MessagePollVotes.where.eq('message_id'),
				],
			}).bind({
				channel_id: channelId,
				bucket,
				message_id: messageId,
			}),
		);
	}

	async removeAllVotesBulk(channelId: ChannelID, messageIds: Array<MessageID>): Promise<void> {
		const batch = new BatchBuilder();
		for (const messageId of messageIds) {
			const bucket = BucketUtils.makeBucket(messageId);
			batch.addPrepared(
				MessagePollVotes.delete({
					where: [
						MessagePollVotes.where.eq('channel_id'),
						MessagePollVotes.where.eq('bucket'),
						MessagePollVotes.where.eq('message_id'),
					],
				}).bind({
					channel_id: channelId,
					bucket,
					message_id: messageId,
				}),
			);
		}
		await batch.execute();
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
