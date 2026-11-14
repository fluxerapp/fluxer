// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, EmojiID, MessageID, UserID} from '@app/api/BrandedTypes';
import {createEmojiID} from '@app/api/BrandedTypes';
import type {MessageRepository} from '@app/api/channel/repositories/MessageRepository';
import {deleteOneOrMany, fetchMany, fetchOne, upsertOne} from '@app/api/database/CassandraQueryExecution';
import {Db} from '@app/api/database/CassandraTypes';
import type {ChannelPinRow, MessageReactionRow} from '@app/api/database/types/MessageTypes';
import type {
	MessagePollAnswerVotersPage,
	MessagePollSelectedAnswer,
	MessagePollVoteRow,
} from '@app/api/database/types/PollTypes';
import type {Message} from '@app/api/models/Message';
import {MessageReaction} from '@app/api/models/MessageReaction';
import {ChannelPins, MessagePollVotes, MessageReactions, Messages} from '@app/api/Tables';
import {snowflakeToDate} from '@fluxer/snowflake/src/Snowflake';
import * as BucketUtils from '@fluxer/snowflake/src/SnowflakeBuckets';

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
const FETCH_FIRST_EMOJI_NAME_QUERY = MessageReactions.selectCql({
	columns: ['emoji_name'],
	where: [
		MessageReactions.where.eq('channel_id'),
		MessageReactions.where.eq('bucket'),
		MessageReactions.where.eq('message_id'),
		MessageReactions.where.eq('emoji_id'),
	],
	limit: 1,
});
const FETCH_NEXT_EMOJI_NAME_QUERY = MessageReactions.selectCql({
	columns: ['emoji_name'],
	where: [
		MessageReactions.where.eq('channel_id'),
		MessageReactions.where.eq('bucket'),
		MessageReactions.where.eq('message_id'),
		MessageReactions.where.eq('emoji_id'),
		MessageReactions.where.gt('emoji_name', 'after_emoji_name'),
	],
	limit: 1,
});
const FETCH_NEXT_EMOJI_ID_QUERY = MessageReactions.selectCql({
	columns: ['emoji_id'],
	where: [
		MessageReactions.where.eq('channel_id'),
		MessageReactions.where.eq('bucket'),
		MessageReactions.where.eq('message_id'),
		MessageReactions.where.gt('emoji_id', 'after_emoji_id'),
	],
	limit: 1,
});
const DELETE_REACTIONS_BY_EMOJI_ID_QUERY = MessageReactions.deleteCql({
	where: [
		MessageReactions.where.eq('channel_id'),
		MessageReactions.where.eq('bucket'),
		MessageReactions.where.eq('message_id'),
		MessageReactions.where.eq('emoji_id'),
	],
});
const DELETE_REACTIONS_BY_EMOJI_QUERY = MessageReactions.deleteCql({
	where: [
		MessageReactions.where.eq('channel_id'),
		MessageReactions.where.eq('bucket'),
		MessageReactions.where.eq('message_id'),
		MessageReactions.where.eq('emoji_id'),
		MessageReactions.where.eq('emoji_name'),
	],
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
		MessagePollVotes.where.in('answer_id', 'answer_ids'),
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
const UNICODE_EMOJI_ID = createEmojiID(0n);

export type ReactionEmojiKey = {id: EmojiID; name?: undefined} | {id?: undefined; name: string};

export class MessageInteractionRepository {
	private messageRepository: MessageRepository;

	constructor(messageRepository: MessageRepository) {
		this.messageRepository = messageRepository;
	}

	async listChannelPins(
		channelId: ChannelID,
		beforePinnedTimestamp: Date,
		limit: number = 50,
		minMessageTimestamp: number | null = null,
	): Promise<{messages: Array<Message>; scannedCount: number; oldestPinnedTimestamp: Date | null}> {
		const pins = await fetchMany<ChannelPinRow>(
			createFetchChannelPinsQuery(limit).bind({
				channel_id: channelId,
				before_pinned_timestamp: beforePinnedTimestamp,
			}),
		);
		let oldestPinnedTimestamp: Date | null = null;
		const messages: Array<Message> = [];
		for (const pin of pins) {
			if (!oldestPinnedTimestamp || pin.pinned_timestamp.getTime() < oldestPinnedTimestamp.getTime()) {
				oldestPinnedTimestamp = pin.pinned_timestamp;
			}
			if (minMessageTimestamp !== null && snowflakeToDate(pin.message_id).getTime() < minMessageTimestamp) {
				continue;
			}
			const message = await this.messageRepository.getMessage(channelId, pin.message_id);
			if (message) {
				messages.push(message);
			}
		}
		return {messages, scannedCount: pins.length, oldestPinnedTimestamp};
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
		if (!message?.pinnedTimestamp) {
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
		emoji: ReactionEmojiKey,
		limit: number = 25,
		after?: UserID,
	): Promise<Array<MessageReaction>> {
		const bucket = BucketUtils.makeBucket(messageId);
		const emojiId = emoji.id ?? UNICODE_EMOJI_ID;
		const names = await this.listStoredEmojiNames(channelId, bucket, messageId, emoji);
		const pages = await Promise.all(
			names.map((emojiName) =>
				after
					? fetchMany<MessageReactionRow>(
							createFetchReactionUsersByEmojiQuery(limit, true).bind({
								channel_id: channelId,
								bucket,
								message_id: messageId,
								emoji_id: emojiId,
								emoji_name: emojiName,
								after_user_id: after,
							}),
						)
					: fetchMany<MessageReactionRow>(
							createFetchReactionUsersByEmojiQuery(limit, false).bind({
								channel_id: channelId,
								bucket,
								message_id: messageId,
								emoji_id: emojiId,
								emoji_name: emojiName,
							}),
						),
			),
		);
		const byUser = new Map<UserID, MessageReactionRow>();
		for (const row of pages.flat().sort((a, b) => Number(a.user_id - b.user_id))) {
			if (!byUser.has(row.user_id)) byUser.set(row.user_id, row);
		}
		return [...byUser.values()].slice(0, limit).map((row) => new MessageReaction(row));
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
		const reactionData: MessageReactionRow = {
			channel_id: channelId,
			bucket,
			message_id: messageId,
			user_id: userId,
			emoji_id: emojiId ?? UNICODE_EMOJI_ID,
			emoji_name: emojiName,
			emoji_animated: emojiAnimated,
			created_at: new Date(),
		};
		await upsertOne(MessageReactions.upsertAll(reactionData));
		await this.setHasReaction(channelId, messageId, true);
		return new MessageReaction(reactionData);
	}

	async removeReaction(
		channelId: ChannelID,
		messageId: MessageID,
		userId: UserID,
		emoji: ReactionEmojiKey,
	): Promise<void> {
		const bucket = BucketUtils.makeBucket(messageId);
		const names = await this.listStoredEmojiNames(channelId, bucket, messageId, emoji);
		for (const emojiName of names) {
			await deleteOneOrMany(
				MessageReactions.deleteByPk({
					channel_id: channelId,
					bucket,
					message_id: messageId,
					user_id: userId,
					emoji_id: emoji.id ?? UNICODE_EMOJI_ID,
					emoji_name: emojiName,
				}),
			);
		}
		const hasReactions = await this.messageHasAnyReactions(channelId, messageId);
		await this.setHasReaction(channelId, messageId, hasReactions);
	}

	async getVoteAnswers(
		channelId: ChannelID,
		messageId: MessageID,
		userId: UserID,
		answerIds: Array<number>,
	): Promise<Array<MessagePollSelectedAnswer>> {
		if (answerIds.length === 0) return [];
		const bucket = BucketUtils.makeBucket(messageId);
		const rows = await fetchMany<Pick<MessagePollVoteRow, 'answer_id'>>(
			CHECK_USER_VOTE_ANSWERS_QUERY.bind({
				channel_id: channelId,
				bucket,
				message_id: messageId,
				answer_ids: answerIds,
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
			userIds: pageRows?.map((row) => row.user_id) ?? [],
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

	async removeAllReactionsForEmoji(channelId: ChannelID, messageId: MessageID, emoji: ReactionEmojiKey): Promise<void> {
		const bucket = BucketUtils.makeBucket(messageId);
		if (emoji.id !== undefined) {
			await deleteOneOrMany(DELETE_REACTIONS_BY_EMOJI_ID_QUERY, {
				channel_id: channelId,
				bucket,
				message_id: messageId,
				emoji_id: emoji.id,
			});
		} else {
			await deleteOneOrMany(DELETE_REACTIONS_BY_EMOJI_QUERY, {
				channel_id: channelId,
				bucket,
				message_id: messageId,
				emoji_id: UNICODE_EMOJI_ID,
				emoji_name: emoji.name,
			});
		}
		const hasReactions = await this.messageHasAnyReactions(channelId, messageId);
		await this.setHasReaction(channelId, messageId, hasReactions);
	}

	async countUniqueReactions(channelId: ChannelID, messageId: MessageID, limit: number): Promise<number> {
		const key = {channel_id: channelId, bucket: BucketUtils.makeBucket(messageId), message_id: messageId};
		let count = 0;
		let unicode = await fetchOne<Pick<MessageReactionRow, 'emoji_name'>>(FETCH_FIRST_EMOJI_NAME_QUERY, {
			...key,
			emoji_id: UNICODE_EMOJI_ID,
		});
		while (unicode) {
			count += 1;
			if (count >= limit) return count;
			unicode = await fetchOne<Pick<MessageReactionRow, 'emoji_name'>>(FETCH_NEXT_EMOJI_NAME_QUERY, {
				...key,
				emoji_id: UNICODE_EMOJI_ID,
				after_emoji_name: unicode.emoji_name,
			});
		}
		let custom = await fetchOne<Pick<MessageReactionRow, 'emoji_id'>>(FETCH_NEXT_EMOJI_ID_QUERY, {
			...key,
			after_emoji_id: UNICODE_EMOJI_ID,
		});
		while (custom) {
			count += 1;
			if (count >= limit) return count;
			custom = await fetchOne<Pick<MessageReactionRow, 'emoji_id'>>(FETCH_NEXT_EMOJI_ID_QUERY, {
				...key,
				after_emoji_id: custom.emoji_id,
			});
		}
		return count;
	}

	async checkUserReactionExists(
		channelId: ChannelID,
		messageId: MessageID,
		userId: UserID,
		emoji: ReactionEmojiKey,
	): Promise<boolean> {
		const bucket = BucketUtils.makeBucket(messageId);
		const names = await this.listStoredEmojiNames(channelId, bucket, messageId, emoji);
		for (const emojiName of names) {
			const reaction = await fetchOne<MessageReactionRow>(CHECK_USER_REACTION_EXISTS_QUERY, {
				channel_id: channelId,
				bucket,
				message_id: messageId,
				user_id: userId,
				emoji_id: emoji.id ?? UNICODE_EMOJI_ID,
				emoji_name: emojiName,
			});
			if (reaction) return true;
		}
		return false;
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

	private async listStoredEmojiNames(
		channelId: ChannelID,
		bucket: number,
		messageId: MessageID,
		emoji: ReactionEmojiKey,
	): Promise<Array<string>> {
		if (emoji.id === undefined) return [emoji.name];
		const key = {channel_id: channelId, bucket, message_id: messageId, emoji_id: emoji.id};
		const names: Array<string> = [];
		let row = await fetchOne<Pick<MessageReactionRow, 'emoji_name'>>(FETCH_FIRST_EMOJI_NAME_QUERY, key);
		while (row) {
			names.push(row.emoji_name);
			row = await fetchOne<Pick<MessageReactionRow, 'emoji_name'>>(FETCH_NEXT_EMOJI_NAME_QUERY, {
				...key,
				after_emoji_name: row.emoji_name,
			});
		}
		return names;
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
