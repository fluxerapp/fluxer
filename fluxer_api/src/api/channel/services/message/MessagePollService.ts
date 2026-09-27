// SPDX-License-Identifier: AGPL-3.0-or-later

import type {LimitConfigService} from '@app/api/limits/LimitConfigService';
import {resolveLimitSafe} from '@app/api/limits/LimitConfigUtils';
import {createLimitMatchContext} from '@app/api/limits/LimitMatchContextBuilder';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {Channel} from '@app/api/models/Channel';
import {Message} from '@app/api/models/Message';
import type {User} from '@app/api/models/User';
import type {PollMessageExpiryRow} from '@app/api/Tables';
import type {IUserRepository} from '@app/api/user/IUserRepository';
import {mapUserToPartialResponse} from '@app/api/user/UserMappers';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {MAX_POLL_VOTES_PER_ANSWER} from '@fluxer/constants/src/LimitConstants';
import {CannotEditOtherUserMessageError} from '@fluxer/errors/src/domains/channel/CannotEditOtherUserMessageError';
import {CannotSelectMultipleAnswersError} from '@fluxer/errors/src/domains/channel/CannotSelectMultipleAnswersError';
import {CannotVoteOnFinalizedPollError} from '@fluxer/errors/src/domains/channel/CannotVoteOnFinalizedPollError';
import {CannotVoteOnNonPollError} from '@fluxer/errors/src/domains/channel/CannotVoteOnNonPollError';
import {MaxPollVotesPerAnswerError} from '@fluxer/errors/src/domains/channel/MaxPollVotesPerAnswerError';
import {UnknownMessageError} from '@fluxer/errors/src/domains/channel/UnknownMessageError';
import {UnknownPollAnswerError} from '@fluxer/errors/src/domains/channel/UnknownPollAnswerError';
import type {PollAnswerVotersResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {snowflakeToDate} from '@fluxer/snowflake/src/Snowflake';
import type {ChannelID, MessageID, UserID} from '../../../BrandedTypes';
import type {IChannelRepositoryAggregate} from '../../repositories/IChannelRepositoryAggregate';
import type {PollMessageExpiryRepository} from '../../repositories/PollMessageExpiryRepository';
import type {AuthenticatedChannel} from '../AuthenticatedChannel';
import type {MessageReactionService} from '../interaction/MessageReactionService';
import type {MessageChannelAuthService} from './MessageChannelAuthService';
import type {MessageDispatchService} from './MessageDispatchService';
import type {MessageSendService} from './MessageSendService';
import {MissingPermissionsError} from '@fluxer/errors/src/domains/core/MissingPermissionsError';
import { ReactionType } from '@fluxer/constants/src/EmojiConstants';

interface MessagePollServiceDeps {
	channelAuthService: MessageChannelAuthService;
	channelRepository: IChannelRepositoryAggregate;
	userRepository: IUserRepository;
	dispatchService: MessageDispatchService;
	pollExpiryRepository: PollMessageExpiryRepository;
	messageReactionService: MessageReactionService;
	messageSendService: MessageSendService;
	limitConfigService: LimitConfigService;
}

export class MessagePollService {
	public readonly expiry: PollMessageExpiryRepository;

	constructor(private readonly deps: MessagePollServiceDeps) {
		this.expiry = deps.pollExpiryRepository;
	}

	private async assertMessageHistoryAccess({
		authChannel,
		messageId,
	}: {
		authChannel: AuthenticatedChannel;
		messageId: MessageID;
	}): Promise<void> {
		const {guild, hasPermission} = authChannel;
		if (!guild) {
			return;
		}
		if (await hasPermission(Permissions.READ_MESSAGE_HISTORY)) {
			return;
		}
		const cutoff = guild.message_history_cutoff;
		if (!cutoff || snowflakeToDate(messageId).getTime() < new Date(cutoff).getTime()) {
			throw new UnknownMessageError();
		}
	}

	async endPoll({
		userId,
		channelId,
		messageId,
		expiryRow,
		requestCache,
	}: {
		userId: UserID;
		channelId: ChannelID;
		messageId: MessageID;
		requestCache: RequestCache;
		expiryRow?: PollMessageExpiryRow;
	}): Promise<void> {
		const authChannel = await this.deps.channelAuthService.getChannelAuthenticated({
			userId,
			channelId,
		});
		await this.assertMessageHistoryAccess({authChannel, messageId});
		const {channel} = authChannel;
		const message = await this.deps.channelRepository.messages.getMessage(channel.id, messageId);
		if (message?.authorId !== userId) throw new CannotEditOtherUserMessageError();

		return await this.endPollBypassAuth({channel, message, requestCache, expiryRow});
	}

	async endPollBypassAuth({
		channel,
		message,
		requestCache,
		expiryRow,
	}: {
		channel: Channel;
		message: Message | null;
		requestCache: RequestCache;
		expiryRow?: PollMessageExpiryRow;
	}): Promise<void> {
		if (!message) throw new UnknownMessageError();
		if (!message.poll) throw new UnknownMessageError();

		const oldMessageRow = message.toRow();
		const newMessageRow = message.toRow();
		const poll = newMessageRow.poll;
		if (poll) {
			if (poll.results) {
				poll.results.is_finalized = true;
			} else {
				poll.results = {
					is_finalized: true,
					answer_counts: (poll.answers ?? []).map((answer) => ({
						id: answer.answer_id,
						count: 0,
					})),
				};
			}
		}

		await this.deps.channelRepository.messages.upsertMessage(newMessageRow, oldMessageRow);

		const answerCounts = newMessageRow.poll?.results?.answer_counts;
		if (newMessageRow.poll?.results) newMessageRow.poll.results.answer_counts = null;
		await this.deps.dispatchService.dispatchMessageUpdate({
			channel,
			message: new Message(newMessageRow),
		});

		if (message.authorId) {
			const user = await this.deps.userRepository.findUnique(message.authorId);
			if (user) {
				let victorAnswerVotes = 0;
				let totalVotes = 0;
				for (const answerCount of answerCounts ?? []) {
					victorAnswerVotes = Math.max(victorAnswerVotes, answerCount.count ?? 0);
					totalVotes += answerCount.count ?? 0;
				}

				await this.deps.messageSendService.sendSimpleMessageBypassAuth({
					channelId: channel.id,
					user,
					data: {
						content: '',
						allowed_mentions: {
							users: [message.authorId],
						},
						embeds: [
							{
								type: 'poll_result',
								fields: [
									{
										name: 'poll_question_text',
										value: poll?.question?.text ?? '',
										inline: false,
									},
									{
										name: 'victor_answer_votes',
										value: `${victorAnswerVotes}`,
										inline: false,
									},
									{
										name: 'total_votes',
										value: `${totalVotes}`,
										inline: false,
									},
								],
							},
						],
						message_reference: {
							type: 0,
							channel_id: message.channelId,
							message_id: message.id,
						},
					},
					mentionAuthor: true,
					requestCache,
				});
			}
		}

		const row = expiryRow ? expiryRow : await this.deps.pollExpiryRepository.fetchById(message.id);
		if (row) {
			await this.deps.pollExpiryRepository.deleteRecords({
				expiry_bucket: row.expiry_bucket,
				expires_at: row.expires_at,
				message_id: message.id,
			});
		}
	}

	async removeAllVotes(channelId: ChannelID, messageId: MessageID): Promise<void> {
		await this.deps.channelRepository.messageInteractions.removeAllVotes(channelId, messageId);
	}

	async removeAllVotesBulk(channelId: ChannelID, messageIds: Array<MessageID>): Promise<void> {
		await this.deps.channelRepository.messageInteractions.removeAllVotesBulk(channelId, messageIds);
	}

	async getVotesForAnswer({
		userId,
		channelId,
		messageId,
		answerId,
		limit,
		after,
	}: {
		userId: UserID;
		channelId: ChannelID;
		messageId: MessageID;
		answerId: number;
		limit?: number;
		after?: UserID;
	}): Promise<PollAnswerVotersResponse> {
		const authChannel = await this.deps.channelAuthService.getChannelAuthenticated({
			userId,
			channelId,
		});
		await this.assertMessageHistoryAccess({authChannel, messageId});
		const {channel, hasPermission} = authChannel;
		const message = await this.deps.channelRepository.messages.getMessage(channel.id, messageId);
		if (!message) throw new UnknownMessageError();

		if (!message.poll || !message.poll.answers || !message.poll.answers.find((answer) => answer.answer_id === answerId))
			throw new UnknownPollAnswerError();

		if (message.poll.anonymous_voting) {
			const managesMessages = await hasPermission(Permissions.MANAGE_MESSAGES);
			const canSeeVotes = await hasPermission(Permissions.SEE_VOTES_ON_ANONYMOUS_POLLS);
			if (!managesMessages && !canSeeVotes) throw new MissingPermissionsError();
		}

		const response = await this.deps.channelRepository.messageInteractions.getVotesForAnswer(
			channelId,
			messageId,
			answerId,
			limit,
			after,
		);
		const users = await this.deps.userRepository.listUsers(response.userIds);
		return {
			users: users.map((user) => mapUserToPartialResponse(user)),
			has_more: response.hasMore ?? false,
			next_after: response.nextAfter ?? null,
		};
	}

	async vote({
		user,
		channelId,
		messageId,
		answerIds,
	}: {
		user: User;
		channelId: ChannelID;
		messageId: MessageID;
		answerIds: Array<number>;
	}): Promise<void> {
		const authChannel = await this.deps.channelAuthService.getChannelAuthenticated({
			userId: user.id,
			channelId,
		});
		await this.assertMessageHistoryAccess({authChannel, messageId});
		const {channel, guild} = authChannel;
		const message = await this.deps.channelRepository.messages.getMessage(channel.id, messageId);
		if (!message) throw new UnknownMessageError();

		const oldMessageRow = message.toRow();
		const newMessageRow = message.toRow();
		const poll = newMessageRow.poll;
		if (!poll) throw new CannotVoteOnNonPollError();

		if (poll.results?.is_finalized) throw new CannotVoteOnFinalizedPollError();
		if (!poll.allow_multiselect && answerIds.length > 1) throw new CannotSelectMultipleAnswersError();

		if (!poll.results) {
			poll.results = {
				is_finalized: false,
				answer_counts: null,
			};
		}
		if (!poll.results.answer_counts) {
			poll.results.answer_counts = (poll.answers ?? []).map((answer) => ({
				id: answer.answer_id,
				count: 0,
			}));
		}

		const existingAnswers = await this.deps.channelRepository.messageInteractions.getVoteAnswers(
			channelId,
			messageId,
			user.id,
		);
		const existingAnswerIds = existingAnswers.map((answer) => answer.id);

		if (answerIds.length === 0) {
			for (const answerId of existingAnswerIds) {
				await this.deps.messageReactionService.removeReaction({
					authChannel,
					messageId,
					actorId: user.id,
					targetId: user.id,
					emoji: `${answerId}:${answerId}`,
					reactionType: ReactionType.PollVote,
				});
				const answerCount = poll.results.answer_counts.find((ac) => ac.id === answerId);
				if (answerCount) {
					if (!answerCount.count) answerCount.count = 0;
					answerCount.count--;
				}
			}
		} else {
			const ctx = createLimitMatchContext({user, guildFeatures: guild?.features});
			const evaluationContext = guild?.features ? 'guild' : 'user';
			const configSnapshot = this.deps.limitConfigService.getConfigSnapshot();
			const maxPollVotesCount = Math.floor(
				resolveLimitSafe(configSnapshot, ctx, 'max_poll_votes_per_answer', MAX_POLL_VOTES_PER_ANSWER, evaluationContext),
			);

			for (const answerId of answerIds) {
				if (existingAnswerIds.includes(answerId)) continue;
				const answerCount = poll.results.answer_counts.find((ac) => ac.id === answerId);
				if (answerCount) {
					if (!answerCount.count) answerCount.count = 0;
					if (answerCount.count >= maxPollVotesCount) throw new MaxPollVotesPerAnswerError(maxPollVotesCount);
					await this.deps.messageReactionService.addReaction({
						authChannel,
						messageId,
						userId: user.id,
						emoji: `${answerId}:${answerId}`,
						reactionType: ReactionType.PollVote,
					});
					answerCount.count++;
				}
			}
			for (const existingAnswerId of existingAnswerIds) {
				if (answerIds.includes(existingAnswerId)) continue;
				const answerCount = poll.results.answer_counts.find((ac) => ac.id === existingAnswerId);
				if (answerCount) {
					if (!answerCount.count) answerCount.count = 0;
					await this.deps.messageReactionService.removeReaction({
						authChannel,
						messageId,
						actorId: user.id,
						targetId: user.id,
						emoji: `${existingAnswerId}:${existingAnswerId}`,
						reactionType: ReactionType.PollVote,
					});
					answerCount.count--;
				}
			}
		}

		await this.deps.channelRepository.messages.upsertMessage(newMessageRow, oldMessageRow);
	}
}
