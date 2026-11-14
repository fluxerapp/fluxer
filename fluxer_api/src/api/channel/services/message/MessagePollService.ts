// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, MessageID, UserID} from '@app/api/BrandedTypes';
import type {ChannelRepository} from '@app/api/channel/ChannelRepository';
import type {PollMessageExpiryRepository} from '@app/api/channel/repositories/PollMessageExpiryRepository';
import type {AuthenticatedChannel} from '@app/api/channel/services/AuthenticatedChannel';
import type {MessageReactionService} from '@app/api/channel/services/interaction/MessageReactionService';
import type {MessageChannelAuthService} from '@app/api/channel/services/message/MessageChannelAuthService';
import {dispatchMessageUpdateBroadcast} from '@app/api/channel/services/message/MessageGatewayDispatch';
import type {MessageSendService} from '@app/api/channel/services/message/MessageSendService';
import type {MessageWriteLock} from '@app/api/channel/services/message/MessageWriteLock';
import type {ThreadViewer} from '@app/api/experiment/ChannelThreadsGate';
import {isGuildMemberTimedOut} from '@app/api/guild/GuildModel';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {LimitConfigService} from '@app/api/limits/LimitConfigService';
import {resolveLimitSafe} from '@app/api/limits/LimitConfigUtils';
import {createLimitMatchContext} from '@app/api/limits/LimitMatchContextBuilder';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {Channel} from '@app/api/models/Channel';
import {Message} from '@app/api/models/Message';
import type {User} from '@app/api/models/User';
import type {PollMessageExpiryRow} from '@app/api/Tables';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {mapUserToPartialResponse} from '@app/api/user/UserMappers';
import {MessageTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {ReactionType} from '@fluxer/constants/src/EmojiConstants';
import {MAX_POLL_VOTES_PER_ANSWER} from '@fluxer/constants/src/LimitConstants';
import {CannotEditOtherUserMessageError} from '@fluxer/errors/src/domains/channel/CannotEditOtherUserMessageError';
import {CannotSelectMultipleAnswersError} from '@fluxer/errors/src/domains/channel/CannotSelectMultipleAnswersError';
import {CannotVoteOnFinalizedPollError} from '@fluxer/errors/src/domains/channel/CannotVoteOnFinalizedPollError';
import {CannotVoteOnNonPollError} from '@fluxer/errors/src/domains/channel/CannotVoteOnNonPollError';
import {MaxPollVotesPerAnswerError} from '@fluxer/errors/src/domains/channel/MaxPollVotesPerAnswerError';
import {UnknownMessageError} from '@fluxer/errors/src/domains/channel/UnknownMessageError';
import {UnknownPollAnswerError} from '@fluxer/errors/src/domains/channel/UnknownPollAnswerError';
import {MissingPermissionsError} from '@fluxer/errors/src/domains/core/MissingPermissionsError';
import type {PollAnswerVotersResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {snowflakeToDate} from '@fluxer/snowflake/src/Snowflake';

interface MessagePollServiceDeps {
	channelAuthService: MessageChannelAuthService;
	channelRepository: ChannelRepository;
	userRepository: UserRepository;
	gatewayService: IGatewayService;
	pollExpiryRepository: PollMessageExpiryRepository;
	messageReactionService: MessageReactionService;
	messageSendService: MessageSendService;
	limitConfigService: LimitConfigService;
	messageWriteLock: MessageWriteLock;
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
		viewer,
		channelId,
		messageId,
		expiryRow,
		requestCache,
	}: {
		userId: UserID;
		viewer: ThreadViewer;
		channelId: ChannelID;
		messageId: MessageID;
		requestCache: RequestCache;
		expiryRow?: PollMessageExpiryRow;
	}): Promise<void> {
		const authChannel = await this.deps.channelAuthService.getChannelAuthenticated({
			userId,
			channelId,
			viewer,
		});
		await this.assertMessageHistoryAccess({authChannel, messageId});
		const {channel} = authChannel;
		const message = await this.deps.channelRepository.messages.getMessage(channel.id, messageId);
		if (message?.authorId !== userId) throw new CannotEditOtherUserMessageError();

		return await this.endPollBypassAuth({channel, viewer, message, requestCache, expiryRow});
	}

	async endPollBypassAuth({
		channel,
		viewer,
		message,
		requestCache,
		expiryRow,
	}: {
		channel: Channel;
		viewer: ThreadViewer;
		message: Message | null;
		requestCache: RequestCache;
		expiryRow?: PollMessageExpiryRow;
	}): Promise<void> {
		if (!message) throw new UnknownMessageError();
		if (!message.poll) throw new UnknownMessageError();

		const finalized = await this.deps.messageWriteLock.withFreshMessage(channel.id, message.id, async (fresh) => {
			if (!fresh?.poll) throw new UnknownMessageError();
			if (fresh.poll.results?.is_finalized) return null;
			const oldMessageRow = fresh.toRow();
			const newMessageRow = fresh.toRow();
			const poll = newMessageRow.poll!;
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
			await this.deps.channelRepository.messages.upsertMessage(newMessageRow, oldMessageRow);
			return {message: fresh, row: newMessageRow};
		});

		const row = expiryRow ? expiryRow : await this.deps.pollExpiryRepository.fetchById(message.id);
		if (row) {
			await this.deps.pollExpiryRepository.deleteRecords({
				expiry_bucket: row.expiry_bucket,
				expires_at: row.expires_at,
				message_id: message.id,
			});
		}
		if (!finalized) return;

		const {message: fresh, row: newMessageRow} = finalized;
		const poll = newMessageRow.poll;
		const answerCounts = poll?.results?.answer_counts;
		if (poll?.results) poll.results.answer_counts = null;
		await dispatchMessageUpdateBroadcast({
			gatewayService: this.deps.gatewayService,
			channel,
			message: new Message(newMessageRow),
		});

		if (!fresh.authorId) return;
		if (!(await this.authorCanStillPost(channel, fresh.authorId))) return;
		const user = await this.deps.userRepository.findUnique(fresh.authorId);
		if (!user) return;

		let victorAnswerVotes = 0;
		let totalVotes = 0;
		for (const answerCount of answerCounts ?? []) {
			victorAnswerVotes = Math.max(victorAnswerVotes, answerCount.count ?? 0);
			totalVotes += answerCount.count ?? 0;
		}

		await this.deps.messageSendService.sendSimpleMessageBypassAuth({
			channelId: channel.id,
			user,
			viewer,
			messageType: MessageTypes.POLL_RESULT,
			data: {
				content: '',
				allowed_mentions: {
					users: [fresh.authorId],
				},
				embeds: [
					{
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
					channel_id: fresh.channelId,
					message_id: fresh.id,
				},
			},
			mentionAuthor: true,
			requestCache,
		});
	}

	private async authorCanStillPost(channel: Channel, authorId: UserID): Promise<boolean> {
		if (!channel.guildId) return true;
		try {
			const {success, memberData} = await this.deps.gatewayService.getGuildMember({
				guildId: channel.guildId,
				userId: authorId,
			});
			return success && memberData != null && !isGuildMemberTimedOut(memberData);
		} catch {
			return false;
		}
	}

	async getVotesForAnswer({
		userId,
		viewer,
		channelId,
		messageId,
		answerId,
		limit,
		after,
	}: {
		userId: UserID;
		viewer: ThreadViewer;
		channelId: ChannelID;
		messageId: MessageID;
		answerId: number;
		limit?: number;
		after?: UserID;
	}): Promise<PollAnswerVotersResponse> {
		const authChannel = await this.deps.channelAuthService.getChannelAuthenticated({
			userId,
			channelId,
			viewer,
		});
		await this.assertMessageHistoryAccess({authChannel, messageId});
		const {channel, hasPermission} = authChannel;
		const message = await this.deps.channelRepository.messages.getMessage(channel.id, messageId);
		if (!message) throw new UnknownMessageError();

		if (!message.poll?.answers?.find((answer) => answer.answer_id === answerId)) throw new UnknownPollAnswerError();

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
		viewer,
		channelId,
		messageId,
		answerIds,
	}: {
		user: User;
		viewer: ThreadViewer;
		channelId: ChannelID;
		messageId: MessageID;
		answerIds: Array<number>;
	}): Promise<void> {
		const authChannel = await this.deps.channelAuthService.getChannelAuthenticated({
			userId: user.id,
			channelId,
			viewer,
		});
		await this.assertMessageHistoryAccess({authChannel, messageId});
		const {channel, guild} = authChannel;
		await this.deps.messageWriteLock.withFreshMessage(channel.id, messageId, async (message) => {
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
				(poll.answers ?? []).flatMap((answer) => (answer.answer_id == null ? [] : [answer.answer_id])),
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
					resolveLimitSafe(
						configSnapshot,
						ctx,
						'max_poll_votes_per_answer',
						MAX_POLL_VOTES_PER_ANSWER,
						evaluationContext,
					),
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
		});
	}
}
