// SPDX-License-Identifier: AGPL-3.0-or-later

import {ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import type {ChannelPinResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import type {PollCustomOptionRequest, PollVoteRequest} from '@fluxer/schema/src/domains/message/PollSchemas';
import type {UserPartialResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import type {ChannelID, MessageID, UserID} from '../../BrandedTypes';
import type {GuildAuditLogService} from '../../guild/GuildAuditLogService';
import type {IGuildRepositoryAggregate} from '../../guild/repositories/IGuildRepositoryAggregate';
import type {IGatewayService} from '../../infrastructure/IGatewayService';
import type {ISnowflakeService} from '../../infrastructure/ISnowflakeService';
import type {LimitConfigService} from '../../limits/LimitConfigService';
import type {RequestCache} from '../../middleware/RequestCacheMiddleware';
import type {Channel} from '../../models/Channel';
import type {Message} from '../../models/Message';
import type {MessageReaction} from '../../models/MessageReaction';
import type {IUserRepository} from '../../user/IUserRepository';
import {assertGuildMemberCanCommunicate} from '../../utils/GuildCommunicationUtils';
import type {IChannelRepository} from '../IChannelRepository';
import {MessageInteractionAuthService} from './interaction/MessageInteractionAuthService';
import {MessagePinService} from './interaction/MessagePinService';
import {MessagePollVoteService} from './interaction/MessagePollVoteService';
import {MessageReactionService} from './interaction/MessageReactionService';
import {MessageReadStateService} from './interaction/MessageReadStateService';
import {dispatchMessageUpdateBroadcast} from './message/MessageGatewayDispatch';
import type {MessagePersistenceService} from './message/MessagePersistenceService';
import type {MessageSearchService} from './message/MessageSearchService';

export class MessageInteractionService {
	readonly authService: MessageInteractionAuthService;
	private readStateService: MessageReadStateService;
	private pinService: MessagePinService;
	private pollVoteService: MessagePollVoteService;
	private reactionService: MessageReactionService;

	constructor(
		channelRepository: IChannelRepository,
		userRepository: IUserRepository,
		guildRepository: IGuildRepositoryAggregate,
		private gatewayService: IGatewayService,
		snowflakeService: ISnowflakeService,
		messagePersistenceService: MessagePersistenceService,
		messageSearchService: MessageSearchService,
		guildAuditLogService: GuildAuditLogService,
		limitConfigService: LimitConfigService,
	) {
		this.authService = new MessageInteractionAuthService(
			channelRepository,
			userRepository,
			guildRepository,
			gatewayService,
		);
		this.readStateService = new MessageReadStateService(gatewayService);
		this.pinService = new MessagePinService(
			gatewayService,
			channelRepository,
			snowflakeService,
			messagePersistenceService,
			guildAuditLogService,
		);
		this.pollVoteService = new MessagePollVoteService(
			gatewayService,
			channelRepository,
			snowflakeService,
			messagePersistenceService,
			messageSearchService,
			guildAuditLogService,
		);
		this.reactionService = new MessageReactionService(
			gatewayService,
			channelRepository,
			userRepository,
			guildRepository,
			limitConfigService,
		);
	}

	async startTyping({userId, channelId}: {userId: UserID; channelId: ChannelID}): Promise<void> {
		const authChannel = await this.authService.getChannelAuthenticated({userId, channelId});
		await authChannel.checkPermission(Permissions.SEND_MESSAGES);
		assertGuildMemberCanCommunicate(authChannel.member);
		await this.readStateService.startTyping({authChannel, userId});
	}

	async getChannelPins({
		userId,
		channelId,
		requestCache,
		beforeTimestamp,
		limit,
	}: {
		userId: UserID;
		channelId: ChannelID;
		requestCache: RequestCache;
		beforeTimestamp?: Date;
		limit?: number;
	}): Promise<{
		items: Array<ChannelPinResponse>;
		has_more: boolean;
	}> {
		const authChannel = await this.authService.getChannelAuthenticated({userId, channelId});
		return this.pinService.getChannelPins({authChannel, userId, requestCache, beforeTimestamp, limit});
	}

	async pinMessage({
		userId,
		channelId,
		messageId,
		requestCache,
	}: {
		userId: UserID;
		channelId: ChannelID;
		messageId: MessageID;
		requestCache: RequestCache;
	}): Promise<void> {
		const authChannel = await this.authService.getChannelAuthenticated({userId, channelId});
		if (!authChannel.guild && authChannel.channel.type !== ChannelTypes.DM_PERSONAL_NOTES) {
			await this.authService.validateDMSendPermissions({channelId, userId});
		}
		await this.pinService.pinMessage({authChannel, messageId, userId, requestCache});
	}

	async unpinMessage({
		userId,
		channelId,
		messageId,
		requestCache,
	}: {
		userId: UserID;
		channelId: ChannelID;
		messageId: MessageID;
		requestCache: RequestCache;
	}): Promise<void> {
		const authChannel = await this.authService.getChannelAuthenticated({userId, channelId});
		if (!authChannel.guild && authChannel.channel.type !== ChannelTypes.DM_PERSONAL_NOTES) {
			await this.authService.validateDMSendPermissions({channelId, userId});
		}
		await this.pinService.unpinMessage({authChannel, messageId, userId, requestCache});
	}

	async votePoll({
		userId,
		channelId,
		messageId,
		data,
	}: {
		userId: UserID;
		channelId: ChannelID;
		messageId: MessageID;
		data: PollVoteRequest;
		requestCache: RequestCache;
	}): Promise<void> {
		const authChannel = await this.authService.getChannelAuthenticated({userId, channelId});
		await this.pollVoteService.vote({authChannel, messageId, userId, data});
	}

	async addCustomPollOption({
		userId,
		channelId,
		messageId,
		data,
	}: {
		userId: UserID;
		channelId: ChannelID;
		messageId: MessageID;
		data: PollCustomOptionRequest;
		requestCache: RequestCache;
	}): Promise<void> {
		const authChannel = await this.authService.getChannelAuthenticated({userId, channelId});
		if (!authChannel.guild && authChannel.channel.type !== ChannelTypes.DM_PERSONAL_NOTES) {
			await this.authService.validateDMSendPermissions({channelId, userId});
		}
		await this.pollVoteService.addCustomOption({authChannel, messageId, userId, data});
	}

	async removeOwnPollVote({
		userId,
		channelId,
		messageId,
	}: {
		userId: UserID;
		channelId: ChannelID;
		messageId: MessageID;
		requestCache: RequestCache;
	}): Promise<void> {
		const authChannel = await this.authService.getChannelAuthenticated({userId, channelId});
		await this.pollVoteService.removeOwnVote({authChannel, messageId, userId});
	}

	async closePoll({
		userId,
		channelId,
		messageId,
	}: {
		userId: UserID;
		channelId: ChannelID;
		messageId: MessageID;
		requestCache: RequestCache;
	}): Promise<void> {
		const authChannel = await this.authService.getChannelAuthenticated({userId, channelId});
		await this.pollVoteService.closePoll({authChannel, messageId, userId});
	}

	async getUsersForReaction({
		userId,
		channelId,
		messageId,
		emoji,
		limit,
		after,
	}: {
		userId: UserID;
		channelId: ChannelID;
		messageId: MessageID;
		emoji: string;
		limit?: number;
		after?: UserID;
	}): Promise<{
		users: Array<UserPartialResponse>;
		has_more: boolean;
		next_after: string | null;
	}> {
		const authChannel = await this.authService.getChannelAuthenticated({userId, channelId});
		return this.reactionService.getUsersForReaction({authChannel, messageId, emoji, limit, after, userId});
	}

	async addReaction({
		userId,
		sessionId,
		channelId,
		messageId,
		emoji,
	}: {
		userId: UserID;
		sessionId?: string;
		channelId: ChannelID;
		messageId: MessageID;
		emoji: string;
		requestCache: RequestCache;
	}): Promise<void> {
		const authChannel = await this.authService.getChannelAuthenticated({userId, channelId});
		await this.reactionService.addReaction({authChannel, messageId, emoji, userId, sessionId});
	}

	async removeReaction({
		userId,
		sessionId,
		channelId,
		messageId,
		emoji,
		targetId,
	}: {
		userId: UserID;
		sessionId?: string;
		channelId: ChannelID;
		messageId: MessageID;
		emoji: string;
		targetId: UserID;
		requestCache: RequestCache;
	}): Promise<void> {
		const authChannel = await this.authService.getChannelAuthenticated({userId, channelId});
		await this.reactionService.removeReaction({authChannel, messageId, emoji, targetId, sessionId, actorId: userId});
	}

	async removeOwnReaction({
		userId,
		sessionId,
		channelId,
		messageId,
		emoji,
		requestCache,
	}: {
		userId: UserID;
		sessionId?: string;
		channelId: ChannelID;
		messageId: MessageID;
		emoji: string;
		requestCache: RequestCache;
	}): Promise<void> {
		await this.removeReaction({userId, sessionId, channelId, messageId, emoji, targetId: userId, requestCache});
	}

	async removeAllReactionsForEmoji({
		userId,
		channelId,
		messageId,
		emoji,
	}: {
		userId: UserID;
		channelId: ChannelID;
		messageId: MessageID;
		emoji: string;
	}): Promise<void> {
		const authChannel = await this.authService.getChannelAuthenticated({userId, channelId});
		await this.reactionService.removeAllReactionsForEmoji({authChannel, messageId, emoji, actorId: userId});
	}

	async removeAllReactions({
		userId,
		channelId,
		messageId,
	}: {
		userId: UserID;
		channelId: ChannelID;
		messageId: MessageID;
	}): Promise<void> {
		const authChannel = await this.authService.getChannelAuthenticated({userId, channelId});
		await this.reactionService.removeAllReactions({authChannel, messageId, actorId: userId});
	}

	async getMessageReactions({
		userId,
		channelId,
		messageId,
	}: {
		userId: UserID;
		channelId: ChannelID;
		messageId: MessageID;
	}): Promise<Array<MessageReaction>> {
		const authChannel = await this.authService.getChannelAuthenticated({userId, channelId});
		return this.reactionService.getMessageReactions({authChannel, messageId});
	}

	async dispatchMessageUpdate({
		channel,
		message,
		currentUserId,
	}: {
		channel: Channel;
		message: Message;
		requestCache: RequestCache;
		currentUserId?: UserID;
	}): Promise<void> {
		await dispatchMessageUpdateBroadcast({
			gatewayService: this.gatewayService,
			currentUserId,
			channel,
			message,
		});
	}
}
