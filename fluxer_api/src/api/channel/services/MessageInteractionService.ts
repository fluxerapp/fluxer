// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, MessageID, PersonaID, UserID} from '@app/api/BrandedTypes';
import type {IChannelRepository} from '@app/api/channel/IChannelRepository';
import {MessageInteractionAuthService} from '@app/api/channel/services/interaction/MessageInteractionAuthService';
import {MessagePinAuthService} from '@app/api/channel/services/interaction/MessagePinAuthService';
import {MessagePinService} from '@app/api/channel/services/interaction/MessagePinService';
import {MessageReactionService} from '@app/api/channel/services/interaction/MessageReactionService';
import {MessageReadStateService} from '@app/api/channel/services/interaction/MessageReadStateService';
import {dispatchMessageUpdateBroadcast} from '@app/api/channel/services/message/MessageGatewayDispatch';
import type {MessagePersistenceService} from '@app/api/channel/services/message/MessagePersistenceService';
import type {GuildAuditLogService} from '@app/api/guild/GuildAuditLogService';
import type {IGuildRepositoryAggregate} from '@app/api/guild/repositories/IGuildRepositoryAggregate';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {ISnowflakeService} from '@app/api/infrastructure/ISnowflakeService';
import type {LimitConfigService} from '@app/api/limits/LimitConfigService';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {Channel} from '@app/api/models/Channel';
import type {Message} from '@app/api/models/Message';
import type {MessageReaction} from '@app/api/models/MessageReaction';
import type {IUserRepository} from '@app/api/user/IUserRepository';
import {
	assertMayStartConversation,
	getNewConversationLimit,
	oneToOneDmRecipient,
} from '@app/api/user/NewConversationLimit';
import {assertGuildMemberCanCommunicate} from '@app/api/utils/GuildCommunicationUtils';
import {ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {NewConversationsLimitedError} from '@fluxer/errors/src/domains/user/NewConversationsLimitedError';
import type {ChannelPinResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import type {UserPartialResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';

export class MessageInteractionService {
	readonly authService: MessageInteractionAuthService;
	private pinAuthService: MessagePinAuthService;
	private readStateService: MessageReadStateService;
	private pinService: MessagePinService;
	private reactionService: MessageReactionService;

	constructor(
		private channelRepository: IChannelRepository,
		private userRepository: IUserRepository,
		guildRepository: IGuildRepositoryAggregate,
		private gatewayService: IGatewayService,
		snowflakeService: ISnowflakeService,
		messagePersistenceService: MessagePersistenceService,
		guildAuditLogService: GuildAuditLogService,
		limitConfigService: LimitConfigService,
	) {
		this.authService = new MessageInteractionAuthService(
			channelRepository,
			userRepository,
			guildRepository,
			gatewayService,
		);
		this.pinAuthService = new MessagePinAuthService(channelRepository, userRepository, guildRepository, gatewayService);
		this.readStateService = new MessageReadStateService(gatewayService);
		this.pinService = new MessagePinService(
			gatewayService,
			channelRepository,
			snowflakeService,
			messagePersistenceService,
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

	async startTyping({userId, channelId, personaId}: {userId: UserID; channelId: ChannelID; personaId?: PersonaID}): Promise<void> {
		const authChannel = await this.authService.getChannelAuthenticated({userId, channelId});
		await authChannel.checkPermission(Permissions.SEND_MESSAGES);
		assertGuildMemberCanCommunicate(authChannel.member);
		if (!authChannel.guild && (await this.startsNewConversation(authChannel.channel, userId))) return;
		await this.readStateService.startTyping({authChannel, userId, personaId});
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
		const authChannel = await this.pinAuthService.getChannelAuthenticated({userId, channelId});
		return this.pinService.getChannelPins({authChannel, userId, requestCache, beforeTimestamp, limit});
	}

	async pinMessage({
		userId,
		channelId,
		messageId,
		requestCache,
		auditLogReason,
	}: {
		userId: UserID;
		channelId: ChannelID;
		messageId: MessageID;
		requestCache: RequestCache;
		auditLogReason?: string | null;
	}): Promise<void> {
		const authChannel = await this.authService.getChannelAuthenticated({userId, channelId});
		if (!authChannel.guild && authChannel.channel.type !== ChannelTypes.DM_PERSONAL_NOTES) {
			await this.authService.validateDMSendPermissions({channel: authChannel.channel, userId});
			await this.assertConversationAllowed(authChannel.channel, userId);
		}
		await this.pinService.pinMessage({authChannel, messageId, userId, requestCache, auditLogReason});
	}

	async unpinMessage({
		userId,
		channelId,
		messageId,
		requestCache,
		auditLogReason,
	}: {
		userId: UserID;
		channelId: ChannelID;
		messageId: MessageID;
		requestCache: RequestCache;
		auditLogReason?: string | null;
	}): Promise<void> {
		const authChannel = await this.authService.getChannelAuthenticated({userId, channelId});
		if (!authChannel.guild && authChannel.channel.type !== ChannelTypes.DM_PERSONAL_NOTES) {
			await this.authService.validateDMSendPermissions({channel: authChannel.channel, userId});
		}
		await this.pinService.unpinMessage({authChannel, messageId, userId, requestCache, auditLogReason});
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
		if (!authChannel.guild) {
			await this.assertConversationAllowed(authChannel.channel, userId);
		}
		await this.reactionService.addReaction({authChannel, messageId, emoji, userId, sessionId});
	}

	private async startsNewConversation(channel: Channel, userId: UserID): Promise<boolean> {
		try {
			await this.assertConversationAllowed(channel, userId);
			return false;
		} catch (error) {
			if (error instanceof NewConversationsLimitedError) return true;
			throw error;
		}
	}

	private async assertConversationAllowed(channel: Channel, userId: UserID): Promise<void> {
		const targetId = oneToOneDmRecipient(channel, userId);
		if (targetId === null || !(await getNewConversationLimit(userId))) return;
		const user = await this.userRepository.findUnique(userId);
		if (!user) return;
		await assertMayStartConversation({
			user,
			targetId,
			users: this.userRepository,
			messages: this.channelRepository.messages,
			channel,
		});
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
		await this.reactionService.removeAllReactionsForEmoji({authChannel, messageId, emoji});
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
		await this.reactionService.removeAllReactions({authChannel, messageId});
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
