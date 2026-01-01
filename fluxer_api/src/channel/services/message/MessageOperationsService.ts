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

import type {ChannelID, GuildID, MessageID, UserID} from '~/BrandedTypes';
import type {MessageRequest, MessageUpdateRequest} from '~/channel/ChannelModel';
import type {IFavoriteMemeRepository} from '~/favorite_meme/IFavoriteMemeRepository';
import type {GuildAuditLogService} from '~/guild/GuildAuditLogService';
import type {ICloudflarePurgeQueue} from '~/infrastructure/CloudflarePurgeQueue';
import type {ICacheService} from '~/infrastructure/ICacheService';
import type {IGatewayService} from '~/infrastructure/IGatewayService';
import type {IRateLimitService} from '~/infrastructure/IRateLimitService';
import type {IStorageService} from '~/infrastructure/IStorageService';
import type {SnowflakeService} from '~/infrastructure/SnowflakeService';
import type {Message, User, Webhook} from '~/Models';
import type {RequestCache} from '~/middleware/RequestCacheMiddleware';
import type {IUserRepository} from '~/user/IUserRepository';
import type {IChannelRepositoryAggregate} from '../../repositories/IChannelRepositoryAggregate';
import type {MessageChannelAuthService} from './MessageChannelAuthService';
import {MessageDeleteService} from './MessageDeleteService';
import type {MessageDispatchService} from './MessageDispatchService';
import {MessageEditService} from './MessageEditService';
import type {MessageMentionService} from './MessageMentionService';
import {MessageOperationsHelpers} from './MessageOperationsHelpers';
import type {MessagePersistenceService} from './MessagePersistenceService';
import type {MessageProcessingService} from './MessageProcessingService';
import type {MessageSearchService} from './MessageSearchService';
import {MessageSendService} from './MessageSendService';
import type {MessageValidationService} from './MessageValidationService';

export class MessageOperationsService {
	private readonly sendService: MessageSendService;
	private readonly editService: MessageEditService;
	private readonly deleteService: MessageDeleteService;
	private readonly operationsHelpers: MessageOperationsHelpers;

	constructor(
		channelRepository: IChannelRepositoryAggregate,
		userRepository: IUserRepository,
		cacheService: ICacheService,
		storageService: IStorageService,
		gatewayService: IGatewayService,
		snowflakeService: SnowflakeService,
		rateLimitService: IRateLimitService,
		cloudflarePurgeQueue: ICloudflarePurgeQueue,
		favoriteMemeRepository: IFavoriteMemeRepository,
		validationService: MessageValidationService,
		mentionService: MessageMentionService,
		searchService: MessageSearchService,
		persistenceService: MessagePersistenceService,
		channelAuthService: MessageChannelAuthService,
		processingService: MessageProcessingService,
		guildAuditLogService: GuildAuditLogService,
		dispatchService: MessageDispatchService,
	) {
		this.operationsHelpers = new MessageOperationsHelpers({
			channelRepository,
			cacheService,
			storageService,
			snowflakeService,
			favoriteMemeRepository,
		});

		this.sendService = new MessageSendService({
			channelRepository,
			storageService,
			gatewayService,
			snowflakeService,
			rateLimitService,
			favoriteMemeRepository,
			validationService,
			mentionService,
			searchService,
			persistenceService,
			channelAuthService,
			processingService,
			dispatchService,
			embedAttachmentResolver: persistenceService.getEmbedAttachmentResolver(),
			operationsHelpers: this.operationsHelpers,
		});

		this.editService = new MessageEditService({
			channelRepository,
			userRepository,
			validationService,
			persistenceService,
			channelAuthService,
			processingService,
			dispatchService,
			searchService,
			embedAttachmentResolver: persistenceService.getEmbedAttachmentResolver(),
			mentionService,
		});

		this.deleteService = new MessageDeleteService({
			channelRepository,
			storageService,
			cloudflarePurgeQueue,
			validationService,
			channelAuthService,
			dispatchService,
			searchService,
			guildAuditLogService,
		});
	}

	async sendMessage({
		user,
		channelId,
		data,
		requestCache,
	}: {
		user: User;
		channelId: ChannelID;
		data: MessageRequest;
		requestCache: RequestCache;
	}): Promise<Message> {
		return this.sendService.sendMessage({user, channelId, data, requestCache});
	}

	async sendWebhookMessage({
		webhook,
		data,
		username,
		avatar,
		requestCache,
	}: {
		webhook: Webhook;
		data: MessageRequest;
		username?: string | null;
		avatar?: string | null;
		requestCache: RequestCache;
	}): Promise<Message> {
		return this.sendService.sendWebhookMessage({webhook, data, username, avatar, requestCache});
	}

	async validateMessageCanBeSent({
		user,
		channelId,
		data,
	}: {
		user: User;
		channelId: ChannelID;
		data: MessageRequest;
	}): Promise<void> {
		return this.sendService.validateMessageCanBeSent({user, channelId, data});
	}

	async editMessage({
		userId,
		channelId,
		messageId,
		data,
		requestCache,
	}: {
		userId: UserID;
		channelId: ChannelID;
		messageId: MessageID;
		data: MessageUpdateRequest;
		requestCache: RequestCache;
	}): Promise<Message> {
		return this.editService.editMessage({userId, channelId, messageId, data, requestCache});
	}

	async deleteMessage({
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
		return this.deleteService.deleteMessage({userId, channelId, messageId, requestCache});
	}

	async bulkDeleteMessages({
		userId,
		channelId,
		messageIds,
	}: {
		userId: UserID;
		channelId: ChannelID;
		messageIds: Array<MessageID>;
	}): Promise<void> {
		return this.deleteService.bulkDeleteMessages({userId, channelId, messageIds});
	}

	async deleteUserMessagesInGuild({
		userId,
		guildId,
		days,
	}: {
		userId: UserID;
		guildId: GuildID;
		days: number;
	}): Promise<void> {
		return this.deleteService.deleteUserMessagesInGuild({userId, guildId, days});
	}
}
