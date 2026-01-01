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

import {type ChannelID, createMessageID, createUserID, type GuildID, type MessageID, type UserID} from '~/BrandedTypes';
import {GuildOperations, Permissions} from '~/Constants';
import {AuditLogActionType} from '~/constants/AuditLogActionType';
import {
	CannotExecuteOnDmError,
	FeatureTemporarilyDisabledError,
	InputValidationError,
	MissingPermissionsError,
	UnknownMessageError,
} from '~/Errors';
import type {GuildAuditLogService} from '~/guild/GuildAuditLogService';
import type {ICloudflarePurgeQueue} from '~/infrastructure/CloudflarePurgeQueue';
import type {IStorageService} from '~/infrastructure/IStorageService';
import {getMetricsService} from '~/infrastructure/MetricsService';
import type {Channel, Message} from '~/Models';
import type {RequestCache} from '~/middleware/RequestCacheMiddleware';
import {getSnowflake} from '~/utils/SnowflakeUtils';
import type {IChannelRepositoryAggregate} from '../../repositories/IChannelRepositoryAggregate';
import type {MessageChannelAuthService} from './MessageChannelAuthService';
import type {MessageDispatchService} from './MessageDispatchService';
import {isOperationDisabled, purgeMessageAttachments} from './MessageHelpers';
import type {MessageSearchService} from './MessageSearchService';
import type {MessageValidationService} from './MessageValidationService';

interface MessageDeleteServiceDeps {
	channelRepository: IChannelRepositoryAggregate;
	storageService: IStorageService;
	cloudflarePurgeQueue: ICloudflarePurgeQueue;
	validationService: MessageValidationService;
	channelAuthService: MessageChannelAuthService;
	dispatchService: MessageDispatchService;
	searchService: MessageSearchService;
	guildAuditLogService: GuildAuditLogService;
}

export class MessageDeleteService {
	private readonly guildAuditLogService: GuildAuditLogService;

	constructor(private readonly deps: MessageDeleteServiceDeps) {
		this.guildAuditLogService = deps.guildAuditLogService;
	}

	async deleteMessage({
		userId,
		channelId,
		messageId,
	}: {
		userId: UserID;
		channelId: ChannelID;
		messageId: MessageID;
		requestCache: RequestCache;
	}): Promise<void> {
		try {
			const {channel, guild, hasPermission} = await this.deps.channelAuthService.getChannelAuthenticated({
				userId,
				channelId,
			});

			if (isOperationDisabled(guild, GuildOperations.SEND_MESSAGE)) {
				throw new FeatureTemporarilyDisabledError();
			}

			const message = await this.deps.channelRepository.messages.getMessage(channelId, messageId);
			if (!message) throw new UnknownMessageError();

			const canDelete = await this.deps.validationService.canDeleteMessage({message, userId, guild, hasPermission});
			if (!canDelete) throw new MissingPermissionsError();

			if (message.pinnedTimestamp) {
				await this.deps.channelRepository.messageInteractions.removeChannelPin(channelId, messageId);
			}

			await purgeMessageAttachments(message, this.deps.storageService, this.deps.cloudflarePurgeQueue);
			await this.deps.channelRepository.messages.deleteMessage(
				channelId,
				messageId,
				message.authorId || createUserID(0n),
				message.pinnedTimestamp || undefined,
			);

			await this.deps.dispatchService.dispatchMessageDelete({channel, messageId, message});

			if (message.pinnedTimestamp) {
				await this.deps.dispatchService.dispatchEvent({
					channel,
					event: 'CHANNEL_PINS_UPDATE',
					data: {
						channel_id: channel.id.toString(),
						last_pin_timestamp: channel.lastPinTimestamp?.toISOString() ?? null,
					},
				});
			}

			if (channel.guildId) {
				await this.guildAuditLogService
					.createBuilder(channel.guildId, userId)
					.withAction(AuditLogActionType.MESSAGE_DELETE, message.id.toString())
					.withMetadata({channel_id: channel.id.toString()})
					.withReason(null)
					.commit();
			}

			if (channel.indexedAt) {
				void this.deps.searchService.deleteMessageIndex(messageId);
			}

			getMetricsService().counter({name: 'message.delete'});
		} catch (error) {
			getMetricsService().counter({name: 'message.delete.error'});
			throw error;
		}
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
		if (messageIds.length === 0) {
			throw InputValidationError.create('message_ids', 'message_ids cannot be empty');
		}

		if (messageIds.length > 100) {
			throw InputValidationError.create('message_ids', 'Cannot delete more than 100 messages at once');
		}

		const {channel, guild, checkPermission} = await this.deps.channelAuthService.getChannelAuthenticated({
			userId,
			channelId,
		});

		if (!guild) throw new CannotExecuteOnDmError();
		await checkPermission(Permissions.MANAGE_MESSAGES);

		const messages = await Promise.all(
			messageIds.map((id) => this.deps.channelRepository.messages.getMessage(channelId, id)),
		);
		const existingMessages = messages.filter((m: Message | null) => m && m.channelId === channelId) as Array<Message>;

		if (existingMessages.length === 0) return;

		await Promise.all(
			existingMessages.map((message) =>
				purgeMessageAttachments(message, this.deps.storageService, this.deps.cloudflarePurgeQueue),
			),
		);
		await this.deps.channelRepository.messages.bulkDeleteMessages(channelId, messageIds);

		await this.deps.dispatchService.dispatchMessageDeleteBulk({channel, messageIds});

		if (channel.guildId && existingMessages.length > 0) {
			await this.guildAuditLogService
				.createBuilder(channel.guildId, userId)
				.withAction(AuditLogActionType.MESSAGE_BULK_DELETE, null)
				.withMetadata({
					channel_id: channel.id.toString(),
					count: existingMessages.length.toString(),
				})
				.withReason(null)
				.commit();
		}
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
		const channels = await this.deps.channelRepository.channelData.listGuildChannels(guildId);

		const cutoffTimestamp = Date.now() - days * 24 * 60 * 60 * 1000;
		const afterSnowflake = createMessageID(getSnowflake(cutoffTimestamp));

		await Promise.all(
			channels.map(async (channel: Channel) => {
				const batchSize = 100;
				let beforeMessageId: MessageID | undefined;
				let hasMore = true;

				while (hasMore) {
					const messages = await this.deps.channelRepository.messages.listMessages(
						channel.id,
						beforeMessageId,
						batchSize,
						afterSnowflake,
					);

					if (messages.length === 0) {
						hasMore = false;
						break;
					}

					const userMessages = messages.filter((msg: Message) => msg.authorId === userId);

					if (userMessages.length > 0) {
						const messageIds = userMessages.map((msg: Message) => msg.id);

						await Promise.all(
							userMessages.map((message: Message) =>
								purgeMessageAttachments(message, this.deps.storageService, this.deps.cloudflarePurgeQueue),
							),
						);

						await this.deps.channelRepository.messages.bulkDeleteMessages(channel.id, messageIds);

						await this.deps.dispatchService.dispatchMessageDeleteBulk({channel, messageIds});
					}

					if (messages.length < batchSize) {
						hasMore = false;
					} else {
						beforeMessageId = messages[messages.length - 1].id;
					}
				}
			}),
		);
	}
}
