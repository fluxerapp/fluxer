// SPDX-License-Identifier: AGPL-3.0-or-later

import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {GuildOperations} from '@fluxer/constants/src/GuildConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InvalidChannelTypeError} from '@fluxer/errors/src/domains/channel/InvalidChannelTypeError';
import {UnknownMessageError} from '@fluxer/errors/src/domains/channel/UnknownMessageError';
import {CannotExecuteOnDmError} from '@fluxer/errors/src/domains/core/CannotExecuteOnDmError';
import {FeatureTemporarilyDisabledError} from '@fluxer/errors/src/domains/core/FeatureTemporarilyDisabledError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {MissingPermissionsError} from '@fluxer/errors/src/domains/core/MissingPermissionsError';
import {createSnowflakeFromTimestamp} from '@fluxer/snowflake/src/Snowflake';
import {ms} from 'itty-time';
import type {ChannelID, GuildID, MessageID, UserID} from '../../../BrandedTypes';
import {createMessageID, createUserID} from '../../../BrandedTypes';
import type {GuildAuditLogService} from '../../../guild/GuildAuditLogService';
import type {IPurgeQueue} from '../../../infrastructure/BunnyPurgeQueue';
import type {IGatewayService} from '../../../infrastructure/IGatewayService';
import type {IStorageService} from '../../../infrastructure/IStorageService';
import type {RequestCache} from '../../../middleware/RequestCacheMiddleware';
import type {Channel} from '../../../models/Channel';
import type {Message} from '../../../models/Message';
import type {Webhook} from '../../../models/Webhook';
import type {IChannelRepositoryAggregate} from '../../repositories/IChannelRepositoryAggregate';
import type {MessageChannelAuthService} from './MessageChannelAuthService';
import type {MessageDispatchService} from './MessageDispatchService';
import {isOperationDisabled, purgeMessageAttachments} from './MessageHelpers';
import type {MessageSearchService} from './MessageSearchService';
import type {MessageValidationService} from './MessageValidationService';

interface MessageDeleteServiceDeps {
	channelRepository: IChannelRepositoryAggregate;
	storageService: IStorageService;
	purgeQueue: IPurgeQueue;
	validationService: MessageValidationService;
	channelAuthService: MessageChannelAuthService;
	dispatchService: MessageDispatchService;
	searchService: MessageSearchService;
	gatewayService: IGatewayService;
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
		skipGuildAuditLog,
	}: {
		userId: UserID;
		channelId: ChannelID;
		messageId: MessageID;
		requestCache: RequestCache;
		skipGuildAuditLog?: boolean;
	}): Promise<void> {
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
		await purgeMessageAttachments(message, this.deps.storageService, this.deps.purgeQueue);
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
		if (channel.guildId && !skipGuildAuditLog) {
			await this.guildAuditLogService
				.createBuilder(channel.guildId, userId)
				.withAction(AuditLogActionType.MESSAGE_DELETE, message.id.toString())
				.withMetadata({channel_id: channel.id.toString()})
				.withReason(null)
				.commit();
		}
		await this.deps.searchService.deleteMessageIndex(messageId);
	}

	async deleteWebhookMessage({
		webhook,
		messageId,
	}: {
		webhook: Webhook;
		messageId: MessageID;
		requestCache: RequestCache;
	}): Promise<void> {
		const channelId = webhook.channelId!;
		const channel = await this.deps.channelRepository.channelData.findUnique(channelId);
		if (!channel || !channel.guildId) {
			throw new CannotExecuteOnDmError();
		}
		const message = await this.deps.channelRepository.messages.getMessage(channelId, messageId);
		if (!message) throw new UnknownMessageError();
		if (message.webhookId !== webhook.id) {
			throw new MissingPermissionsError();
		}
		if (message.pinnedTimestamp) {
			await this.deps.channelRepository.messageInteractions.removeChannelPin(channelId, messageId);
		}
		await purgeMessageAttachments(message, this.deps.storageService, this.deps.purgeQueue);
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
		await this.deps.searchService.deleteMessageIndex(messageId);
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
			throw InputValidationError.fromCode('message_ids', ValidationErrorCodes.MESSAGE_IDS_CANNOT_BE_EMPTY);
		}
		if (messageIds.length > 100) {
			throw InputValidationError.fromCode('message_ids', ValidationErrorCodes.CANNOT_DELETE_MORE_THAN_100_MESSAGES);
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
		const existingMessages = messages.filter(isExistingMessageInChannel(channelId));
		if (existingMessages.length === 0) return;
		await Promise.all(
			existingMessages.map((message) =>
				purgeMessageAttachments(message, this.deps.storageService, this.deps.purgeQueue),
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
		await this.deps.searchService.deleteMessagesIndex(messageIds);
	}

	async purgePersonalNotesMessages({userId, channelId}: {userId: UserID; channelId: ChannelID}): Promise<{
		deletedCount: number;
	}> {
		const {channel} = await this.deps.channelAuthService.getChannelAuthenticated({userId, channelId});
		if (
			channel.type !== ChannelTypes.DM_PERSONAL_NOTES ||
			!this.deps.channelAuthService.isPersonalNotesChannel({userId, channelId})
		) {
			throw new InvalidChannelTypeError();
		}
		const PAGE_SIZE = 100;
		let beforeMessageId: MessageID | undefined;
		let totalDeleted = 0;
		while (true) {
			const messages = await this.deps.channelRepository.messages.listMessages(channelId, beforeMessageId, PAGE_SIZE);
			if (messages.length === 0) break;
			const messageIds = messages.map((message) => message.id);
			await Promise.all(
				messages.map((message) => purgeMessageAttachments(message, this.deps.storageService, this.deps.purgeQueue)),
			);
			await this.deps.channelRepository.messages.bulkDeleteMessages(channelId, messageIds);
			await this.deps.dispatchService.dispatchMessageDeleteBulk({channel, messageIds});
			await this.deps.searchService.deleteMessagesIndex(messageIds);
			totalDeleted += messages.length;
			if (messages.length < PAGE_SIZE) break;
			beforeMessageId = messages[messages.length - 1].id;
		}
		return {deletedCount: totalDeleted};
	}

	async deleteUserMessagesInGuild({
		userId,
		guildId,
		seconds,
	}: {
		userId: UserID;
		guildId: GuildID;
		seconds: number;
	}): Promise<void> {
		const channels = await this.deps.channelRepository.channelData.listGuildChannels(guildId);
		const cutoffTimestamp = Date.now() - seconds * ms('1 second');
		const cutoffSnowflake = createMessageID(createSnowflakeFromTimestamp(cutoffTimestamp));
		await Promise.all(
			channels.map(async (channel: Channel) => {
				const batchSize = 100;
				let beforeMessageId: MessageID | undefined;
				while (true) {
					const messages = await this.deps.channelRepository.messages.listMessages(
						channel.id,
						beforeMessageId,
						batchSize,
					);
					if (messages.length === 0) break;
					const inWindow = messages.filter((msg: Message) => msg.id > cutoffSnowflake);
					const userMessages = inWindow.filter((msg: Message) => msg.authorId === userId);
					if (userMessages.length > 0) {
						const messageIds = userMessages.map((msg: Message) => msg.id);
						await Promise.all(
							userMessages.map((message: Message) =>
								purgeMessageAttachments(message, this.deps.storageService, this.deps.purgeQueue),
							),
						);
						await this.deps.channelRepository.messages.bulkDeleteMessages(channel.id, messageIds);
						await this.deps.dispatchService.dispatchMessageDeleteBulk({channel, messageIds});
						await this.deps.searchService.deleteMessagesIndex(messageIds);
					}
					if (inWindow.length < messages.length || messages.length < batchSize) break;
					beforeMessageId = messages[messages.length - 1].id;
				}
			}),
		);
	}
}

function isExistingMessageInChannel(channelId: ChannelID): (message: Message | null) => message is Message {
	return (message: Message | null): message is Message => message?.channelId === channelId;
}
