// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID, MessageID, UserID} from '@app/api/BrandedTypes';
import {createMessageID, createUserID} from '@app/api/BrandedTypes';
import type {ChannelRepository} from '@app/api/channel/ChannelRepository';
import type {CrosspostPropagation} from '@app/api/channel/services/message/CrosspostPropagation';
import type {MessageChannelAuthService} from '@app/api/channel/services/message/MessageChannelAuthService';
import type {MessageDispatchService} from '@app/api/channel/services/message/MessageDispatchService';
import {
	decrementThreadMessageCount,
	isOperationDisabled,
	purgeMessageAttachments,
} from '@app/api/channel/services/message/MessageHelpers';
import type {MessageSearchService} from '@app/api/channel/services/message/MessageSearchService';
import type {MessageValidationService} from '@app/api/channel/services/message/MessageValidationService';
import type {ThreadViewer} from '@app/api/experiment/ChannelThreadsGate';
import type {GuildAuditLogService} from '@app/api/guild/GuildAuditLogService';
import type {IPurgeQueue} from '@app/api/infrastructure/CachePurgeQueue';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {IStorageService} from '@app/api/infrastructure/IStorageService';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {Channel} from '@app/api/models/Channel';
import type {Message} from '@app/api/models/Message';
import type {Webhook} from '@app/api/models/Webhook';
import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {GuildOperations} from '@fluxer/constants/src/GuildConstants';
import {TEXT_THREAD_PARENT_CHANNEL_TYPES} from '@fluxer/constants/src/ThreadConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InvalidChannelTypeError} from '@fluxer/errors/src/domains/channel/InvalidChannelTypeError';
import {UnknownMessageError} from '@fluxer/errors/src/domains/channel/UnknownMessageError';
import {CannotExecuteOnDmError} from '@fluxer/errors/src/domains/core/CannotExecuteOnDmError';
import {FeatureTemporarilyDisabledError} from '@fluxer/errors/src/domains/core/FeatureTemporarilyDisabledError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {MissingPermissionsError} from '@fluxer/errors/src/domains/core/MissingPermissionsError';
import {createSnowflakeFromTimestamp} from '@fluxer/snowflake/src/Snowflake';
import {ms} from 'itty-time';

interface MessageDeleteServiceDeps {
	channelRepository: ChannelRepository;
	storageService: IStorageService;
	purgeQueue: IPurgeQueue;
	validationService: MessageValidationService;
	channelAuthService: MessageChannelAuthService;
	dispatchService: MessageDispatchService;
	searchService: MessageSearchService;
	gatewayService: IGatewayService;
	guildAuditLogService: GuildAuditLogService;
	crosspostPropagation: CrosspostPropagation;
}

export class MessageDeleteService {
	private readonly guildAuditLogService: GuildAuditLogService;

	constructor(private readonly deps: MessageDeleteServiceDeps) {
		this.guildAuditLogService = deps.guildAuditLogService;
	}

	async deleteMessage({
		userId,
		viewer,
		channelId,
		messageId,
		skipGuildAuditLog,
		auditLogReason,
	}: {
		userId: UserID;
		viewer: ThreadViewer;
		channelId: ChannelID;
		messageId: MessageID;
		requestCache: RequestCache;
		skipGuildAuditLog?: boolean;
		auditLogReason?: string | null;
	}): Promise<void> {
		const {channel, guild, hasPermission, thread} = await this.deps.channelAuthService.getChannelAuthenticated({
			userId,
			channelId,
			viewer,
		});
		if (isOperationDisabled(guild, GuildOperations.SEND_MESSAGE)) {
			throw new FeatureTemporarilyDisabledError();
		}
		const message = await this.deps.channelRepository.messages.getMessage(channelId, messageId);
		if (!message) {
			if (
				thread?.state.hasStarter &&
				TEXT_THREAD_PARENT_CHANNEL_TYPES.has(thread.parent.type) &&
				messageId.toString() === channel.id.toString()
			) {
				throw new MissingPermissionsError();
			}
			throw new UnknownMessageError();
		}
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
		await this.decrementThreadMessageCount(channel, [messageId]);
		await this.deps.dispatchService.dispatchMessageDelete({channel, messageId, message});
		await this.deps.crosspostPropagation.enqueueCrosspostSourceRemoval({
			messages: [message],
			mode: 'source_deleted',
			channel,
		});
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
				.withThreadScope(channel.isThread())
				.withReason(auditLogReason ?? null)
				.commit();
		}
		await this.deps.searchService.deleteMessageIndex(messageId);
	}

	async deleteWebhookMessage({
		webhook,
		thread,
		messageId,
	}: {
		webhook: Webhook;
		thread?: Channel | null;
		messageId: MessageID;
		requestCache: RequestCache;
	}): Promise<void> {
		const channelId = thread?.id ?? webhook.channelId!;
		const channel = thread ?? (await this.deps.channelRepository.channelData.findUnique(channelId));
		if (!channel?.guildId) {
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
		await this.decrementThreadMessageCount(channel, [messageId]);
		await this.deps.dispatchService.dispatchMessageDelete({channel, messageId, message});
		await this.deps.crosspostPropagation.enqueueCrosspostSourceRemoval({
			messages: [message],
			mode: 'source_deleted',
			channel,
		});
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

	private async decrementThreadMessageCount(channel: Channel, messageIds: Array<MessageID>): Promise<void> {
		await decrementThreadMessageCount(this.deps.channelRepository, channel, messageIds);
	}

	async bulkDeleteMessages({
		userId,
		viewer,
		channelId,
		messageIds,
		auditLogReason,
	}: {
		userId: UserID;
		viewer: ThreadViewer;
		channelId: ChannelID;
		messageIds: Array<MessageID>;
		auditLogReason?: string | null;
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
			viewer,
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
		const existingIds = existingMessages.map((message) => message.id);
		await this.decrementThreadMessageCount(channel, existingIds);
		await this.deps.dispatchService.dispatchMessageDeleteBulk({
			channel,
			messageIds: channel.isThread() ? existingIds : messageIds,
		});
		await this.deps.crosspostPropagation.enqueueCrosspostSourceRemoval({
			messages: existingMessages,
			mode: 'source_deleted',
			channel,
		});
		if (channel.guildId && existingMessages.length > 0) {
			await this.guildAuditLogService
				.createBuilder(channel.guildId, userId)
				.withAction(AuditLogActionType.MESSAGE_BULK_DELETE, null)
				.withMetadata({
					channel_id: channel.id.toString(),
					count: existingMessages.length.toString(),
				})
				.withThreadScope(channel.isThread())
				.withReason(auditLogReason ?? null)
				.commit();
		}
		await this.deps.searchService.deleteMessagesIndex(messageIds);
	}

	async purgePersonalNotesMessages({
		userId,
		channelId,
		viewer,
	}: {
		userId: UserID;
		viewer: ThreadViewer;
		channelId: ChannelID;
	}): Promise<{
		deletedCount: number;
	}> {
		const {channel} = await this.deps.channelAuthService.getChannelAuthenticated({userId, channelId, viewer});
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
		const cutoffTimestamp = Date.now() - seconds * ms('1 second');
		const guildChannels = await this.deps.channelRepository.channelData.listGuildChannels(guildId, 'complete');
		const threadIds = await this.deps.channelRepository.threads.listGuildThreadIds(guildId, {
			parents: guildChannels,
			activeSince: new Date(cutoffTimestamp),
		});
		const channels =
			threadIds.length > 0
				? [...guildChannels, ...(await this.deps.channelRepository.channelData.listChannels(threadIds))]
				: guildChannels;
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
						await this.decrementThreadMessageCount(channel, messageIds);
						await this.deps.dispatchService.dispatchMessageDeleteBulk({channel, messageIds});
						await this.deps.crosspostPropagation.enqueueCrosspostSourceRemoval({
							messages: userMessages,
							mode: 'source_deleted',
							channel,
						});
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
