// SPDX-License-Identifier: AGPL-3.0-or-later

import type {MessageID, UserID} from '@app/api/BrandedTypes';
import {createMessageID} from '@app/api/BrandedTypes';
import type {ChannelRepository} from '@app/api/channel/ChannelRepository';
import type {AuthenticatedChannel} from '@app/api/channel/services/AuthenticatedChannel';
import {dispatchChannelEvent} from '@app/api/channel/services/ChannelGatewayDispatch';
import {
	dispatchMessageCreateBroadcast,
	dispatchMessageUpdateBroadcast,
} from '@app/api/channel/services/message/MessageGatewayDispatch';
import {ensureTextChannel, isOperationDisabled} from '@app/api/channel/services/message/MessageHelpers';
import type {MessagePersistenceService} from '@app/api/channel/services/message/MessagePersistenceService';
import {createMessageResponseDataService} from '@app/api/channel/services/message/MessageResponseDataService';
import {assertThreadInteractionAllowed} from '@app/api/channel/services/thread/ThreadInteractionGuards';
import type {GuildAuditLogService} from '@app/api/guild/GuildAuditLogService';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {ISnowflakeService} from '@app/api/infrastructure/ISnowflakeService';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {Channel} from '@app/api/models/Channel';
import type {Message} from '@app/api/models/Message';
import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {MessageTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {GuildOperations} from '@fluxer/constants/src/GuildConstants';
import {THREAD_ONLY_CHANNEL_TYPES} from '@fluxer/constants/src/ThreadConstants';
import {CannotEditSystemMessageError} from '@fluxer/errors/src/domains/channel/CannotEditSystemMessageError';
import {InvalidChannelTypeError} from '@fluxer/errors/src/domains/channel/InvalidChannelTypeError';
import {UnknownMessageError} from '@fluxer/errors/src/domains/channel/UnknownMessageError';
import {FeatureTemporarilyDisabledError} from '@fluxer/errors/src/domains/core/FeatureTemporarilyDisabledError';
import type {ChannelPinResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {snowflakeToDate} from '@fluxer/snowflake/src/Snowflake';

const PIN_LIST_UNBOUNDED_TIMESTAMP = new Date('9999-12-31T23:59:59.999Z');

export class MessagePinService {
	constructor(
		private gatewayService: IGatewayService,
		private channelRepository: ChannelRepository,
		private snowflakeService: ISnowflakeService,
		private messagePersistenceService: MessagePersistenceService,
		private readonly guildAuditLogService: GuildAuditLogService,
	) {}

	async getChannelPins({
		authChannel,
		userId,
		beforeTimestamp,
		limit,
	}: {
		authChannel: AuthenticatedChannel;
		userId: UserID;
		beforeTimestamp?: Date;
		limit?: number;
	}): Promise<{
		items: Array<ChannelPinResponse>;
		has_more: boolean;
	}> {
		const {channel} = authChannel;
		if (THREAD_ONLY_CHANNEL_TYPES.has(channel.type)) throw new InvalidChannelTypeError();
		ensureTextChannel(channel);
		const hasReadHistory = !authChannel.guild || (await authChannel.hasPermission(Permissions.READ_MESSAGE_HISTORY));
		if (!hasReadHistory) {
			const cutoff = authChannel.guild?.message_history_cutoff;
			if (!cutoff) {
				return {items: [], has_more: false};
			}
		}
		const pageSize = Math.min(limit ?? 50, 50);
		const cutoffTimestamp = hasReadHistory ? null : new Date(authChannel.guild!.message_history_cutoff!).getTime();
		const filtered: Array<Message> = [];
		let before = beforeTimestamp ?? PIN_LIST_UNBOUNDED_TIMESTAMP;
		let exhausted = false;
		while (filtered.length <= pageSize && !exhausted) {
			const messages = await this.channelRepository.messageInteractions.listChannelPins(
				channel.id,
				before,
				pageSize + 1,
			);
			const sorted = messages.sort((a, b) => (b.pinnedTimestamp?.getTime() ?? 0) - (a.pinnedTimestamp?.getTime() ?? 0));
			exhausted = messages.length <= pageSize;
			for (const message of sorted) {
				if (cutoffTimestamp === null || snowflakeToDate(message.id).getTime() >= cutoffTimestamp) {
					filtered.push(message);
				}
			}
			const last = sorted[sorted.length - 1];
			if (!last?.pinnedTimestamp) {
				break;
			}
			before = last.pinnedTimestamp;
		}
		const hasMore = filtered.length > pageSize;
		const trimmed = filtered.slice(0, pageSize);
		const access = {
			sourceGuildId: channel.guildId,
			messageHistoryCutoff: hasReadHistory ? null : (authChannel.guild?.message_history_cutoff ?? null),
			canReadMessageHistory: hasReadHistory,
		};
		const responseDataService = createMessageResponseDataService();
		const messageResponses = await responseDataService.buildMessages({
			userId,
			messages: trimmed,
			access,
		});
		const responseByMessageId = new Map(messageResponses.map((response) => [response.id, response]));
		const items = trimmed.flatMap((message: Message) => {
			const messageResponse = responseByMessageId.get(message.id.toString());
			if (!messageResponse || !message.pinnedTimestamp) return [];
			const {referenced_message: _referencedMessage, reactions: _reactions, ...pinnedMessage} = messageResponse;
			return [
				{
					message: pinnedMessage,
					pinned_at: message.pinnedTimestamp.toISOString(),
				},
			];
		});
		return {
			items,
			has_more: hasMore,
		};
	}

	async pinMessage({
		authChannel,
		messageId,
		userId,
		auditLogReason,
	}: {
		authChannel: AuthenticatedChannel;
		messageId: MessageID;
		userId: UserID;
		requestCache: RequestCache;
		auditLogReason?: string | null;
	}): Promise<void> {
		const {channel} = authChannel;
		const message = await this.loadPinnableMessage(authChannel, messageId);
		if (message.pinnedTimestamp) return;
		const now = new Date();
		const updatedMessageData = {...message.toRow(), pinned_timestamp: now};
		const updatedMessage = await this.channelRepository.messages.upsertMessage(updatedMessageData, message.toRow());
		await this.channelRepository.messageInteractions.addChannelPin(channel.id, messageId, now);
		const updatedChannelData = {...channel.toRow(), last_pin_timestamp: now};
		const updatedChannel = await this.channelRepository.channelData.upsert(updatedChannelData, channel.toRow());
		await this.dispatchChannelPinsUpdate(updatedChannel);
		await this.sendPinSystemMessage({channel, message, userId});
		await dispatchMessageUpdateBroadcast({
			gatewayService: this.gatewayService,
			channel,
			message: updatedMessage,
		});
		await this.recordPinAudit(AuditLogActionType.MESSAGE_PIN, channel, messageId, userId, auditLogReason);
	}

	async unpinMessage({
		authChannel,
		messageId,
		userId,
		auditLogReason,
	}: {
		authChannel: AuthenticatedChannel;
		messageId: MessageID;
		userId: UserID;
		auditLogReason?: string | null;
	}): Promise<void> {
		const {channel} = authChannel;
		const message = await this.loadPinnableMessage(authChannel, messageId);
		if (!message.pinnedTimestamp) return;
		const updatedMessageData = {...message.toRow(), pinned_timestamp: null};
		const updatedMessage = await this.channelRepository.messages.upsertMessage(updatedMessageData, message.toRow());
		await this.channelRepository.messageInteractions.removeChannelPin(channel.id, messageId);
		await this.dispatchChannelPinsUpdate(channel);
		await dispatchMessageUpdateBroadcast({
			gatewayService: this.gatewayService,
			channel,
			message: updatedMessage,
		});
		await this.recordPinAudit(AuditLogActionType.MESSAGE_UNPIN, channel, messageId, userId, auditLogReason);
	}

	private async loadPinnableMessage(authChannel: AuthenticatedChannel, messageId: MessageID): Promise<Message> {
		const {channel, guild, checkPermission} = authChannel;
		if (guild) {
			await checkPermission(Permissions.PIN_MESSAGES);
			if (isOperationDisabled(guild, GuildOperations.SEND_MESSAGE)) {
				throw new FeatureTemporarilyDisabledError();
			}
		}
		ensureTextChannel(channel);
		assertThreadInteractionAllowed(authChannel, 'pin');
		const message = await this.channelRepository.messages.getMessage(channel.id, messageId);
		if (!message) throw new UnknownMessageError();
		this.validateMessagePinnable(message);
		return message;
	}

	private async recordPinAudit(
		action: AuditLogActionType,
		channel: Channel,
		messageId: MessageID,
		userId: UserID,
		auditLogReason?: string | null,
	): Promise<void> {
		if (!channel.guildId) return;
		await this.guildAuditLogService
			.createBuilder(channel.guildId, userId)
			.withAction(action, messageId.toString())
			.withMetadata({
				channel_id: channel.id.toString(),
				message_id: messageId.toString(),
			})
			.withThreadScope(channel.isThread())
			.withReason(auditLogReason ?? null)
			.commit();
	}

	private validateMessagePinnable(message: Message): void {
		const pinnableTypes: ReadonlySet<Message['type']> = new Set([MessageTypes.DEFAULT, MessageTypes.REPLY]);
		if (!pinnableTypes.has(message.type)) {
			throw new CannotEditSystemMessageError();
		}
	}

	private async sendPinSystemMessage({
		channel,
		message,
		userId,
	}: {
		channel: Channel;
		message: Message;
		userId: UserID;
	}): Promise<void> {
		const messageId = createMessageID(await this.snowflakeService.generateForChannel(channel.id));
		const systemMessage = await this.messagePersistenceService.createSystemMessage({
			messageId,
			channelId: channel.id,
			userId,
			type: MessageTypes.CHANNEL_PINNED_MESSAGE,
			guildId: channel.guildId,
			messageReference: {
				channel_id: channel.id,
				message_id: message.id,
				guild_id: null,
				type: 0,
			},
		});
		await dispatchMessageCreateBroadcast({
			gatewayService: this.gatewayService,
			channel,
			message: systemMessage,
		});
	}

	private async dispatchChannelPinsUpdate(channel: Channel): Promise<void> {
		await dispatchChannelEvent({
			gatewayService: this.gatewayService,
			channel,
			event: 'CHANNEL_PINS_UPDATE',
			data: {
				channel_id: channel.id.toString(),
				last_pin_timestamp: channel.lastPinTimestamp?.toISOString() ?? null,
			},
		});
	}
}
