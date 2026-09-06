// SPDX-License-Identifier: AGPL-3.0-or-later

import {dispatchChannelEvent} from '@app/api/channel/services/ChannelGatewayDispatch';
import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {MessageTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {GuildOperations} from '@fluxer/constants/src/GuildConstants';
import {CannotEditSystemMessageError} from '@fluxer/errors/src/domains/channel/CannotEditSystemMessageError';
import {UnknownMessageError} from '@fluxer/errors/src/domains/channel/UnknownMessageError';
import {FeatureTemporarilyDisabledError} from '@fluxer/errors/src/domains/core/FeatureTemporarilyDisabledError';
import type {ChannelPinResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {snowflakeToDate} from '@fluxer/snowflake/src/Snowflake';
import type {MessageID, UserID} from '../../../BrandedTypes';
import {createMessageID} from '../../../BrandedTypes';
import type {GuildAuditLogService} from '../../../guild/GuildAuditLogService';
import type {IGatewayService} from '../../../infrastructure/IGatewayService';
import type {ISnowflakeService} from '../../../infrastructure/ISnowflakeService';
import type {RequestCache} from '../../../middleware/RequestCacheMiddleware';
import type {Channel} from '../../../models/Channel';
import type {Message} from '../../../models/Message';
import type {IChannelRepositoryAggregate} from '../../repositories/IChannelRepositoryAggregate';
import type {AuthenticatedChannel} from '../AuthenticatedChannel';
import {dispatchMessageCreateBroadcast, dispatchMessageUpdateBroadcast} from '../message/MessageGatewayDispatch';
import type {MessagePersistenceService} from '../message/MessagePersistenceService';
import {createMessageResponseDataService} from '../message/MessageResponseDataService';
import {MessageInteractionBase} from './MessageInteractionBase';

const PIN_LIST_UNBOUNDED_TIMESTAMP = new Date('9999-12-31T23:59:59.999Z');

export class MessagePinService extends MessageInteractionBase {
	constructor(
		gatewayService: IGatewayService,
		private channelRepository: IChannelRepositoryAggregate,
		private snowflakeService: ISnowflakeService,
		private messagePersistenceService: MessagePersistenceService,
		private readonly guildAuditLogService: GuildAuditLogService,
	) {
		super(gatewayService);
	}

	private async assertMessageHistoryAccess({
		authChannel,
		messageId,
	}: {
		authChannel: AuthenticatedChannel;
		messageId: MessageID;
	}): Promise<void> {
		if (!authChannel.guild) {
			return;
		}
		if (await authChannel.hasPermission(Permissions.READ_MESSAGE_HISTORY)) {
			return;
		}
		const cutoff = authChannel.guild.message_history_cutoff;
		if (!cutoff || snowflakeToDate(messageId).getTime() < new Date(cutoff).getTime()) {
			throw new UnknownMessageError();
		}
	}

	async getChannelPins({
		authChannel,
		userId,
		beforeTimestamp,
		limit,
	}: {
		authChannel: AuthenticatedChannel;
		userId: UserID;
		requestCache: RequestCache;
		beforeTimestamp?: Date;
		limit?: number;
	}): Promise<{
		items: Array<ChannelPinResponse>;
		has_more: boolean;
	}> {
		const {channel} = authChannel;
		this.ensureTextChannel(channel);
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
	}: {
		authChannel: AuthenticatedChannel;
		messageId: MessageID;
		userId: UserID;
		requestCache: RequestCache;
	}): Promise<void> {
		const {channel, guild, checkPermission} = authChannel;
		if (guild) {
			await checkPermission(Permissions.PIN_MESSAGES);
			if (this.isOperationDisabled(guild, GuildOperations.SEND_MESSAGE)) {
				throw new FeatureTemporarilyDisabledError();
			}
		}
		this.ensureTextChannel(channel);
		await this.assertMessageHistoryAccess({authChannel, messageId});
		const message = await this.channelRepository.messages.getMessage(channel.id, messageId);
		if (!message) throw new UnknownMessageError();
		this.validateMessagePinnable(message);
		if (message.pinnedTimestamp) return;
		const now = new Date();
		const updatedMessageData = {...message.toRow(), pinned_timestamp: now};
		const updatedMessage = await this.channelRepository.messages.upsertMessage(updatedMessageData, message.toRow());
		await this.channelRepository.messageInteractions.addChannelPin(channel.id, messageId, now);
		const updatedChannelData = {...channel.toRow(), last_pin_timestamp: now};
		const updatedChannel = await this.channelRepository.channelData.upsert(updatedChannelData);
		await this.dispatchChannelPinsUpdate(updatedChannel);
		await this.sendPinSystemMessage({channel, message, userId});
		await dispatchMessageUpdateBroadcast({
			gatewayService: this.gatewayService,
			channel,
			message: updatedMessage,
		});
		if (channel.guildId) {
			await this.guildAuditLogService
				.createBuilder(channel.guildId, userId)
				.withAction(AuditLogActionType.MESSAGE_PIN, messageId.toString())
				.withMetadata({
					channel_id: channel.id.toString(),
					message_id: messageId.toString(),
				})
				.withReason(null)
				.commit();
		}
	}

	async unpinMessage({
		authChannel,
		messageId,
		userId,
	}: {
		authChannel: AuthenticatedChannel;
		messageId: MessageID;
		userId: UserID;
		requestCache: RequestCache;
	}): Promise<void> {
		const {channel, guild, checkPermission} = authChannel;
		if (guild) {
			await checkPermission(Permissions.PIN_MESSAGES);
			if (this.isOperationDisabled(guild, GuildOperations.SEND_MESSAGE)) {
				throw new FeatureTemporarilyDisabledError();
			}
		}
		this.ensureTextChannel(channel);
		await this.assertMessageHistoryAccess({authChannel, messageId});
		const message = await this.channelRepository.messages.getMessage(channel.id, messageId);
		if (!message) throw new UnknownMessageError();
		this.validateMessagePinnable(message);
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
		if (channel.guildId) {
			await this.guildAuditLogService
				.createBuilder(channel.guildId, userId)
				.withAction(AuditLogActionType.MESSAGE_UNPIN, messageId.toString())
				.withMetadata({
					channel_id: channel.id.toString(),
					message_id: messageId.toString(),
				})
				.withReason(null)
				.commit();
		}
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
