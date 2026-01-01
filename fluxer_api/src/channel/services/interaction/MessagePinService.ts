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

import {createMessageID, type MessageID, type UserID} from '~/BrandedTypes';
import {GuildOperations, MessageTypes, Permissions} from '~/Constants';
import type {ChannelPinResponse} from '~/channel/ChannelModel';
import {mapMessageToResponse} from '~/channel/ChannelModel';
import {AuditLogActionType} from '~/constants/AuditLogActionType';
import {CannotEditSystemMessageError, FeatureTemporarilyDisabledError, UnknownMessageError} from '~/Errors';
import type {GuildAuditLogService} from '~/guild/GuildAuditLogService';
import type {IGatewayService} from '~/infrastructure/IGatewayService';
import type {IMediaService} from '~/infrastructure/IMediaService';
import type {SnowflakeService} from '~/infrastructure/SnowflakeService';
import type {UserCacheService} from '~/infrastructure/UserCacheService';
import type {Channel, Message} from '~/Models';
import type {RequestCache} from '~/middleware/RequestCacheMiddleware';
import type {IChannelRepositoryAggregate} from '../../repositories/IChannelRepositoryAggregate';
import type {AuthenticatedChannel} from '../AuthenticatedChannel';
import type {MessagePersistenceService} from '../message/MessagePersistenceService';
import {MessageInteractionBase} from './MessageInteractionBase';

export class MessagePinService extends MessageInteractionBase {
	constructor(
		gatewayService: IGatewayService,
		private channelRepository: IChannelRepositoryAggregate,
		private userCacheService: UserCacheService,
		private mediaService: IMediaService,
		private snowflakeService: SnowflakeService,
		private messagePersistenceService: MessagePersistenceService,
		private readonly guildAuditLogService: GuildAuditLogService,
	) {
		super(gatewayService);
	}

	async getChannelPins({
		authChannel,
		requestCache,
		beforeTimestamp,
		limit,
	}: {
		authChannel: AuthenticatedChannel;
		requestCache: RequestCache;
		beforeTimestamp?: Date;
		limit?: number;
	}): Promise<{items: Array<ChannelPinResponse>; has_more: boolean}> {
		const {channel} = authChannel;
		this.ensureTextChannel(channel);

		if (authChannel.guild && !(await authChannel.hasPermission(Permissions.READ_MESSAGE_HISTORY))) {
			return {items: [], has_more: false};
		}

		const pageSize = Math.min(limit ?? 50, 50);
		const effectiveBefore = beforeTimestamp ?? new Date();
		const messages = await this.channelRepository.messageInteractions.listChannelPins(
			channel.id,
			effectiveBefore,
			pageSize + 1,
		);
		const sorted = messages.sort((a, b) => (b.pinnedTimestamp?.getTime() ?? 0) - (a.pinnedTimestamp?.getTime() ?? 0));
		const hasMore = sorted.length > pageSize;
		const trimmed = hasMore ? sorted.slice(0, pageSize) : sorted;

		const items = await Promise.all(
			trimmed.map(async (message: Message) => ({
				message: await mapMessageToResponse({
					message,
					userCacheService: this.userCacheService,
					requestCache,
					mediaService: this.mediaService,
				}),
				pinned_at: message.pinnedTimestamp!.toISOString(),
			})),
		);

		return {
			items,
			has_more: hasMore,
		};
	}

	async pinMessage({
		authChannel,
		messageId,
		userId,
		requestCache,
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

		const message = await this.channelRepository.messages.getMessage(channel.id, messageId);
		if (!message) throw new UnknownMessageError();

		this.validateMessagePinnable(message);

		if (message.pinnedTimestamp) return;

		const now = new Date();
		const updatedMessageData = {...message.toRow(), pinned_timestamp: now};
		await this.channelRepository.messages.upsertMessage(updatedMessageData, message.toRow());

		await this.channelRepository.messageInteractions.addChannelPin(channel.id, messageId, now);

		const updatedChannelData = {...channel.toRow(), last_pin_timestamp: now};
		const updatedChannel = await this.channelRepository.channelData.upsert(updatedChannelData);

		await this.dispatchChannelPinsUpdate(updatedChannel);
		await this.sendPinSystemMessage({channel, message, userId, requestCache});

		const updatedMessage = await this.channelRepository.messages.getMessage(channel.id, messageId);
		if (!updatedMessage) throw new UnknownMessageError();

		await this.dispatchMessageUpdate({channel, message: updatedMessage, requestCache});

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
		requestCache,
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

		const message = await this.channelRepository.messages.getMessage(channel.id, messageId);
		if (!message) throw new UnknownMessageError();

		this.validateMessagePinnable(message);

		if (!message.pinnedTimestamp) return;

		const updatedMessageData = {...message.toRow(), pinned_timestamp: null};
		await this.channelRepository.messages.upsertMessage(updatedMessageData, message.toRow());

		await this.channelRepository.messageInteractions.removeChannelPin(channel.id, messageId);

		await this.dispatchChannelPinsUpdate(channel);

		const updatedMessage = await this.channelRepository.messages.getMessage(channel.id, messageId);
		if (!updatedMessage) throw new UnknownMessageError();

		await this.dispatchMessageUpdate({channel, message: updatedMessage, requestCache});

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
		requestCache,
	}: {
		channel: Channel;
		message: Message;
		userId: UserID;
		requestCache: RequestCache;
	}): Promise<void> {
		const messageId = createMessageID(this.snowflakeService.generate());
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

		await this.dispatchMessageCreate({channel, message: systemMessage, requestCache});
	}

	private async dispatchChannelPinsUpdate(channel: Channel): Promise<void> {
		await this.dispatchEvent({
			channel,
			event: 'CHANNEL_PINS_UPDATE',
			data: {
				channel_id: channel.id.toString(),
				last_pin_timestamp: channel.lastPinTimestamp?.toISOString() ?? null,
			},
		});
	}

	private async dispatchMessageCreate({
		channel,
		message,
		requestCache,
		currentUserId,
		nonce,
	}: {
		channel: Channel;
		message: Message;
		requestCache: RequestCache;
		currentUserId?: UserID;
		nonce?: string;
	}): Promise<void> {
		const messageResponse = await mapMessageToResponse({
			message,
			currentUserId,
			nonce,
			userCacheService: this.userCacheService,
			requestCache,
			mediaService: this.mediaService,
		});

		await this.dispatchEvent({
			channel,
			event: 'MESSAGE_CREATE',
			data: {...messageResponse, channel_type: channel.type},
		});
	}

	private async dispatchMessageUpdate({
		channel,
		message,
		requestCache,
		currentUserId,
	}: {
		channel: Channel;
		message: Message;
		requestCache: RequestCache;
		currentUserId?: UserID;
	}): Promise<void> {
		const messageResponse = await mapMessageToResponse({
			message,
			currentUserId,
			userCacheService: this.userCacheService,
			requestCache,
			mediaService: this.mediaService,
		});

		await this.dispatchEvent({
			channel,
			event: 'MESSAGE_UPDATE',
			data: messageResponse,
		});
	}
}
