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

import {AttachmentDecayService} from '~/attachment/AttachmentDecayService';
import {type ChannelID, channelIdToUserId, type MessageID, type UserID} from '~/BrandedTypes';
import {ChannelTypes, type GatewayDispatchEvent} from '~/Constants';
import {mapMessageToResponse} from '~/channel/ChannelModel';
import type {IChannelRepositoryAggregate} from '~/channel/repositories/IChannelRepositoryAggregate';
import type {IGatewayService} from '~/infrastructure/IGatewayService';
import type {IMediaService} from '~/infrastructure/IMediaService';
import type {UserCacheService} from '~/infrastructure/UserCacheService';
import type {Channel, Message} from '~/Models';
import type {RequestCache} from '~/middleware/RequestCacheMiddleware';
import {collectMessageAttachments} from './MessageHelpers';

export class MessageDispatchService {
	constructor(
		private gatewayService: IGatewayService,
		private userCacheService: UserCacheService,
		private mediaService: IMediaService,
		private channelRepository: IChannelRepositoryAggregate,
		private attachmentDecayService: AttachmentDecayService = new AttachmentDecayService(),
	) {}

	async dispatchEvent(params: {channel: Channel; event: GatewayDispatchEvent; data: unknown}): Promise<void> {
		const {channel, event, data} = params;

		if (channel.type === ChannelTypes.DM_PERSONAL_NOTES) {
			return this.gatewayService.dispatchPresence({
				userId: channelIdToUserId(channel.id),
				event,
				data,
			});
		}

		if (channel.guildId) {
			return this.gatewayService.dispatchGuild({guildId: channel.guildId, event, data});
		}

		await Promise.all(
			Array.from(channel.recipientIds).map((recipientId) =>
				this.gatewayService.dispatchPresence({userId: recipientId, event, data}),
			),
		);
	}

	async dispatchMessageCreate({
		channel,
		message,
		requestCache,
		currentUserId,
		nonce,
		tts,
	}: {
		channel: Channel;
		message: Message;
		requestCache: RequestCache;
		currentUserId?: UserID;
		nonce?: string;
		tts?: boolean;
	}): Promise<void> {
		const messageAttachments = collectMessageAttachments(message);
		const attachmentDecayMap =
			messageAttachments.length > 0
				? await this.attachmentDecayService.fetchMetadata(messageAttachments.map((att) => ({attachmentId: att.id})))
				: undefined;

		const messageResponse = await mapMessageToResponse({
			message,
			currentUserId,
			nonce,
			tts,
			userCacheService: this.userCacheService,
			requestCache,
			mediaService: this.mediaService,
			attachmentDecayMap,
			getReferencedMessage: (channelId: ChannelID, messageId: MessageID) =>
				this.channelRepository.messages.getMessage(channelId, messageId),
		});

		await this.dispatchEvent({
			channel,
			event: 'MESSAGE_CREATE',
			data: {...messageResponse, channel_type: channel.type},
		});
	}

	async dispatchMessageUpdate({
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
		const messageAttachments = collectMessageAttachments(message);
		const attachmentDecayMap =
			messageAttachments.length > 0
				? await this.attachmentDecayService.fetchMetadata(messageAttachments.map((att) => ({attachmentId: att.id})))
				: undefined;

		const messageResponse = await mapMessageToResponse({
			message,
			currentUserId,
			userCacheService: this.userCacheService,
			requestCache,
			mediaService: this.mediaService,
			attachmentDecayMap,
			getReferencedMessage: (channelId: ChannelID, messageId: MessageID) =>
				this.channelRepository.messages.getMessage(channelId, messageId),
		});

		await this.dispatchEvent({
			channel,
			event: 'MESSAGE_UPDATE',
			data: messageResponse,
		});
	}

	async dispatchMessageDelete({
		channel,
		messageId,
		message,
	}: {
		channel: Channel;
		messageId: MessageID;
		message: Message;
	}): Promise<void> {
		await this.dispatchEvent({
			channel,
			event: 'MESSAGE_DELETE',
			data: {
				channel_id: channel.id.toString(),
				id: messageId.toString(),
				content: message.content,
				author_id: message.authorId?.toString(),
			},
		});
	}

	async dispatchMessageDeleteBulk({
		channel,
		messageIds,
	}: {
		channel: Channel;
		messageIds: Array<MessageID>;
	}): Promise<void> {
		await this.dispatchEvent({
			channel,
			event: 'MESSAGE_DELETE_BULK',
			data: {
				channel_id: channel.id.toString(),
				ids: messageIds.map((id) => id.toString()),
			},
		});
	}
}
