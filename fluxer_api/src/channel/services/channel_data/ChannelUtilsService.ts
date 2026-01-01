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

import {channelIdToUserId, type MessageID, type UserID} from '~/BrandedTypes';
import {Config} from '~/Config';
import {ChannelTypes, type GatewayDispatchEvent} from '~/Constants';
import {mapChannelToResponse, mapMessageToResponse} from '~/channel/ChannelModel';
import {makeAttachmentCdnKey, makeAttachmentCdnUrl} from '~/channel/services/message/MessageHelpers';
import type {ICloudflarePurgeQueue} from '~/infrastructure/CloudflarePurgeQueue';
import type {IGatewayService} from '~/infrastructure/IGatewayService';
import type {IMediaService} from '~/infrastructure/IMediaService';
import type {IStorageService} from '~/infrastructure/IStorageService';
import type {UserCacheService} from '~/infrastructure/UserCacheService';
import type {Channel, Message} from '~/Models';
import type {RequestCache} from '~/middleware/RequestCacheMiddleware';
import type {IChannelRepositoryAggregate} from '../../repositories/IChannelRepositoryAggregate';

export class ChannelUtilsService {
	constructor(
		private channelRepository: IChannelRepositoryAggregate,
		private userCacheService: UserCacheService,
		private storageService: IStorageService,
		private gatewayService: IGatewayService,
		private cloudflarePurgeQueue: ICloudflarePurgeQueue,
		private mediaService: IMediaService,
	) {}

	async purgeChannelAttachments(channel: Channel): Promise<void> {
		const batchSize = 100;
		let hasMore = true;
		let beforeMessageId: MessageID | undefined;

		while (hasMore) {
			const messages = await this.channelRepository.messages.listMessages(channel.id, beforeMessageId, batchSize);

			if (messages.length === 0) {
				hasMore = false;
				break;
			}

			await Promise.all(messages.map((message: Message) => this.purgeMessageAttachments(message)));

			if (messages.length < batchSize) {
				hasMore = false;
			} else {
				beforeMessageId = messages[messages.length - 1].id;
			}
		}
	}

	private async purgeMessageAttachments(message: Message): Promise<void> {
		const cdnUrls: Array<string> = [];

		await Promise.all(
			message.attachments.map(async (attachment) => {
				const cdnKey = makeAttachmentCdnKey(message.channelId, attachment.id, attachment.filename);
				await this.storageService.deleteObject(Config.s3.buckets.cdn, cdnKey);

				if (Config.cloudflare.purgeEnabled) {
					const cdnUrl = makeAttachmentCdnUrl(message.channelId, attachment.id, attachment.filename);
					cdnUrls.push(cdnUrl);
				}
			}),
		);

		if (Config.cloudflare.purgeEnabled && cdnUrls.length > 0) {
			await this.cloudflarePurgeQueue.addUrls(cdnUrls);
		}
	}

	private async dispatchEvent(params: {channel: Channel; event: GatewayDispatchEvent; data: unknown}): Promise<void> {
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
		} else {
			for (const recipientId of channel.recipientIds) {
				await this.gatewayService.dispatchPresence({userId: recipientId, event, data});
			}
		}
	}

	async dispatchChannelUpdate({channel, requestCache}: {channel: Channel; requestCache: RequestCache}): Promise<void> {
		if (channel.guildId) {
			const channelResponse = await mapChannelToResponse({
				channel,
				currentUserId: null,
				userCacheService: this.userCacheService,
				requestCache,
			});

			await this.dispatchEvent({
				channel,
				event: 'CHANNEL_UPDATE',
				data: channelResponse,
			});
			return;
		}

		for (const userId of channel.recipientIds) {
			const channelResponse = await mapChannelToResponse({
				channel,
				currentUserId: userId,
				userCacheService: this.userCacheService,
				requestCache,
			});

			await this.gatewayService.dispatchPresence({
				userId,
				event: 'CHANNEL_UPDATE',
				data: channelResponse,
			});
		}
	}

	async dispatchChannelDelete({channel, requestCache}: {channel: Channel; requestCache: RequestCache}): Promise<void> {
		const channelResponse = await mapChannelToResponse({
			channel,
			currentUserId: null,
			userCacheService: this.userCacheService,
			requestCache,
		});

		await this.dispatchEvent({
			channel,
			event: 'CHANNEL_DELETE',
			data: channelResponse,
		});
	}

	async dispatchDmChannelDelete({
		channel,
		userId,
		requestCache,
	}: {
		channel: Channel;
		userId: UserID;
		requestCache: RequestCache;
	}): Promise<void> {
		await this.gatewayService.dispatchPresence({
			userId,
			event: 'CHANNEL_DELETE',
			data: await mapChannelToResponse({
				channel,
				currentUserId: null,
				userCacheService: this.userCacheService,
				requestCache,
			}),
		});
	}

	async dispatchMessageCreate({
		channel,
		message,
		requestCache,
	}: {
		channel: Channel;
		message: Message;
		requestCache: RequestCache;
	}): Promise<void> {
		const messageResponse = await mapMessageToResponse({
			message,
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
}
