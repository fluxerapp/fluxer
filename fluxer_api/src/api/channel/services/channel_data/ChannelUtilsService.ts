// SPDX-License-Identifier: AGPL-3.0-or-later

import type {MessageID, UserID} from '@app/api/BrandedTypes';
import {mapChannelToResponse} from '@app/api/channel/ChannelMappers';
import type {IChannelRepositoryAggregate} from '@app/api/channel/repositories/IChannelRepositoryAggregate';
import {dispatchChannelEvent} from '@app/api/channel/services/ChannelGatewayDispatch';
import {dispatchMessageCreateBroadcast} from '@app/api/channel/services/message/MessageGatewayDispatch';
import {purgeMessageAttachments} from '@app/api/channel/services/message/MessageHelpers';
import type {IPurgeQueue} from '@app/api/infrastructure/CachePurgeQueue';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {IStorageService} from '@app/api/infrastructure/IStorageService';
import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {Channel} from '@app/api/models/Channel';
import type {Message} from '@app/api/models/Message';

export class ChannelUtilsService {
	constructor(
		private channelRepository: IChannelRepositoryAggregate,
		private userCacheService: UserCacheService,
		private storageService: IStorageService,
		private gatewayService: IGatewayService,
		private purgeQueue: IPurgeQueue,
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
		await purgeMessageAttachments(message, this.storageService, this.purgeQueue);
	}

	async dispatchChannelUpdate({channel, requestCache}: {channel: Channel; requestCache: RequestCache}): Promise<void> {
		if (channel.guildId) {
			const channelResponse = await mapChannelToResponse({
				channel,
				currentUserId: null,
				userCacheService: this.userCacheService,
				requestCache,
			});
			await dispatchChannelEvent({
				gatewayService: this.gatewayService,
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
		await dispatchChannelEvent({
			gatewayService: this.gatewayService,
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
	}: {
		channel: Channel;
		message: Message;
		requestCache: RequestCache;
	}): Promise<void> {
		await dispatchMessageCreateBroadcast({
			gatewayService: this.gatewayService,
			channel,
			message,
		});
	}
}
