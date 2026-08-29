// SPDX-License-Identifier: AGPL-3.0-or-later

import {dispatchChannelEvent} from '@app/api/channel/services/ChannelGatewayDispatch';
import type {MessageID, UserID} from '../../../BrandedTypes';
import type {IPurgeQueue} from '../../../infrastructure/BunnyPurgeQueue';
import type {IGatewayService} from '../../../infrastructure/IGatewayService';
import type {IStorageService} from '../../../infrastructure/IStorageService';
import type {UserCacheService} from '../../../infrastructure/UserCacheService';
import type {RequestCache} from '../../../middleware/RequestCacheMiddleware';
import type {Channel} from '../../../models/Channel';
import type {Message} from '../../../models/Message';
import {mapChannelToResponse} from '../../ChannelMappers';
import type {IChannelRepositoryAggregate} from '../../repositories/IChannelRepositoryAggregate';
import {dispatchMessageCreateBroadcast} from '../message/MessageGatewayDispatch';
import {purgeMessageAttachments} from '../message/MessageHelpers';

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
