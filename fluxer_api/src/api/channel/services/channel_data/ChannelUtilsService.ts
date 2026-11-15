// SPDX-License-Identifier: AGPL-3.0-or-later

import type {MessageID} from '@app/api/BrandedTypes';
import {mapChannelToResponse} from '@app/api/channel/ChannelMappers';
import type {ChannelRepository} from '@app/api/channel/ChannelRepository';
import {dispatchChannelEvent} from '@app/api/channel/services/ChannelGatewayDispatch';
import {purgeMessageAttachments} from '@app/api/channel/services/message/MessageHelpers';
import {withThreadParentFields} from '@app/api/channel/services/thread/ThreadParentSettings';
import type {IPurgeQueue} from '@app/api/infrastructure/CachePurgeQueue';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {IStorageService} from '@app/api/infrastructure/IStorageService';
import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {Channel} from '@app/api/models/Channel';
import type {Message} from '@app/api/models/Message';

export class ChannelUtilsService {
	constructor(
		private channelRepository: ChannelRepository,
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
			const channelResponse = await withThreadParentFields(
				this.channelRepository.threads,
				channel,
				await mapChannelToResponse({
					channel,
					currentUserId: null,
					userCacheService: this.userCacheService,
					requestCache,
				}),
			);
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
}
