// SPDX-License-Identifier: AGPL-3.0-or-later

import {type ChannelID, createChannelID, type MessageID} from '../../../BrandedTypes';
import type {IChannelRepository} from '../../../channel/IChannelRepository';
import type {IGatewayService} from '../../../infrastructure/IGatewayService';

interface BulkDeleteDispatcherDeps {
	channelRepository: IChannelRepository;
	gatewayService: IGatewayService;
	batchSize: number;
}

export function createBulkDeleteDispatcher({channelRepository, gatewayService, batchSize}: BulkDeleteDispatcherDeps) {
	const messagesByChannel = new Map<string, Array<MessageID>>();
	const track = (channelId: ChannelID, messageId: MessageID) => {
		const channelIdStr = channelId.toString();
		if (!messagesByChannel.has(channelIdStr)) {
			messagesByChannel.set(channelIdStr, []);
		}
		messagesByChannel.get(channelIdStr)!.push(messageId);
	};
	const flush = async (force: boolean) => {
		for (const [channelIdStr, messageIdsBatch] of messagesByChannel.entries()) {
			if (!force && messageIdsBatch.length < batchSize) {
				continue;
			}
			if (messageIdsBatch.length === 0) {
				continue;
			}
			const channelId = createChannelID(BigInt(channelIdStr));
			const channel = await channelRepository.findUnique(channelId);
			if (channel) {
				const payloadIds = messageIdsBatch.map((id) => id.toString());
				if (channel.guildId) {
					await gatewayService.dispatchGuild({
						guildId: channel.guildId,
						event: 'MESSAGE_DELETE_BULK',
						data: {
							channel_id: channelIdStr,
							ids: payloadIds,
						},
					});
				} else {
					for (const recipientId of channel.recipientIds) {
						await gatewayService.dispatchPresence({
							userId: recipientId,
							event: 'MESSAGE_DELETE_BULK',
							data: {
								channel_id: channelIdStr,
								ids: payloadIds,
							},
						});
					}
				}
			}
			messagesByChannel.set(channelIdStr, []);
		}
	};
	return {track, flush};
}
