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

import {type ChannelID, createChannelID, type MessageID} from '~/BrandedTypes';
import type {IChannelRepository} from '~/channel/IChannelRepository';
import type {IGatewayService} from '~/infrastructure/IGatewayService';

export const chunkArray = <T>(items: Array<T>, chunkSize: number): Array<Array<T>> => {
	const chunks: Array<Array<T>> = [];
	for (let i = 0; i < items.length; i += chunkSize) {
		chunks.push(items.slice(i, i + chunkSize));
	}
	return chunks;
};

interface BulkDeleteDispatcherDeps {
	channelRepository: IChannelRepository;
	gatewayService: IGatewayService;
	batchSize: number;
}

export const createBulkDeleteDispatcher = ({
	channelRepository,
	gatewayService,
	batchSize,
}: BulkDeleteDispatcherDeps) => {
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
};
