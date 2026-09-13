// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import {createChannelID} from '@app/api/BrandedTypes';
import {mapChannelToResponse} from '@app/api/channel/ChannelMappers';
import type {IChannelRepository} from '@app/api/channel/IChannelRepository';
import {createMessageResponseDataService} from '@app/api/channel/services/message/MessageResponseDataService';
import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {Channel} from '@app/api/models/Channel';
import type {Message} from '@app/api/models/Message';
import {mapWithConcurrency} from '@app/api/utils/ConcurrencyUtils';
import type {MessageSearchResultsResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';

const CHANNEL_LOOKUP_CONCURRENCY = 16;

export class MessageSearchResponseMapper {
	constructor(
		private readonly channelRepository: IChannelRepository,
		private readonly userCacheService: UserCacheService,
	) {}

	async mapSearchResultToResponses(
		messages: Array<Message>,
		userId: UserID,
		requestCache: RequestCache,
	): Promise<{
		messages: Array<MessageSearchResultsResponse['messages'][number]>;
		channels: Array<MessageSearchResultsResponse['channels'][number]>;
	}> {
		const orderedChannelIds = Array.from(new Set(messages.map((message) => message.channelId.toString())));
		const channels = await mapWithConcurrency(orderedChannelIds, CHANNEL_LOOKUP_CONCURRENCY, (channelId) =>
			this.channelRepository.findUnique(createChannelID(BigInt(channelId))),
		);
		const channelById = new Map(
			channels
				.filter((channel): channel is Channel => channel !== null)
				.map((channel) => [channel.id.toString(), channel] as const),
		);
		const renderableMessages = messages.filter((message) => channelById.has(message.channelId.toString()));
		const messageResponses = await createMessageResponseDataService().buildMessagesForChannels({
			userId,
			messages: renderableMessages,
			channelById,
		});
		const searchMessages = messageResponses.map(
			({referenced_message: _referencedMessage, ...searchMessage}) => searchMessage,
		);
		const respondedChannelIds = new Set(searchMessages.map((message) => message.channel_id));
		const orderedChannels = orderedChannelIds
			.filter((channelId) => respondedChannelIds.has(channelId))
			.map((channelId) => channelById.get(channelId))
			.filter((channel): channel is Channel => channel !== undefined);
		const channelResponses = await mapWithConcurrency(orderedChannels, CHANNEL_LOOKUP_CONCURRENCY, (channel) =>
			mapChannelToResponse({
				channel,
				currentUserId: userId,
				userCacheService: this.userCacheService,
				requestCache,
			}),
		);
		return {
			messages: searchMessages,
			channels: channelResponses,
		};
	}
}
