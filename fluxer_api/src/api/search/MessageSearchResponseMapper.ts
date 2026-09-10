// SPDX-License-Identifier: AGPL-3.0-or-later

import type {MessageSearchResultsResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import type {UserID} from '../BrandedTypes';
import {createChannelID} from '../BrandedTypes';
import {mapChannelToResponse} from '../channel/ChannelMappers';
import type {IChannelRepository} from '../channel/IChannelRepository';
import {createMessageResponseDataService} from '../channel/services/message/MessageResponseDataService';
import type {UserCacheService} from '../infrastructure/UserCacheService';
import type {RequestCache} from '../middleware/RequestCacheMiddleware';
import type {Channel} from '../models/Channel';
import type {Message} from '../models/Message';
import {mapWithConcurrency} from '../utils/ConcurrencyUtils';

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
