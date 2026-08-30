// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	MessageResponse,
	MessageSearchResultsResponse,
} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import type {UserID} from '../BrandedTypes';
import {createChannelID, createMessageID} from '../BrandedTypes';
import {mapChannelToResponse} from '../channel/ChannelMappers';
import type {IChannelRepository} from '../channel/IChannelRepository';
import {
	createMessageResponseDataService,
	messageResponseAccessForChannel,
} from '../channel/services/message/MessageResponseDataService';
import type {UserCacheService} from '../infrastructure/UserCacheService';
import type {RequestCache} from '../middleware/RequestCacheMiddleware';
import type {Channel} from '../models/Channel';

export class MessageSearchResponseMapper {
	constructor(
		private readonly channelRepository: IChannelRepository,
		private readonly userCacheService: UserCacheService,
	) {}

	async mapSearchResultToResponses(
		result: {
			hits: Array<{
				channelId: string;
				id: string;
			}>;
			total: number;
		},
		userId: UserID,
		requestCache: RequestCache,
	): Promise<{
		messages: Array<MessageSearchResultsResponse['messages'][number]>;
		channels: Array<MessageSearchResultsResponse['channels'][number]>;
	}> {
		const messageEntries = result.hits.map((hit) => ({
			channelId: createChannelID(BigInt(hit.channelId)),
			messageId: createMessageID(BigInt(hit.id)),
		}));
		const orderedChannelIds = new Set<string>();
		for (const entry of messageEntries) {
			orderedChannelIds.add(entry.channelId.toString());
		}
		const channels = await Promise.all(
			Array.from(orderedChannelIds).map((channelId) =>
				this.channelRepository.findUnique(createChannelID(BigInt(channelId))),
			),
		);
		const validChannels = channels.filter((channel): channel is Channel => channel !== null);
		const channelById = new Map(validChannels.map((channel) => [channel.id.toString(), channel] as const));
		const responseDataService = createMessageResponseDataService();
		const messageResponsesWithEntries = await Promise.all(
			messageEntries.map(async (entry) => {
				const channel = channelById.get(entry.channelId.toString());
				if (!channel) return null;
				const message = await responseDataService.getMessage({
					userId,
					channelId: entry.channelId,
					messageId: entry.messageId,
					access: messageResponseAccessForChannel(channel),
				});
				return message ? {message, channelId: entry.channelId.toString()} : null;
			}),
		);
		const validMessageResponseEntries = messageResponsesWithEntries.filter(
			(entry): entry is {message: MessageResponse; channelId: string} => entry !== null,
		);
		const messageResponses = validMessageResponseEntries.map((entry) => {
			const {referenced_message: _referencedMessage, ...searchMessage} = entry.message;
			return searchMessage;
		});
		const orderedResponseChannelIds = new Set(validMessageResponseEntries.map((entry) => entry.channelId));
		const orderedChannels = Array.from(orderedResponseChannelIds)
			.map((channelId) => channelById.get(channelId))
			.filter((channel): channel is Channel => channel !== undefined);
		const channelResponses = await Promise.all(
			orderedChannels.map((channel) =>
				mapChannelToResponse({
					channel,
					currentUserId: userId,
					userCacheService: this.userCacheService,
					requestCache,
				}),
			),
		);
		return {
			messages: messageResponses,
			channels: channelResponses,
		};
	}
}
