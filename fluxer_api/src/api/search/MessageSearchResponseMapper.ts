// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildID, UserID} from '@app/api/BrandedTypes';
import {createChannelID} from '@app/api/BrandedTypes';
import {mapChannelToResponse} from '@app/api/channel/ChannelMappers';
import type {IChannelRepository} from '@app/api/channel/IChannelRepository';
import {createMessageResponseDataService} from '@app/api/channel/services/message/MessageResponseDataService';
import {maskThreadArtifactsByChannel} from '@app/api/channel/services/message/ThreadMessageResponses';
import {mapThreadMemberToResponse, mapThreadToResponse} from '@app/api/channel/services/thread/ThreadMappers';
import {loadThreadViews} from '@app/api/channel/services/thread/ThreadViews';
import {
	everEnabled,
	THREAD_CHANNEL_TYPES,
	type ThreadViewer,
	viewerActive,
} from '@app/api/experiment/ChannelThreadsGate';
import {maskChannelResponseThreadBits} from '@app/api/guild/services/ThreadPermissionBits';
import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {Channel} from '@app/api/models/Channel';
import type {Message} from '@app/api/models/Message';
import type {ThreadMember} from '@app/api/models/ThreadMember';
import {mapWithConcurrency} from '@app/api/utils/ConcurrencyUtils';
import type {ThreadChannelResponse} from '@fluxer/schema/src/domains/channel/ThreadRequestSchemas';
import type {ThreadMemberResponse} from '@fluxer/schema/src/domains/channel/ThreadSchemas';
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
		viewer: ThreadViewer,
		requestCache: RequestCache,
	): Promise<{
		messages: Array<MessageSearchResultsResponse['messages'][number]>;
		channels: Array<MessageSearchResultsResponse['channels'][number]>;
		threads?: Array<ThreadChannelResponse>;
		members?: Array<ThreadMemberResponse>;
	}> {
		const orderedChannelIds = Array.from(new Set(messages.map((message) => message.channelId.toString())));
		const channels = await mapWithConcurrency(orderedChannelIds, CHANNEL_LOOKUP_CONCURRENCY, (channelId) =>
			this.channelRepository.findUnique(createChannelID(BigInt(channelId))),
		);
		const channelById = new Map(
			channels
				.filter(
					(channel): channel is Channel =>
						channel !== null &&
						(!THREAD_CHANNEL_TYPES.has(channel.type) ||
							(channel.guildId !== null && viewerActive(viewer, channel.guildId))),
				)
				.map((channel) => [channel.id.toString(), channel] as const),
		);
		const renderableMessages = messages.filter((message) => channelById.has(message.channelId.toString()));
		const messageResponses = await createMessageResponseDataService().buildMessagesForChannels({
			userId,
			messages: renderableMessages,
			channelById,
		});
		const searchMessages = maskThreadArtifactsByChannel(viewer, channelById, messageResponses).map(
			({referenced_message: _referencedMessage, ...searchMessage}) => searchMessage,
		);
		const respondedChannelIds = new Set(searchMessages.map((message) => message.channel_id));
		const respondedChannels = orderedChannelIds
			.filter((channelId) => respondedChannelIds.has(channelId))
			.map((channelId) => channelById.get(channelId))
			.filter((channel): channel is Channel => channel !== undefined);
		const orderedChannels = respondedChannels.filter((channel) => !THREAD_CHANNEL_TYPES.has(channel.type));
		const threadChannels = respondedChannels.filter((channel) => THREAD_CHANNEL_TYPES.has(channel.type));
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
			channels: await this.maskThreadBits(orderedChannels, channelResponses, viewer),
			...(threadChannels.length > 0 ? await this.mapThreads(threadChannels, userId) : {}),
		};
	}

	private async mapThreads(
		threadChannels: Array<Channel>,
		userId: UserID,
	): Promise<{threads: Array<ThreadChannelResponse>; members: Array<ThreadMemberResponse>}> {
		const threadIds = threadChannels.map((channel) => channel.id);
		const states = await this.channelRepository.threads.getStates(threadIds);
		const views = await loadThreadViews(this.channelRepository, states);
		const members = await mapWithConcurrency(threadIds, CHANNEL_LOOKUP_CONCURRENCY, (threadId) =>
			this.channelRepository.threads.getMember(threadId, userId),
		);
		return {
			threads: views.map((view) => mapThreadToResponse(view)),
			members: members
				.filter((member): member is ThreadMember => member !== null)
				.map((member) => mapThreadMemberToResponse(member, {self: true})),
		};
	}

	private async maskThreadBits(
		channels: Array<Channel>,
		responses: Array<MessageSearchResultsResponse['channels'][number]>,
		viewer: ThreadViewer,
	): Promise<Array<MessageSearchResultsResponse['channels'][number]>> {
		if (!everEnabled()) return responses;
		const guildIds = new Map<string, GuildID>();
		for (const channel of channels) {
			if (channel.guildId) guildIds.set(channel.guildId.toString(), channel.guildId);
		}
		const maskedById = new Map<string, MessageSearchResultsResponse['channels'][number]>();
		await Promise.all(
			Array.from(guildIds.entries()).map(async ([guildKey, guildId]) => {
				const guildResponses = responses.filter((response) => response.guild_id === guildKey);
				for (const response of await maskChannelResponseThreadBits(guildId, viewer, guildResponses)) {
					maskedById.set(response.id, response);
				}
			}),
		);
		return responses.map((response) => maskedById.get(response.id) ?? response);
	}
}
