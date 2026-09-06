// SPDX-License-Identifier: AGPL-3.0-or-later

import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import type {GlobalSearchMessagesRequest} from '@fluxer/schema/src/domains/message/MessageRequestSchemas';
import type {MessageResponse, MessageSearchResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import type {IWorkerService} from '@pkgs/worker/src/contracts/IWorkerService';
import {createChannelID, createGuildID, type UserID} from '../BrandedTypes';
import type {IChannelRepository} from '../channel/IChannelRepository';
import type {ChannelService} from '../channel/services/ChannelService';
import type {GuildService} from '../guild/services/GuildService';
import type {UserCacheService} from '../infrastructure/UserCacheService';
import type {RequestCache} from '../middleware/RequestCacheMiddleware';
import type {IUserRepository} from '../user/IUserRepository';
import type {WorkerTaskName} from '../worker/WorkerLaneConfig';
import {GlobalSearchService} from './GlobalSearchService';

function omitReferencedMessages(result: MessageSearchResponse): MessageSearchResponse {
	if (!('messages' in result)) {
		return result;
	}
	return {
		...result,
		messages: result.messages.map((message) => {
			const {referenced_message: _referencedMessage, ...rest} = message as MessageResponse;
			return rest;
		}),
	};
}

export class SearchService {
	private readonly globalSearch: GlobalSearchService;
	private readonly channelService: ChannelService;
	private readonly guildService: GuildService;

	constructor(params: {
		channelRepository: IChannelRepository;
		channelService: ChannelService;
		guildService: GuildService;
		userRepository: IUserRepository;
		userCacheService: UserCacheService;
		workerService: IWorkerService<WorkerTaskName>;
	}) {
		this.channelService = params.channelService;
		this.guildService = params.guildService;
		this.globalSearch = new GlobalSearchService(
			params.channelRepository,
			params.guildService,
			params.userRepository,
			params.userCacheService,
			params.workerService,
		);
	}

	async searchMessages(params: {
		userId: UserID;
		requestCache: RequestCache;
		data: GlobalSearchMessagesRequest;
	}): Promise<MessageSearchResponse> {
		const {userId, requestCache, data} = params;
		const {channel_id, channel_ids, context_channel_id, context_guild_id, ...searchParams} = data;
		const contextChannelId = context_channel_id ? createChannelID(context_channel_id) : null;
		const contextGuildId = context_guild_id ? createGuildID(context_guild_id) : null;
		const channelIds = (channel_ids ?? channel_id)?.map((id) => createChannelID(id)) ?? [];
		const scope = searchParams.scope ?? 'current';
		let result: MessageSearchResponse;
		switch (scope) {
			case 'all_guilds':
				result = await this.guildService.search.searchAllGuilds({
					userId,
					channelIds,
					searchParams,
					requestCache,
				});
				break;
			case 'all_dms':
			case 'open_dms':
				result = await this.globalSearch.searchAcrossDms({
					userId,
					scope,
					searchParams,
					requestCache,
					includeChannelId: contextChannelId,
					requestedChannelIds: channelIds,
				});
				break;
			case 'all':
				result = await this.globalSearch.searchAcrossGuildsAndDms({
					userId,
					dmScope: 'all_dms',
					searchParams,
					requestCache,
					includeChannelId: contextChannelId,
					requestedChannelIds: channelIds,
				});
				break;
			case 'open_dms_and_all_guilds':
				result = await this.globalSearch.searchAcrossGuildsAndDms({
					userId,
					dmScope: 'open_dms',
					searchParams,
					requestCache,
					includeChannelId: contextChannelId,
					requestedChannelIds: channelIds,
				});
				break;
			default:
				if (contextGuildId) {
					result = await this.guildService.search.searchMessages({
						userId,
						guildId: contextGuildId,
						channelIds,
						searchParams,
						requestCache,
					});
				} else if (contextChannelId) {
					result = await this.channelService.messages.retrieval.searchMessages({
						userId,
						channelId: contextChannelId,
						searchParams,
						requestCache,
					});
				} else {
					throw InputValidationError.fromCode('context', ValidationErrorCodes.CONTEXT_CHANNEL_OR_GUILD_ID_REQUIRED);
				}
				break;
		}
		return omitReferencedMessages(result);
	}
}
