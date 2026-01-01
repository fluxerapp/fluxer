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

import type {HonoApp} from '~/App';
import {createChannelID, createGuildID} from '~/BrandedTypes';
import {MessageSearchRequest, type MessageSearchResponse} from '~/channel/ChannelModel';
import type {IChannelRepository} from '~/channel/IChannelRepository';
import type {ChannelService} from '~/channel/services/ChannelService';
import {ChannelIndexingError, InputValidationError} from '~/Errors';
import type {GuildService} from '~/guild/services/GuildService';
import type {IMediaService} from '~/infrastructure/IMediaService';
import type {UserCacheService} from '~/infrastructure/UserCacheService';
import {DefaultUserOnly, LoginRequired} from '~/middleware/AuthMiddleware';
import {RateLimitMiddleware} from '~/middleware/RateLimitMiddleware';
import {RateLimitConfigs} from '~/RateLimitConfig';
import {Int64Type, z} from '~/Schema';
import {GlobalSearchService} from '~/search/GlobalSearchService';
import type {IUserRepository} from '~/user/IUserRepository';
import {Validator} from '~/Validator';
import type {IWorkerService} from '~/worker/IWorkerService';

const SearchMessagesRequest = MessageSearchRequest.extend({
	context_channel_id: Int64Type.optional(),
	context_guild_id: Int64Type.optional(),
	channel_ids: z.array(Int64Type).max(500).optional(),
});

export const SearchController = (app: HonoApp) => {
	app.post(
		'/search/messages',
		RateLimitMiddleware(RateLimitConfigs.SEARCH_MESSAGES),
		LoginRequired,
		DefaultUserOnly,
		Validator('json', SearchMessagesRequest),
		async (ctx) => {
			const params = ctx.req.valid('json');
			const userId = ctx.get('user').id;
			const requestCache = ctx.get('requestCache');
			const {channel_ids, context_channel_id, context_guild_id, ...searchParams} = params;
			const contextChannelId = context_channel_id ? createChannelID(context_channel_id) : null;
			const contextGuildId = context_guild_id ? createGuildID(context_guild_id) : null;
			const channelIds = channel_ids?.map((id) => createChannelID(id)) ?? [];

			const globalSearch = new GlobalSearchService(
				ctx.get('channelRepository') as IChannelRepository,
				ctx.get('channelService') as ChannelService,
				ctx.get('guildService') as GuildService,
				ctx.get('userRepository') as IUserRepository,
				ctx.get('userCacheService') as UserCacheService,
				ctx.get('mediaService') as IMediaService,
				ctx.get('workerService') as IWorkerService,
			);

			const scope = searchParams.scope ?? 'current';

			let result: MessageSearchResponse | {indexing: true};
			switch (scope) {
				case 'all_guilds':
					result = await ctx.get('guildService').searchAllGuilds({
						userId,
						channelIds,
						searchParams,
						requestCache,
					});
					break;
				case 'all_dms':
				case 'open_dms':
					result = await globalSearch.searchAcrossDms({
						userId,
						scope,
						searchParams,
						requestCache,
						includeChannelId: contextChannelId,
						requestedChannelIds: channelIds,
					});
					break;
				case 'all':
					result = await globalSearch.searchAcrossGuildsAndDms({
						userId,
						dmScope: 'all_dms',
						searchParams,
						requestCache,
						includeChannelId: contextChannelId,
						requestedChannelIds: channelIds,
					});
					break;
				case 'open_dms_and_all_guilds':
					result = await globalSearch.searchAcrossGuildsAndDms({
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
						result = await ctx.get('guildService').searchMessages({
							userId,
							guildId: contextGuildId,
							channelIds,
							searchParams,
							requestCache,
						});
					} else if (contextChannelId) {
						result = await ctx.get('channelService').searchMessages({
							userId,
							channelId: contextChannelId,
							searchParams,
							requestCache,
						});
					} else {
						throw InputValidationError.create('context', 'Context channel or guild ID is required');
					}
					break;
			}

			if ('indexing' in result && result.indexing) {
				throw new ChannelIndexingError();
			}

			return ctx.json(result);
		},
	);
};
