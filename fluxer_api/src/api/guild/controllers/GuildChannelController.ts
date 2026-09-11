// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	ChannelCreateRequest,
	ChannelPositionUpdateRequest,
} from '@fluxer/schema/src/domains/channel/ChannelRequestSchemas';
import {ChannelListResponse, ChannelResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import {GuildIdParam} from '@fluxer/schema/src/domains/common/CommonParamSchemas';

import {createChannelID, createGuildID} from '../../BrandedTypes';
import {LoginRequired} from '../../middleware/AuthMiddleware';
import {RateLimitMiddleware} from '../../middleware/RateLimitMiddleware';
import {OpenAPI} from '../../middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '../../RateLimitConfig';
import type {HonoApp} from '../../types/HonoEnv';
import {Validator} from '../../Validator';

export function GuildChannelController(app: HonoApp) {
	app.get(
		'/guilds/:guild_id/channels',
		RateLimitMiddleware(RateLimitConfigs.GUILD_CHANNELS_LIST),
		LoginRequired,
		Validator('param', GuildIdParam),
		OpenAPI({
			operationId: 'list_guild_channels',
			summary: 'List guild channels',
			responseSchema: ChannelListResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Guilds'],
			description: 'List guild channels. Returns all channels in the guild that the user has access to view.',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			const requestCache = ctx.get('requestCache');
			return ctx.json(await ctx.get('guildService').channels.getChannels({userId, guildId, requestCache}));
		},
	);
	app.post(
		'/guilds/:guild_id/channels',
		RateLimitMiddleware(RateLimitConfigs.GUILD_CHANNEL_CREATE),
		LoginRequired,
		Validator('param', GuildIdParam),
		Validator('json', ChannelCreateRequest),
		OpenAPI({
			operationId: 'create_guild_channel',
			summary: 'Create guild channel',
			responseSchema: ChannelResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Guilds'],
			description:
				'Create guild channel. Requires manage_channels permission. Creates a new text, voice, or category channel in the guild.',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			const data = ctx.req.valid('json');
			const requestCache = ctx.get('requestCache');
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			return ctx.json(
				await ctx.get('guildService').channels.createChannel({userId, guildId, data, requestCache}, auditLogReason),
			);
		},
	);
	app.patch(
		'/guilds/:guild_id/channels',
		RateLimitMiddleware(RateLimitConfigs.GUILD_CHANNEL_POSITIONS),
		LoginRequired,
		Validator('param', GuildIdParam),
		Validator('json', ChannelPositionUpdateRequest),
		OpenAPI({
			operationId: 'update_guild_channel_positions',
			summary: 'Update channel positions',
			responseSchema: null,
			statusCode: 204,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Guilds'],
			description:
				'Update channel positions. Requires manage_channels permission. Reorders channels and optionally changes parent categories and permission locks.',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			const payload = ctx.req.valid('json');
			const requestCache = ctx.get('requestCache');
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			await ctx.get('guildService').channels.updateChannelPositions(
				{
					userId,
					guildId,
					updates: payload.map((item) => ({
						channelId: createChannelID(item.id),
						position: item.position,
						parentId: item.parent_id == null ? item.parent_id : createChannelID(item.parent_id),
						precedingSiblingId:
							item.preceding_sibling_id == null
								? item.preceding_sibling_id
								: createChannelID(item.preceding_sibling_id),
						lockPermissions: item.lock_permissions ?? false,
					})),
					requestCache,
				},
				auditLogReason,
			);
			return ctx.body(null, 204);
		},
	);
}
