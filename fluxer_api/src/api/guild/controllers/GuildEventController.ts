// SPDX-License-Identifier: AGPL-3.0-or-later

import {createGuildEventID, createGuildID} from '@app/api/BrandedTypes';
import {LoginRequired} from '@app/api/middleware/AuthMiddleware';
import {RateLimit} from '@app/api/middleware/RateLimitMiddleware';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import type {HonoApp} from '@app/api/types/HonoEnv';
import {Validator} from '@app/api/Validator';
import {
	GuildEventCreateRequest,
	GuildEventIdParam,
	GuildEventListResponse,
	GuildEventResponse,
	GuildEventUpdateRequest,
} from '@fluxer/schema/src/domains/guild/GuildEventSchemas';
import {GuildIdParam} from '@fluxer/schema/src/domains/common/CommonParamSchemas';

export function GuildEventController(app: HonoApp) {
	app.get(
		'/guilds/:guild_id/events',
		LoginRequired,
		RateLimit(RateLimitConfigs.GUILD_EVENTS_LIST),
		Validator('param', GuildIdParam),
		OpenAPI({
			operationId: 'list_guild_events',
			summary: 'List community events',
			responseSchema: GuildEventListResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Guilds'],
		}),
		async (ctx) => {
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			const userId = ctx.get('user').id;
			return ctx.json(await ctx.get('guildService').events.list({userId, guildId}));
		},
	);

	app.post(
		'/guilds/:guild_id/events',
		LoginRequired,
		RateLimit(RateLimitConfigs.GUILD_EVENT_CREATE),
		Validator('param', GuildIdParam),
		Validator('json', GuildEventCreateRequest),
		OpenAPI({
			operationId: 'create_guild_event',
			summary: 'Create a community event',
			description: 'Requires CREATE_EVENTS or MANAGE_EVENTS. Event images pass through explicit-media and banned-content scanners.',
			requestSchema: GuildEventCreateRequest,
			responseSchema: GuildEventResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Guilds'],
		}),
		async (ctx) => {
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			const userId = ctx.get('user').id;
			return ctx.json(await ctx.get('guildService').events.create({userId, guildId, data: ctx.req.valid('json')}));
		},
	);

	app.patch(
		'/guilds/:guild_id/events/:event_id',
		LoginRequired,
		RateLimit(RateLimitConfigs.GUILD_EVENT_UPDATE),
		Validator('param', GuildEventIdParam),
		Validator('json', GuildEventUpdateRequest),
		OpenAPI({
			operationId: 'update_guild_event',
			summary: 'Update a community event',
			requestSchema: GuildEventUpdateRequest,
			responseSchema: GuildEventResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Guilds'],
		}),
		async (ctx) => {
			const params = ctx.req.valid('param');
			return ctx.json(
				await ctx.get('guildService').events.update({
					userId: ctx.get('user').id,
					guildId: createGuildID(params.guild_id),
					eventId: createGuildEventID(params.event_id),
					data: ctx.req.valid('json'),
				}),
			);
		},
	);

	app.delete(
		'/guilds/:guild_id/events/:event_id',
		LoginRequired,
		RateLimit(RateLimitConfigs.GUILD_EVENT_DELETE),
		Validator('param', GuildEventIdParam),
		OpenAPI({
			operationId: 'delete_guild_event',
			summary: 'Delete a community event',
			responseSchema: null,
			statusCode: 204,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Guilds'],
		}),
		async (ctx) => {
			const params = ctx.req.valid('param');
			await ctx.get('guildService').events.delete({
				userId: ctx.get('user').id,
				guildId: createGuildID(params.guild_id),
				eventId: createGuildEventID(params.event_id),
			});
			return ctx.body(null, 204);
		},
	);
}
