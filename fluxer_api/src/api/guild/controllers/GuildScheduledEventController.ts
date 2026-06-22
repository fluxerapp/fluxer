// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	GuildIdParam,
	GuildIdScheduledEventIdParam,
} from '@fluxer/schema/src/domains/common/CommonParamSchemas';
import {
	GuildScheduledEventCreateRequest,
	GuildScheduledEventUpdateRequest,
} from '@fluxer/schema/src/domains/guild/GuildRequestSchemas';
import {
	GuildScheduledEventResponse,
} from '@fluxer/schema/src/domains/guild/GuildScheduledEventSchemas';
import {z} from 'zod';
import {createGuildID, createScheduledEventID} from '../../BrandedTypes';
import {LoginRequired} from '../../middleware/AuthMiddleware';
import {RateLimitMiddleware} from '../../middleware/RateLimitMiddleware';
import {OpenAPI} from '../../middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '../../RateLimitConfig';
import type {HonoApp} from '../../types/HonoEnv';
import {Validator} from '../../Validator';

export function GuildScheduledEventController(app: HonoApp) {
	app.get(
		'/guilds/:guild_id/scheduled-events',
		RateLimitMiddleware(RateLimitConfigs.GUILD_SCHEDULED_EVENTS_LIST),
		LoginRequired,
		Validator('param', GuildIdParam),
		OpenAPI({
			operationId: 'list_guild_scheduled_events',
			summary: 'List guild scheduled events',
			responseSchema: z.array(GuildScheduledEventResponse),
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Guilds'],
			description:
				'Returns all scheduled events for a guild. Requires guild membership.',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			return ctx.json(await ctx.get('guildService').events.listEvents({userId, guildId}));
		},
	);

	app.post(
		'/guilds/:guild_id/scheduled-events',
		RateLimitMiddleware(RateLimitConfigs.GUILD_SCHEDULED_EVENT_CREATE),
		LoginRequired,
		Validator('param', GuildIdParam),
		Validator('json', GuildScheduledEventCreateRequest),
		OpenAPI({
			operationId: 'create_guild_scheduled_event',
			summary: 'Create guild scheduled event',
			responseSchema: GuildScheduledEventResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Guilds'],
			description:
				'Creates a new scheduled event for a guild. Requires MANAGE_EVENTS permission.',
		}),
		async (ctx) => {
			const user = ctx.get('user');
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			const data = ctx.req.valid('json');
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			return ctx.json(
				await ctx.get('guildService').events.createEvent({user, guildId, data}, auditLogReason),
			);
		},
	);

	app.get(
		'/guilds/:guild_id/scheduled-events/:event_id',
		RateLimitMiddleware(RateLimitConfigs.GUILD_SCHEDULED_EVENT_GET),
		LoginRequired,
		Validator('param', GuildIdScheduledEventIdParam),
		OpenAPI({
			operationId: 'get_guild_scheduled_event',
			summary: 'Get guild scheduled event',
			responseSchema: GuildScheduledEventResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Guilds'],
			description:
				'Returns a specific scheduled event for a guild. Requires guild membership.',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const {guild_id, event_id} = ctx.req.valid('param');
			const guildId = createGuildID(guild_id);
			const eventId = createScheduledEventID(event_id);
			return ctx.json(await ctx.get('guildService').events.getEvent({userId, guildId, eventId}));
		},
	);

	app.patch(
		'/guilds/:guild_id/scheduled-events/:event_id',
		RateLimitMiddleware(RateLimitConfigs.GUILD_SCHEDULED_EVENT_UPDATE),
		LoginRequired,
		Validator('param', GuildIdScheduledEventIdParam),
		Validator('json', GuildScheduledEventUpdateRequest),
		OpenAPI({
			operationId: 'update_guild_scheduled_event',
			summary: 'Update guild scheduled event',
			responseSchema: GuildScheduledEventResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Guilds'],
			description:
				'Updates an existing scheduled event. Requires MANAGE_EVENTS permission.',
		}),
		async (ctx) => {
			const user = ctx.get('user');
			const {guild_id, event_id} = ctx.req.valid('param');
			const guildId = createGuildID(guild_id);
			const eventId = createScheduledEventID(event_id);
			const data = ctx.req.valid('json');
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			return ctx.json(
				await ctx.get('guildService').events.updateEvent({user, guildId, eventId, data}, auditLogReason),
			);
		},
	);

	app.delete(
		'/guilds/:guild_id/scheduled-events/:event_id',
		RateLimitMiddleware(RateLimitConfigs.GUILD_SCHEDULED_EVENT_DELETE),
		LoginRequired,
		Validator('param', GuildIdScheduledEventIdParam),
		OpenAPI({
			operationId: 'delete_guild_scheduled_event',
			summary: 'Delete guild scheduled event',
			responseSchema: null,
			statusCode: 204,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Guilds'],
			description:
				'Deletes a scheduled event. Requires MANAGE_EVENTS permission.',
		}),
		async (ctx) => {
			const user = ctx.get('user');
			const {guild_id, event_id} = ctx.req.valid('param');
			const guildId = createGuildID(guild_id);
			const eventId = createScheduledEventID(event_id);
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			await ctx.get('guildService').events.deleteEvent({user, guildId, eventId}, auditLogReason);
			return ctx.body(null, 204);
		},
	);

	app.put(
		'/guilds/:guild_id/scheduled-events/:event_id/users/@me',
		RateLimitMiddleware(RateLimitConfigs.GUILD_SCHEDULED_EVENT_RSVP),
		LoginRequired,
		Validator('param', GuildIdScheduledEventIdParam),
		OpenAPI({
			operationId: 'rsvp_guild_scheduled_event',
			summary: 'RSVP to guild scheduled event',
			responseSchema: null,
			statusCode: 204,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Guilds'],
			description:
				'Subscribe (RSVP) to a scheduled event. Requires guild membership.',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const {guild_id, event_id} = ctx.req.valid('param');
			const guildId = createGuildID(guild_id);
			const eventId = createScheduledEventID(event_id);
			await ctx.get('guildService').events.rsvpEvent({userId, guildId, eventId});
			return ctx.body(null, 204);
		},
	);

	app.delete(
		'/guilds/:guild_id/scheduled-events/:event_id/users/@me',
		RateLimitMiddleware(RateLimitConfigs.GUILD_SCHEDULED_EVENT_RSVP),
		LoginRequired,
		Validator('param', GuildIdScheduledEventIdParam),
		OpenAPI({
			operationId: 'unrsvp_guild_scheduled_event',
			summary: 'Un-RSVP from guild scheduled event',
			responseSchema: null,
			statusCode: 204,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Guilds'],
			description:
				'Unsubscribe from a scheduled event. Requires guild membership.',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const {guild_id, event_id} = ctx.req.valid('param');
			const guildId = createGuildID(guild_id);
			const eventId = createScheduledEventID(event_id);
			await ctx.get('guildService').events.unrsvpEvent({userId, guildId, eventId});
			return ctx.body(null, 204);
		},
	);
}
