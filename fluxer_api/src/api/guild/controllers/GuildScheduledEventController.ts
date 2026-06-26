// SPDX-License-Identifier: AGPL-3.0-or-later

import {GuildIdParam, GuildIdScheduledEventIdParam} from '@fluxer/schema/src/domains/common/CommonParamSchemas';
import {
	GuildScheduledEventListResponse,
	GuildScheduledEventResponse,
	GuildScheduledEventRsvpListResponse,
	GuildScheduledEventRsvpResponse,
} from '@fluxer/schema/src/domains/guild/GuildScheduledEventSchemas';
import {
	GuildScheduledEventCreateRequest,
	GuildScheduledEventRsvpRequest,
	GuildScheduledEventUpdateRequest,
} from '@fluxer/schema/src/domains/guild/GuildRequestSchemas';
import {createGuildID, createScheduledEventID} from '../../BrandedTypes';
import {LoginRequired} from '../../middleware/AuthMiddleware';
import {OpenAPI} from '../../middleware/ResponseTypeMiddleware';
import type {HonoApp} from '../../types/HonoEnv';
import {Validator} from '../../Validator';

// ponytail: rate limits skipped — add when events are actively used

export function GuildScheduledEventController(app: HonoApp) {
	app.post(
		'/guilds/:guild_id/scheduled-events',
		LoginRequired,
		Validator('param', GuildIdParam),
		Validator('json', GuildScheduledEventCreateRequest),
		OpenAPI({
			operationId: 'create_guild_scheduled_event',
			summary: 'Create scheduled event',
			responseSchema: GuildScheduledEventResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Guilds'],
			description: 'Create a guild scheduled event. Requires manage_guild permission.',
		}),
		async (ctx) => {
			const user = ctx.get('user');
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			const data = ctx.req.valid('json');
			const event = await ctx.get('guildService').events.createEvent(user.id, guildId, data);
			return ctx.json(event);
		},
	);

	app.get(
		'/guilds/:guild_id/scheduled-events',
		LoginRequired,
		Validator('param', GuildIdParam),
		OpenAPI({
			operationId: 'list_guild_scheduled_events',
			summary: 'List guild scheduled events',
			responseSchema: GuildScheduledEventListResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Guilds'],
			description: 'List scheduled events for a guild.',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			const events = await ctx.get('guildService').events.listEvents(userId, guildId);
			return ctx.json(events);
		},
	);

	app.get(
		'/guilds/:guild_id/scheduled-events/:scheduled_event_id',
		LoginRequired,
		Validator('param', GuildIdScheduledEventIdParam),
		OpenAPI({
			operationId: 'get_guild_scheduled_event',
			summary: 'Get scheduled event',
			responseSchema: GuildScheduledEventResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Guilds'],
			description: 'Get a single scheduled event by ID.',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const {guild_id, scheduled_event_id} = ctx.req.valid('param');
			const guildId = createGuildID(guild_id);
			const eventId = createScheduledEventID(scheduled_event_id);
			const event = await ctx.get('guildService').events.getEvent(userId, guildId, eventId);
			if (!event) return ctx.json({error: 'Unknown scheduled event'}, 404);
			return ctx.json(event);
		},
	);

	app.patch(
		'/guilds/:guild_id/scheduled-events/:scheduled_event_id',
		LoginRequired,
		Validator('param', GuildIdScheduledEventIdParam),
		Validator('json', GuildScheduledEventUpdateRequest),
		OpenAPI({
			operationId: 'update_guild_scheduled_event',
			summary: 'Update scheduled event',
			responseSchema: GuildScheduledEventResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Guilds'],
			description: 'Update a guild scheduled event. Requires manage_guild permission.',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const {guild_id, scheduled_event_id} = ctx.req.valid('param');
			const guildId = createGuildID(guild_id);
			const eventId = createScheduledEventID(scheduled_event_id);
			const data = ctx.req.valid('json');
			const event = await ctx.get('guildService').events.updateEvent(userId, guildId, eventId, data);
			if (!event) return ctx.json({error: 'Unknown scheduled event'}, 404);
			return ctx.json(event);
		},
	);

	app.delete(
		'/guilds/:guild_id/scheduled-events/:scheduled_event_id',
		LoginRequired,
		Validator('param', GuildIdScheduledEventIdParam),
		OpenAPI({
			operationId: 'delete_guild_scheduled_event',
			summary: 'Delete scheduled event',
			responseSchema: null,
			statusCode: 204,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Guilds'],
			description: 'Delete a guild scheduled event. Requires manage_guild permission.',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const {guild_id, scheduled_event_id} = ctx.req.valid('param');
			const guildId = createGuildID(guild_id);
			const eventId = createScheduledEventID(scheduled_event_id);
			await ctx.get('guildService').events.deleteEvent(userId, guildId, eventId);
			return ctx.body(null, 204);
		},
	);

	app.put(
		'/guilds/:guild_id/scheduled-events/:scheduled_event_id/rsvp',
		LoginRequired,
		Validator('param', GuildIdScheduledEventIdParam),
		Validator('json', GuildScheduledEventRsvpRequest),
		OpenAPI({
			operationId: 'set_guild_scheduled_event_rsvp',
			summary: 'Set RSVP for scheduled event',
			responseSchema: GuildScheduledEventRsvpResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Guilds'],
			description: 'Set RSVP status for the current user on a scheduled event.',
		}),
		async (ctx) => {
			const user = ctx.get('user');
			const {guild_id, scheduled_event_id} = ctx.req.valid('param');
			const guildId = createGuildID(guild_id);
			const eventId = createScheduledEventID(scheduled_event_id);
			const {status} = ctx.req.valid('json');
			const rsvp = await ctx.get('guildService').events.setRsvp(user.id, guildId, eventId, status);
			return ctx.json(rsvp);
		},
	);

	app.get(
		'/guilds/:guild_id/scheduled-events/:scheduled_event_id/rsvps',
		LoginRequired,
		Validator('param', GuildIdScheduledEventIdParam),
		OpenAPI({
			operationId: 'list_guild_scheduled_event_rsvps',
			summary: 'List RSVPs for scheduled event',
			responseSchema: GuildScheduledEventRsvpListResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Guilds'],
			description: 'List all RSVPs for a scheduled event.',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const {guild_id, scheduled_event_id} = ctx.req.valid('param');
			const guildId = createGuildID(guild_id);
			const eventId = createScheduledEventID(scheduled_event_id);
			const rsvps = await ctx.get('guildService').events.listRsvps(userId, guildId, eventId);
			return ctx.json(rsvps);
		},
	);
}
