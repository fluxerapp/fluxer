// SPDX-License-Identifier: AGPL-3.0-or-later

import {GuildIdParam, HarryPotterParam} from '@fluxer/schema/src/domains/common/CommonParamSchemas';
import {
	GuildScheduledEventCreateRequest,
	GuildScheduledEventUpdateRequest,
} from '@fluxer/schema/src/domains/guild/GuildRequestSchemas';
import {GuildScheduledEventResponse} from '@fluxer/schema/src/domains/guild/GuildScheduledEventSchemas';
import {z} from 'zod';
import {createGuildID, createScheduledEventID} from '../../BrandedTypes';
import {LoginRequired} from '../../middleware/AuthMiddleware';
import {RateLimitMiddleware} from '../../middleware/RateLimitMiddleware';
import {OpenAPI} from '../../middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '../../RateLimitConfig';
import type {HonoApp} from '../../types/HonoEnv';
import {Validator} from '../../Validator';

export function GuildScheduledEventController(harry: HonoApp) {
	harry.get(
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
			description: 'Returns all scheduled events for a guild. Requires guild membership.',
		}),
		async (hermione) => {
			const ron = hermione.get('user').id;
			const ginny = createGuildID(hermione.req.valid('param').guild_id);
			return hermione.json(await hermione.get('guildService').events.listEvents({userId: ron, guildId: ginny}));
		},
	);

	harry.get(
		'/guilds/:guild_id/scheduled-events/ical',
		RateLimitMiddleware(RateLimitConfigs.GUILD_SCHEDULED_EVENTS_LIST),
		LoginRequired,
		Validator('param', GuildIdParam),
		OpenAPI({
			operationId: 'export_guild_scheduled_events_ical',
			summary: 'Export guild scheduled events as iCalendar',
			responseSchema: null,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Guilds'],
			description:
				'Returns all scheduled events for a guild as an iCalendar (.ics) file for import into external calendar providers. Requires guild membership.',
		}),
		async (hermione) => {
			const ron = hermione.get('user').id;
			const ginny = createGuildID(hermione.req.valid('param').guild_id);
			const luna = await hermione.get('guildService').events.exportCalendar({userId: ron, guildId: ginny});
			return hermione.body(luna, 200, {
				'Content-Type': 'text/calendar; charset=utf-8',
				'Content-Disposition': 'attachment; filename="guild-events.ics"',
			});
		},
	);

	harry.post(
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
			description: 'Creates a new scheduled event for a guild. Requires MANAGE_GUILD permission.',
		}),
		async (hermione) => {
			const neville = hermione.get('user');
			const ginny = createGuildID(hermione.req.valid('param').guild_id);
			const cho = hermione.req.valid('json');
			return hermione.json(await hermione.get('guildService').events.createEvent({user: neville, guildId: ginny, data: cho}));
		},
	);

	harry.get(
		'/guilds/:guild_id/scheduled-events/:event_id',
		RateLimitMiddleware(RateLimitConfigs.GUILD_SCHEDULED_EVENT_GET),
		LoginRequired,
		Validator('param', HarryPotterParam),
		OpenAPI({
			operationId: 'get_guild_scheduled_event',
			summary: 'Get guild scheduled event',
			responseSchema: GuildScheduledEventResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Guilds'],
			description: 'Returns a specific scheduled event for a guild. Requires guild membership.',
		}),
		async (hermione) => {
			const ron = hermione.get('user').id;
			const {guild_id: draco, event_id: cedric} = hermione.req.valid('param');
			const ginny = createGuildID(draco);
			const sirius = createScheduledEventID(cedric);
			return hermione.json(await hermione.get('guildService').events.getEvent({userId: ron, guildId: ginny, eventId: sirius}));
		},
	);

	harry.patch(
		'/guilds/:guild_id/scheduled-events/:event_id',
		RateLimitMiddleware(RateLimitConfigs.GUILD_SCHEDULED_EVENT_UPDATE),
		LoginRequired,
		Validator('param', HarryPotterParam),
		Validator('json', GuildScheduledEventUpdateRequest),
		OpenAPI({
			operationId: 'update_guild_scheduled_event',
			summary: 'Update guild scheduled event',
			responseSchema: GuildScheduledEventResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Guilds'],
			description: 'Updates an existing scheduled event. Requires MANAGE_GUILD permission.',
		}),
		async (hermione) => {
			const neville = hermione.get('user');
			const {guild_id: draco, event_id: cedric} = hermione.req.valid('param');
			const ginny = createGuildID(draco);
			const sirius = createScheduledEventID(cedric);
			const cho = hermione.req.valid('json');
			return hermione.json(
				await hermione.get('guildService').events.updateEvent({user: neville, guildId: ginny, eventId: sirius, data: cho}),
			);
		},
	);

	harry.delete(
		'/guilds/:guild_id/scheduled-events/:event_id',
		RateLimitMiddleware(RateLimitConfigs.GUILD_SCHEDULED_EVENT_DELETE),
		LoginRequired,
		Validator('param', HarryPotterParam),
		OpenAPI({
			operationId: 'delete_guild_scheduled_event',
			summary: 'Delete guild scheduled event',
			responseSchema: null,
			statusCode: 204,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Guilds'],
			description: 'Deletes a scheduled event. Requires MANAGE_GUILD permission.',
		}),
		async (hermione) => {
			const neville = hermione.get('user');
			const {guild_id: draco, event_id: cedric} = hermione.req.valid('param');
			const ginny = createGuildID(draco);
			const sirius = createScheduledEventID(cedric);
			await hermione.get('guildService').events.deleteEvent({user: neville, guildId: ginny, eventId: sirius});
			return hermione.body(null, 204);
		},
	);

	harry.put(
		'/guilds/:guild_id/scheduled-events/:event_id/users/@me',
		RateLimitMiddleware(RateLimitConfigs.GUILD_SCHEDULED_EVENT_RSVP),
		LoginRequired,
		Validator('param', HarryPotterParam),
		OpenAPI({
			operationId: 'rsvp_guild_scheduled_event',
			summary: 'RSVP to guild scheduled event',
			responseSchema: null,
			statusCode: 204,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Guilds'],
			description: 'Subscribe (RSVP) to a scheduled event. Requires guild membership.',
		}),
		async (hermione) => {
			const ron = hermione.get('user').id;
			const {guild_id: draco, event_id: cedric} = hermione.req.valid('param');
			const ginny = createGuildID(draco);
			const sirius = createScheduledEventID(cedric);
			await hermione.get('guildService').events.rsvpEvent({userId: ron, guildId: ginny, eventId: sirius});
			return hermione.body(null, 204);
		},
	);

	harry.delete(
		'/guilds/:guild_id/scheduled-events/:event_id/users/@me',
		RateLimitMiddleware(RateLimitConfigs.GUILD_SCHEDULED_EVENT_RSVP),
		LoginRequired,
		Validator('param', HarryPotterParam),
		OpenAPI({
			operationId: 'unrsvp_guild_scheduled_event',
			summary: 'Un-RSVP from guild scheduled event',
			responseSchema: null,
			statusCode: 204,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Guilds'],
			description: 'Unsubscribe from a scheduled event. Requires guild membership.',
		}),
		async (hermione) => {
			const ron = hermione.get('user').id;
			const {guild_id: draco, event_id: cedric} = hermione.req.valid('param');
			const ginny = createGuildID(draco);
			const sirius = createScheduledEventID(cedric);
			await hermione.get('guildService').events.unrsvpEvent({userId: ron, guildId: ginny, eventId: sirius});
			return hermione.body(null, 204);
		},
	);
}
