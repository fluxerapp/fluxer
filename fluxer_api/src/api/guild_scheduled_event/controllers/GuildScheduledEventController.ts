// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	GuildScheduledEventCreateRequest,
	GuildScheduledEventUpdateRequest,
	GuildScheduledEventResponse,
} from '@fluxer/schema/src/domains/guild_scheduled_event/GuildScheduledEventSchemas';
import {
	GuildIdParam,
	ChannelIdParam,
} from '@fluxer/schema/src/domains/common/CommonParamSchemas';
import {createGuildID, createChannelID, createGuildScheduledEventID} from '../../BrandedTypes';
import {DefaultUserOnly, LoginRequired} from '../../middleware/AuthMiddleware';
import {RateLimitMiddleware} from '../../middleware/RateLimitMiddleware';
import {OpenAPI} from '../../middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '../../RateLimitConfig';
import type {HonoApp} from '../../types/HonoEnv';
import {Validator} from '../../Validator';
import {z} from 'zod';

const EventIdParam = z.object({
	event_id: z.string().describe('The ID of the scheduled event'),
});

export function GuildScheduledEventController(app: HonoApp) {
	app.get(
		'/guilds/:guild_id/scheduled-events',
		RateLimitMiddleware(RateLimitConfigs.GUILD_GET),
		LoginRequired,
		Validator('param', GuildIdParam),
		OpenAPI({
			operationId: 'list_guild_scheduled_events',
			summary: 'List scheduled events for a guild',
			description: 'Returns a list of scheduled events for the specified guild.',
			responseSchema: z.array(GuildScheduledEventResponse),
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: 'Guild Scheduled Events',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			const service = ctx.get('guildScheduledEventService');
			const events = await service.listEventsByGuild(guildId);
			const responses = [];
			for (const event of events) {
				const subscriberCount = await service.getSubscriberCount(event.id);
				responses.push(event.toResponse(subscriberCount));
			}
			return ctx.json(responses);
		},
	);

	app.get(
		'/guilds/:guild_id/scheduled-events/:event_id',
		RateLimitMiddleware(RateLimitConfigs.GUILD_GET),
		LoginRequired,
		Validator('param', GuildIdParam),
		Validator('param', EventIdParam),
		OpenAPI({
			operationId: 'get_guild_scheduled_event',
			summary: 'Get a scheduled event',
			description: 'Returns a specific scheduled event from the guild.',
			responseSchema: GuildScheduledEventResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: 'Guild Scheduled Events',
		}),
		async (ctx) => {
			const eventId = createGuildScheduledEventID(BigInt(ctx.req.valid('param').event_id));
			const service = ctx.get('guildScheduledEventService');
			const event = await service.getEvent(eventId);
			if (!event) {
				return ctx.json({message: 'Event not found'}, 404);
			}
			const subscriberCount = await service.getSubscriberCount(event.id);
			return ctx.json(event.toResponse(subscriberCount));
		},
	);

	app.post(
		'/guilds/:guild_id/scheduled-events',
		RateLimitMiddleware(RateLimitConfigs.GUILD_UPDATE),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', GuildIdParam),
		Validator('json', GuildScheduledEventCreateRequest),
		OpenAPI({
			operationId: 'create_guild_scheduled_event',
			summary: 'Create a scheduled event',
			description: 'Creates a new scheduled event in the guild.',
			requestSchema: GuildScheduledEventCreateRequest,
			responseSchema: GuildScheduledEventResponse,
			statusCode: 201,
			security: ['bearerToken', 'sessionToken'],
			tags: 'Guild Scheduled Events',
		}),
		async (ctx) => {
			const user = ctx.get('user');
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			const data = ctx.req.valid('json');
			const service = ctx.get('guildScheduledEventService');

			const event = await service.createEvent({
				user,
				guildId,
				name: data.name,
				description: data.description,
				scheduledStartTime: new Date(data.scheduled_start_time),
				scheduledEndTime: data.scheduled_end_time ? new Date(data.scheduled_end_time) : null,
				privacyLevel: data.privacy_level,
				entityType: data.entity_type,
				entityId: data.entity_id != null ? createChannelID(BigInt(data.entity_id)) : null,
				channelId: data.channel_id != null ? createChannelID(BigInt(data.channel_id)) : null,
				location: data.location,
			});

			const subscriberCount = await service.getSubscriberCount(event.id);
			return ctx.json(event.toResponse(subscriberCount), 201);
		},
	);

	app.patch(
		'/guilds/:guild_id/scheduled-events/:event_id',
		RateLimitMiddleware(RateLimitConfigs.GUILD_UPDATE),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', GuildIdParam),
		Validator('param', EventIdParam),
		Validator('json', GuildScheduledEventUpdateRequest),
		OpenAPI({
			operationId: 'update_guild_scheduled_event',
			summary: 'Update a scheduled event',
			description: 'Updates an existing scheduled event.',
			requestSchema: GuildScheduledEventUpdateRequest,
			responseSchema: GuildScheduledEventResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: 'Guild Scheduled Events',
		}),
		async (ctx) => {
			const user = ctx.get('user');
			const eventId = createGuildScheduledEventID(BigInt(ctx.req.valid('param').event_id));
			const data = ctx.req.valid('json');
			const service = ctx.get('guildScheduledEventService');

			const event = await service.updateEvent({
				user,
				eventId,
				name: data.name,
				description: data.description,
				scheduledStartTime: data.scheduled_start_time ? new Date(data.scheduled_start_time) : undefined,
				scheduledEndTime: data.scheduled_end_time !== undefined
					? (data.scheduled_end_time ? new Date(data.scheduled_end_time) : null)
					: undefined,
				privacyLevel: data.privacy_level,
				entityType: data.entity_type,
				entityId: data.entity_id !== undefined
					? (data.entity_id != null ? createChannelID(BigInt(data.entity_id)) : null)
					: undefined,
				channelId: data.channel_id !== undefined
					? (data.channel_id != null ? createChannelID(BigInt(data.channel_id)) : null)
					: undefined,
				location: data.location,
				status: data.status,
			});

			const subscriberCount = await service.getSubscriberCount(event.id);
			return ctx.json(event.toResponse(subscriberCount));
		},
	);

	app.delete(
		'/guilds/:guild_id/scheduled-events/:event_id',
		RateLimitMiddleware(RateLimitConfigs.GUILD_UPDATE),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', GuildIdParam),
		Validator('param', EventIdParam),
		OpenAPI({
			operationId: 'delete_guild_scheduled_event',
			summary: 'Delete a scheduled event',
			description: 'Deletes a scheduled event from the guild.',
			responseSchema: null,
			statusCode: 204,
			security: ['bearerToken', 'sessionToken'],
			tags: 'Guild Scheduled Events',
		}),
		async (ctx) => {
			const user = ctx.get('user');
			const eventId = createGuildScheduledEventID(BigInt(ctx.req.valid('param').event_id));
			const service = ctx.get('guildScheduledEventService');
			await service.deleteEvent(user, eventId);
			return ctx.body(null, 204);
		},
	);

	app.put(
		'/guilds/:guild_id/scheduled-events/:event_id/subscribe',
		RateLimitMiddleware(RateLimitConfigs.GUILD_UPDATE),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', GuildIdParam),
		Validator('param', EventIdParam),
		OpenAPI({
			operationId: 'subscribe_to_guild_scheduled_event',
			summary: 'Subscribe to a scheduled event',
			description: 'Subscribes the current user to a scheduled event (RSVP "going").',
			responseSchema: null,
			statusCode: 204,
			security: ['bearerToken', 'sessionToken'],
			tags: 'Guild Scheduled Events',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			const eventId = createGuildScheduledEventID(BigInt(ctx.req.valid('param').event_id));
			const service = ctx.get('guildScheduledEventService');
			await service.subscribeUser(userId, eventId, guildId);
			return ctx.body(null, 204);
		},
	);

	app.delete(
		'/guilds/:guild_id/scheduled-events/:event_id/subscribe',
		RateLimitMiddleware(RateLimitConfigs.GUILD_UPDATE),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', GuildIdParam),
		Validator('param', EventIdParam),
		OpenAPI({
			operationId: 'unsubscribe_from_guild_scheduled_event',
			summary: 'Unsubscribe from a scheduled event',
			description: 'Unsubscribes the current user from a scheduled event.',
			responseSchema: null,
			statusCode: 204,
			security: ['bearerToken', 'sessionToken'],
			tags: 'Guild Scheduled Events',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			const eventId = createGuildScheduledEventID(BigInt(ctx.req.valid('param').event_id));
			const service = ctx.get('guildScheduledEventService');
			await service.unsubscribeUser(userId, eventId, guildId);
			return ctx.body(null, 204);
		},
	);

	app.get(
		'/guilds/:guild_id/scheduled-events/:event_id/subscribers',
		RateLimitMiddleware(RateLimitConfigs.GUILD_GET),
		LoginRequired,
		Validator('param', GuildIdParam),
		Validator('param', EventIdParam),
		OpenAPI({
			operationId: 'list_guild_scheduled_event_subscribers',
			summary: 'List subscribers of a scheduled event',
			description: 'Returns a list of user IDs subscribed to the scheduled event.',
			responseSchema: z.array(z.string()),
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: 'Guild Scheduled Events',
		}),
		async (ctx) => {
			const eventId = createGuildScheduledEventID(BigInt(ctx.req.valid('param').event_id));
			const service = ctx.get('guildScheduledEventService');
			const subscribers = await service.listSubscribers(eventId);
			return ctx.json(subscribers.map((id) => id.toString()));
		},
	);

	app.get(
		'/users/@me/scheduled-events',
		RateLimitMiddleware(RateLimitConfigs.USER_GET),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'list_user_scheduled_events',
			summary: 'List user subscribed events',
			description: 'Returns a list of scheduled events the current user is subscribed to.',
			responseSchema: z.array(GuildScheduledEventResponse),
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: 'Guild Scheduled Events',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const service = ctx.get('guildScheduledEventService');
			const events = await service.listEventsByUser(userId);
			const responses = [];
			for (const event of events) {
				const subscriberCount = await service.getSubscriberCount(event.id);
				responses.push(event.toResponse(subscriberCount));
			}
			return ctx.json(responses);
		},
	);
}
