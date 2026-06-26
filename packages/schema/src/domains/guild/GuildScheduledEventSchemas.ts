// SPDX-License-Identifier: AGPL-3.0-or-later

import {SnowflakeStringType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

export const GuildScheduledEventResponse = z.object({
	id: SnowflakeStringType.describe('The unique identifier for this scheduled event'),
	guild_id: SnowflakeStringType.describe('The guild this scheduled event belongs to'),
	channel_id: SnowflakeStringType.optional().describe('The channel this event is associated with, if any'),
	creator_id: SnowflakeStringType.describe('The user who created this scheduled event'),
	name: z.string().describe('The name of the scheduled event'),
	description: z.string().optional().describe('The description of the scheduled event'),
	scheduled_start_time: z.string().describe('The start time of the event (ISO 8601)'),
	scheduled_end_time: z.string().optional().describe('The end time of the event (ISO 8601)'),
	privacy_level: z.number().describe('The privacy level of the event'),
	status: z.number().describe('The status of the event'),
	entity_type: z.number().optional().nullable().describe('The entity type of the event'),
	entity_id: SnowflakeStringType.optional().nullable().describe('The entity ID associated with this event'),
	location: z.string().optional().nullable().describe('The location for external events'),
	image: z.string().optional().nullable().describe('The image hash for the event'),
});
export type GuildScheduledEventResponse = z.infer<typeof GuildScheduledEventResponse>;

export const GuildScheduledEventListResponse = z.array(GuildScheduledEventResponse);
export type GuildScheduledEventListResponse = z.infer<typeof GuildScheduledEventListResponse>;

export const GuildScheduledEventRsvpResponse = z.object({
	guild_id: SnowflakeStringType.describe('The guild this RSVP belongs to'),
	scheduled_event_id: SnowflakeStringType.describe('The scheduled event ID'),
	user_id: SnowflakeStringType.describe('The user who made the RSVP'),
	status: z.number().describe('The RSVP status'),
});
export type GuildScheduledEventRsvpResponse = z.infer<typeof GuildScheduledEventRsvpResponse>;

export const GuildScheduledEventRsvpListResponse = z.array(GuildScheduledEventRsvpResponse);
export type GuildScheduledEventRsvpListResponse = z.infer<typeof GuildScheduledEventRsvpListResponse>;
