// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	GUILD_SCHEDULED_EVENT_DESCRIPTION_MAX_LENGTH,
	GUILD_SCHEDULED_EVENT_NAME_MAX_LENGTH,
	GuildScheduledEventEntityType,
	GuildScheduledEventPrivacyLevel,
} from '@fluxer/constants/src/GuildScheduledEventConstants';
import {
	createStringType,
	createNamedLiteral,
	SnowflakeType,
	SnowflakeStringType,
} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

export const GuildScheduledEventCreateRequest = z.object({
	channel_id: SnowflakeType.optional().describe('The ID of the channel to schedule the event in (for voice/stage events)'),
	name: createStringType(1, GUILD_SCHEDULED_EVENT_NAME_MAX_LENGTH).describe('The name of the event'),
	description: createStringType(0, GUILD_SCHEDULED_EVENT_DESCRIPTION_MAX_LENGTH)
		.optional()
		.describe('The description of the event'),
	scheduled_start_time: z.string().describe('ISO 8601 timestamp for when the event is scheduled to start'),
	scheduled_end_time: z.string().nullable().optional().describe('ISO 8601 timestamp for when the event is scheduled to end'),
	privacy_level: createNamedLiteral(
		[GuildScheduledEventPrivacyLevel.GUILD_ONLY, 'GUILD_ONLY'],
		'The privacy level of the event',
	).optional().describe('The privacy level of the event'),
	entity_type: z
		.number()
		.int()
		.min(1)
		.max(3)
		.describe('The entity type of the event (1=stage, 2=voice, 3=external)'),
	entity_id: SnowflakeType.nullable().optional().describe('The ID of the channel for voice/stage events'),
	location: createStringType(0, 1000).nullable().optional().describe('The location of the event (for external events)'),
});

export type GuildScheduledEventCreateRequest = z.infer<typeof GuildScheduledEventCreateRequest>;

export const GuildScheduledEventUpdateRequest = z.object({
	channel_id: SnowflakeType.optional().describe('The ID of the channel to schedule the event in'),
	name: createStringType(1, GUILD_SCHEDULED_EVENT_NAME_MAX_LENGTH).optional().describe('The name of the event'),
	description: createStringType(0, GUILD_SCHEDULED_EVENT_DESCRIPTION_MAX_LENGTH)
		.nullable()
		.optional()
		.describe('The description of the event'),
	scheduled_start_time: z.string().optional().describe('ISO 8601 timestamp for when the event is scheduled to start'),
	scheduled_end_time: z.string().nullable().optional().describe('ISO 8601 timestamp for when the event is scheduled to end'),
	privacy_level: createNamedLiteral(
		[GuildScheduledEventPrivacyLevel.GUILD_ONLY, 'GUILD_ONLY'],
		'The privacy level of the event',
	).optional(),
	entity_type: z.number().int().min(1).max(3).optional().describe('The entity type of the event'),
	entity_id: SnowflakeType.nullable().optional().describe('The ID of the channel for voice/stage events'),
	location: createStringType(0, 1000).nullable().optional().describe('The location of the event'),
	status: z.number().int().min(0).max(3).optional().describe('The status of the event (0=scheduled, 1=active, 2=completed, 3=cancelled)'),
});

export type GuildScheduledEventUpdateRequest = z.infer<typeof GuildScheduledEventUpdateRequest>;

export const GuildScheduledEventResponse = z.object({
	id: SnowflakeStringType.describe('The ID of the event'),
	guild_id: SnowflakeStringType.describe('The ID of the guild'),
	channel_id: SnowflakeStringType.nullable().describe('The ID of the channel'),
	creator_id: SnowflakeStringType.describe('The ID of the user who created the event'),
	name: z.string().describe('The name of the event'),
	description: z.string().nullable().describe('The description of the event'),
	scheduled_start_time: z.string().describe('ISO 8601 timestamp for when the event starts'),
	scheduled_end_time: z.string().nullable().describe('ISO 8601 timestamp for when the event ends'),
	privacy_level: z.number().describe('The privacy level of the event'),
	status: z.number().describe('The status of the event'),
	entity_type: z.number().describe('The entity type of the event'),
	entity_id: SnowflakeStringType.nullable().describe('The entity ID of the event'),
	location: z.string().nullable().describe('The location of the event'),
	image: z.string().nullable().describe('The image hash of the event'),
	created_at: z.string().describe('When the event was created'),
	updated_at: z.string().describe('When the event was last updated'),
	version: z.number().describe('The version of the event'),
	subscriber_count: z.number().describe('The number of subscribers'),
});

export type GuildScheduledEventResponse = z.infer<typeof GuildScheduledEventResponse>;
