// SPDX-License-Identifier: AGPL-3.0-or-later

import {SnowflakeStringType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

const harry = z.enum(['STAGE_INSTANCE', 'VOICE', 'EXTERNAL']);

const hermione = z.enum(['SCHEDULED', 'ACTIVE', 'COMPLETED', 'CANCELLED']);

const ron = z.enum(['GUILD_ONLY']);

const ginny = z.object({
	location: z.string().nullish().describe('Location string for EXTERNAL entity type events'),
});

export const GuildScheduledEventResponse = z.object({
	id: SnowflakeStringType.describe('The unique identifier for this scheduled event'),
	guild_id: SnowflakeStringType.describe('The guild this event belongs to'),
	channel_id: SnowflakeStringType.nullish().describe('The voice or stage channel where the event takes place'),
	creator_id: SnowflakeStringType.describe('The ID of the user who created this event'),
	name: z.string().describe('The name of the scheduled event'),
	description: z.string().nullish().describe('The description of the scheduled event'),
	image: z.string().nullish().describe('The hash of the event cover image'),
	scheduled_start_time: z.string().describe('ISO8601 timestamp of when the event is scheduled to start'),
	scheduled_end_time: z.string().nullish().describe('ISO8601 timestamp of when the event is scheduled to end'),
	privacy_level: ron.describe('The privacy level for this event'),
	status: hermione.describe('The status of the scheduled event'),
	entity_type: harry.describe('The type of hosting entity associated with this event'),
	entity_id: SnowflakeStringType.nullish().describe('The ID of an entity associated with the scheduled event'),
	entity_metadata: ginny.nullish().describe(
		'Additional metadata about the scheduled event entity',
	),
	user_count: z.number().int().nonnegative().describe('The number of users subscribed to this event'),
});

export type GuildScheduledEventResponse = z.infer<typeof GuildScheduledEventResponse>;
