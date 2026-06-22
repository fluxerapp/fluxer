// SPDX-License-Identifier: AGPL-3.0-or-later

import {SnowflakeStringType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

export const ScheduledEventEntityTypeSchema = z.enum(['STAGE_INSTANCE', 'VOICE', 'EXTERNAL']);
export type ScheduledEventEntityType = z.infer<typeof ScheduledEventEntityTypeSchema>;

export const ScheduledEventStatusSchema = z.enum(['SCHEDULED', 'ACTIVE', 'COMPLETED', 'CANCELLED']);
export type ScheduledEventStatus = z.infer<typeof ScheduledEventStatusSchema>;

export const ScheduledEventPrivacyLevelSchema = z.enum(['GUILD_ONLY']);
export type ScheduledEventPrivacyLevel = z.infer<typeof ScheduledEventPrivacyLevelSchema>;

export const GuildScheduledEventEntityMetadata = z.object({
	location: z.string().nullish().describe('Location string for EXTERNAL entity type events'),
});

export type GuildScheduledEventEntityMetadata = z.infer<typeof GuildScheduledEventEntityMetadata>;

export const GuildScheduledEventResponse = z.object({
	id: SnowflakeStringType.describe('The unique identifier for this scheduled event'),
	guild_id: SnowflakeStringType.describe('The guild this event belongs to'),
	channel_id: SnowflakeStringType.nullish().describe('The voice or stage channel where the event takes place'),
	creator_id: SnowflakeStringType.describe('The ID of the user who created this event'),
	name: z.string().describe('The name of the scheduled event'),
	description: z.string().nullish().describe('The description of the scheduled event'),
	image: z.string().nullish().describe('URL of the event cover image'),
	scheduled_start_time: z.string().describe('ISO8601 timestamp of when the event is scheduled to start'),
	scheduled_end_time: z.string().nullish().describe('ISO8601 timestamp of when the event is scheduled to end'),
	privacy_level: ScheduledEventPrivacyLevelSchema.describe('The privacy level for this event'),
	status: ScheduledEventStatusSchema.describe('The status of the scheduled event'),
	entity_type: ScheduledEventEntityTypeSchema.describe('The type of hosting entity associated with this event'),
	entity_id: SnowflakeStringType.nullish().describe('The ID of an entity associated with the scheduled event'),
	entity_metadata: GuildScheduledEventEntityMetadata.nullish().describe(
		'Additional metadata about the scheduled event entity',
	),
	user_count: z.number().int().nonnegative().describe('The number of users subscribed to this event'),
});

export type GuildScheduledEventResponse = z.infer<typeof GuildScheduledEventResponse>;

export interface GuildScheduledEvent {
	readonly id: string;
	readonly guild_id: string;
	readonly channel_id?: string | null;
	readonly creator_id: string;
	readonly name: string;
	readonly description?: string | null;
	readonly image?: string | null;
	readonly scheduled_start_time: string;
	readonly scheduled_end_time?: string | null;
	readonly privacy_level: string;
	readonly status: string;
	readonly entity_type: string;
	readonly entity_id?: string | null;
	readonly entity_metadata?: {location?: string | null} | null;
	readonly user_count: number;
}
