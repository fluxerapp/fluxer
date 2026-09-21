// SPDX-License-Identifier: AGPL-3.0-or-later

import {IsoTimestampStringType} from '@fluxer/schema/src/primitives/DateValidators';
import {SnowflakeStringType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

const EventNameType = z.string().trim().min(1).max(100);
const EventDescriptionType = z.string().trim().max(2000);
const EventLocationType = z.string().trim().max(200);
const EventImageType = z.string().min(1);

export const GuildEventIdParam = z.object({
	guild_id: SnowflakeStringType,
	event_id: SnowflakeStringType,
});

export const GuildEventCreateRequest = z
	.object({
		name: EventNameType,
		description: EventDescriptionType.nullish(),
		location: EventLocationType.nullish(),
		starts_at: IsoTimestampStringType,
		ends_at: IsoTimestampStringType.nullish(),
		image: EventImageType.nullish(),
	})
	.superRefine((value, ctx) => {
		if (value.ends_at && Date.parse(value.ends_at) <= Date.parse(value.starts_at)) {
			ctx.addIssue({code: 'custom', path: ['ends_at'], message: 'Event end must be after its start'});
		}
	});

export const GuildEventUpdateRequest = z
	.object({
		name: EventNameType.optional(),
		description: EventDescriptionType.nullish(),
		location: EventLocationType.nullish(),
		starts_at: IsoTimestampStringType.optional(),
		ends_at: IsoTimestampStringType.nullish(),
		image: EventImageType.nullish(),
	})
	.superRefine((value, ctx) => {
		if (value.starts_at && value.ends_at && Date.parse(value.ends_at) <= Date.parse(value.starts_at)) {
			ctx.addIssue({code: 'custom', path: ['ends_at'], message: 'Event end must be after its start'});
		}
	});

export const GuildEventResponse = z.object({
	id: SnowflakeStringType,
	guild_id: SnowflakeStringType,
	creator_id: SnowflakeStringType,
	name: z.string(),
	description: z.string().nullable(),
	location: z.string().nullable(),
	starts_at: IsoTimestampStringType,
	ends_at: IsoTimestampStringType.nullable(),
	image_url: z.string().nullable(),
	created_at: IsoTimestampStringType,
});

export const GuildEventListResponse = z.array(GuildEventResponse);

export type GuildEventCreate = z.infer<typeof GuildEventCreateRequest>;
export type GuildEventUpdate = z.infer<typeof GuildEventUpdateRequest>;
export type GuildEvent = z.infer<typeof GuildEventResponse>;
