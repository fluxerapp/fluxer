// SPDX-License-Identifier: AGPL-3.0-or-later

import {GuildVisitSources} from '@fluxer/constants/src/GuildVisitConstants';
import {SnowflakeStringType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

export const GuildVisitSourceSchema = z
	.union([
		z.literal(GuildVisitSources.INVITE),
		z.literal(GuildVisitSources.VANITY_URL),
		z.literal(GuildVisitSources.DISCOVERY),
		z.literal(GuildVisitSources.OPERATOR),
	])
	.describe('How the visit started: 1 invite, 2 custom invite URL, 3 discovery, 4 the instance community');

export const GuildVisitResponse = z.object({
	guild_id: SnowflakeStringType.describe('ID of the guild being visited'),
	source: GuildVisitSourceSchema,
	started_at: z.iso.datetime().describe('When the visit started'),
	opened_at: z.iso.datetime().describe('When the visit was last opened, updated at most once a day'),
});

export type GuildVisitResponse = z.infer<typeof GuildVisitResponse>;

export const GuildVisitListResponse = z.array(GuildVisitResponse).describe('Guilds the current user is visiting');

export type GuildVisitListResponse = z.infer<typeof GuildVisitListResponse>;

export const GuildVisitStartResponse = z.object({
	guild_id: SnowflakeStringType.describe('ID of the guild'),
	member: z.boolean().describe('True when the current user is already a member, in which case no visit starts'),
	visit: GuildVisitResponse.nullable().describe('The visit, or null when the current user is already a member'),
});

export type GuildVisitStartResponse = z.infer<typeof GuildVisitStartResponse>;
