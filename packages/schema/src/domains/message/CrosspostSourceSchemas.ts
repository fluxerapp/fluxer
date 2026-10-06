// SPDX-License-Identifier: AGPL-3.0-or-later

import {GuildFeatureSchema} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {Int32Type, SnowflakeStringType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

export const CrosspostSourceGuildResponse = z.object({
	id: SnowflakeStringType.describe('The ID of the source community'),
	name: z.string().describe('The name of the source community'),
	icon: z.string().nullish().describe('The hash of the source community icon'),
	banner: z.string().nullish().describe('The hash of the source community banner'),
	description: z
		.string()
		.nullable()
		.describe('The discovery description of the source community, or null when it is not listed in discovery'),
	features: z
		.array(GuildFeatureSchema)
		.describe('The public badge features of the source community: VERIFIED, PARTNERED and DISCOVERABLE only'),
	approximate_member_count: Int32Type.nullable().describe(
		'Approximate number of members in the source community, or null when unavailable',
	),
	approximate_presence_count: Int32Type.nullable().describe(
		'Approximate number of online members in the source community, or null when unavailable',
	),
	discoverable: z.boolean().describe('Whether the source community can be joined through discovery'),
});
export type CrosspostSourceGuildResponse = z.infer<typeof CrosspostSourceGuildResponse>;

export const CrosspostSourceResponse = z.object({
	guild: CrosspostSourceGuildResponse.describe('The community the message was published from'),
});
export type CrosspostSourceResponse = z.infer<typeof CrosspostSourceResponse>;
