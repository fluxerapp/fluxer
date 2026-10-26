// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	CUSTOM_BADGE_DESCRIPTION_MAX_LENGTH,
	CUSTOM_BADGE_NAME_MAX_LENGTH,
	CustomBadgeTypes,
	MAX_CUSTOM_BADGES_PER_ENTITY,
} from '@fluxer/constants/src/BadgeConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {
	createNamedStringLiteralUnion,
	createStringType,
	SnowflakeStringType,
	SnowflakeType,
	withOpenApiType,
} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {URLType} from '@fluxer/schema/src/primitives/UrlValidators';
import {z} from 'zod';

export const CustomBadgeTypeSchema = withOpenApiType(
	createNamedStringLiteralUnion(
		[
			[CustomBadgeTypes.USER, 'User', 'Shown on the profiles of the users it is assigned to'],
			[CustomBadgeTypes.GUILD, 'Guild', 'Shown next to the names of the communities it is assigned to'],
		],
		'Where the badge is shown',
	),
	'CustomBadgeType',
);

const CustomBadgeHrefType = URLType.refine(
	(value) => value.startsWith('https://'),
	ValidationErrorCodes.INVALID_URL_FORMAT,
).describe('Absolute https URL opened when the badge is clicked');

const CustomBadgeImageType = z
	.string()
	.max(16_000_000)
	.describe('The badge image as a base64 data URL. PNG, JPEG, GIF, WebP and SVG images are accepted');

export const AdminCustomBadgeResponse = z.object({
	id: SnowflakeStringType.describe('The ID of the badge'),
	type: CustomBadgeTypeSchema,
	name: z.string().describe('The name of the badge'),
	description: z.string().nullable().describe('Text shown under the name when hovering over the badge, or null'),
	asset_url: z.string().describe('URL of the badge image, served as WebP by the media proxy'),
	href: z.string().nullable().describe('URL opened when the badge is clicked, or null'),
});

export type AdminCustomBadgeResponse = z.infer<typeof AdminCustomBadgeResponse>;

export const AdminCustomBadgeListResponse = z.object({
	badges: z.array(AdminCustomBadgeResponse).describe('Badges defined on this instance, in display order'),
});

export type AdminCustomBadgeListResponse = z.infer<typeof AdminCustomBadgeListResponse>;

export const CreateCustomBadgeRequest = z.object({
	type: CustomBadgeTypeSchema,
	name: createStringType(1, CUSTOM_BADGE_NAME_MAX_LENGTH).describe('The name of the badge'),
	description: createStringType(1, CUSTOM_BADGE_DESCRIPTION_MAX_LENGTH)
		.nullish()
		.describe('Text shown when hovering over the badge'),
	href: CustomBadgeHrefType.nullish(),
	image: CustomBadgeImageType,
});

export type CreateCustomBadgeRequest = z.infer<typeof CreateCustomBadgeRequest>;

export const UpdateCustomBadgeRequest = z.object({
	name: createStringType(1, CUSTOM_BADGE_NAME_MAX_LENGTH).describe('The name of the badge'),
	description: createStringType(1, CUSTOM_BADGE_DESCRIPTION_MAX_LENGTH)
		.nullable()
		.describe('Text shown under the name when hovering over the badge, or null'),
	href: CustomBadgeHrefType.nullable(),
	image: CustomBadgeImageType.optional().describe(
		'A new image as a base64 data URL. Leave it out to keep the current one',
	),
});

export type UpdateCustomBadgeRequest = z.infer<typeof UpdateCustomBadgeRequest>;

export const CustomBadgeIdParam = z.object({
	badge_id: SnowflakeType.describe('The ID of the badge'),
});

export const CustomBadgeIdsType = z
	.array(SnowflakeType)
	.max(MAX_CUSTOM_BADGES_PER_ENTITY)
	.describe('IDs of the badges to assign, replacing the current ones');

export const UpdateUserCustomBadgesRequest = z.object({
	badge_ids: CustomBadgeIdsType,
});

export type UpdateUserCustomBadgesRequest = z.infer<typeof UpdateUserCustomBadgesRequest>;

export const CustomBadgeIdListResponse = z
	.array(SnowflakeStringType)
	.max(MAX_CUSTOM_BADGES_PER_ENTITY)
	.describe('IDs of the custom badges assigned to it');
