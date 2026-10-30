// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	BUILT_IN_BADGE_KINDS,
	CUSTOM_BADGE_DESCRIPTION_MAX_LENGTH,
	CUSTOM_BADGE_LOOKUP_IDS_MAX,
	CUSTOM_BADGE_NAME_MAX_LENGTH,
	CUSTOM_BADGE_PAGE_LIMIT_DEFAULT,
	CUSTOM_BADGE_PAGE_LIMIT_MAX,
	CUSTOM_BADGE_REORDER_IDS_MAX,
	CUSTOM_BADGES_PER_GUILD_CEILING,
	CUSTOM_BADGES_PER_USER_CEILING,
	CustomBadgeTypes,
} from '@fluxer/constants/src/BadgeConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {createQueryIntegerType} from '@fluxer/schema/src/primitives/QueryValidators';
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

export const CustomBadgePositionType = z
	.string()
	.regex(/^-?[0-9]{1,19}$/)
	.describe('Display position of the badge. Badges are shown in ascending order of this value');

export const CustomBadgeCursorType = z
	.string()
	.regex(/^[gu]-?[0-9]{1,19}$/)
	.describe('Opaque cursor from the next field of a previous page');

export const AdminCustomBadgeResponse = z.object({
	id: SnowflakeStringType.describe('The ID of the badge'),
	type: CustomBadgeTypeSchema,
	position: CustomBadgePositionType,
	name: z.string().describe('The name of the badge'),
	description: z.string().nullable().describe('Text shown under the name when hovering over the badge, or null'),
	asset_url: z.string().describe('URL of the badge image, served as WebP by the media proxy'),
	href: z.string().nullable().describe('URL opened when the badge is clicked, or null'),
});

export type AdminCustomBadgeResponse = z.infer<typeof AdminCustomBadgeResponse>;

export const BuiltInBadgeKindSchema = withOpenApiType(
	z
		.enum(BUILT_IN_BADGE_KINDS)
		.describe(
			'A built-in badge whose image can be replaced: the staff or premium profile badge, or a community badge starting with guild_',
		),
	'BuiltInBadgeKind',
);

export const AdminBuiltInBadgeImageResponse = z.object({
	kind: BuiltInBadgeKindSchema,
	asset_url: z
		.string()
		.nullable()
		.describe('URL of the image shown in place of the built-in artwork, or null when the built-in artwork is used'),
	built_in_asset: z
		.string()
		.nullable()
		.describe(
			'Hash of the built-in artwork at {static_cdn}/badges/{hash}.webp, or null for a guild badge, which clients draw themselves',
		),
});

export type AdminBuiltInBadgeImageResponse = z.infer<typeof AdminBuiltInBadgeImageResponse>;

export const AdminCustomBadgeSettingsResponse = z.object({
	max_per_user: z
		.number()
		.int()
		.min(1)
		.max(CUSTOM_BADGES_PER_USER_CEILING)
		.describe('Most custom badges one user can be given'),
	max_per_guild: z
		.number()
		.int()
		.min(1)
		.max(CUSTOM_BADGES_PER_GUILD_CEILING)
		.describe('Most custom badges one community can be given'),
});

export type AdminCustomBadgeSettingsResponse = z.infer<typeof AdminCustomBadgeSettingsResponse>;

export const UpdateCustomBadgeSettingsRequest = AdminCustomBadgeSettingsResponse.partial();

export type UpdateCustomBadgeSettingsRequest = z.infer<typeof UpdateCustomBadgeSettingsRequest>;

export const AdminCustomBadgeListQuery = z.object({
	type: CustomBadgeTypeSchema.optional().describe(
		'Only list badges of this type. Without it the list holds every user badge, then every guild badge',
	),
	query: createStringType(1, 64)
		.optional()
		.describe('Only list badges with this ID, or with this text anywhere in the name, ignoring case'),
	ids: z
		.string()
		.regex(new RegExp(`^[0-9]{1,20}(?:,[0-9]{1,20}){0,${CUSTOM_BADGE_LOOKUP_IDS_MAX - 1}}$`))
		.optional()
		.describe(
			`Up to ${CUSTOM_BADGE_LOOKUP_IDS_MAX} badge IDs separated by commas. Only these badges are listed, and the page has no next. Cannot be sent with query or after`,
		),
	after: CustomBadgeCursorType.optional(),
	limit: createQueryIntegerType({
		defaultValue: CUSTOM_BADGE_PAGE_LIMIT_DEFAULT,
		minValue: 1,
		maxValue: CUSTOM_BADGE_PAGE_LIMIT_MAX,
	}).describe(`Most badges to return, from 1 to ${CUSTOM_BADGE_PAGE_LIMIT_MAX}`),
});

export type AdminCustomBadgeListQuery = z.infer<typeof AdminCustomBadgeListQuery>;

export const AdminCustomBadgeListResponse = z.object({
	badges: z.array(AdminCustomBadgeResponse).describe('One page of badges, in display order'),
	next: CustomBadgeCursorType.nullable().describe(
		'Pass this as after to get the next page, or null when there are no more badges to look at. A search page can hold fewer badges than the limit and still have a next page',
	),
	version: z
		.string()
		.describe('Version of the badge catalog. It changes whenever a badge is created, edited, deleted or moved'),
	settings: AdminCustomBadgeSettingsResponse,
	built_in: z
		.array(AdminBuiltInBadgeImageResponse)
		.max(BUILT_IN_BADGE_KINDS.length)
		.describe('Every built-in badge and the image this instance shows for it'),
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
	.max(CUSTOM_BADGES_PER_USER_CEILING)
	.describe(
		'IDs of the badges to assign, replacing the current ones. The badge settings set how many a user or community can have',
	);

export const UpdateUserCustomBadgesRequest = z.object({
	badge_ids: CustomBadgeIdsType,
});

export type UpdateUserCustomBadgesRequest = z.infer<typeof UpdateUserCustomBadgesRequest>;

export const ReorderCustomBadgesRequest = z.object({
	badge_ids: z
		.array(SnowflakeType)
		.max(CUSTOM_BADGE_REORDER_IDS_MAX)
		.describe(
			`IDs of every custom badge on this instance, user and guild, in the new display order. Instances with more than ${CUSTOM_BADGE_REORDER_IDS_MAX} badges move badges one at a time`,
		),
});

export type ReorderCustomBadgesRequest = z.infer<typeof ReorderCustomBadgesRequest>;

export const CustomBadgeMoveDirectionSchema = withOpenApiType(
	createNamedStringLiteralUnion(
		[
			['up', 'Up', 'Swap places with the badge shown before it'],
			['down', 'Down', 'Swap places with the badge shown after it'],
			['first', 'First', 'Show it before every other badge of its type'],
			['last', 'Last', 'Show it after every other badge of its type'],
		],
		'Where to move the badge',
	),
	'CustomBadgeMoveDirection',
);

export const MoveCustomBadgeRequest = z.object({
	badge_id: SnowflakeType.describe('The ID of the badge to move'),
	direction: CustomBadgeMoveDirectionSchema.optional(),
	after_id: SnowflakeType.nullable()
		.optional()
		.describe(
			'Show the badge right after this badge of the same type, or first when null. Send either this or direction',
		),
});

export type MoveCustomBadgeRequest = z.infer<typeof MoveCustomBadgeRequest>;

export const CustomBadgeIdListResponse = z
	.array(SnowflakeStringType)
	.max(CUSTOM_BADGES_PER_USER_CEILING)
	.describe('IDs of the custom badges assigned to it');

export const BuiltInBadgeKindParam = z.object({
	kind: BuiltInBadgeKindSchema,
});

export const ReplaceBuiltInBadgeImageRequest = z.object({
	image: CustomBadgeImageType,
});

export type ReplaceBuiltInBadgeImageRequest = z.infer<typeof ReplaceBuiltInBadgeImageRequest>;
