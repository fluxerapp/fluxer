// SPDX-License-Identifier: AGPL-3.0-or-later

import {BadgeTypes, BuiltinBadges, MAX_BADGES_PER_ENTITY} from '@fluxer/constants/src/BadgeConstants';
import {
	createNamedStringLiteralUnion,
	SnowflakeStringType,
	withOpenApiType,
} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

export const BadgeTypeEnum = withOpenApiType(
	createNamedStringLiteralUnion(
		[
			[BadgeTypes.USER, 'User', 'Badge shown on user profiles'],
			[BadgeTypes.GUILD, 'Guild', 'Badge shown next to community names'],
		],
		'The target entity type of the badge',
	),
	'BadgeType',
);

export const BuiltinBadgeEnum = withOpenApiType(
	createNamedStringLiteralUnion(
		[
			[BuiltinBadges.STAFF, 'Staff', 'Staff member badge'],
			[BuiltinBadges.PREMIUM, 'Premium', 'Premium subscriber badge'],
			[BuiltinBadges.VERIFIED, 'Verified', 'Verified community badge'],
			[BuiltinBadges.PARTNERED, 'Partnered', 'Partnered community badge'],
			[BuiltinBadges.DISCOVERABLE, 'Discoverable', 'Discoverable community badge'],
		],
		'Type of built-in icon override',
	),
	'BuiltinBadge',
);

export const BadgeResponse = z.object({
	id: SnowflakeStringType.describe('The ID of the badge'),
	type: BadgeTypeEnum,
	name: z.string().describe('The name of the badge'),
	tooltip: z.string().describe('The text shown when hovering over the badge'),
	icon: z.string().describe('SVG markup of the badge icon'),
	url: z.string().nullable().describe('The URL opened when the badge is clicked'),
	position: z.number().int().describe('Display order of the badge'),
});

export type BadgeResponse = z.infer<typeof BadgeResponse>;

export const BuiltinBadgeIconsResponse = z
	.object({
		staff: z.string().nullable(),
		premium: z.string().nullable(),
		verified: z.string().nullable(),
		partnered: z.string().nullable(),
		discoverable: z.string().nullable(),
	})
	.describe('SVG markup to override the built-in badge with, or null to reset to default');

export type BuiltinBadgeIconsResponse = z.infer<typeof BuiltinBadgeIconsResponse>;

export const BadgesResponse = z.object({
	version: z.number().int().describe('Version of the badge configuration'),
	badges: z.array(BadgeResponse).describe('Badges defined on this instance'),
	builtin_icons: BuiltinBadgeIconsResponse,
});

export type BadgesResponse = z.infer<typeof BadgesResponse>;

export const BadgeIdsResponse = z
	.array(SnowflakeStringType)
	.max(MAX_BADGES_PER_ENTITY)
	.describe('IDs of the badges assigned to the entity');

export const BadgesUpdateDispatchData = z.object({
	version: BadgesResponse.shape.version,
	badges: z.array(BadgeResponse).optional().describe('Badges that were created or updated'),
	deleted_badge_ids: z.array(SnowflakeStringType).optional().describe('IDs of badges that were deleted'),
	builtin_icons: BuiltinBadgeIconsResponse.partial().optional().describe('Built-in badge icons that are overridden'),
});

export type BadgesUpdateDispatchData = z.infer<typeof BadgesUpdateDispatchData>;

export const UserBadgesUpdateDispatchData = z.object({
	user_id: SnowflakeStringType.describe('The ID of the member whose badges changed'),
	badges: z.array(SnowflakeStringType).describe('IDs of the badges now assigned to the member'),
});

export type UserBadgesUpdateDispatchData = z.infer<typeof UserBadgesUpdateDispatchData>;
