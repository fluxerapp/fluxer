// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	MAX_BADGE_ICON_INPUT_LENGTH,
	MAX_BADGE_NAME_LENGTH,
	MAX_BADGE_TOOLTIP_LENGTH,
	MAX_BADGES_PER_ENTITY,
	MAX_BADGES_PER_TYPE,
} from '@fluxer/constants/src/BadgeConstants';
import {BadgeResponse, BadgeTypeEnum, BuiltinBadgeEnum} from '@fluxer/schema/src/domains/badge/BadgeSchemas';
import {createStringType, SnowflakeType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {URLType} from '@fluxer/schema/src/primitives/UrlValidators';
import {z} from 'zod';

const BadgeIconType = z
	.string()
	.min(1)
	.max(MAX_BADGE_ICON_INPUT_LENGTH)
	.describe('SVG markup of the badge icon');
const BadgePositionType = z.number().int().min(0).max(MAX_BADGES_PER_TYPE);

export const CreateBadgeRequest = z.object({
	type: BadgeTypeEnum,
	name: createStringType(1, MAX_BADGE_NAME_LENGTH).describe('The name of the badge'),
	tooltip: createStringType(1, MAX_BADGE_TOOLTIP_LENGTH).describe('The text shown when hovering over the badge'),
	icon: BadgeIconType,
	url: URLType.nullish().describe('The URL opened when the badge is clicked'),
	position: BadgePositionType.optional().describe('The display order of the badge'),
});

export type CreateBadgeRequest = z.infer<typeof CreateBadgeRequest>;

export const UpdateBadgeRequest = z.object({
	name: createStringType(1, MAX_BADGE_NAME_LENGTH).optional().describe('The name of the badge'),
	tooltip: createStringType(1, MAX_BADGE_TOOLTIP_LENGTH).optional().describe('The text shown when hovering over the badge'),
	icon: BadgeIconType.optional(),
	url: URLType.nullish().describe('The URL opened when the badge is clicked, or null to remove the URL'),
	position: BadgePositionType.optional().describe('The display order of the badge'),
});

export type UpdateBadgeRequest = z.infer<typeof UpdateBadgeRequest>;

export const BadgeIdParam = z.object({
	badge_id: SnowflakeType.describe('The ID of the badge'),
});

export const BuiltinBadgeParam = z.object({
	badge: BuiltinBadgeEnum,
});

export const UpdateBuiltinBadgeIconRequest = z.object({
	icon: BadgeIconType.nullable().describe('SVG markup of the badge icon, or null to restore the default icon'),
});

export type UpdateBuiltinBadgeIconRequest = z.infer<typeof UpdateBuiltinBadgeIconRequest>;

export const BadgeAssignmentRequest = z.object({
	badge_ids: z
		.array(SnowflakeType)
		.max(MAX_BADGES_PER_ENTITY)
		.describe('IDs of the badges to write to the account record'),
});

export type BadgeAssignmentRequest = z.infer<typeof BadgeAssignmentRequest>;

export const BadgeMutationResponse = z.object({
	badge: BadgeResponse,
});

export type BadgeMutationResponse = z.infer<typeof BadgeMutationResponse>;
