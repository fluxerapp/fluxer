// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ValueOf} from '@fluxer/constants/src/ValueOf';

export const BadgeTypes = {
	USER: 'user',
	GUILD: 'guild',
} as const;

export type BadgeType = ValueOf<typeof BadgeTypes>;

export const BuiltinBadges = {
	STAFF: 'staff',
	PREMIUM: 'premium',
	VERIFIED: 'verified',
	PARTNERED: 'partnered',
	DISCOVERABLE: 'discoverable',
} as const;

export type BuiltinBadge = ValueOf<typeof BuiltinBadges>;

export const MAX_BADGES_PER_TYPE = 10;
export const MAX_BADGES_PER_ENTITY = 10;
export const MAX_BADGE_NAME_LENGTH = 64;
export const MAX_BADGE_TOOLTIP_LENGTH = 256;
export const MAX_BADGE_ICON_INPUT_LENGTH = 25 * 1024;
