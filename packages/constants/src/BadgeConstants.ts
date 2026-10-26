// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ValueOf} from '@fluxer/constants/src/ValueOf';

export const CustomBadgeTypes = {
	USER: 'user',
	GUILD: 'guild',
} as const;

export type CustomBadgeType = ValueOf<typeof CustomBadgeTypes>;

export const MAX_CUSTOM_BADGES_PER_TYPE = 10;
export const MAX_CUSTOM_BADGES_PER_ENTITY = 10;
export const CUSTOM_BADGE_NAME_MAX_LENGTH = 64;
export const CUSTOM_BADGE_DESCRIPTION_MAX_LENGTH = 256;
const PROFILE_BADGE_KINDS = [
	'staff',
	'partner',
	'bug_hunter',
	'operator_pass',
	'visionary',
	'visionary_number',
	'plutonium',
	'premium',
] as const;

export type ProfileBadgeKind = (typeof PROFILE_BADGE_KINDS)[number];

export type ProfileBadgeKey = ProfileBadgeKind | `custom:${string}`;

export const MAX_HIDDEN_PROFILE_BADGES = PROFILE_BADGE_KINDS.length + MAX_CUSTOM_BADGES_PER_TYPE;

export const PREMIUM_PROFILE_BADGE_KINDS: ReadonlyArray<ProfileBadgeKind> = ['plutonium', 'visionary', 'premium'];

const CUSTOM_PROFILE_BADGE_KEY_PREFIX = 'custom:';

export function customProfileBadgeKey(badgeId: string): ProfileBadgeKey {
	return `${CUSTOM_PROFILE_BADGE_KEY_PREFIX}${badgeId}`;
}

export function customBadgeIdFromProfileBadgeKey(key: string): string | null {
	if (!key.startsWith(CUSTOM_PROFILE_BADGE_KEY_PREFIX)) return null;
	const id = key.slice(CUSTOM_PROFILE_BADGE_KEY_PREFIX.length);
	return /^[0-9]{1,20}$/.test(id) ? id : null;
}

export function isProfileBadgeKey(key: string): key is ProfileBadgeKey {
	return (PROFILE_BADGE_KINDS as ReadonlyArray<string>).includes(key) || customBadgeIdFromProfileBadgeKey(key) !== null;
}
