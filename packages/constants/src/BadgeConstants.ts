// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ValueOf} from '@fluxer/constants/src/ValueOf';

export const CustomBadgeTypes = {
	USER: 'user',
	GUILD: 'guild',
} as const;

export type CustomBadgeType = ValueOf<typeof CustomBadgeTypes>;

export const CUSTOM_BADGES_PER_USER_CEILING = 1000;
export const CUSTOM_BADGES_PER_GUILD_CEILING = 200;
export const DEFAULT_CUSTOM_BADGES_PER_USER = 100;
export const DEFAULT_CUSTOM_BADGES_PER_GUILD = 50;
export const DISCOVERY_GUILD_BADGE_LIMIT = 100;
export const CUSTOM_BADGE_LOOKUP_IDS_MAX = 100;
export const CUSTOM_BADGE_PAGE_LIMIT_MAX = 200;
export const CUSTOM_BADGE_PAGE_LIMIT_DEFAULT = 100;
export const CUSTOM_BADGE_REORDER_IDS_MAX = 1000;
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

export const MAX_HIDDEN_PROFILE_BADGES = PROFILE_BADGE_KINDS.length + CUSTOM_BADGES_PER_USER_CEILING;

export const BUILT_IN_GUILD_BADGE_KINDS = ['verified', 'partnered', 'discoverable'] as const;

export type BuiltInGuildBadgeKind = (typeof BUILT_IN_GUILD_BADGE_KINDS)[number];

const REPLACEABLE_PROFILE_BADGE_KINDS = ['staff', 'premium'] as const satisfies ReadonlyArray<ProfileBadgeKind>;

export type ReplaceableProfileBadgeKind = (typeof REPLACEABLE_PROFILE_BADGE_KINDS)[number];

export type BuiltInBadgeKind = ReplaceableProfileBadgeKind | `guild_${BuiltInGuildBadgeKind}`;

export const BUILT_IN_BADGE_KINDS = [
	...REPLACEABLE_PROFILE_BADGE_KINDS,
	'guild_verified',
	'guild_partnered',
	'guild_discoverable',
] as const satisfies ReadonlyArray<BuiltInBadgeKind>;

export const HIDEABLE_BUILT_IN_BADGE_KINDS = [
	'guild_verified',
	'guild_partnered',
	'guild_discoverable',
] as const satisfies ReadonlyArray<BuiltInBadgeKind>;

export type HideableBuiltInBadgeKind = (typeof HIDEABLE_BUILT_IN_BADGE_KINDS)[number];

export function isHideableBuiltInBadgeKind(kind: BuiltInBadgeKind): kind is HideableBuiltInBadgeKind {
	return (HIDEABLE_BUILT_IN_BADGE_KINDS as ReadonlyArray<string>).includes(kind);
}

export function isReplaceableProfileBadgeKind(kind: ProfileBadgeKind): kind is ReplaceableProfileBadgeKind {
	return (REPLACEABLE_PROFILE_BADGE_KINDS as ReadonlyArray<string>).includes(kind);
}

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
