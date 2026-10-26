// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	PREMIUM_PROFILE_BADGE_KINDS,
	type ProfileBadgeKey,
	type ProfileBadgeKind,
} from '@fluxer/constants/src/BadgeConstants';
import type {ValueOf} from '@fluxer/constants/src/ValueOf';

export const UNCATEGORIZED_FOLDER_ID = -1;
export const GuildFolderFlags = {
	SHOW_ICON_WHEN_COLLAPSED: 1 << 0,
} as const;
export const GuildFolderFlagsDescriptions: Record<keyof typeof GuildFolderFlags, string> = {
	SHOW_ICON_WHEN_COLLAPSED: 'Show the selected icon instead of guild previews when the folder is collapsed',
};
export const GuildFolderIcons = {
	FOLDER: 'folder',
	STAR: 'star',
	HEART: 'heart',
	BOOKMARK: 'bookmark',
	GAME_CONTROLLER: 'game_controller',
	SHIELD: 'shield',
	MUSIC_NOTE: 'music_note',
} as const;

export type GuildFolderIcon = ValueOf<typeof GuildFolderIcons>;

export const DEFAULT_GUILD_FOLDER_ICON: GuildFolderIcon = GuildFolderIcons.FOLDER;
export const UserAuthenticatorTypes = {
	TOTP: 0,
	WEBAUTHN: 2,
} as const;

export type UserAuthenticatorType = ValueOf<typeof UserAuthenticatorTypes>;

export const UserAuthenticatorTypesDescriptions: Record<keyof typeof UserAuthenticatorTypes, string> = {
	TOTP: 'Time-based one-time password authenticator',
	WEBAUTHN: 'WebAuthn authenticator',
};
export const UserPremiumTypes = {
	NONE: 0,
	SUBSCRIPTION: 1,
	LIFETIME: 2,
} as const;

export type UserPremiumType = ValueOf<typeof UserPremiumTypes>;

export const PREMIUM_GRACE_PERIOD_DAYS = 3;
export const PREMIUM_PAYMENT_RECOVERY_GRACE_DAYS = {monthly: 7, yearly: 14} as const;

export const UserPremiumTypesDescriptions: Record<keyof typeof UserPremiumTypes, string> = {
	NONE: 'No premium subscription',
	SUBSCRIPTION: 'Active premium subscription',
	LIFETIME: 'Lifetime premium subscription',
};
export const UserFlags = {
	STAFF: 1n << 0n,
	PARTNER: 1n << 2n,
	BUG_HUNTER: 1n << 3n,
	HIGH_GLOBAL_RATE_LIMIT: 1n << 33n,
	FRIENDLY_BOT: 1n << 4n,
	FRIENDLY_BOT_MANUAL_APPROVAL: 1n << 5n,
	SPAMMER: 1n << 6n,
	PROFILE_HIDDEN: 1n << 7n,
	DELETED: 1n << 34n,
	SELF_DELETED: 1n << 36n,
	DISABLED: 1n << 38n,
	HAS_SESSION_STARTED: 1n << 39n,
	RATE_LIMIT_BYPASS: 1n << 47n,
	REPORT_BANNED: 1n << 48n,
	VERIFIED_NOT_UNDERAGE: 1n << 49n,
	ACCOUNT_LIMITED: 1n << 50n,
	HAS_DISMISSED_PREMIUM_ONBOARDING: 1n << 51n,
	APP_STORE_REVIEWER: 1n << 53n,
	AGE_VERIFIED_ADULT: 1n << 60n,
	LIMIT_EXEMPT: 1n << 62n,
} as const;
export const UserFlagsDescriptions: Record<keyof typeof UserFlags, string> = {
	STAFF: 'User is a staff member',
	PARTNER: 'User is a partner',
	BUG_HUNTER: 'User is a bug hunter',
	HIGH_GLOBAL_RATE_LIMIT: 'User has elevated global rate limits',
	FRIENDLY_BOT: 'Bot accepts friend requests from users',
	FRIENDLY_BOT_MANUAL_APPROVAL: 'Bot requires manual approval for friend requests',
	SPAMMER: 'User is flagged as a spammer',
	PROFILE_HIDDEN: 'User profile details are hidden from other users',
	DELETED: 'User account has been deleted',
	SELF_DELETED: 'User account was self-deleted',
	DISABLED: 'User account is disabled',
	HAS_SESSION_STARTED: 'User has started a session',
	RATE_LIMIT_BYPASS: 'User can bypass rate limits',
	REPORT_BANNED: 'User is banned from reporting',
	VERIFIED_NOT_UNDERAGE: 'User is verified as not underage',
	ACCOUNT_LIMITED: 'User account is limited',
	HAS_DISMISSED_PREMIUM_ONBOARDING: 'User has dismissed premium onboarding',
	APP_STORE_REVIEWER: 'User is an app store reviewer',
	AGE_VERIFIED_ADULT: 'User has verified their age as an adult',
	LIMIT_EXEMPT: 'User is permanently exempt from account limitation',
};
export const PremiumFlags = {
	DISCRIMINATOR: 1 << 0,
	BADGE_MASKED: 1 << 2,
	BADGE_TIMESTAMP_HIDDEN: 1 << 3,
	PERKS_SANITIZED: 1 << 5,
	PURCHASE_DISABLED: 1 << 6,
	ENABLED_OVERRIDE: 1 << 7,
	PERKS_DISABLED: 1 << 8,
	OPERATOR_PASS_BADGE_TIMESTAMP_HIDDEN: 1 << 11,
	OPERATOR_PASS_BADGE_COUNT_HIDDEN: 1 << 12,
} as const;
export const PremiumFlagsDescriptions: Record<keyof typeof PremiumFlags, string> = {
	DISCRIMINATOR: 'User has a premium discriminator',
	BADGE_MASKED: 'User has masked their premium badge',
	BADGE_TIMESTAMP_HIDDEN: 'User has hidden their premium badge timestamp',
	PERKS_SANITIZED: 'User premium perks are sanitized',
	PURCHASE_DISABLED: 'Premium purchase is disabled for this user',
	ENABLED_OVERRIDE: 'Premium status is enabled via override',
	PERKS_DISABLED: 'User has temporarily disabled premium perks',
	OPERATOR_PASS_BADGE_TIMESTAMP_HIDDEN: 'User has hidden when they first got the Operator Pass',
	OPERATOR_PASS_BADGE_COUNT_HIDDEN: 'User has hidden how many Operator Passes they hold',
};
const LEGACY_PREMIUM_BADGE_HIDDEN = 1 << 1;
const LEGACY_PREMIUM_BADGE_SEQUENCE_HIDDEN = 1 << 4;
const LEGACY_OPERATOR_PASS_BADGE_HIDDEN = 1 << 9;
const LEGACY_STAFF_HIDDEN = 1n << 57n;
const LEGACY_PREMIUM_FLAG_BITS_TO_NEW: ReadonlyArray<readonly [bigint, number]> = [
	[1n << 37n, PremiumFlags.DISCRIMINATOR],
	[1n << 40n, LEGACY_PREMIUM_BADGE_HIDDEN],
	[1n << 41n, PremiumFlags.BADGE_MASKED],
	[1n << 42n, PremiumFlags.BADGE_TIMESTAMP_HIDDEN],
	[1n << 43n, LEGACY_PREMIUM_BADGE_SEQUENCE_HIDDEN],
	[1n << 44n, PremiumFlags.PERKS_SANITIZED],
	[1n << 45n, PremiumFlags.PURCHASE_DISABLED],
	[1n << 46n, PremiumFlags.ENABLED_OVERRIDE],
];
export const LEGACY_PREMIUM_FLAGS_MASK: bigint = LEGACY_PREMIUM_FLAG_BITS_TO_NEW.reduce(
	(mask, [legacy]) => mask | legacy,
	0n,
);
export const LEGACY_DEAD_USER_FLAGS_MASK: bigint =
	(1n << 35n) | (1n << 52n) | (1n << 54n) | (1n << 55n) | (1n << 56n) | (1n << 58n) | (1n << 61n);

export function extractPremiumFlagsFromLegacyUserFlags(legacyFlags: bigint): number {
	let result = 0;
	for (const [legacy, modern] of LEGACY_PREMIUM_FLAG_BITS_TO_NEW) {
		if ((legacyFlags & legacy) !== 0n) {
			result |= modern;
		}
	}
	return result;
}

const LEGACY_PREMIUM_BADGE_HIDE_BITS: ReadonlyArray<readonly [number, ReadonlyArray<ProfileBadgeKind>]> = [
	[LEGACY_PREMIUM_BADGE_HIDDEN, PREMIUM_PROFILE_BADGE_KINDS],
	[LEGACY_PREMIUM_BADGE_SEQUENCE_HIDDEN, ['visionary_number']],
	[LEGACY_OPERATOR_PASS_BADGE_HIDDEN, ['operator_pass']],
];
export const LEGACY_BADGE_HIDE_PREMIUM_FLAGS_MASK: number =
	LEGACY_PREMIUM_BADGE_HIDDEN | LEGACY_PREMIUM_BADGE_SEQUENCE_HIDDEN | LEGACY_OPERATOR_PASS_BADGE_HIDDEN;
export const LEGACY_BADGE_HIDE_USER_FLAGS_MASK: bigint = LEGACY_STAFF_HIDDEN;

export function legacyHiddenProfileBadges(flags: bigint, premiumFlags: number): Array<ProfileBadgeKind> {
	const hidden = new Set<ProfileBadgeKind>();
	if ((flags & LEGACY_STAFF_HIDDEN) !== 0n) hidden.add('staff');
	const folded = premiumFlags | extractPremiumFlagsFromLegacyUserFlags(flags);
	for (const [bit, kinds] of LEGACY_PREMIUM_BADGE_HIDE_BITS) {
		if ((folded & bit) === 0) continue;
		for (const kind of kinds) hidden.add(kind);
	}
	return [...hidden];
}

export function clearLegacyBadgeHideBits(flags: bigint, premiumFlags: number): {flags: bigint; premiumFlags: number} {
	return {
		flags: flags & ~LEGACY_PREMIUM_FLAGS_MASK & ~LEGACY_DEAD_USER_FLAGS_MASK & ~LEGACY_BADGE_HIDE_USER_FLAGS_MASK,
		premiumFlags:
			(premiumFlags | extractPremiumFlagsFromLegacyUserFlags(flags)) & ~LEGACY_BADGE_HIDE_PREMIUM_FLAGS_MASK,
	};
}

export const PUBLIC_USER_FLAGS =
	UserFlags.STAFF |
	UserFlags.PARTNER |
	UserFlags.BUG_HUNTER |
	UserFlags.FRIENDLY_BOT |
	UserFlags.FRIENDLY_BOT_MANUAL_APPROVAL |
	UserFlags.SPAMMER;
const HIDEABLE_FLAG_BADGES: ReadonlyArray<readonly [ProfileBadgeKind, bigint]> = [
	['staff', UserFlags.STAFF],
	['partner', UserFlags.PARTNER],
	['bug_hunter', UserFlags.BUG_HUNTER],
];

export function stripHiddenProfileBadgeFlags(flags: bigint, hiddenProfileBadges: ReadonlySet<ProfileBadgeKey>): bigint {
	let visible = flags;
	for (const [kind, flag] of HIDEABLE_FLAG_BADGES) {
		if (hiddenProfileBadges.has(kind)) visible &= ~flag;
	}
	return visible;
}
export const DELETED_USER_USERNAME = 'DeletedUser';
export const DELETED_USER_GLOBAL_NAME = 'Deleted User';
export const DELETED_USER_DISCRIMINATOR = 0;
export const DELETED_USER_ID = 1n;
export const PublicUserFlags = {
	STAFF: Number(UserFlags.STAFF),
	PARTNER: Number(UserFlags.PARTNER),
	BUG_HUNTER: Number(UserFlags.BUG_HUNTER),
	FRIENDLY_BOT: Number(UserFlags.FRIENDLY_BOT),
	FRIENDLY_BOT_MANUAL_APPROVAL: Number(UserFlags.FRIENDLY_BOT_MANUAL_APPROVAL),
	SPAMMER: Number(UserFlags.SPAMMER),
} as const;
export const PublicUserFlagsDescriptions: Record<keyof typeof PublicUserFlags, string> = {
	STAFF: 'User is a staff member',
	PARTNER: 'User is a partner',
	BUG_HUNTER: 'User is a bug hunter',
	FRIENDLY_BOT: 'Bot accepts friend requests from users',
	FRIENDLY_BOT_MANUAL_APPROVAL: 'Bot requires manual approval for friend requests',
	SPAMMER: 'User is flagged as a spammer',
};
export const ThemeTypes = {
	DARK: 'dark',
	DARK_LEGACY: 'dark_legacy',
	COAL: 'coal',
	LIGHT: 'light',
	SYSTEM: 'system',
} as const;

export type ThemeType = ValueOf<typeof ThemeTypes>;

export const TimeFormatTypes = {
	AUTO: 0,
	TWELVE_HOUR: 1,
	TWENTY_FOUR_HOUR: 2,
} as const;

export type TimeFormatType = ValueOf<typeof TimeFormatTypes>;

export const TimeFormatTypesDescriptions: Record<keyof typeof TimeFormatTypes, string> = {
	AUTO: 'Automatically detect time format based on locale',
	TWELVE_HOUR: 'Use 12-hour time format (AM/PM)',
	TWENTY_FOUR_HOUR: 'Use 24-hour time format',
};
export const StickerAnimationOptions = {
	ALWAYS_ANIMATE: 0,
	ANIMATE_ON_INTERACTION: 1,
	NEVER_ANIMATE: 2,
} as const;

export type StickerAnimationOption = ValueOf<typeof StickerAnimationOptions>;

export const StickerAnimationOptionsDescriptions: Record<keyof typeof StickerAnimationOptions, string> = {
	ALWAYS_ANIMATE: 'Always animate stickers',
	ANIMATE_ON_INTERACTION: 'Animate stickers on hover/interaction',
	NEVER_ANIMATE: 'Never animate stickers',
};
export const RenderSpoilers = {
	ALWAYS: 0,
	ON_CLICK: 1,
	IF_MODERATOR: 2,
} as const;

export type RenderSpoilersValue = ValueOf<typeof RenderSpoilers>;

export const RenderSpoilersDescriptions: Record<keyof typeof RenderSpoilers, string> = {
	ALWAYS: 'Always reveal spoiler content',
	ON_CLICK: 'Reveal spoiler content on click',
	IF_MODERATOR: 'Reveal spoiler content if moderator',
};
export const UserExplicitContentFilterTypes = {
	DISABLED: 0,
	NON_FRIENDS: 1,
	FRIENDS_AND_NON_FRIENDS: 2,
} as const;
export const SensitiveMediaFilterLevel = {
	SHOW: 0,
	BLUR: 1,
	BLOCK: 2,
} as const;

export type SensitiveMediaFilterLevelValue = ValueOf<typeof SensitiveMediaFilterLevel>;

export const SensitiveMediaFilterLevelDescriptions: Record<keyof typeof SensitiveMediaFilterLevel, string> = {
	SHOW: 'Show sensitive media without any filter',
	BLUR: 'Blur sensitive media until manually revealed',
	BLOCK: 'Completely hide sensitive media',
};
export const FriendSourceFlags = {
	MUTUAL_FRIENDS: 1 << 0,
	MUTUAL_GUILDS: 1 << 1,
	NO_RELATION: 1 << 2,
} as const;
export const FriendSourceFlagsDescriptions: Record<keyof typeof FriendSourceFlags, string> = {
	MUTUAL_FRIENDS: 'Allow friend requests from users who share mutual friends',
	MUTUAL_GUILDS: 'Allow friend requests from users in mutual guilds',
	NO_RELATION: 'Allow friend requests from users with no existing relation',
};
export const IncomingCallFlags = {
	FRIENDS_OF_FRIENDS: 1 << 0,
	GUILD_MEMBERS: 1 << 1,
	EVERYONE: 1 << 2,
	FRIENDS_ONLY: 1 << 3,
	NOBODY: 1 << 4,
	SILENT_EVERYONE: 1 << 5,
} as const;
export const IncomingCallFlagsDescriptions: Record<keyof typeof IncomingCallFlags, string> = {
	FRIENDS_OF_FRIENDS: 'Allow incoming calls from friends of friends',
	GUILD_MEMBERS: 'Allow incoming calls from guild members',
	EVERYONE: 'Allow incoming calls from everyone',
	FRIENDS_ONLY: 'Allow incoming calls only from friends',
	NOBODY: 'Block all incoming calls',
	SILENT_EVERYONE: 'Allow calls from everyone but receive them silently',
};
export const GroupDmAddPermissionFlags = {
	FRIENDS_OF_FRIENDS: 1 << 0,
	GUILD_MEMBERS: 1 << 1,
	EVERYONE: 1 << 2,
	FRIENDS_ONLY: 1 << 3,
	NOBODY: 1 << 4,
} as const;
export const GroupDmAddPermissionFlagsDescriptions: Record<keyof typeof GroupDmAddPermissionFlags, string> = {
	FRIENDS_OF_FRIENDS: 'Allow friends of friends to add user to group DMs',
	GUILD_MEMBERS: 'Allow guild members to add user to group DMs',
	EVERYONE: 'Allow everyone to add user to group DMs',
	FRIENDS_ONLY: 'Allow only friends to add user to group DMs',
	NOBODY: 'Block everyone from adding user to group DMs',
};
export const ProfilePrivacyLevels = {
	ALL_GUILDS: 0,
	SMALL_GUILDS_ONLY: 1,
	FRIENDS_ONLY: 2,
} as const;

export type ProfilePrivacyLevel = (typeof ProfilePrivacyLevels)[keyof typeof ProfilePrivacyLevels];

export const ProfilePrivacyLevelsDescriptions: Record<keyof typeof ProfilePrivacyLevels, string> = {
	ALL_GUILDS: 'Profile visible to friends and members of any shared guild',
	SMALL_GUILDS_ONLY: 'Profile visible to friends and members of shared guilds with at most 200 members',
	FRIENDS_ONLY: 'Profile visible only to friends',
};
export const ProfileFieldPrivacyFlags = {
	EVERYONE: 1 << 0,
	FRIENDS: 1 << 1,
	MUTUAL_GUILDS: 1 << 2,
} as const;

export const ProfileFieldPrivacyFlagsDescriptions: Record<keyof typeof ProfileFieldPrivacyFlags, string> = {
	EVERYONE: 'Allow anyone who can view the full profile to see this profile field',
	FRIENDS: 'Allow friends to see this profile field',
	MUTUAL_GUILDS: 'Allow members from mutual guilds to see this profile field',
};
export const SMALL_GUILD_MEMBER_THRESHOLD = 200;
export const VOICE_ACTIVITY_SHARING_COOLDOWN_MS = 24 * 60 * 60 * 1000;
export const UserNotificationSettings = {
	ALL_MESSAGES: 0,
	ONLY_MENTIONS: 1,
	NO_MESSAGES: 2,
	INHERIT: 3,
} as const;

export const UserNotificationSettingsDescriptions: Record<keyof typeof UserNotificationSettings, string> = {
	ALL_MESSAGES: 'Receive notifications for all messages',
	ONLY_MENTIONS: 'Only receive notifications for mentions',
	NO_MESSAGES: 'Do not receive any notifications',
	INHERIT: 'Inherit notification settings from parent',
};
export const MentionReplyPreferences = {
	NO_PREFERENCE: 0,
	PREFER_MENTION: 1,
	PREFER_NO_MENTION: 2,
} as const;

export type MentionReplyPreference = ValueOf<typeof MentionReplyPreferences>;

export const MentionReplyPreferencesDescriptions: Record<keyof typeof MentionReplyPreferences, string> = {
	NO_PREFERENCE:
		'Respect the sender intent on each reply, with no warning when toggling the @ mention. On a guild member, inherits the value from the user-level preference.',
	PREFER_MENTION: 'Default replies to @mention this user, and warn the sender when they disable the mention',
	PREFER_NO_MENTION: 'Default replies to omit the @mention for this user, and warn the sender when they enable it',
};
export const RelationshipTypes = {
	FRIEND: 1,
	BLOCKED: 2,
	INCOMING_REQUEST: 3,
	OUTGOING_REQUEST: 4,
} as const;

export type RelationshipType = ValueOf<typeof RelationshipTypes>;

export const RelationshipTypesDescriptions: Record<keyof typeof RelationshipTypes, string> = {
	FRIEND: 'User is a friend',
	BLOCKED: 'User is blocked',
	INCOMING_REQUEST: 'Pending incoming friend request',
	OUTGOING_REQUEST: 'Pending outgoing friend request',
};
