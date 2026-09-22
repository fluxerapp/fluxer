// SPDX-License-Identifier: AGPL-3.0-or-later

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
	DELETED: 1n << 34n,
	DISABLED_SUSPICIOUS_ACTIVITY: 1n << 35n,
	SELF_DELETED: 1n << 36n,
	DISABLED: 1n << 38n,
	HAS_SESSION_STARTED: 1n << 39n,
	RATE_LIMIT_BYPASS: 1n << 47n,
	REPORT_BANNED: 1n << 48n,
	VERIFIED_NOT_UNDERAGE: 1n << 49n,
	HAS_DISMISSED_PREMIUM_ONBOARDING: 1n << 51n,
	APP_STORE_REVIEWER: 1n << 53n,
	STAFF_HIDDEN: 1n << 57n,
	AGE_VERIFIED_ADULT: 1n << 60n,
	FORCE_INBOUND_PHONE_VERIFICATION: 1n << 61n,
	NOT_SUSPICIOUS: 1n << 62n,
} as const;
export const UserFlagsDescriptions: Record<keyof typeof UserFlags, string> = {
	STAFF: 'User is a staff member',
	PARTNER: 'User is a partner',
	BUG_HUNTER: 'User is a bug hunter',
	HIGH_GLOBAL_RATE_LIMIT: 'User has elevated global rate limits',
	FRIENDLY_BOT: 'Bot accepts friend requests from users',
	FRIENDLY_BOT_MANUAL_APPROVAL: 'Bot requires manual approval for friend requests',
	SPAMMER: 'User is flagged as a spammer',
	DELETED: 'User account has been deleted',
	DISABLED_SUSPICIOUS_ACTIVITY: 'User account disabled due to suspicious activity',
	SELF_DELETED: 'User account was self-deleted',
	DISABLED: 'User account is disabled',
	HAS_SESSION_STARTED: 'User has started a session',
	RATE_LIMIT_BYPASS: 'User can bypass rate limits',
	REPORT_BANNED: 'User is banned from reporting',
	VERIFIED_NOT_UNDERAGE: 'User is verified as not underage',
	HAS_DISMISSED_PREMIUM_ONBOARDING: 'User has dismissed premium onboarding',
	APP_STORE_REVIEWER: 'User is an app store reviewer',
	STAFF_HIDDEN: 'User staff status is hidden from public flags',
	AGE_VERIFIED_ADULT: 'User has verified their age as an adult via credit card verification',
	FORCE_INBOUND_PHONE_VERIFICATION:
		'User is forced through inbound (expensive-destination) phone verification regardless of phone prefix, for debugging',
	NOT_SUSPICIOUS:
		'User is permanently exempt from automatic suspicious-activity flagging on RPC session start (does not require a prior payment)',
};
export const PremiumFlags = {
	DISCRIMINATOR: 1 << 0,
	BADGE_HIDDEN: 1 << 1,
	BADGE_MASKED: 1 << 2,
	BADGE_TIMESTAMP_HIDDEN: 1 << 3,
	BADGE_SEQUENCE_HIDDEN: 1 << 4,
	PERKS_SANITIZED: 1 << 5,
	PURCHASE_DISABLED: 1 << 6,
	ENABLED_OVERRIDE: 1 << 7,
	PERKS_DISABLED: 1 << 8,
} as const;
export const PremiumFlagsDescriptions: Record<keyof typeof PremiumFlags, string> = {
	DISCRIMINATOR: 'User has a premium discriminator',
	BADGE_HIDDEN: 'User has hidden their premium badge',
	BADGE_MASKED: 'User has masked their premium badge',
	BADGE_TIMESTAMP_HIDDEN: 'User has hidden their premium badge timestamp',
	BADGE_SEQUENCE_HIDDEN: 'User has hidden their premium badge sequence',
	PERKS_SANITIZED: 'User premium perks are sanitized',
	PURCHASE_DISABLED: 'Premium purchase is disabled for this user',
	ENABLED_OVERRIDE: 'Premium status is enabled via override',
	PERKS_DISABLED: 'User has temporarily disabled premium perks',
};
const LEGACY_PREMIUM_FLAG_BITS_TO_NEW: ReadonlyArray<readonly [bigint, number]> = [
	[1n << 37n, PremiumFlags.DISCRIMINATOR],
	[1n << 40n, PremiumFlags.BADGE_HIDDEN],
	[1n << 41n, PremiumFlags.BADGE_MASKED],
	[1n << 42n, PremiumFlags.BADGE_TIMESTAMP_HIDDEN],
	[1n << 43n, PremiumFlags.BADGE_SEQUENCE_HIDDEN],
	[1n << 44n, PremiumFlags.PERKS_SANITIZED],
	[1n << 45n, PremiumFlags.PURCHASE_DISABLED],
	[1n << 46n, PremiumFlags.ENABLED_OVERRIDE],
];
export const LEGACY_PREMIUM_FLAGS_MASK: bigint = LEGACY_PREMIUM_FLAG_BITS_TO_NEW.reduce(
	(mask, [legacy]) => mask | legacy,
	0n,
);
export const LEGACY_DEAD_USER_FLAGS_MASK: bigint = (1n << 52n) | (1n << 54n) | (1n << 55n) | (1n << 56n) | (1n << 58n);

export function extractPremiumFlagsFromLegacyUserFlags(legacyFlags: bigint): number {
	let result = 0;
	for (const [legacy, modern] of LEGACY_PREMIUM_FLAG_BITS_TO_NEW) {
		if ((legacyFlags & legacy) !== 0n) {
			result |= modern;
		}
	}
	return result;
}

export const PUBLIC_USER_FLAGS =
	UserFlags.STAFF |
	UserFlags.PARTNER |
	UserFlags.BUG_HUNTER |
	UserFlags.FRIENDLY_BOT |
	UserFlags.FRIENDLY_BOT_MANUAL_APPROVAL |
	UserFlags.SPAMMER;
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
export const SuspiciousActivityFlags = {
	REQUIRE_VERIFIED_EMAIL: 1 << 0,
	REQUIRE_REVERIFIED_EMAIL: 1 << 1,
	REQUIRE_VERIFIED_PHONE: 1 << 2,
	REQUIRE_REVERIFIED_PHONE: 1 << 3,
	REQUIRE_VERIFIED_EMAIL_OR_VERIFIED_PHONE: 1 << 4,
	REQUIRE_REVERIFIED_EMAIL_OR_VERIFIED_PHONE: 1 << 5,
	REQUIRE_VERIFIED_EMAIL_OR_REVERIFIED_PHONE: 1 << 6,
	REQUIRE_REVERIFIED_EMAIL_OR_REVERIFIED_PHONE: 1 << 7,
	REQUIRE_INBOUND_PHONE_VERIFICATION: 1 << 8,
} as const;
export const SuspiciousActivityFlagsDescriptions: Record<keyof typeof SuspiciousActivityFlags, string> = {
	REQUIRE_VERIFIED_EMAIL: 'Requires verified email address',
	REQUIRE_REVERIFIED_EMAIL: 'Requires re-verified email address',
	REQUIRE_VERIFIED_PHONE: 'Requires verified phone number',
	REQUIRE_REVERIFIED_PHONE: 'Requires re-verified phone number',
	REQUIRE_VERIFIED_EMAIL_OR_VERIFIED_PHONE: 'Requires verified email or verified phone',
	REQUIRE_REVERIFIED_EMAIL_OR_VERIFIED_PHONE: 'Requires re-verified email or re-verified phone',
	REQUIRE_VERIFIED_EMAIL_OR_REVERIFIED_PHONE: 'Requires verified email or re-verified phone',
	REQUIRE_REVERIFIED_EMAIL_OR_REVERIFIED_PHONE: 'Requires re-verified email or re-verified phone',
	REQUIRE_INBOUND_PHONE_VERIFICATION: 'Requires inbound SMS verification (user must text code to platform number)',
};
export const ALL_SUSPICIOUS_ACTIVITY_FLAGS = Object.values(SuspiciousActivityFlags).reduce(
	(mask, flag) => mask | flag,
	0,
);
export const DEFERRED_PHONE_ON_COMMUNITY_JOIN = 1 << 16;
export const PHONE_GATE_PROMOTED_FROM_DEFERRAL = 1 << 17;
export const DEFERRABLE_PHONE_FLAGS =
	SuspiciousActivityFlags.REQUIRE_VERIFIED_PHONE | SuspiciousActivityFlags.REQUIRE_REVERIFIED_PHONE;
export const NEVER_DEFERRABLE_PHONE_FLAGS = SuspiciousActivityFlags.REQUIRE_INBOUND_PHONE_VERIFICATION;
export const PHONE_REQUIREMENT_FLAGS = DEFERRABLE_PHONE_FLAGS | NEVER_DEFERRABLE_PHONE_FLAGS;
export function imposePhoneRequirements(currentFlags: number, addedFlags: number): number {
	const nextFlags = currentFlags | addedFlags;
	if ((addedFlags & DEFERRABLE_PHONE_FLAGS) === 0) {
		return nextFlags;
	}
	return nextFlags & ~DEFERRED_PHONE_ON_COMMUNITY_JOIN & ~PHONE_GATE_PROMOTED_FROM_DEFERRAL;
}
export const ADMIN_PHONE_TOGGLE_CLEARABLE_FLAGS =
	DEFERRED_PHONE_ON_COMMUNITY_JOIN |
	PHONE_GATE_PROMOTED_FROM_DEFERRAL |
	SuspiciousActivityFlags.REQUIRE_VERIFIED_PHONE |
	SuspiciousActivityFlags.REQUIRE_INBOUND_PHONE_VERIFICATION;
export const PHONE_ADD_CLEARABLE_FLAGS =
	DEFERRED_PHONE_ON_COMMUNITY_JOIN |
	PHONE_GATE_PROMOTED_FROM_DEFERRAL |
	SuspiciousActivityFlags.REQUIRE_VERIFIED_PHONE |
	SuspiciousActivityFlags.REQUIRE_REVERIFIED_PHONE |
	SuspiciousActivityFlags.REQUIRE_VERIFIED_EMAIL_OR_VERIFIED_PHONE |
	SuspiciousActivityFlags.REQUIRE_REVERIFIED_EMAIL_OR_VERIFIED_PHONE |
	SuspiciousActivityFlags.REQUIRE_VERIFIED_EMAIL_OR_REVERIFIED_PHONE |
	SuspiciousActivityFlags.REQUIRE_REVERIFIED_EMAIL_OR_REVERIFIED_PHONE |
	SuspiciousActivityFlags.REQUIRE_INBOUND_PHONE_VERIFICATION;
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
