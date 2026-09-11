// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	VOICE_CHANNEL_BITRATE_DEFAULT,
	VOICE_CHANNEL_BITRATE_MAX_128_KBPS,
	VOICE_CHANNEL_BITRATE_MAX_256_KBPS,
	VOICE_CHANNEL_BITRATE_MAX_384_KBPS,
	VOICE_CHANNEL_BITRATE_MAX_STANDARD,
	VOICE_CHANNEL_BITRATE_MIN,
} from '@fluxer/constants/src/LimitConstants';
import type {ValueOf} from '@fluxer/constants/src/ValueOf';

export const GuildVerificationLevel = {
	NONE: 0,
	LOW: 1,
	MEDIUM: 2,
	HIGH: 3,
	VERY_HIGH: 4,
} as const;

export type GuildVerificationLevelValue = ValueOf<typeof GuildVerificationLevel>;

export function getEffectiveGuildVerificationLevel(verificationLevel: number, isDiscoverable: boolean): number {
	if (!isDiscoverable) {
		return verificationLevel;
	}
	return Math.max(verificationLevel, GuildVerificationLevel.LOW);
}

export const GuildMFALevel = {
	NONE: 0,
	ELEVATED: 1,
} as const;

export type GuildMFALevelValue = ValueOf<typeof GuildMFALevel>;

export const GuildSplashCardAlignment = {
	CENTER: 0,
	LEFT: 1,
	RIGHT: 2,
} as const;

export type GuildSplashCardAlignmentValue = ValueOf<typeof GuildSplashCardAlignment>;

export const SystemChannelFlags = {
	SUPPRESS_JOIN_NOTIFICATIONS: 1 << 0,
} as const;
export const SystemChannelFlagsDescriptions: Record<keyof typeof SystemChannelFlags, string> = {
	SUPPRESS_JOIN_NOTIFICATIONS: 'Suppress member join notifications in system channel',
};
export const GuildOperations = {
	PUSH_NOTIFICATIONS: 1 << 0,
	EVERYONE_MENTIONS: 1 << 1,
	TYPING_EVENTS: 1 << 2,
	INSTANT_INVITES: 1 << 3,
	SEND_MESSAGE: 1 << 4,
	REACTIONS: 1 << 5,
	MEMBER_LIST_UPDATES: 1 << 6,
} as const;
export const GuildOperationsDescriptions: Record<keyof typeof GuildOperations, string> = {
	PUSH_NOTIFICATIONS: 'Allow push notifications for this guild',
	EVERYONE_MENTIONS: 'Allow @everyone mentions in this guild',
	TYPING_EVENTS: 'Enable typing indicator events',
	INSTANT_INVITES: 'Allow creation of instant invites',
	SEND_MESSAGE: 'Allow sending messages in the guild',
	REACTIONS: 'Allow adding reactions to messages',
	MEMBER_LIST_UPDATES: 'Enable member list update events',
};
export const GuildMemberProfileFlags = {
	AVATAR_UNSET: 1 << 0,
	BANNER_UNSET: 1 << 1,
} as const;
export const GuildMemberProfileFlagsDescriptions: Record<keyof typeof GuildMemberProfileFlags, string> = {
	AVATAR_UNSET: 'Guild member avatar is unset',
	BANNER_UNSET: 'Guild member banner is unset',
};
export const GuildExplicitContentFilterTypes = {
	DISABLED: 0,
	MEMBERS_WITHOUT_ROLES: 1,
	ALL_MEMBERS: 2,
} as const;

export type GuildExplicitContentFilterType = ValueOf<typeof GuildExplicitContentFilterTypes>;

export const GuildNSFWLevel = {
	SAFE: 0,
	AGE_RESTRICTED: 3,
} as const;

export type GuildNSFWLevelValue = ValueOf<typeof GuildNSFWLevel>;
const LEGACY_EXPLICIT_NSFW_LEVEL = 1;
const LEGACY_DEDICATED_SAFE_NSFW_LEVEL = 2;

export function normalizeLegacyNsfwLevel(value: number): number {
	if (value === LEGACY_EXPLICIT_NSFW_LEVEL) return GuildNSFWLevel.AGE_RESTRICTED;
	if (value === LEGACY_DEDICATED_SAFE_NSFW_LEVEL) return GuildNSFWLevel.SAFE;
	return value;
}

export const ContentWarningLevel = {
	INHERIT: 0,
	CONTENT_WARNING: 1,
} as const;
export const CONTENT_WARNING_TEXT_MAX_LENGTH = 200;
export const GuildFeatures = {
	ANIMATED_ICON: 'ANIMATED_ICON',
	ANIMATED_BANNER: 'ANIMATED_BANNER',
	AUDIO_BITRATE_128_KBPS: 'AUDIO_BITRATE_128_KBPS',
	AUDIO_BITRATE_256_KBPS: 'AUDIO_BITRATE_256_KBPS',
	AUDIO_BITRATE_384_KBPS: 'AUDIO_BITRATE_384_KBPS',
	BANNER: 'BANNER',
	CLONE_EMOJI_DISABLED: 'CLONE_EMOJI_DISABLED',
	CLONE_EMOJI_ENABLED: 'CLONE_EMOJI_ENABLED',
	CLONE_STICKER_DISABLED: 'CLONE_STICKER_DISABLED',
	CLONE_STICKER_ENABLED: 'CLONE_STICKER_ENABLED',
	DETACHED_BANNER: 'DETACHED_BANNER',
	INVITE_SPLASH: 'INVITE_SPLASH',
	INVITES_DISABLED: 'INVITES_DISABLED',
	RAID_DETECTED: 'RAID_DETECTED',
	TEXT_CHANNEL_FLEXIBLE_NAMES: 'TEXT_CHANNEL_FLEXIBLE_NAMES',
	HIDE_OWNER_CROWN: 'HIDE_OWNER_CROWN',
	MORE_EMOJI: 'MORE_EMOJI',
	MORE_STICKERS: 'MORE_STICKERS',
	UNLIMITED_EMOJI: 'UNLIMITED_EMOJI',
	UNLIMITED_STICKERS: 'UNLIMITED_STICKERS',
	EXPRESSION_PURGE_ALLOWED: 'EXPRESSION_PURGE_ALLOWED',
	VANITY_URL: 'VANITY_URL',
	DISCOVERABLE: 'DISCOVERABLE',
	PARTNERED: 'PARTNERED',
	VERIFIED: 'VERIFIED',
	VIP_VOICE: 'VIP_VOICE',
	VOICE_E2EE: 'VOICE_E2EE',
	UNAVAILABLE_FOR_EVERYONE: 'UNAVAILABLE_FOR_EVERYONE',
	UNAVAILABLE_FOR_EVERYONE_BUT_STAFF: 'UNAVAILABLE_FOR_EVERYONE_BUT_STAFF',
	UNAVAILABLE_HIDDEN: 'UNAVAILABLE_HIDDEN',
	VISIONARY: 'VISIONARY',
	LARGE_GUILD_OVERRIDE: 'LARGE_GUILD_OVERRIDE',
	VERY_LARGE_GUILD: 'VERY_LARGE_GUILD',
} as const;

export type GuildFeature = ValueOf<typeof GuildFeatures>;

export function getMaxVoiceChannelBitrate(features: Iterable<string> | null | undefined): number {
	if (features == null) {
		return VOICE_CHANNEL_BITRATE_MAX_STANDARD;
	}
	let maximum = VOICE_CHANNEL_BITRATE_MAX_STANDARD;
	for (const feature of features) {
		if (feature === GuildFeatures.AUDIO_BITRATE_384_KBPS) {
			maximum = Math.max(maximum, VOICE_CHANNEL_BITRATE_MAX_384_KBPS);
		} else if (feature === GuildFeatures.AUDIO_BITRATE_256_KBPS) {
			maximum = Math.max(maximum, VOICE_CHANNEL_BITRATE_MAX_256_KBPS);
		} else if (feature === GuildFeatures.AUDIO_BITRATE_128_KBPS) {
			maximum = Math.max(maximum, VOICE_CHANNEL_BITRATE_MAX_128_KBPS);
		}
	}
	return maximum;
}

export function clampVoiceChannelBitrate(bitrate: number, features: Iterable<string> | null | undefined): number {
	return Math.min(Math.max(bitrate, VOICE_CHANNEL_BITRATE_MIN), getMaxVoiceChannelBitrate(features));
}

export function resolveVoiceChannelBitrate(
	bitrate: number | null | undefined,
	features: Iterable<string> | null | undefined,
): number {
	if (typeof bitrate !== 'number' || !Number.isFinite(bitrate) || bitrate <= 0) {
		return VOICE_CHANNEL_BITRATE_DEFAULT;
	}
	return clampVoiceChannelBitrate(Math.round(bitrate), features);
}

export const JoinSourceTypes = {
	CREATOR: 0,
	INSTANT_INVITE: 1,
	VANITY_URL: 2,
	BOT_INVITE: 3,
	ADMIN_FORCE_ADD: 4,
	DISCOVERY: 6,
} as const;

export type JoinSourceType = ValueOf<typeof JoinSourceTypes>;

export const GUILD_MEMBERS_REINDEX_AFTER_TIMESTAMP = 1779557400;
