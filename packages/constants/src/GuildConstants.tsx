/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {ValueOf} from '@fluxer/constants/src/ValueOf';

export const GuildVerificationLevel = {
	NONE: 0,
	LOW: 1,
	MEDIUM: 2,
	HIGH: 3,
	VERY_HIGH: 4,
} as const;

export const GuildMFALevel = {
	NONE: 0,
	ELEVATED: 1,
} as const;

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

export const GuildNSFWLevel = {
	DEFAULT: 0,
	EXPLICIT: 1,
	SAFE: 2,
	AGE_RESTRICTED: 3,
} as const;

export const GuildNSFWLevelDescriptions: Record<keyof typeof GuildNSFWLevel, string> = {
	DEFAULT: 'Default NSFW level',
	EXPLICIT: 'Guild contains explicit content',
	SAFE: 'Guild is safe for all ages',
	AGE_RESTRICTED: 'Guild is age-restricted',
};

export const GuildFeatures = {
	ANIMATED_ICON: 'ANIMATED_ICON',
	ANIMATED_BANNER: 'ANIMATED_BANNER',
	BANNER: 'BANNER',
	DETACHED_BANNER: 'DETACHED_BANNER',
	INVITE_SPLASH: 'INVITE_SPLASH',
	INVITES_DISABLED: 'INVITES_DISABLED',
	TEXT_CHANNEL_FLEXIBLE_NAMES: 'TEXT_CHANNEL_FLEXIBLE_NAMES',
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
	UNAVAILABLE_FOR_EVERYONE: 'UNAVAILABLE_FOR_EVERYONE',
	UNAVAILABLE_FOR_EVERYONE_BUT_STAFF: 'UNAVAILABLE_FOR_EVERYONE_BUT_STAFF',
	VISIONARY: 'VISIONARY',
	OPERATOR: 'OPERATOR',
	LARGE_GUILD_OVERRIDE: 'LARGE_GUILD_OVERRIDE',
	VERY_LARGE_GUILD: 'VERY_LARGE_GUILD',
} as const;

export type GuildFeature = ValueOf<typeof GuildFeatures>;

export const JoinSourceTypes = {
	CREATOR: 0,
	INSTANT_INVITE: 1,
	VANITY_URL: 2,
	BOT_INVITE: 3,
	ADMIN_FORCE_ADD: 4,
} as const;

export const DEFAULT_RULE_COUNT = 0;
export const MAX_RULE_COUNT = 100;

export const MemberSortType = {
	JOIN_DATE_DESC: 1,
	JOIN_DATE_ASC: 2,
} as const;

export type MemberSortTypeValue = ValueOf<typeof MemberSortType>;

export const GUILD_MEMBERS_REINDEX_AFTER_TIMESTAMP = 1769813072;
