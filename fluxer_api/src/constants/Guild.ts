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

export const SystemChannelFlags = {
	SUPPRESS_JOIN_NOTIFICATIONS: 1 << 0,
} as const;

export const GuildOperations = {
	PUSH_NOTIFICATIONS: 1 << 0,
	EVERYONE_MENTIONS: 1 << 1,
	TYPING_EVENTS: 1 << 2,
	INSTANT_INVITES: 1 << 3,
	SEND_MESSAGE: 1 << 4,
	REACTIONS: 1 << 5,
} as const;

export const GuildExplicitContentFilterTypes = {
	DISABLED: 0,
	MEMBERS_WITHOUT_ROLES: 1,
	ALL_MEMBERS: 2,
} as const;

export const GuildFeatures = {
	ANIMATED_ICON: 'ANIMATED_ICON',
	ANIMATED_BANNER: 'ANIMATED_BANNER',
	BANNER: 'BANNER',
	DETACHED_BANNER: 'DETACHED_BANNER',
	INVITE_SPLASH: 'INVITE_SPLASH',
	INVITES_DISABLED: 'INVITES_DISABLED',
	DISALLOW_UNCLAIMED_ACCOUNTS: 'DISALLOW_UNCLAIMED_ACCOUNTS',
	TEXT_CHANNEL_FLEXIBLE_NAMES: 'TEXT_CHANNEL_FLEXIBLE_NAMES',
	MORE_EMOJI: 'MORE_EMOJI',
	MORE_STICKERS: 'MORE_STICKERS',
	UNLIMITED_EMOJI: 'UNLIMITED_EMOJI',
	UNLIMITED_STICKERS: 'UNLIMITED_STICKERS',
	EXPRESSION_PURGE_ALLOWED: 'EXPRESSION_PURGE_ALLOWED',
	VANITY_URL: 'VANITY_URL',
	VERIFIED: 'VERIFIED',
	VIP_VOICE: 'VIP_VOICE',
	UNAVAILABLE_FOR_EVERYONE: 'UNAVAILABLE_FOR_EVERYONE',
	UNAVAILABLE_FOR_EVERYONE_BUT_STAFF: 'UNAVAILABLE_FOR_EVERYONE_BUT_STAFF',
	VISIONARY: 'VISIONARY',
	OPERATOR: 'OPERATOR',
	LARGE_GUILD_OVERRIDE: 'LARGE_GUILD_OVERRIDE',
} as const;

export const GuildSplashCardAlignment = {
	CENTER: 0,
	LEFT: 1,
	RIGHT: 2,
} as const;

export type GuildSplashCardAlignmentValue = (typeof GuildSplashCardAlignment)[keyof typeof GuildSplashCardAlignment];

export const StickerFormatTypes = {
	PNG: 1,
	APNG: 2,
	LOTTIE: 3,
	GIF: 4,
} as const;

export const GuildMemberProfileFlags = {
	AVATAR_UNSET: 1 << 0,
	BANNER_UNSET: 1 << 1,
} as const;

export const JoinSourceTypes = {
	INVITE: 0,
	BOT_INVITE: 1,
} as const;
