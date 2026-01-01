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

export const UserAuthenticatorTypes = {
	TOTP: 0,
	SMS: 1,
	WEBAUTHN: 2,
} as const;

export const UserPremiumTypes = {
	NONE: 0,
	SUBSCRIPTION: 1,
	LIFETIME: 2,
} as const;

export const UserFlags = {
	STAFF: 1n << 0n,
	CTP_MEMBER: 1n << 1n,
	PARTNER: 1n << 2n,
	BUG_HUNTER: 1n << 3n,
	HIGH_GLOBAL_RATE_LIMIT: 1n << 33n,
	DELETED: 1n << 34n,
	DISABLED_SUSPICIOUS_ACTIVITY: 1n << 35n,
	SELF_DELETED: 1n << 36n,
	PREMIUM_DISCRIMINATOR: 1n << 37n,
	DISABLED: 1n << 38n,
	HAS_SESSION_STARTED: 1n << 39n,
	PREMIUM_BADGE_HIDDEN: 1n << 40n,
	PREMIUM_BADGE_MASKED: 1n << 41n,
	PREMIUM_BADGE_TIMESTAMP_HIDDEN: 1n << 42n,
	PREMIUM_BADGE_SEQUENCE_HIDDEN: 1n << 43n,
	PREMIUM_PERKS_SANITIZED: 1n << 44n,
	PREMIUM_PURCHASE_DISABLED: 1n << 45n,
	PREMIUM_ENABLED_OVERRIDE: 1n << 46n,
	RATE_LIMIT_BYPASS: 1n << 47n,
	REPORT_BANNED: 1n << 48n,
	VERIFIED_NOT_UNDERAGE: 1n << 49n,
	PENDING_MANUAL_VERIFICATION: 1n << 50n,
	HAS_DISMISSED_PREMIUM_ONBOARDING: 1n << 51n,
	USED_MOBILE_CLIENT: 1n << 52n,
	APP_STORE_REVIEWER: 1n << 53n,
} as const;

export const PUBLIC_USER_FLAGS = UserFlags.STAFF | UserFlags.CTP_MEMBER | UserFlags.PARTNER | UserFlags.BUG_HUNTER;

export const SuspiciousActivityFlags = {
	REQUIRE_VERIFIED_EMAIL: 1 << 0,
	REQUIRE_REVERIFIED_EMAIL: 1 << 1,
	REQUIRE_VERIFIED_PHONE: 1 << 2,
	REQUIRE_REVERIFIED_PHONE: 1 << 3,
	REQUIRE_VERIFIED_EMAIL_OR_VERIFIED_PHONE: 1 << 4,
	REQUIRE_REVERIFIED_EMAIL_OR_VERIFIED_PHONE: 1 << 5,
	REQUIRE_VERIFIED_EMAIL_OR_REVERIFIED_PHONE: 1 << 6,
	REQUIRE_REVERIFIED_EMAIL_OR_REVERIFIED_PHONE: 1 << 7,
} as const;

export const Locales = {
	AR: 'ar',
	BG: 'bg',
	CS: 'cs',
	DA: 'da',
	DE: 'de',
	EL: 'el',
	EN_GB: 'en-GB',
	EN_US: 'en-US',
	ES_ES: 'es-ES',
	ES_419: 'es-419',
	FI: 'fi',
	FR: 'fr',
	HE: 'he',
	HI: 'hi',
	HR: 'hr',
	HU: 'hu',
	ID: 'id',
	IT: 'it',
	JA: 'ja',
	KO: 'ko',
	LT: 'lt',
	NL: 'nl',
	NO: 'no',
	PL: 'pl',
	PT_BR: 'pt-BR',
	RO: 'ro',
	RU: 'ru',
	SV_SE: 'sv-SE',
	TH: 'th',
	TR: 'tr',
	UK: 'uk',
	VI: 'vi',
	ZH_CN: 'zh-CN',
	ZH_TW: 'zh-TW',
} as const;

export const StatusTypes = {
	ONLINE: 'online',
	DND: 'dnd',
	IDLE: 'idle',
	INVISIBLE: 'invisible',
} as const;

export const ThemeTypes = {
	DARK: 'dark',
	COAL: 'coal',
	LIGHT: 'light',
	SYSTEM: 'system',
} as const;

export const StickerAnimationOptions = {
	ALWAYS_ANIMATE: 0,
	ANIMATE_ON_INTERACTION: 1,
	NEVER_ANIMATE: 2,
} as const;

export const RenderSpoilers = {
	ALWAYS: 0,
	ON_CLICK: 1,
	IF_MODERATOR: 2,
} as const;

export const UserExplicitContentFilterTypes = {
	DISABLED: 0,
	NON_FRIENDS: 1,
	FRIENDS_AND_NON_FRIENDS: 2,
} as const;

export const FriendSourceFlags = {
	MUTUAL_FRIENDS: 1 << 0,
	MUTUAL_GUILDS: 1 << 1,
	NO_RELATION: 1 << 2,
} as const;

export const IncomingCallFlags = {
	FRIENDS_OF_FRIENDS: 1 << 0,
	GUILD_MEMBERS: 1 << 1,
	EVERYONE: 1 << 2,
	FRIENDS_ONLY: 1 << 3,
	NOBODY: 1 << 4,
	SILENT_EVERYONE: 1 << 5,
} as const;

export const GroupDmAddPermissionFlags = {
	FRIENDS_OF_FRIENDS: 1 << 0,
	GUILD_MEMBERS: 1 << 1,
	EVERYONE: 1 << 2,
	FRIENDS_ONLY: 1 << 3,
	NOBODY: 1 << 4,
} as const;

export const UserNotificationSettings = {
	ALL_MESSAGES: 0,
	ONLY_MENTIONS: 1,
	NO_MESSAGES: 2,
	INHERIT: 3,
} as const;

export const RelationshipTypes = {
	FRIEND: 1,
	BLOCKED: 2,
	INCOMING_REQUEST: 3,
	OUTGOING_REQUEST: 4,
} as const;
