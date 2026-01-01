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

import type {I18n, MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';

export const FLUXER_EPOCH = 1420070400000;
export const ME = '@me';
export const FAVORITES_GUILD_ID = '1337';
export const FLUXERBOT_ID = '0';

export const OAuth2Scopes = ['identify', 'email', 'guilds', 'bot', 'applications.commands'] as const;
export type OAuth2Scope = (typeof OAuth2Scopes)[number];

const OAuth2ScopeDescriptorsInternal: Record<OAuth2Scope, MessageDescriptor> = {
	identify: msg`Access your basic profile information (username, avatar, etc.)`,
	email: msg`View your email address`,
	guilds: msg`View the communities you are a member of`,
	bot: msg`Add a bot to a community with requested permissions`,
	'applications.commands': msg`Manage slash commands for this application`,
};

export function getOAuth2ScopeDescription(i18n: I18n, scope: OAuth2Scope): string {
	return i18n._(OAuth2ScopeDescriptorsInternal[scope]);
}

export const DEFAULT_ACCENT_COLOR = '#4641D9';

export const MAX_GUILDS_PREMIUM = 200;
export const MAX_GUILDS_NON_PREMIUM = 100;
export const MAX_GUILD_EMOJIS_ANIMATED = 50;
export const MAX_GUILD_EMOJIS_STATIC = 50;
export const MAX_GUILD_EMOJIS_ANIMATED_MORE_EMOJI = 250;
export const MAX_GUILD_EMOJIS_STATIC_MORE_EMOJI = 250;
export const MAX_GUILD_STICKERS = 50;
export const MAX_GUILD_STICKERS_MORE_STICKERS = 250;
export const MAX_CHANNELS_PER_CATEGORY = 50;

export const MAX_MESSAGE_LENGTH_PREMIUM = 4000;
export const MAX_MESSAGE_LENGTH_NON_PREMIUM = 2000;
export const MAX_ATTACHMENTS_PER_MESSAGE = 10;

export const MAX_MESSAGES_PER_CHANNEL = 50;
export const MAX_LOADED_MESSAGES = MAX_MESSAGES_PER_CHANNEL * 4;
export const TRUNCATED_MESSAGE_VIEW_SIZE = MAX_LOADED_MESSAGES * 0.5;
export const MAX_MESSAGE_CACHE_SIZE = MAX_MESSAGES_PER_CHANNEL * 5;

export const NEW_MESSAGES_BAR_BUFFER = 32;

export const MAX_BOOKMARKS_PREMIUM = 300;
export const MAX_BOOKMARKS_NON_PREMIUM = 50;
export const MAX_FAVORITE_MEMES_PREMIUM = 500;
export const MAX_FAVORITE_MEMES_NON_PREMIUM = 50;
export const MAX_FAVORITE_MEME_TAGS = 10;

export const MAX_BIO_LENGTH_PREMIUM = 320;
export const MAX_BIO_LENGTH_NON_PREMIUM = 160;

export const EMOJI_MAX_SIZE = 384 * 1024;
export const SKIN_TONE_SURROGATES = ['🏻', '🏼', '🏽', '🏾', '🏿'];

export const STICKER_MAX_SIZE = 512 * 1024;

export const ATTACHMENT_MAX_SIZE_PREMIUM = 500 * 1024 * 1024;
export const ATTACHMENT_MAX_SIZE_NON_PREMIUM = 25 * 1024 * 1024;

export const API_CODE_VERSION = 1;

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
	STAFF: 1 << 0,
	CTP_MEMBER: 1 << 1,
	PARTNER: 1 << 2,
	BUG_HUNTER: 1 << 3,
} as const;

export const StatusTypes = {
	ONLINE: 'online',
	DND: 'dnd',
	IDLE: 'idle',
	INVISIBLE: 'invisible',
	OFFLINE: 'offline',
} as const;

export type StatusType = (typeof StatusTypes)[keyof typeof StatusTypes];

const STATUS_VALUES = Object.values(StatusTypes) as Array<StatusType>;
const STATUS_SET = new Set<StatusType>(STATUS_VALUES);

export const isStatusType = (value: unknown): value is StatusType =>
	typeof value === 'string' && STATUS_SET.has(value as StatusType);

export const normalizeStatus = (value: unknown): StatusType => (isStatusType(value) ? value : StatusTypes.OFFLINE);

export const OFFLINE_STATUS_TYPES: Set<StatusType> = new Set([StatusTypes.OFFLINE, StatusTypes.INVISIBLE]);

export const isOfflineStatus = (
	status: StatusType,
): status is typeof StatusTypes.OFFLINE | typeof StatusTypes.INVISIBLE =>
	status === StatusTypes.OFFLINE || status === StatusTypes.INVISIBLE;

const StatusTypeToLabelDescriptorsInternal: Record<StatusType, MessageDescriptor> = {
	[StatusTypes.ONLINE]: msg`Online`,
	[StatusTypes.DND]: msg`Do Not Disturb`,
	[StatusTypes.IDLE]: msg`Idle`,
	[StatusTypes.INVISIBLE]: msg`Invisible`,
	[StatusTypes.OFFLINE]: msg`Offline`,
};

export function getStatusTypeLabel(i18n: I18n, statusType: StatusType | string): string {
	const normalized = isStatusType(statusType) ? statusType : normalizeStatus(statusType);
	return i18n._(StatusTypeToLabelDescriptorsInternal[normalized]);
}

const StatusTypeToDescriptionDescriptorsInternal: Record<StatusType, MessageDescriptor> = {
	[StatusTypes.ONLINE]: msg`Charged up on 1.21 gigawatts, ready to talk`,
	[StatusTypes.DND]: msg`In the zone, please do not disturb`,
	[StatusTypes.IDLE]: msg`Took the DeLorean out, but I'll be back in time`,
	[StatusTypes.INVISIBLE]: msg`Currently stuck in 1885, appearing offline`,
	[StatusTypes.OFFLINE]: msg`Currently stuck in 1885, appearing offline`,
};

export function getStatusTypeDescription(i18n: I18n, statusType: StatusType | string): string {
	const normalized = isStatusType(statusType) ? statusType : normalizeStatus(statusType);
	return i18n._(StatusTypeToDescriptionDescriptorsInternal[normalized]);
}

export const InviteTypes = {
	GUILD: 0,
	GROUP_DM: 1,
	EMOJI_PACK: 2,
	STICKER_PACK: 3,
} as const;

export const ThemeTypes = {
	DARK: 'dark',
	COAL: 'coal',
	LIGHT: 'light',
	SYSTEM: 'system',
} as const;

export const FeatureFlags = {
	MESSAGE_SCHEDULING: 'message_scheduling',
	EXPRESSION_PACKS: 'expression_packs',
} as const;

export type FeatureFlag = (typeof FeatureFlags)[keyof typeof FeatureFlags];

export const ALL_FEATURE_FLAGS: Array<FeatureFlag> = Object.values(FeatureFlags);

export const TimeFormatTypes = {
	AUTO: 0,
	TWELVE_HOUR: 1,
	TWENTY_FOUR_HOUR: 2,
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

export const GuildExplicitContentFilterTypes = {
	DISABLED: 0,
	MEMBERS_WITHOUT_ROLES: 1,
	ALL_MEMBERS: 2,
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

export const MessageNotifications = {
	NULL: -1,
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

export type GuildSplashCardAlignmentValue = (typeof GuildSplashCardAlignment)[keyof typeof GuildSplashCardAlignment];

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

export const ChannelTypes = {
	GUILD_TEXT: 0,
	DM: 1,
	GUILD_VOICE: 2,
	GROUP_DM: 3,
	GUILD_CATEGORY: 4,
	GUILD_LINK: 998,
	DM_PERSONAL_NOTES: 999,
} as const;

export const TEXT_BASED_CHANNEL_TYPES = new Set<number>([
	ChannelTypes.GUILD_TEXT,
	ChannelTypes.DM,
	ChannelTypes.DM_PERSONAL_NOTES,
	ChannelTypes.GROUP_DM,
]);

export const QuickSwitcherResultTypes = {
	HEADER: 'header',
	USER: 'user',
	GROUP_DM: 'group_dm',
	TEXT_CHANNEL: 'text_channel',
	VOICE_CHANNEL: 'voice_channel',
	GUILD: 'guild',
	VIRTUAL_GUILD: 'virtual_guild',
	SETTINGS: 'settings',
	QUICK_ACTION: 'quick_action',
	LINK: 'link',
} as const;

export type QuickSwitcherResultType = (typeof QuickSwitcherResultTypes)[keyof typeof QuickSwitcherResultTypes];

export const MessageTypes = {
	DEFAULT: 0,
	RECIPIENT_ADD: 1,
	RECIPIENT_REMOVE: 2,
	CALL: 3,
	CHANNEL_NAME_CHANGE: 4,
	CHANNEL_ICON_CHANGE: 5,
	CHANNEL_PINNED_MESSAGE: 6,
	USER_JOIN: 7,
	REPLY: 19,
	CLIENT_SYSTEM: 99,
} as const;
export type MessageTypeValue = (typeof MessageTypes)[keyof typeof MessageTypes];

export const MESSAGE_TYPE_DELETABLE = {
	[MessageTypes.DEFAULT]: true,
	[MessageTypes.REPLY]: true,
	[MessageTypes.CHANNEL_PINNED_MESSAGE]: true,
	[MessageTypes.USER_JOIN]: true,
	[MessageTypes.RECIPIENT_ADD]: false,
	[MessageTypes.RECIPIENT_REMOVE]: false,
	[MessageTypes.CALL]: false,
	[MessageTypes.CHANNEL_NAME_CHANGE]: false,
	[MessageTypes.CHANNEL_ICON_CHANGE]: false,
	[MessageTypes.CLIENT_SYSTEM]: false,
} as const satisfies Record<MessageTypeValue, boolean>;

export const isMessageTypeDeletable = (type: number): boolean => {
	return type in MESSAGE_TYPE_DELETABLE ? MESSAGE_TYPE_DELETABLE[type as MessageTypeValue] : false;
};

export const MessageFlags = {
	SUPPRESS_EMBEDS: 1 << 2,
	SUPPRESS_NOTIFICATIONS: 1 << 12,
	COMPACT_ATTACHMENTS: 1 << 17,
} as const;

export const MessageAttachmentFlags = {
	IS_SPOILER: 1 << 3,
	CONTAINS_EXPLICIT_MEDIA: 1 << 4,
	IS_ANIMATED: 1 << 5,
} as const;

export const MessageEmbedTypes = {
	RICH: 'rich',
	ARTICLE: 'article',
	LINK: 'link',
	IMAGE: 'image',
	VIDEO: 'video',
	AUDIO: 'audio',
	GIFV: 'gifv',
} as const;

export const MessageStates = {
	SENT: 'SENT',
	SENDING: 'SENDING',
	EDITING: 'EDITING',
	FAILED: 'FAILED',
} as const;

export const MessagePreviewContext = {
	SETTINGS: 'SETTINGS',
	LIST_POPOUT: 'LIST_POPOUT',
} as const;

export const Permissions = {
	CREATE_INSTANT_INVITE: 1n << 0n,
	KICK_MEMBERS: 1n << 1n,
	BAN_MEMBERS: 1n << 2n,
	ADMINISTRATOR: 1n << 3n,
	MANAGE_CHANNELS: 1n << 4n,
	MANAGE_GUILD: 1n << 5n,
	ADD_REACTIONS: 1n << 6n,
	VIEW_AUDIT_LOG: 1n << 7n,
	PRIORITY_SPEAKER: 1n << 8n,
	STREAM: 1n << 9n,
	VIEW_CHANNEL: 1n << 10n,
	SEND_MESSAGES: 1n << 11n,
	SEND_TTS_MESSAGES: 1n << 12n,
	MANAGE_MESSAGES: 1n << 13n,
	EMBED_LINKS: 1n << 14n,
	ATTACH_FILES: 1n << 15n,
	READ_MESSAGE_HISTORY: 1n << 16n,
	MENTION_EVERYONE: 1n << 17n,
	USE_EXTERNAL_EMOJIS: 1n << 18n,
	CONNECT: 1n << 20n,
	SPEAK: 1n << 21n,
	MUTE_MEMBERS: 1n << 22n,
	DEAFEN_MEMBERS: 1n << 23n,
	MOVE_MEMBERS: 1n << 24n,
	USE_VAD: 1n << 25n,
	CHANGE_NICKNAME: 1n << 26n,
	MANAGE_NICKNAMES: 1n << 27n,
	MANAGE_ROLES: 1n << 28n,
	MANAGE_WEBHOOKS: 1n << 29n,
	MANAGE_EXPRESSIONS: 1n << 30n,
	USE_EXTERNAL_STICKERS: 1n << 37n,
	MODERATE_MEMBERS: 1n << 40n,
	CREATE_EXPRESSIONS: 1n << 43n,
	PIN_MESSAGES: 1n << 51n,
	BYPASS_SLOWMODE: 1n << 52n,
	UPDATE_RTC_REGION: 1n << 53n,
} as const;

export const ALL_PERMISSIONS = Object.values(Permissions).reduce((acc, p) => acc | p, 0n);

export const DEFAULT_PERMISSIONS =
	Permissions.CREATE_INSTANT_INVITE |
	Permissions.ADD_REACTIONS |
	Permissions.STREAM |
	Permissions.VIEW_CHANNEL |
	Permissions.SEND_MESSAGES |
	Permissions.EMBED_LINKS |
	Permissions.ATTACH_FILES |
	Permissions.READ_MESSAGE_HISTORY |
	Permissions.USE_EXTERNAL_EMOJIS |
	Permissions.CONNECT |
	Permissions.SPEAK |
	Permissions.USE_VAD |
	Permissions.CHANGE_NICKNAME |
	Permissions.USE_EXTERNAL_STICKERS |
	Permissions.CREATE_EXPRESSIONS;

export const ElevatedPermissions =
	Permissions.KICK_MEMBERS |
	Permissions.BAN_MEMBERS |
	Permissions.ADMINISTRATOR |
	Permissions.MANAGE_CHANNELS |
	Permissions.MANAGE_GUILD |
	Permissions.MANAGE_ROLES |
	Permissions.MANAGE_MESSAGES |
	Permissions.MANAGE_WEBHOOKS |
	Permissions.MANAGE_EXPRESSIONS |
	Permissions.MODERATE_MEMBERS;

export const GatewayOpcodes = {
	DISPATCH: 0,
	HEARTBEAT: 1,
	IDENTIFY: 2,
	PRESENCE_UPDATE: 3,
	VOICE_STATE_UPDATE: 4,
	VOICE_SERVER_PING: 5,
	RESUME: 6,
	RECONNECT: 7,
	REQUEST_GUILD_MEMBERS: 8,
	INVALID_SESSION: 9,
	HELLO: 10,
	HEARTBEAT_ACK: 11,
	GATEWAY_ERROR: 12,
	CALL_CONNECT: 13,
	LAZY_REQUEST: 14,
} as const;

export const LARGE_GUILD_THRESHOLD = 250;
export const MEMBER_CHUNK_SIZE = 1000;

export const GatewayIdentifyFlags = {
	USE_CANARY_API: 1 << 0,
} as const;

export const GatewayCloseCodes = {
	UNKNOWN_ERROR: 4000,
	UNKNOWN_OPCODE: 4001,
	DECODE_ERROR: 4002,
	NOT_AUTHENTICATED: 4003,
	AUTHENTICATION_FAILED: 4004,
	ALREADY_AUTHENTICATED: 4005,
	INVALID_SEQ: 4007,
	RATE_LIMITED: 4008,
	SESSION_TIMEOUT: 4009,
	INVALID_SHARD: 4010,
	SHARDING_REQUIRED: 4011,
	INVALID_API_VERSION: 4012,
} as const;

export const GatewayErrorCodes = {
	VOICE_CONNECTION_NOT_FOUND: 'VOICE_CONNECTION_NOT_FOUND',
	VOICE_CHANNEL_NOT_FOUND: 'VOICE_CHANNEL_NOT_FOUND',
	VOICE_INVALID_CHANNEL_TYPE: 'VOICE_INVALID_CHANNEL_TYPE',
	VOICE_MEMBER_NOT_FOUND: 'VOICE_MEMBER_NOT_FOUND',
	VOICE_MEMBER_TIMED_OUT: 'VOICE_MEMBER_TIMED_OUT',
	VOICE_USER_NOT_IN_VOICE: 'VOICE_USER_NOT_IN_VOICE',
	VOICE_GUILD_NOT_FOUND: 'VOICE_GUILD_NOT_FOUND',
	VOICE_PERMISSION_DENIED: 'VOICE_PERMISSION_DENIED',
	VOICE_CHANNEL_FULL: 'VOICE_CHANNEL_FULL',
	VOICE_MISSING_CONNECTION_ID: 'VOICE_MISSING_CONNECTION_ID',
	VOICE_INVALID_USER_ID: 'VOICE_INVALID_USER_ID',
	VOICE_INVALID_CHANNEL_ID: 'VOICE_INVALID_CHANNEL_ID',
	VOICE_INVALID_STATE: 'VOICE_INVALID_STATE',
	VOICE_USER_MISMATCH: 'VOICE_USER_MISMATCH',
	VOICE_TOKEN_FAILED: 'VOICE_TOKEN_FAILED',
	VOICE_GUILD_ID_MISSING: 'VOICE_GUILD_ID_MISSING',
	VOICE_INVALID_GUILD_ID: 'VOICE_INVALID_GUILD_ID',
	DM_NOT_RECIPIENT: 'DM_NOT_RECIPIENT',
	DM_INVALID_CHANNEL_TYPE: 'DM_INVALID_CHANNEL_TYPE',
	UNKNOWN_ERROR: 'UNKNOWN_ERROR',
} as const;
export type GatewayErrorCode = (typeof GatewayErrorCodes)[keyof typeof GatewayErrorCodes];

export const APIErrorCodes = {
	GENERAL_ERROR: 'GENERAL_ERROR',
	UNKNOWN_CHANNEL: 'UNKNOWN_CHANNEL',
	UNKNOWN_GUILD: 'UNKNOWN_GUILD',
	UNKNOWN_INVITE: 'UNKNOWN_INVITE',
	UNKNOWN_MEMBER: 'UNKNOWN_MEMBER',
	UNKNOWN_MESSAGE: 'UNKNOWN_MESSAGE',
	UNKNOWN_ROLE: 'UNKNOWN_ROLE',
	UNKNOWN_USER: 'UNKNOWN_USER',
	UNKNOWN_EMOJI: 'UNKNOWN_EMOJI',
	UNKNOWN_PACK: 'UNKNOWN_PACK',
	UNKNOWN_WEBHOOK: 'UNKNOWN_WEBHOOK',
	UNKNOWN_BETA_CODE: 'UNKNOWN_BETA_CODE',
	EXPLICIT_CONTENT_CANNOT_BE_SENT: 'EXPLICIT_CONTENT_CANNOT_BE_SENT',
	FILE_SIZE_TOO_LARGE: 'FILE_SIZE_TOO_LARGE',
	MAX_GUILDS: 'MAX_GUILDS',
	MAX_FRIENDS: 'MAX_FRIENDS',
	MAX_PINS_PER_CHANNEL: 'MAX_PINS_PER_CHANNEL',
	MAX_GUILD_ROLES: 'MAX_GUILD_ROLES',
	MAX_WEBHOOKS: 'MAX_WEBHOOKS',
	MAX_EMOJIS: 'MAX_EMOJIS',
	MAX_REACTIONS: 'MAX_REACTIONS',
	MAX_GUILD_CHANNELS: 'MAX_GUILD_CHANNELS',
	MAX_CATEGORY_CHANNELS: 'MAX_CATEGORY_CHANNELS',
	MAX_INVITES: 'MAX_INVITES',
	MAX_PACKS: 'MAX_PACKS',
	MAX_ANIMATED_EMOJIS: 'MAX_ANIMATED_EMOJIS',
	MAX_GUILD_MEMBERS: 'MAX_GUILD_MEMBERS',
	MAX_WEBHOOKS_PER_GUILD: 'MAX_WEBHOOKS_PER_GUILD',
	MAX_BETA_CODES_REACHED: 'MAX_BETA_CODES_REACHED',
	RATE_LIMITED: 'RATE_LIMITED',
	SLOWMODE_RATE_LIMITED: 'SLOWMODE_RATE_LIMITED',
	CAPTCHA_REQUIRED: 'CAPTCHA_REQUIRED',
	INVALID_CAPTCHA: 'INVALID_CAPTCHA',
	UNAUTHORIZED: 'UNAUTHORIZED',
	USER_BANNED_FROM_GUILD: 'USER_BANNED_FROM_GUILD',
	USER_IP_BANNED_FROM_GUILD: 'USER_IP_BANNED_FROM_GUILD',
	MISSING_ACCESS: 'MISSING_ACCESS',
	PREMIUM_REQUIRED: 'PREMIUM_REQUIRED',
	CANNOT_EXECUTE_ON_DM: 'CANNOT_EXECUTE_ON_DM',
	CANNOT_EDIT_OTHER_USER_MESSAGE: 'CANNOT_EDIT_OTHER_USER_MESSAGE',
	CANNOT_SEND_EMPTY_MESSAGE: 'CANNOT_SEND_EMPTY_MESSAGE',
	CANNOT_SEND_MESSAGES_TO_USER: 'CANNOT_SEND_MESSAGES_TO_USER',
	CANNOT_SEND_MESSAGES_IN_NON_TEXT_CHANNEL: 'CANNOT_SEND_MESSAGES_IN_NON_TEXT_CHANNEL',
	COMMUNICATION_DISABLED: 'COMMUNICATION_DISABLED',
	MISSING_PERMISSIONS: 'MISSING_PERMISSIONS',
	INVALID_FORM_BODY: 'INVALID_FORM_BODY',
	CANNOT_MODIFY_SYSTEM_WEBHOOK: 'CANNOT_MODIFY_SYSTEM_WEBHOOK',
	TWO_FACTOR_REQUIRED: 'TWO_FACTOR_REQUIRED',
	FRIEND_REQUEST_BLOCKED: 'FRIEND_REQUEST_BLOCKED',
	CANNOT_SEND_FRIEND_REQUEST_TO_BLOCKED_USER: 'CANNOT_SEND_FRIEND_REQUEST_TO_BLOCKED_USER',
	BOTS_CANNOT_HAVE_FRIENDS: 'BOTS_CANNOT_HAVE_FRIENDS',
	CANNOT_SEND_FRIEND_REQUEST_TO_SELF: 'CANNOT_SEND_FRIEND_REQUEST_TO_SELF',
	NO_USERS_WITH_FLUXERTAG_EXIST: 'NO_USERS_WITH_FLUXERTAG_EXIST',
	ALREADY_FRIENDS: 'ALREADY_FRIENDS',
	DISCRIMINATOR_REQUIRED: 'DISCRIMINATOR_REQUIRED',
	TWO_FA_NOT_ENABLED: 'TWO_FA_NOT_ENABLED',
	MISSING_ACL: 'MISSING_ACL',
	IP_BANNED: 'IP_BANNED',
	IP_AUTHORIZATION_REQUIRED: 'IP_AUTHORIZATION_REQUIRED',
	USER_NOT_IN_VOICE: 'USER_NOT_IN_VOICE',
	CANNOT_MODIFY_VOICE_STATE: 'CANNOT_MODIFY_VOICE_STATE',
	ACCOUNT_DISABLED: 'ACCOUNT_DISABLED',
	ACCOUNT_SCHEDULED_FOR_DELETION: 'ACCOUNT_SCHEDULED_FOR_DELETION',
	TAG_ALREADY_TAKEN: 'TAG_ALREADY_TAKEN',
	INVALID_PHONE_NUMBER: 'INVALID_PHONE_NUMBER',
	PHONE_ALREADY_USED: 'PHONE_ALREADY_USED',
	PHONE_VERIFICATION_REQUIRED: 'PHONE_VERIFICATION_REQUIRED',
	INVALID_PHONE_VERIFICATION_CODE: 'INVALID_PHONE_VERIFICATION_CODE',
	PHONE_RATE_LIMIT_EXCEEDED: 'PHONE_RATE_LIMIT_EXCEEDED',
	SMS_MFA_NOT_ENABLED: 'SMS_MFA_NOT_ENABLED',
	SMS_MFA_REQUIRES_TOTP: 'SMS_MFA_REQUIRES_TOTP',
	PHONE_REQUIRED_FOR_SMS_MFA: 'PHONE_REQUIRED_FOR_SMS_MFA',
	INVALID_WEBAUTHN_CREDENTIAL: 'INVALID_WEBAUTHN_CREDENTIAL',
	WEBAUTHN_CREDENTIAL_LIMIT_REACHED: 'WEBAUTHN_CREDENTIAL_LIMIT_REACHED',
	UNKNOWN_WEBAUTHN_CREDENTIAL: 'UNKNOWN_WEBAUTHN_CREDENTIAL',
	PASSKEY_AUTHENTICATION_FAILED: 'PASSKEY_AUTHENTICATION_FAILED',
	ACCOUNT_SUSPICIOUS_ACTIVITY: 'ACCOUNT_SUSPICIOUS_ACTIVITY',
	PREMIUM_PURCHASE_BLOCKED: 'PREMIUM_PURCHASE_BLOCKED',
	HARVEST_ON_COOLDOWN: 'HARVEST_ON_COOLDOWN',
	UNKNOWN_GIFT_CODE: 'UNKNOWN_GIFT_CODE',
	GIFT_CODE_ALREADY_REDEEMED: 'GIFT_CODE_ALREADY_REDEEMED',
	STRIPE_ERROR: 'STRIPE_ERROR',
	STRIPE_WEBHOOK_SIGNATURE_INVALID: 'STRIPE_WEBHOOK_SIGNATURE_INVALID',
	USER_OWNS_GUILDS: 'USER_OWNS_GUILDS',
	FEATURE_TEMPORARILY_DISABLED: 'FEATURE_TEMPORARILY_DISABLED',
	INVITES_DISABLED: 'INVITES_DISABLED',
	TEMPORARY_INVITE_REQUIRES_PRESENCE: 'TEMPORARY_INVITE_REQUIRES_PRESENCE',
	MAX_GROUP_DM_RECIPIENTS: 'MAX_GROUP_DM_RECIPIENTS',
	NOT_FRIENDS_WITH_USER: 'NOT_FRIENDS_WITH_USER',
	INVALID_CHANNEL_TYPE: 'INVALID_CHANNEL_TYPE',
	NSFW_CONTENT_AGE_RESTRICTED: 'NSFW_CONTENT_AGE_RESTRICTED',
	MAX_BOOKMARKS: 'MAX_BOOKMARKS',
	MAX_PACK_EXPRESSIONS: 'MAX_PACK_EXPRESSIONS',
	MAX_FAVORITE_MEMES: 'MAX_FAVORITE_MEMES',
	CANNOT_REDEEM_PLUTONIUM_WITH_VISIONARY: 'CANNOT_REDEEM_PLUTONIUM_WITH_VISIONARY',
	NO_VISIONARY_SLOTS_AVAILABLE: 'NO_VISIONARY_SLOTS_AVAILABLE',
	CANNOT_SHRINK_RESERVED_SLOTS: 'CANNOT_SHRINK_RESERVED_SLOTS',
	UNCLAIMED_ACCOUNT_RESTRICTED: 'UNCLAIMED_ACCOUNT_RESTRICTED',
	GUILD_DISALLOWS_UNCLAIMED_ACCOUNTS: 'GUILD_DISALLOWS_UNCLAIMED_ACCOUNTS',
} as const;

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
	VERIFIED: 'VERIFIED',
	VIP_VOICE: 'VIP_VOICE',
	UNAVAILABLE_FOR_EVERYONE: 'UNAVAILABLE_FOR_EVERYONE',
	UNAVAILABLE_FOR_EVERYONE_BUT_STAFF: 'UNAVAILABLE_FOR_EVERYONE_BUT_STAFF',
	VISIONARY: 'VISIONARY',
	OPERATOR: 'OPERATOR',
	DISALLOW_UNCLAIMED_ACCOUNTS: 'DISALLOW_UNCLAIMED_ACCOUNTS',
	LARGE_GUILD_OVERRIDE: 'LARGE_GUILD_OVERRIDE',
} as const;

export const JumpTypes = {
	ANIMATED: 'ANIMATED',
	INSTANT: 'INSTANT',
	NONE: 'NONE',
} as const;
export type JumpTypes = (typeof JumpTypes)[keyof typeof JumpTypes];
