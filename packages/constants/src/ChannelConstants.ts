// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ValueOf} from '@fluxer/constants/src/ValueOf';

export const ChannelTypes = {
	GUILD_TEXT: 0,
	DM: 1,
	GUILD_VOICE: 2,
	GROUP_DM: 3,
	GUILD_CATEGORY: 4,
	GUILD_LINK: 998,
	DM_PERSONAL_NOTES: 999,
} as const;

export type ChannelType = ValueOf<typeof ChannelTypes>;

export const GUILD_TEXT_BASED_CHANNEL_TYPES = new Set<number>([ChannelTypes.GUILD_TEXT, ChannelTypes.GUILD_VOICE]);
export const TEXT_BASED_CHANNEL_TYPES = new Set<number>([
	...GUILD_TEXT_BASED_CHANNEL_TYPES,
	ChannelTypes.DM,
	ChannelTypes.DM_PERSONAL_NOTES,
	ChannelTypes.GROUP_DM,
]);
export const AUTOMATIC_VOICE_REGION_ID = 'automatic';
export const ChannelOverwriteTypes = {
	ROLE: 0,
	MEMBER: 1,
} as const;
export const ChannelOverwriteTypesDescriptions: Record<keyof typeof ChannelOverwriteTypes, string> = {
	ROLE: 'Overwrite applies to a role',
	MEMBER: 'Overwrite applies to a member',
};
export const InviteTypes = {
	GUILD: 0,
	GROUP_DM: 1,
	EMOJI_PACK: 2,
	STICKER_PACK: 3,
} as const;
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

export type MessageTypeValue = ValueOf<typeof MessageTypes>;

const MESSAGE_TYPE_DELETABLE = {
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

export function isMessageTypeDeletable(type: number): boolean {
	return type in MESSAGE_TYPE_DELETABLE ? MESSAGE_TYPE_DELETABLE[type as MessageTypeValue] : false;
}

export const MessageReferenceTypes = {
	DEFAULT: 0,
	FORWARD: 1,
} as const;

export type MessageReferenceTypeValue = ValueOf<typeof MessageReferenceTypes>;

export const MessageReferenceTypesDescriptions: Record<keyof typeof MessageReferenceTypes, string> = {
	DEFAULT: 'Default reference (reply)',
	FORWARD: 'Forwarded message reference',
};
export const AllowedMentionParseTypes = {
	USERS: 'users',
	ROLES: 'roles',
	EVERYONE: 'everyone',
} as const;
export const AllowedMentionParseTypesDescriptions: Record<keyof typeof AllowedMentionParseTypes, string> = {
	USERS: 'Parse user mentions from the message content',
	ROLES: 'Parse role mentions from the message content',
	EVERYONE: 'Parse @everyone and @here mentions from the message content',
};
export const MessageFlags = {
	SUPPRESS_EMBEDS: 1 << 2,
	SUPPRESS_NOTIFICATIONS: 1 << 12,
	VOICE_MESSAGE: 1 << 13,
} as const;
export const MessageFlagsDescriptions: Record<keyof typeof MessageFlags, string> = {
	SUPPRESS_EMBEDS: 'Do not include embeds when serialising this message',
	SUPPRESS_NOTIFICATIONS: 'This message will not trigger push or desktop notifications',
	VOICE_MESSAGE: 'This message is a voice message',
};
export const SENDABLE_MESSAGE_FLAGS =
	MessageFlags.SUPPRESS_EMBEDS | MessageFlags.SUPPRESS_NOTIFICATIONS | MessageFlags.VOICE_MESSAGE;
export const MessageAttachmentFlags = {
	IS_SPOILER: 1 << 3,
	CONTAINS_EXPLICIT_MEDIA: 1 << 4,
	IS_ANIMATED: 1 << 5,
} as const;
export const MessageAttachmentFlagsDescriptions: Record<keyof typeof MessageAttachmentFlags, string> = {
	IS_SPOILER: 'Attachment is marked as a spoiler',
	CONTAINS_EXPLICIT_MEDIA: 'Attachment contains explicit media content',
	IS_ANIMATED: 'Attachment is animated',
};
export const EmbedMediaFlags = {
	CONTAINS_EXPLICIT_MEDIA: 1 << 4,
	IS_ANIMATED: 1 << 5,
} as const;
export const EmbedMediaFlagsDescriptions: Record<keyof typeof EmbedMediaFlags, string> = {
	CONTAINS_EXPLICIT_MEDIA: 'Embed media contains explicit content',
	IS_ANIMATED: 'Embed media is animated',
};
export const MessageEmbedTypes = {
	RICH: 'rich',
	ARTICLE: 'article',
	LINK: 'link',
	IMAGE: 'image',
	VIDEO: 'video',
	AUDIO: 'audio',
	GIFV: 'gifv',
	BLUESKY: 'bluesky',
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
	VIEW_CHANNEL_MEMBERS: 1n << 54n,
} as const;
export const PermissionsDescriptions: Record<keyof typeof Permissions, string> = {
	CREATE_INSTANT_INVITE: 'Allows creation of instant invites',
	KICK_MEMBERS: 'Allows kicking members from the guild',
	BAN_MEMBERS: 'Allows banning members from the guild',
	ADMINISTRATOR: 'Grants all permissions and bypasses channel permission overwrites',
	MANAGE_CHANNELS: 'Allows management, editing, and deletion of channels',
	MANAGE_GUILD: 'Allows management and editing of the guild',
	ADD_REACTIONS: 'Allows adding reactions to messages',
	VIEW_AUDIT_LOG: 'Allows viewing of the audit log',
	PRIORITY_SPEAKER: 'Allows using priority speaker in a voice channel',
	STREAM: 'Allows the user to go live',
	VIEW_CHANNEL: 'Allows viewing a channel',
	SEND_MESSAGES: 'Allows sending messages in a channel',
	SEND_TTS_MESSAGES: 'Allows sending text-to-speech messages',
	MANAGE_MESSAGES: 'Allows for deleting and pinning messages',
	EMBED_LINKS: 'Links sent will have an embed automatically',
	ATTACH_FILES: 'Allows uploading files',
	READ_MESSAGE_HISTORY: 'Allows reading message history',
	MENTION_EVERYONE: 'Allows using @everyone and @here mentions',
	USE_EXTERNAL_EMOJIS: 'Allows using emojis from other guilds',
	CONNECT: 'Allows connecting to a voice channel',
	SPEAK: 'Allows speaking in a voice channel',
	MUTE_MEMBERS: 'Allows muting members in voice channels',
	DEAFEN_MEMBERS: 'Allows deafening members in voice channels',
	MOVE_MEMBERS: 'Allows moving members between voice channels',
	USE_VAD: 'Allows using voice activity detection',
	CHANGE_NICKNAME: 'Allows changing own nickname',
	MANAGE_NICKNAMES: 'Allows changing other members nicknames',
	MANAGE_ROLES: 'Allows management and editing of roles',
	MANAGE_WEBHOOKS: 'Allows management and editing of webhooks',
	MANAGE_EXPRESSIONS: 'Allows management of guild expressions',
	USE_EXTERNAL_STICKERS: 'Allows using stickers from other guilds',
	MODERATE_MEMBERS: 'Allows timing out users',
	CREATE_EXPRESSIONS: 'Allows creating guild expressions',
	PIN_MESSAGES: 'Allows pinning messages',
	BYPASS_SLOWMODE: 'Allows bypassing slowmode',
	UPDATE_RTC_REGION: 'Allows updating the voice region',
	VIEW_CHANNEL_MEMBERS: 'Allows viewing the member list in a channel',
};
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
	Permissions.VIEW_CHANNEL_MEMBERS;
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
export const CHANNEL_REINDEX_AFTER_TIMESTAMP = 1779557400;
