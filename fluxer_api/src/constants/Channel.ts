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
} as const;
type MessageTypeValue = (typeof MessageTypes)[keyof typeof MessageTypes];

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
} as const satisfies Record<MessageTypeValue, boolean>;

export const isMessageTypeDeletable = (type: number): boolean => {
	return type in MESSAGE_TYPE_DELETABLE ? MESSAGE_TYPE_DELETABLE[type as MessageTypeValue] : false;
};

export const MessageReferenceTypes = {
	DEFAULT: 0,
	FORWARD: 1,
} as const;

export const MessageFlags = {
	SUPPRESS_EMBEDS: 1 << 2,
	SUPPRESS_NOTIFICATIONS: 1 << 12,
	COMPACT_ATTACHMENTS: 1 << 17,
} as const;

export const SENDABLE_MESSAGE_FLAGS =
	MessageFlags.SUPPRESS_EMBEDS | MessageFlags.SUPPRESS_NOTIFICATIONS | MessageFlags.COMPACT_ATTACHMENTS;

export const MessageAttachmentFlags = {
	IS_SPOILER: 1 << 3,
	CONTAINS_EXPLICIT_MEDIA: 1 << 4,
	IS_ANIMATED: 1 << 5,
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
