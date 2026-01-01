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

export enum AuditLogActionType {
	GUILD_UPDATE = 1,

	CHANNEL_CREATE = 10,
	CHANNEL_UPDATE = 11,
	CHANNEL_DELETE = 12,
	CHANNEL_OVERWRITE_CREATE = 13,
	CHANNEL_OVERWRITE_UPDATE = 14,
	CHANNEL_OVERWRITE_DELETE = 15,

	MEMBER_KICK = 20,
	MEMBER_PRUNE = 21,
	MEMBER_BAN_ADD = 22,
	MEMBER_BAN_REMOVE = 23,
	MEMBER_UPDATE = 24,
	MEMBER_ROLE_UPDATE = 25,
	MEMBER_MOVE = 26,
	MEMBER_DISCONNECT = 27,
	BOT_ADD = 28,

	ROLE_CREATE = 30,
	ROLE_UPDATE = 31,
	ROLE_DELETE = 32,

	INVITE_CREATE = 40,
	INVITE_UPDATE = 41,
	INVITE_DELETE = 42,

	WEBHOOK_CREATE = 50,
	WEBHOOK_UPDATE = 51,
	WEBHOOK_DELETE = 52,

	EMOJI_CREATE = 60,
	EMOJI_UPDATE = 61,
	EMOJI_DELETE = 62,

	STICKER_CREATE = 90,
	STICKER_UPDATE = 91,
	STICKER_DELETE = 92,

	MESSAGE_DELETE = 72,
	MESSAGE_BULK_DELETE = 73,
	MESSAGE_PIN = 74,
	MESSAGE_UNPIN = 75,
}

export const ALL_AUDIT_LOG_ACTION_TYPES: ReadonlyArray<AuditLogActionType> = [
	AuditLogActionType.GUILD_UPDATE,
	AuditLogActionType.CHANNEL_CREATE,
	AuditLogActionType.CHANNEL_UPDATE,
	AuditLogActionType.CHANNEL_DELETE,
	AuditLogActionType.CHANNEL_OVERWRITE_CREATE,
	AuditLogActionType.CHANNEL_OVERWRITE_UPDATE,
	AuditLogActionType.CHANNEL_OVERWRITE_DELETE,
	AuditLogActionType.MEMBER_KICK,
	AuditLogActionType.MEMBER_PRUNE,
	AuditLogActionType.MEMBER_BAN_ADD,
	AuditLogActionType.MEMBER_BAN_REMOVE,
	AuditLogActionType.MEMBER_UPDATE,
	AuditLogActionType.MEMBER_ROLE_UPDATE,
	AuditLogActionType.MEMBER_MOVE,
	AuditLogActionType.MEMBER_DISCONNECT,
	AuditLogActionType.BOT_ADD,
	AuditLogActionType.ROLE_CREATE,
	AuditLogActionType.ROLE_UPDATE,
	AuditLogActionType.ROLE_DELETE,
	AuditLogActionType.INVITE_CREATE,
	AuditLogActionType.INVITE_UPDATE,
	AuditLogActionType.INVITE_DELETE,
	AuditLogActionType.WEBHOOK_CREATE,
	AuditLogActionType.WEBHOOK_UPDATE,
	AuditLogActionType.WEBHOOK_DELETE,
	AuditLogActionType.EMOJI_CREATE,
	AuditLogActionType.EMOJI_UPDATE,
	AuditLogActionType.EMOJI_DELETE,
	AuditLogActionType.STICKER_CREATE,
	AuditLogActionType.STICKER_UPDATE,
	AuditLogActionType.STICKER_DELETE,
	AuditLogActionType.MESSAGE_DELETE,
	AuditLogActionType.MESSAGE_BULK_DELETE,
	AuditLogActionType.MESSAGE_PIN,
	AuditLogActionType.MESSAGE_UNPIN,
];
