// SPDX-License-Identifier: AGPL-3.0-or-later

export const LOG_PAGE_SIZE = 50;

export enum AuditLogTargetType {
	ALL = 'all',
	GUILD = 'guild',
	CHANNEL = 'channel',
	USER = 'user',
	ROLE = 'role',
	INVITE = 'invite',
	WEBHOOK = 'webhook',
	EMOJI = 'emoji',
	STICKER = 'sticker',
	MESSAGE = 'message',
}

export enum AuditLogActionKind {
	CREATE = 'create',
	UPDATE = 'update',
	DELETE = 'delete',
}
