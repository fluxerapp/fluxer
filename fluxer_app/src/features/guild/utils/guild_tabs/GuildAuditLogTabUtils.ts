// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	AuditLogActionKind,
	AuditLogTargetType,
} from '@app/features/guild/components/modals/guild_tabs/GuildAuditLogTabConstants';
import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';

const targetTypeMap: Partial<Record<AuditLogActionType, AuditLogTargetType>> = {
	[AuditLogActionType.GUILD_UPDATE]: AuditLogTargetType.GUILD,
	[AuditLogActionType.CHANNEL_CREATE]: AuditLogTargetType.CHANNEL,
	[AuditLogActionType.CHANNEL_UPDATE]: AuditLogTargetType.CHANNEL,
	[AuditLogActionType.CHANNEL_DELETE]: AuditLogTargetType.CHANNEL,
	[AuditLogActionType.CHANNEL_OVERWRITE_CREATE]: AuditLogTargetType.CHANNEL,
	[AuditLogActionType.CHANNEL_OVERWRITE_UPDATE]: AuditLogTargetType.CHANNEL,
	[AuditLogActionType.CHANNEL_OVERWRITE_DELETE]: AuditLogTargetType.CHANNEL,
	[AuditLogActionType.MEMBER_KICK]: AuditLogTargetType.USER,
	[AuditLogActionType.MEMBER_PRUNE]: AuditLogTargetType.USER,
	[AuditLogActionType.MEMBER_BAN_ADD]: AuditLogTargetType.USER,
	[AuditLogActionType.MEMBER_BAN_REMOVE]: AuditLogTargetType.USER,
	[AuditLogActionType.MEMBER_UPDATE]: AuditLogTargetType.USER,
	[AuditLogActionType.MEMBER_ROLE_UPDATE]: AuditLogTargetType.USER,
	[AuditLogActionType.MEMBER_MOVE]: AuditLogTargetType.USER,
	[AuditLogActionType.MEMBER_DISCONNECT]: AuditLogTargetType.USER,
	[AuditLogActionType.BOT_ADD]: AuditLogTargetType.USER,
	[AuditLogActionType.ROLE_CREATE]: AuditLogTargetType.ROLE,
	[AuditLogActionType.ROLE_UPDATE]: AuditLogTargetType.ROLE,
	[AuditLogActionType.ROLE_DELETE]: AuditLogTargetType.ROLE,
	[AuditLogActionType.INVITE_CREATE]: AuditLogTargetType.INVITE,
	[AuditLogActionType.INVITE_UPDATE]: AuditLogTargetType.INVITE,
	[AuditLogActionType.INVITE_DELETE]: AuditLogTargetType.INVITE,
	[AuditLogActionType.WEBHOOK_CREATE]: AuditLogTargetType.WEBHOOK,
	[AuditLogActionType.WEBHOOK_UPDATE]: AuditLogTargetType.WEBHOOK,
	[AuditLogActionType.WEBHOOK_DELETE]: AuditLogTargetType.WEBHOOK,
	[AuditLogActionType.EMOJI_CREATE]: AuditLogTargetType.EMOJI,
	[AuditLogActionType.EMOJI_UPDATE]: AuditLogTargetType.EMOJI,
	[AuditLogActionType.EMOJI_DELETE]: AuditLogTargetType.EMOJI,
	[AuditLogActionType.STICKER_CREATE]: AuditLogTargetType.STICKER,
	[AuditLogActionType.STICKER_UPDATE]: AuditLogTargetType.STICKER,
	[AuditLogActionType.STICKER_DELETE]: AuditLogTargetType.STICKER,
	[AuditLogActionType.MESSAGE_DELETE]: AuditLogTargetType.MESSAGE,
	[AuditLogActionType.MESSAGE_BULK_DELETE]: AuditLogTargetType.MESSAGE,
	[AuditLogActionType.MESSAGE_PIN]: AuditLogTargetType.MESSAGE,
	[AuditLogActionType.MESSAGE_UNPIN]: AuditLogTargetType.MESSAGE,
};

export function getTargetType(actionType: AuditLogActionType): AuditLogTargetType {
	return targetTypeMap[actionType] ?? AuditLogTargetType.ALL;
}

const createActions = new Set<AuditLogActionType>([
	AuditLogActionType.CHANNEL_CREATE,
	AuditLogActionType.CHANNEL_OVERWRITE_CREATE,
	AuditLogActionType.ROLE_CREATE,
	AuditLogActionType.INVITE_CREATE,
	AuditLogActionType.WEBHOOK_CREATE,
	AuditLogActionType.EMOJI_CREATE,
	AuditLogActionType.STICKER_CREATE,
	AuditLogActionType.BOT_ADD,
	AuditLogActionType.MESSAGE_PIN,
]);
const updateActions = new Set<AuditLogActionType>([
	AuditLogActionType.GUILD_UPDATE,
	AuditLogActionType.CHANNEL_UPDATE,
	AuditLogActionType.CHANNEL_OVERWRITE_UPDATE,
	AuditLogActionType.MEMBER_UPDATE,
	AuditLogActionType.MEMBER_ROLE_UPDATE,
	AuditLogActionType.ROLE_UPDATE,
	AuditLogActionType.INVITE_UPDATE,
	AuditLogActionType.WEBHOOK_UPDATE,
	AuditLogActionType.EMOJI_UPDATE,
	AuditLogActionType.STICKER_UPDATE,
	AuditLogActionType.MEMBER_MOVE,
	AuditLogActionType.MEMBER_DISCONNECT,
]);

export function getActionKind(actionType: AuditLogActionType): AuditLogActionKind {
	if (createActions.has(actionType)) return AuditLogActionKind.CREATE;
	if (updateActions.has(actionType)) return AuditLogActionKind.UPDATE;
	return AuditLogActionKind.DELETE;
}
