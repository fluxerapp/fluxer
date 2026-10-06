// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	presentChannelCreate,
	presentChannelDelete,
	presentChannelOverwriteCreate,
	presentChannelOverwriteDelete,
	presentChannelOverwriteUpdate,
	presentChannelUpdate,
} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogChannelPresentation';
import {
	presentEmojiCreate,
	presentEmojiDelete,
	presentEmojiUpdate,
	presentMessageBulkDelete,
	presentMessageDelete,
	presentMessagePin,
	presentMessageUnpin,
	presentStickerCreate,
	presentStickerDelete,
	presentStickerUpdate,
} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogExpressionMessagePresentation';
import {presentGuildUpdate} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogGuildPresentation';
import {
	presentInviteCreate,
	presentInviteDelete,
	presentInviteUpdate,
	presentWebhookCreate,
	presentWebhookDelete,
	presentWebhookUpdate,
} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogInviteWebhookPresentation';
import {
	presentBotAdd,
	presentMemberBanAdd,
	presentMemberBanRemove,
	presentMemberDisconnect,
	presentMemberKick,
	presentMemberMove,
	presentMemberPrune,
	presentMemberRoleUpdate,
	presentMemberUpdate,
} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogMemberPresentation';
import type {
	AuditLogBlock,
	AuditLogDomainResult,
	AuditLogEntryPresentation,
	AuditLogPresentationContext,
	AuditLogPresenter,
} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogPresentationTypes';
import {
	presentRoleCreate,
	presentRoleDelete,
	presentRoleUpdate,
} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogRolePresentation';
import {UNKNOWN_ACTION_SUMMARY} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogSharedMessages';
import {actorPlaceholder, decodeAuditReason} from '@app/features/guild/utils/guild_tabs/audit_log/AuditLogValues';
import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import type {GuildAuditLogEntryResponse} from '@fluxer/schema/src/domains/guild/GuildAuditLogSchemas';

const PRESENTERS = {
	[AuditLogActionType.GUILD_UPDATE]: presentGuildUpdate,
	[AuditLogActionType.CHANNEL_CREATE]: presentChannelCreate,
	[AuditLogActionType.CHANNEL_UPDATE]: presentChannelUpdate,
	[AuditLogActionType.CHANNEL_DELETE]: presentChannelDelete,
	[AuditLogActionType.CHANNEL_OVERWRITE_CREATE]: presentChannelOverwriteCreate,
	[AuditLogActionType.CHANNEL_OVERWRITE_UPDATE]: presentChannelOverwriteUpdate,
	[AuditLogActionType.CHANNEL_OVERWRITE_DELETE]: presentChannelOverwriteDelete,
	[AuditLogActionType.MEMBER_KICK]: presentMemberKick,
	[AuditLogActionType.MEMBER_PRUNE]: presentMemberPrune,
	[AuditLogActionType.MEMBER_BAN_ADD]: presentMemberBanAdd,
	[AuditLogActionType.MEMBER_BAN_REMOVE]: presentMemberBanRemove,
	[AuditLogActionType.MEMBER_UPDATE]: presentMemberUpdate,
	[AuditLogActionType.MEMBER_ROLE_UPDATE]: presentMemberRoleUpdate,
	[AuditLogActionType.MEMBER_MOVE]: presentMemberMove,
	[AuditLogActionType.MEMBER_DISCONNECT]: presentMemberDisconnect,
	[AuditLogActionType.BOT_ADD]: presentBotAdd,
	[AuditLogActionType.ROLE_CREATE]: presentRoleCreate,
	[AuditLogActionType.ROLE_UPDATE]: presentRoleUpdate,
	[AuditLogActionType.ROLE_DELETE]: presentRoleDelete,
	[AuditLogActionType.INVITE_CREATE]: presentInviteCreate,
	[AuditLogActionType.INVITE_UPDATE]: presentInviteUpdate,
	[AuditLogActionType.INVITE_DELETE]: presentInviteDelete,
	[AuditLogActionType.WEBHOOK_CREATE]: presentWebhookCreate,
	[AuditLogActionType.WEBHOOK_UPDATE]: presentWebhookUpdate,
	[AuditLogActionType.WEBHOOK_DELETE]: presentWebhookDelete,
	[AuditLogActionType.EMOJI_CREATE]: presentEmojiCreate,
	[AuditLogActionType.EMOJI_UPDATE]: presentEmojiUpdate,
	[AuditLogActionType.EMOJI_DELETE]: presentEmojiDelete,
	[AuditLogActionType.STICKER_CREATE]: presentStickerCreate,
	[AuditLogActionType.STICKER_UPDATE]: presentStickerUpdate,
	[AuditLogActionType.STICKER_DELETE]: presentStickerDelete,
	[AuditLogActionType.MESSAGE_DELETE]: presentMessageDelete,
	[AuditLogActionType.MESSAGE_BULK_DELETE]: presentMessageBulkDelete,
	[AuditLogActionType.MESSAGE_PIN]: presentMessagePin,
	[AuditLogActionType.MESSAGE_UNPIN]: presentMessageUnpin,
} satisfies Record<AuditLogActionType, AuditLogPresenter>;

function presentUnknownAction(entry: GuildAuditLogEntryResponse): AuditLogDomainResult {
	return {
		summary: {descriptor: UNKNOWN_ACTION_SUMMARY, values: {actor: actorPlaceholder(entry)}},
		rows: [],
		blocks: [],
	};
}

export function presentAuditLogEntry(
	entry: GuildAuditLogEntryResponse,
	context: AuditLogPresentationContext,
): AuditLogEntryPresentation {
	const result = Object.hasOwn(PRESENTERS, entry.action_type)
		? PRESENTERS[entry.action_type](entry, context)
		: presentUnknownAction(entry);
	const reason = decodeAuditReason(entry.reason);
	const blocks: Array<AuditLogBlock> =
		reason === null ? result.blocks : [{kind: 'reason', text: reason}, ...result.blocks];
	return {
		summary: result.summary,
		rows: result.rows,
		blocks,
		expandable: result.rows.length > 0 || blocks.length > 0,
	};
}
