// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApplicationID, ChannelID, GuildID, MessageID, ReportID, UserID, WebhookID} from '@app/api/BrandedTypes';
import type {MessageAttachment, MessageEmbed, MessageStickerItem} from '@app/api/database/types/MessageTypes';
import type {ReportFlowStepRecord} from '@app/api/report/flows/ReportFlowRegistry';
import type {ReportProfileSnapshot} from '@fluxer/schema/src/domains/report/ReportProfileSnapshotSchemas';

export type {IARMessageContextRow, IARSubmissionRow} from '@app/api/database/types/ReportTypes';

export enum ReportStatus {
	PENDING = 0,
	RESOLVED = 1,
}

export enum ReportType {
	MESSAGE = 0,
	USER = 1,
	GUILD = 2,
}

const REPORT_STATUS_STRINGS: Record<ReportStatus, string> = {
	[ReportStatus.PENDING]: 'pending',
	[ReportStatus.RESOLVED]: 'resolved',
};

export function reportStatusToString(status: ReportStatus | number): string {
	return REPORT_STATUS_STRINGS[status as ReportStatus] ?? 'unknown';
}

export interface IARMessageContext {
	messageId: MessageID;
	channelId: ChannelID | null;
	authorId: UserID | null;
	webhookId: WebhookID | null;
	authorUsername: string;
	authorDiscriminator: number;
	authorAvatarHash: string | null;
	content: string | null;
	timestamp: Date;
	editedTimestamp: Date | null;
	type: number;
	flags: number;
	mentionEveryone: boolean;
	mentionUsers: Array<bigint>;
	mentionRoles: Array<bigint>;
	mentionChannels: Array<bigint>;
	attachments: Array<MessageAttachment>;
	embeds: Array<MessageEmbed>;
	stickers: Array<MessageStickerItem>;
	missingAttachments: Array<MessageAttachment>;
}

export interface IARSubmission {
	reportId: ReportID;
	reporterId: UserID | null;
	reporterEmail: string | null;
	reporterFullLegalName: string | null;
	reporterCountryOfResidence: string | null;
	reportedAt: Date;
	status: number;
	reportType: number;
	category: string;
	additionalInfo: string | null;
	reportedUserId: UserID | null;
	reportedUserAvatarHash: string | null;
	reportedGuildId: GuildID | null;
	reportedGuildName: string | null;
	reportedGuildIconHash: string | null;
	reportedMessageId: MessageID | null;
	reportedChannelId: ChannelID | null;
	reportedChannelName: string | null;
	messageContext: Array<IARMessageContext> | null;
	guildContextId: GuildID | null;
	resolvedAt: Date | null;
	resolvedByAdminId: UserID | null;
	publicComment: string | null;
	auditLogReason: string | null;
	reportedGuildInviteCode: string | null;
	reportedGuildNsfw: boolean | null;
	reportedGuildContentWarningLevel: number | null;
	reportedGuildContentWarningText: string | null;
	reportedChannelNsfwOverride: boolean | null;
	reportedChannelContentWarningLevel: number | null;
	reportedChannelContentWarningText: string | null;
	reportedChannelEffectiveNsfw: boolean | null;
	reportedChannelEffectiveContentWarningLevel: number | null;
	reportedChannelEffectiveContentWarningText: string | null;
	reason: string | null;
	flowRevision: string | null;
	flowSteps: Array<ReportFlowStepRecord> | null;
	flowLocale: string | null;
	flowSurface: string | null;
	reporterGoodFaithConfirmed: boolean | null;
	reportedWebhookId: WebhookID | null;
	reportedWebhookName: string | null;
	reportedWebhookAvatarHash: string | null;
	reportedWebhookDefaultName: string | null;
	reportedWebhookDefaultAvatarHash: string | null;
	reportedWebhookType: number | null;
	reportedWebhookApplicationId: ApplicationID | null;
	reportedWebhookChannelId: ChannelID | null;
	reportedWebhookGuildId: GuildID | null;
	reportedWebhookCreatedAt: Date | null;
	reportedWebhookCreatorId: UserID | null;
	reportedWebhookCreatorUsername: string | null;
	reportedWebhookCreatorDiscriminator: number | null;
	reportedWebhookCreatorGlobalName: string | null;
	reportedWebhookCreatorAvatarHash: string | null;
	reportedProfileSnapshot: ReportProfileSnapshot | null;
	legalHoldUntil: Date | null;
	legalHoldReason: string | null;
}
