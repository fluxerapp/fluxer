// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID, MessageID, ReportID, UserID} from '../BrandedTypes';
import type {MessageAttachment, MessageEmbed, MessageStickerItem} from '../database/types/MessageTypes';
import type {
	DSAReportEmailVerificationRow,
	DSAReportTicketRow,
	IARSubmissionRow,
	MessageReportSubmissionByReporterRow,
} from '../database/types/ReportTypes';

export type {IARMessageContextRow, IARSubmissionRow} from '../database/types/ReportTypes';

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
	authorId: UserID;
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
}

export abstract class IReportRepository {
	abstract createReport(data: IARSubmissionRow): Promise<IARSubmission>;

	abstract reserveMessageReportByReporter(data: MessageReportSubmissionByReporterRow): Promise<boolean>;

	abstract deleteMessageReportByReporter(reporterId: UserID, channelId: ChannelID, messageId: MessageID): Promise<void>;

	abstract getReport(reportId: ReportID): Promise<IARSubmission | null>;

	abstract resolveReport(
		reportId: ReportID,
		resolvedByAdminId: UserID,
		publicComment: string | null,
		auditLogReason: string | null,
	): Promise<IARSubmission>;

	abstract listAllReportsPaginated(limit: number, lastReportId?: ReportID): Promise<Array<IARSubmission>>;

	abstract upsertDsaEmailVerification(row: DSAReportEmailVerificationRow): Promise<void>;

	abstract deleteDsaEmailVerification(emailLower: string): Promise<void>;

	abstract getDsaEmailVerification(emailLower: string): Promise<DSAReportEmailVerificationRow | null>;

	abstract createDsaTicket(row: DSAReportTicketRow): Promise<void>;

	abstract getDsaTicket(ticket: string): Promise<DSAReportTicketRow | null>;

	abstract deleteDsaTicket(ticket: string): Promise<void>;
}
