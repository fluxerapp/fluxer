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

import type {ChannelID, GuildID, MessageID, ReportID, UserID} from '~/BrandedTypes';
import type {MessageAttachment, MessageEmbed, MessageStickerItem} from '~/database/types/MessageTypes';
import type {DSAReportEmailVerificationRow, DSAReportTicketRow} from '~/database/types/ReportTypes';

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

type MentionCollection = ReadonlyArray<bigint> | Set<bigint> | null | undefined;

export interface IARMessageContextRow {
	message_id: bigint;
	channel_id: bigint | null;
	author_id: bigint;
	author_username: string;
	author_discriminator: number;
	author_avatar_hash: string | null;
	content: string | null;
	timestamp: Date;
	edited_timestamp: Date | null;
	type: number;
	flags: number;
	mention_everyone: boolean;
	mention_users: MentionCollection;
	mention_roles: MentionCollection;
	mention_channels: MentionCollection;
	attachments: Array<MessageAttachment> | null;
	embeds: Array<MessageEmbed> | null;
	sticker_items: Array<MessageStickerItem> | null;
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

export interface IARSubmissionRow {
	report_id: bigint;
	reporter_id: bigint | null;
	reporter_email: string | null;
	reporter_full_legal_name: string | null;
	reporter_country_of_residence: string | null;
	reported_at: Date;
	status: number;
	report_type: number;
	category: string;
	additional_info: string | null;
	reported_user_id: bigint | null;
	reported_user_avatar_hash: string | null;
	reported_guild_id: bigint | null;
	reported_guild_name: string | null;
	reported_guild_icon_hash: string | null;
	reported_message_id: bigint | null;
	reported_channel_id: bigint | null;
	reported_channel_name: string | null;
	message_context: Array<IARMessageContextRow> | null;
	guild_context_id: bigint | null;
	resolved_at: Date | null;
	resolved_by_admin_id: bigint | null;
	public_comment: string | null;
	audit_log_reason: string | null;
	reported_guild_invite_code: string | null;
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
}

export abstract class IReportRepository {
	abstract createReport(data: IARSubmissionRow): Promise<IARSubmission>;
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
