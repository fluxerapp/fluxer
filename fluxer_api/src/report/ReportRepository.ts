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

import type {ReportID, UserID} from '~/BrandedTypes';
import {createChannelID, createGuildID, createMessageID, createReportID, createUserID} from '~/BrandedTypes';
import {Db, executeConditional, fetchMany, fetchOne, upsertOne} from '~/database/Cassandra';
import type {DSAReportEmailVerificationRow, DSAReportTicketRow} from '~/database/types/ReportTypes';
import {ReportAlreadyResolvedError, UnknownReportError} from '~/Errors';
import {DSAReportEmailVerifications, DSAReportTickets, IARSubmissions} from '~/Tables';
import type {
	IARMessageContext,
	IARMessageContextRow,
	IARSubmission,
	IARSubmissionRow,
	IReportRepository,
} from './IReportRepository';

const GET_REPORT_QUERY = IARSubmissions.select({
	where: IARSubmissions.where.eq('report_id'),
	limit: 1,
});

const createFetchAllReportsPaginatedQuery = (limit: number) =>
	IARSubmissions.select({
		where: IARSubmissions.where.tokenGt('report_id', 'last_report_id'),
		limit,
	});

const GET_DSA_EMAIL_VERIFICATION_QUERY = DSAReportEmailVerifications.select({
	where: DSAReportEmailVerifications.where.eq('email_lower'),
	limit: 1,
});

const GET_DSA_REPORT_TICKET_QUERY = DSAReportTickets.select({
	where: DSAReportTickets.where.eq('ticket'),
	limit: 1,
});

const createFetchAllReportsFirstPageQuery = (limit: number) => IARSubmissions.select({limit});

export class ReportRepository implements IReportRepository {
	async createReport(data: IARSubmissionRow): Promise<IARSubmission> {
		await upsertOne(IARSubmissions.insert(data));
		return this.mapRowToSubmission(data);
	}

	async getReport(reportId: ReportID): Promise<IARSubmission | null> {
		const row = await fetchOne<IARSubmissionRow>(GET_REPORT_QUERY.bind({report_id: reportId}));
		return row ? this.mapRowToSubmission(row) : null;
	}

	async resolveReport(
		reportId: ReportID,
		resolvedByAdminId: UserID,
		publicComment: string | null,
		auditLogReason: string | null,
	): Promise<IARSubmission> {
		const report = await this.getReport(reportId);
		if (!report) {
			throw new UnknownReportError();
		}

		const resolvedAt = new Date();
		const newStatus = 1;

		const q = IARSubmissions.patchByPkIf(
			{report_id: reportId},
			{
				resolved_at: Db.set(resolvedAt),
				resolved_by_admin_id: Db.set(resolvedByAdminId),
				public_comment: Db.set(publicComment),
				audit_log_reason: Db.set(auditLogReason),
				status: Db.set(newStatus),
			},
			{col: 'status', expectedParam: 'expected_status', expectedValue: 0},
		);

		const result = await executeConditional(q);
		if (!result.applied) {
			throw new ReportAlreadyResolvedError();
		}

		return {
			...report,
			resolvedAt,
			resolvedByAdminId,
			publicComment,
			auditLogReason,
			status: newStatus,
		};
	}

	private mapRowToSubmission(row: IARSubmissionRow): IARSubmission {
		return {
			reportId: createReportID(row.report_id),
			reporterId: row.reporter_id ? createUserID(row.reporter_id) : null,
			reporterEmail: row.reporter_email,
			reporterFullLegalName: row.reporter_full_legal_name,
			reporterCountryOfResidence: row.reporter_country_of_residence,
			reportedAt: row.reported_at,
			status: row.status,
			reportType: row.report_type,
			category: row.category,
			additionalInfo: row.additional_info,
			reportedUserId: row.reported_user_id ? createUserID(row.reported_user_id) : null,
			reportedUserAvatarHash: row.reported_user_avatar_hash,
			reportedGuildId: row.reported_guild_id ? createGuildID(row.reported_guild_id) : null,
			reportedGuildName: row.reported_guild_name,
			reportedGuildIconHash: row.reported_guild_icon_hash,
			reportedMessageId: row.reported_message_id ? createMessageID(row.reported_message_id) : null,
			reportedChannelId: row.reported_channel_id ? createChannelID(row.reported_channel_id) : null,
			reportedChannelName: row.reported_channel_name,
			messageContext: row.message_context ? this.mapMessageContext(row.message_context) : null,
			guildContextId: row.guild_context_id ? createGuildID(row.guild_context_id) : null,
			resolvedAt: row.resolved_at,
			resolvedByAdminId: row.resolved_by_admin_id ? createUserID(row.resolved_by_admin_id) : null,
			publicComment: row.public_comment,
			auditLogReason: row.audit_log_reason,
			reportedGuildInviteCode: row.reported_guild_invite_code,
		};
	}

	async listAllReportsPaginated(limit: number, lastReportId?: ReportID): Promise<Array<IARSubmission>> {
		let reports: Array<IARSubmissionRow>;

		if (lastReportId) {
			const query = createFetchAllReportsPaginatedQuery(limit);
			reports = await fetchMany<IARSubmissionRow>(query.bind({last_report_id: lastReportId}));
		} else {
			const query = createFetchAllReportsFirstPageQuery(limit);
			reports = await fetchMany<IARSubmissionRow>(query.bind({}));
		}

		return reports.map((report) => this.mapRowToSubmission(report));
	}

	async upsertDsaEmailVerification(row: DSAReportEmailVerificationRow): Promise<void> {
		await upsertOne(DSAReportEmailVerifications.insert(row));
	}

	async getDsaEmailVerification(emailLower: string): Promise<DSAReportEmailVerificationRow | null> {
		const row = await fetchOne<DSAReportEmailVerificationRow>(
			GET_DSA_EMAIL_VERIFICATION_QUERY.bind({email_lower: emailLower}),
		);
		return row ?? null;
	}

	async deleteDsaEmailVerification(emailLower: string): Promise<void> {
		await DSAReportEmailVerifications.deleteByPk({email_lower: emailLower});
	}

	async createDsaTicket(row: DSAReportTicketRow): Promise<void> {
		await upsertOne(DSAReportTickets.insert(row));
	}

	async getDsaTicket(ticket: string): Promise<DSAReportTicketRow | null> {
		const row = await fetchOne<DSAReportTicketRow>(GET_DSA_REPORT_TICKET_QUERY.bind({ticket}));
		return row ?? null;
	}

	async deleteDsaTicket(ticket: string): Promise<void> {
		await DSAReportTickets.deleteByPk({ticket});
	}

	private mapMessageContext(rawContext: Array<IARMessageContextRow>): Array<IARMessageContext> {
		const toBigintArray = (collection: ReadonlyArray<bigint> | Set<bigint> | null | undefined): Array<bigint> =>
			collection ? Array.from(collection) : [];

		return rawContext.map((msg) => ({
			messageId: createMessageID(msg.message_id),
			authorId: createUserID(msg.author_id),
			channelId: msg.channel_id ? createChannelID(msg.channel_id) : null,
			authorUsername: msg.author_username,
			authorDiscriminator: msg.author_discriminator,
			authorAvatarHash: msg.author_avatar_hash,
			content: msg.content,
			timestamp: msg.timestamp,
			editedTimestamp: msg.edited_timestamp,
			type: msg.type,
			flags: msg.flags,
			mentionEveryone: msg.mention_everyone,
			mentionUsers: toBigintArray(msg.mention_users),
			mentionRoles: toBigintArray(msg.mention_roles),
			mentionChannels: toBigintArray(msg.mention_channels),
			attachments: msg.attachments ?? [],
			embeds: msg.embeds ?? [],
			stickers: msg.sticker_items ?? [],
		}));
	}
}
