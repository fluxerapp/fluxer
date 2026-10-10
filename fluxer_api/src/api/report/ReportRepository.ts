// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ReportID, UserID} from '@app/api/BrandedTypes';
import {
	createApplicationID,
	createChannelID,
	createGuildID,
	createMessageID,
	createReportID,
	createUserID,
	createWebhookID,
} from '@app/api/BrandedTypes';
import {executeConditional, fetchMany, fetchOne, upsertOne} from '@app/api/database/CassandraQueryExecution';
import {Db} from '@app/api/database/CassandraTypes';
import type {
	DSAReportEmailVerificationRow,
	DSAReportTicketRow,
	GuildReportSubmissionByReporterRow,
	MessageReportSubmissionByReporterRow,
	UserReportSubmissionByReporterRow,
} from '@app/api/database/types/ReportTypes';
import {parseReportFlowSteps} from '@app/api/report/flows/ReportFlowRegistry';
import {
	type IARMessageContext,
	type IARMessageContextRow,
	type IARSubmission,
	type IARSubmissionRow,
	ReportStatus,
	ReportType,
} from '@app/api/report/ReportModels';
import {
	DSAReportEmailVerifications,
	DSAReportTickets,
	GuildReportSubmissionsByReporter,
	IARSubmissions,
	MessageReportSubmissionsByReporter,
	UserReportSubmissionsByReporter,
} from '@app/api/Tables';
import {ReportAlreadyResolvedError} from '@fluxer/errors/src/domains/moderation/ReportAlreadyResolvedError';
import {UnknownReportError} from '@fluxer/errors/src/domains/moderation/UnknownReportError';
import {parseReportProfileSnapshot} from '@fluxer/schema/src/domains/report/ReportProfileSnapshotSchemas';

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
const DSA_EMAIL_VERIFICATION_WRITE_ATTEMPTS = 5;
function createFetchAllReportsFirstPageQuery(limit: number) {
	return IARSubmissions.select({limit});
}

export class ReportRepository {
	async createReport(data: IARSubmissionRow): Promise<IARSubmission> {
		const row: IARSubmissionRow = {
			...data,
			reported_profile_snapshot: data.reported_profile_snapshot ?? null,
			legal_hold_until: data.legal_hold_until ?? null,
			legal_hold_reason: data.legal_hold_reason ?? null,
		};
		await upsertOne(IARSubmissions.insert(row));
		return this.mapRowToSubmission(row);
	}

	async reserveMessageReportByReporter(data: MessageReportSubmissionByReporterRow): Promise<boolean> {
		return executeConditional(MessageReportSubmissionsByReporter.insertIfNotExists(data));
	}

	async releaseMessageReportByReporter(data: MessageReportSubmissionByReporterRow): Promise<void> {
		await executeConditional(
			MessageReportSubmissionsByReporter.conditionalDeleteByPk(
				{reporter_id: data.reporter_id, channel_id: data.channel_id, message_id: data.message_id},
				{report_id: data.report_id},
			),
		);
	}

	async reserveUserReportByReporter(data: UserReportSubmissionByReporterRow): Promise<boolean> {
		return executeConditional(UserReportSubmissionsByReporter.insertIfNotExists(data));
	}

	async releaseUserReportByReporter(data: UserReportSubmissionByReporterRow): Promise<void> {
		await executeConditional(
			UserReportSubmissionsByReporter.conditionalDeleteByPk(
				{reporter_id: data.reporter_id, reported_user_id: data.reported_user_id},
				{report_id: data.report_id},
			),
		);
	}

	async reserveGuildReportByReporter(data: GuildReportSubmissionByReporterRow): Promise<boolean> {
		return executeConditional(GuildReportSubmissionsByReporter.insertIfNotExists(data));
	}

	async releaseGuildReportByReporter(data: GuildReportSubmissionByReporterRow): Promise<void> {
		await executeConditional(
			GuildReportSubmissionsByReporter.conditionalDeleteByPk(
				{reporter_id: data.reporter_id, reported_guild_id: data.reported_guild_id},
				{report_id: data.report_id},
			),
		);
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
		if (report.status !== ReportStatus.PENDING) {
			throw new ReportAlreadyResolvedError();
		}
		const resolvedAt = new Date();
		const newStatus = ReportStatus.RESOLVED;
		const resolved = await executeConditional(
			IARSubmissions.conditionalPatchByPk(
				{report_id: reportId},
				{
					resolved_at: Db.set(resolvedAt),
					resolved_by_admin_id: Db.set(resolvedByAdminId),
					public_comment: Db.set(publicComment),
					audit_log_reason: Db.set(auditLogReason),
					status: Db.set(newStatus),
				},
				{status: ReportStatus.PENDING},
			),
		);
		if (!resolved) {
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

	async setReportLegalHold(
		reportId: ReportID,
		legalHoldUntil: Date | null,
		legalHoldReason: string | null,
	): Promise<IARSubmission> {
		const report = await this.getReport(reportId);
		if (!report) {
			throw new UnknownReportError();
		}
		const written = await executeConditional(
			IARSubmissions.conditionalPatchByPk(
				{report_id: reportId},
				{legal_hold_until: Db.set(legalHoldUntil), legal_hold_reason: Db.set(legalHoldReason)},
				{report_type: report.reportType},
			),
		);
		if (!written) {
			throw new UnknownReportError();
		}
		return {...report, legalHoldUntil, legalHoldReason};
	}

	async clearReporterEmail(reportId: ReportID, reporterId: UserID): Promise<boolean> {
		return executeConditional(
			IARSubmissions.conditionalPatchByPk(
				{report_id: reportId},
				{reporter_email: Db.set(null)},
				{reporter_id: reporterId},
			),
		);
	}

	async deleteReport(reportId: ReportID): Promise<void> {
		const report = await this.getReport(reportId);
		if (!report) {
			return;
		}
		await this.releaseReporterReservation(report);
		await executeConditional(
			IARSubmissions.conditionalDeleteByPk({report_id: reportId}, {report_type: report.reportType}),
		);
	}

	private async releaseReporterReservation(report: IARSubmission): Promise<void> {
		const {reporterId, reportId, reportedAt} = report;
		if (!reporterId) {
			return;
		}
		const base = {reporter_id: reporterId, report_id: reportId, reported_at: reportedAt};
		if (report.reportType === ReportType.MESSAGE && report.reportedChannelId && report.reportedMessageId) {
			await this.releaseMessageReportByReporter({
				...base,
				channel_id: report.reportedChannelId,
				message_id: report.reportedMessageId,
			});
		} else if (report.reportType === ReportType.USER && report.reportedUserId) {
			await this.releaseUserReportByReporter({...base, reported_user_id: report.reportedUserId});
		} else if (report.reportType === ReportType.GUILD && report.reportedGuildId) {
			await this.releaseGuildReportByReporter({...base, reported_guild_id: report.reportedGuildId});
		}
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
			reportedGuildNsfw: row.reported_guild_nsfw ?? null,
			reportedGuildContentWarningLevel: row.reported_guild_content_warning_level ?? null,
			reportedGuildContentWarningText: row.reported_guild_content_warning_text ?? null,
			reportedChannelNsfwOverride: row.reported_channel_nsfw_override ?? null,
			reportedChannelContentWarningLevel: row.reported_channel_content_warning_level ?? null,
			reportedChannelContentWarningText: row.reported_channel_content_warning_text ?? null,
			reportedChannelEffectiveNsfw: row.reported_channel_effective_nsfw ?? null,
			reportedChannelEffectiveContentWarningLevel: row.reported_channel_effective_content_warning_level ?? null,
			reportedChannelEffectiveContentWarningText: row.reported_channel_effective_content_warning_text ?? null,
			reason: row.reason ?? null,
			flowRevision: row.flow_revision ?? null,
			flowSteps: parseReportFlowSteps(row.flow_steps),
			flowLocale: row.flow_locale ?? null,
			flowSurface: row.flow_surface ?? null,
			reporterGoodFaithConfirmed: row.reporter_good_faith_confirmed ?? null,
			reportedWebhookId: row.reported_webhook_id ? createWebhookID(row.reported_webhook_id) : null,
			reportedWebhookName: row.reported_webhook_name ?? null,
			reportedWebhookAvatarHash: row.reported_webhook_avatar_hash ?? null,
			reportedWebhookDefaultName: row.reported_webhook_default_name ?? null,
			reportedWebhookDefaultAvatarHash: row.reported_webhook_default_avatar_hash ?? null,
			reportedWebhookType: row.reported_webhook_type ?? null,
			reportedWebhookApplicationId: row.reported_webhook_application_id
				? createApplicationID(row.reported_webhook_application_id)
				: null,
			reportedWebhookChannelId: row.reported_webhook_channel_id
				? createChannelID(row.reported_webhook_channel_id)
				: null,
			reportedWebhookGuildId: row.reported_webhook_guild_id ? createGuildID(row.reported_webhook_guild_id) : null,
			reportedWebhookCreatedAt: row.reported_webhook_created_at ?? null,
			reportedWebhookCreatorId: row.reported_webhook_creator_id ? createUserID(row.reported_webhook_creator_id) : null,
			reportedWebhookCreatorUsername: row.reported_webhook_creator_username ?? null,
			reportedWebhookCreatorDiscriminator: row.reported_webhook_creator_discriminator ?? null,
			reportedWebhookCreatorGlobalName: row.reported_webhook_creator_global_name ?? null,
			reportedWebhookCreatorAvatarHash: row.reported_webhook_creator_avatar_hash ?? null,
			reportedProfileSnapshot: parseReportProfileSnapshot(row.reported_profile_snapshot),
			legalHoldUntil: row.legal_hold_until ?? null,
			legalHoldReason: row.legal_hold_reason ?? null,
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
		for (let attempt = 0; attempt < DSA_EMAIL_VERIFICATION_WRITE_ATTEMPTS; attempt++) {
			const current = await this.getDsaEmailVerification(row.email_lower);
			const written = await executeConditional(
				current
					? DSAReportEmailVerifications.conditionalPatchByPk(
							{email_lower: row.email_lower},
							{
								code_hash: Db.set(row.code_hash),
								expires_at: Db.set(row.expires_at),
								last_sent_at: Db.set(row.last_sent_at),
							},
							{code_hash: current.code_hash},
						)
					: DSAReportEmailVerifications.insertIfNotExists(row),
			);
			if (written) return;
		}
		throw new Error('DSA email verification kept changing during write');
	}

	async getDsaEmailVerification(emailLower: string): Promise<DSAReportEmailVerificationRow | null> {
		const row = await fetchOne<DSAReportEmailVerificationRow>(
			GET_DSA_EMAIL_VERIFICATION_QUERY.bind({email_lower: emailLower}),
		);
		return row ?? null;
	}

	async consumeDsaEmailVerification(emailLower: string, codeHash: string): Promise<boolean> {
		return executeConditional(
			DSAReportEmailVerifications.conditionalDeleteByPk({email_lower: emailLower}, {code_hash: codeHash}),
		);
	}

	async createDsaTicket(row: DSAReportTicketRow): Promise<void> {
		const created = await executeConditional(DSAReportTickets.insertIfNotExists(row));
		if (!created) {
			throw new Error('DSA ticket already exists');
		}
	}

	async getDsaTicket(ticket: string): Promise<DSAReportTicketRow | null> {
		const row = await fetchOne<DSAReportTicketRow>(GET_DSA_REPORT_TICKET_QUERY.bind({ticket}));
		return row ?? null;
	}

	async consumeDsaTicket(ticket: string, emailLower: string): Promise<boolean> {
		return executeConditional(DSAReportTickets.conditionalDeleteByPk({ticket}, {email_lower: emailLower}));
	}

	private mapMessageContext(rawContext: Array<IARMessageContextRow>): Array<IARMessageContext> {
		const toBigintArray = (collection: ReadonlyArray<bigint> | Set<bigint> | null | undefined): Array<bigint> =>
			collection ? Array.from(collection) : [];
		return rawContext.map((msg) => ({
			messageId: createMessageID(msg.message_id),
			authorId: msg.author_id ? createUserID(msg.author_id) : null,
			webhookId: msg.webhook_id ? createWebhookID(msg.webhook_id) : null,
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
			missingAttachments: msg.missing_attachments ?? [],
		}));
	}
}
