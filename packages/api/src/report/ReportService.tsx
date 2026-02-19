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

import {createHash, randomBytes} from 'node:crypto';
import type {ChannelID, GuildID, MessageID, ReportID, UserID} from '@fluxer/api/src/BrandedTypes';
import {
	createChannelID,
	createGuildID,
	createInviteCode,
	createMessageID,
	createReportID,
	createUserID,
} from '@fluxer/api/src/BrandedTypes';
import {Config} from '@fluxer/api/src/Config';
import type {IChannelRepository} from '@fluxer/api/src/channel/IChannelRepository';
import * as MessageHelpers from '@fluxer/api/src/channel/services/message/MessageHelpers';
import type {MessageAttachment} from '@fluxer/api/src/database/types/MessageTypes';
import type {DSAReportTicketRow} from '@fluxer/api/src/database/types/ReportTypes';
import type {IGuildRepositoryAggregate} from '@fluxer/api/src/guild/repositories/IGuildRepositoryAggregate';
import type {IEmailDnsValidationService} from '@fluxer/api/src/infrastructure/IEmailDnsValidationService';
import type {IStorageService} from '@fluxer/api/src/infrastructure/IStorageService';
import type {SnowflakeService} from '@fluxer/api/src/infrastructure/SnowflakeService';
import type {IInviteRepository} from '@fluxer/api/src/invite/IInviteRepository';
import {Logger} from '@fluxer/api/src/Logger';
import type {Attachment} from '@fluxer/api/src/models/Attachment';
import type {User} from '@fluxer/api/src/models/User';
import type {
	IARMessageContextRow,
	IARSubmission,
	IARSubmissionRow,
	IReportRepository,
} from '@fluxer/api/src/report/IReportRepository';
import {ReportStatus, ReportType} from '@fluxer/api/src/report/IReportRepository';
import type {IReportSearchService} from '@fluxer/api/src/search/IReportSearchService';
import type {IUserRepository} from '@fluxer/api/src/user/IUserRepository';
import {InviteTypes} from '@fluxer/constants/src/ChannelConstants';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import type {IEmailService} from '@fluxer/email/src/IEmailService';
import {CannotReportOwnMessageError} from '@fluxer/errors/src/domains/channel/CannotReportOwnMessageError';
import {UnknownChannelError} from '@fluxer/errors/src/domains/channel/UnknownChannelError';
import {UnknownMessageError} from '@fluxer/errors/src/domains/channel/UnknownMessageError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {RateLimitError} from '@fluxer/errors/src/domains/core/RateLimitError';
import {CannotReportOwnGuildError} from '@fluxer/errors/src/domains/guild/CannotReportOwnGuildError';
import {UnknownGuildError} from '@fluxer/errors/src/domains/guild/UnknownGuildError';
import {UnknownInviteError} from '@fluxer/errors/src/domains/invite/UnknownInviteError';
import {CannotReportYourselfError} from '@fluxer/errors/src/domains/moderation/CannotReportYourselfError';
import {InvalidDsaReportTargetError} from '@fluxer/errors/src/domains/moderation/InvalidDsaReportTargetError';
import {InvalidDsaTicketError} from '@fluxer/errors/src/domains/moderation/InvalidDsaTicketError';
import {InvalidDsaVerificationCodeError} from '@fluxer/errors/src/domains/moderation/InvalidDsaVerificationCodeError';
import {ReportBannedError} from '@fluxer/errors/src/domains/moderation/ReportBannedError';
import {UnknownReportError} from '@fluxer/errors/src/domains/moderation/UnknownReportError';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import type {DsaReportRequest} from '@fluxer/schema/src/domains/report/ReportSchemas';
import {snowflakeToDate} from '@fluxer/snowflake/src/Snowflake';
import {recordCounter} from '@fluxer/telemetry/src/Metrics';
import {ms} from 'itty-time';

interface ReporterMetadata {
	id: UserID | null;
	email: string | null;
	fullLegalName: string | null;
	countryOfResidence: string | null;
}

const REPORT_RATE_LIMIT_WINDOW = ms('1 hour');
const REPORT_RATE_LIMIT_MAX = 5;
const MESSAGE_CONTEXT_WINDOW = 25;

const DSA_CODE_CHARSET = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
const DSA_CODE_SEGMENT_LENGTH = 4;
const DSA_CODE_SEPARATOR = '-';
const DSA_TICKET_BYTES = 32;

export class ReportService {
	private reportRateLimitMap = new Map<string, Array<number>>();
	private cleanupInterval: NodeJS.Timeout;

	constructor(
		private reportRepository: IReportRepository,
		private channelRepository: IChannelRepository,
		private guildRepository: IGuildRepositoryAggregate,
		private userRepository: IUserRepository,
		private inviteRepository: IInviteRepository,
		private emailService: IEmailService,
		private emailDnsValidationService: IEmailDnsValidationService,
		private snowflakeService: SnowflakeService,
		private storageService: IStorageService,
		private reportSearchService: IReportSearchService | null = null,
	) {
		this.cleanupInterval = setInterval(() => this.cleanupRateLimitMap(), ms('5 minutes'));
	}

	async reportMessage(
		reporter: ReporterMetadata,
		channelId: ChannelID,
		messageId: MessageID,
		category: string,
		additionalInfo?: string,
	): Promise<IARSubmission> {
		await this.checkReportBan(reporter.id);
		const reporterKey = this.getReporterRateLimitKey(reporter);
		await this.checkRateLimit(reporterKey);

		const channel = await this.channelRepository.findUnique(channelId);
		if (!channel) {
			throw new UnknownChannelError();
		}

		const message = await this.channelRepository.getMessage(channelId, messageId);
		if (!message) {
			throw new UnknownMessageError();
		}

		if (reporter.id && message.authorId === reporter.id) {
			throw new CannotReportOwnMessageError();
		}

		const [reportedUser, messageContext] = await Promise.all([
			this.userRepository.findUnique(message.authorId!),
			this.gatherMessageContext(channelId, messageId),
		]);

		if (!reportedUser) {
			throw new UnknownUserError();
		}

		const reportId = createReportID(await this.snowflakeService.generate());
		const reportData: IARSubmissionRow = {
			report_id: reportId,
			reporter_id: reporter.id,
			reporter_email: reporter.email,
			reporter_full_legal_name: reporter.fullLegalName,
			reporter_country_of_residence: reporter.countryOfResidence,
			reported_at: new Date(),
			status: ReportStatus.PENDING,
			report_type: ReportType.MESSAGE,
			category,
			additional_info: additionalInfo || null,
			reported_user_id: message.authorId,
			reported_user_avatar_hash: reportedUser.avatarHash || null,
			reported_guild_id: channel.guildId || null,
			reported_guild_name: null,
			reported_guild_icon_hash: null,
			reported_message_id: messageId,
			reported_channel_id: channelId,
			reported_channel_name: channel.name || null,
			message_context: messageContext,
			guild_context_id: channel.guildId || null,
			resolved_at: null,
			resolved_by_admin_id: null,
			public_comment: null,
			audit_log_reason: null,
			reported_guild_invite_code: null,
		};

		if (channel.guildId) {
			const guild = await this.guildRepository.findUnique(channel.guildId);
			if (guild) {
				reportData.reported_guild_name = guild.name;
				reportData.reported_guild_icon_hash = guild.iconHash || null;
			}
		}

		this.trackRateLimit(reporterKey);

		const report = await this.reportRepository.createReport(reportData);

		emitReportMetric('reports.iar.created', report);

		if (this.reportSearchService && 'indexReport' in this.reportSearchService) {
			await this.reportSearchService.indexReport(report).catch((error) => {
				Logger.error({error, reportId: report.reportId}, 'Failed to index message report in search');
			});
		}

		return report;
	}

	async reportUser(
		reporter: ReporterMetadata,
		reportedUserId: UserID,
		category: string,
		additionalInfo?: string,
		guildId?: GuildID,
	): Promise<IARSubmission> {
		await this.checkReportBan(reporter.id);
		const reporterKey = this.getReporterRateLimitKey(reporter);
		await this.checkRateLimit(reporterKey);

		if (reporter.id && reportedUserId === reporter.id) {
			throw new CannotReportYourselfError();
		}

		const reportedUser = await this.userRepository.findUnique(reportedUserId);
		if (!reportedUser) {
			throw new UnknownUserError();
		}

		const reportId = createReportID(await this.snowflakeService.generate());
		const reportData: IARSubmissionRow = {
			report_id: reportId,
			reporter_id: reporter.id,
			reporter_email: reporter.email,
			reporter_full_legal_name: reporter.fullLegalName,
			reporter_country_of_residence: reporter.countryOfResidence,
			reported_at: new Date(),
			status: ReportStatus.PENDING,
			report_type: ReportType.USER,
			category,
			additional_info: additionalInfo || null,
			reported_user_id: reportedUserId,
			reported_user_avatar_hash: reportedUser.avatarHash || null,
			reported_guild_id: guildId || null,
			reported_guild_name: null,
			reported_guild_icon_hash: null,
			reported_message_id: null,
			reported_channel_id: null,
			reported_channel_name: null,
			message_context: null,
			guild_context_id: guildId || null,
			resolved_at: null,
			resolved_by_admin_id: null,
			public_comment: null,
			audit_log_reason: null,
			reported_guild_invite_code: null,
		};

		if (guildId) {
			const guild = await this.guildRepository.findUnique(guildId);
			if (guild) {
				reportData.reported_guild_name = guild.name;
				reportData.reported_guild_icon_hash = guild.iconHash || null;
			}
		}

		this.trackRateLimit(reporterKey);

		const report = await this.reportRepository.createReport(reportData);

		emitReportMetric('reports.iar.created', report);

		if (this.reportSearchService && 'indexReport' in this.reportSearchService) {
			await this.reportSearchService.indexReport(report).catch((error) => {
				Logger.error({error, reportId: report.reportId}, 'Failed to index user report in search');
			});
		}

		return report;
	}

	async reportGuild(
		reporter: ReporterMetadata,
		guildId: GuildID,
		category: string,
		additionalInfo?: string,
	): Promise<IARSubmission> {
		await this.checkReportBan(reporter.id);
		const reporterKey = this.getReporterRateLimitKey(reporter);
		await this.checkRateLimit(reporterKey);

		const guild = await this.guildRepository.findUnique(guildId);
		if (!guild) {
			throw new UnknownGuildError();
		}

		if (reporter.id && guild.ownerId === reporter.id) {
			throw new CannotReportOwnGuildError();
		}

		const reportId = createReportID(await this.snowflakeService.generate());
		const reportData: IARSubmissionRow = {
			report_id: reportId,
			reporter_id: reporter.id,
			reporter_email: reporter.email,
			reporter_full_legal_name: reporter.fullLegalName,
			reporter_country_of_residence: reporter.countryOfResidence,
			reported_at: new Date(),
			status: ReportStatus.PENDING,
			report_type: ReportType.GUILD,
			category,
			additional_info: additionalInfo || null,
			reported_user_id: null,
			reported_user_avatar_hash: null,
			reported_guild_id: guildId,
			reported_guild_name: guild.name,
			reported_guild_icon_hash: guild.iconHash || null,
			reported_message_id: null,
			reported_channel_id: null,
			reported_channel_name: null,
			message_context: null,
			guild_context_id: guildId,
			resolved_at: null,
			resolved_by_admin_id: null,
			public_comment: null,
			audit_log_reason: null,
			reported_guild_invite_code: null,
		};

		this.trackRateLimit(reporterKey);

		const report = await this.reportRepository.createReport(reportData);

		emitReportMetric('reports.iar.created', report);

		if (this.reportSearchService && 'indexReport' in this.reportSearchService) {
			await this.reportSearchService.indexReport(report).catch((error) => {
				Logger.error({error, reportId: report.reportId}, 'Failed to index guild report in search');
			});
		}

		return report;
	}

	async sendDsaReportVerificationCode(email: string): Promise<void> {
		const normalizedEmail = this.normalizeEmail(email);
		const hasValidDns = await this.emailDnsValidationService.hasValidDnsRecords(normalizedEmail);
		if (!hasValidDns) {
			throw InputValidationError.fromCode('email', ValidationErrorCodes.INVALID_EMAIL_ADDRESS);
		}

		const verificationCode = this.generateDsaVerificationCode();
		const expiresAt = new Date(Date.now() + ms('10 minutes'));

		await this.reportRepository.upsertDsaEmailVerification({
			email_lower: normalizedEmail,
			code_hash: this.hashVerificationCode(verificationCode),
			expires_at: expiresAt,
			last_sent_at: new Date(),
		});

		await this.emailService.sendDsaReportVerificationCode(normalizedEmail, verificationCode, expiresAt);
	}

	async verifyDsaReportEmail(email: string, code: string): Promise<string> {
		const normalizedEmail = this.normalizeEmail(email);
		const verificationRow = await this.reportRepository.getDsaEmailVerification(normalizedEmail);
		if (!verificationRow || verificationRow.expires_at.getTime() < Date.now()) {
			throw new InvalidDsaVerificationCodeError();
		}

		if (this.hashVerificationCode(code) !== verificationRow.code_hash) {
			throw new InvalidDsaVerificationCodeError();
		}

		await this.reportRepository.deleteDsaEmailVerification(normalizedEmail);

		const ticket = this.generateDsaTicket();
		await this.reportRepository.createDsaTicket({
			ticket,
			email_lower: normalizedEmail,
			expires_at: new Date(Date.now() + ms('1 hour')),
			created_at: new Date(),
		});

		return ticket;
	}

	async createDsaReport(report: DsaReportRequest): Promise<IARSubmission> {
		const ticket = await this.consumeDsaTicket(report.ticket);
		const reporterMeta: ReporterMetadata = {
			id: null,
			email: ticket.email_lower,
			fullLegalName: report.reporter_full_legal_name,
			countryOfResidence: report.reporter_country_of_residence,
		};

		await this.checkReportBan(null);
		const reporterKey = this.getReporterRateLimitKey(reporterMeta);
		await this.checkRateLimit(reporterKey);

		const reportId = createReportID(await this.snowflakeService.generate());
		const reportRow = await this.buildDsaReportRow(reportId, report, reporterMeta);

		this.trackRateLimit(reporterKey);

		const createdReport = await this.reportRepository.createReport(reportRow);

		emitReportMetric('reports.iar.created', createdReport);

		if (this.reportSearchService && 'indexReport' in this.reportSearchService) {
			await this.reportSearchService.indexReport(createdReport).catch((error) => {
				Logger.error({error, reportId: createdReport.reportId}, 'Failed to index DSA report in search');
			});
		}

		return createdReport;
	}

	private async buildDsaReportRow(
		reportId: ReportID,
		report: DsaReportRequest,
		reporter: ReporterMetadata,
	): Promise<IARSubmissionRow> {
		switch (report.report_type) {
			case 'message':
				return this.buildDsaMessageReportRow(reportId, report, reporter);
			case 'user':
				return this.buildDsaUserReportRow(reportId, report, reporter);
			case 'guild':
				return this.buildDsaGuildReportRow(reportId, report, reporter);
			default:
				throw new InvalidDsaReportTargetError();
		}
	}

	private async buildDsaMessageReportRow(
		reportId: ReportID,
		report: Extract<DsaReportRequest, {report_type: 'message'}>,
		reporter: ReporterMetadata,
	): Promise<IARSubmissionRow> {
		const {channelId, messageId} = this.extractChannelAndMessageFromLink(report.message_link);
		const channel = await this.channelRepository.findUnique(channelId);
		if (!channel) throw new UnknownChannelError();

		const message = await this.channelRepository.getMessage(channelId, messageId);
		if (!message) throw new UnknownMessageError();

		if (message.authorId == null) {
			throw new UnknownUserError();
		}

		if (report.reported_user_tag) {
			const tagged = await this.findUserByTag(report.reported_user_tag);
			if (tagged.id !== message.authorId) {
				throw new InvalidDsaReportTargetError();
			}
		}

		const reportedUser = await this.userRepository.findUnique(message.authorId);
		if (!reportedUser) {
			throw new UnknownUserError();
		}

		const messageContext = await this.gatherMessageContext(channelId, messageId);
		const guild = channel.guildId ? await this.guildRepository.findUnique(channel.guildId) : null;

		return {
			report_id: reportId,
			reporter_id: null,
			reporter_email: reporter.email,
			reporter_full_legal_name: reporter.fullLegalName,
			reporter_country_of_residence: reporter.countryOfResidence,
			reported_at: new Date(),
			status: ReportStatus.PENDING,
			report_type: ReportType.MESSAGE,
			category: report.category,
			additional_info: report.additional_info ?? null,
			reported_user_id: message.authorId,
			reported_user_avatar_hash: reportedUser.avatarHash || null,
			reported_guild_id: channel.guildId || null,
			reported_guild_name: guild?.name ?? null,
			reported_guild_icon_hash: guild?.iconHash ?? null,
			reported_message_id: messageId,
			reported_channel_id: channelId,
			reported_channel_name: channel.name || null,
			message_context: messageContext,
			guild_context_id: channel.guildId || null,
			resolved_at: null,
			resolved_by_admin_id: null,
			public_comment: null,
			audit_log_reason: null,
			reported_guild_invite_code: null,
		};
	}

	private async buildDsaUserReportRow(
		reportId: ReportID,
		report: Extract<DsaReportRequest, {report_type: 'user'}>,
		reporter: ReporterMetadata,
	): Promise<IARSubmissionRow> {
		const target = await this.resolveDsaUser(report.user_id ?? undefined, report.user_tag ?? undefined);

		return {
			report_id: reportId,
			reporter_id: null,
			reporter_email: reporter.email,
			reporter_full_legal_name: reporter.fullLegalName,
			reporter_country_of_residence: reporter.countryOfResidence,
			reported_at: new Date(),
			status: ReportStatus.PENDING,
			report_type: ReportType.USER,
			category: report.category,
			additional_info: report.additional_info ?? null,
			reported_user_id: target.id,
			reported_user_avatar_hash: target.avatarHash || null,
			reported_guild_id: null,
			reported_guild_name: null,
			reported_guild_icon_hash: null,
			reported_message_id: null,
			reported_channel_id: null,
			reported_channel_name: null,
			message_context: null,
			guild_context_id: null,
			resolved_at: null,
			resolved_by_admin_id: null,
			public_comment: null,
			audit_log_reason: null,
			reported_guild_invite_code: null,
		};
	}

	private async buildDsaGuildReportRow(
		reportId: ReportID,
		report: Extract<DsaReportRequest, {report_type: 'guild'}>,
		reporter: ReporterMetadata,
	): Promise<IARSubmissionRow> {
		const guildId = createGuildID(report.guild_id);
		const guild = await this.guildRepository.findUnique(guildId);
		if (!guild) {
			throw new UnknownGuildError();
		}

		let inviteCode: string | null = null;
		if (report.invite_code) {
			inviteCode = this.sanitizeInviteCode(report.invite_code);
			if (!inviteCode) {
				throw new InvalidDsaReportTargetError();
			}
			await this.validateInviteForGuild(inviteCode, guildId);
		}

		return {
			report_id: reportId,
			reporter_id: null,
			reporter_email: reporter.email,
			reporter_full_legal_name: reporter.fullLegalName,
			reporter_country_of_residence: reporter.countryOfResidence,
			reported_at: new Date(),
			status: ReportStatus.PENDING,
			report_type: ReportType.GUILD,
			category: report.category,
			additional_info: report.additional_info ?? null,
			reported_user_id: null,
			reported_user_avatar_hash: null,
			reported_guild_id: guildId,
			reported_guild_name: guild.name,
			reported_guild_icon_hash: guild.iconHash || null,
			reported_message_id: null,
			reported_channel_id: null,
			reported_channel_name: null,
			message_context: null,
			guild_context_id: guildId,
			resolved_at: null,
			resolved_by_admin_id: null,
			public_comment: null,
			audit_log_reason: null,
			reported_guild_invite_code: inviteCode,
		};
	}

	private async resolveDsaUser(userId?: bigint, userTag?: string | null): Promise<User> {
		if (userId != null) {
			const user = await this.userRepository.findUnique(createUserID(userId));
			if (!user) {
				throw new UnknownUserError();
			}
			if (userTag) {
				const taggedUser = await this.findUserByTag(userTag);
				if (taggedUser.id !== user.id) {
					throw new InvalidDsaReportTargetError();
				}
			}
			return user;
		}
		if (userTag) {
			return this.findUserByTag(userTag);
		}
		throw new InvalidDsaReportTargetError();
	}

	private async findUserByTag(tag: string): Promise<User> {
		const parsed = this.parseFluxerTag(tag);
		if (!parsed) {
			throw new InvalidDsaReportTargetError();
		}
		const user = await this.userRepository.findByUsernameDiscriminator(parsed.username, parsed.discriminator);
		if (!user) {
			throw new UnknownUserError();
		}
		return user;
	}

	private async consumeDsaTicket(ticket: string): Promise<DSAReportTicketRow> {
		const ticketRow = await this.reportRepository.getDsaTicket(ticket);
		if (!ticketRow || ticketRow.expires_at.getTime() < Date.now()) {
			throw new InvalidDsaTicketError();
		}
		await this.reportRepository.deleteDsaTicket(ticket);
		return ticketRow;
	}

	private generateDsaVerificationCode(): string {
		const segments: Array<string> = [];
		for (let i = 0; i < 2; i += 1) {
			let segment = '';
			for (let j = 0; j < DSA_CODE_SEGMENT_LENGTH; j += 1) {
				const index = randomBytes(1)[0] % DSA_CODE_CHARSET.length;
				segment += DSA_CODE_CHARSET[index];
			}
			segments.push(segment);
		}
		return segments.join(DSA_CODE_SEPARATOR);
	}

	private hashVerificationCode(code: string): string {
		return createHash('sha256').update(code).digest('hex');
	}

	private generateDsaTicket(): string {
		return randomBytes(DSA_TICKET_BYTES).toString('hex');
	}

	private normalizeEmail(email: string): string {
		return email.trim().toLowerCase();
	}

	private parseFluxerTag(tag: string): {username: string; discriminator: number} | null {
		const trimmed = tag.trim();
		const match = /^(.+)#(\d{4})$/.exec(trimmed);
		if (!match) return null;
		return {
			username: match[1],
			discriminator: Number.parseInt(match[2], 10),
		};
	}

	private extractChannelAndMessageFromLink(link: string): {channelId: ChannelID; messageId: MessageID} {
		let parsed: URL;
		try {
			parsed = new URL(link);
		} catch {
			throw new UnknownMessageError();
		}
		const segments = parsed.pathname.split('/').filter((segment) => segment.length > 0);
		if (segments.length < 4 || segments[0] !== 'channels') {
			throw new UnknownMessageError();
		}
		const channelIdSegment = segments[2];
		const messageIdSegment = segments[3];
		return {
			channelId: createChannelID(BigInt(channelIdSegment)),
			messageId: createMessageID(BigInt(messageIdSegment)),
		};
	}

	private sanitizeInviteCode(raw: string): string {
		const trimmed = raw.trim();
		const segments = trimmed.split('/').filter((segment) => segment.length > 0);
		const candidate = segments.length > 0 ? segments[segments.length - 1] : trimmed;
		return candidate;
	}

	private async validateInviteForGuild(code: string, guildId: GuildID): Promise<void> {
		const invite = await this.inviteRepository.findUnique(createInviteCode(code));
		if (!invite) {
			throw new UnknownInviteError();
		}
		if (invite.type !== InviteTypes.GUILD || !invite.guildId || invite.guildId !== guildId) {
			throw new InvalidDsaReportTargetError();
		}
	}

	async getReport(reportId: ReportID): Promise<IARSubmission> {
		const report = await this.reportRepository.getReport(reportId);
		if (!report) {
			throw new UnknownReportError();
		}
		return report;
	}

	async listMyReports(reporterId: UserID, limit?: number, offset?: number): Promise<Array<IARSubmission>> {
		if (!this.reportSearchService) {
			throw new Error('Search service not available');
		}

		const {hits} = await this.reportSearchService.listReportsByReporter(reporterId, limit, offset);

		const reportIds = hits.map((hit) => createReportID(BigInt(hit.id)));
		const reports = await Promise.all(reportIds.map((id) => this.reportRepository.getReport(id)));

		return reports.filter((report): report is IARSubmission => report !== null);
	}

	async listReportsByStatus(status: number, limit?: number, offset?: number): Promise<Array<IARSubmission>> {
		if (!this.reportSearchService) {
			throw new Error('Search service not available');
		}

		const {hits} = await this.reportSearchService.listReportsByStatus(status, limit, offset);

		const reportIds = hits.map((hit) => createReportID(BigInt(hit.id)));
		const reports = await Promise.all(reportIds.map((id) => this.reportRepository.getReport(id)));

		return reports.filter((report): report is IARSubmission => report !== null);
	}

	async listAllReportsPaginated(limit: number, lastReportId?: ReportID): Promise<Array<IARSubmission>> {
		return this.reportRepository.listAllReportsPaginated(limit, lastReportId);
	}

	async resolveReport(
		reportId: ReportID,
		adminUserId: UserID,
		publicComment: string | null,
		auditLogReason: string | null,
	): Promise<IARSubmission> {
		const report = await this.reportRepository.resolveReport(reportId, adminUserId, publicComment, auditLogReason);

		emitReportMetric('reports.iar.resolved', report, {status: 'resolved'});

		if (this.reportSearchService && 'updateReport' in this.reportSearchService) {
			await this.reportSearchService.updateReport(report).catch((error) => {
				Logger.error({error, reportId: report.reportId}, 'Failed to update report in search index');
			});
		}

		return report;
	}

	private async gatherMessageContext(
		channelId: ChannelID,
		targetMessageId: MessageID,
	): Promise<Array<IARMessageContextRow>> {
		const messagesBefore = await this.channelRepository.listMessages(
			channelId,
			targetMessageId,
			MESSAGE_CONTEXT_WINDOW,
		);

		const messagesAfter = await this.channelRepository.listMessages(
			channelId,
			undefined,
			MESSAGE_CONTEXT_WINDOW,
			targetMessageId,
		);

		const targetMessage = await this.channelRepository.getMessage(channelId, targetMessageId);
		if (!targetMessage) {
			return [];
		}

		messagesBefore.reverse();
		const allMessages = [...messagesBefore, targetMessage, ...messagesAfter];

		const userIds = new Set<UserID>();
		for (const msg of allMessages) {
			if (msg.authorId) {
				userIds.add(msg.authorId);
			}
		}

		const users = new Map<UserID, User>();
		for (const userId of userIds) {
			const user = await this.userRepository.findUnique(userId);
			if (user) {
				users.set(userId, user);
			}
		}

		const context: Array<IARMessageContextRow> = [];
		for (const message of allMessages) {
			const author = message.authorId != null ? users.get(message.authorId) : null;
			if (!author) continue;

			const clonedAttachments = message.attachments
				? await this.cloneAttachmentsForReport(message.attachments, channelId)
				: [];

			context.push({
				message_id: message.id,
				channel_id: channelId,
				author_id: message.authorId!,
				author_username: author.username,
				author_discriminator: author.discriminator,
				author_avatar_hash: author.avatarHash || null,
				content: message.content || null,
				timestamp: snowflakeToDate(message.id),
				edited_timestamp: message.editedTimestamp || null,
				type: message.type,
				flags: message.flags,
				mention_everyone: message.mentionEveryone,
				mention_users: message.mentionedUserIds.size > 0 ? Array.from(message.mentionedUserIds) : null,
				mention_roles: message.mentionedRoleIds.size > 0 ? Array.from(message.mentionedRoleIds) : null,
				mention_channels: message.mentionedChannelIds.size > 0 ? Array.from(message.mentionedChannelIds) : null,
				attachments: clonedAttachments.length > 0 ? clonedAttachments : null,
				embeds: message.embeds.length > 0 ? message.embeds.map((embed) => embed.toMessageEmbed()) : null,
				sticker_items:
					message.stickers.length > 0 ? message.stickers.map((sticker) => sticker.toMessageStickerItem()) : null,
			});
		}

		return context;
	}

	private async cloneAttachmentsForReport(
		attachments: Array<Attachment>,
		sourceChannelId: ChannelID,
	): Promise<Array<MessageAttachment>> {
		const clonedAttachments: Array<MessageAttachment> = [];

		for (const attachment of attachments) {
			const sourceKey = MessageHelpers.makeAttachmentCdnKey(sourceChannelId, attachment.id, attachment.filename);

			try {
				await this.storageService.copyObject({
					sourceBucket: Config.s3.buckets.cdn,
					sourceKey,
					destinationBucket: Config.s3.buckets.reports,
					destinationKey: sourceKey,
					newContentType: attachment.contentType,
				});

				const clonedAttachment: MessageAttachment = {
					attachment_id: attachment.id,
					filename: attachment.filename,
					size: BigInt(attachment.size),
					title: attachment.title,
					description: attachment.description,
					width: attachment.width,
					height: attachment.height,
					content_type: attachment.contentType,
					content_hash: attachment.contentHash,
					placeholder: attachment.placeholder,
					flags: attachment.flags ?? 0,
					duration: attachment.duration,
					nsfw: attachment.nsfw,
					waveform: attachment.waveform ?? null,
				};
				clonedAttachments.push(clonedAttachment);
			} catch (error) {
				Logger.error(
					{error, attachmentId: attachment.id, filename: attachment.filename, sourceChannelId},
					'Failed to clone attachment for report',
				);
			}
		}

		return clonedAttachments;
	}

	private async checkReportBan(userId: UserID | null): Promise<void> {
		if (!userId) {
			return;
		}
		const user = await this.userRepository.findUnique(userId);
		if (user && (user.flags & UserFlags.REPORT_BANNED) !== 0n) {
			throw new ReportBannedError();
		}
	}

	private getReporterRateLimitKey(reporter: ReporterMetadata): string {
		if (reporter.id) {
			return `user:${reporter.id.toString()}`;
		}
		if (reporter.email) {
			return `email:${reporter.email.toLowerCase()}`;
		}
		return 'anonymous';
	}

	private async checkRateLimit(key: string): Promise<void> {
		const now = Date.now();
		const userReports = this.reportRateLimitMap.get(key) || [];
		const recentReports = userReports.filter((timestamp) => now - timestamp < REPORT_RATE_LIMIT_WINDOW);

		if (recentReports.length >= REPORT_RATE_LIMIT_MAX) {
			const oldestReport = Math.min(...recentReports);
			const retryAfter = Math.ceil((oldestReport + REPORT_RATE_LIMIT_WINDOW - now) / 1000);
			const resetTime = new Date(oldestReport + REPORT_RATE_LIMIT_WINDOW);
			throw new RateLimitError({
				message: `Too many reports. Try again in ${retryAfter} seconds.`,
				retryAfter,
				limit: REPORT_RATE_LIMIT_MAX,
				resetTime,
			});
		}
	}

	private trackRateLimit(key: string): void {
		const now = Date.now();
		const userReports = this.reportRateLimitMap.get(key) || [];
		const recentReports = userReports.filter((timestamp) => now - timestamp < REPORT_RATE_LIMIT_WINDOW);
		recentReports.push(now);
		this.reportRateLimitMap.set(key, recentReports);
	}

	private cleanupRateLimitMap(): void {
		const now = Date.now();
		for (const [key, timestamps] of this.reportRateLimitMap.entries()) {
			const recentReports = timestamps.filter((timestamp) => now - timestamp < REPORT_RATE_LIMIT_WINDOW);
			if (recentReports.length === 0) {
				this.reportRateLimitMap.delete(key);
			} else {
				this.reportRateLimitMap.set(key, recentReports);
			}
		}
	}

	public shutdown(): void {
		if (this.cleanupInterval) {
			clearInterval(this.cleanupInterval);
		}
	}
}

function emitReportMetric(metricName: string, report: IARSubmission, extra: Record<string, string> = {}): void {
	const dimensions: Record<string, string> = {
		report_type: reportTypeLabel(report.reportType),
		category: report.category || 'unknown',
		...extra,
	};

	if (report.reportedGuildId) {
		dimensions['reported_guild_id'] = report.reportedGuildId.toString();
	}
	if (report.reportedUserId) {
		dimensions['reported_user_id'] = report.reportedUserId.toString();
	}

	recordCounter({
		name: metricName,
		dimensions,
	});
}

function reportTypeLabel(type: number): string {
	switch (type) {
		case ReportType.MESSAGE:
			return 'message';
		case ReportType.USER:
			return 'user';
		case ReportType.GUILD:
			return 'guild';
		default:
			return 'unknown';
	}
}
