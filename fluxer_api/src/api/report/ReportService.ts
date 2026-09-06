// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash, randomBytes, randomInt} from 'node:crypto';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {InviteTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {CannotReportOwnMessageError} from '@fluxer/errors/src/domains/channel/CannotReportOwnMessageError';
import {UnknownChannelError} from '@fluxer/errors/src/domains/channel/UnknownChannelError';
import {UnknownMessageError} from '@fluxer/errors/src/domains/channel/UnknownMessageError';
import {ConflictError} from '@fluxer/errors/src/domains/core/ConflictError';
import {FeatureTemporarilyDisabledError} from '@fluxer/errors/src/domains/core/FeatureTemporarilyDisabledError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {RateLimitError} from '@fluxer/errors/src/domains/core/RateLimitError';
import {CannotReportGuildError} from '@fluxer/errors/src/domains/guild/CannotReportGuildError';
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
import {SnowflakeType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {snowflakeToDate} from '@fluxer/snowflake/src/Snowflake';
import type {IEmailService} from '@pkgs/email/src/IEmailService';
import type {IRateLimitService} from '@pkgs/rate_limit/src/IRateLimitService';
import {ms} from 'itty-time';
import type {ChannelID, GuildID, InviteCode, MessageID, ReportID, UserID} from '../BrandedTypes';
import {
	createChannelID,
	createGuildID,
	createInviteCode,
	createMessageID,
	createReportID,
	createUserID,
} from '../BrandedTypes';
import {Config} from '../Config';
import type {IChannelRepository} from '../channel/IChannelRepository';
import type {AuthenticatedChannel} from '../channel/services/AuthenticatedChannel';
import {MessageChannelAuthService} from '../channel/services/message/MessageChannelAuthService';
import * as MessageHelpers from '../channel/services/message/MessageHelpers';
import type {ContentWarningChannelLike} from '../channel/utils/EffectiveContentWarning';
import {
	channelToContentWarningView,
	computeEffectiveChannelNsfw,
	computeEffectiveContentWarning,
	guildToContentWarningView,
} from '../channel/utils/EffectiveContentWarning';
import type {MessageAttachment} from '../database/types/MessageTypes';
import type {DSAReportTicketRow} from '../database/types/ReportTypes';
import type {IGuildRepositoryAggregate} from '../guild/repositories/IGuildRepositoryAggregate';
import type {IEmailDnsValidationService} from '../infrastructure/IEmailDnsValidationService';
import type {IGatewayService} from '../infrastructure/IGatewayService';
import type {ISnowflakeService} from '../infrastructure/ISnowflakeService';
import type {IStorageService} from '../infrastructure/IStorageService';
import type {IInviteRepository} from '../invite/IInviteRepository';
import {Logger} from '../Logger';
import type {Attachment} from '../models/Attachment';
import type {Channel} from '../models/Channel';
import type {Guild} from '../models/Guild';
import type {Message} from '../models/Message';
import type {User} from '../models/User';
import type {IReportSearchService} from '../search/IReportSearchService';
import type {IUserRepository} from '../user/IUserRepository';
import type {IARMessageContextRow, IARSubmission, IARSubmissionRow, IReportRepository} from './IReportRepository';
import {ReportStatus, ReportType} from './IReportRepository';

interface ReporterMetadata {
	id: UserID | null;
	email: string | null;
	fullLegalName: string | null;
	countryOfResidence: string | null;
}

const REPORT_RATE_LIMIT_WINDOW = ms('1 hour');
const REPORT_RATE_LIMIT_MAX = 5;
const MESSAGE_REPORT_USER_GUILD_RATE_LIMIT_MAX = 4;
const MESSAGE_REPORT_USER_CHANNEL_RATE_LIMIT_MAX = 3;
const MESSAGE_REPORT_TARGET_MESSAGE_RATE_LIMIT_MAX = 20;
const MESSAGE_CONTEXT_WINDOW = 25;
const DSA_CODE_CHARSET = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
const DSA_CODE_SEGMENT_LENGTH = 4;
const DSA_CODE_SEPARATOR = '-';
const DSA_TICKET_BYTES = 32;

export class ReportService {
	private readonly messageChannelAuthService: MessageChannelAuthService;

	constructor(
		private reportRepository: IReportRepository,
		private channelRepository: IChannelRepository,
		private guildRepository: IGuildRepositoryAggregate,
		private userRepository: IUserRepository,
		private inviteRepository: IInviteRepository,
		private emailService: IEmailService,
		private emailDnsValidationService: IEmailDnsValidationService,
		private snowflakeService: ISnowflakeService,
		private storageService: IStorageService,
		private gatewayService: IGatewayService,
		private rateLimitService: IRateLimitService,
		private reportSearchService: IReportSearchService | null = null,
	) {
		this.messageChannelAuthService = new MessageChannelAuthService(
			this.channelRepository,
			this.userRepository,
			this.guildRepository,
			this.gatewayService,
		);
	}

	async reportMessage(
		reporter: ReporterMetadata,
		channelId: ChannelID,
		messageId: MessageID,
		category: string,
	): Promise<IARSubmission> {
		await this.checkReportBan(reporter.id);
		const reporterKey = this.getReporterRateLimitKey(reporter);
		await this.ensureReportRateLimit(this.createReportRateLimitIdentifier(reporterKey), REPORT_RATE_LIMIT_MAX, false);
		const {authChannel, channel, message} = await this.getReportableMessageForReporter({
			reporterId: reporter.id,
			channelId,
			messageId,
		});
		const reportedUserId = message.authorId;
		if (reportedUserId == null) {
			throw new UnknownMessageError();
		}
		if (reporter.id && reportedUserId === reporter.id) {
			throw new CannotReportOwnMessageError();
		}
		const [reportedUser, messageContext] = await Promise.all([
			this.userRepository.findUnique(reportedUserId),
			this.gatherMessageContext(channelId, messageId, authChannel),
		]);
		if (!reportedUser) {
			throw new UnknownUserError();
		}
		const reportId = createReportID(await this.snowflakeService.generate());
		const guild = channel.guildId ? await this.guildRepository.findUnique(channel.guildId) : null;
		const contentWarningSnapshot = await this.buildContentWarningSnapshot(guild, channel);
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
			additional_info: null,
			reported_user_id: reportedUserId,
			reported_user_avatar_hash: reportedUser.avatarHash || null,
			reported_guild_id: channel.guildId || null,
			reported_guild_name: guild?.name ?? null,
			reported_guild_icon_hash: guild?.iconHash || null,
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
			...contentWarningSnapshot,
		};
		let duplicateReservationCreated = false;
		if (reporter.id) {
			duplicateReservationCreated = await this.reportRepository.reserveMessageReportByReporter({
				reporter_id: reporter.id,
				channel_id: channelId,
				message_id: messageId,
				report_id: reportId,
				reported_at: reportData.reported_at,
			});
			if (!duplicateReservationCreated) {
				throw new ConflictError({code: APIErrorCodes.CONFLICT});
			}
		}
		try {
			await this.consumeMessageReportRateLimits({reporter, channel, message});
			const report = await this.reportRepository.createReport(reportData);
			if (this.reportSearchService && 'indexReport' in this.reportSearchService) {
				await this.reportSearchService.indexReport(report).catch((error) => {
					Logger.error({error, reportId: report.reportId}, 'Failed to index message report in search');
				});
			}
			return report;
		} catch (error) {
			if (duplicateReservationCreated && reporter.id) {
				await this.reportRepository
					.deleteMessageReportByReporter(reporter.id, channelId, messageId)
					.catch((cleanupError) => {
						Logger.error({error: cleanupError, reportId}, 'Failed to clean up message report duplicate reservation');
					});
			}
			throw error;
		}
	}

	async reportUser(
		reporter: ReporterMetadata,
		reportedUserId: UserID,
		category: string,
		guildId?: GuildID,
	): Promise<IARSubmission> {
		await this.checkReportBan(reporter.id);
		const reporterKey = this.getReporterRateLimitKey(reporter);
		if (reporter.id && reportedUserId === reporter.id) {
			throw new CannotReportYourselfError();
		}
		const reportedUser = await this.userRepository.findUnique(reportedUserId);
		if (!reportedUser) {
			throw new UnknownUserError();
		}
		const reportId = createReportID(await this.snowflakeService.generate());
		const guild = guildId ? await this.guildRepository.findUnique(guildId) : null;
		if (guildId && !guild) {
			throw new UnknownGuildError();
		}
		const contentWarningSnapshot = await this.buildContentWarningSnapshot(guild, null);
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
			additional_info: null,
			reported_user_id: reportedUserId,
			reported_user_avatar_hash: reportedUser.avatarHash || null,
			reported_guild_id: guildId || null,
			reported_guild_name: guild?.name ?? null,
			reported_guild_icon_hash: guild?.iconHash || null,
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
			...contentWarningSnapshot,
		};
		await this.ensureReportRateLimit(this.createReportRateLimitIdentifier(reporterKey), REPORT_RATE_LIMIT_MAX, true);
		const report = await this.reportRepository.createReport(reportData);
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
		inviteCode?: InviteCode,
	): Promise<IARSubmission> {
		await this.checkReportBan(reporter.id);
		const reporterKey = this.getReporterRateLimitKey(reporter);
		const guild = await this.guildRepository.findUnique(guildId);
		if (!guild) {
			throw new UnknownGuildError();
		}
		if (reporter.id && guild.ownerId === reporter.id) {
			throw new CannotReportOwnGuildError();
		}
		await this.authorizeGuildReporter(reporter.id, guild, inviteCode);
		const reportedInviteCode = inviteCode ?? null;
		const reportId = createReportID(await this.snowflakeService.generate());
		const contentWarningSnapshot = await this.buildContentWarningSnapshot(guild, null);
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
			additional_info: null,
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
			reported_guild_invite_code: reportedInviteCode,
			...contentWarningSnapshot,
		};
		await this.ensureReportRateLimit(this.createReportRateLimitIdentifier(reporterKey), REPORT_RATE_LIMIT_MAX, true);
		const report = await this.reportRepository.createReport(reportData);
		if (this.reportSearchService && 'indexReport' in this.reportSearchService) {
			await this.reportSearchService.indexReport(report).catch((error) => {
				Logger.error({error, reportId: report.reportId}, 'Failed to index guild report in search');
			});
		}
		return report;
	}

	private async authorizeGuildReporter(
		reporterId: UserID | null,
		guild: Guild,
		inviteCode: InviteCode | undefined,
	): Promise<void> {
		if (reporterId) {
			const member = await this.guildRepository.getMember(guild.id, reporterId);
			if (member) return;
		}
		if (guild.features.has(GuildFeatures.DISCOVERABLE)) return;
		if (inviteCode) {
			const invite = await this.inviteRepository.findUnique(inviteCode);
			if (invite && invite.guildId === guild.id) return;
		}
		throw new CannotReportGuildError();
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
		const ticket = randomBytes(DSA_TICKET_BYTES).toString('hex');
		await this.reportRepository.createDsaTicket({
			ticket,
			email_lower: normalizedEmail,
			expires_at: new Date(Date.now() + ms('1 hour')),
			created_at: new Date(),
		});
		return ticket;
	}

	async createDsaReport(report: DsaReportRequest): Promise<IARSubmission> {
		const ticket = await this.readDsaTicket(report.ticket);
		const reporterMeta: ReporterMetadata = {
			id: null,
			email: ticket.email_lower,
			fullLegalName: report.reporter_full_legal_name,
			countryOfResidence: report.reporter_country_of_residence,
		};
		await this.checkReportBan(null);
		const reporterKey = this.getReporterRateLimitKey(reporterMeta);
		await this.ensureReportRateLimit(this.createReportRateLimitIdentifier(reporterKey), REPORT_RATE_LIMIT_MAX, false);
		const reportId = createReportID(await this.snowflakeService.generate());
		const reportRow = await this.buildDsaReportRow(reportId, report, reporterMeta);
		await this.ensureReportRateLimit(this.createReportRateLimitIdentifier(reporterKey), REPORT_RATE_LIMIT_MAX, true);
		await this.reportRepository.deleteDsaTicket(report.ticket);
		const createdReport = await this.reportRepository.createReport(reportRow);
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
		report: Extract<
			DsaReportRequest,
			{
				report_type: 'message';
			}
		>,
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
		const contentWarningSnapshot = await this.buildContentWarningSnapshot(guild, channel);
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
			...contentWarningSnapshot,
		};
	}

	private async buildDsaUserReportRow(
		reportId: ReportID,
		report: Extract<
			DsaReportRequest,
			{
				report_type: 'user';
			}
		>,
		reporter: ReporterMetadata,
	): Promise<IARSubmissionRow> {
		const target = await this.resolveDsaUser(report.user_id ?? undefined, report.user_tag ?? undefined);
		const contentWarningSnapshot = await this.buildContentWarningSnapshot(null, null);
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
			...contentWarningSnapshot,
		};
	}

	private async buildDsaGuildReportRow(
		reportId: ReportID,
		report: Extract<
			DsaReportRequest,
			{
				report_type: 'guild';
			}
		>,
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
		const contentWarningSnapshot = await this.buildContentWarningSnapshot(guild, null);
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
			...contentWarningSnapshot,
		};
	}

	private async getReportableMessageForReporter({
		reporterId,
		channelId,
		messageId,
	}: {
		reporterId: UserID | null;
		channelId: ChannelID;
		messageId: MessageID;
	}): Promise<{
		authChannel: AuthenticatedChannel;
		channel: Channel;
		message: Message;
	}> {
		if (!reporterId) {
			throw new UnknownChannelError();
		}
		const authChannel = await this.messageChannelAuthService.getChannelAuthenticated({userId: reporterId, channelId});
		if (!(await this.canAccessMessage(authChannel, messageId))) {
			throw new UnknownMessageError();
		}
		const message = await this.channelRepository.getMessage(channelId, messageId);
		if (!message || message.channelId !== channelId) {
			throw new UnknownMessageError();
		}
		return {
			authChannel,
			channel: authChannel.channel,
			message,
		};
	}

	private async canAccessMessage(authChannel: AuthenticatedChannel, messageId: MessageID): Promise<boolean> {
		if (!authChannel.guild) {
			return true;
		}
		if (await authChannel.hasPermission(Permissions.READ_MESSAGE_HISTORY)) {
			return true;
		}
		const cutoff = authChannel.guild.message_history_cutoff;
		if (!cutoff) {
			return false;
		}
		return snowflakeToDate(messageId).getTime() >= new Date(cutoff).getTime();
	}

	private async buildContentWarningSnapshot(
		guild: Guild | null,
		channel: Channel | null,
	): Promise<
		Pick<
			IARSubmissionRow,
			| 'reported_guild_nsfw'
			| 'reported_guild_content_warning_level'
			| 'reported_guild_content_warning_text'
			| 'reported_channel_nsfw_override'
			| 'reported_channel_content_warning_level'
			| 'reported_channel_content_warning_text'
			| 'reported_channel_effective_nsfw'
			| 'reported_channel_effective_content_warning_level'
			| 'reported_channel_effective_content_warning_text'
		>
	> {
		if (!guild) {
			return {
				reported_guild_nsfw: null,
				reported_guild_content_warning_level: null,
				reported_guild_content_warning_text: null,
				reported_channel_nsfw_override: channel?.nsfwOverride ?? null,
				reported_channel_content_warning_level: channel?.contentWarningLevel ?? null,
				reported_channel_content_warning_text: channel?.contentWarningText ?? null,
				reported_channel_effective_nsfw: null,
				reported_channel_effective_content_warning_level: null,
				reported_channel_effective_content_warning_text: null,
			};
		}
		const guildView = guildToContentWarningView(guild);
		let parentCategoryView: ContentWarningChannelLike | null = null;
		if (channel?.parentId) {
			const parent = await this.channelRepository.findUnique(channel.parentId);
			if (parent) {
				parentCategoryView = channelToContentWarningView(parent);
			}
		}
		let effectiveNsfw: boolean | null = null;
		let effectiveLevel: number | null = null;
		let effectiveText: string | null = null;
		if (channel) {
			const channelView = channelToContentWarningView(channel);
			effectiveNsfw = computeEffectiveChannelNsfw(channelView, parentCategoryView, guildView);
			const effective = computeEffectiveContentWarning(channelView, parentCategoryView, guildView);
			effectiveLevel = effective.level;
			effectiveText = effective.text;
		}
		return {
			reported_guild_nsfw: guild.nsfw,
			reported_guild_content_warning_level: guild.contentWarningLevel,
			reported_guild_content_warning_text: guild.contentWarningText,
			reported_channel_nsfw_override: channel?.nsfwOverride ?? null,
			reported_channel_content_warning_level: channel?.contentWarningLevel ?? null,
			reported_channel_content_warning_text: channel?.contentWarningText ?? null,
			reported_channel_effective_nsfw: effectiveNsfw,
			reported_channel_effective_content_warning_level: effectiveLevel,
			reported_channel_effective_content_warning_text: effectiveText,
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

	private async readDsaTicket(ticket: string): Promise<DSAReportTicketRow> {
		const ticketRow = await this.reportRepository.getDsaTicket(ticket);
		if (!ticketRow || ticketRow.expires_at.getTime() < Date.now()) {
			throw new InvalidDsaTicketError();
		}
		return ticketRow;
	}

	private generateDsaVerificationCode(): string {
		const segments: Array<string> = [];
		for (let i = 0; i < 2; i += 1) {
			let segment = '';
			for (let j = 0; j < DSA_CODE_SEGMENT_LENGTH; j += 1) {
				segment += DSA_CODE_CHARSET[randomInt(DSA_CODE_CHARSET.length)];
			}
			segments.push(segment);
		}
		return segments.join(DSA_CODE_SEPARATOR);
	}

	private hashVerificationCode(code: string): string {
		return createHash('sha256').update(code).digest('hex');
	}

	private normalizeEmail(email: string): string {
		return email.trim().toLowerCase();
	}

	private parseFluxerTag(tag: string): {
		username: string;
		discriminator: number;
	} | null {
		const trimmed = tag.trim();
		const match = /^(.+)#(\d{4})$/.exec(trimmed);
		if (!match) return null;
		return {
			username: match[1],
			discriminator: Number.parseInt(match[2], 10),
		};
	}

	private extractChannelAndMessageFromLink(link: string): {
		channelId: ChannelID;
		messageId: MessageID;
	} {
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
		const channelId = SnowflakeType.safeParse(segments[2]);
		const messageId = SnowflakeType.safeParse(segments[3]);
		if (!channelId.success || !messageId.success) {
			throw new UnknownMessageError();
		}
		return {
			channelId: createChannelID(channelId.data),
			messageId: createMessageID(messageId.data),
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
			throw new FeatureTemporarilyDisabledError();
		}
		const {hits} = await this.reportSearchService.listReportsByReporter(reporterId, limit, offset);
		const reportIds = hits.map((hit) => createReportID(BigInt(hit.id)));
		const reports = await Promise.all(reportIds.map((id) => this.reportRepository.getReport(id)));
		return reports.filter((report): report is IARSubmission => report !== null);
	}

	async listReportsByStatus(status: number, limit?: number, offset?: number): Promise<Array<IARSubmission>> {
		if (!this.reportSearchService) {
			throw new FeatureTemporarilyDisabledError();
		}
		const {hits} = await this.reportSearchService.listReportsByStatus(status, limit, offset);
		const reportIds = hits.map((hit) => createReportID(BigInt(hit.id)));
		const reports = await Promise.all(reportIds.map((id) => this.reportRepository.getReport(id)));
		return reports.filter((report): report is IARSubmission => report !== null);
	}

	async resolveReport(
		reportId: ReportID,
		adminUserId: UserID,
		publicComment: string | null,
		auditLogReason: string | null,
	): Promise<IARSubmission> {
		const report = await this.reportRepository.resolveReport(reportId, adminUserId, publicComment, auditLogReason);
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
		authChannel?: AuthenticatedChannel,
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
		const allMessages = await this.filterReportableContextMessages(
			[...messagesBefore, targetMessage, ...messagesAfter],
			authChannel,
		);
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

	private async filterReportableContextMessages(
		messages: Array<Message>,
		authChannel?: AuthenticatedChannel,
	): Promise<Array<Message>> {
		if (!authChannel?.guild) {
			return messages;
		}
		if (await authChannel.hasPermission(Permissions.READ_MESSAGE_HISTORY)) {
			return messages;
		}
		const cutoff = authChannel.guild.message_history_cutoff;
		if (!cutoff) {
			return [];
		}
		const cutoffTime = new Date(cutoff).getTime();
		return messages.filter((message) => snowflakeToDate(message.id).getTime() >= cutoffTime);
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

	private createReportRateLimitIdentifier(key: string): string {
		return `report:create:${key}`;
	}

	private async consumeMessageReportRateLimits({
		reporter,
		channel,
		message,
	}: {
		reporter: ReporterMetadata;
		channel: Channel;
		message: Message;
	}): Promise<void> {
		const reporterKey = this.getReporterRateLimitKey(reporter);
		const checks: Array<{
			identifier: string;
			maxAttempts: number;
		}> = [
			{
				identifier: this.createReportRateLimitIdentifier(reporterKey),
				maxAttempts: REPORT_RATE_LIMIT_MAX,
			},
			{
				identifier: `report:message:channel:${reporterKey}:${channel.id.toString()}`,
				maxAttempts: MESSAGE_REPORT_USER_CHANNEL_RATE_LIMIT_MAX,
			},
			{
				identifier: `report:message:target:${message.id.toString()}`,
				maxAttempts: MESSAGE_REPORT_TARGET_MESSAGE_RATE_LIMIT_MAX,
			},
		];
		if (channel.guildId) {
			checks.push({
				identifier: `report:message:guild:${reporterKey}:${channel.guildId.toString()}`,
				maxAttempts: MESSAGE_REPORT_USER_GUILD_RATE_LIMIT_MAX,
			});
		}
		for (const check of checks) {
			await this.ensureReportRateLimit(check.identifier, check.maxAttempts, false);
		}
		for (const check of checks) {
			await this.ensureReportRateLimit(check.identifier, check.maxAttempts, true);
		}
	}

	private async ensureReportRateLimit(identifier: string, maxAttempts: number, consume: boolean): Promise<void> {
		const result = consume
			? await this.rateLimitService.checkLimit({
					identifier,
					maxAttempts,
					windowMs: REPORT_RATE_LIMIT_WINDOW,
				})
			: await this.rateLimitService.peekLimit({
					identifier,
					maxAttempts,
					windowMs: REPORT_RATE_LIMIT_WINDOW,
				});
		if (!result.allowed) {
			throw new RateLimitError({
				retryAfter: result.retryAfter,
				retryAfterDecimal: result.retryAfterDecimal,
				limit: result.limit,
				resetTime: result.resetTime,
				resetAfterDecimal: result.resetAfterDecimal,
			});
		}
	}

	public shutdown(): void {}
}
