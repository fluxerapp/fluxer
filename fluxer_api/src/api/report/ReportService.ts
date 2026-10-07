// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash, randomBytes, randomInt} from 'node:crypto';
import type {ChannelID, GuildID, InviteCode, MessageID, ReportID, UserID, WebhookID} from '@app/api/BrandedTypes';
import {
	createAttachmentID,
	createChannelID,
	createGuildID,
	createInviteCode,
	createMessageID,
	createReportID,
	createUserID,
	guildIdToRoleId,
} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import type {IChannelRepository} from '@app/api/channel/IChannelRepository';
import type {AuthenticatedChannel} from '@app/api/channel/services/AuthenticatedChannel';
import {MessageChannelAuthService} from '@app/api/channel/services/message/MessageChannelAuthService';
import * as MessageHelpers from '@app/api/channel/services/message/MessageHelpers';
import type {ContentWarningChannelLike} from '@app/api/channel/utils/EffectiveContentWarning';
import {
	channelToContentWarningView,
	computeEffectiveChannelNsfw,
	computeEffectiveContentWarning,
	guildToContentWarningView,
} from '@app/api/channel/utils/EffectiveContentWarning';
import {resolveNsfwScopeChannel} from '@app/api/channel/utils/ThreadNsfwScope';
import type {MessageAttachment} from '@app/api/database/types/MessageTypes';
import type {
	DSAReportTicketRow,
	GuildReportSubmissionByReporterRow,
	MessageReportSubmissionByReporterRow,
	UserReportSubmissionByReporterRow,
} from '@app/api/database/types/ReportTypes';
import type {ThreadViewer} from '@app/api/experiment/ChannelThreadsGate';
import type {IGuildRepositoryAggregate} from '@app/api/guild/repositories/IGuildRepositoryAggregate';
import {emitActivity} from '@app/api/infrastructure/activity/ActivityEvents';
import type {ReportOutcome, ReportTarget, ResolvedBy} from '@app/api/infrastructure/activity/Contract.generated';
import {emitReportResolved} from '@app/api/infrastructure/activity/ModerationEvents';
import type {IEmailDnsValidationService} from '@app/api/infrastructure/IEmailDnsValidationService';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {ISnowflakeService} from '@app/api/infrastructure/ISnowflakeService';
import type {IStorageService} from '@app/api/infrastructure/IStorageService';
import type {IInviteRepository} from '@app/api/invite/IInviteRepository';
import {Logger} from '@app/api/Logger';
import type {Attachment} from '@app/api/models/Attachment';
import type {Channel} from '@app/api/models/Channel';
import type {Guild} from '@app/api/models/Guild';
import type {GuildMember} from '@app/api/models/GuildMember';
import type {Message} from '@app/api/models/Message';
import type {User} from '@app/api/models/User';
import type {Webhook} from '@app/api/models/Webhook';
import {resolveReportFlowAnswers, resolveReportFlowLocale} from '@app/api/report/flows/ReportFlowRegistry';
import type {
	IARMessageContextRow,
	IARSubmission,
	IARSubmissionRow,
	IReportRepository,
} from '@app/api/report/IReportRepository';
import {ReportStatus, ReportType} from '@app/api/report/IReportRepository';
import type {IReportSearchService} from '@app/api/search/IReportSearchService';
import type {IUserRepository} from '@app/api/user/IUserRepository';
import {isUnderEnforcement} from '@app/api/user/ProfileVisibility';
import {isAccountClosed} from '@app/api/user/UserHelpers';
import type {IWebhookRepository} from '@app/api/webhook/IWebhookRepository';
import {buildHashedAssetKey} from '@app/api/worker/utils/AssetArchiveHelpers';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ME} from '@fluxer/constants/src/AppConstants';
import {InviteTypes, MessageFlags, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import {DELETED_USER_USERNAME, UserFlags} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {CannotReportOwnMessageError} from '@fluxer/errors/src/domains/channel/CannotReportOwnMessageError';
import {UnknownChannelError} from '@fluxer/errors/src/domains/channel/UnknownChannelError';
import {UnknownMessageError} from '@fluxer/errors/src/domains/channel/UnknownMessageError';
import {ConflictError} from '@fluxer/errors/src/domains/core/ConflictError';
import {FeatureTemporarilyDisabledError} from '@fluxer/errors/src/domains/core/FeatureTemporarilyDisabledError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {RateLimitError} from '@fluxer/errors/src/domains/core/RateLimitError';
import {ServiceUnavailableError} from '@fluxer/errors/src/domains/core/ServiceUnavailableError';
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
import type {ReportFlowSurface} from '@fluxer/schema/src/domains/report/ReportFlowSchemas';
import {
	type ReportProfileAssetSnapshot,
	type ReportProfileGuildSnapshot,
	type ReportProfileMemberSnapshot,
	type ReportProfileUserSnapshot,
	serializeReportProfileSnapshot,
} from '@fluxer/schema/src/domains/report/ReportProfileSnapshotSchemas';
import type {DsaReportRequest} from '@fluxer/schema/src/domains/report/ReportSchemas';
import {SnowflakeType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {snowflakeToDate} from '@fluxer/snowflake/src/Snowflake';
import type {IEmailService} from '@pkgs/email/src/IEmailService';
import type {IRateLimitService} from '@pkgs/rate_limit/src/IRateLimitService';
import {ms} from 'itty-time';

interface ReporterMetadata {
	id: UserID | null;
	email: string | null;
	fullLegalName: string | null;
	countryOfResidence: string | null;
}

export interface ReportFlowRecord {
	reason: string;
	revisionHash: string;
	stepsJson: string;
	locale: string;
	surface: ReportFlowSurface;
}

interface ReportedAuthor {
	userId: UserID | null;
	webhookId: WebhookID | null;
	webhookName: string | null;
	webhookAvatarHash: string | null;
}

interface DsaReportClassification {
	category: string;
	flow: ReportFlowRecord | null;
}

type MessageContextScope = 'window' | 'target';

interface ProfileSnapshotSubject {
	user?: User | null;
	memberGuildId?: GuildID | null;
	guild?: Guild | null;
}

interface ClonedReportAttachments {
	preserved: Array<MessageAttachment>;
	missing: Array<MessageAttachment>;
}

interface DsaReportDraft {
	row: IARSubmissionRow;
	profileSubject: ProfileSnapshotSubject;
}

type ReportFlowColumns = Pick<
	IARSubmissionRow,
	'reason' | 'flow_revision' | 'flow_steps' | 'flow_locale' | 'flow_surface' | 'reporter_good_faith_confirmed'
>;

function buildReportFlowColumns(flow: ReportFlowRecord | null): ReportFlowColumns {
	return {
		reason: flow?.reason ?? null,
		flow_revision: flow?.revisionHash ?? null,
		flow_steps: flow?.stepsJson ?? null,
		flow_locale: flow?.locale ?? null,
		flow_surface: flow?.surface ?? null,
		reporter_good_faith_confirmed: flow?.surface === 'dsa' ? true : null,
	};
}

type ReportedWebhookColumns = Pick<
	IARSubmissionRow,
	| 'reported_webhook_id'
	| 'reported_webhook_name'
	| 'reported_webhook_avatar_hash'
	| 'reported_webhook_default_name'
	| 'reported_webhook_default_avatar_hash'
	| 'reported_webhook_type'
	| 'reported_webhook_application_id'
	| 'reported_webhook_channel_id'
	| 'reported_webhook_guild_id'
	| 'reported_webhook_created_at'
	| 'reported_webhook_creator_id'
	| 'reported_webhook_creator_username'
	| 'reported_webhook_creator_discriminator'
	| 'reported_webhook_creator_global_name'
	| 'reported_webhook_creator_avatar_hash'
>;

function buildReportedWebhookColumns(
	author: ReportedAuthor | null,
	webhook: Webhook | null = null,
	creator: User | null = null,
): ReportedWebhookColumns {
	const creatorSnapshot = creator && !isAccountClosed(creator) ? creator : null;
	return {
		reported_webhook_id: author?.webhookId ?? null,
		reported_webhook_name: author?.webhookName ?? null,
		reported_webhook_avatar_hash: author?.webhookAvatarHash ?? null,
		reported_webhook_default_name: webhook?.name ?? null,
		reported_webhook_default_avatar_hash: webhook?.avatarHash ?? null,
		reported_webhook_type: webhook?.type ?? null,
		reported_webhook_application_id: creator?.isBot ? creator.id : null,
		reported_webhook_channel_id: webhook?.channelId ?? null,
		reported_webhook_guild_id: webhook?.guildId ?? null,
		reported_webhook_created_at: author?.webhookId ? snowflakeToDate(author.webhookId) : null,
		reported_webhook_creator_id: webhook?.creatorId ?? null,
		reported_webhook_creator_username: creatorSnapshot?.username ?? null,
		reported_webhook_creator_discriminator: creatorSnapshot?.discriminator ?? null,
		reported_webhook_creator_global_name: creatorSnapshot?.globalName ?? null,
		reported_webhook_creator_avatar_hash: creatorSnapshot?.avatarHash ?? null,
	};
}

function messageAuthor(message: Message): ReportedAuthor | null {
	if (message.authorId) {
		return {userId: message.authorId, webhookId: null, webhookName: null, webhookAvatarHash: null};
	}
	if (message.webhookId) {
		return {
			userId: null,
			webhookId: message.webhookId,
			webhookName: message.webhookName,
			webhookAvatarHash: message.webhookAvatarHash,
		};
	}
	return null;
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
const DSA_EMAIL_SEND_RECIPIENT_MAX = 3;
const DSA_EMAIL_SEND_RECIPIENT_WINDOW = ms('1 hour');
const DSA_EMAIL_RESEND_COOLDOWN = ms('1 minute');
const DSA_EMAIL_VERIFY_ADDRESS_MAX = 5;
const DSA_EMAIL_VERIFY_ADDRESS_WINDOW = ms('10 minutes');
const PUBLIC_CHANNEL_PERMISSIONS = Permissions.VIEW_CHANNEL | Permissions.READ_MESSAGE_HISTORY;

async function emitReportFiled(row: IARSubmissionRow, target: ReportTarget): Promise<void> {
	const key = row.reported_user_id ?? row.reporter_id ?? 0n;
	await emitActivity(
		'report_filed',
		key.toString(),
		{
			report_id: row.report_id.toString(),
			reporter_id: (row.reporter_id ?? 0n).toString(),
			category: row.category,
			target_type: target,
			reported_user_id: row.reported_user_id?.toString() ?? null,
			guild_id: row.reported_guild_id?.toString() ?? null,
			message_id: row.reported_message_id?.toString() ?? null,
			channel_id: row.reported_channel_id?.toString() ?? null,
		},
		null,
		row.report_id.toString(),
	);
}

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
		private webhookRepository: IWebhookRepository,
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
		viewer: ThreadViewer,
		channelId: ChannelID,
		messageId: MessageID,
		category: string,
		flow: ReportFlowRecord | null = null,
	): Promise<IARSubmission> {
		await this.checkReportBan(reporter.id);
		const reporterKey = this.getReporterRateLimitKey(reporter);
		await this.ensureReportRateLimit(this.createReportRateLimitIdentifier(reporterKey), REPORT_RATE_LIMIT_MAX, false);
		const {authChannel, channel, message} = await this.getReportableMessageForReporter({
			reporterId: reporter.id,
			viewer,
			channelId,
			messageId,
		});
		const author = await this.resolveReportedAuthor(message);
		if (!author) {
			throw new UnknownMessageError();
		}
		if (reporter.id && author.userId === reporter.id) {
			throw new CannotReportOwnMessageError();
		}
		const [reportedUser, messageContext, webhookColumns] = await Promise.all([
			author.userId ? this.userRepository.findUnique(author.userId) : null,
			this.gatherMessageContext(channelId, messageId, author, authChannel),
			this.snapshotReportedWebhook(author),
		]);
		if (author.userId && !reportedUser) {
			throw new UnknownUserError();
		}
		const reportId = createReportID(await this.snowflakeService.generate());
		const guild = channel.guildId ? await this.guildRepository.findUnique(channel.guildId) : null;
		const contentWarningSnapshot = await this.buildContentWarningSnapshot(guild, channel);
		const reportData: IARSubmissionRow = {
			report_id: reportId,
			reporter_id: reporter.id,
			reporter_email: null,
			reporter_full_legal_name: reporter.fullLegalName,
			reporter_country_of_residence: reporter.countryOfResidence,
			reported_at: new Date(),
			status: ReportStatus.PENDING,
			report_type: ReportType.MESSAGE,
			category,
			additional_info: null,
			reported_user_id: author.userId,
			reported_user_avatar_hash: reportedUser?.avatarHash || null,
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
			...buildReportFlowColumns(flow),
			...webhookColumns,
		};
		let reservation: MessageReportSubmissionByReporterRow | null = null;
		if (reporter.id) {
			reservation = {
				reporter_id: reporter.id,
				channel_id: channelId,
				message_id: messageId,
				report_id: reportId,
				reported_at: reportData.reported_at,
			};
			if (!(await this.reportRepository.reserveMessageReportByReporter(reservation))) {
				throw new ConflictError({code: APIErrorCodes.CONFLICT});
			}
		}
		try {
			await this.consumeMessageReportRateLimits({reporter, channel, message});
			reportData.reported_profile_snapshot = await this.captureProfileSnapshot(reportId, {
				user: reportedUser,
				memberGuildId: channel.guildId,
			});
			const report = await this.reportRepository.createReport(reportData);
			await emitReportFiled(reportData, 'message');
			if (this.reportSearchService && 'indexReport' in this.reportSearchService) {
				await this.reportSearchService.indexReport(report).catch((error) => {
					Logger.error({error, reportId: report.reportId}, 'Failed to index message report in search');
				});
			}
			return report;
		} catch (error) {
			if (reservation) {
				await this.reportRepository.releaseMessageReportByReporter(reservation).catch((cleanupError) => {
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
		flow: ReportFlowRecord | null = null,
	): Promise<IARSubmission> {
		await this.checkReportBan(reporter.id);
		const rateLimitIdentifier = this.createReportRateLimitIdentifier(this.getReporterRateLimitKey(reporter));
		await this.ensureReportRateLimit(rateLimitIdentifier, REPORT_RATE_LIMIT_MAX, false);
		if (reporter.id && reportedUserId === reporter.id) {
			throw new CannotReportYourselfError();
		}
		const reportedUser = await this.userRepository.findUnique(reportedUserId);
		if (!reportedUser) {
			throw new UnknownUserError();
		}
		const reportId = createReportID(await this.snowflakeService.generate());
		const guild = guildId ? await this.getGuildContextForReporter(reporter.id, guildId) : null;
		const contentWarningSnapshot = await this.buildContentWarningSnapshot(guild, null);
		const reportData: IARSubmissionRow = {
			report_id: reportId,
			reporter_id: reporter.id,
			reporter_email: null,
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
			...buildReportFlowColumns(flow),
			...buildReportedWebhookColumns(null),
		};
		let reservation: UserReportSubmissionByReporterRow | null = null;
		if (reporter.id) {
			reservation = {
				reporter_id: reporter.id,
				reported_user_id: reportedUserId,
				report_id: reportId,
				reported_at: reportData.reported_at,
			};
			if (!(await this.reportRepository.reserveUserReportByReporter(reservation))) {
				throw new ConflictError({code: APIErrorCodes.CONFLICT});
			}
		}
		try {
			await this.ensureReportRateLimit(rateLimitIdentifier, REPORT_RATE_LIMIT_MAX, true);
			reportData.reported_profile_snapshot = await this.captureProfileSnapshot(reportId, {
				user: reportedUser,
				memberGuildId: guildId,
			});
			const report = await this.reportRepository.createReport(reportData);
			await emitReportFiled(reportData, 'user');
			if (this.reportSearchService && 'indexReport' in this.reportSearchService) {
				await this.reportSearchService.indexReport(report).catch((error) => {
					Logger.error({error, reportId: report.reportId}, 'Failed to index user report in search');
				});
			}
			return report;
		} catch (error) {
			if (reservation) {
				await this.reportRepository.releaseUserReportByReporter(reservation).catch((cleanupError) => {
					Logger.error({error: cleanupError, reportId}, 'Failed to clean up user report duplicate reservation');
				});
			}
			throw error;
		}
	}

	async reportGuild(
		reporter: ReporterMetadata,
		guildId: GuildID,
		category: string,
		inviteCode?: InviteCode,
	): Promise<IARSubmission> {
		await this.checkReportBan(reporter.id);
		const rateLimitIdentifier = this.createReportRateLimitIdentifier(this.getReporterRateLimitKey(reporter));
		await this.ensureReportRateLimit(rateLimitIdentifier, REPORT_RATE_LIMIT_MAX, false);
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
			reporter_email: null,
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
			...buildReportFlowColumns(null),
			...buildReportedWebhookColumns(null),
		};
		let reservation: GuildReportSubmissionByReporterRow | null = null;
		if (reporter.id) {
			reservation = {
				reporter_id: reporter.id,
				reported_guild_id: guildId,
				report_id: reportId,
				reported_at: reportData.reported_at,
			};
			if (!(await this.reportRepository.reserveGuildReportByReporter(reservation))) {
				throw new ConflictError({code: APIErrorCodes.CONFLICT});
			}
		}
		try {
			await this.ensureReportRateLimit(rateLimitIdentifier, REPORT_RATE_LIMIT_MAX, true);
			reportData.reported_profile_snapshot = await this.captureProfileSnapshot(reportId, {guild});
			const report = await this.reportRepository.createReport(reportData);
			await emitReportFiled(reportData, 'guild');
			if (this.reportSearchService && 'indexReport' in this.reportSearchService) {
				await this.reportSearchService.indexReport(report).catch((error) => {
					Logger.error({error, reportId: report.reportId}, 'Failed to index guild report in search');
				});
			}
			return report;
		} catch (error) {
			if (reservation) {
				await this.reportRepository.releaseGuildReportByReporter(reservation).catch((cleanupError) => {
					Logger.error({error: cleanupError, reportId}, 'Failed to clean up guild report duplicate reservation');
				});
			}
			throw error;
		}
	}

	private async getGuildContextForReporter(reporterId: UserID | null, guildId: GuildID): Promise<Guild> {
		const guild = await this.guildRepository.findUnique(guildId);
		if (!guild || !reporterId || !(await this.guildRepository.getMember(guildId, reporterId))) {
			throw new UnknownGuildError();
		}
		return guild;
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

	async sendDsaReportVerificationCode(email: string, locale: string | null = null): Promise<void> {
		const normalizedEmail = this.normalizeEmail(email);
		await this.ensureDsaResendCooldown(normalizedEmail);
		const recipientLimit = await this.rateLimitService.checkLimit({
			identifier: `dsa:report:email:send:recipient:${normalizedEmail}`,
			maxAttempts: DSA_EMAIL_SEND_RECIPIENT_MAX,
			windowMs: DSA_EMAIL_SEND_RECIPIENT_WINDOW,
		});
		if (!recipientLimit.allowed) {
			throw new RateLimitError({
				retryAfter: recipientLimit.retryAfter,
				retryAfterDecimal: recipientLimit.retryAfterDecimal,
				limit: recipientLimit.limit,
				resetTime: recipientLimit.resetTime,
				resetAfterDecimal: recipientLimit.resetAfterDecimal,
			});
		}
		const hasValidDns = await this.emailDnsValidationService.hasValidDnsRecords(normalizedEmail);
		if (!hasValidDns) {
			throw InputValidationError.fromCode('email', ValidationErrorCodes.EMAIL_DOMAIN_CANNOT_RECEIVE_MAIL);
		}
		const verificationCode = this.generateDsaVerificationCode();
		const codeHash = this.hashVerificationCode(verificationCode);
		const expiresAt = new Date(Date.now() + ms('10 minutes'));
		await this.reportRepository.upsertDsaEmailVerification({
			email_lower: normalizedEmail,
			code_hash: codeHash,
			expires_at: expiresAt,
			last_sent_at: new Date(),
		});
		await this.rateLimitService.resetLimit(this.createDsaVerifyAttemptIdentifier(normalizedEmail));
		const sent = await this.emailService.sendDsaReportVerificationCode(
			normalizedEmail,
			verificationCode,
			expiresAt,
			locale,
		);
		if (!sent) {
			await this.reportRepository.consumeDsaEmailVerification(normalizedEmail, codeHash);
			Logger.warn({template: 'dsa_report_verification'}, 'DSA report verification email was not sent');
			throw new ServiceUnavailableError();
		}
	}

	private async ensureDsaResendCooldown(normalizedEmail: string): Promise<void> {
		const current = await this.reportRepository.getDsaEmailVerification(normalizedEmail);
		if (!current) {
			return;
		}
		const now = Date.now();
		if (current.expires_at.getTime() < now) {
			return;
		}
		const remainingMs = current.last_sent_at.getTime() + DSA_EMAIL_RESEND_COOLDOWN - now;
		if (remainingMs <= 0) {
			return;
		}
		throw new RateLimitError({
			retryAfter: Math.ceil(remainingMs / 1000),
			retryAfterDecimal: remainingMs / 1000,
			limit: 1,
			resetTime: new Date(now + remainingMs),
			resetAfterDecimal: remainingMs / 1000,
		});
	}

	private createDsaVerifyAttemptIdentifier(normalizedEmail: string): string {
		return `dsa:report:email:verify:address:${normalizedEmail}`;
	}

	async verifyDsaReportEmail(email: string, code: string): Promise<string> {
		const normalizedEmail = this.normalizeEmail(email);
		const verificationRow = await this.reportRepository.getDsaEmailVerification(normalizedEmail);
		const attempt = await this.rateLimitService.checkLimit({
			identifier: this.createDsaVerifyAttemptIdentifier(normalizedEmail),
			maxAttempts: DSA_EMAIL_VERIFY_ADDRESS_MAX,
			windowMs: DSA_EMAIL_VERIFY_ADDRESS_WINDOW,
		});
		if (!attempt.allowed) {
			if (verificationRow) {
				await this.reportRepository.consumeDsaEmailVerification(normalizedEmail, verificationRow.code_hash);
			}
			throw new InvalidDsaVerificationCodeError();
		}
		if (!verificationRow || verificationRow.expires_at.getTime() < Date.now()) {
			throw new InvalidDsaVerificationCodeError();
		}
		if (this.hashVerificationCode(code) !== verificationRow.code_hash) {
			throw new InvalidDsaVerificationCodeError();
		}
		const consumed = await this.reportRepository.consumeDsaEmailVerification(
			normalizedEmail,
			verificationRow.code_hash,
		);
		if (!consumed) {
			throw new InvalidDsaVerificationCodeError();
		}
		const ticket = randomBytes(DSA_TICKET_BYTES).toString('hex');
		await this.reportRepository.createDsaTicket({
			ticket,
			email_lower: normalizedEmail,
			expires_at: new Date(Date.now() + ms('1 hour')),
			created_at: new Date(),
		});
		return ticket;
	}

	async createDsaReport(report: DsaReportRequest, requestLocale: string | null = null): Promise<IARSubmission> {
		const ticket = await this.readDsaTicket(report.ticket);
		const classification = this.classifyDsaReport(report, requestLocale);
		const reporterMeta: ReporterMetadata = {
			id: null,
			email: ticket.email_lower,
			fullLegalName: report.reporter_full_legal_name ?? null,
			countryOfResidence: report.reporter_country_of_residence,
		};
		await this.checkReportBan(null);
		const reporterKey = this.getReporterRateLimitKey(reporterMeta);
		await this.ensureReportRateLimit(this.createReportRateLimitIdentifier(reporterKey), REPORT_RATE_LIMIT_MAX, false);
		const reportId = createReportID(await this.snowflakeService.generate());
		const {row: reportRow, profileSubject} = await this.buildDsaReportRow(
			reportId,
			report,
			reporterMeta,
			classification,
		);
		await this.ensureReportRateLimit(this.createReportRateLimitIdentifier(reporterKey), REPORT_RATE_LIMIT_MAX, true);
		const claimed = await this.reportRepository.consumeDsaTicket(report.ticket, ticket.email_lower);
		if (!claimed) {
			throw new InvalidDsaTicketError();
		}
		reportRow.reported_profile_snapshot = await this.captureProfileSnapshot(reportId, profileSubject);
		const createdReport = await this.reportRepository.createReport(reportRow);
		await emitReportFiled(reportRow, 'dsa');
		if (this.reportSearchService && 'indexReport' in this.reportSearchService) {
			await this.reportSearchService.indexReport(createdReport).catch((error) => {
				Logger.error({error, reportId: createdReport.reportId}, 'Failed to index DSA report in search');
			});
		}
		await this.sendDsaReportReceipt(
			ticket.email_lower,
			createdReport.reportId,
			report.report_type,
			classification.flow?.locale ?? requestLocale,
		);
		return createdReport;
	}

	private async sendDsaReportReceipt(
		email: string,
		reportId: ReportID,
		targetKind: DsaReportRequest['report_type'],
		locale: string | null,
	): Promise<void> {
		try {
			const sent = await this.emailService.sendReportReceivedEmail(email, reportId.toString(), targetKind, locale);
			if (!sent) {
				Logger.warn({template: 'report_received', reportId}, 'DSA report receipt was not sent');
			}
		} catch (error) {
			Logger.error({error, reportId}, 'Failed to send DSA report receipt');
		}
	}

	private classifyDsaReport(report: DsaReportRequest, requestLocale: string | null): DsaReportClassification {
		if (!report.steps || !report.revision_hash) {
			if (!report.category) {
				throw InputValidationError.fromCode('category', ValidationErrorCodes.INVALID_FORMAT);
			}
			return {category: report.category, flow: null};
		}
		const answers = resolveReportFlowAnswers({
			target: report.report_type,
			surface: 'dsa',
			revisionHash: report.revision_hash,
			steps: report.steps,
		});
		if (!answers.isCurrentRevision) {
			Logger.warn(
				{
					clientRevision: report.revision_hash,
					currentRevision: answers.currentRevisionHash,
					target: report.report_type,
				},
				'Accepted a DSA report flow submission from an outdated revision',
			);
		}
		if (answers.legacyCategory !== 'child_safety' && !report.reporter_full_legal_name) {
			throw InputValidationError.fromCode('reporter_full_legal_name', ValidationErrorCodes.INVALID_FORMAT);
		}
		return {
			category: answers.legacyCategory,
			flow: {
				reason: answers.reason,
				revisionHash: report.revision_hash,
				stepsJson: answers.stepsJson,
				locale: resolveReportFlowLocale(report.locale ?? requestLocale),
				surface: 'dsa',
			},
		};
	}

	private async buildDsaReportRow(
		reportId: ReportID,
		report: DsaReportRequest,
		reporter: ReporterMetadata,
		classification: DsaReportClassification,
	): Promise<DsaReportDraft> {
		switch (report.report_type) {
			case 'message':
				return this.buildDsaMessageReportRow(reportId, report, reporter, classification);
			case 'user':
				return this.buildDsaUserReportRow(reportId, report, reporter, classification);
			case 'guild':
				return this.buildDsaGuildReportRow(reportId, report, reporter, classification);
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
		classification: DsaReportClassification,
	): Promise<DsaReportDraft> {
		const {guildSegment, channelId, messageId} = this.extractChannelAndMessageFromLink(report.message_link);
		const channel = await this.channelRepository.findUnique(channelId);
		if (!channel || guildSegment !== (channel.guildId?.toString() ?? ME)) {
			throw new UnknownMessageError();
		}
		const message = await this.channelRepository.getMessage(channelId, messageId);
		if (!message || message.channelId !== channelId) {
			throw new UnknownMessageError();
		}
		const author = await this.resolveReportedAuthor(message);
		if (!author) {
			throw new UnknownMessageError();
		}
		if (report.reported_user_tag && !(await this.isTagOfUser(report.reported_user_tag, author.userId))) {
			throw new UnknownMessageError();
		}
		const reportedUser = author.userId ? await this.userRepository.findUnique(author.userId) : null;
		if (author.userId && !reportedUser) {
			throw new UnknownMessageError();
		}
		const scope: MessageContextScope = (await this.isChannelReadableByEveryone(channel)) ? 'window' : 'target';
		const [messageContext, webhookColumns] = await Promise.all([
			this.gatherMessageContext(channelId, messageId, author, undefined, scope),
			this.snapshotReportedWebhook(author),
		]);
		const guild = channel.guildId ? await this.guildRepository.findUnique(channel.guildId) : null;
		const contentWarningSnapshot = await this.buildContentWarningSnapshot(guild, channel);
		const row: IARSubmissionRow = {
			report_id: reportId,
			reporter_id: null,
			reporter_email: reporter.email,
			reporter_full_legal_name: reporter.fullLegalName,
			reporter_country_of_residence: reporter.countryOfResidence,
			reported_at: new Date(),
			status: ReportStatus.PENDING,
			report_type: ReportType.MESSAGE,
			category: classification.category,
			additional_info: report.additional_info ?? null,
			reported_user_id: author.userId,
			reported_user_avatar_hash: reportedUser?.avatarHash || null,
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
			...buildReportFlowColumns(classification.flow),
			...webhookColumns,
		};
		return {row, profileSubject: {user: reportedUser, memberGuildId: channel.guildId}};
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
		classification: DsaReportClassification,
	): Promise<DsaReportDraft> {
		const target = await this.resolveDsaUser(report.user_id ?? undefined, report.user_tag ?? undefined);
		const contentWarningSnapshot = await this.buildContentWarningSnapshot(null, null);
		const row: IARSubmissionRow = {
			report_id: reportId,
			reporter_id: null,
			reporter_email: reporter.email,
			reporter_full_legal_name: reporter.fullLegalName,
			reporter_country_of_residence: reporter.countryOfResidence,
			reported_at: new Date(),
			status: ReportStatus.PENDING,
			report_type: ReportType.USER,
			category: classification.category,
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
			...buildReportFlowColumns(classification.flow),
			...buildReportedWebhookColumns(null),
		};
		return {row, profileSubject: {user: target}};
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
		classification: DsaReportClassification,
	): Promise<DsaReportDraft> {
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
		const row: IARSubmissionRow = {
			report_id: reportId,
			reporter_id: null,
			reporter_email: reporter.email,
			reporter_full_legal_name: reporter.fullLegalName,
			reporter_country_of_residence: reporter.countryOfResidence,
			reported_at: new Date(),
			status: ReportStatus.PENDING,
			report_type: ReportType.GUILD,
			category: classification.category,
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
			...buildReportFlowColumns(classification.flow),
			...buildReportedWebhookColumns(null),
		};
		return {row, profileSubject: {guild}};
	}

	private async getReportableMessageForReporter({
		reporterId,
		viewer,
		channelId,
		messageId,
	}: {
		reporterId: UserID | null;
		viewer: ThreadViewer;
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
		const authChannel = await this.messageChannelAuthService.getChannelAuthenticated({
			userId: reporterId,
			channelId,
			viewer,
		});
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

	private async snapshotReportedWebhook(author: ReportedAuthor): Promise<ReportedWebhookColumns> {
		if (!author.webhookId) {
			return buildReportedWebhookColumns(author);
		}
		const webhook = await this.webhookRepository.findUnique(author.webhookId);
		const creator = webhook?.creatorId ? await this.userRepository.findUnique(webhook.creatorId) : null;
		return buildReportedWebhookColumns(author, webhook, creator);
	}

	private async resolveReportedAuthor(message: Message): Promise<ReportedAuthor | null> {
		if ((message.flags & MessageFlags.IS_CROSSPOST) === 0) {
			return messageAuthor(message);
		}
		if ((message.flags & MessageFlags.SOURCE_MESSAGE_DELETED) !== 0) {
			return null;
		}
		const reference = message.reference;
		if (!reference?.messageId) {
			return null;
		}
		const source = await this.channelRepository.getMessage(reference.channelId, reference.messageId);
		if (!source || (source.flags & MessageFlags.IS_CROSSPOST) !== 0) {
			return null;
		}
		return messageAuthor(source);
	}

	private async canAccessMessage(authChannel: AuthenticatedChannel, messageId: MessageID): Promise<boolean> {
		if (!authChannel.guild) {
			return true;
		}
		if (await authChannel.hasPermission(Permissions.READ_MESSAGE_HISTORY)) {
			return true;
		}
		const floorMs = this.reportableFloorMs(authChannel);
		return floorMs !== null && snowflakeToDate(messageId).getTime() >= floorMs;
	}

	private reportableFloorMs(authChannel: AuthenticatedChannel): number | null {
		const floors = [authChannel.guild?.message_history_cutoff, authChannel.member?.joined_at]
			.filter((value): value is string => Boolean(value))
			.map((value) => new Date(value).getTime());
		return floors.length > 0 ? Math.min(...floors) : null;
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
		const scope = channel
			? await resolveNsfwScopeChannel(channel, (channelId) => this.channelRepository.findUnique(channelId))
			: null;
		let parentCategoryView: ContentWarningChannelLike | null = null;
		if (scope?.parentId) {
			const parent = await this.channelRepository.findUnique(scope.parentId);
			if (parent) {
				parentCategoryView = channelToContentWarningView(parent);
			}
		}
		let effectiveNsfw: boolean | null = null;
		let effectiveLevel: number | null = null;
		let effectiveText: string | null = null;
		if (scope) {
			const channelView = channelToContentWarningView(scope);
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

	private async isTagOfUser(tag: string, userId: UserID | null): Promise<boolean> {
		const parsed = this.parseFluxerTag(tag);
		if (!parsed || !userId) {
			return false;
		}
		const user = await this.userRepository.findByUsernameDiscriminator(parsed.username, parsed.discriminator);
		return user?.id === userId;
	}

	private async isChannelReadableByEveryone(channel: Channel): Promise<boolean> {
		if (!channel.guildId) {
			return false;
		}
		const everyoneRole = await this.guildRepository.getRole(guildIdToRoleId(channel.guildId), channel.guildId);
		if (!everyoneRole) {
			return false;
		}
		if ((everyoneRole.permissions & Permissions.ADMINISTRATOR) !== 0n) {
			return true;
		}
		const overwrite = channel.permissionOverwrites.get(everyoneRole.id);
		const permissions = overwrite
			? (everyoneRole.permissions & ~overwrite.deny) | overwrite.allow
			: everyoneRole.permissions;
		return (permissions & PUBLIC_CHANNEL_PERMISSIONS) === PUBLIC_CHANNEL_PERMISSIONS;
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
		guildSegment: string;
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
			guildSegment: segments[1],
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

	async listReportsByStatus(
		status: number,
		limit?: number,
		offset?: number,
	): Promise<{reports: Array<IARSubmission>; total: number}> {
		if (!this.reportSearchService) {
			throw new FeatureTemporarilyDisabledError();
		}
		const {hits, total} = await this.reportSearchService.listReportsByStatus(status, limit, offset);
		const reportIds = hits.map((hit) => createReportID(BigInt(hit.id)));
		const loaded = await Promise.all(reportIds.map((id) => this.reportRepository.getReport(id)));
		const reports = loaded.filter((report): report is IARSubmission => report !== null);
		const orphanedReportIds = reportIds.filter((_, index) => loaded[index] === null).map((id) => id.toString());
		if (orphanedReportIds.length > 0) {
			Logger.warn(
				{orphanedReportIds, status},
				'Report search index lists reports that are no longer stored, run refresh_search_index reports',
			);
		}
		return {reports, total};
	}

	async resolveReport(
		reportId: ReportID,
		adminUserId: UserID,
		publicComment: string | null,
		auditLogReason: string | null,
		resolution: {outcome?: ReportOutcome; resolvedBy?: ResolvedBy} = {},
	): Promise<IARSubmission> {
		const report = await this.reportRepository.resolveReport(reportId, adminUserId, publicComment, auditLogReason);
		if (this.reportSearchService && 'updateReport' in this.reportSearchService) {
			await this.reportSearchService.updateReport(report).catch((error) => {
				Logger.error({error, reportId: report.reportId}, 'Failed to update report in search index');
			});
		}
		const outcome = resolution.outcome ?? (await this.observedOutcome(report));
		await emitReportResolved(report, outcome, resolution.resolvedBy ?? 'staff');
		return report;
	}

	private async observedOutcome(report: IARSubmission): Promise<ReportOutcome> {
		if (!report.reportedUserId) return 'unspecified';
		try {
			const reported = await this.userRepository.findUnique(report.reportedUserId);
			return reported && isUnderEnforcement(reported) ? 'actioned' : 'unspecified';
		} catch (error) {
			Logger.warn({error, reportId: report.reportId}, 'Could not read the reported account for a resolved report');
			return 'unspecified';
		}
	}

	private async gatherMessageContext(
		channelId: ChannelID,
		targetMessageId: MessageID,
		targetAuthor: ReportedAuthor,
		authChannel?: AuthenticatedChannel,
		scope: MessageContextScope = 'window',
	): Promise<Array<IARMessageContextRow>> {
		const messagesBefore =
			scope === 'window'
				? await this.channelRepository.listMessages(channelId, targetMessageId, MESSAGE_CONTEXT_WINDOW)
				: [];
		const messagesAfter =
			scope === 'window'
				? await this.channelRepository.listMessages(channelId, undefined, MESSAGE_CONTEXT_WINDOW, targetMessageId)
				: [];
		const targetMessage = await this.channelRepository.getMessage(channelId, targetMessageId);
		if (!targetMessage) {
			return [];
		}
		messagesBefore.reverse();
		const allMessages = await this.filterReportableContextMessages(
			[...messagesBefore, targetMessage, ...messagesAfter],
			authChannel,
		);
		const contextAuthor = (msg: Message): ReportedAuthor | null =>
			msg.id === targetMessageId ? targetAuthor : messageAuthor(msg);
		const userIds = new Set<UserID>();
		for (const msg of allMessages) {
			const authorId = contextAuthor(msg)?.userId;
			if (authorId) {
				userIds.add(authorId);
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
			const identity = contextAuthor(message);
			if (!identity) continue;
			const author = identity.userId ? users.get(identity.userId) : null;
			const attachments = await this.cloneAttachmentsForReport(
				message.attachments,
				MessageHelpers.attachmentStorageChannelId(message),
				channelId,
			);
			const embedAttachments = await this.cloneOwnedEmbedAttachmentsForReport(message, channelId);
			const preservedAttachments = [...attachments.preserved, ...embedAttachments.preserved];
			const missingAttachments = [...attachments.missing, ...embedAttachments.missing];
			context.push({
				message_id: message.id,
				channel_id: channelId,
				author_id: identity.userId,
				webhook_id: identity.webhookId,
				author_username: author ? author.username : (identity.webhookName ?? DELETED_USER_USERNAME),
				author_discriminator: author ? author.discriminator : 0,
				author_avatar_hash: author ? author.avatarHash || null : identity.webhookAvatarHash,
				content: message.content || null,
				timestamp: snowflakeToDate(message.id),
				edited_timestamp: message.editedTimestamp || null,
				type: message.type,
				flags: message.flags,
				mention_everyone: message.mentionEveryone,
				mention_users: message.mentionedUserIds.size > 0 ? Array.from(message.mentionedUserIds) : null,
				mention_roles: message.mentionedRoleIds.size > 0 ? Array.from(message.mentionedRoleIds) : null,
				mention_channels: message.mentionedChannelIds.size > 0 ? Array.from(message.mentionedChannelIds) : null,
				attachments: preservedAttachments.length > 0 ? preservedAttachments : null,
				missing_attachments: missingAttachments.length > 0 ? missingAttachments : null,
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
		const floorMs = this.reportableFloorMs(authChannel);
		if (floorMs === null) {
			return [];
		}
		return messages.filter((message) => snowflakeToDate(message.id).getTime() >= floorMs);
	}

	private async cloneAttachmentsForReport(
		attachments: Array<Attachment>,
		sourceChannelId: ChannelID,
		reportedChannelId: ChannelID,
	): Promise<ClonedReportAttachments> {
		const cloned: ClonedReportAttachments = {preserved: [], missing: []};
		for (const attachment of attachments) {
			const sourceKey = MessageHelpers.makeAttachmentCdnKey(sourceChannelId, attachment.id, attachment.filename);
			const snapshot: MessageAttachment = {
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
			try {
				await this.storageService.copyObject({
					sourceBucket: Config.s3.buckets.cdn,
					sourceKey,
					destinationBucket: Config.s3.buckets.reports,
					destinationKey: MessageHelpers.makeAttachmentCdnKey(reportedChannelId, attachment.id, attachment.filename),
					newContentType: attachment.contentType,
				});
				cloned.preserved.push(snapshot);
			} catch (error) {
				Logger.error(
					{error, attachmentId: attachment.id, filename: attachment.filename, sourceChannelId},
					'Failed to clone attachment for report',
				);
				cloned.missing.push(snapshot);
			}
		}
		return cloned;
	}

	private async cloneOwnedEmbedAttachmentsForReport(
		message: Message,
		reportedChannelId: ChannelID,
	): Promise<ClonedReportAttachments> {
		const cloned: ClonedReportAttachments = {preserved: [], missing: []};
		for (const {key, media} of MessageHelpers.collectOwnedEmbedAttachments(message)) {
			let snapshot: MessageAttachment | null = null;
			try {
				const [, , attachmentId, ...filenameParts] = key.split('/');
				const filename = filenameParts.join('/');
				const id = createAttachmentID(BigInt(attachmentId!));
				snapshot = {
					attachment_id: id,
					filename,
					size: 0n,
					title: null,
					description: media.description,
					width: media.width,
					height: media.height,
					content_type: media.content_type ?? 'application/octet-stream',
					content_hash: media.content_hash,
					placeholder: media.placeholder,
					flags: media.flags & ~MessageHelpers.EMBED_MEDIA_OWNED_ATTACHMENT_FLAG,
					duration: media.duration,
					nsfw: null,
					waveform: null,
				};
				const metadata = await this.storageService.getObjectMetadata(Config.s3.buckets.cdn, key);
				if (!metadata) {
					Logger.warn({key, reportedChannelId}, 'Embed attachment for report is no longer stored');
					cloned.missing.push(snapshot);
					continue;
				}
				snapshot.size = BigInt(metadata.contentLength);
				snapshot.content_type = media.content_type ?? metadata.contentType;
				await this.storageService.copyObject({
					sourceBucket: Config.s3.buckets.cdn,
					sourceKey: key,
					destinationBucket: Config.s3.buckets.reports,
					destinationKey: MessageHelpers.makeAttachmentCdnKey(reportedChannelId, id, filename),
					newContentType: snapshot.content_type,
				});
				cloned.preserved.push(snapshot);
			} catch (error) {
				Logger.error({error, key, reportedChannelId}, 'Failed to clone embed attachment for report');
				if (snapshot) {
					cloned.missing.push(snapshot);
				}
			}
		}
		return cloned;
	}

	private async captureProfileSnapshot(reportId: ReportID, subject: ProfileSnapshotSubject): Promise<string | null> {
		const user = subject.user ?? null;
		const guild = subject.guild ?? null;
		if (!user && !guild) {
			return null;
		}
		const member =
			user && subject.memberGuildId && !isAccountClosed(user)
				? await this.guildRepository.getMember(subject.memberGuildId, user.id)
				: null;
		return serializeReportProfileSnapshot({
			captured_at: new Date().toISOString(),
			user: user ? await this.snapshotUserProfile(reportId, user) : null,
			member: member ? await this.snapshotMemberProfile(reportId, member) : null,
			guild: guild ? await this.snapshotGuildProfile(reportId, guild) : null,
		});
	}

	private async snapshotUserProfile(reportId: ReportID, user: User): Promise<ReportProfileUserSnapshot> {
		const id = user.id.toString();
		if (isAccountClosed(user)) {
			return {
				id,
				username: null,
				discriminator: null,
				global_name: null,
				bio: null,
				pronouns: null,
				avatar: null,
				banner: null,
			};
		}
		return {
			id,
			username: user.username,
			discriminator: user.discriminator,
			global_name: user.globalName,
			bio: user.bio,
			pronouns: user.pronouns,
			avatar: await this.cloneProfileAssetForReport(reportId, 'user_avatar', 'avatars', id, user.avatarHash),
			banner: await this.cloneProfileAssetForReport(reportId, 'user_banner', 'banners', id, user.bannerHash),
		};
	}

	private async snapshotMemberProfile(reportId: ReportID, member: GuildMember): Promise<ReportProfileMemberSnapshot> {
		const assetBase = `guilds/${member.guildId}/users/${member.userId}`;
		return {
			guild_id: member.guildId.toString(),
			nick: member.nickname,
			bio: member.bio,
			pronouns: member.pronouns,
			joined_at: member.joinedAt.toISOString(),
			avatar: await this.cloneProfileAssetForReport(reportId, 'member_avatar', assetBase, 'avatars', member.avatarHash),
			banner: await this.cloneProfileAssetForReport(reportId, 'member_banner', assetBase, 'banners', member.bannerHash),
		};
	}

	private async snapshotGuildProfile(reportId: ReportID, guild: Guild): Promise<ReportProfileGuildSnapshot> {
		const id = guild.id.toString();
		return {
			id,
			name: guild.name,
			vanity_url_code: guild.vanityUrlCode,
			icon: await this.cloneProfileAssetForReport(reportId, 'guild_icon', 'icons', id, guild.iconHash),
			banner: await this.cloneProfileAssetForReport(reportId, 'guild_banner', 'banners', id, guild.bannerHash),
			splash: await this.cloneProfileAssetForReport(reportId, 'guild_splash', 'splashes', id, guild.splashHash),
		};
	}

	private async cloneProfileAssetForReport(
		reportId: ReportID,
		kind: string,
		sourcePrefix: string,
		sourceEntity: string,
		hash: string | null,
	): Promise<ReportProfileAssetSnapshot | null> {
		if (!hash) {
			return null;
		}
		const key = `reports/${reportId}/profile/${kind}/${hash}`;
		try {
			await this.storageService.copyObject({
				sourceBucket: Config.s3.buckets.cdn,
				sourceKey: buildHashedAssetKey(sourcePrefix, sourceEntity, hash),
				destinationBucket: Config.s3.buckets.reports,
				destinationKey: key,
			});
			return {hash, key};
		} catch (error) {
			Logger.error({error, reportId, kind}, 'Failed to clone profile asset for report');
			return {hash, key: null};
		}
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
