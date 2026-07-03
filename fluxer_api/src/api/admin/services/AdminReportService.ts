// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import type {SearchReportsRequest} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {getEmailTemplate} from '@pkgs/email/src/email_i18n/EmailI18n';
import {seconds} from 'itty-time';
import type {ApiContext} from '../../ApiContext';
import {
	type ChannelID,
	createReportID,
	createUserID,
	type GuildID,
	type ReportID,
	type UserID,
} from '../../BrandedTypes';
import {Config} from '../../Config';
import type {IChannelRepository} from '../../channel/IChannelRepository';
import type {ChannelService} from '../../channel/services/ChannelService';
import {makeAttachmentCdnKey} from '../../channel/services/message/MessageHelpers';
import {
	createMessageResponseDataService,
	type MessageResponseAccessContext,
	messageResponseAccessForChannel,
	messageResponseAccessForGuild,
} from '../../channel/services/message/MessageResponseDataService';
import {SYSTEM_USER_ID} from '../../constants/Core';
import type {NcmecAttachmentStatusResponse, NcmecSubmissionService} from '../../csam/NcmecSubmissionService';
import type {MessageAttachment} from '../../database/types/MessageTypes';
import type {IGuildRepositoryAggregate} from '../../guild/repositories/IGuildRepositoryAggregate';
import type {IStorageService} from '../../infrastructure/IStorageService';
import type {UserCacheService} from '../../infrastructure/UserCacheService';
import {Logger} from '../../Logger';
import type {RequestCache} from '../../middleware/RequestCacheMiddleware';
import {createRequestCache} from '../../middleware/RequestCacheMiddleware';
import type {User} from '../../models/User';
import type {IARMessageContext, IARSubmission} from '../../report/IReportRepository';
import type {ReportService} from '../../report/ReportService';
import {getReportSearchService} from '../../SearchFactory';
import type {UserChannelService} from '../../user/services/UserChannelService';
import {assertSafeByteSize} from '../../utils/ByteSizeUtils';
import type {AdminAuditService} from './AdminAuditService';

interface AdminReportServiceDeps {
	apiContext: ApiContext;
	reportService: ReportService;
	guildRepository: IGuildRepositoryAggregate;
	channelRepository: IChannelRepository;
	channelService: ChannelService;
	storageService: IStorageService;
	auditService: AdminAuditService;
	userCacheService: UserCacheService;
	userChannelService: UserChannelService;
	ncmecSubmissionService: NcmecSubmissionService;
}

interface ReportNsfwLookupCache {
	channelNsfwByChannelId: Map<string, boolean | null>;
	guildNsfwLevelByGuildId: Map<string, number | null>;
}

function createReportNsfwLookupCache(): ReportNsfwLookupCache {
	return {
		channelNsfwByChannelId: new Map(),
		guildNsfwLevelByGuildId: new Map(),
	};
}

export class AdminReportService {
	constructor(private readonly deps: AdminReportServiceDeps) {}

	async listReports(status: number, acls: ReadonlySet<string>, limit?: number, offset?: number) {
		const {reportService} = this.deps;
		const requestedLimit = limit || 50;
		const currentOffset = offset || 0;
		const reports = await reportService.listReportsByStatus(status, requestedLimit, currentOffset);
		const requestCache = createRequestCache();
		const reportNsfwLookupCache = createReportNsfwLookupCache();
		const reportResponses = await Promise.all(
			reports.map((report: IARSubmission) =>
				this.mapReportToResponse(report, false, requestCache, acls, reportNsfwLookupCache),
			),
		);
		return {
			reports: reportResponses,
		};
	}

	async getReport(reportId: ReportID, acls: ReadonlySet<string>) {
		const {reportService} = this.deps;
		const report = await reportService.getReport(reportId);
		const requestCache = createRequestCache();
		const reportNsfwLookupCache = createReportNsfwLookupCache();
		return this.mapReportToResponse(report, true, requestCache, acls, reportNsfwLookupCache);
	}

	async resolveReport(
		reportId: ReportID,
		adminUserId: UserID,
		publicComment: string | null,
		auditLogReason: string | null,
	) {
		const {reportService, auditService} = this.deps;
		const {users: userRepository, email: emailService} = this.deps.apiContext.services;
		const resolvedReport = await reportService.resolveReport(reportId, adminUserId, publicComment, auditLogReason);
		await auditService.createAuditLog({
			adminUserId,
			targetType: 'report',
			targetId: BigInt(reportId),
			action: 'resolve_report',
			auditLogReason,
			metadata: new Map([
				['report_id', reportId.toString()],
				['report_type', resolvedReport.reportType.toString()],
			]),
		});
		if (resolvedReport.reporterId) {
			const reporter = await userRepository.findUnique(resolvedReport.reporterId);
			if (reporter) {
				const commentForTemplate = publicComment ?? '';
				await this.sendResolvedReportSystemDm({
					reporter,
					reportId,
					publicComment: commentForTemplate,
				});
				if (reporter.email) {
					await emailService.sendReportResolvedEmail(
						reporter.email,
						reporter.username,
						reportId.toString(),
						commentForTemplate,
						reporter.locale,
					);
				}
			}
		}
		return {
			report_id: resolvedReport.reportId.toString(),
			status: resolvedReport.status,
			resolved_at: resolvedReport.resolvedAt?.toISOString() ?? null,
			public_comment: resolvedReport.publicComment,
		};
	}

	private async sendResolvedReportSystemDm({
		reporter,
		reportId,
		publicComment,
	}: {
		reporter: User;
		reportId: ReportID;
		publicComment: string;
	}): Promise<void> {
		const {users: userRepository} = this.deps.apiContext.services;
		const systemUser = await userRepository.findUnique(SYSTEM_USER_ID);
		if (!systemUser) {
			Logger.warn(
				{reportId: reportId.toString(), reporterId: reporter.id.toString()},
				'Skipping report review system DM because system user does not exist',
			);
			return;
		}
		const template = getEmailTemplate('report_resolved', reporter.locale, {
			username: reporter.username,
			reportId: reportId.toString(),
			publicComment,
			hasComment: publicComment ? 'yes' : 'no',
		});
		if (!template.ok) {
			Logger.warn(
				{
					reportId: reportId.toString(),
					reporterId: reporter.id.toString(),
					locale: reporter.locale,
					error: template.error,
				},
				'Skipping report review system DM because the email template could not be resolved',
			);
			return;
		}
		const requestCache = createRequestCache();
		try {
			const dmChannel = await this.deps.userChannelService.ensureDmOpenForBothUsers({
				userId: systemUser.id,
				recipientId: reporter.id,
				userCacheService: this.deps.userCacheService,
				requestCache,
			});
			await this.deps.channelService.messages.send.sendMessage({
				user: systemUser,
				channelId: dmChannel.id,
				data: {
					content: template.value.body,
				},
				requestCache,
			});
		} catch (error) {
			Logger.warn(
				{reportId: reportId.toString(), reporterId: reporter.id.toString(), error},
				'Failed to send report review system DM',
			);
		} finally {
			requestCache.clear();
		}
	}

	async searchReports(data: SearchReportsRequest, acls: ReadonlySet<string>) {
		const reportSearchService = getReportSearchService();
		if (!reportSearchService) {
			throw new Error('Search is not enabled');
		}
		const filters: Record<string, string | number> = {};
		if (data.reporter_id !== undefined) {
			filters['reporterId'] = data.reporter_id.toString();
		}
		if (data.status !== undefined) {
			filters['status'] = data.status;
		}
		if (data.report_type !== undefined) {
			filters['reportType'] = data.report_type;
		}
		if (data.category !== undefined) {
			filters['category'] = data.category;
		}
		if (data.reported_user_id !== undefined) {
			filters['reportedUserId'] = data.reported_user_id.toString();
		}
		if (data.reported_guild_id !== undefined) {
			filters['reportedGuildId'] = data.reported_guild_id.toString();
		}
		if (data.reported_channel_id !== undefined) {
			filters['reportedChannelId'] = data.reported_channel_id.toString();
		}
		if (data.guild_context_id !== undefined) {
			filters['guildContextId'] = data.guild_context_id.toString();
		}
		if (data.resolved_by_admin_id !== undefined) {
			filters['resolvedByAdminId'] = data.resolved_by_admin_id.toString();
		}
		if (data.sort_by) {
			filters['sortBy'] = data.sort_by;
		}
		if (data.sort_order) {
			filters['sortOrder'] = data.sort_order;
		}
		const {hits, total} = await reportSearchService.searchReports(data.query || '', filters, {
			limit: data.limit,
			offset: data.offset,
		});
		const requestCache = createRequestCache();
		const reportNsfwLookupCache = createReportNsfwLookupCache();
		const orderedReports = await this.loadReportsInSearchOrder(hits.map((hit) => createReportID(BigInt(hit.id))));
		const reports = await Promise.all(
			orderedReports.map((report) =>
				this.mapReportToResponse(report, false, requestCache, acls, reportNsfwLookupCache),
			),
		);
		const missingCount = hits.length - orderedReports.length;
		return {
			reports,
			total: Math.max(0, total - missingCount),
			offset: data.offset,
			limit: data.limit,
		};
	}

	private async loadReportsInSearchOrder(reportIds: Array<ReportID>): Promise<Array<IARSubmission>> {
		const reports = await Promise.all(
			reportIds.map(async (reportId) => {
				try {
					return await this.deps.reportService.getReport(reportId);
				} catch (_error) {
					return null;
				}
			}),
		);
		return reports.filter((report): report is IARSubmission => report !== null);
	}

	private async mapReportToResponse(
		report: IARSubmission,
		includeContext: boolean,
		requestCache: RequestCache,
		acls: ReadonlySet<string>,
		reportNsfwLookupCache: ReportNsfwLookupCache,
	) {
		const reporterInfo = await this.buildUserTag(report.reporterId, requestCache);
		const reportedUserInfo = await this.buildUserTag(report.reportedUserId, requestCache);
		const canViewReporterPii = acls.has(AdminACLs.REPORT_VIEW_REPORTER_PII) || acls.has(AdminACLs.WILDCARD);
		const reportedGuildNsfwLevel =
			report.reportedGuildNsfw !== null
				? report.reportedGuildNsfw
					? 3
					: 0
				: await this.getGuildNsfwLevelForContext(
						report.reportedChannelId,
						report.reportedGuildId ?? report.guildContextId ?? null,
						reportNsfwLookupCache,
					);
		const reportedChannelNsfw =
			report.reportedChannelEffectiveNsfw !== null
				? report.reportedChannelEffectiveNsfw
				: await this.getChannelNsfwState(report.reportedChannelId, reportNsfwLookupCache);
		const baseResponse = {
			report_id: report.reportId.toString(),
			reporter_id: report.reporterId?.toString() ?? null,
			reporter_tag: reporterInfo?.tag ?? null,
			reporter_username: reporterInfo?.username ?? null,
			reporter_global_name: reporterInfo?.global_name ?? null,
			reporter_discriminator: reporterInfo?.discriminator ?? null,
			reporter_email: canViewReporterPii ? report.reporterEmail : null,
			reporter_full_legal_name: canViewReporterPii ? report.reporterFullLegalName : null,
			reporter_country_of_residence: canViewReporterPii ? report.reporterCountryOfResidence : null,
			reported_at: report.reportedAt.toISOString(),
			status: report.status,
			report_type: report.reportType,
			category: report.category,
			additional_info: report.additionalInfo,
			reported_user_id: report.reportedUserId?.toString() ?? null,
			reported_user_tag: reportedUserInfo?.tag ?? null,
			reported_user_username: reportedUserInfo?.username ?? null,
			reported_user_global_name: reportedUserInfo?.global_name ?? null,
			reported_user_discriminator: reportedUserInfo?.discriminator ?? null,
			reported_user_avatar_hash: report.reportedUserAvatarHash,
			reported_guild_id: report.reportedGuildId?.toString() ?? null,
			reported_guild_name: report.reportedGuildName,
			reported_guild_icon_hash: report.reportedGuildIconHash,
			reported_message_id: report.reportedMessageId?.toString() ?? null,
			reported_channel_id: report.reportedChannelId?.toString() ?? null,
			reported_channel_name: report.reportedChannelName,
			reported_channel_nsfw: reportedChannelNsfw,
			reported_guild_invite_code: report.reportedGuildInviteCode,
			reported_guild_nsfw_level: reportedGuildNsfwLevel,
			reported_guild_nsfw: report.reportedGuildNsfw,
			reported_guild_content_warning_level: report.reportedGuildContentWarningLevel,
			reported_guild_content_warning_text: report.reportedGuildContentWarningText,
			reported_channel_nsfw_override: report.reportedChannelNsfwOverride,
			reported_channel_content_warning_level: report.reportedChannelContentWarningLevel,
			reported_channel_content_warning_text: report.reportedChannelContentWarningText,
			reported_channel_effective_nsfw: report.reportedChannelEffectiveNsfw,
			reported_channel_effective_content_warning_level: report.reportedChannelEffectiveContentWarningLevel,
			reported_channel_effective_content_warning_text: report.reportedChannelEffectiveContentWarningText,
			resolved_at: report.resolvedAt?.toISOString() ?? null,
			resolved_by_admin_id: report.resolvedByAdminId?.toString() ?? null,
			public_comment: report.publicComment,
		};
		if (!includeContext) {
			return baseResponse;
		}
		const attachmentStatusesById = await this.getAttachmentStatusesById(report);
		const priorReportsByAuthor = await this.getPriorReportsForContext(report);
		const messageContext =
			report.messageContext && report.messageContext.length > 0
				? await Promise.all(
						report.messageContext.map((message) =>
							this.mapReportMessageContextToResponse(
								message,
								report.reportedChannelId ?? null,
								report.reportedGuildId ?? report.guildContextId ?? null,
								reportNsfwLookupCache,
								attachmentStatusesById,
								priorReportsByAuthor,
							),
						),
					)
				: [];
		const messageResponses = await this.getLiveMessageResponsesForContext(report);
		const mutualDmChannelId = await this.getMutualDmChannelId(report);
		return {
			...baseResponse,
			mutual_dm_channel_id: mutualDmChannelId,
			message_context: messageContext,
			message_responses: messageResponses,
		};
	}

	private async getLiveMessageResponsesForContext(report: IARSubmission): Promise<Array<MessageResponse>> {
		const contexts = report.messageContext ?? [];
		if (contexts.length === 0) return [];
		const responses = await Promise.all(
			contexts.map(async (message) => {
				const channelId = message.channelId ?? report.reportedChannelId;
				if (!channelId) return null;
				try {
					const access = await this.getMessageResponseAccessForAdmin(channelId);
					return await createMessageResponseDataService().getMessage({
						userId: createUserID(0n),
						channelId,
						messageId: message.messageId,
						access,
					});
				} catch (error) {
					Logger.warn(
						{
							error,
							reportId: report.reportId.toString(),
							channelId: channelId.toString(),
							messageId: message.messageId.toString(),
						},
						'Failed to resolve live admin report message response',
					);
					return null;
				}
			}),
		);
		return responses.filter((message): message is MessageResponse => message !== null);
	}

	private async getMessageResponseAccessForAdmin(channelId: ChannelID): Promise<MessageResponseAccessContext> {
		const channel = await this.deps.channelRepository.findUnique(channelId);
		return channel ? messageResponseAccessForChannel(channel) : messageResponseAccessForGuild(null);
	}

	private async getMutualDmChannelId(report: IARSubmission): Promise<string | null> {
		if (report.reportType !== 1 || !report.reporterId || !report.reportedUserId) {
			return null;
		}
		const {users: userRepository} = this.deps.apiContext.services;
		const mutualDmChannel = await userRepository.findExistingDmState(report.reporterId, report.reportedUserId);
		return mutualDmChannel ? mutualDmChannel.id.toString() : null;
	}

	private async mapReportMessageContextToResponse(
		message: IARMessageContext,
		fallbackChannelId: ChannelID | null,
		fallbackGuildId: GuildID | null,
		reportNsfwLookupCache: ReportNsfwLookupCache,
		attachmentStatusesById: Map<string, NcmecAttachmentStatusResponse>,
		priorReportsByAuthor: Map<string, Array<string>>,
	) {
		const channelId = message.channelId ?? fallbackChannelId;
		const channelNsfw = await this.getChannelNsfwState(channelId, reportNsfwLookupCache);
		const guildNsfwLevel = await this.getGuildNsfwLevelForContext(channelId, fallbackGuildId, reportNsfwLookupCache);
		const attachments =
			message.attachments && message.attachments.length > 0
				? (
						await Promise.all(
							message.attachments.map((attachment) =>
								this.mapReportAttachmentToResponse(attachment, channelId, attachmentStatusesById),
							),
						)
					).filter(
						(
							attachment,
						): attachment is {
							id: string;
							filename: string;
							url: string;
							nsfw: boolean | null;
							content_type: string | null;
							width: number | null;
							height: number | null;
							size: number | null;
							ncmec_status: string;
							ncmec_report_id: string | null;
							ncmec_failure_reason: string | null;
						} => attachment !== null,
					)
				: [];
		return {
			id: message.messageId.toString(),
			channel_id: channelId ? channelId.toString() : '',
			channel_nsfw: channelNsfw,
			channel_content_warning_level: null,
			channel_content_warning_text: null,
			guild_id: fallbackGuildId ? fallbackGuildId.toString() : null,
			guild_nsfw_level: guildNsfwLevel,
			guild_nsfw: null,
			guild_content_warning_level: null,
			guild_content_warning_text: null,
			content: message.content ?? '',
			timestamp: message.timestamp.toISOString(),
			attachments,
			author_id: message.authorId.toString(),
			author_username: message.authorUsername,
			author_global_name: null,
			author_discriminator: message.authorDiscriminator.toString().padStart(4, '0'),
			author_avatar: message.authorAvatarHash,
			user_prior_ncmec_report_ids: priorReportsByAuthor.get(message.authorId.toString()) ?? [],
		};
	}

	private async getPriorReportsForContext(report: IARSubmission): Promise<Map<string, Array<string>>> {
		const authorIds = new Set<string>();
		for (const message of report.messageContext ?? []) {
			authorIds.add(message.authorId.toString());
		}
		if (report.reportedUserId) authorIds.add(report.reportedUserId.toString());
		if (authorIds.size === 0) return new Map();
		const userIds = [...authorIds].map((value) => createUserID(BigInt(value)));
		return this.deps.ncmecSubmissionService.getUserPriorReportIds(userIds);
	}

	private async getChannelNsfwState(
		channelId: ChannelID | null,
		reportNsfwLookupCache: ReportNsfwLookupCache,
	): Promise<boolean | null> {
		if (!channelId) {
			return null;
		}
		const channelIdString = channelId.toString();
		if (reportNsfwLookupCache.channelNsfwByChannelId.has(channelIdString)) {
			return reportNsfwLookupCache.channelNsfwByChannelId.get(channelIdString) ?? null;
		}
		const channel = await this.deps.channelRepository.findUnique(channelId);
		const channelNsfw = channel?.isNsfw ?? null;
		reportNsfwLookupCache.channelNsfwByChannelId.set(channelIdString, channelNsfw);
		return channelNsfw;
	}

	private async getGuildNsfwLevel(
		guildId: GuildID | null,
		reportNsfwLookupCache: ReportNsfwLookupCache,
	): Promise<number | null> {
		if (!guildId) {
			return null;
		}
		const guildIdString = guildId.toString();
		if (reportNsfwLookupCache.guildNsfwLevelByGuildId.has(guildIdString)) {
			return reportNsfwLookupCache.guildNsfwLevelByGuildId.get(guildIdString) ?? null;
		}
		const guild = await this.deps.guildRepository.findUnique(guildId);
		const guildNsfwLevel = guild?.nsfwLevel ?? null;
		reportNsfwLookupCache.guildNsfwLevelByGuildId.set(guildIdString, guildNsfwLevel);
		return guildNsfwLevel;
	}

	private async getGuildNsfwLevelForContext(
		channelId: ChannelID | null,
		fallbackGuildId: GuildID | null,
		reportNsfwLookupCache: ReportNsfwLookupCache,
	): Promise<number | null> {
		if (fallbackGuildId) {
			return this.getGuildNsfwLevel(fallbackGuildId, reportNsfwLookupCache);
		}
		if (!channelId) {
			return null;
		}
		const channel = await this.deps.channelRepository.findUnique(channelId);
		if (!channel?.guildId) {
			return null;
		}
		return this.getGuildNsfwLevel(channel.guildId, reportNsfwLookupCache);
	}

	private async mapReportAttachmentToResponse(
		attachment: MessageAttachment,
		channelId: ChannelID | null,
		attachmentStatusesById: Map<string, NcmecAttachmentStatusResponse>,
	): Promise<{
		id: string;
		filename: string;
		url: string;
		nsfw: boolean | null;
		content_type: string | null;
		width: number | null;
		height: number | null;
		size: number | null;
		ncmec_status: string;
		ncmec_report_id: string | null;
		ncmec_failure_reason: string | null;
	} | null> {
		if (!attachment || attachment.attachment_id == null || !attachment.filename || !channelId) {
			return null;
		}
		const {storageService} = this.deps;
		const attachmentId = attachment.attachment_id;
		const filename = String(attachment.filename);
		const key = makeAttachmentCdnKey(channelId, attachmentId, filename);
		try {
			const url = await storageService.getPresignedDownloadURL({
				bucket: Config.s3.buckets.reports,
				key,
				expiresIn: seconds('5 minutes'),
			});
			return {
				id: attachment.attachment_id.toString(),
				filename,
				url,
				nsfw: attachment.nsfw ?? null,
				content_type: attachment.content_type ?? null,
				width: attachment.width ?? null,
				height: attachment.height ?? null,
				size: attachment.size != null ? assertSafeByteSize(attachment.size, 'admin report attachment size') : null,
				ncmec_status: attachmentStatusesById.get(attachment.attachment_id.toString())?.status ?? 'not_submitted',
				ncmec_report_id: attachmentStatusesById.get(attachment.attachment_id.toString())?.ncmec_report_id ?? null,
				ncmec_failure_reason: attachmentStatusesById.get(attachment.attachment_id.toString())?.failure_reason ?? null,
			};
		} catch (error) {
			Logger.error(
				{error, attachmentId, filename, channelId},
				'Failed to generate presigned URL for report attachment',
			);
		}
		return null;
	}

	private async getAttachmentStatusesById(report: IARSubmission) {
		const attachmentIds = (report.messageContext ?? []).flatMap((message) =>
			message.attachments.map((attachment) => attachment.attachment_id),
		);
		return this.deps.ncmecSubmissionService.getAttachmentStatuses(attachmentIds);
	}

	private async buildUserTag(userId: UserID | null, requestCache: RequestCache): Promise<UserTagInfo | null> {
		if (!userId) {
			return null;
		}
		try {
			const user = await this.deps.userCacheService.getUserPartialResponse(userId, requestCache);
			const discriminator = user.discriminator?.padStart(4, '0') ?? '0000';
			return {
				tag: `${user.username}#${discriminator}`,
				username: user.username,
				global_name: user.global_name ?? null,
				discriminator,
			};
		} catch (error) {
			Logger.warn({userId: userId.toString(), error}, 'Failed to resolve user tag for report');
			return null;
		}
	}
}

interface UserTagInfo {
	tag: string;
	username: string;
	global_name: string | null;
	discriminator: string;
}
