// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import type {AdminAuditService} from '@app/api/admin/services/AdminAuditService';
import {
	describeReporterNotice,
	type ReporterResolutionNotifier,
} from '@app/api/admin/services/ReporterResolutionNotifier';
import {
	type ChannelID,
	createReportID,
	createUserID,
	type GuildID,
	type ReportID,
	type UserID,
} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import type {IChannelRepository} from '@app/api/channel/IChannelRepository';
import {makeAttachmentCdnKey} from '@app/api/channel/services/message/MessageHelpers';
import {
	createMessageResponseDataService,
	type MessageResponseAccessContext,
	messageResponseAccessForChannel,
	messageResponseAccessForGuild,
} from '@app/api/channel/services/message/MessageResponseDataService';
import {resolveNsfwScopeChannel} from '@app/api/channel/utils/ThreadNsfwScope';
import type {MessageAttachment} from '@app/api/database/types/MessageTypes';
import type {IGuildRepositoryAggregate} from '@app/api/guild/repositories/IGuildRepositoryAggregate';
import type {IStorageService} from '@app/api/infrastructure/IStorageService';
import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import {Logger} from '@app/api/Logger';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import {createRequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import {describeReportFlowAnswers} from '@app/api/report/flows/ReportFlowRegistry';
import {findReportReason, listReportReasons} from '@app/api/report/flows/ReportReasonCatalog';
import type {IARMessageContext, IARSubmission} from '@app/api/report/IReportRepository';
import type {ReportService} from '@app/api/report/ReportService';
import {getReportSearchService} from '@app/api/SearchFactory';
import {isHiddenPartial} from '@app/api/user/ProfileVisibility';
import {formatUserTag} from '@app/api/user/UserTag';
import {assertSafeByteSize} from '@app/api/utils/ByteSizeUtils';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {DELETED_USER_DISCRIMINATOR, DELETED_USER_USERNAME} from '@fluxer/constants/src/UserConstants';
import {FeatureTemporarilyDisabledError} from '@fluxer/errors/src/domains/core/FeatureTemporarilyDisabledError';
import {UnknownReportError} from '@fluxer/errors/src/domains/moderation/UnknownReportError';
import type {ReportSearchFilters} from '@fluxer/schema/src/contracts/search/SearchDocumentTypes';
import type {
	AdminReportReasonsResponse,
	SearchReportsRequest,
	UpdateReportRequest,
} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import type {ReportProfileAssetSnapshot} from '@fluxer/schema/src/domains/report/ReportProfileSnapshotSchemas';
import type {UserPartialResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {seconds} from 'itty-time';

interface AdminReportServiceDeps {
	apiContext: ApiContext;
	reportService: ReportService;
	guildRepository: IGuildRepositoryAggregate;
	channelRepository: IChannelRepository;
	storageService: IStorageService;
	auditService: AdminAuditService;
	userCacheService: UserCacheService;
	reporterResolutionNotifier: ReporterResolutionNotifier;
}

type StaffReportResolution = NonNullable<UpdateReportRequest['resolution']>;

interface ReportLookupCache {
	channelNsfwByChannelId: Map<string, boolean | null>;
	guildNsfwLevelByGuildId: Map<string, number | null>;
	reporterEmailByReporterId: Map<string, Promise<string | null>>;
}

function createReportLookupCache(): ReportLookupCache {
	return {
		channelNsfwByChannelId: new Map(),
		guildNsfwLevelByGuildId: new Map(),
		reporterEmailByReporterId: new Map(),
	};
}

function mapUserPartialBotFlag(user: UserPartialResponse): boolean | null {
	if (user.bot) {
		return true;
	}
	const isUnresolved =
		user.username === DELETED_USER_USERNAME &&
		user.discriminator === DELETED_USER_DISCRIMINATOR.toString().padStart(4, '0');
	return isUnresolved ? null : false;
}

function mapMissingAttachmentToResponse(attachment: MessageAttachment) {
	return {
		id: attachment.attachment_id.toString(),
		filename: String(attachment.filename),
		nsfw: attachment.nsfw ?? null,
		content_type: attachment.content_type ?? null,
		width: attachment.width ?? null,
		height: attachment.height ?? null,
		size: attachment.size != null ? assertSafeByteSize(attachment.size, 'admin report attachment size') : null,
	};
}

function mapReportReasonFields(report: IARSubmission) {
	const reason = report.reason ? findReportReason(report.reason) : null;
	const flow =
		report.flowSteps && report.flowRevision && report.flowSurface
			? describeReportFlowAnswers({
					revisionHash: report.flowRevision,
					surface: report.flowSurface,
					locale: report.flowLocale,
					steps: report.flowSteps,
				})
			: null;
	return {
		reason: report.reason,
		reason_label: reason?.label ?? report.reason,
		reason_highest_priority: reason?.highestPriority ?? null,
		flow,
		reporter_good_faith_confirmed: report.reporterGoodFaithConfirmed,
	};
}

export class AdminReportService {
	constructor(private readonly deps: AdminReportServiceDeps) {}

	async listReports(status: number, acls: ReadonlySet<string>, limit?: number, offset?: number) {
		const {reportService} = this.deps;
		const requestedLimit = limit || 50;
		const currentOffset = offset || 0;
		const {reports, total} = await reportService.listReportsByStatus(status, requestedLimit, currentOffset);
		const requestCache = createRequestCache();
		const reportLookupCache = createReportLookupCache();
		const reportResponses = await Promise.all(
			reports.map((report: IARSubmission) =>
				this.mapReportToResponse(report, false, requestCache, acls, reportLookupCache),
			),
		);
		return {
			reports: reportResponses,
			total,
			offset: currentOffset,
			limit: requestedLimit,
		};
	}

	listReportReasons(): AdminReportReasonsResponse {
		return {
			reasons: listReportReasons().map((reason) => ({
				key: reason.key,
				label: reason.label,
				highest_priority: reason.highestPriority,
				legacy_category_message: reason.legacyCategories.message,
				legacy_category_user: reason.legacyCategories.user,
				legacy_category_guild: reason.legacyCategories.guild,
			})),
		};
	}

	async getReport(reportId: ReportID, acls: ReadonlySet<string>) {
		const {reportService} = this.deps;
		const report = await reportService.getReport(reportId);
		const requestCache = createRequestCache();
		const reportLookupCache = createReportLookupCache();
		return this.mapReportToResponse(report, true, requestCache, acls, reportLookupCache);
	}

	async resolveReport(
		reportId: ReportID,
		adminUserId: UserID,
		publicComment: string | null,
		auditLogReason: string | null,
		notifyReporter: boolean,
		resolution?: StaffReportResolution,
	) {
		const {reportService, auditService, reporterResolutionNotifier} = this.deps;
		const sentComment = notifyReporter ? publicComment : null;
		const internalComment = notifyReporter ? null : publicComment;
		const resolvedReport = await reportService.resolveReport(reportId, adminUserId, sentComment, auditLogReason, {
			outcome: resolution,
			resolvedBy: 'staff',
		});
		const notice = await reporterResolutionNotifier.notifyReporterOfResolution(resolvedReport, sentComment);
		await auditService.createAuditLog({
			adminUserId,
			targetType: 'report',
			targetId: BigInt(reportId),
			action: 'resolve_report',
			auditLogReason,
			metadata: new Map([
				['report_id', reportId.toString()],
				['report_type', resolvedReport.reportType.toString()],
				['notify_reporter', notifyReporter ? 'true' : 'false'],
				...(internalComment ? [['internal_comment', internalComment] as [string, string]] : []),
				...describeReporterNotice(notice),
				...(resolution ? [['resolution', resolution] as [string, string]] : []),
			]),
		});
		return {
			report_id: resolvedReport.reportId.toString(),
			status: resolvedReport.status,
			resolved_at: resolvedReport.resolvedAt?.toISOString() ?? null,
			public_comment: resolvedReport.publicComment,
		};
	}

	async searchReports(data: SearchReportsRequest, acls: ReadonlySet<string>) {
		const reportSearchService = getReportSearchService();
		if (!reportSearchService) {
			throw new FeatureTemporarilyDisabledError();
		}
		const filters: ReportSearchFilters = {};
		if (data.reporter_id !== undefined) {
			filters.reporterId = data.reporter_id.toString();
		}
		if (data.status !== undefined) {
			filters.status = data.status;
		}
		if (data.report_type !== undefined) {
			filters.reportType = data.report_type;
		}
		if (data.category !== undefined) {
			filters.category = data.category;
		}
		if (data.reason !== undefined) {
			filters.reason = data.reason;
		}
		if (data.reported_user_id !== undefined) {
			filters.reportedUserId = data.reported_user_id.toString();
		}
		if (data.reported_webhook_id !== undefined) {
			filters.reportedWebhookId = data.reported_webhook_id.toString();
		}
		if (data.reported_guild_id !== undefined) {
			filters.reportedGuildId = data.reported_guild_id.toString();
		}
		if (data.reported_channel_id !== undefined) {
			filters.reportedChannelId = data.reported_channel_id.toString();
		}
		if (data.guild_context_id !== undefined) {
			filters.guildContextId = data.guild_context_id.toString();
		}
		if (data.resolved_by_admin_id !== undefined) {
			filters.resolvedByAdminId = data.resolved_by_admin_id.toString();
		}
		if (data.sort_by) {
			filters.sortBy = data.sort_by;
		}
		if (data.sort_order) {
			filters.sortOrder = data.sort_order;
		}
		const {hits, total} = await reportSearchService.searchReports(data.query || '', filters, {
			limit: data.limit,
			offset: data.offset,
		});
		const requestCache = createRequestCache();
		const reportLookupCache = createReportLookupCache();
		const reportIds = hits.map((hit) => createReportID(BigInt(hit.id)));
		const loaded = await this.loadReportsInSearchOrder(reportIds);
		const orderedReports = loaded.filter((report): report is IARSubmission => report !== null);
		const orphanedReportIds = reportIds.filter((_, index) => loaded[index] === null).map((id) => id.toString());
		if (orphanedReportIds.length > 0) {
			Logger.warn(
				{orphanedReportIds},
				'Report search index lists reports that are no longer stored, run refresh_search_index reports',
			);
		}
		const reports = await Promise.all(
			orderedReports.map((report) => this.mapReportToResponse(report, false, requestCache, acls, reportLookupCache)),
		);
		return {
			reports,
			total,
			offset: data.offset,
			limit: data.limit,
		};
	}

	private loadReportsInSearchOrder(reportIds: Array<ReportID>): Promise<Array<IARSubmission | null>> {
		return Promise.all(
			reportIds.map(async (reportId) => {
				try {
					return await this.deps.reportService.getReport(reportId);
				} catch (error) {
					if (error instanceof UnknownReportError) {
						return null;
					}
					throw error;
				}
			}),
		);
	}

	private async mapReportToResponse(
		report: IARSubmission,
		includeContext: boolean,
		requestCache: RequestCache,
		acls: ReadonlySet<string>,
		reportLookupCache: ReportLookupCache,
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
						reportLookupCache,
					);
		const reportedChannelNsfw =
			report.reportedChannelEffectiveNsfw !== null
				? report.reportedChannelEffectiveNsfw
				: await this.getChannelNsfwState(report.reportedChannelId, reportLookupCache);
		const baseResponse = {
			report_id: report.reportId.toString(),
			reporter_id: report.reporterId?.toString() ?? null,
			reporter_tag: reporterInfo?.tag ?? null,
			reporter_username: reporterInfo?.username ?? null,
			reporter_global_name: reporterInfo?.global_name ?? null,
			reporter_discriminator: reporterInfo?.discriminator ?? null,
			reporter_email: canViewReporterPii ? await this.getReporterEmail(report, reportLookupCache) : null,
			reporter_full_legal_name: canViewReporterPii ? report.reporterFullLegalName : null,
			reporter_country_of_residence: canViewReporterPii ? report.reporterCountryOfResidence : null,
			reported_at: report.reportedAt.toISOString(),
			status: report.status,
			report_type: report.reportType,
			category: report.category,
			...mapReportReasonFields(report),
			additional_info: report.additionalInfo,
			reported_user_id: report.reportedUserId?.toString() ?? null,
			reported_user_tag: reportedUserInfo?.tag ?? null,
			reported_user_username: reportedUserInfo?.username ?? null,
			reported_user_global_name: reportedUserInfo?.global_name ?? null,
			reported_user_discriminator: reportedUserInfo?.discriminator ?? null,
			reported_user_avatar_hash: report.reportedUserAvatarHash,
			reported_user_bot: reportedUserInfo?.bot ?? null,
			reported_webhook_id: report.reportedWebhookId?.toString() ?? null,
			reported_webhook_name: report.reportedWebhookName,
			reported_webhook_avatar_hash: report.reportedWebhookAvatarHash,
			reported_webhook_default_name: report.reportedWebhookDefaultName,
			reported_webhook_default_avatar_hash: report.reportedWebhookDefaultAvatarHash,
			reported_webhook_type: report.reportedWebhookType,
			reported_webhook_application_id: report.reportedWebhookApplicationId?.toString() ?? null,
			reported_webhook_channel_id: report.reportedWebhookChannelId?.toString() ?? null,
			reported_webhook_guild_id: report.reportedWebhookGuildId?.toString() ?? null,
			reported_webhook_created_at: report.reportedWebhookCreatedAt?.toISOString() ?? null,
			...mapWebhookCreatorFields(report),
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
		const authorBotFlags = await this.getContextAuthorBotFlags(report, requestCache);
		const messageContext = (
			await Promise.all(
				(report.messageContext ?? []).map((message) =>
					this.mapReportMessageContextToResponse(
						message,
						report.reportedChannelId ?? null,
						report.reportedGuildId ?? report.guildContextId ?? null,
						reportLookupCache,
						authorBotFlags,
					),
				),
			)
		).filter((message) => message !== null);
		const messageResponses = await this.getLiveMessageResponsesForContext(report);
		const mutualDmChannelId = await this.getMutualDmChannelId(report);
		return {
			...baseResponse,
			mutual_dm_channel_id: mutualDmChannelId,
			message_context: messageContext,
			message_responses: messageResponses,
			reported_profile_snapshot: await this.mapProfileSnapshotToResponse(report),
			legal_hold_until: report.legalHoldUntil?.toISOString() ?? null,
			legal_hold_reason: report.legalHoldReason,
		};
	}

	private getReporterEmail(report: IARSubmission, reportLookupCache: ReportLookupCache): Promise<string | null> {
		const reporterId = report.reporterId;
		if (!reporterId) {
			return Promise.resolve(report.reporterEmail);
		}
		const key = reporterId.toString();
		const cached = reportLookupCache.reporterEmailByReporterId.get(key);
		if (cached) {
			return cached;
		}
		const email = this.deps.apiContext.services.users.findUnique(reporterId).then((user) => user?.email ?? null);
		reportLookupCache.reporterEmailByReporterId.set(key, email);
		return email;
	}

	private async getContextAuthorBotFlags(
		report: IARSubmission,
		requestCache: RequestCache,
	): Promise<Map<string, boolean | null>> {
		const authorIds = [
			...new Set((report.messageContext ?? []).flatMap((message) => (message.authorId ? [message.authorId] : []))),
		];
		if (authorIds.length === 0) {
			return new Map();
		}
		try {
			const partials = await this.deps.userCacheService.getUserPartialResponses(authorIds, requestCache);
			return new Map([...partials].map(([userId, partial]) => [userId.toString(), mapUserPartialBotFlag(partial)]));
		} catch (error) {
			Logger.warn({reportId: report.reportId.toString(), error}, 'Failed to resolve author bot flags for report');
			return new Map();
		}
	}

	private async mapProfileSnapshotToResponse(report: IARSubmission) {
		const snapshot = report.reportedProfileSnapshot;
		if (!snapshot) {
			return null;
		}
		const asset = (value: ReportProfileAssetSnapshot | null) => this.mapProfileSnapshotAsset(report.reportId, value);
		const {user, member, guild} = snapshot;
		return {
			captured_at: snapshot.captured_at,
			user: user
				? {
						id: user.id,
						username: user.username,
						discriminator: user.discriminator === null ? null : user.discriminator.toString().padStart(4, '0'),
						global_name: user.global_name,
						bio: user.bio,
						pronouns: user.pronouns,
						avatar: await asset(user.avatar),
						banner: await asset(user.banner),
					}
				: null,
			member: member
				? {
						guild_id: member.guild_id,
						nick: member.nick,
						bio: member.bio,
						pronouns: member.pronouns,
						joined_at: member.joined_at,
						avatar: await asset(member.avatar),
						banner: await asset(member.banner),
					}
				: null,
			guild: guild
				? {
						id: guild.id,
						name: guild.name,
						vanity_url_code: guild.vanity_url_code,
						icon: await asset(guild.icon),
						banner: await asset(guild.banner),
						splash: await asset(guild.splash),
					}
				: null,
		};
	}

	private async mapProfileSnapshotAsset(
		reportId: ReportID,
		asset: ReportProfileAssetSnapshot | null,
	): Promise<{hash: string; url: string | null} | null> {
		if (!asset) {
			return null;
		}
		if (!asset.key) {
			return {hash: asset.hash, url: null};
		}
		try {
			const url = await this.deps.storageService.getPresignedDownloadURL({
				bucket: Config.s3.buckets.reports,
				key: asset.key,
				expiresIn: seconds('5 minutes'),
			});
			return {hash: asset.hash, url};
		} catch (error) {
			Logger.error(
				{error, reportId: reportId.toString(), key: asset.key},
				'Failed to generate presigned URL for report profile asset',
			);
			return {hash: asset.hash, url: null};
		}
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
		const access = channel ? messageResponseAccessForChannel(channel) : messageResponseAccessForGuild(null);
		return {...access, includeHidden: true};
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
		reportLookupCache: ReportLookupCache,
		authorBotFlags: Map<string, boolean | null>,
	) {
		const channelId = message.channelId ?? fallbackChannelId;
		if (!channelId) {
			return null;
		}
		const authorId = (message.authorId ?? message.webhookId ?? 0n).toString();
		const channelNsfw = await this.getChannelNsfwState(channelId, reportLookupCache);
		const guildNsfwLevel = await this.getGuildNsfwLevelForContext(channelId, fallbackGuildId, reportLookupCache);
		const attachments =
			message.attachments && message.attachments.length > 0
				? (
						await Promise.all(
							message.attachments.map((attachment) => this.mapReportAttachmentToResponse(attachment, channelId)),
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
						} => attachment !== null,
					)
				: [];
		return {
			id: message.messageId.toString(),
			channel_id: channelId.toString(),
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
			author_id: authorId,
			author_username: message.authorUsername,
			author_global_name: null,
			author_discriminator: message.authorDiscriminator.toString().padStart(4, '0'),
			author_avatar: message.authorAvatarHash,
			webhook_id: message.webhookId?.toString() ?? null,
			author_bot: message.authorId ? (authorBotFlags.get(authorId) ?? null) : null,
			missing_attachments: message.missingAttachments
				.filter((attachment) => attachment.attachment_id != null && attachment.filename)
				.map(mapMissingAttachmentToResponse),
		};
	}

	private async getChannelNsfwState(
		channelId: ChannelID | null,
		reportLookupCache: ReportLookupCache,
	): Promise<boolean | null> {
		if (!channelId) {
			return null;
		}
		const channelIdString = channelId.toString();
		if (reportLookupCache.channelNsfwByChannelId.has(channelIdString)) {
			return reportLookupCache.channelNsfwByChannelId.get(channelIdString) ?? null;
		}
		const channel = await this.deps.channelRepository.findUnique(channelId);
		const scope = channel
			? await resolveNsfwScopeChannel(channel, (id) => this.deps.channelRepository.findUnique(id))
			: null;
		const channelNsfw = scope?.isNsfw ?? null;
		reportLookupCache.channelNsfwByChannelId.set(channelIdString, channelNsfw);
		return channelNsfw;
	}

	private async getGuildNsfwLevel(
		guildId: GuildID | null,
		reportLookupCache: ReportLookupCache,
	): Promise<number | null> {
		if (!guildId) {
			return null;
		}
		const guildIdString = guildId.toString();
		if (reportLookupCache.guildNsfwLevelByGuildId.has(guildIdString)) {
			return reportLookupCache.guildNsfwLevelByGuildId.get(guildIdString) ?? null;
		}
		const guild = await this.deps.guildRepository.findUnique(guildId);
		const guildNsfwLevel = guild?.nsfwLevel ?? null;
		reportLookupCache.guildNsfwLevelByGuildId.set(guildIdString, guildNsfwLevel);
		return guildNsfwLevel;
	}

	private async getGuildNsfwLevelForContext(
		channelId: ChannelID | null,
		fallbackGuildId: GuildID | null,
		reportLookupCache: ReportLookupCache,
	): Promise<number | null> {
		if (fallbackGuildId) {
			return this.getGuildNsfwLevel(fallbackGuildId, reportLookupCache);
		}
		if (!channelId) {
			return null;
		}
		const channel = await this.deps.channelRepository.findUnique(channelId);
		if (!channel?.guildId) {
			return null;
		}
		return this.getGuildNsfwLevel(channel.guildId, reportLookupCache);
	}

	private async mapReportAttachmentToResponse(
		attachment: MessageAttachment,
		channelId: ChannelID | null,
	): Promise<{
		id: string;
		filename: string;
		url: string;
		nsfw: boolean | null;
		content_type: string | null;
		width: number | null;
		height: number | null;
		size: number | null;
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
			};
		} catch (error) {
			Logger.error(
				{error, attachmentId, filename, channelId},
				'Failed to generate presigned URL for report attachment',
			);
		}
		return null;
	}

	private async buildUserTag(userId: UserID | null, requestCache: RequestCache): Promise<UserTagInfo | null> {
		if (!userId) {
			return null;
		}
		try {
			const cached = await this.deps.userCacheService.getUserPartialResponse(userId, requestCache);
			const stored = isHiddenPartial(cached) ? await this.deps.apiContext.services.users.findUnique(userId) : null;
			const user = stored
				? {
						...cached,
						username: stored.username,
						global_name: stored.globalName,
						discriminator: stored.discriminator.toString(),
						bot: stored.isBot,
					}
				: cached;
			const discriminator = user.discriminator?.padStart(4, '0') ?? '0000';
			return {
				tag: formatUserTag({
					username: user.username,
					discriminator: Number.parseInt(discriminator, 10),
					isBot: user.bot ?? false,
				}),
				username: user.username,
				global_name: user.global_name ?? null,
				discriminator,
				bot: mapUserPartialBotFlag(user),
			};
		} catch (error) {
			Logger.warn({userId: userId.toString(), error}, 'Failed to resolve user tag for report');
			return null;
		}
	}
}

function mapWebhookCreatorFields(report: IARSubmission) {
	const creatorDiscriminator = report.reportedWebhookCreatorDiscriminator;
	const discriminator = creatorDiscriminator === null ? null : creatorDiscriminator.toString().padStart(4, '0');
	const username = report.reportedWebhookCreatorUsername;
	const creatorId = report.reportedWebhookCreatorId;
	return {
		reported_webhook_creator_id: creatorId?.toString() ?? null,
		reported_webhook_creator_tag:
			username !== null && creatorDiscriminator !== null
				? formatUserTag({
						username,
						discriminator: creatorDiscriminator,
						isBot: creatorId !== null && creatorId.toString() === report.reportedWebhookApplicationId?.toString(),
					})
				: null,
		reported_webhook_creator_username: username,
		reported_webhook_creator_global_name: report.reportedWebhookCreatorGlobalName,
		reported_webhook_creator_discriminator: discriminator,
		reported_webhook_creator_avatar_hash: report.reportedWebhookCreatorAvatarHash,
	};
}

interface UserTagInfo {
	tag: string;
	username: string;
	global_name: string | null;
	discriminator: string;
	bot: boolean | null;
}
