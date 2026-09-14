// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import type {EmojiID, GuildID, RoleID, StickerID, UserID} from '@app/api/BrandedTypes';
import {createWebhookID} from '@app/api/BrandedTypes';
import type {IChannelRepository} from '@app/api/channel/IChannelRepository';
import type {ChannelService} from '@app/api/channel/services/ChannelService';
import {resolveExpressionSourceGuild} from '@app/api/guild/ExpressionSourceGuild';
import {
	collectGuildAuditLogUserIds,
	isNoopGuildAuditLog,
	mapGuildAuditLogEntry,
	type StoredGuildAuditLogEntryResponse,
} from '@app/api/guild/GuildAuditLogEntryMapper';
import type {GuildAuditLogService} from '@app/api/guild/GuildAuditLogService';
import type {IGuildRepositoryAggregate} from '@app/api/guild/repositories/IGuildRepositoryAggregate';
import {GuildChannelService} from '@app/api/guild/services/GuildChannelService';
import {GuildContentService} from '@app/api/guild/services/GuildContentService';
import {GuildDataService} from '@app/api/guild/services/GuildDataService';
import {GuildMemberService} from '@app/api/guild/services/GuildMemberService';
import {createGuildMfaEnforcer} from '@app/api/guild/services/GuildMfaEnforcement';
import {GuildModerationService} from '@app/api/guild/services/GuildModerationService';
import {GuildRoleService} from '@app/api/guild/services/GuildRoleService';
import {GuildSearchService} from '@app/api/guild/services/GuildSearchService';
import type {AvatarService} from '@app/api/infrastructure/AvatarService';
import type {EntityAssetService} from '@app/api/infrastructure/EntityAssetService';
import type {IAssetDeletionQueue} from '@app/api/infrastructure/IAssetDeletionQueue';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import type {InviteRepository} from '@app/api/invite/InviteRepository';
import type {LimitConfigService} from '@app/api/limits/LimitConfigService';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {GuildAuditLog} from '@app/api/models/GuildAuditLog';
import type {Webhook} from '@app/api/models/Webhook';
import type {IUserRepository} from '@app/api/user/IUserRepository';
import {getCachedUserPartialResponses} from '@app/api/user/UserCacheHelpers';
import type {IWebhookRepository} from '@app/api/webhook/IWebhookRepository';
import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {MissingAccessError} from '@fluxer/errors/src/domains/core/MissingAccessError';
import {MissingPermissionsError} from '@fluxer/errors/src/domains/core/MissingPermissionsError';
import {ResourceLockedError} from '@fluxer/errors/src/domains/core/ResourceLockedError';
import {UnknownGuildEmojiError} from '@fluxer/errors/src/domains/guild/UnknownGuildEmojiError';
import {UnknownGuildError} from '@fluxer/errors/src/domains/guild/UnknownGuildError';
import {UnknownGuildStickerError} from '@fluxer/errors/src/domains/guild/UnknownGuildStickerError';
import type {
	AuditLogWebhookResponse,
	GuildAuditLogListResponse,
} from '@fluxer/schema/src/domains/guild/GuildAuditLogSchemas';
import type {
	GuildEmojiMetadataResponse,
	GuildExpressionSourceGuildResponse,
	GuildStickerMetadataResponse,
} from '@fluxer/schema/src/domains/guild/GuildEmojiSchemas';
import type {GuildUpdateRequest} from '@fluxer/schema/src/domains/guild/GuildRequestSchemas';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import type {IpInfoService} from '@pkgs/geoip/src/IpInfoService';

interface StoredAuditLogWebhookResponse extends Omit<AuditLogWebhookResponse, 'type'> {
	type: number;
}

interface StoredGuildAuditLogListResponse extends Omit<GuildAuditLogListResponse, 'audit_log_entries' | 'webhooks'> {
	audit_log_entries: Array<StoredGuildAuditLogEntryResponse>;
	webhooks: Array<StoredAuditLogWebhookResponse>;
}

interface GuildAuth {
	guildData: GuildResponse;
	checkPermission: (permission: bigint) => Promise<void>;
	checkTargetMember: (targetUserId: UserID) => Promise<void>;
	getAssignableRoleIds: () => Promise<Array<RoleID>>;
	getMaxRolePosition: () => Promise<number>;
	getMyPermissions: () => Promise<bigint>;
	hasPermission: (permission: bigint) => Promise<boolean>;
	canManageRoles: (targetUserId: UserID, targetRoleId: RoleID) => Promise<boolean>;
}

const GUILD_UPDATE_LOCK_TTL_SECONDS = 10;
const GUILD_UPDATE_LOCK_RETRY_DELAY_MS = 50;
const GUILD_UPDATE_LOCK_MAX_WAIT_MS = 5000;

export class GuildService {
	public readonly data: GuildDataService;
	public readonly members: GuildMemberService;
	public readonly roles: GuildRoleService;
	public readonly moderation: GuildModerationService;
	public readonly content: GuildContentService;
	public readonly channels: GuildChannelService;
	public readonly search: GuildSearchService;
	private readonly guildRepository: IGuildRepositoryAggregate;
	private readonly cacheService: ICacheService;
	private readonly userCacheService: UserCacheService;
	private readonly webhookRepository: IWebhookRepository;
	private readonly guildAuditLogService: GuildAuditLogService;
	private readonly gatewayService: IGatewayService;
	private readonly userRepository: IUserRepository;

	constructor(
		apiContext: ApiContext,
		guildRepository: IGuildRepositoryAggregate,
		channelRepository: IChannelRepository,
		inviteRepository: InviteRepository,
		channelService: ChannelService,
		userCacheService: UserCacheService,
		entityAssetService: EntityAssetService,
		avatarService: AvatarService,
		assetDeletionQueue: IAssetDeletionQueue,
		webhookRepository: IWebhookRepository,
		guildAuditLogService: GuildAuditLogService,
		limitConfigService: LimitConfigService,
		ipInfoService: IpInfoService,
	) {
		const {
			cache: cacheService,
			gateway: gatewayService,
			users: userRepository,
			worker: workerService,
			snowflake: snowflakeService,
			rateLimit: rateLimitService,
		} = apiContext.services;
		this.gatewayService = gatewayService;
		this.guildRepository = guildRepository;
		this.cacheService = cacheService;
		this.userCacheService = userCacheService;
		this.webhookRepository = webhookRepository;
		this.guildAuditLogService = guildAuditLogService;
		this.userRepository = userRepository;
		this.data = new GuildDataService(
			guildRepository,
			channelRepository,
			inviteRepository,
			channelService,
			gatewayService,
			entityAssetService,
			userRepository,
			snowflakeService,
			webhookRepository,
			guildAuditLogService,
			limitConfigService,
		);
		this.members = new GuildMemberService(
			guildRepository,
			channelService,
			userCacheService,
			gatewayService,
			entityAssetService,
			userRepository,
			rateLimitService,
			guildAuditLogService,
			limitConfigService,
			ipInfoService,
		);
		this.roles = new GuildRoleService(
			guildRepository,
			snowflakeService,
			cacheService,
			gatewayService,
			guildAuditLogService,
			limitConfigService,
			userRepository,
		);
		this.moderation = new GuildModerationService(
			guildRepository,
			userRepository,
			gatewayService,
			userCacheService,
			workerService,
			guildAuditLogService,
			ipInfoService,
		);
		this.content = new GuildContentService(
			guildRepository,
			userCacheService,
			gatewayService,
			avatarService,
			snowflakeService,
			guildAuditLogService,
			assetDeletionQueue,
			limitConfigService,
		);
		this.channels = new GuildChannelService(
			channelRepository,
			guildRepository,
			userCacheService,
			gatewayService,
			cacheService,
			snowflakeService,
			guildAuditLogService,
			limitConfigService,
			userRepository,
		);
		this.search = new GuildSearchService(
			channelRepository,
			userCacheService,
			gatewayService,
			userRepository,
			workerService,
		);
	}

	async updateGuild(
		params: {
			userId: UserID;
			guildId: GuildID;
			data: GuildUpdateRequest;
			requestCache: RequestCache;
		},
		auditLogReason?: string | null,
	): Promise<GuildResponse> {
		const {guildId, requestCache} = params;
		const {guild, previousFeatures, updatedFeatures} = await this.withGuildUpdateLock(guildId, requestCache, () =>
			this.data.updateGuild(params, auditLogReason),
		);
		if (
			previousFeatures.has(GuildFeatures.TEXT_CHANNEL_FLEXIBLE_NAMES) &&
			!updatedFeatures.has(GuildFeatures.TEXT_CHANNEL_FLEXIBLE_NAMES)
		) {
			await this.channels.sanitizeTextChannelNames({guildId, requestCache});
		}
		return guild;
	}

	private async withGuildUpdateLock<T>(guildId: GuildID, requestCache: RequestCache, fn: () => Promise<T>): Promise<T> {
		const lockKey = `guild:${guildId}:update`;
		const lockToken = await this.acquireGuildUpdateLock(lockKey);
		if (!lockToken) {
			throw new ResourceLockedError();
		}
		try {
			requestCache.guilds.delete(guildId);
			return await fn();
		} finally {
			await this.cacheService.releaseLock(lockKey, lockToken);
		}
	}

	private async acquireGuildUpdateLock(lockKey: string): Promise<string | null> {
		const startTime = Date.now();
		while (Date.now() - startTime < GUILD_UPDATE_LOCK_MAX_WAIT_MS) {
			const token = await this.cacheService.acquireLock(lockKey, GUILD_UPDATE_LOCK_TTL_SECONDS);
			if (token) {
				return token;
			}
			await new Promise((resolve) => setTimeout(resolve, GUILD_UPDATE_LOCK_RETRY_DELAY_MS));
		}
		return null;
	}

	async getEmojiMetadata(emojiId: EmojiID): Promise<GuildEmojiMetadataResponse> {
		const emoji = await this.guildRepository.getEmojiById(emojiId);
		if (!emoji) throw new UnknownGuildEmojiError();
		const guild = await this.data.getGuildSystem(emoji.guildId);
		return {
			id: emoji.id.toString(),
			guild_id: guild.id.toString(),
			name: emoji.name,
			animated: emoji.isAnimated,
			allow_cloning: guild.features.has(GuildFeatures.CLONE_EMOJI_ENABLED),
		};
	}

	async getStickerMetadata(stickerId: StickerID): Promise<GuildStickerMetadataResponse> {
		const sticker = await this.guildRepository.getStickerById(stickerId);
		if (!sticker) throw new UnknownGuildStickerError();
		const guild = await this.data.getGuildSystem(sticker.guildId);
		return {
			id: sticker.id.toString(),
			guild_id: guild.id.toString(),
			name: sticker.name,
			animated: sticker.animated,
			allow_cloning: guild.features.has(GuildFeatures.CLONE_STICKER_ENABLED),
		};
	}

	async getEmojiSource(emojiId: EmojiID, userId: UserID): Promise<GuildExpressionSourceGuildResponse> {
		const emoji = await this.guildRepository.getEmojiById(emojiId);
		if (!emoji) throw new UnknownGuildEmojiError();
		return this.resolveExpressionSourceGuild(emoji.guildId, userId);
	}

	async getStickerSource(stickerId: StickerID, userId: UserID): Promise<GuildExpressionSourceGuildResponse> {
		const sticker = await this.guildRepository.getStickerById(stickerId);
		if (!sticker) throw new UnknownGuildStickerError();
		return this.resolveExpressionSourceGuild(sticker.guildId, userId);
	}

	private resolveExpressionSourceGuild(guildId: GuildID, userId: UserID): Promise<GuildExpressionSourceGuildResponse> {
		return resolveExpressionSourceGuild({
			loadGuild: () => this.gatewayService.getGuildData({guildId, userId, skipMembershipCheck: true}),
			isMember: async () => (await this.gatewayService.getGuildMember({guildId, userId})).success,
		});
	}

	async listGuildAuditLogs(params: {
		userId: UserID;
		guildId: GuildID;
		requestCache: RequestCache;
		limit?: number;
		beforeLogId?: bigint;
		afterLogId?: bigint;
		filterUserId?: UserID;
		actionType?: AuditLogActionType;
	}): Promise<StoredGuildAuditLogListResponse> {
		const {userId, guildId} = params;
		const [hasPermission, guild] = await Promise.all([
			this.gatewayService.checkPermission({
				guildId,
				userId,
				permission: Permissions.VIEW_AUDIT_LOG,
			}),
			this.guildRepository.findUnique(guildId),
		]);
		if (!guild) {
			throw new UnknownGuildError();
		}
		if (!hasPermission) {
			throw new MissingPermissionsError();
		}
		return this.fetchGuildAuditLogs(params);
	}

	async fetchGuildAuditLogs(params: {
		guildId: GuildID;
		requestCache: RequestCache;
		limit?: number;
		beforeLogId?: bigint;
		afterLogId?: bigint;
		filterUserId?: UserID;
		actionType?: AuditLogActionType;
	}): Promise<StoredGuildAuditLogListResponse> {
		const {guildId, requestCache, limit = 50, beforeLogId, afterLogId, filterUserId, actionType} = params;
		if (beforeLogId !== undefined && afterLogId !== undefined) {
			throw InputValidationError.fromCode('before', ValidationErrorCodes.CANNOT_SPECIFY_BOTH_BEFORE_AND_AFTER);
		}
		const effectiveLimit = Math.max(1, Math.min(limit, 100));
		const shouldBatch = actionType === undefined && !filterUserId;
		let processedLogs: Array<GuildAuditLog> = [];
		let currentBeforeLogId = beforeLogId;
		let currentAfterLogId = afterLogId;
		while (processedLogs.length < effectiveLimit) {
			const fetchLimit = Math.min(effectiveLimit * 2, 200);
			const logs = await this.guildRepository.listAuditLogs({
				guildId,
				limit: fetchLimit,
				beforeLogId: currentBeforeLogId,
				afterLogId: currentAfterLogId,
				userId: filterUserId,
				actionType,
			});
			if (logs.length === 0) {
				break;
			}
			if (shouldBatch) {
				const batchResult = await this.guildAuditLogService.batchConsecutiveMessageDeleteLogs(guildId, logs);
				for (const log of batchResult.processedLogs) {
					if (processedLogs.length < effectiveLimit && !isNoopGuildAuditLog(log.actionType, log.changes)) {
						processedLogs.push(log);
					}
				}
			} else {
				for (const log of logs) {
					if (processedLogs.length < effectiveLimit && !isNoopGuildAuditLog(log.actionType, log.changes)) {
						processedLogs.push(log);
					}
				}
			}
			if (logs.length < fetchLimit) {
				break;
			}
			const lastLog = logs[logs.length - 1];
			if (afterLogId !== undefined) {
				currentAfterLogId = lastLog.logId;
			} else {
				currentBeforeLogId = lastLog.logId;
			}
		}
		processedLogs = processedLogs.slice(0, effectiveLimit);
		const userIdSet = new Set<UserID>();
		for (const log of processedLogs) {
			for (const referencedUserId of collectGuildAuditLogUserIds(log)) {
				userIdSet.add(referencedUserId);
			}
		}
		const [userPartials, webhookRecords] = await Promise.all([
			getCachedUserPartialResponses({
				userIds: Array.from(userIdSet),
				userCacheService: this.userCacheService,
				requestCache,
			}),
			this.loadAuditLogWebhooks(processedLogs),
		]);
		const entries = processedLogs.map((log) => mapGuildAuditLogEntry(log));
		const users = Array.from(userPartials.values());
		const webhooks = this.buildAuditLogWebhookResponses(webhookRecords.webhooks);
		return {
			audit_log_entries: entries,
			users,
			webhooks,
		};
	}

	private async loadAuditLogWebhooks(logs: Array<GuildAuditLog>): Promise<{
		webhooks: Array<Webhook>;
	}> {
		const webhookIds = new Set<string>();
		for (const log of logs) {
			if (this.isWebhookAction(log.actionType) && log.targetId) {
				webhookIds.add(log.targetId);
			}
		}
		if (webhookIds.size === 0) {
			return {webhooks: []};
		}
		const webhookPromises = Array.from(webhookIds, (id) => {
			try {
				return this.webhookRepository.findUnique(createWebhookID(BigInt(id)));
			} catch {
				return Promise.resolve(null);
			}
		});
		const results = await Promise.all(webhookPromises);
		const foundWebhooks = results.filter((webhook): webhook is Webhook => webhook !== null);
		return {webhooks: foundWebhooks};
	}

	private buildAuditLogWebhookResponses(webhooks: Array<Webhook>): Array<StoredAuditLogWebhookResponse> {
		return webhooks.map((webhook) => ({
			id: webhook.id.toString(),
			type: webhook.type,
			guild_id: webhook.guildId?.toString() ?? null,
			channel_id: webhook.channelId?.toString() ?? null,
			name: webhook.name,
			avatar_hash: webhook.avatarHash,
		}));
	}

	private isWebhookAction(actionType: AuditLogActionType): boolean {
		return (
			actionType === AuditLogActionType.WEBHOOK_CREATE ||
			actionType === AuditLogActionType.WEBHOOK_UPDATE ||
			actionType === AuditLogActionType.WEBHOOK_DELETE
		);
	}

	async getGuildAuthenticated({userId, guildId}: {userId: UserID; guildId: GuildID}): Promise<GuildAuth> {
		const guildData = await this.gatewayService.getGuildData({guildId, userId});
		if (!guildData) throw new MissingAccessError();
		const enforceGuildMfa = await createGuildMfaEnforcer({userRepository: this.userRepository, guildData, userId});
		const checkPermission = async (permission: bigint) => {
			const hasPermission = await this.gatewayService.checkPermission({guildId, userId, permission});
			if (!hasPermission) throw new MissingPermissionsError();
			enforceGuildMfa(permission);
		};
		const checkTargetMember = async (targetUserId: UserID) => {
			const canManage = await this.gatewayService.checkTargetMember({guildId, userId, targetUserId});
			if (!canManage) throw new MissingPermissionsError();
		};
		const getAssignableRoleIds = async () => this.gatewayService.getAssignableRoles({guildId, userId});
		const getMaxRolePosition = async () => this.gatewayService.getUserMaxRolePosition({guildId, userId});
		const getMyPermissions = async () => this.gatewayService.getUserPermissions({guildId, userId});
		const hasPermission = async (permission: bigint) =>
			this.gatewayService.checkPermission({guildId, userId, permission});
		const canManageRoles = async (targetUserId: UserID, targetRoleId: RoleID) =>
			this.gatewayService.canManageRoles({guildId, userId, targetUserId, roleId: targetRoleId});
		return {
			guildData,
			checkPermission,
			checkTargetMember,
			getAssignableRoleIds,
			getMaxRolePosition,
			getMyPermissions,
			hasPermission,
			canManageRoles,
		};
	}
}
