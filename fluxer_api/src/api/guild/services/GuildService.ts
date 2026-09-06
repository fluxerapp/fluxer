// SPDX-License-Identifier: AGPL-3.0-or-later

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
	GuildEmojiMetadataResponse,
	GuildStickerMetadataResponse,
} from '@fluxer/schema/src/domains/guild/GuildEmojiSchemas';
import type {GuildUpdateRequest} from '@fluxer/schema/src/domains/guild/GuildRequestSchemas';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import type {UserPartialResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import type {IpInfoService} from '@pkgs/geoip/src/IpInfoService';
import type {ApiContext} from '../../ApiContext';
import type {EmojiID, GuildID, RoleID, StickerID, UserID} from '../../BrandedTypes';
import {createUserID, createWebhookID} from '../../BrandedTypes';
import type {IChannelRepository} from '../../channel/IChannelRepository';
import type {ChannelService} from '../../channel/services/ChannelService';
import type {AvatarService} from '../../infrastructure/AvatarService';
import type {EntityAssetService} from '../../infrastructure/EntityAssetService';
import type {IAssetDeletionQueue} from '../../infrastructure/IAssetDeletionQueue';
import type {IGatewayService} from '../../infrastructure/IGatewayService';
import type {UserCacheService} from '../../infrastructure/UserCacheService';
import type {InviteRepository} from '../../invite/InviteRepository';
import type {LimitConfigService} from '../../limits/LimitConfigService';
import type {RequestCache} from '../../middleware/RequestCacheMiddleware';
import type {GuildAuditLog} from '../../models/GuildAuditLog';
import type {Webhook} from '../../models/Webhook';
import type {IUserRepository} from '../../user/IUserRepository';
import {getCachedUserPartialResponses} from '../../user/UserCacheHelpers';
import type {IWebhookRepository} from '../../webhook/IWebhookRepository';
import type {GuildAuditLogService} from '../GuildAuditLogService';
import type {GuildAuditLogChange} from '../GuildAuditLogTypes';
import type {IGuildRepositoryAggregate} from '../repositories/IGuildRepositoryAggregate';
import {GuildChannelService} from './GuildChannelService';
import {GuildContentService} from './GuildContentService';
import {GuildDataService} from './GuildDataService';
import {GuildMemberService} from './GuildMemberService';
import {createGuildMfaEnforcer} from './GuildMfaEnforcement';
import {GuildModerationService} from './GuildModerationService';
import {GuildRoleService} from './GuildRoleService';
import {GuildSearchService} from './GuildSearchService';

interface AuditLogOptions {
	channel_id?: string;
	count?: number;
	delete_member_days?: string;
	id?: string;
	integration_type?: number;
	message_id?: string;
	members_removed?: number;
	role_name?: string;
	type?: number;
	inviter_id?: string;
	max_age?: number;
	max_uses?: number;
	temporary?: boolean;
	uses?: number;
}

interface GuildAuditLogEntryResponse {
	id: string;
	action_type: number;
	user_id: string | null;
	target_id: string | null;
	reason?: string;
	options?: AuditLogOptions;
	changes?: GuildAuditLogChange;
}

interface AuditLogWebhook {
	id: string;
	type: number;
	guild_id: string | null;
	channel_id: string | null;
	name: string;
	avatar_hash: string | null;
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
			allow_cloning: !guild.features.has(GuildFeatures.CLONE_EMOJI_DISABLED),
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
			allow_cloning: !guild.features.has(GuildFeatures.CLONE_STICKER_DISABLED),
		};
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
	}): Promise<{
		audit_log_entries: Array<GuildAuditLogEntryResponse>;
		users: Array<UserPartialResponse>;
		webhooks: Array<AuditLogWebhook>;
	}> {
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
	}): Promise<{
		audit_log_entries: Array<GuildAuditLogEntryResponse>;
		users: Array<UserPartialResponse>;
		webhooks: Array<AuditLogWebhook>;
	}> {
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
					if (processedLogs.length < effectiveLimit) {
						processedLogs.push(log);
					}
				}
			} else {
				for (const log of logs) {
					if (processedLogs.length < effectiveLimit) {
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
			userIdSet.add(log.userId);
			const targetUserId = this.getAuditLogTargetUserId(log);
			if (targetUserId) {
				userIdSet.add(targetUserId);
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
		const entries = processedLogs.map((log) => this.mapAuditLogToEntry(log));
		const users = Array.from(userPartials.values());
		const webhooks = this.buildAuditLogWebhookResponses(webhookRecords.webhooks);
		return {
			audit_log_entries: entries,
			users,
			webhooks,
		};
	}

	private mapAuditLogToEntry(log: GuildAuditLog): GuildAuditLogEntryResponse {
		return {
			id: log.logId.toString(),
			action_type: log.actionType,
			user_id: log.userId.toString(),
			target_id: log.targetId,
			reason: log.reason ?? undefined,
			options: this.buildAuditLogOptions(log.options),
			changes: this.scrubSensitiveChanges(log.changes),
		};
	}

	private scrubSensitiveChanges(changes: GuildAuditLogChange | null | undefined): GuildAuditLogChange | undefined {
		if (!changes) {
			return undefined;
		}
		const scrubbed = changes.filter((change) => change.key !== 'ip');
		return scrubbed.length > 0 ? scrubbed : undefined;
	}

	private buildAuditLogOptions(options: Map<string, string>): AuditLogOptions | undefined {
		if (!options.size) {
			return undefined;
		}
		const mapped: AuditLogOptions = {};
		for (const [key, value] of options) {
			switch (key) {
				case 'channel_id':
					mapped.channel_id = value;
					break;
				case 'count':
					this.assignNumericOption(mapped, 'count', value);
					break;
				case 'delete_member_days':
					mapped.delete_member_days = value;
					break;
				case 'delete_message_days':
					if (!mapped.delete_member_days) {
						mapped.delete_member_days = value;
					}
					break;
				case 'id':
					mapped.id = value;
					break;
				case 'integration_type':
					this.assignNumericOption(mapped, 'integration_type', value);
					break;
				case 'message_id':
					mapped.message_id = value;
					break;
				case 'members_removed':
					this.assignNumericOption(mapped, 'members_removed', value);
					break;
				case 'role_name':
					mapped.role_name = value;
					break;
				case 'type':
					this.assignNumericOption(mapped, 'type', value);
					break;
				case 'inviter_id':
					mapped.inviter_id = value;
					break;
				case 'max_age':
					this.assignNumericOption(mapped, 'max_age', value);
					break;
				case 'max_uses':
					this.assignNumericOption(mapped, 'max_uses', value);
					break;
				case 'uses':
					this.assignNumericOption(mapped, 'uses', value);
					break;
				case 'temporary':
					mapped.temporary = this.parseBooleanOption(value);
					break;
				default:
					break;
			}
		}
		return Object.keys(mapped).length === 0 ? undefined : mapped;
	}

	private parseBooleanOption(value: string): boolean {
		return value === 'true' || value === '1';
	}

	private assignNumericOption(
		target: AuditLogOptions,
		key: 'count' | 'integration_type' | 'members_removed' | 'type' | 'max_age' | 'max_uses' | 'uses',
		value: string,
	): void {
		const parsed = Number(value);
		if (!Number.isNaN(parsed)) {
			target[key] = parsed;
		}
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

	private buildAuditLogWebhookResponses(webhooks: Array<Webhook>): Array<AuditLogWebhook> {
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

	private getAuditLogTargetUserId(log: GuildAuditLog): UserID | null {
		if (!log.targetId || !this.isUserTargetAction(log.actionType)) {
			return null;
		}
		try {
			return createUserID(BigInt(log.targetId));
		} catch {
			return null;
		}
	}

	private isUserTargetAction(actionType: AuditLogActionType): boolean {
		return (
			actionType === AuditLogActionType.MEMBER_KICK ||
			actionType === AuditLogActionType.MEMBER_PRUNE ||
			actionType === AuditLogActionType.MEMBER_BAN_ADD ||
			actionType === AuditLogActionType.MEMBER_BAN_REMOVE ||
			actionType === AuditLogActionType.MEMBER_UPDATE ||
			actionType === AuditLogActionType.MEMBER_ROLE_UPDATE ||
			actionType === AuditLogActionType.MEMBER_MOVE ||
			actionType === AuditLogActionType.MEMBER_DISCONNECT ||
			actionType === AuditLogActionType.BOT_ADD
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
