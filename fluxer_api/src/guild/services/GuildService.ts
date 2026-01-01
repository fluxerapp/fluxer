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

import type {ChannelID, EmojiID, GuildID, RoleID, StickerID, UserID} from '~/BrandedTypes';
import {createUserID, createWebhookID} from '~/BrandedTypes';
import {AuditLogActionType, GuildFeatures, Permissions} from '~/Constants';
import type {
	ChannelCreateRequest,
	ChannelResponse,
	MessageSearchRequest,
	MessageSearchResponse,
} from '~/channel/ChannelModel';
import type {IChannelRepository} from '~/channel/IChannelRepository';
import type {ChannelService} from '~/channel/services/ChannelService';
import {MissingAccessError, MissingPermissionsError, UnknownGuildError} from '~/Errors';
import type {GuildAuditLogChange} from '~/guild/GuildAuditLogTypes';
import type {AvatarService} from '~/infrastructure/AvatarService';
import type {EntityAssetService} from '~/infrastructure/EntityAssetService';
import type {IAssetDeletionQueue} from '~/infrastructure/IAssetDeletionQueue';
import type {ICacheService} from '~/infrastructure/ICacheService';
import type {IGatewayService} from '~/infrastructure/IGatewayService';
import type {IMediaService} from '~/infrastructure/IMediaService';
import type {IRateLimitService} from '~/infrastructure/IRateLimitService';
import type {SnowflakeService} from '~/infrastructure/SnowflakeService';
import type {UserCacheService} from '~/infrastructure/UserCacheService';
import type {InviteRepository} from '~/invite/InviteRepository';
import type {Guild, GuildAuditLog, GuildMember, User, Webhook} from '~/Models';
import type {RequestCache} from '~/middleware/RequestCacheMiddleware';
import type {IUserRepository} from '~/user/IUserRepository';
import {getCachedUserPartialResponses} from '~/user/UserCacheHelpers';
import type {UserPartialResponse} from '~/user/UserModel';
import type {IWebhookRepository} from '~/webhook/IWebhookRepository';
import type {IWorkerService} from '~/worker/IWorkerService';
import type {GuildAuditLogService} from '../GuildAuditLogService';
import type {
	GuildBanResponse,
	GuildCreateRequest,
	GuildEmojiResponse,
	GuildEmojiWithUserResponse,
	GuildMemberResponse,
	GuildMemberUpdateRequest,
	GuildPartialResponse,
	GuildResponse,
	GuildRoleCreateRequest,
	GuildRoleResponse,
	GuildRoleUpdateRequest,
	GuildStickerResponse,
	GuildStickerWithUserResponse,
	GuildUpdateRequest,
	GuildVanityURLResponse,
} from '../GuildModel';
import type {IGuildRepository} from '../IGuildRepository';
import {GuildChannelService} from './GuildChannelService';
import {GuildContentService} from './GuildContentService';
import {GuildDataService} from './GuildDataService';
import {GuildMemberService} from './GuildMemberService';
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

interface GuildAuditLogEntry {
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

export class GuildService {
	public readonly data: GuildDataService;
	public readonly members: GuildMemberService;
	public readonly roles: GuildRoleService;
	public readonly moderation: GuildModerationService;
	public readonly content: GuildContentService;
	public readonly channels: GuildChannelService;
	public readonly search: GuildSearchService;
	private readonly guildRepository: IGuildRepository;
	private readonly userCacheService: UserCacheService;
	private readonly webhookRepository: IWebhookRepository;
	private readonly guildAuditLogService: GuildAuditLogService;

	private readonly gatewayService: IGatewayService;

	constructor(
		guildRepository: IGuildRepository,
		channelRepository: IChannelRepository,
		inviteRepository: InviteRepository,
		channelService: ChannelService,
		userCacheService: UserCacheService,
		gatewayService: IGatewayService,
		entityAssetService: EntityAssetService,
		avatarService: AvatarService,
		assetDeletionQueue: IAssetDeletionQueue,
		userRepository: IUserRepository,
		mediaService: IMediaService,
		cacheService: ICacheService,
		snowflakeService: SnowflakeService,
		rateLimitService: IRateLimitService,
		workerService: IWorkerService,
		webhookRepository: IWebhookRepository,
		guildAuditLogService: GuildAuditLogService,
	) {
		this.gatewayService = gatewayService;
		this.guildRepository = guildRepository;
		this.userCacheService = userCacheService;
		this.webhookRepository = webhookRepository;
		this.guildAuditLogService = guildAuditLogService;

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
		);
		this.roles = new GuildRoleService(
			guildRepository,
			guildRepository,
			snowflakeService,
			cacheService,
			gatewayService,
			guildAuditLogService,
		);
		this.moderation = new GuildModerationService(
			guildRepository,
			userRepository,
			gatewayService,
			userCacheService,
			workerService,
			guildAuditLogService,
		);
		this.content = new GuildContentService(
			guildRepository,
			userCacheService,
			gatewayService,
			avatarService,
			snowflakeService,
			guildAuditLogService,
			assetDeletionQueue,
		);
		this.channels = new GuildChannelService(
			channelRepository,
			userCacheService,
			gatewayService,
			cacheService,
			snowflakeService,
			guildAuditLogService,
		);
		this.search = new GuildSearchService(
			channelRepository,
			channelService,
			userCacheService,
			gatewayService,
			userRepository,
			mediaService,
			workerService,
		);
	}

	async getGuild({userId, guildId}: {userId: UserID; guildId: GuildID}): Promise<GuildResponse> {
		return this.data.getGuild({userId, guildId});
	}

	async getUserGuilds(userId: UserID): Promise<Array<GuildResponse>> {
		return this.data.getUserGuilds(userId);
	}

	async getPublicGuildData(guildId: GuildID): Promise<GuildPartialResponse> {
		return this.data.getPublicGuildData(guildId);
	}

	async getGuildSystem(guildId: GuildID): Promise<Guild> {
		return this.data.getGuildSystem(guildId);
	}

	async createGuild(
		params: {user: User; data: GuildCreateRequest},
		auditLogReason?: string | null,
	): Promise<GuildResponse> {
		return this.data.createGuild(params, auditLogReason);
	}

	async updateGuild(
		params: {userId: UserID; guildId: GuildID; data: GuildUpdateRequest; requestCache: RequestCache},
		auditLogReason?: string | null,
	): Promise<GuildResponse> {
		return this.data.updateGuild(params, auditLogReason);
	}

	async updateTextChannelFlexibleNamesFeature(
		params: {userId: UserID; guildId: GuildID; enabled: boolean; requestCache: RequestCache},
		auditLogReason?: string | null,
	): Promise<GuildResponse> {
		const {userId, guildId, enabled, requestCache} = params;

		const hasPermission = await this.gatewayService.checkPermission({
			guildId,
			userId,
			permission: Permissions.MANAGE_GUILD,
		});
		if (!hasPermission) {
			throw new MissingPermissionsError();
		}

		const currentGuild = await this.data.getGuildSystem(guildId);
		const hadFlexibleNamesEnabled = currentGuild.features.has(GuildFeatures.TEXT_CHANNEL_FLEXIBLE_NAMES);

		const featuresSet = new Set(currentGuild.features);
		if (enabled) {
			featuresSet.add(GuildFeatures.TEXT_CHANNEL_FLEXIBLE_NAMES);
		} else {
			featuresSet.delete(GuildFeatures.TEXT_CHANNEL_FLEXIBLE_NAMES);
		}
		const featuresPayload = Array.from(featuresSet);

		const updatedGuild = await this.data.updateGuild(
			{
				userId,
				guildId,
				data: {features: featuresPayload} as GuildUpdateRequest,
				requestCache,
			},
			auditLogReason,
		);

		if (hadFlexibleNamesEnabled && !enabled) {
			await this.channels.sanitizeTextChannelNames({guildId, requestCache});
		}

		return updatedGuild;
	}

	async updateDetachedBannerFeature(
		params: {userId: UserID; guildId: GuildID; enabled: boolean; requestCache: RequestCache},
		auditLogReason?: string | null,
	): Promise<GuildResponse> {
		const {userId, guildId, enabled, requestCache} = params;

		const hasPermission = await this.gatewayService.checkPermission({
			guildId,
			userId,
			permission: Permissions.MANAGE_GUILD,
		});
		if (!hasPermission) {
			throw new MissingPermissionsError();
		}

		const currentGuild = await this.data.getGuildSystem(guildId);

		const featuresSet = new Set(currentGuild.features);
		if (enabled) {
			featuresSet.add(GuildFeatures.DETACHED_BANNER);
		} else {
			featuresSet.delete(GuildFeatures.DETACHED_BANNER);
		}
		const featuresPayload = Array.from(featuresSet);

		const updatedGuild = await this.data.updateGuild(
			{
				userId,
				guildId,
				data: {features: featuresPayload} as GuildUpdateRequest,
				requestCache,
			},
			auditLogReason,
		);

		return updatedGuild;
	}

	async updateDisallowUnclaimedAccountsFeature(
		params: {userId: UserID; guildId: GuildID; enabled: boolean; requestCache: RequestCache},
		auditLogReason?: string | null,
	): Promise<GuildResponse> {
		const {userId, guildId, enabled, requestCache} = params;

		const hasPermission = await this.gatewayService.checkPermission({
			guildId,
			userId,
			permission: Permissions.MANAGE_GUILD,
		});
		if (!hasPermission) {
			throw new MissingPermissionsError();
		}

		const currentGuild = await this.data.getGuildSystem(guildId);

		const featuresSet = new Set(currentGuild.features);
		if (enabled) {
			featuresSet.add(GuildFeatures.DISALLOW_UNCLAIMED_ACCOUNTS);
		} else {
			featuresSet.delete(GuildFeatures.DISALLOW_UNCLAIMED_ACCOUNTS);
		}
		const featuresPayload = Array.from(featuresSet);

		const updatedGuild = await this.data.updateGuild(
			{
				userId,
				guildId,
				data: {features: featuresPayload} as GuildUpdateRequest,
				requestCache,
			},
			auditLogReason,
		);

		return updatedGuild;
	}

	async getVanityURL(params: {userId: UserID; guildId: GuildID}): Promise<GuildVanityURLResponse> {
		return this.data.getVanityURL(params);
	}

	async updateVanityURL(
		params: {userId: UserID; guildId: GuildID; code: string | null; requestCache: RequestCache},
		auditLogReason?: string | null,
	): Promise<{code: string}> {
		return this.data.updateVanityURL(params, auditLogReason);
	}

	async deleteGuild(params: {user: User; guildId: GuildID}, auditLogReason?: string | null): Promise<void> {
		return this.data.deleteGuild(params, auditLogReason);
	}

	async deleteGuildAsAdmin(guildId: GuildID, auditLogReason?: string | null): Promise<void> {
		return this.data.deleteGuildForAdmin(guildId, auditLogReason);
	}

	async transferOwnership(
		params: {userId: UserID; guildId: GuildID; newOwnerId: UserID},
		auditLogReason?: string | null,
	): Promise<GuildResponse> {
		return this.data.transferOwnership(params, auditLogReason);
	}

	async checkGuildVerification(params: {user: User; guild: Guild; member: GuildMember}): Promise<void> {
		return this.data.checkGuildVerification(params);
	}

	async getMembers(params: {
		userId: UserID;
		guildId: GuildID;
		requestCache: RequestCache;
	}): Promise<Array<GuildMemberResponse>> {
		return this.members.getMembers(params);
	}

	async getMember(params: {
		userId: UserID;
		targetId: UserID;
		guildId: GuildID;
		requestCache: RequestCache;
	}): Promise<GuildMemberResponse> {
		return this.members.getMember(params);
	}

	async updateMember(
		params: {
			userId: UserID;
			targetId: UserID;
			guildId: GuildID;
			data: GuildMemberUpdateRequest | Omit<GuildMemberUpdateRequest, 'roles'>;
			requestCache: RequestCache;
		},
		auditLogReason?: string | null,
	): Promise<GuildMemberResponse> {
		return this.members.updateMember(params, auditLogReason);
	}

	async addMemberRole(
		params: {userId: UserID; targetId: UserID; guildId: GuildID; roleId: RoleID; requestCache: RequestCache},
		auditLogReason?: string | null,
	): Promise<void> {
		return this.members.addMemberRole(params, auditLogReason);
	}

	async removeMemberRole(
		params: {userId: UserID; targetId: UserID; guildId: GuildID; roleId: RoleID; requestCache: RequestCache},
		auditLogReason?: string | null,
	): Promise<void> {
		return this.members.removeMemberRole(params, auditLogReason);
	}

	async removeMember(
		params: {userId: UserID; targetId: UserID; guildId: GuildID},
		auditLogReason?: string | null,
	): Promise<void> {
		return this.members.removeMember(params, auditLogReason);
	}

	async addUserToGuild(params: {
		userId: UserID;
		guildId: GuildID;
		sendJoinMessage?: boolean;
		skipGuildLimitCheck?: boolean;
		skipBanCheck?: boolean;
		isTemporary?: boolean;
		joinSourceType?: number;
		requestCache: RequestCache;
		initiatorId?: UserID;
	}): Promise<GuildMember> {
		return this.members.addUserToGuild(params);
	}

	async leaveGuild(params: {userId: UserID; guildId: GuildID}, auditLogReason?: string | null): Promise<void> {
		return this.members.leaveGuild(params, auditLogReason);
	}

	async createRole(
		params: {userId: UserID; guildId: GuildID; data: GuildRoleCreateRequest},
		auditLogReason?: string | null,
	): Promise<GuildRoleResponse> {
		return this.roles.createRole(params, auditLogReason);
	}

	async updateRole(
		params: {userId: UserID; guildId: GuildID; roleId: RoleID; data: GuildRoleUpdateRequest},
		auditLogReason?: string | null,
	): Promise<GuildRoleResponse> {
		return this.roles.updateRole(params, auditLogReason);
	}

	async deleteRole(
		params: {userId: UserID; guildId: GuildID; roleId: RoleID},
		auditLogReason?: string | null,
	): Promise<void> {
		return this.roles.deleteRole(params, auditLogReason);
	}

	async updateRolePositions(
		params: {userId: UserID; guildId: GuildID; updates: Array<{roleId: RoleID; position?: number}>},
		auditLogReason?: string | null,
	): Promise<void> {
		return this.roles.updateRolePositions(params, auditLogReason);
	}

	async listRoles(params: {userId: UserID; guildId: GuildID}): Promise<Array<GuildRoleResponse>> {
		return this.roles.listRoles(params);
	}

	async updateHoistPositions(
		params: {userId: UserID; guildId: GuildID; updates: Array<{roleId: RoleID; hoistPosition: number}>},
		auditLogReason?: string | null,
	): Promise<void> {
		return this.roles.updateHoistPositions(params, auditLogReason);
	}

	async resetHoistPositions(params: {userId: UserID; guildId: GuildID}, auditLogReason?: string | null): Promise<void> {
		return this.roles.resetHoistPositions(params, auditLogReason);
	}

	async banMember(
		params: {
			userId: UserID;
			targetId: UserID;
			guildId: GuildID;
			deleteMessageDays?: number;
			reason?: string | null;
			banDurationSeconds?: number;
		},
		auditLogReason?: string | null,
	): Promise<void> {
		return this.moderation.banMember(params, auditLogReason);
	}

	async unbanMember(
		params: {userId: UserID; targetId: UserID; guildId: GuildID},
		auditLogReason?: string | null,
	): Promise<void> {
		return this.moderation.unbanMember(params, auditLogReason);
	}

	async listBans(params: {
		userId: UserID;
		guildId: GuildID;
		requestCache: RequestCache;
	}): Promise<Array<GuildBanResponse>> {
		return this.moderation.listBans(params);
	}

	async checkUserBanStatus(params: {userId: UserID; guildId: GuildID}): Promise<void> {
		return this.moderation.checkUserBanStatus(params);
	}

	async getEmojis(params: {
		userId: UserID;
		guildId: GuildID;
		requestCache: RequestCache;
	}): Promise<Array<GuildEmojiWithUserResponse>> {
		return this.content.getEmojis(params);
	}

	async getEmojiUser(params: {
		userId: UserID;
		guildId: GuildID;
		emojiId: EmojiID;
		requestCache: RequestCache;
	}): Promise<UserPartialResponse> {
		return this.content.getEmojiUser(params);
	}

	async createEmoji(
		params: {user: User; guildId: GuildID; name: string; image: string},
		auditLogReason?: string | null,
	): Promise<GuildEmojiResponse> {
		return this.content.createEmoji(params, auditLogReason);
	}

	async bulkCreateEmojis(
		params: {user: User; guildId: GuildID; emojis: Array<{name: string; image: string}>},
		auditLogReason?: string | null,
	): Promise<{
		success: Array<GuildEmojiResponse>;
		failed: Array<{name: string; error: string}>;
	}> {
		return this.content.bulkCreateEmojis(params, auditLogReason);
	}

	async updateEmoji(
		params: {userId: UserID; guildId: GuildID; emojiId: EmojiID; name: string},
		auditLogReason?: string | null,
	): Promise<GuildEmojiResponse> {
		return this.content.updateEmoji(params, auditLogReason);
	}

	async deleteEmoji(
		params: {userId: UserID; guildId: GuildID; emojiId: EmojiID; purge?: boolean},
		auditLogReason?: string | null,
	): Promise<void> {
		return this.content.deleteEmoji(params, auditLogReason);
	}

	async getStickers(params: {
		userId: UserID;
		guildId: GuildID;
		requestCache: RequestCache;
	}): Promise<Array<GuildStickerWithUserResponse>> {
		return this.content.getStickers(params);
	}

	async getStickerUser(params: {
		userId: UserID;
		guildId: GuildID;
		stickerId: StickerID;
		requestCache: RequestCache;
	}): Promise<UserPartialResponse> {
		return this.content.getStickerUser(params);
	}

	async createSticker(
		params: {
			user: User;
			guildId: GuildID;
			name: string;
			description?: string | null;
			tags: Array<string>;
			image: string;
		},
		auditLogReason?: string | null,
	): Promise<GuildStickerResponse> {
		return this.content.createSticker(params, auditLogReason);
	}

	async bulkCreateStickers(
		params: {
			user: User;
			guildId: GuildID;
			stickers: Array<{name: string; description?: string | null; tags: Array<string>; image: string}>;
		},
		auditLogReason?: string | null,
	): Promise<{
		success: Array<GuildStickerResponse>;
		failed: Array<{name: string; error: string}>;
	}> {
		return this.content.bulkCreateStickers(params, auditLogReason);
	}

	async updateSticker(
		params: {
			userId: UserID;
			guildId: GuildID;
			stickerId: StickerID;
			name: string;
			description?: string | null;
			tags: Array<string>;
		},
		auditLogReason?: string | null,
	): Promise<GuildStickerResponse> {
		return this.content.updateSticker(params, auditLogReason);
	}

	async deleteSticker(
		params: {userId: UserID; guildId: GuildID; stickerId: StickerID; purge?: boolean},
		auditLogReason?: string | null,
	): Promise<void> {
		return this.content.deleteSticker(params, auditLogReason);
	}

	async getChannels(params: {
		userId: UserID;
		guildId: GuildID;
		requestCache: RequestCache;
	}): Promise<Array<ChannelResponse>> {
		return this.channels.getChannels(params);
	}

	async createChannel(
		params: {userId: UserID; guildId: GuildID; data: ChannelCreateRequest; requestCache: RequestCache},
		auditLogReason?: string | null,
	): Promise<ChannelResponse> {
		return this.channels.createChannel(params, auditLogReason);
	}

	async updateChannelPositions(
		params: {
			userId: UserID;
			guildId: GuildID;
			updates: Array<{
				channelId: ChannelID;
				position?: number;
				parentId: ChannelID | null | undefined;
				lockPermissions: boolean;
			}>;
			requestCache: RequestCache;
		},
		auditLogReason?: string | null,
	): Promise<void> {
		return this.channels.updateChannelPositions(params, auditLogReason);
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
		audit_log_entries: Array<GuildAuditLogEntry>;
		users: Array<UserPartialResponse>;
		webhooks: Array<AuditLogWebhook>;
	}> {
		const {userId, guildId, requestCache, limit = 50, beforeLogId, afterLogId, filterUserId, actionType} = params;

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

		const effectiveLimit = Math.max(1, Math.min(limit, 100));
		const shouldBatch = actionType === undefined && !filterUserId;

		let processedLogs: Array<GuildAuditLog> = [];
		let currentBeforeLogId = beforeLogId;
		let currentAfterLogId = afterLogId;
		const maxIterations = 5;
		let iterations = 0;

		while (processedLogs.length < effectiveLimit && iterations < maxIterations) {
			iterations++;

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

	private mapAuditLogToEntry(log: GuildAuditLog): GuildAuditLogEntry {
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

	async searchMessages(params: {
		userId: UserID;
		guildId: GuildID;
		channelIds: Array<ChannelID>;
		searchParams: MessageSearchRequest;
		requestCache: RequestCache;
	}): Promise<MessageSearchResponse | {indexing: true}> {
		return this.search.searchMessages(params);
	}

	async searchAllGuilds(params: {
		userId: UserID;
		channelIds: Array<ChannelID>;
		searchParams: MessageSearchRequest;
		requestCache: RequestCache;
	}): Promise<MessageSearchResponse | {indexing: true}> {
		return this.search.searchAllGuilds(params);
	}

	async collectAccessibleGuildChannels(userId: UserID) {
		return this.search.collectAccessibleGuildChannels(userId);
	}

	async getGuildAuthenticated({userId, guildId}: {userId: UserID; guildId: GuildID}): Promise<GuildAuth> {
		const guildData = await this.gatewayService.getGuildData({guildId, userId});
		if (!guildData) throw new MissingAccessError();

		const checkPermission = async (permission: bigint) => {
			const hasPermission = await this.gatewayService.checkPermission({guildId, userId, permission});
			if (!hasPermission) throw new MissingPermissionsError();
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
