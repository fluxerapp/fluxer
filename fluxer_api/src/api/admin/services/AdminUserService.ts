// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import type {
	CancelBulkMessageDeletionRequest,
	ListUserChangeLogRequest,
	ListUserDmChannelsRequest,
	ListUserGroupDmChannelsRequest,
} from '@fluxer/schema/src/domains/admin/AdminUserSchemas';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import type Stripe from 'stripe';
import type {ApiContext} from '../../ApiContext';
import {createChannelID, createUserID, type UserID} from '../../BrandedTypes';
import type {IChannelRepository} from '../../channel/IChannelRepository';
import type {IGuildRepositoryAggregate} from '../../guild/repositories/IGuildRepositoryAggregate';
import type {IDiscriminatorService} from '../../infrastructure/DiscriminatorService';
import type {EntityAssetService} from '../../infrastructure/EntityAssetService';
import type {KVAccountDeletionQueueService} from '../../infrastructure/KVAccountDeletionQueueService';
import type {KVBulkMessageDeletionQueueService} from '../../infrastructure/KVBulkMessageDeletionQueueService';
import type {UserCacheService} from '../../infrastructure/UserCacheService';
import {getBillingRepository} from '../../middleware/ServiceRegistry';
import type {ReportService} from '../../report/ReportService';
import type {IRiskHistoryRepository} from '../../risk/HistoricalOutcomeRepository';
import type {IUserRepository} from '../../user/IUserRepository';
import type {UserContactChangeLogService} from '../../user/services/UserContactChangeLogService';
import {mapUserToAdminResponse} from '../models/UserTypes';
import type {AdminAuditService} from './AdminAuditService';
import type {AdminBanManagementService} from './AdminBanManagementService';
import {AdminUserBanService} from './AdminUserBanService';
import {AdminUserDeletionService} from './AdminUserDeletionService';
import {AdminUserLookupService} from './AdminUserLookupService';
import {AdminUserProfileService} from './AdminUserProfileService';
import {AdminUserSecurityService} from './AdminUserSecurityService';
import {AdminUserUpdatePropagator} from './AdminUserUpdatePropagator';

interface AdminUserServiceDeps {
	apiContext: ApiContext;
	guildRepository: IGuildRepositoryAggregate;
	channelRepository: IChannelRepository;
	discriminatorService: IDiscriminatorService;
	entityAssetService: EntityAssetService;
	auditService: AdminAuditService;
	userCacheService: UserCacheService;
	banManagementService: AdminBanManagementService;
	kvDeletionQueue: KVAccountDeletionQueueService;
	bulkMessageDeletionQueue: KVBulkMessageDeletionQueueService;
	stripe: Stripe | null;
	riskHistoryRepository: Pick<IRiskHistoryRepository, 'recordOutcomeForUser'>;
	reportService: ReportService;
}

export class AdminUserService {
	readonly lookupService: AdminUserLookupService;
	readonly profileService: AdminUserProfileService;
	readonly securityService: AdminUserSecurityService;
	readonly banService: AdminUserBanService;
	readonly deletionService: AdminUserDeletionService;
	private readonly updatePropagator: AdminUserUpdatePropagator;
	private readonly contactChangeLogService: UserContactChangeLogService;
	private readonly auditService: AdminAuditService;
	private readonly userRepository: IUserRepository;
	private readonly guildRepository: IGuildRepositoryAggregate;
	private readonly channelRepository: IChannelRepository;
	private readonly bulkMessageDeletionQueue: KVBulkMessageDeletionQueueService;
	private readonly cacheService: ICacheService;
	private readonly gatewayService: ApiContext['services']['gateway'];

	constructor(deps: AdminUserServiceDeps) {
		const {users, cache, gateway, contactChangeLog} = deps.apiContext.services;
		this.updatePropagator = new AdminUserUpdatePropagator({
			userCacheService: deps.userCacheService,
			userRepository: users,
			guildRepository: deps.guildRepository,
			gatewayService: gateway,
		});
		this.userRepository = users;
		this.guildRepository = deps.guildRepository;
		this.channelRepository = deps.channelRepository;
		this.auditService = deps.auditService;
		this.bulkMessageDeletionQueue = deps.bulkMessageDeletionQueue;
		this.cacheService = cache;
		this.gatewayService = gateway;
		this.lookupService = new AdminUserLookupService({
			apiContext: deps.apiContext,
		});
		this.profileService = new AdminUserProfileService({
			apiContext: deps.apiContext,
			discriminatorService: deps.discriminatorService,
			entityAssetService: deps.entityAssetService,
			auditService: deps.auditService,
			updatePropagator: this.updatePropagator,
			guildRepository: deps.guildRepository,
		});
		this.securityService = new AdminUserSecurityService({
			apiContext: deps.apiContext,
			auditService: deps.auditService,
			updatePropagator: this.updatePropagator,
			riskHistoryRepository: deps.riskHistoryRepository,
		});
		this.banService = new AdminUserBanService({
			apiContext: deps.apiContext,
			auditService: deps.auditService,
			updatePropagator: this.updatePropagator,
		});
		this.deletionService = new AdminUserDeletionService({
			apiContext: deps.apiContext,
			auditService: deps.auditService,
			banManagementService: deps.banManagementService,
			reportService: deps.reportService,
			updatePropagator: this.updatePropagator,
			kvDeletionQueue: deps.kvDeletionQueue,
			stripe: deps.stripe,
			billingRepository: getBillingRepository(),
		});
		this.contactChangeLogService = contactChangeLog;
	}

	async listUserDmChannels(data: ListUserDmChannelsRequest) {
		const userId = createUserID(data.user_id);
		const user = await this.userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		const channels = await this.userRepository.listHistoricalDmChannelsPaginated(userId, {
			limit: data.limit,
			beforeChannelId: data.before ? createChannelID(data.before) : undefined,
			afterChannelId: data.after ? createChannelID(data.after) : undefined,
		});
		const channelModels = await this.channelRepository.listChannels(channels.map((channel) => channel.channelId));
		const channelModelById = new Map(
			channelModels.filter((c): c is NonNullable<typeof c> => c != null).map((c) => [c.id, c]),
		);
		const guildIds = channelModels
			.map((channel) => channel?.guildId)
			.filter((guildId): guildId is NonNullable<typeof guildId> => guildId !== null && guildId !== undefined);
		const guilds = await this.guildRepository.listGuilds(guildIds);
		const guildNsfwLevels = new Map(guilds.map((guild) => [guild.id.toString(), guild.nsfwLevel]));
		const channelNsfwById = new Map(channelModels.map((channel) => [channel?.id.toString(), channel?.isNsfw ?? null]));
		const channelGuildIdById = new Map(
			channelModels.map((channel) => [channel?.id.toString(), channel?.guildId ?? null]),
		);
		const allRecipientIds = new Set<UserID>();
		for (const channel of channels) {
			for (const recipientId of channel.recipientIds) {
				allRecipientIds.add(recipientId);
			}
		}
		for (const model of channelModels) {
			if (model?.ownerId) {
				allRecipientIds.add(model.ownerId);
			}
		}
		const resolvedUsers = await this.resolveUsers([...allRecipientIds]);
		return {
			channels: channels.map((channel) => {
				const model = channelModelById.get(channel.channelId);
				return {
					channel_id: channel.channelId.toString(),
					channel_type: channel.channelType,
					channel_nsfw: channelNsfwById.get(channel.channelId.toString()) ?? null,
					guild_nsfw_level: (() => {
						const guildId = channelGuildIdById.get(channel.channelId.toString());
						if (!guildId) {
							return null;
						}
						return guildNsfwLevels.get(guildId.toString()) ?? null;
					})(),
					recipient_ids: channel.recipientIds.map((recipientId) => recipientId.toString()),
					recipients: channel.recipientIds
						.map((recipientId) => resolvedUsers.get(recipientId))
						.filter((u): u is NonNullable<typeof u> => u != null),
					last_message_id: channel.lastMessageId?.toString() ?? null,
					is_open: channel.open,
					name: model?.name ?? null,
					icon: model?.iconHash ?? null,
					owner_id: model?.ownerId?.toString() ?? null,
				};
			}),
		};
	}

	private async resolveUsers(userIds: Array<UserID>): Promise<
		Map<
			UserID,
			{
				id: string;
				username: string;
				discriminator: string;
				global_name: string | null;
				avatar: string | null;
			}
		>
	> {
		const results = new Map<
			UserID,
			{
				id: string;
				username: string;
				discriminator: string;
				global_name: string | null;
				avatar: string | null;
			}
		>();
		const users = await Promise.all(userIds.map((id) => this.userRepository.findUnique(id)));
		for (let i = 0; i < userIds.length; i++) {
			const user = users[i];
			if (user) {
				results.set(userIds[i], {
					id: user.id.toString(),
					username: user.username,
					discriminator: String(user.discriminator).padStart(4, '0'),
					global_name: user.globalName,
					avatar: user.avatarHash,
				});
			}
		}
		return results;
	}

	async listUserGroupDmChannels(data: ListUserGroupDmChannelsRequest) {
		const userId = createUserID(data.user_id);
		const user = await this.userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		const summaries = await this.userRepository.listPrivateChannelSummaries(userId);
		const groupDmSummaries = summaries.filter((s) => s.isGroupDm);
		if (groupDmSummaries.length === 0) {
			return {channels: []};
		}
		const channelModels = await this.channelRepository.listChannels(groupDmSummaries.map((s) => s.channelId));
		const allUserIds = new Set<UserID>();
		for (const model of channelModels) {
			if (model) {
				for (const recipientId of model.recipientIds) {
					allUserIds.add(recipientId);
				}
				if (model.ownerId) {
					allUserIds.add(model.ownerId);
				}
			}
		}
		const resolvedUsers = await this.resolveUsers([...allUserIds]);
		return {
			channels: groupDmSummaries.map((summary, i) => {
				const model = channelModels[i];
				const recipientIds = model ? [...model.recipientIds] : [];
				return {
					channel_id: summary.channelId.toString(),
					channel_type: summary.channelType,
					channel_nsfw: model?.isNsfw ?? null,
					guild_nsfw_level: null,
					recipient_ids: recipientIds.map((id) => id.toString()),
					recipients: recipientIds
						.map((id) => resolvedUsers.get(id))
						.filter((u): u is NonNullable<typeof u> => u != null),
					last_message_id: summary.lastMessageId?.toString() ?? null,
					is_open: summary.open,
					name: model?.name ?? null,
					icon: model?.iconHash ?? null,
					owner_id: model?.ownerId?.toString() ?? null,
				};
			}),
		};
	}

	async listUserChangeLog(data: ListUserChangeLogRequest, acls: ReadonlySet<string>) {
		if (!acls.has(AdminACLs.USER_VIEW_CONTACT_LOG) && !acls.has(AdminACLs.WILDCARD)) {
			return {entries: [], next_page_token: null};
		}
		const canViewEmail = acls.has(AdminACLs.USER_VIEW_EMAIL) || acls.has(AdminACLs.WILDCARD);
		const entries = await this.contactChangeLogService.listLogs({
			userId: createUserID(data.user_id),
			limit: data.limit,
			beforeEventId: data.page_token,
		});
		const lastEntry = entries.length === data.limit && entries.length > 0 ? entries.at(-1) : null;
		const nextPageToken = lastEntry?.event_id != null ? lastEntry.event_id.toString() : null;
		return {
			entries: entries.map((entry) => {
				const isEmailField = entry.field === 'email';
				const shouldRedact = isEmailField && !canViewEmail;
				return {
					event_id: entry.event_id != null ? entry.event_id.toString() : String(entry.user_id),
					field: entry.field,
					old_value: shouldRedact ? '[redacted]' : (entry.old_value ?? null),
					new_value: shouldRedact ? '[redacted]' : (entry.new_value ?? null),
					reason: entry.reason ?? null,
					actor_user_id: entry.actor_user_id ? entry.actor_user_id.toString() : null,
					event_at: entry.event_at.toISOString(),
				};
			}),
			next_page_token: nextPageToken,
		};
	}

	async cancelBulkMessageDeletion(
		data: CancelBulkMessageDeletionRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
		acls: ReadonlySet<string>,
	) {
		const userId = createUserID(data.user_id);
		const user = await this.userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		const updatedUser = await this.userRepository.patchUpsert(
			userId,
			{
				pending_bulk_message_deletion_at: null,
				pending_bulk_message_deletion_channel_count: null,
				pending_bulk_message_deletion_message_count: null,
			},
			user.toRow(),
		);
		await this.bulkMessageDeletionQueue.removeFromQueue(userId);
		await this.auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'cancel_bulk_message_deletion',
			auditLogReason,
			metadata: new Map(),
		});
		return {
			user: await mapUserToAdminResponse(updatedUser, this.cacheService, acls, this.gatewayService),
		};
	}
}
