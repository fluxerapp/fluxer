// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, UserID} from '@app/api/BrandedTypes';
import {createUserID} from '@app/api/BrandedTypes';
import type {IChannelRepositoryAggregate} from '@app/api/channel/repositories/IChannelRepositoryAggregate';
import {ChannelAuthService} from '@app/api/channel/services/channel_data/ChannelAuthService';
import type {ChannelUpdateData} from '@app/api/channel/services/channel_data/ChannelOperationsService';
import {ChannelOperationsService} from '@app/api/channel/services/channel_data/ChannelOperationsService';
import {ChannelUtilsService} from '@app/api/channel/services/channel_data/ChannelUtilsService';
import {GroupDmUpdateService} from '@app/api/channel/services/channel_data/GroupDmUpdateService';
import type {MessagePersistenceService} from '@app/api/channel/services/message/MessagePersistenceService';
import type {GuildAuditLogService} from '@app/api/guild/GuildAuditLogService';
import type {IGuildRepositoryAggregate} from '@app/api/guild/repositories/IGuildRepositoryAggregate';
import type {AvatarService} from '@app/api/infrastructure/AvatarService';
import type {IPurgeQueue} from '@app/api/infrastructure/CachePurgeQueue';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {ILiveKitService} from '@app/api/infrastructure/ILiveKitService';
import type {ISnowflakeService} from '@app/api/infrastructure/ISnowflakeService';
import type {IStorageService} from '@app/api/infrastructure/IStorageService';
import type {IVoiceRoomStore} from '@app/api/infrastructure/IVoiceRoomStore';
import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import type {IInviteRepository} from '@app/api/invite/IInviteRepository';
import type {LimitConfigService} from '@app/api/limits/LimitConfigService';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {Channel} from '@app/api/models/Channel';
import type {IUserRepository} from '@app/api/user/IUserRepository';
import type {VoiceAvailabilityService} from '@app/api/voice/VoiceAvailabilityService';
import type {IWebhookRepository} from '@app/api/webhook/IWebhookRepository';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import type {ChannelUpdateRequest} from '@fluxer/schema/src/domains/channel/ChannelRequestSchemas';
import type {IRateLimitService} from '@pkgs/rate_limit/src/IRateLimitService';

type GuildChannelUpdateRequest = Exclude<
	ChannelUpdateRequest,
	{
		type: typeof ChannelTypes.GROUP_DM;
	}
>;
type GuildChannelUpdatePayload = Omit<GuildChannelUpdateRequest, 'type'>;

export class ChannelDataService {
	public readonly auth: ChannelAuthService;
	public readonly operations: ChannelOperationsService;
	public readonly groupDmUpdate: GroupDmUpdateService;
	public readonly utils: ChannelUtilsService;

	constructor(
		channelRepository: IChannelRepositoryAggregate,
		userRepository: IUserRepository,
		guildRepository: IGuildRepositoryAggregate,
		userCacheService: UserCacheService,
		storageService: IStorageService,
		gatewayService: IGatewayService,
		avatarService: AvatarService,
		snowflakeService: ISnowflakeService,
		purgeQueue: IPurgeQueue,
		voiceRoomStore: IVoiceRoomStore,
		liveKitService: ILiveKitService,
		voiceAvailabilityService: VoiceAvailabilityService | null,
		messagePersistenceService: MessagePersistenceService,
		guildAuditLogService: GuildAuditLogService,
		inviteRepository: IInviteRepository,
		webhookRepository: IWebhookRepository,
		limitConfigService: LimitConfigService,
		rateLimitService: IRateLimitService,
	) {
		this.utils = new ChannelUtilsService(
			channelRepository,
			userCacheService,
			storageService,
			gatewayService,
			purgeQueue,
		);
		this.auth = new ChannelAuthService(channelRepository, userRepository, guildRepository, gatewayService);
		this.operations = new ChannelOperationsService(
			channelRepository,
			userRepository,
			gatewayService,
			this.auth,
			this.utils,
			voiceRoomStore,
			liveKitService,
			voiceAvailabilityService,
			guildAuditLogService,
			inviteRepository,
			webhookRepository,
			guildRepository,
			limitConfigService,
			rateLimitService,
		);
		this.groupDmUpdate = new GroupDmUpdateService(
			channelRepository,
			userRepository,
			avatarService,
			snowflakeService,
			this.utils,
			messagePersistenceService,
		);
	}

	async editChannel({
		userId,
		channelId,
		data,
		clientFeatures,
		requestCache,
		auditLogReason,
	}: {
		userId: UserID;
		channelId: ChannelID;
		data: Omit<ChannelUpdateRequest, 'type'>;
		clientFeatures: ReadonlySet<string>;
		requestCache: RequestCache;
		auditLogReason: string | null;
	}): Promise<Channel> {
		const {channel} = await this.auth.getChannelAuthenticated({userId, channelId, skipNsfwValidation: true});
		if (channel.type === ChannelTypes.GROUP_DM) {
			return await this.groupDmUpdate.updateGroupDmChannel({
				userId,
				channelId,
				name: data.name !== undefined ? data.name : undefined,
				icon: data.icon !== undefined ? data.icon : undefined,
				ownerId: data.owner_id ? createUserID(data.owner_id) : undefined,
				nicks: data.nicks,
				requestCache,
			});
		}
		const guildChannelData = data as GuildChannelUpdatePayload;
		const channelUpdateData: ChannelUpdateData = {};
		if ('name' in guildChannelData && guildChannelData.name !== undefined && guildChannelData.name !== null) {
			channelUpdateData.name = guildChannelData.name;
		}
		if (guildChannelData.topic !== undefined) {
			channelUpdateData.topic = guildChannelData.topic ?? null;
		}
		if (guildChannelData.url !== undefined) {
			channelUpdateData.url = guildChannelData.url ?? null;
		}
		if (guildChannelData.parent_id !== undefined) {
			channelUpdateData.parent_id = guildChannelData.parent_id ?? null;
		}
		if (guildChannelData.bitrate !== undefined) {
			channelUpdateData.bitrate = guildChannelData.bitrate ?? null;
		}
		if (guildChannelData.user_limit !== undefined) {
			channelUpdateData.user_limit = guildChannelData.user_limit ?? null;
		}
		if (guildChannelData.voice_connection_limit !== undefined) {
			channelUpdateData.voice_connection_limit = guildChannelData.voice_connection_limit ?? null;
		}
		if (guildChannelData.nsfw !== undefined) {
			channelUpdateData.nsfw = guildChannelData.nsfw ?? undefined;
		}
		if (guildChannelData.nsfw_override !== undefined) {
			channelUpdateData.nsfw_override = guildChannelData.nsfw_override ?? null;
		}
		if (guildChannelData.content_warning_level !== undefined) {
			channelUpdateData.content_warning_level = guildChannelData.content_warning_level;
		}
		if (guildChannelData.content_warning_text !== undefined) {
			channelUpdateData.content_warning_text = guildChannelData.content_warning_text ?? null;
		}
		if (guildChannelData.rate_limit_per_user !== undefined) {
			channelUpdateData.rate_limit_per_user = guildChannelData.rate_limit_per_user ?? undefined;
		}
		if (guildChannelData.permission_overwrites !== undefined) {
			channelUpdateData.permission_overwrites = guildChannelData.permission_overwrites ?? null;
		}
		if (guildChannelData.rtc_region !== undefined) {
			channelUpdateData.rtc_region = guildChannelData.rtc_region ?? null;
		}
		if (guildChannelData.icon !== undefined) {
			channelUpdateData.icon = guildChannelData.icon ?? null;
		}
		if (guildChannelData.owner_id !== undefined) {
			channelUpdateData.owner_id = guildChannelData.owner_id ?? null;
		}
		if (guildChannelData.nicks !== undefined) {
			channelUpdateData.nicks = guildChannelData.nicks ?? null;
		}
		return this.operations.editChannel({
			userId,
			channelId,
			data: channelUpdateData,
			clientFeatures,
			requestCache,
			auditLogReason,
		});
	}
}
