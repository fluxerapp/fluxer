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

import {type ChannelID, createUserID, type UserID} from '~/BrandedTypes';
import {ChannelTypes} from '~/Constants';
import {
	type ChannelPartialResponse,
	type ChannelUpdateRequest,
	mapChannelToPartialResponse,
} from '~/channel/ChannelModel';
import type {GuildAuditLogService} from '~/guild/GuildAuditLogService';
import type {IGuildRepository} from '~/guild/IGuildRepository';
import type {AvatarService} from '~/infrastructure/AvatarService';
import type {ICloudflarePurgeQueue} from '~/infrastructure/CloudflarePurgeQueue';
import type {IGatewayService} from '~/infrastructure/IGatewayService';
import type {ILiveKitService} from '~/infrastructure/ILiveKitService';
import type {IMediaService} from '~/infrastructure/IMediaService';
import type {IStorageService} from '~/infrastructure/IStorageService';
import type {IVoiceRoomStore} from '~/infrastructure/IVoiceRoomStore';
import type {SnowflakeService} from '~/infrastructure/SnowflakeService';
import type {UserCacheService} from '~/infrastructure/UserCacheService';
import type {IInviteRepository} from '~/invite/IInviteRepository';
import type {Channel} from '~/Models';
import type {RequestCache} from '~/middleware/RequestCacheMiddleware';
import type {IUserRepository} from '~/user/IUserRepository';
import type {VoiceAvailabilityService} from '~/voice/VoiceAvailabilityService';
import type {VoiceRegionAvailability} from '~/voice/VoiceModel';
import type {IWebhookRepository} from '~/webhook/IWebhookRepository';
import type {IChannelRepositoryAggregate} from '../repositories/IChannelRepositoryAggregate';
import type {AuthenticatedChannel} from './AuthenticatedChannel';
import {ChannelAuthService} from './channel_data/ChannelAuthService';
import {ChannelOperationsService, type ChannelUpdateData} from './channel_data/ChannelOperationsService';
import {ChannelUtilsService} from './channel_data/ChannelUtilsService';
import {GroupDmUpdateService} from './channel_data/GroupDmUpdateService';
import type {MessagePersistenceService} from './message/MessagePersistenceService';

type GuildChannelUpdateRequest = Exclude<ChannelUpdateRequest, {type: typeof ChannelTypes.GROUP_DM}>;
type GuildChannelUpdatePayload = Omit<GuildChannelUpdateRequest, 'type'>;

export class ChannelDataService {
	private channelAuthService: ChannelAuthService;
	private channelOperationsService: ChannelOperationsService;
	public readonly groupDmUpdateService: GroupDmUpdateService;
	private channelUtilsService: ChannelUtilsService;

	constructor(
		channelRepository: IChannelRepositoryAggregate,
		userRepository: IUserRepository,
		guildRepository: IGuildRepository,
		userCacheService: UserCacheService,
		storageService: IStorageService,
		gatewayService: IGatewayService,
		mediaService: IMediaService,
		avatarService: AvatarService,
		snowflakeService: SnowflakeService,
		cloudflarePurgeQueue: ICloudflarePurgeQueue,
		voiceRoomStore: IVoiceRoomStore,
		liveKitService: ILiveKitService,
		voiceAvailabilityService: VoiceAvailabilityService | undefined,
		messagePersistenceService: MessagePersistenceService,
		guildAuditLogService: GuildAuditLogService,
		inviteRepository: IInviteRepository,
		webhookRepository: IWebhookRepository,
	) {
		this.channelUtilsService = new ChannelUtilsService(
			channelRepository,
			userCacheService,
			storageService,
			gatewayService,
			cloudflarePurgeQueue,
			mediaService,
		);

		this.channelAuthService = new ChannelAuthService(
			channelRepository,
			userRepository,
			guildRepository,
			gatewayService,
		);

		this.channelOperationsService = new ChannelOperationsService(
			channelRepository,
			userRepository,
			gatewayService,
			this.channelAuthService,
			this.channelUtilsService,
			voiceRoomStore,
			liveKitService,
			voiceAvailabilityService,
			guildAuditLogService,
			inviteRepository,
			webhookRepository,
		);

		this.groupDmUpdateService = new GroupDmUpdateService(
			channelRepository,
			avatarService,
			snowflakeService,
			this.channelUtilsService,
			messagePersistenceService,
		);
	}

	async getChannel({userId, channelId}: {userId: UserID; channelId: ChannelID}): Promise<Channel> {
		return this.channelOperationsService.getChannel({userId, channelId});
	}

	async getChannelAuthenticated({
		userId,
		channelId,
	}: {
		userId: UserID;
		channelId: ChannelID;
	}): Promise<AuthenticatedChannel> {
		return this.channelAuthService.getChannelAuthenticated({userId, channelId});
	}

	async getPublicChannelData(channelId: ChannelID): Promise<ChannelPartialResponse> {
		const channel = await this.channelOperationsService.getPublicChannelData(channelId);
		return mapChannelToPartialResponse(channel);
	}

	async getChannelMemberCount(channelId: ChannelID): Promise<number> {
		return this.channelOperationsService.getChannelMemberCount(channelId);
	}

	async getChannelSystem(channelId: ChannelID): Promise<Channel | null> {
		return this.channelOperationsService.getChannelSystem(channelId);
	}

	async editChannel({
		userId,
		channelId,
		data,
		requestCache,
	}: {
		userId: UserID;
		channelId: ChannelID;
		data: Omit<ChannelUpdateRequest, 'type'>;
		requestCache: RequestCache;
	}): Promise<Channel> {
		const {channel} = await this.channelAuthService.getChannelAuthenticated({userId, channelId});

		if (channel.type === ChannelTypes.GROUP_DM) {
			return await this.updateGroupDmChannel({
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

		if (guildChannelData.nsfw !== undefined) {
			channelUpdateData.nsfw = guildChannelData.nsfw ?? undefined;
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

		return this.channelOperationsService.editChannel({userId, channelId, data: channelUpdateData, requestCache});
	}

	async deleteChannel({
		userId,
		channelId,
		requestCache,
	}: {
		userId: UserID;
		channelId: ChannelID;
		requestCache: RequestCache;
	}): Promise<void> {
		return this.channelOperationsService.deleteChannel({userId, channelId, requestCache});
	}

	async getAvailableRtcRegions({
		userId,
		channelId,
	}: {
		userId: UserID;
		channelId: ChannelID;
	}): Promise<Array<VoiceRegionAvailability>> {
		return this.channelOperationsService.getAvailableRtcRegions({userId, channelId});
	}

	async updateGroupDmChannel({
		userId,
		channelId,
		name,
		icon,
		ownerId,
		nicks,
		requestCache,
	}: {
		userId: UserID;
		channelId: ChannelID;
		name?: string | null;
		icon?: string | null;
		ownerId?: UserID;
		nicks?: Record<string, string | null> | null;
		requestCache: RequestCache;
	}): Promise<Channel> {
		return this.groupDmUpdateService.updateGroupDmChannel({
			userId,
			channelId,
			name,
			icon,
			ownerId,
			nicks,
			requestCache,
		});
	}

	async setChannelPermissionOverwrite(params: {
		userId: UserID;
		channelId: ChannelID;
		overwriteId: bigint;
		overwrite: {type: number; allow_: bigint; deny_: bigint};
		requestCache: RequestCache;
	}) {
		return this.channelOperationsService.setChannelPermissionOverwrite(params);
	}

	async deleteChannelPermissionOverwrite(params: {
		userId: UserID;
		channelId: ChannelID;
		overwriteId: bigint;
		requestCache: RequestCache;
	}) {
		return this.channelOperationsService.deleteChannelPermissionOverwrite(params);
	}
}
