// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID, UserID} from '@app/api/BrandedTypes';
import {mapChannelToResponse} from '@app/api/channel/ChannelMappers';
import type {IChannelRepository} from '@app/api/channel/IChannelRepository';
import type {GuildAuditLogService} from '@app/api/guild/GuildAuditLogService';
import type {IGuildRepositoryAggregate} from '@app/api/guild/repositories/IGuildRepositoryAggregate';
import {ChannelOperationsService} from '@app/api/guild/services/channel/ChannelOperationsService';
import {createGuildMfaEnforcer} from '@app/api/guild/services/GuildMfaEnforcement';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {ISnowflakeService} from '@app/api/infrastructure/ISnowflakeService';
import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import type {LimitConfigService} from '@app/api/limits/LimitConfigService';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {IUserRepository} from '@app/api/user/IUserRepository';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {MissingPermissionsError} from '@fluxer/errors/src/domains/core/MissingPermissionsError';
import {UnknownGuildError} from '@fluxer/errors/src/domains/guild/UnknownGuildError';
import type {ChannelCreateRequest} from '@fluxer/schema/src/domains/channel/ChannelRequestSchemas';
import type {ChannelResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';

export class GuildChannelService {
	private readonly channelOps: ChannelOperationsService;

	constructor(
		private readonly channelRepository: IChannelRepository,
		guildRepository: IGuildRepositoryAggregate,
		private readonly userCacheService: UserCacheService,
		private readonly gatewayService: IGatewayService,
		cacheService: ICacheService,
		snowflakeService: ISnowflakeService,
		guildAuditLogService: GuildAuditLogService,
		limitConfigService: LimitConfigService,
		private readonly userRepository: IUserRepository,
	) {
		this.channelOps = new ChannelOperationsService(
			channelRepository,
			guildRepository,
			userCacheService,
			gatewayService,
			cacheService,
			snowflakeService,
			guildAuditLogService,
			limitConfigService,
		);
	}

	async getChannels(params: {
		userId: UserID;
		guildId: GuildID;
		requestCache: RequestCache;
	}): Promise<Array<ChannelResponse>> {
		try {
			await this.gatewayService.getGuildData({guildId: params.guildId, userId: params.userId});
		} catch (error) {
			if (error instanceof UnknownGuildError) {
				throw error;
			}
			throw error;
		}
		const viewableChannelIds = await this.gatewayService.getViewableChannels({
			guildId: params.guildId,
			userId: params.userId,
		});
		const channels = await this.channelRepository.listGuildChannels(params.guildId);
		const viewableChannels = channels.filter((channel) => viewableChannelIds.includes(channel.id));
		return Promise.all(
			viewableChannels.map((channel) => {
				return mapChannelToResponse({
					channel,
					currentUserId: null,
					userCacheService: this.userCacheService,
					requestCache: params.requestCache,
				});
			}),
		);
	}

	async createChannel(
		params: {
			userId: UserID;
			guildId: GuildID;
			data: ChannelCreateRequest;
			requestCache: RequestCache;
		},
		auditLogReason?: string | null,
	): Promise<ChannelResponse> {
		await this.checkPermission({
			userId: params.userId,
			guildId: params.guildId,
			permission: Permissions.MANAGE_CHANNELS,
		});
		return this.channelOps.createChannel(params, auditLogReason);
	}

	async updateChannelPositions(
		params: {
			userId: UserID;
			guildId: GuildID;
			updates: Array<{
				channelId: ChannelID;
				position?: number;
				parentId: ChannelID | null | undefined;
				precedingSiblingId: ChannelID | null | undefined;
				lockPermissions: boolean;
			}>;
			requestCache: RequestCache;
		},
		auditLogReason?: string | null,
	): Promise<void> {
		await this.checkPermission({
			userId: params.userId,
			guildId: params.guildId,
			permission: Permissions.MANAGE_CHANNELS,
		});
		await this.channelOps.updateChannelPositionsByList({
			userId: params.userId,
			guildId: params.guildId,
			updates: params.updates,
			requestCache: params.requestCache,
			auditLogReason: auditLogReason ?? null,
		});
	}

	async sanitizeTextChannelNames(params: {guildId: GuildID; requestCache: RequestCache}): Promise<void> {
		await this.channelOps.sanitizeTextChannelNames(params);
	}

	private async checkPermission(params: {userId: UserID; guildId: GuildID; permission: bigint}): Promise<void> {
		const hasPermission = await this.gatewayService.checkPermission({
			guildId: params.guildId,
			userId: params.userId,
			permission: params.permission,
		});
		if (!hasPermission) throw new MissingPermissionsError();
		const guildData = await this.gatewayService.getGuildData({guildId: params.guildId, userId: params.userId});
		const enforceGuildMfa = await createGuildMfaEnforcer({
			userRepository: this.userRepository,
			guildData,
			userId: params.userId,
		});
		enforceGuildMfa(params.permission);
	}
}
