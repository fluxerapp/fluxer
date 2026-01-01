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

import type {ChannelID, GuildID, UserID} from '~/BrandedTypes';
import {Permissions} from '~/Constants';
import type {ChannelCreateRequest, ChannelResponse} from '~/channel/ChannelModel';
import {mapChannelToResponse} from '~/channel/ChannelModel';
import type {IChannelRepository} from '~/channel/IChannelRepository';
import {MissingPermissionsError} from '~/Errors';
import type {ICacheService} from '~/infrastructure/ICacheService';
import type {IGatewayService} from '~/infrastructure/IGatewayService';
import type {SnowflakeService} from '~/infrastructure/SnowflakeService';
import type {UserCacheService} from '~/infrastructure/UserCacheService';
import type {RequestCache} from '~/middleware/RequestCacheMiddleware';
import type {GuildAuditLogService} from '../GuildAuditLogService';
import {ChannelOperationsService} from './channel/ChannelOperationsService';

export class GuildChannelService {
	private readonly channelOps: ChannelOperationsService;

	constructor(
		private readonly channelRepository: IChannelRepository,
		private readonly userCacheService: UserCacheService,
		private readonly gatewayService: IGatewayService,
		cacheService: ICacheService,
		snowflakeService: SnowflakeService,
		guildAuditLogService: GuildAuditLogService,
	) {
		this.channelOps = new ChannelOperationsService(
			channelRepository,
			userCacheService,
			gatewayService,
			cacheService,
			snowflakeService,
			guildAuditLogService,
		);
	}

	async getChannels(params: {
		userId: UserID;
		guildId: GuildID;
		requestCache: RequestCache;
	}): Promise<Array<ChannelResponse>> {
		await this.gatewayService.getGuildData({guildId: params.guildId, userId: params.userId});
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
		params: {userId: UserID; guildId: GuildID; data: ChannelCreateRequest; requestCache: RequestCache},
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
	}
}
