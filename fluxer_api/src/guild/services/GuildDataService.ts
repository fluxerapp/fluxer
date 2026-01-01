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

import type {GuildID, UserID} from '~/BrandedTypes';
import type {IChannelRepository} from '~/channel/IChannelRepository';
import type {ChannelService} from '~/channel/services/ChannelService';
import type {
	GuildCreateRequest,
	GuildPartialResponse,
	GuildResponse,
	GuildUpdateRequest,
	GuildVanityURLResponse,
} from '~/guild/GuildModel';
import type {EntityAssetService} from '~/infrastructure/EntityAssetService';
import type {IGatewayService} from '~/infrastructure/IGatewayService';
import type {SnowflakeService} from '~/infrastructure/SnowflakeService';
import type {InviteRepository} from '~/invite/InviteRepository';
import type {Guild, GuildMember, User} from '~/Models';
import type {RequestCache} from '~/middleware/RequestCacheMiddleware';
import type {IUserRepository} from '~/user/IUserRepository';
import type {IWebhookRepository} from '~/webhook/IWebhookRepository';
import type {GuildAuditLogService} from '../GuildAuditLogService';
import type {IGuildRepository} from '../IGuildRepository';
import {GuildDataHelpers} from './data/GuildDataHelpers';
import {GuildOperationsService} from './data/GuildOperationsService';
import {GuildOwnershipService} from './data/GuildOwnershipService';
import {GuildVanityService} from './data/GuildVanityService';

export class GuildDataService {
	private readonly helpers: GuildDataHelpers;
	private readonly operationsService: GuildOperationsService;
	private readonly vanityService: GuildVanityService;
	private readonly ownershipService: GuildOwnershipService;

	constructor(
		private readonly guildRepository: IGuildRepository,
		private readonly channelRepository: IChannelRepository,
		private readonly inviteRepository: InviteRepository,
		private readonly channelService: ChannelService,
		private readonly gatewayService: IGatewayService,
		private readonly entityAssetService: EntityAssetService,
		private readonly userRepository: IUserRepository,
		private readonly snowflakeService: SnowflakeService,
		private readonly webhookRepository: IWebhookRepository,
		private readonly guildAuditLogService: GuildAuditLogService,
	) {
		this.helpers = new GuildDataHelpers(this.gatewayService, this.guildAuditLogService);

		this.operationsService = new GuildOperationsService(
			this.guildRepository,
			this.channelRepository,
			this.inviteRepository,
			this.channelService,
			this.gatewayService,
			this.entityAssetService,
			this.userRepository,
			this.snowflakeService,
			this.webhookRepository,
			this.helpers,
		);

		this.vanityService = new GuildVanityService(this.guildRepository, this.inviteRepository, this.helpers);

		this.ownershipService = new GuildOwnershipService(this.guildRepository, this.userRepository, this.helpers);
	}

	async getGuild({userId, guildId}: {userId: UserID; guildId: GuildID}): Promise<GuildResponse> {
		return this.operationsService.getGuild({userId, guildId});
	}

	async getUserGuilds(userId: UserID): Promise<Array<GuildResponse>> {
		return this.operationsService.getUserGuilds(userId);
	}

	async getPublicGuildData(guildId: GuildID): Promise<GuildPartialResponse> {
		return this.operationsService.getPublicGuildData(guildId);
	}

	async getGuildSystem(guildId: GuildID): Promise<Guild> {
		return this.operationsService.getGuildSystem(guildId);
	}

	async createGuild(
		params: {user: User; data: GuildCreateRequest},
		auditLogReason?: string | null,
	): Promise<GuildResponse> {
		return this.operationsService.createGuild(params, auditLogReason);
	}

	async updateGuild(
		params: {userId: UserID; guildId: GuildID; data: GuildUpdateRequest; requestCache: RequestCache},
		auditLogReason?: string | null,
	): Promise<GuildResponse> {
		return this.operationsService.updateGuild(params, auditLogReason);
	}

	async getVanityURL(params: {userId: UserID; guildId: GuildID}): Promise<GuildVanityURLResponse> {
		return this.vanityService.getVanityURL(params);
	}

	async updateVanityURL(
		params: {userId: UserID; guildId: GuildID; code: string | null; requestCache: RequestCache},
		auditLogReason?: string | null,
	): Promise<{code: string}> {
		return this.vanityService.updateVanityURL(params, auditLogReason);
	}

	async deleteGuild(params: {user: User; guildId: GuildID}, auditLogReason?: string | null): Promise<void> {
		return this.operationsService.deleteGuild(params, auditLogReason);
	}

	async deleteGuildForAdmin(guildId: GuildID, _auditLogReason?: string | null): Promise<void> {
		return this.operationsService.deleteGuildById(guildId);
	}

	async transferOwnership(
		params: {userId: UserID; guildId: GuildID; newOwnerId: UserID},
		auditLogReason?: string | null,
	): Promise<GuildResponse> {
		return this.ownershipService.transferOwnership(params, auditLogReason);
	}

	async checkGuildVerification(params: {user: User; guild: Guild; member: GuildMember}): Promise<void> {
		return this.ownershipService.checkGuildVerification(params);
	}
}
