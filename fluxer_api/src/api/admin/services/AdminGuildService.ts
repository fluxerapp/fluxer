// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AdminAuditService} from '@app/api/admin/services/AdminAuditService';
import {AdminGuildLookupService} from '@app/api/admin/services/guild/AdminGuildLookupService';
import {AdminGuildManagementService} from '@app/api/admin/services/guild/AdminGuildManagementService';
import {AdminGuildMembershipService} from '@app/api/admin/services/guild/AdminGuildMembershipService';
import {AdminGuildUpdatePropagator} from '@app/api/admin/services/guild/AdminGuildUpdatePropagator';
import {AdminGuildUpdateService} from '@app/api/admin/services/guild/AdminGuildUpdateService';
import {AdminGuildVanityService} from '@app/api/admin/services/guild/AdminGuildVanityService';
import {createGuildID, createUserID} from '@app/api/BrandedTypes';
import type {IChannelRepository} from '@app/api/channel/IChannelRepository';
import type {IGuildDiscoveryRepository} from '@app/api/guild/repositories/GuildDiscoveryRepository';
import type {IGuildRepositoryAggregate} from '@app/api/guild/repositories/IGuildRepositoryAggregate';
import type {GuildService} from '@app/api/guild/services/GuildService';
import type {EntityAssetService} from '@app/api/infrastructure/EntityAssetService';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {InviteRepository} from '@app/api/invite/InviteRepository';
import {createRequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {IUserRepository} from '@app/api/user/IUserRepository';
import type {ListGuildAuditLogsRequest} from '@fluxer/schema/src/domains/admin/AdminGuildSchemas';

interface AdminGuildServiceDeps {
	guildRepository: IGuildRepositoryAggregate;
	userRepository: IUserRepository;
	channelRepository: IChannelRepository;
	inviteRepository: InviteRepository;
	guildService: GuildService;
	gatewayService: IGatewayService;
	entityAssetService: EntityAssetService;
	auditService: AdminAuditService;
	discoveryRepository: IGuildDiscoveryRepository;
}

export class AdminGuildService {
	readonly lookupService: AdminGuildLookupService;
	readonly updateService: AdminGuildUpdateService;
	readonly vanityService: AdminGuildVanityService;
	readonly membershipService: AdminGuildMembershipService;
	readonly managementService: AdminGuildManagementService;
	private readonly updatePropagator: AdminGuildUpdatePropagator;
	private readonly guildService: GuildService;

	constructor(deps: AdminGuildServiceDeps) {
		this.guildService = deps.guildService;
		this.updatePropagator = new AdminGuildUpdatePropagator({
			gatewayService: deps.gatewayService,
			discoveryRepository: deps.discoveryRepository,
		});
		this.lookupService = new AdminGuildLookupService({
			guildRepository: deps.guildRepository,
			userRepository: deps.userRepository,
			channelRepository: deps.channelRepository,
			gatewayService: deps.gatewayService,
		});
		this.updateService = new AdminGuildUpdateService({
			guildRepository: deps.guildRepository,
			entityAssetService: deps.entityAssetService,
			auditService: deps.auditService,
			updatePropagator: this.updatePropagator,
		});
		this.vanityService = new AdminGuildVanityService({
			guildRepository: deps.guildRepository,
			inviteRepository: deps.inviteRepository,
			auditService: deps.auditService,
			updatePropagator: this.updatePropagator,
		});
		this.membershipService = new AdminGuildMembershipService({
			userRepository: deps.userRepository,
			guildService: deps.guildService,
			auditService: deps.auditService,
		});
		this.managementService = new AdminGuildManagementService({
			guildRepository: deps.guildRepository,
			gatewayService: deps.gatewayService,
			guildService: deps.guildService,
			auditService: deps.auditService,
		});
	}

	async listGuildAuditLogs(data: ListGuildAuditLogsRequest) {
		return this.guildService.fetchGuildAuditLogs({
			guildId: createGuildID(data.guild_id),
			requestCache: createRequestCache(),
			limit: data.limit,
			beforeLogId: data.before,
			afterLogId: data.after,
			filterUserId: data.user_id ? createUserID(data.user_id) : undefined,
			actionType: data.action_type,
		});
	}
}
