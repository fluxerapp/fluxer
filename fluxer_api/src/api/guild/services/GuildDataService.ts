// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelRepository} from '@app/api/channel/ChannelRepository';
import type {ChannelService} from '@app/api/channel/services/ChannelService';
import type {GuildAuditLogService} from '@app/api/guild/GuildAuditLogService';
import {GuildDiscoveryRepository} from '@app/api/guild/repositories/GuildDiscoveryRepository';
import type {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import {GuildDataHelpers} from '@app/api/guild/services/data/GuildDataHelpers';
import {GuildOperationsService} from '@app/api/guild/services/data/GuildOperationsService';
import {GuildOwnershipService} from '@app/api/guild/services/data/GuildOwnershipService';
import {GuildVanityService} from '@app/api/guild/services/data/GuildVanityService';
import type {EntityAssetService} from '@app/api/infrastructure/EntityAssetService';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {ISnowflakeService} from '@app/api/infrastructure/ISnowflakeService';
import type {InviteRepository} from '@app/api/invite/InviteRepository';
import type {LimitConfigService} from '@app/api/limits/LimitConfigService';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import type {WebhookRepository} from '@app/api/webhook/WebhookRepository';

export class GuildDataService {
	private readonly helpers: GuildDataHelpers;
	readonly operations: GuildOperationsService;
	readonly vanity: GuildVanityService;
	readonly ownership: GuildOwnershipService;

	constructor(
		private readonly guildRepository: GuildRepository,
		private readonly channelRepository: ChannelRepository,
		private readonly inviteRepository: InviteRepository,
		private readonly channelService: ChannelService,
		private readonly gatewayService: IGatewayService,
		private readonly entityAssetService: EntityAssetService,
		private readonly userRepository: UserRepository,
		private readonly snowflakeService: ISnowflakeService,
		private readonly webhookRepository: WebhookRepository,
		private readonly guildAuditLogService: GuildAuditLogService,
		private readonly limitConfigService: LimitConfigService,
	) {
		this.helpers = new GuildDataHelpers(this.gatewayService, this.guildAuditLogService, this.userRepository);
		this.operations = new GuildOperationsService(
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
			this.limitConfigService,
			new GuildDiscoveryRepository(),
		);
		this.vanity = new GuildVanityService(this.guildRepository, this.inviteRepository, this.helpers);
		this.ownership = new GuildOwnershipService(this.guildRepository, this.userRepository, this.helpers);
	}
}
