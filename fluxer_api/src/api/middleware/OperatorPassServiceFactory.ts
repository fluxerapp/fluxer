// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelRepository} from '@app/api/channel/ChannelRepository';
import type {ChannelService} from '@app/api/channel/services/ChannelService';
import type {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import type {GuildService} from '@app/api/guild/services/GuildService';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import {getCacheService, getOperatorPassWallService} from '@app/api/middleware/ServiceSingletons';
import {OperatorPassHoldingService} from '@app/api/operator_pass/OperatorPassHoldingService';
import {OperatorPassLabsRoleService} from '@app/api/operator_pass/OperatorPassLabsRole';
import {OperatorPassRemovalService} from '@app/api/operator_pass/OperatorPassRemovalService';
import type {OperatorPassRepository} from '@app/api/operator_pass/OperatorPassRepository';
import type {OperatorPassWallService} from '@app/api/operator_pass/OperatorPassWallService';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import type {IEmailService} from '@pkgs/email/src/IEmailService';

interface OperatorPassServiceDependencies {
	operatorPassRepository: OperatorPassRepository;
	userRepository: UserRepository;
	guildRepository: GuildRepository;
	guildService: GuildService;
	channelRepository: Pick<ChannelRepository, 'channelData'>;
	channelService: ChannelService;
	emailService: IEmailService;
	gatewayService: IGatewayService;
}

export interface OperatorPassServices {
	readonly repository: OperatorPassRepository;
	readonly wallService: OperatorPassWallService;
	readonly holdingService: OperatorPassHoldingService;
	readonly removalService: OperatorPassRemovalService;
	readonly roleService: OperatorPassLabsRoleService;
}

class LazyOperatorPassServices implements OperatorPassServices {
	private cachedRoleService: OperatorPassLabsRoleService | undefined;
	private cachedHoldingService: OperatorPassHoldingService | undefined;
	private cachedRemovalService: OperatorPassRemovalService | undefined;

	constructor(private readonly dependencies: OperatorPassServiceDependencies) {}

	get repository(): OperatorPassRepository {
		return this.dependencies.operatorPassRepository;
	}

	get roleService(): OperatorPassLabsRoleService {
		const dependencies = this.dependencies;
		this.cachedRoleService ??= new OperatorPassLabsRoleService({
			guildRepository: dependencies.guildRepository,
			guildService: dependencies.guildService,
			userRepository: dependencies.userRepository,
			cacheService: getCacheService(),
			channelRepository: dependencies.channelRepository,
			get channelOverwrites() {
				return dependencies.channelService.channelData.operations;
			},
		});
		return this.cachedRoleService;
	}

	get wallService(): OperatorPassWallService {
		return getOperatorPassWallService();
	}

	get holdingService(): OperatorPassHoldingService {
		this.cachedHoldingService ??= new OperatorPassHoldingService({
			repository: this.repository,
			userRepository: this.dependencies.userRepository,
			roleService: this.roleService,
			gatewayService: this.dependencies.gatewayService,
			emailService: this.dependencies.emailService,
			wallService: this.wallService,
			findPurchase: (purchaseId) => this.repository.findPurchase(purchaseId),
		});
		return this.cachedHoldingService;
	}

	get removalService(): OperatorPassRemovalService {
		this.cachedRemovalService ??= new OperatorPassRemovalService(
			this.repository,
			this.dependencies.userRepository,
			this.roleService,
			this.dependencies.emailService,
			this.dependencies.gatewayService,
			this,
		);
		return this.cachedRemovalService;
	}
}

export function createOperatorPassServices(dependencies: OperatorPassServiceDependencies): OperatorPassServices {
	return new LazyOperatorPassServices(dependencies);
}
