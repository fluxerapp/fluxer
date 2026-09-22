// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import type {IChannelRepository} from '@app/api/channel/IChannelRepository';
import type {ChannelService} from '@app/api/channel/services/ChannelService';
import type {IConnectionRepository} from '@app/api/connection/IConnectionRepository';
import type {IGuildRepositoryAggregate} from '@app/api/guild/repositories/IGuildRepositoryAggregate';
import type {GuildService} from '@app/api/guild/services/GuildService';
import type {IDiscriminatorService} from '@app/api/infrastructure/DiscriminatorService';
import type {EntityAssetService} from '@app/api/infrastructure/EntityAssetService';
import type {KVAccountDeletionQueueService} from '@app/api/infrastructure/KVAccountDeletionQueueService';
import type {KVBulkMessageDeletionQueueService} from '@app/api/infrastructure/KVBulkMessageDeletionQueueService';
import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import type {LimitConfigService} from '@app/api/limits/LimitConfigService';
import {UserAccountService} from '@app/api/user/services/UserAccountService';
import {UserChannelService} from '@app/api/user/services/UserChannelService';
import type {UserContactChangeLogService} from '@app/api/user/services/UserContactChangeLogService';
import {UserContentService} from '@app/api/user/services/UserContentService';
import {UserRelationshipService} from '@app/api/user/services/UserRelationshipService';
import type {UserPermissionUtils} from '@app/api/utils/UserPermissionUtils';

export class UserService {
	public readonly accountService: UserAccountService;
	public readonly relationshipService: UserRelationshipService;
	public readonly channelService: UserChannelService;
	public readonly contentService: UserContentService;

	constructor(
		apiContext: ApiContext,
		userCacheService: UserCacheService,
		channelService: ChannelService,
		channelRepository: IChannelRepository,
		guildService: GuildService,
		entityAssetService: EntityAssetService,
		discriminatorService: IDiscriminatorService,
		guildRepository: IGuildRepositoryAggregate,
		userPermissionUtils: UserPermissionUtils,
		kvDeletionQueue: KVAccountDeletionQueueService,
		bulkMessageDeletionQueue: KVBulkMessageDeletionQueueService,
		contactChangeLogService: UserContactChangeLogService,
		connectionRepository: IConnectionRepository,
		limitConfigService: LimitConfigService,
	) {
		this.accountService = new UserAccountService(
			apiContext,
			userCacheService,
			guildService,
			entityAssetService,
			guildRepository,
			discriminatorService,
			kvDeletionQueue,
			contactChangeLogService,
			connectionRepository,
			limitConfigService,
		);
		this.relationshipService = new UserRelationshipService(apiContext, userPermissionUtils, limitConfigService);
		this.channelService = new UserChannelService(
			apiContext,
			channelService,
			channelRepository,
			userPermissionUtils,
			limitConfigService,
		);
		this.contentService = new UserContentService(
			apiContext,
			userCacheService,
			channelService,
			channelRepository,
			bulkMessageDeletionQueue,
			limitConfigService,
		);
	}
}
