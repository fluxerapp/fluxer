// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID, UserID} from '@app/api/BrandedTypes';
import type {ChannelRepository} from '@app/api/channel/ChannelRepository';
import type {MessageRequest} from '@app/api/channel/MessageTypes';
import type {AuthenticatedChannel} from '@app/api/channel/services/AuthenticatedChannel';
import type {BaseChannelAuthService} from '@app/api/channel/services/BaseChannelAuthService';
import type {MessagePersistenceService} from '@app/api/channel/services/message/MessagePersistenceService';
import type {ThreadMessageResponses} from '@app/api/channel/services/message/ThreadMessageResponses';
import type {ThreadViewer} from '@app/api/experiment/ChannelThreadsGate';
import type {GuildAuditLogService} from '@app/api/guild/GuildAuditLogService';
import type {GuildAuditLogChange} from '@app/api/guild/GuildAuditLogTypes';
import {isGuildMemberTimedOut} from '@app/api/guild/GuildModel';
import type {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {ISnowflakeService} from '@app/api/infrastructure/ISnowflakeService';
import type {KVThreadAutoArchiveQueueService} from '@app/api/infrastructure/KVThreadAutoArchiveQueueService';
import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import {Logger} from '@app/api/Logger';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {Channel} from '@app/api/models/Channel';
import type {User} from '@app/api/models/User';
import type {ReadStateService} from '@app/api/read_state/ReadStateService';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import type {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import type {ThreadActor} from '@fluxer/constants/src/ThreadPermissionUtils';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import type {IRateLimitService} from '@pkgs/rate_limit/src/IRateLimitService';

export interface ThreadServiceDeps {
	channelRepository: ChannelRepository;
	guildRepository: GuildRepository;
	userRepository: UserRepository;
	channelAuth: BaseChannelAuthService;
	gatewayService: IGatewayService;
	snowflakeService: ISnowflakeService;
	rateLimitService: IRateLimitService;
	cacheService: ICacheService;
	guildAuditLogService: GuildAuditLogService;
	userCacheService: UserCacheService;
	archiveQueue: KVThreadAutoArchiveQueueService;
	messagePersistence: MessagePersistenceService;
	threadMessageResponses: ThreadMessageResponses;
	purgeChannelAttachments: (channel: Channel) => Promise<void>;
	readStateService: ReadStateService;
	sendForumStarter: (params: ForumStarterParams) => Promise<MessageResponse>;
	validateForumStarter: (params: Pick<ForumStarterParams, 'user' | 'parentAuth' | 'data'>) => Promise<void>;
}

export interface ForumStarterParams {
	user: User;
	viewer: ThreadViewer;
	parentAuth: AuthenticatedChannel;
	threadId: ChannelID;
	data: MessageRequest;
	requestCache: RequestCache;
}

export class ThreadServiceContext {
	readonly channelRepository: ChannelRepository;
	readonly guildRepository: GuildRepository;
	readonly userRepository: UserRepository;
	readonly channelAuth: BaseChannelAuthService;
	readonly gatewayService: IGatewayService;
	readonly snowflakeService: ISnowflakeService;
	readonly rateLimitService: IRateLimitService;
	readonly cacheService: ICacheService;
	readonly guildAuditLogService: GuildAuditLogService;
	readonly userCacheService: UserCacheService;
	readonly archiveQueue: KVThreadAutoArchiveQueueService;
	readonly messagePersistence: MessagePersistenceService;
	readonly threadMessageResponses: ThreadMessageResponses;
	readonly purgeChannelAttachments: (channel: Channel) => Promise<void>;
	readonly readStateService: ReadStateService;
	readonly sendForumStarter: (params: ForumStarterParams) => Promise<MessageResponse>;
	readonly validateForumStarter: (params: Pick<ForumStarterParams, 'user' | 'parentAuth' | 'data'>) => Promise<void>;

	constructor(deps: ThreadServiceDeps) {
		this.channelRepository = deps.channelRepository;
		this.guildRepository = deps.guildRepository;
		this.userRepository = deps.userRepository;
		this.channelAuth = deps.channelAuth;
		this.gatewayService = deps.gatewayService;
		this.snowflakeService = deps.snowflakeService;
		this.rateLimitService = deps.rateLimitService;
		this.cacheService = deps.cacheService;
		this.guildAuditLogService = deps.guildAuditLogService;
		this.userCacheService = deps.userCacheService;
		this.archiveQueue = deps.archiveQueue;
		this.messagePersistence = deps.messagePersistence;
		this.threadMessageResponses = deps.threadMessageResponses;
		this.purgeChannelAttachments = deps.purgeChannelAttachments;
		this.readStateService = deps.readStateService;
		this.sendForumStarter = deps.sendForumStarter;
		this.validateForumStarter = deps.validateForumStarter;
	}

	async parentActor(auth: AuthenticatedChannel, userId: UserID): Promise<ThreadActor> {
		const permissions = await this.gatewayService.getUserPermissions({
			guildId: auth.channel.guildId!,
			userId,
			channelId: auth.channel.id,
		});
		return {
			permissions,
			isOwner: auth.guild?.owner_id === userId.toString(),
			timedOut: isGuildMemberTimedOut(auth.member),
		};
	}

	async recordAudit(params: {
		guildId: GuildID;
		userId: UserID;
		action: AuditLogActionType;
		targetId: ChannelID | UserID;
		auditLogReason: string | null;
		metadata?: Record<string, string>;
		changes?: GuildAuditLogChange | null;
	}): Promise<void> {
		try {
			const builder = this.guildAuditLogService
				.createBuilder(params.guildId, params.userId)
				.withAction(params.action, params.targetId.toString())
				.withReason(params.auditLogReason);
			if (params.metadata) builder.withMetadata(params.metadata);
			if (params.changes) builder.withChanges(params.changes);
			await builder.commit();
		} catch (error) {
			Logger.error(
				{error, guildId: params.guildId.toString(), action: params.action},
				'Failed to record thread audit log',
			);
		}
	}
}
