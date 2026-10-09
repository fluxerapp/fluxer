// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IChannelRepositoryAggregate} from '@app/api/channel/repositories/IChannelRepositoryAggregate';
import type {AttachmentUploadTraceRepository} from '@app/api/channel/repositories/message/AttachmentUploadTraceRepository';
import {CrosspostPropagation} from '@app/api/channel/services/message/CrosspostPropagation';
import {MessageAnonymizationService} from '@app/api/channel/services/message/MessageAnonymizationService';
import {MessageChannelAuthService} from '@app/api/channel/services/message/MessageChannelAuthService';
import {MessageCrosspostService} from '@app/api/channel/services/message/MessageCrosspostService';
import {MessageDeleteService} from '@app/api/channel/services/message/MessageDeleteService';
import {MessageDispatchService} from '@app/api/channel/services/message/MessageDispatchService';
import {MessageEditService} from '@app/api/channel/services/message/MessageEditService';
import {MessageMentionService} from '@app/api/channel/services/message/MessageMentionService';
import {MessageOperationsHelpers} from '@app/api/channel/services/message/MessageOperationsHelpers';
import type {MessagePersistenceService} from '@app/api/channel/services/message/MessagePersistenceService';
import {MessageProcessingService} from '@app/api/channel/services/message/MessageProcessingService';
import {createMessageResponseDataService} from '@app/api/channel/services/message/MessageResponseDataService';
import {MessageRetrievalService} from '@app/api/channel/services/message/MessageRetrievalService';
import {MessageSearchService} from '@app/api/channel/services/message/MessageSearchService';
import {MessageSendService} from '@app/api/channel/services/message/MessageSendService';
import {MessageSystemService} from '@app/api/channel/services/message/MessageSystemService';
import {MessageValidationService} from '@app/api/channel/services/message/MessageValidationService';
import {MessageWriteLock} from '@app/api/channel/services/message/MessageWriteLock';
import {ThreadMessageActivity} from '@app/api/channel/services/message/ThreadMessageActivity';
import type {IFavoriteMemeRepository} from '@app/api/favorite_meme/IFavoriteMemeRepository';
import type {GuildAuditLogService} from '@app/api/guild/GuildAuditLogService';
import type {IGuildRepositoryAggregate} from '@app/api/guild/repositories/IGuildRepositoryAggregate';
import type {IPurgeQueue} from '@app/api/infrastructure/CachePurgeQueue';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {IMediaService} from '@app/api/infrastructure/IMediaService';
import type {ISnowflakeService} from '@app/api/infrastructure/ISnowflakeService';
import type {IStorageService} from '@app/api/infrastructure/IStorageService';
import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import type {LimitConfigService} from '@app/api/limits/LimitConfigService';
import type {ReadStateService} from '@app/api/read_state/ReadStateService';
import type {IUserRepository} from '@app/api/user/IUserRepository';
import type {WorkerTaskName} from '@app/api/worker/WorkerLaneConfig';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import type {IRateLimitService} from '@pkgs/rate_limit/src/IRateLimitService';
import type {IWorkerService} from '@pkgs/worker/src/contracts/IWorkerService';
import {MessagePollService} from '@app/api/channel/services/message/MessagePollService';
import type {MessageReactionService} from '@app/api/channel/services/interaction/MessageReactionService';
import {PollMessageExpiryRepository} from '@app/api/channel/repositories/PollMessageExpiryRepository';

export class MessageService {
	public readonly validation: MessageValidationService;
	public readonly mention: MessageMentionService;
	public readonly search: MessageSearchService;
	public readonly persistence: MessagePersistenceService;
	public readonly channelAuth: MessageChannelAuthService;
	public readonly dispatch: MessageDispatchService;
	public readonly processing: MessageProcessingService;
	public readonly system: MessageSystemService;
	public readonly send: MessageSendService;
	public readonly edit: MessageEditService;
	public readonly poll: MessagePollService;
	public readonly deletion: MessageDeleteService;
	public readonly retrieval: MessageRetrievalService;
	public readonly anonymization: MessageAnonymizationService;
	public readonly writeLock: MessageWriteLock;
	public readonly crosspostPropagation: CrosspostPropagation;
	public readonly crosspost: MessageCrosspostService;
	public readonly threadActivity: ThreadMessageActivity;

	constructor(
		channelRepository: IChannelRepositoryAggregate,
		userRepository: IUserRepository,
		guildRepository: IGuildRepositoryAggregate,
		userCacheService: UserCacheService,
		readStateService: ReadStateService,
		cacheService: ICacheService,
		storageService: IStorageService,
		gatewayService: IGatewayService,
		mediaService: IMediaService,
		workerService: IWorkerService<WorkerTaskName>,
		snowflakeService: ISnowflakeService,
		rateLimitService: IRateLimitService,
		purgeQueue: IPurgeQueue,
		favoriteMemeRepository: IFavoriteMemeRepository,
		guildAuditLogService: GuildAuditLogService,
		persistenceService: MessagePersistenceService,
		attachmentUploadTraceRepository: AttachmentUploadTraceRepository,
		limitConfigService: LimitConfigService,
		messageReactionService: MessageReactionService,
	) {
		this.validation = new MessageValidationService(cacheService, limitConfigService);
		this.writeLock = new MessageWriteLock(cacheService, channelRepository.messages);
		this.crosspostPropagation = new CrosspostPropagation({rateLimitService, workerService});
		this.mention = new MessageMentionService(
			userRepository,
			guildRepository,
			gatewayService,
			workerService,
			createMessageResponseDataService(),
		);
		this.search = new MessageSearchService(userRepository, workerService);
		this.persistence = persistenceService;
		this.channelAuth = new MessageChannelAuthService(
			channelRepository,
			userRepository,
			guildRepository,
			gatewayService,
		);
		this.dispatch = new MessageDispatchService(gatewayService);
		this.processing = new MessageProcessingService(
			channelRepository,
			userRepository,
			userCacheService,
			gatewayService,
			readStateService,
			this.mention,
		);
		this.system = new MessageSystemService(
			channelRepository,
			guildRepository,
			snowflakeService,
			this.persistence,
			this.dispatch,
		);
		const operationsHelpers = new MessageOperationsHelpers({
			channelRepository,
			cacheService,
			storageService,
			mediaService,
			snowflakeService,
			favoriteMemeRepository,
		});
		const pollMessageExpiryRepository = new PollMessageExpiryRepository();
		this.threadActivity = new ThreadMessageActivity(channelRepository, gatewayService, userRepository);
		this.send = new MessageSendService({
			threadActivity: this.threadActivity,
			channelRepository,
			guildRepository,
			userRepository,
			storageService,
			gatewayService,
			snowflakeService,
			rateLimitService,
			favoriteMemeRepository,
			validationService: this.validation,
			mentionService: this.mention,
			searchService: this.search,
			persistenceService: this.persistence,
			channelAuthService: this.channelAuth,
			processingService: this.processing,
			dispatchService: this.dispatch,
			embedAttachmentResolver: this.persistence.getEmbedAttachmentResolver(),
			pollMessageExpiryRepository,
			attachmentUploadTraceRepository,
			operationsHelpers,
			limitConfigService,
			messageWriteLock: this.writeLock,
			crosspostPropagation: this.crosspostPropagation,
		});
		this.edit = new MessageEditService({
			channelRepository,
			userRepository,
			validationService: this.validation,
			persistenceService: this.persistence,
			channelAuthService: this.channelAuth,
			processingService: this.processing,
			dispatchService: this.dispatch,
			searchService: this.search,
			embedAttachmentResolver: this.persistence.getEmbedAttachmentResolver(),
			mentionService: this.mention,
			messageWriteLock: this.writeLock,
			crosspostPropagation: this.crosspostPropagation,
		});
		this.poll = new MessagePollService({
			channelAuthService: this.channelAuth,
			channelRepository,
			userRepository,
			dispatchService: this.dispatch,
			pollExpiryRepository: pollMessageExpiryRepository,
			messageReactionService,
			messageSendService: this.send,
			limitConfigService,
		});
		this.deletion = new MessageDeleteService({
			channelRepository,
			storageService,
			purgeQueue,
			validationService: this.validation,
			channelAuthService: this.channelAuth,
			dispatchService: this.dispatch,
			searchService: this.search,
			pollService: this.poll,
			gatewayService,
			guildAuditLogService,
			crosspostPropagation: this.crosspostPropagation,
		});
		this.crosspost = new MessageCrosspostService({
			channelRepository,
			channelAuthService: this.channelAuth,
			dispatchService: this.dispatch,
			rateLimitService,
			messageWriteLock: this.writeLock,
			crosspostPropagation: this.crosspostPropagation,
		});
		this.retrieval = new MessageRetrievalService(
			channelRepository,
			userCacheService,
			this.channelAuth,
			this.processing,
			this.search,
			userRepository,
		);
		this.anonymization = new MessageAnonymizationService(channelRepository);
	}
}
