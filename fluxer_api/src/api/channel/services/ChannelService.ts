// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import type {ChannelID} from '@app/api/BrandedTypes';
import type {IChannelRepository} from '@app/api/channel/IChannelRepository';
import type {AttachmentUploadTraceRepository} from '@app/api/channel/repositories/message/AttachmentUploadTraceRepository';
import {AttachmentUploadService} from '@app/api/channel/services/AttachmentUploadService';
import {CallService} from '@app/api/channel/services/CallService';
import {ChannelDataService} from '@app/api/channel/services/ChannelDataService';
import {GroupDmOperationsService} from '@app/api/channel/services/group_dm/GroupDmOperationsService';
import {MessageInteractionService} from '@app/api/channel/services/MessageInteractionService';
import {MessageService} from '@app/api/channel/services/MessageService';
import {MessagePersistenceService} from '@app/api/channel/services/message/MessagePersistenceService';
import {UserMessageDeletionService} from '@app/api/channel/services/message/UserMessageDeletionService';
import type {IFavoriteMemeRepository} from '@app/api/favorite_meme/IFavoriteMemeRepository';
import type {GuildAuditLogService} from '@app/api/guild/GuildAuditLogService';
import type {IGuildRepositoryAggregate} from '@app/api/guild/repositories/IGuildRepositoryAggregate';
import type {AvatarService} from '@app/api/infrastructure/AvatarService';
import type {IPurgeQueue} from '@app/api/infrastructure/CachePurgeQueue';
import type {EmbedService} from '@app/api/infrastructure/EmbedService';
import type {ILiveKitService} from '@app/api/infrastructure/ILiveKitService';
import type {IStorageService} from '@app/api/infrastructure/IStorageService';
import type {IVoiceRoomStore} from '@app/api/infrastructure/IVoiceRoomStore';
import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import type {IInviteRepository} from '@app/api/invite/IInviteRepository';
import type {LimitConfigService} from '@app/api/limits/LimitConfigService';
import type {User} from '@app/api/models/User';
import type {ReadStateService} from '@app/api/read_state/ReadStateService';
import type {IUserRepository} from '@app/api/user/IUserRepository';
import {createDirectMessageSpamMitigationService} from '@app/api/user/services/DirectMessageSpamMitigationService';
import type {VoiceAvailabilityService} from '@app/api/voice/VoiceAvailabilityService';
import type {IWebhookRepository} from '@app/api/webhook/IWebhookRepository';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import type {IRateLimitService} from '@pkgs/rate_limit/src/IRateLimitService';
import type {IVirusScanService} from '@pkgs/virus_scan/src/IVirusScanService';

interface SlowmodeState {
	rateLimitPerUser: number;
	retryAfterMs: number;
	nextSendAllowedAt: Date | null;
	canBypass: boolean;
}

export class ChannelService {
	public readonly channelData: ChannelDataService;
	public readonly messages: MessageService;
	public readonly interactions: MessageInteractionService;
	public readonly attachments: AttachmentUploadService;
	public readonly groupDms: GroupDmOperationsService;
	public readonly calls: CallService;
	public readonly userMessageDeletion: UserMessageDeletionService;
	private readonly rateLimitService: IRateLimitService;

	constructor(
		apiContext: ApiContext,
		channelRepository: IChannelRepository,
		userRepository: IUserRepository,
		guildRepository: IGuildRepositoryAggregate,
		userCacheService: UserCacheService,
		embedService: EmbedService,
		readStateService: ReadStateService,
		storageService: IStorageService,
		attachmentUploadTraceRepository: AttachmentUploadTraceRepository,
		avatarService: AvatarService,
		virusScanService: IVirusScanService,
		purgeQueue: IPurgeQueue,
		favoriteMemeRepository: IFavoriteMemeRepository,
		guildAuditLogService: GuildAuditLogService,
		voiceRoomStore: IVoiceRoomStore,
		liveKitService: ILiveKitService,
		inviteRepository: IInviteRepository,
		webhookRepository: IWebhookRepository,
		limitConfigService: LimitConfigService,
		voiceAvailabilityService: VoiceAvailabilityService | null,
	) {
		const {
			cache: cacheService,
			gateway: gatewayService,
			media: mediaService,
			worker: workerService,
			snowflake: snowflakeService,
			rateLimit: rateLimitService,
		} = apiContext.services;
		this.rateLimitService = rateLimitService;
		this.userMessageDeletion = new UserMessageDeletionService({
			channelRepository,
			gatewayService,
			storageService,
			purgeQueue,
		});
		const messagePersistenceService = new MessagePersistenceService(
			channelRepository,
			userRepository,
			guildRepository,
			embedService,
			storageService,
			attachmentUploadTraceRepository,
			mediaService,
			virusScanService,
			snowflakeService,
			readStateService,
			limitConfigService,
		);
		const directMessageSpamMitigationService = createDirectMessageSpamMitigationService(apiContext, userRepository);
		this.channelData = new ChannelDataService(
			channelRepository,
			userRepository,
			guildRepository,
			userCacheService,
			storageService,
			gatewayService,
			avatarService,
			snowflakeService,
			purgeQueue,
			voiceRoomStore,
			liveKitService,
			voiceAvailabilityService,
			messagePersistenceService,
			guildAuditLogService,
			inviteRepository,
			webhookRepository,
			limitConfigService,
			rateLimitService,
		);
		this.messages = new MessageService(
			channelRepository,
			userRepository,
			guildRepository,
			userCacheService,
			readStateService,
			cacheService,
			storageService,
			gatewayService,
			mediaService,
			workerService,
			snowflakeService,
			rateLimitService,
			purgeQueue,
			favoriteMemeRepository,
			guildAuditLogService,
			messagePersistenceService,
			attachmentUploadTraceRepository,
			limitConfigService,
			directMessageSpamMitigationService,
		);
		this.interactions = new MessageInteractionService(
			channelRepository,
			userRepository,
			guildRepository,
			gatewayService,
			snowflakeService,
			messagePersistenceService,
			guildAuditLogService,
			limitConfigService,
		);
		this.attachments = new AttachmentUploadService(
			channelRepository,
			userRepository,
			storageService,
			attachmentUploadTraceRepository,
			purgeQueue,
			this.interactions,
			this.messages,
			limitConfigService,
			gatewayService,
		);
		this.groupDms = new GroupDmOperationsService(
			channelRepository,
			userRepository,
			guildRepository,
			userCacheService,
			gatewayService,
			snowflakeService,
			this.messages.persistence,
			limitConfigService,
		);
		this.calls = new CallService(
			channelRepository,
			userRepository,
			guildRepository,
			gatewayService,
			userCacheService,
			snowflakeService,
			readStateService,
			voiceAvailabilityService,
			voiceRoomStore,
		);
	}

	async getSlowmodeState({user, channelId}: {user: User; channelId: ChannelID}): Promise<SlowmodeState> {
		const auth = await this.channelData.auth.getChannelAuthenticated({userId: user.id, channelId});
		const rateLimitPerUser = auth.channel.rateLimitPerUser ?? 0;
		if (!auth.guild || rateLimitPerUser <= 0 || user.isBot) {
			return {rateLimitPerUser, retryAfterMs: 0, nextSendAllowedAt: null, canBypass: false};
		}
		const canBypass = await auth.hasPermission(Permissions.BYPASS_SLOWMODE);
		if (canBypass) {
			return {rateLimitPerUser, retryAfterMs: 0, nextSendAllowedAt: null, canBypass: true};
		}
		const peek = await this.rateLimitService.peekLimit({
			identifier: `slowmode:${channelId}:${user.id}`,
			maxAttempts: 1,
			windowMs: rateLimitPerUser * 1000,
			algorithm: 'leaky_bucket',
		});
		const retryAfterMs = Math.max(0, peek.resetTime.getTime() - Date.now());
		return {
			rateLimitPerUser,
			retryAfterMs,
			nextSendAllowedAt: retryAfterMs > 0 ? peek.resetTime : null,
			canBypass: false,
		};
	}
}
