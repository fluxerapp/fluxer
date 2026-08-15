// SPDX-License-Identifier: AGPL-3.0-or-later

import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import type {IRateLimitService} from '@pkgs/rate_limit/src/IRateLimitService';
import type {IVirusScanService} from '@pkgs/virus_scan/src/IVirusScanService';
import type {ApiContext} from '../../ApiContext';
import type {ChannelID} from '../../BrandedTypes';
import type {IFavoriteMemeRepository} from '../../favorite_meme/IFavoriteMemeRepository';
import type {GuildAuditLogService} from '../../guild/GuildAuditLogService';
import type {IGuildRepositoryAggregate} from '../../guild/repositories/IGuildRepositoryAggregate';
import type {AvatarService} from '../../infrastructure/AvatarService';
import type {IPurgeQueue} from '../../infrastructure/BunnyPurgeQueue';
import type {EmbedService} from '../../infrastructure/EmbedService';
import type {ILiveKitService} from '../../infrastructure/ILiveKitService';
import type {IStorageService} from '../../infrastructure/IStorageService';
import type {IVoiceRoomStore} from '../../infrastructure/IVoiceRoomStore';
import type {UserCacheService} from '../../infrastructure/UserCacheService';
import type {IInviteRepository} from '../../invite/IInviteRepository';
import type {LimitConfigService} from '../../limits/LimitConfigService';
import type {User} from '../../models/User';
import type {PackService} from '../../pack/PackService';
import type {ReadStateService} from '../../read_state/ReadStateService';
import type {IUserRepository} from '../../user/IUserRepository';
import {createDirectMessageSpamMitigationService} from '../../user/services/DirectMessageSpamMitigationService';
import type {VoiceAvailabilityService} from '../../voice/VoiceAvailabilityService';
import type {IWebhookRepository} from '../../webhook/IWebhookRepository';
import type {IChannelRepository} from '../IChannelRepository';
import type {AttachmentUploadTraceRepository} from '../repositories/message/AttachmentUploadTraceRepository';
import {AttachmentUploadService} from './AttachmentUploadService';
import {CallService} from './CallService';
import {ChannelDataService} from './ChannelDataService';
import {GroupDmOperationsService} from './group_dm/GroupDmOperationsService';
import {MessageInteractionService} from './MessageInteractionService';
import {MessageService} from './MessageService';
import {MessagePersistenceService} from './message/MessagePersistenceService';
import {UserMessageDeletionService} from './message/UserMessageDeletionService';

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
		packService: PackService,
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
			packService,
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
