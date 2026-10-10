// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import type {ChannelID} from '@app/api/BrandedTypes';
import type {ChannelRepository} from '@app/api/channel/ChannelRepository';
import type {AttachmentUploadTraceRepository} from '@app/api/channel/repositories/message/AttachmentUploadTraceRepository';
import {AttachmentUploadService} from '@app/api/channel/services/AttachmentUploadService';
import type {AuthenticatedChannel} from '@app/api/channel/services/AuthenticatedChannel';
import {CallService} from '@app/api/channel/services/CallService';
import {ChannelDataService} from '@app/api/channel/services/ChannelDataService';
import {GroupDmOperationsService} from '@app/api/channel/services/group_dm/GroupDmOperationsService';
import {MessageInteractionService} from '@app/api/channel/services/MessageInteractionService';
import {MessageService} from '@app/api/channel/services/MessageService';
import {MessagePersistenceService} from '@app/api/channel/services/message/MessagePersistenceService';
import {UserMessageDeletionService} from '@app/api/channel/services/message/UserMessageDeletionService';
import {type ThreadViewer, viewerActive} from '@app/api/experiment/ChannelThreadsGate';
import type {FavoriteMemeRepository} from '@app/api/favorite_meme/FavoriteMemeRepository';
import type {GuildAuditLogService} from '@app/api/guild/GuildAuditLogService';
import type {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import type {AvatarService} from '@app/api/infrastructure/AvatarService';
import type {IPurgeQueue} from '@app/api/infrastructure/CachePurgeQueue';
import type {EmbedService} from '@app/api/infrastructure/EmbedService';
import type {ILiveKitService} from '@app/api/infrastructure/ILiveKitService';
import type {IStorageService} from '@app/api/infrastructure/IStorageService';
import type {IVoiceRoomStore} from '@app/api/infrastructure/IVoiceRoomStore';
import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import type {InviteRepository} from '@app/api/invite/InviteRepository';
import type {LimitConfigService} from '@app/api/limits/LimitConfigService';
import type {User} from '@app/api/models/User';
import type {ReadStateService} from '@app/api/read_state/ReadStateService';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import type {VoiceAvailabilityService} from '@app/api/voice/VoiceAvailabilityService';
import type {WebhookRepository} from '@app/api/webhook/WebhookRepository';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {TEXT_THREAD_PARENT_CHANNEL_TYPES} from '@fluxer/constants/src/ThreadConstants';
import type {IRateLimitService} from '@pkgs/rate_limit/src/IRateLimitService';
import type {IVirusScanService} from '@pkgs/virus_scan/src/IVirusScanService';

interface TypingCooldown {
	message_send_cooldown_ms?: number;
	thread_create_cooldown_ms?: number;
}

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
		channelRepository: ChannelRepository,
		userRepository: UserRepository,
		guildRepository: GuildRepository,
		userCacheService: UserCacheService,
		embedService: EmbedService,
		readStateService: ReadStateService,
		storageService: IStorageService,
		attachmentUploadTraceRepository: AttachmentUploadTraceRepository,
		avatarService: AvatarService,
		virusScanService: IVirusScanService,
		purgeQueue: IPurgeQueue,
		favoriteMemeRepository: FavoriteMemeRepository,
		guildAuditLogService: GuildAuditLogService,
		voiceRoomStore: IVoiceRoomStore,
		liveKitService: ILiveKitService,
		inviteRepository: InviteRepository,
		webhookRepository: WebhookRepository,
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
			workerService,
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
			cacheService,
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
			voiceRoomStore,
			liveKitService,
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

	async getTypingCooldown({
		user,
		viewer,
		auth,
	}: {
		user: User;
		viewer: ThreadViewer;
		auth: AuthenticatedChannel;
	}): Promise<TypingCooldown | null> {
		const {channel, guild} = auth;
		if (!guild || user.isBot || !viewerActive(viewer, guild.id)) return null;
		const createsThreads = TEXT_THREAD_PARENT_CHANNEL_TYPES.has(channel.type);
		if (channel.rateLimitPerUser <= 0 || (await auth.hasPermission(Permissions.BYPASS_SLOWMODE))) return null;
		const windowMs = channel.rateLimitPerUser * 1000;
		const [messageSendCooldownMs, threadCreateCooldownMs] = await Promise.all([
			this.peekSlowmode(`slowmode:${channel.id}:${user.id}`, windowMs),
			createsThreads ? this.peekSlowmode(`slowmode-thread:${channel.id}:${user.id}`, windowMs) : Promise.resolve(0),
		]);
		if (messageSendCooldownMs <= 0 && threadCreateCooldownMs <= 0) return null;
		return {
			...(messageSendCooldownMs > 0 ? {message_send_cooldown_ms: messageSendCooldownMs} : {}),
			...(threadCreateCooldownMs > 0 ? {thread_create_cooldown_ms: threadCreateCooldownMs} : {}),
		};
	}

	private async peekSlowmode(identifier: string, windowMs: number): Promise<number> {
		const peek = await this.rateLimitService.peekLimit({
			identifier,
			maxAttempts: 1,
			windowMs,
			algorithm: 'leaky_bucket',
		});
		return Math.max(0, peek.resetTime.getTime() - Date.now());
	}

	async getSlowmodeState({
		user,
		channelId,
		viewer,
	}: {
		user: User;
		viewer: ThreadViewer;
		channelId: ChannelID;
	}): Promise<SlowmodeState> {
		const auth = await this.channelData.auth.getChannelAuthenticated({userId: user.id, channelId, viewer});
		const rateLimitPerUser = auth.channel.rateLimitPerUser ?? 0;
		if (!auth.guild || rateLimitPerUser <= 0 || user.isBot) {
			return {rateLimitPerUser, retryAfterMs: 0, nextSendAllowedAt: null, canBypass: false};
		}
		const canBypass = await auth.hasPermission(Permissions.BYPASS_SLOWMODE);
		if (canBypass) {
			return {rateLimitPerUser, retryAfterMs: 0, nextSendAllowedAt: null, canBypass: true};
		}
		const bucket = auth.channel.isThreadOnly() ? 'slowmode-thread' : 'slowmode';
		const peek = await this.rateLimitService.peekLimit({
			identifier: `${bucket}:${channelId}:${user.id}`,
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
