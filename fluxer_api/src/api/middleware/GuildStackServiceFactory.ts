// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IpInfoService} from '@pkgs/geoip/src/IpInfoService';
import type {IVirusScanService} from '@pkgs/virus_scan/src/IVirusScanService';
import type {ApiContext} from '../ApiContext';
import type {IChannelRepository} from '../channel/IChannelRepository';
import type {AttachmentUploadTraceRepository} from '../channel/repositories/message/AttachmentUploadTraceRepository';
import {ChannelService} from '../channel/services/ChannelService';
import type {IFavoriteMemeRepository} from '../favorite_meme/IFavoriteMemeRepository';
import type {GuildAuditLogService} from '../guild/GuildAuditLogService';
import type {IGuildRepositoryAggregate} from '../guild/repositories/IGuildRepositoryAggregate';
import {GuildService} from '../guild/services/GuildService';
import type {AvatarService} from '../infrastructure/AvatarService';
import type {IPurgeQueue} from '../infrastructure/BunnyPurgeQueue';
import type {EmbedService} from '../infrastructure/EmbedService';
import type {EntityAssetService} from '../infrastructure/EntityAssetService';
import type {IAssetDeletionQueue} from '../infrastructure/IAssetDeletionQueue';
import type {ILiveKitService} from '../infrastructure/ILiveKitService';
import type {IStorageService} from '../infrastructure/IStorageService';
import type {IVoiceRoomStore} from '../infrastructure/IVoiceRoomStore';
import type {UserCacheService} from '../infrastructure/UserCacheService';
import type {InviteRepository} from '../invite/InviteRepository';
import {InviteService} from '../invite/InviteService';
import type {LimitConfigService} from '../limits/LimitConfigService';
import type {ReadStateService} from '../read_state/ReadStateService';
import type {IUserRepository} from '../user/IUserRepository';
import type {VoiceAvailabilityService} from '../voice/VoiceAvailabilityService';
import type {IWebhookRepository} from '../webhook/IWebhookRepository';

interface GuildStackServiceFactoryDependencies {
	apiContext: ApiContext;
	channelRepository: IChannelRepository;
	userRepository: IUserRepository;
	guildRepository: IGuildRepositoryAggregate;
	inviteRepository: InviteRepository;
	webhookRepository: IWebhookRepository;
	favoriteMemeRepository: IFavoriteMemeRepository;
	avatarService: AvatarService;
	entityAssetService: EntityAssetService;
	assetDeletionQueue: IAssetDeletionQueue;
	userCacheService: UserCacheService;
	limitConfigService: LimitConfigService;
	embedService: EmbedService;
	readStateService: ReadStateService;
	storageService: IStorageService;
	attachmentUploadTraceRepository: AttachmentUploadTraceRepository;
	virusScanService: IVirusScanService;
	purgeQueue: IPurgeQueue;
	guildAuditLogService: GuildAuditLogService;
	voiceRoomStore: IVoiceRoomStore;
	liveKitService: ILiveKitService;
	voiceAvailabilityService: VoiceAvailabilityService | null;
	ipInfoService: IpInfoService;
}

export interface GuildStackServices {
	channelService: ChannelService;
	guildService: GuildService;
	inviteService: InviteService;
}

class LazyGuildStackServices implements GuildStackServices {
	private cachedChannelService: ChannelService | undefined;
	private cachedGuildService: GuildService | undefined;
	private cachedInviteService: InviteService | undefined;

	constructor(private readonly dependencies: GuildStackServiceFactoryDependencies) {}

	get channelService(): ChannelService {
		this.cachedChannelService ??= new ChannelService(
			this.dependencies.apiContext,
			this.dependencies.channelRepository,
			this.dependencies.userRepository,
			this.dependencies.guildRepository,
			this.dependencies.userCacheService,
			this.dependencies.embedService,
			this.dependencies.readStateService,
			this.dependencies.storageService,
			this.dependencies.attachmentUploadTraceRepository,
			this.dependencies.avatarService,
			this.dependencies.virusScanService,
			this.dependencies.purgeQueue,
			this.dependencies.favoriteMemeRepository,
			this.dependencies.guildAuditLogService,
			this.dependencies.voiceRoomStore,
			this.dependencies.liveKitService,
			this.dependencies.inviteRepository,
			this.dependencies.webhookRepository,
			this.dependencies.limitConfigService,
			this.dependencies.voiceAvailabilityService,
		);
		return this.cachedChannelService;
	}

	get guildService(): GuildService {
		this.cachedGuildService ??= new GuildService(
			this.dependencies.apiContext,
			this.dependencies.guildRepository,
			this.dependencies.channelRepository,
			this.dependencies.inviteRepository,
			this.channelService,
			this.dependencies.userCacheService,
			this.dependencies.entityAssetService,
			this.dependencies.avatarService,
			this.dependencies.assetDeletionQueue,
			this.dependencies.webhookRepository,
			this.dependencies.guildAuditLogService,
			this.dependencies.limitConfigService,
			this.dependencies.ipInfoService,
		);
		return this.cachedGuildService;
	}

	get inviteService(): InviteService {
		this.cachedInviteService ??= new InviteService(
			this.dependencies.apiContext,
			this.dependencies.inviteRepository,
			this.guildService,
			this.channelService,
			this.dependencies.guildAuditLogService,
			this.dependencies.limitConfigService,
		);
		return this.cachedInviteService;
	}
}

export function createGuildStackServices(dependencies: GuildStackServiceFactoryDependencies): GuildStackServices {
	return new LazyGuildStackServices(dependencies);
}
