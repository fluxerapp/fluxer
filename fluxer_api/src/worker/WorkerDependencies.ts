/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {Redis} from 'ioredis';
import Stripe from 'stripe';
import {AdminRepository} from '~/admin/AdminRepository';
import {AdminArchiveRepository} from '~/admin/repositories/AdminArchiveRepository';
import {Config} from '~/Config';
import {ChannelRepository} from '~/channel/ChannelRepository';
import {ChannelService} from '~/channel/services/ChannelService';
import {FavoriteMemeRepository} from '~/favorite_meme/FavoriteMemeRepository';
import {FeatureFlagRepository} from '~/feature_flag/FeatureFlagRepository';
import {FeatureFlagService} from '~/feature_flag/FeatureFlagService';
import {GuildAuditLogService} from '~/guild/GuildAuditLogService';
import {GuildRepository} from '~/guild/repositories/GuildRepository';
import {ExpressionAssetPurger} from '~/guild/services/content/ExpressionAssetPurger';
import {AssetDeletionQueue} from '~/infrastructure/AssetDeletionQueue';
import {AvatarService} from '~/infrastructure/AvatarService';
import {CloudflarePurgeQueue, NoopCloudflarePurgeQueue} from '~/infrastructure/CloudflarePurgeQueue';
import {DisabledLiveKitService} from '~/infrastructure/DisabledLiveKitService';
import {DiscriminatorService} from '~/infrastructure/DiscriminatorService';
import {EmailService} from '~/infrastructure/EmailService';
import {EmbedService} from '~/infrastructure/EmbedService';
import {GatewayService} from '~/infrastructure/GatewayService';
import type {IEmailService} from '~/infrastructure/IEmailService';
import type {ILiveKitService} from '~/infrastructure/ILiveKitService';
import {InMemoryVoiceRoomStore} from '~/infrastructure/InMemoryVoiceRoomStore';
import type {IVoiceRoomStore} from '~/infrastructure/IVoiceRoomStore';
import {LiveKitService} from '~/infrastructure/LiveKitService';
import {MediaService} from '~/infrastructure/MediaService';
import {RateLimitService} from '~/infrastructure/RateLimitService';
import {RedisAccountDeletionQueueService} from '~/infrastructure/RedisAccountDeletionQueueService';
import {RedisActivityTracker} from '~/infrastructure/RedisActivityTracker';
import {RedisBulkMessageDeletionQueueService} from '~/infrastructure/RedisBulkMessageDeletionQueueService';
import {RedisCacheService} from '~/infrastructure/RedisCacheService';
import type {SnowflakeService} from '~/infrastructure/SnowflakeService';
import {StorageService} from '~/infrastructure/StorageService';
import {TestEmailService} from '~/infrastructure/TestEmailService';
import {UnfurlerService} from '~/infrastructure/UnfurlerService';
import {UserCacheService} from '~/infrastructure/UserCacheService';
import {VirusScanService} from '~/infrastructure/VirusScanService';
import {VoiceRoomStore} from '~/infrastructure/VoiceRoomStore';
import {InviteRepository} from '~/invite/InviteRepository';
import {Logger} from '~/Logger';
import {ApplicationRepository} from '~/oauth/repositories/ApplicationRepository';
import {OAuth2TokenRepository} from '~/oauth/repositories/OAuth2TokenRepository';
import {PackRepository} from '~/pack/PackRepository';
import {PackService} from '~/pack/PackService';
import {ReadStateRepository} from '~/read_state/ReadStateRepository';
import {ReadStateService} from '~/read_state/ReadStateService';
import {ReportRepository} from '~/report/ReportRepository';
import {PaymentRepository} from '~/user/repositories/PaymentRepository';
import {UserDeletionEligibilityService} from '~/user/services/UserDeletionEligibilityService';
import {UserHarvestRepository} from '~/user/UserHarvestRepository';
import {UserRepository} from '~/user/UserRepository';
import {UserPermissionUtils} from '~/utils/UserPermissionUtils';
import {VoiceRepository} from '~/voice/VoiceRepository';
import {VoiceTopology} from '~/voice/VoiceTopology';
import {WebhookRepository} from '~/webhook/WebhookRepository';
import {WorkerService} from './WorkerService';

export interface WorkerDependencies {
	redis: Redis;
	snowflakeService: SnowflakeService;

	userRepository: UserRepository;
	channelRepository: ChannelRepository;
	guildRepository: GuildRepository;
	favoriteMemeRepository: FavoriteMemeRepository;
	applicationRepository: ApplicationRepository;
	oauth2TokenRepository: OAuth2TokenRepository;
	readStateRepository: ReadStateRepository;
	adminRepository: AdminRepository;
	reportRepository: ReportRepository;
	paymentRepository: PaymentRepository;
	userHarvestRepository: UserHarvestRepository;
	adminArchiveRepository: AdminArchiveRepository;
	voiceRepository: VoiceRepository | null;

	cacheService: RedisCacheService;
	userCacheService: UserCacheService;
	storageService: StorageService;
	assetDeletionQueue: AssetDeletionQueue;
	cloudflarePurgeQueue: CloudflarePurgeQueue | NoopCloudflarePurgeQueue;
	featureFlagService: FeatureFlagService;

	gatewayService: GatewayService;
	mediaService: MediaService;
	discriminatorService: DiscriminatorService;
	avatarService: AvatarService;
	virusScanService: VirusScanService;
	rateLimitService: RateLimitService;
	emailService: IEmailService;

	workerService: WorkerService;
	unfurlerService: UnfurlerService;
	embedService: EmbedService;
	readStateService: ReadStateService;
	userPermissionUtils: UserPermissionUtils;
	activityTracker: RedisActivityTracker;
	deletionQueueService: RedisAccountDeletionQueueService;
	bulkMessageDeletionQueueService: RedisBulkMessageDeletionQueueService;
	deletionEligibilityService: UserDeletionEligibilityService;

	voiceRoomStore: IVoiceRoomStore;
	liveKitService: ILiveKitService;
	voiceTopology: VoiceTopology | null;

	channelService: ChannelService;
	guildAuditLogService: GuildAuditLogService;

	stripe: Stripe | null;
}

export async function initializeWorkerDependencies(snowflakeService: SnowflakeService): Promise<WorkerDependencies> {
	Logger.info('Initializing worker dependencies...');

	const redis = new Redis(Config.redis.url);

	const userRepository = new UserRepository();
	const channelRepository = new ChannelRepository();
	const guildRepository = new GuildRepository();
	const favoriteMemeRepository = new FavoriteMemeRepository();
	const applicationRepository = new ApplicationRepository();
	const oauth2TokenRepository = new OAuth2TokenRepository();
	const readStateRepository = new ReadStateRepository();
	const adminRepository = new AdminRepository();
	const adminArchiveRepository = new AdminArchiveRepository();
	const reportRepository = new ReportRepository();
	const paymentRepository = new PaymentRepository();
	const userHarvestRepository = new UserHarvestRepository();

	const cacheService = new RedisCacheService(redis);
	const featureFlagRepository = new FeatureFlagRepository();
	const featureFlagService = new FeatureFlagService(featureFlagRepository, cacheService);
	await featureFlagService.initialize();
	const userCacheService = new UserCacheService(cacheService, userRepository);
	const storageService = new StorageService();
	const assetDeletionQueue = new AssetDeletionQueue(redis);
	const cloudflarePurgeQueue = Config.cloudflare.purgeEnabled
		? new CloudflarePurgeQueue(redis)
		: new NoopCloudflarePurgeQueue();

	const gatewayService = new GatewayService();
	const mediaService = new MediaService();
	const discriminatorService = new DiscriminatorService(userRepository, cacheService);
	const avatarService = new AvatarService(storageService, mediaService);
	const virusScanService = new VirusScanService(cacheService);
	const rateLimitService = new RateLimitService(cacheService);
	const packRepository = new PackRepository();
	const packAssetPurger = new ExpressionAssetPurger(assetDeletionQueue);
	const packService = new PackService(
		packRepository,
		guildRepository,
		avatarService,
		snowflakeService,
		packAssetPurger,
		userRepository,
		userCacheService,
		featureFlagService,
	);
	const emailService: IEmailService = Config.dev.testModeEnabled
		? new TestEmailService()
		: new EmailService(userRepository);

	const workerService = new WorkerService();
	const guildAuditLogService = new GuildAuditLogService(guildRepository, snowflakeService, workerService);
	const unfurlerService = new UnfurlerService(cacheService, mediaService);
	const embedService = new EmbedService(channelRepository, cacheService, unfurlerService, mediaService, workerService);
	const readStateService = new ReadStateService(readStateRepository, gatewayService);
	const userPermissionUtils = new UserPermissionUtils(userRepository, guildRepository);
	const activityTracker = new RedisActivityTracker(redis);
	const deletionQueueService = new RedisAccountDeletionQueueService(redis, userRepository);
	const bulkMessageDeletionQueueService = new RedisBulkMessageDeletionQueueService(redis);
	const deletionEligibilityService = new UserDeletionEligibilityService(redis);

	let voiceRepository: VoiceRepository | null = null;
	let voiceTopology: VoiceTopology | null = null;
	let voiceRoomStore: IVoiceRoomStore;
	let liveKitService: ILiveKitService;

	if (Config.voice.enabled) {
		voiceRepository = new VoiceRepository();
		voiceTopology = new VoiceTopology(voiceRepository, null);
		await voiceTopology.initialize();
		voiceRoomStore = new VoiceRoomStore(redis);
		liveKitService = new LiveKitService(voiceTopology);
		Logger.info('Voice services initialized');
	} else {
		voiceRoomStore = new InMemoryVoiceRoomStore();
		liveKitService = new DisabledLiveKitService();
	}

	const inviteRepository = new InviteRepository();
	const webhookRepository = new WebhookRepository();

	const channelService = new ChannelService(
		channelRepository,
		userRepository,
		guildRepository,
		packService,
		userCacheService,
		embedService,
		readStateService,
		cacheService,
		storageService,
		gatewayService,
		mediaService,
		avatarService,
		workerService,
		virusScanService,
		snowflakeService,
		rateLimitService,
		cloudflarePurgeQueue,
		favoriteMemeRepository,
		guildAuditLogService,
		voiceRoomStore,
		liveKitService,
		inviteRepository,
		webhookRepository,
	);

	let stripe: Stripe | null = null;
	if (Config.stripe.enabled && Config.stripe.secretKey) {
		stripe = new Stripe(Config.stripe.secretKey, {
			apiVersion: '2025-12-15.clover',
		});
		Logger.info('Stripe initialized');
	}

	Logger.info('Worker dependencies initialized successfully');

	return {
		redis,
		snowflakeService,
		userRepository,
		channelRepository,
		guildRepository,
		favoriteMemeRepository,
		applicationRepository,
		oauth2TokenRepository,
		readStateRepository,
		adminRepository,
		reportRepository,
		paymentRepository,
		userHarvestRepository,
		adminArchiveRepository,
		voiceRepository,
		cacheService,
		userCacheService,
		storageService,
		assetDeletionQueue,
		cloudflarePurgeQueue,
		featureFlagService,
		gatewayService,
		mediaService,
		discriminatorService,
		avatarService,
		virusScanService,
		rateLimitService,
		emailService,
		workerService,
		unfurlerService,
		embedService,
		readStateService,
		userPermissionUtils,
		activityTracker,
		deletionQueueService,
		bulkMessageDeletionQueueService,
		deletionEligibilityService,
		voiceRoomStore,
		liveKitService,
		voiceTopology,
		channelService,
		guildAuditLogService,
		stripe,
	};
}

export async function shutdownWorkerDependencies(deps: WorkerDependencies): Promise<void> {
	Logger.info('Shutting down worker dependencies...');
	deps.featureFlagService.shutdown();
	await deps.redis.quit();
	Logger.info('Worker dependencies shut down successfully');
}
