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

import {createMiddleware} from 'hono/factory';
import {Redis} from 'ioredis';
import type {HonoEnv} from '~/App';
import {AdminRepository} from '~/admin/AdminRepository';
import {AdminService} from '~/admin/AdminService';
import {AdminArchiveRepository} from '~/admin/repositories/AdminArchiveRepository';
import {AdminArchiveService} from '~/admin/services/AdminArchiveService';
import {AuthService} from '~/auth/AuthService';
import {AuthMfaService} from '~/auth/services/AuthMfaService';
import {DesktopHandoffService} from '~/auth/services/DesktopHandoffService';
import {Config} from '~/Config';
import {ChannelRepository as ProdChannelRepository} from '~/channel/ChannelRepository';
import {ChannelService} from '~/channel/services/ChannelService';
import {ScheduledMessageService} from '~/channel/services/ScheduledMessageService';
import {StreamPreviewService} from '~/channel/services/StreamPreviewService';
import {FavoriteMemeRepository} from '~/favorite_meme/FavoriteMemeRepository';
import {FavoriteMemeService} from '~/favorite_meme/FavoriteMemeService';
import {FeatureFlagRepository} from '~/feature_flag/FeatureFlagRepository';
import {FeatureFlagService} from '~/feature_flag/FeatureFlagService';
import {GuildAuditLogService} from '~/guild/GuildAuditLogService';
import {GuildRepository as ProdGuildRepository} from '~/guild/repositories/GuildRepository';
import {ExpressionAssetPurger} from '~/guild/services/content/ExpressionAssetPurger';
import {GuildService} from '~/guild/services/GuildService';
import {AssetDeletionQueue} from '~/infrastructure/AssetDeletionQueue';
import {AvatarService} from '~/infrastructure/AvatarService';
import {
	CloudflarePurgeQueue,
	type ICloudflarePurgeQueue,
	NoopCloudflarePurgeQueue,
} from '~/infrastructure/CloudflarePurgeQueue';
import {DisabledLiveKitService} from '~/infrastructure/DisabledLiveKitService';
import {DisabledVirusScanService} from '~/infrastructure/DisabledVirusScanService';
import {DiscriminatorService as ProdDiscriminatorService} from '~/infrastructure/DiscriminatorService';
import {EmailService as ProdEmailService} from '~/infrastructure/EmailService';
import {EmbedService} from '~/infrastructure/EmbedService';
import {EntityAssetService} from '~/infrastructure/EntityAssetService';
import {GatewayService as ProdGatewayService} from '~/infrastructure/GatewayService';
import type {IAssetDeletionQueue} from '~/infrastructure/IAssetDeletionQueue';
import type {ICacheService} from '~/infrastructure/ICacheService';
import type {IEmailService} from '~/infrastructure/IEmailService';
import type {ILiveKitService} from '~/infrastructure/ILiveKitService';
import {InMemoryVoiceRoomStore} from '~/infrastructure/InMemoryVoiceRoomStore';
import type {IVoiceRoomStore} from '~/infrastructure/IVoiceRoomStore';
import {LiveKitService} from '~/infrastructure/LiveKitService';
import {LiveKitWebhookService} from '~/infrastructure/LiveKitWebhookService';
import {MediaService as ProdMediaService} from '~/infrastructure/MediaService';
import {PendingJoinInviteStore} from '~/infrastructure/PendingJoinInviteStore';
import {RateLimitService} from '~/infrastructure/RateLimitService';
import {RedisAccountDeletionQueueService} from '~/infrastructure/RedisAccountDeletionQueueService';
import {RedisActivityTracker} from '~/infrastructure/RedisActivityTracker';
import {RedisBulkMessageDeletionQueueService} from '~/infrastructure/RedisBulkMessageDeletionQueueService';
import {RedisCacheService} from '~/infrastructure/RedisCacheService';
import {SMSService} from '~/infrastructure/SMSService';
import {SnowflakeService} from '~/infrastructure/SnowflakeService';
import {StorageService as ProdStorageService} from '~/infrastructure/StorageService';
import {TestEmailService} from '~/infrastructure/TestEmailService';
import {UnfurlerService as ProdUnfurlerService} from '~/infrastructure/UnfurlerService';
import {UserCacheService} from '~/infrastructure/UserCacheService';
import {VirusScanService as ProdVirusScanService} from '~/infrastructure/VirusScanService';
import {VoiceRoomStore} from '~/infrastructure/VoiceRoomStore';
import {InviteRepository as ProdInviteRepository} from '~/invite/InviteRepository';
import {InviteService} from '~/invite/InviteService';
import {getReportSearchService} from '~/Meilisearch';
import {ApplicationService} from '~/oauth/ApplicationService';
import {BotAuthService} from '~/oauth/BotAuthService';
import {BotMfaMirrorService} from '~/oauth/BotMfaMirrorService';
import {OAuth2Service} from '~/oauth/OAuth2Service';
import {ApplicationRepository} from '~/oauth/repositories/ApplicationRepository';
import {OAuth2TokenRepository} from '~/oauth/repositories/OAuth2TokenRepository';
import {PackRepository} from '~/pack/PackRepository';
import {PackService} from '~/pack/PackService';
import {ReadStateRepository as ProdReadStateRepository} from '~/read_state/ReadStateRepository';
import {ReadStateService} from '~/read_state/ReadStateService';
import {ReportRepository} from '~/report/ReportRepository';
import {ReportService} from '~/report/ReportService';
import {RpcService} from '~/rpc/RpcService';
import {StripeService} from '~/stripe/StripeService';
import {TenorService as ProdTenorService} from '~/tenor/TenorService';
import {EmailChangeRepository} from '~/user/repositories/auth/EmailChangeRepository';
import {ScheduledMessageRepository} from '~/user/repositories/ScheduledMessageRepository';
import {UserContactChangeLogRepository} from '~/user/repositories/UserContactChangeLogRepository';
import {EmailChangeService} from '~/user/services/EmailChangeService';
import {UserContactChangeLogService} from '~/user/services/UserContactChangeLogService';
import {UserRepository as ProdUserRepository} from '~/user/UserRepository';
import {UserService} from '~/user/UserService';
import {UserPermissionUtils} from '~/utils/UserPermissionUtils';
import {VoiceAvailabilityService} from '~/voice/VoiceAvailabilityService';
import {VoiceRepository} from '~/voice/VoiceRepository';
import {VoiceService} from '~/voice/VoiceService';
import {VoiceTopology} from '~/voice/VoiceTopology';
import {SendGridWebhookService} from '~/webhook/SendGridWebhookService';
import {WebhookRepository as ProdWebhookRepository} from '~/webhook/WebhookRepository';
import {WebhookService} from '~/webhook/WebhookService';
import {WorkerService as ProdWorkerService} from '~/worker/WorkerService';

const ChannelRepository = ProdChannelRepository;
const UserRepository = ProdUserRepository;
const GuildRepository = ProdGuildRepository;
const InviteRepository = ProdInviteRepository;
const WebhookRepository = ProdWebhookRepository;
const ReadStateRepository = ProdReadStateRepository;

const MediaService = ProdMediaService;
const UnfurlerService = ProdUnfurlerService;
const GatewayService = ProdGatewayService;
const StorageService = ProdStorageService;
const TenorService = ProdTenorService;
const EmailService = ProdEmailService;
const WorkerService = ProdWorkerService;
const DiscriminatorService = ProdDiscriminatorService;

const VirusScanService = Config.clamav.enabled ? ProdVirusScanService : DisabledVirusScanService;
const testEmailServiceInstance = Config.dev.testModeEnabled ? new TestEmailService() : null;

const redis = new Redis(Config.redis.url);

const cacheService: ICacheService = new RedisCacheService(redis);
const pendingJoinInviteStore = new PendingJoinInviteStore(cacheService);
const rateLimitService = new RateLimitService(cacheService);
const snowflakeService = new SnowflakeService(redis);
const cloudflarePurgeQueue: ICloudflarePurgeQueue = Config.cloudflare.purgeEnabled
	? new CloudflarePurgeQueue(redis)
	: new NoopCloudflarePurgeQueue();
const assetDeletionQueue: IAssetDeletionQueue = new AssetDeletionQueue(redis);

const featureFlagRepository = new FeatureFlagRepository();
const featureFlagService = new FeatureFlagService(featureFlagRepository, cacheService);
let featureFlagServiceInitialized = false;

let voiceTopology: VoiceTopology | null = null;
let voiceAvailabilityService: VoiceAvailabilityService | null = null;
let liveKitServiceInstance: ILiveKitService | null = null;
let voiceRoomStoreInstance: IVoiceRoomStore | null = null;
let voiceConfigSubscriber: Redis | null = null;
let voiceInitializationPromise: Promise<void> | null = null;

export async function ensureVoiceResourcesInitialized(): Promise<void> {
	if (!Config.voice.enabled) {
		if (!liveKitServiceInstance) {
			liveKitServiceInstance = new DisabledLiveKitService();
		}
		if (!voiceRoomStoreInstance) {
			voiceRoomStoreInstance = new InMemoryVoiceRoomStore();
		}
		voiceTopology = null;
		voiceAvailabilityService = null;
		return;
	}

	if (voiceTopology && voiceAvailabilityService && liveKitServiceInstance && voiceRoomStoreInstance) {
		return;
	}

	if (!voiceInitializationPromise) {
		voiceInitializationPromise = (async () => {
			const voiceRepository = new VoiceRepository();
			if (!voiceConfigSubscriber) {
				voiceConfigSubscriber = new Redis(Config.redis.url);
			}
			const topology = new VoiceTopology(voiceRepository, voiceConfigSubscriber);
			await topology.initialize();
			voiceTopology = topology;
			voiceAvailabilityService = new VoiceAvailabilityService(topology);
			liveKitServiceInstance = new LiveKitService(topology);
			voiceRoomStoreInstance = new VoiceRoomStore(redis);
		})().finally(() => {
			voiceInitializationPromise = null;
		});
	}

	await voiceInitializationPromise;
}

export const ServiceMiddleware = createMiddleware<HonoEnv>(async (ctx, next) => {
	await snowflakeService.initialize();

	if (!featureFlagServiceInitialized) {
		await featureFlagService.initialize();
		featureFlagServiceInitialized = true;
	}

	const userRepository = new UserRepository();
	const guildRepository = new GuildRepository();
	const channelRepository = new ChannelRepository();
	const inviteRepository = new InviteRepository();
	const webhookRepository = new WebhookRepository();
	const readStateRepository = new ReadStateRepository();
	const favoriteMemeRepository = new FavoriteMemeRepository();
	const reportRepository = new ReportRepository();
	const adminRepository = new AdminRepository();
	const adminArchiveRepository = new AdminArchiveRepository();
	const voiceRepository = new VoiceRepository();
	const applicationRepository = new ApplicationRepository();
	const oauth2TokenRepository = new OAuth2TokenRepository();

	const userCacheService = new UserCacheService(cacheService, userRepository);
	const redisAccountDeletionQueue = new RedisAccountDeletionQueueService(redis, userRepository);
	const redisBulkMessageDeletionQueue = new RedisBulkMessageDeletionQueueService(redis);
	const redisActivityTracker = new RedisActivityTracker(redis);
	const mediaService = new MediaService();
	const storageService = new StorageService();
	const gatewayService = new GatewayService();
	const workerService = new WorkerService();
	const botMfaMirrorService = new BotMfaMirrorService(applicationRepository, userRepository, gatewayService);

	const unfurlerService = new UnfurlerService(cacheService, mediaService);
	const embedService = new EmbedService(channelRepository, cacheService, unfurlerService, mediaService, workerService);
	const readStateService = new ReadStateService(readStateRepository, gatewayService);
	const avatarService = new AvatarService(storageService, mediaService);
	const entityAssetService = new EntityAssetService(storageService, mediaService, assetDeletionQueue);

	const emailService: IEmailService = testEmailServiceInstance ?? new EmailService(userRepository);
	const smsService = new SMSService();
	const virusScanService = new VirusScanService(cacheService);
	await virusScanService.initialize();

	await ensureVoiceResourcesInitialized();
	const liveKitService: ILiveKitService = liveKitServiceInstance ?? new DisabledLiveKitService();
	const voiceRoomStore: IVoiceRoomStore = voiceRoomStoreInstance ?? new InMemoryVoiceRoomStore();

	const userPermissionUtils = new UserPermissionUtils(userRepository, guildRepository);
	const guildAuditLogService = new GuildAuditLogService(guildRepository, snowflakeService, workerService);

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
		voiceAvailabilityService ?? undefined,
	);

	const scheduledMessageRepository = new ScheduledMessageRepository();
	const scheduledMessageService = new ScheduledMessageService(
		channelService,
		scheduledMessageRepository,
		workerService,
		snowflakeService,
		channelRepository,
		featureFlagService,
	);

	const streamPreviewService = new StreamPreviewService(storageService, cacheService);

	const guildService = new GuildService(
		guildRepository,
		channelRepository,
		inviteRepository,
		channelService,
		userCacheService,
		gatewayService,
		entityAssetService,
		avatarService,
		assetDeletionQueue,
		userRepository,
		mediaService,
		cacheService,
		snowflakeService,
		rateLimitService,
		workerService,
		webhookRepository,
		guildAuditLogService,
	);

	const inviteService = new InviteService(
		inviteRepository,
		guildService,
		channelService,
		gatewayService,
		guildAuditLogService,
		userRepository,
		packRepository,
		packService,
	);

	const favoriteMemeService = new FavoriteMemeService(
		favoriteMemeRepository,
		channelService,
		storageService,
		mediaService,
		snowflakeService,
		gatewayService,
		unfurlerService,
	);

	const discriminatorService = new DiscriminatorService(userRepository, cacheService);

	const hasVoiceInfrastructure =
		Config.voice.enabled &&
		voiceTopology !== null &&
		liveKitService instanceof LiveKitService &&
		voiceRoomStore instanceof VoiceRoomStore;

	const liveKitWebhookService =
		hasVoiceInfrastructure && voiceTopology
			? new LiveKitWebhookService(voiceRoomStore, gatewayService, userRepository, liveKitService, voiceTopology)
			: undefined;

	const voiceService =
		hasVoiceInfrastructure && voiceAvailabilityService
			? new VoiceService(
					liveKitService,
					guildRepository,
					userRepository,
					channelRepository,
					voiceRoomStore,
					voiceAvailabilityService,
				)
			: undefined;

	const contactChangeLogRepository = new UserContactChangeLogRepository();
	const contactChangeLogService = new UserContactChangeLogService(contactChangeLogRepository);

	const authMfaService = new AuthMfaService(
		userRepository,
		cacheService,
		smsService,
		gatewayService,
		botMfaMirrorService,
	);
	const authService = new AuthService(
		userRepository,
		inviteService,
		cacheService,
		gatewayService,
		rateLimitService,
		emailService,
		smsService,
		snowflakeService,
		discriminatorService,
		redisAccountDeletionQueue,
		redisActivityTracker,
		pendingJoinInviteStore,
		contactChangeLogService,
		botMfaMirrorService,
		authMfaService,
	);
	const desktopHandoffService = new DesktopHandoffService(cacheService);

	const reportSearchService = getReportSearchService();
	const reportService = new ReportService(
		reportRepository,
		channelRepository,
		guildRepository,
		userRepository,
		inviteRepository,
		emailService,
		snowflakeService,
		storageService,
		reportSearchService,
	);

	const adminService = new AdminService(
		userRepository,
		guildRepository,
		channelRepository,
		adminRepository,
		inviteRepository,
		inviteService,
		pendingJoinInviteStore,
		discriminatorService,
		snowflakeService,
		guildService,
		authService,
		gatewayService,
		userCacheService,
		entityAssetService,
		assetDeletionQueue,
		emailService,
		mediaService,
		storageService,
		reportService,
		workerService,
		cacheService,
		voiceRepository,
		botMfaMirrorService,
		contactChangeLogService,
		redisBulkMessageDeletionQueue,
	);

	const adminArchiveService = new AdminArchiveService(
		adminArchiveRepository,
		userRepository,
		guildRepository,
		storageService,
		snowflakeService,
		workerService,
	);

	const botAuthService = new BotAuthService(applicationRepository);

	const rpcService = new RpcService(
		userRepository,
		guildRepository,
		channelRepository,
		userCacheService,
		readStateService,
		authService,
		gatewayService,
		discriminatorService,
		favoriteMemeRepository,
		packService,
		botAuthService,
		inviteRepository,
		webhookRepository,
		storageService,
		voiceService,
		voiceAvailabilityService ?? undefined,
		rateLimitService,
		mediaService,
		featureFlagService,
	);

	const webhookService = new WebhookService(
		webhookRepository,
		guildService,
		channelService,
		channelRepository,
		cacheService,
		gatewayService,
		avatarService,
		mediaService,
		snowflakeService,
		guildAuditLogService,
	);

	const tenorService = new TenorService(cacheService, mediaService);

	const emailChangeRepository = new EmailChangeRepository();
	const emailChangeService = new EmailChangeService(
		emailChangeRepository,
		emailService,
		userRepository,
		rateLimitService,
	);

	const userService = new UserService(
		userRepository,
		userRepository,
		userRepository,
		userRepository,
		userRepository,
		userRepository,
		authService,
		userCacheService,
		channelService,
		channelRepository,
		guildService,
		gatewayService,
		entityAssetService,
		mediaService,
		packService,
		emailService,
		snowflakeService,
		discriminatorService,
		rateLimitService,
		guildRepository,
		workerService,
		userPermissionUtils,
		redisAccountDeletionQueue,
		redisBulkMessageDeletionQueue,
		botMfaMirrorService,
		contactChangeLogService,
	);

	let stripeService: StripeService | null = null;
	if (!Config.instance.selfHosted) {
		stripeService = new StripeService(
			userRepository,
			authService,
			gatewayService,
			emailService,
			guildRepository,
			guildService,
			cacheService,
		);
	}

	const sendGridWebhookService = new SendGridWebhookService(userRepository, gatewayService);

	const applicationService = new ApplicationService({
		applicationRepository,
		userRepository,
		userService,
		userCacheService,
		entityAssetService,
		discriminatorService,
		snowflakeService,
		botAuthService,
		gatewayService,
	});

	const oauth2Service = new OAuth2Service({
		userRepository,
		applicationRepository,
		oauth2TokenRepository,
		cacheService,
	});

	ctx.set('adminService', adminService);
	ctx.set('adminArchiveService', adminArchiveService);
	ctx.set('applicationRepository', applicationRepository);
	ctx.set('applicationService', applicationService);
	ctx.set('authMfaService', authMfaService);
	ctx.set('authService', authService);
	ctx.set('botAuthService', botAuthService);
	ctx.set('cacheService', cacheService);
	ctx.set('channelService', channelService);
	ctx.set('channelRepository', channelRepository);
	ctx.set('streamPreviewService', streamPreviewService);
	ctx.set('desktopHandoffService', desktopHandoffService);
	ctx.set('emailService', emailService);
	ctx.set('embedService', embedService);
	ctx.set('entityAssetService', entityAssetService);
	ctx.set('favoriteMemeService', favoriteMemeService);
	ctx.set('gatewayService', gatewayService);
	ctx.set('guildService', guildService);
	ctx.set('emailChangeService', emailChangeService);
	ctx.set('inviteService', inviteService);
	ctx.set('packService', packService);
	ctx.set('packRepository', packRepository);
	if (liveKitWebhookService) {
		ctx.set('liveKitWebhookService', liveKitWebhookService);
	}
	ctx.set('mediaService', mediaService);
	ctx.set('oauth2Service', oauth2Service);
	ctx.set('oauth2TokenRepository', oauth2TokenRepository);
	ctx.set('rateLimitService', rateLimitService);
	ctx.set('readStateService', readStateService);
	ctx.set('redisActivityTracker', redisActivityTracker);
	ctx.set('reportService', reportService);
	ctx.set('rpcService', rpcService);
	ctx.set('sendGridWebhookService', sendGridWebhookService);
	ctx.set('snowflakeService', snowflakeService);
	ctx.set('storageService', storageService);
	if (stripeService) {
		ctx.set('stripeService', stripeService);
	}
	ctx.set('sudoModeValid', false);
	ctx.set('tenorService', tenorService);
	ctx.set('userCacheService', userCacheService);
	ctx.set('userRepository', userRepository);
	ctx.set('userService', userService);
	ctx.set('scheduledMessageService', scheduledMessageService);
	ctx.set('webhookService', webhookService);
	ctx.set('workerService', workerService);
	ctx.set('featureFlagService', featureFlagService);

	await next();
});
