// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AdminRepository} from '@app/api/admin/AdminRepository';
import type {AdminArchiveRepository} from '@app/api/admin/repositories/AdminArchiveRepository';
import {BillingRepository} from '@app/api/billing/repositories/BillingRepository';
import {Config} from '@app/api/Config';
import {createApiContext} from '@app/api/CreateApiContext';
import type {ChannelRepository} from '@app/api/channel/ChannelRepository';
import type {ChannelService} from '@app/api/channel/services/ChannelService';
import type {ConnectionRepository} from '@app/api/connection/ConnectionRepository';
import {ConnectionService} from '@app/api/connection/ConnectionService';
import type {NcmecSubmissionService} from '@app/api/csam/NcmecSubmissionService';
import {DonationRepository} from '@app/api/donation/DonationRepository';
import type {IDonationRepository} from '@app/api/donation/IDonationRepository';
import type {FavoriteMemeRepository} from '@app/api/favorite_meme/FavoriteMemeRepository';
import type {GuildAuditLogService} from '@app/api/guild/GuildAuditLogService';
import type {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import type {GuildService} from '@app/api/guild/services/GuildService';
import type {AvatarService} from '@app/api/infrastructure/AvatarService';
import type {IPurgeQueue} from '@app/api/infrastructure/CachePurgeQueue';
import type {DiscriminatorService} from '@app/api/infrastructure/DiscriminatorService';
import type {EmbedService} from '@app/api/infrastructure/EmbedService';
import type {IAssetDeletionQueue} from '@app/api/infrastructure/IAssetDeletionQueue';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {ILiveKitService} from '@app/api/infrastructure/ILiveKitService';
import type {IMediaService} from '@app/api/infrastructure/IMediaService';
import type {ISnowflakeService} from '@app/api/infrastructure/ISnowflakeService';
import type {IStorageService} from '@app/api/infrastructure/IStorageService';
import type {IUnfurlerService} from '@app/api/infrastructure/IUnfurlerService';
import type {IVoiceRoomStore} from '@app/api/infrastructure/IVoiceRoomStore';
import type {KVAccountDeletionQueueService} from '@app/api/infrastructure/KVAccountDeletionQueueService';
import type {KVActivityTracker} from '@app/api/infrastructure/KVActivityTracker';
import type {KVBulkMessageDeletionQueueService} from '@app/api/infrastructure/KVBulkMessageDeletionQueueService';
import type {PremiumStateReconciliationQueueService} from '@app/api/infrastructure/PremiumStateReconciliationQueueService';
import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import type {InstanceConfigRepository} from '@app/api/instance/InstanceConfigRepository';
import type {InviteService} from '@app/api/invite/InviteService';
import {Logger} from '@app/api/Logger';
import type {LimitConfigService} from '@app/api/limits/LimitConfigService';
import {createGuildStackServices} from '@app/api/middleware/GuildStackServiceFactory';
import {getIpInfoService} from '@app/api/middleware/ServiceMiddleware';
import {
	ensureVoiceResourcesInitialized,
	getGatewayService,
	getKVClient,
	getLiveKitServiceInstance,
	getMediaService,
	getVoiceAvailabilityService,
	getVoiceRoomStoreInstance,
	getVoiceTopology,
	getWorkerService,
} from '@app/api/middleware/ServiceRegistry';
import {
	createUserCacheService,
	ensureVirusScanInitialized,
	getAdminArchiveRepository,
	getAdminRepository,
	getApplicationRepository,
	getAssetDeletionQueue,
	getAttachmentUploadTraceRepository,
	getAvatarService,
	getCacheService,
	getChannelRepository,
	getConnectionRepository,
	getContactChangeLogService,
	getDiscriminatorService,
	getEmailService,
	getEmbedService,
	getEntityAssetService,
	getFavoriteMemeRepository,
	getGuildAuditLogService,
	getGuildRepository,
	getInstanceConfigRepository,
	getInviteRepository,
	getKVAccountDeletionQueue,
	getKVActivityTracker,
	getKVBulkMessageDeletionQueue,
	getLimitConfigService,
	getNcmecSubmissionService,
	getOAuth2TokenRepository,
	getPremiumStateReconciliationQueueService,
	getPurgeQueue,
	getRateLimitService,
	getReadStateRepository,
	getReadStateService,
	getReportRepository,
	getStorageService,
	getUnfurlerService,
	getUserPermissionUtils,
	getUserRepository,
	getVirusScanServiceInstance,
	getVoiceRepository,
	getWebhookRepository,
} from '@app/api/middleware/ServiceSingletons';
import type {ApplicationRepository} from '@app/api/oauth/repositories/ApplicationRepository';
import type {OAuth2TokenRepository} from '@app/api/oauth/repositories/OAuth2TokenRepository';
import type {ReadStateRepository} from '@app/api/read_state/ReadStateRepository';
import type {ReadStateService} from '@app/api/read_state/ReadStateService';
import type {ReportRepository} from '@app/api/report/ReportRepository';
import {STRIPE_API_VERSION} from '@app/api/stripe/StripeApiVersion';
import {PaymentRepository} from '@app/api/user/repositories/PaymentRepository';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import type {UserContactChangeLogService} from '@app/api/user/services/UserContactChangeLogService';
import {UserDeletionEligibilityService} from '@app/api/user/services/UserDeletionEligibilityService';
import {UserHarvestRepository} from '@app/api/user/UserHarvestRepository';
import type {UserPermissionUtils} from '@app/api/utils/UserPermissionUtils';
import type {VoiceRepository} from '@app/api/voice/VoiceRepository';
import type {VoiceTopology} from '@app/api/voice/VoiceTopology';
import type {WorkerTaskName} from '@app/api/worker/WorkerLaneConfig';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import type {IEmailService} from '@pkgs/email/src/IEmailService';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import type {RateLimitService} from '@pkgs/rate_limit/src/RateLimitService';
import type {IVirusScanService} from '@pkgs/virus_scan/src/IVirusScanService';
import type {IWorkerService} from '@pkgs/worker/src/contracts/IWorkerService';
import Stripe from 'stripe';

export interface WorkerDependencies {
	kvClient: IKVProvider;
	snowflakeService: ISnowflakeService;
	limitConfigService: LimitConfigService;
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
	voiceRepository: VoiceRepository;
	connectionRepository: ConnectionRepository;
	connectionService: ConnectionService;
	cacheService: ICacheService;
	userCacheService: UserCacheService;
	storageService: IStorageService;
	assetDeletionQueue: IAssetDeletionQueue;
	purgeQueue: IPurgeQueue;
	gatewayService: IGatewayService;
	mediaService: IMediaService;
	discriminatorService: DiscriminatorService;
	avatarService: AvatarService;
	virusScanService: IVirusScanService;
	rateLimitService: RateLimitService;
	emailService: IEmailService;
	instanceConfigRepository: InstanceConfigRepository;
	inviteService: InviteService;
	workerService: IWorkerService<WorkerTaskName>;
	unfurlerService: IUnfurlerService;
	embedService: EmbedService;
	readStateService: ReadStateService;
	userPermissionUtils: UserPermissionUtils;
	activityTracker: KVActivityTracker;
	deletionQueueService: KVAccountDeletionQueueService;
	bulkMessageDeletionQueueService: KVBulkMessageDeletionQueueService;
	premiumStateReconciliationQueueService: PremiumStateReconciliationQueueService;
	deletionEligibilityService: UserDeletionEligibilityService;
	voiceRoomStore: IVoiceRoomStore;
	liveKitService: ILiveKitService;
	voiceTopology: VoiceTopology | null;
	channelService: ChannelService;
	guildAuditLogService: GuildAuditLogService;
	contactChangeLogService: UserContactChangeLogService;
	ncmecSubmissionService: NcmecSubmissionService;
	donationRepository: IDonationRepository;
	guildService: GuildService;
	billingRepository: BillingRepository;
	stripe: Stripe | null;
}

export async function initializeWorkerDependencies(snowflakeService: ISnowflakeService): Promise<WorkerDependencies> {
	Logger.info('Initializing worker dependencies...');
	const kvClient = getKVClient();
	const userRepository = getUserRepository();
	const channelRepository = getChannelRepository();
	const guildRepository = getGuildRepository();
	const favoriteMemeRepository = getFavoriteMemeRepository();
	const applicationRepository = getApplicationRepository();
	const oauth2TokenRepository = getOAuth2TokenRepository();
	const readStateRepository = getReadStateRepository();
	const adminRepository = getAdminRepository();
	const adminArchiveRepository = getAdminArchiveRepository();
	const reportRepository = getReportRepository();
	const paymentRepository = new PaymentRepository();
	const donationRepository = new DonationRepository();
	const userHarvestRepository = new UserHarvestRepository();
	const connectionRepository = getConnectionRepository();
	const cacheService = getCacheService();
	const instanceConfigRepository = getInstanceConfigRepository();
	const limitConfigService = getLimitConfigService();
	await instanceConfigRepository.initialize();
	await limitConfigService.initialize();
	limitConfigService.setAsGlobalInstance();
	const userCacheService = createUserCacheService();
	const storageService = getStorageService();
	const assetDeletionQueue = getAssetDeletionQueue();
	const purgeQueue = getPurgeQueue();
	const gatewayService = getGatewayService();
	const connectionService = new ConnectionService(connectionRepository, gatewayService);
	const mediaService = getMediaService();
	const discriminatorService = getDiscriminatorService();
	const ncmecSubmissionService = getNcmecSubmissionService();
	const avatarService = getAvatarService();
	const entityAssetService = getEntityAssetService();
	await ensureVirusScanInitialized();
	const virusScanService = getVirusScanServiceInstance();
	const rateLimitService = getRateLimitService();
	const emailService = getEmailService();
	const workerService = getWorkerService();
	const guildAuditLogService = getGuildAuditLogService();
	const unfurlerService = getUnfurlerService();
	const embedService = getEmbedService();
	const readStateService = getReadStateService();
	const userPermissionUtils = getUserPermissionUtils();
	const activityTracker = getKVActivityTracker();
	const deletionQueueService = getKVAccountDeletionQueue();
	const bulkMessageDeletionQueueService = getKVBulkMessageDeletionQueue();
	const premiumStateReconciliationQueueService = getPremiumStateReconciliationQueueService();
	const deletionEligibilityService = new UserDeletionEligibilityService(kvClient);
	await ensureVoiceResourcesInitialized();
	const voiceRepository = getVoiceRepository();
	const voiceTopology = getVoiceTopology();
	const voiceRoomStore = getVoiceRoomStoreInstance();
	const liveKitService = getLiveKitServiceInstance();
	if (voiceRoomStore === null || liveKitService === null) {
		throw new Error('Voice resources are unavailable during worker initialization');
	}
	const voiceAvailabilityService = getVoiceAvailabilityService();
	if (Config.voice.enabled && voiceTopology !== null) {
		Logger.info('Voice services initialized');
	}
	const inviteRepository = getInviteRepository();
	const webhookRepository = getWebhookRepository();
	const ipInfoService = getIpInfoService();
	const contactChangeLogService = getContactChangeLogService();
	const apiContext = createApiContext();
	const {channelService, guildService, inviteService} = createGuildStackServices({
		apiContext,
		channelRepository,
		userRepository,
		guildRepository,
		inviteRepository,
		webhookRepository,
		favoriteMemeRepository,
		avatarService,
		entityAssetService,
		assetDeletionQueue,
		userCacheService,
		limitConfigService,
		embedService,
		readStateService,
		storageService,
		attachmentUploadTraceRepository: getAttachmentUploadTraceRepository(),
		virusScanService,
		purgeQueue,
		guildAuditLogService,
		voiceRoomStore,
		liveKitService,
		voiceAvailabilityService,
		ipInfoService,
	});
	const billingRepository = new BillingRepository(snowflakeService, kvClient);
	let stripe: Stripe | null = null;
	if (Config.stripe.enabled && Config.stripe.secretKey) {
		stripe = new Stripe(Config.stripe.secretKey, {
			apiVersion: STRIPE_API_VERSION,
			httpClient: Config.dev.testModeEnabled ? Stripe.createFetchHttpClient() : undefined,
		});
		Logger.info('Stripe initialized');
	}
	Logger.info('Worker dependencies initialized successfully');
	return {
		kvClient,
		snowflakeService,
		limitConfigService,
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
		connectionRepository,
		connectionService,
		cacheService,
		userCacheService,
		storageService,
		assetDeletionQueue,
		purgeQueue,
		gatewayService,
		mediaService,
		discriminatorService,
		avatarService,
		virusScanService,
		rateLimitService,
		emailService,
		instanceConfigRepository,
		inviteService,
		workerService,
		unfurlerService,
		embedService,
		readStateService,
		userPermissionUtils,
		activityTracker,
		deletionQueueService,
		bulkMessageDeletionQueueService,
		premiumStateReconciliationQueueService,
		deletionEligibilityService,
		voiceRoomStore,
		liveKitService,
		voiceTopology,
		channelService,
		guildService,
		donationRepository,
		billingRepository,
		guildAuditLogService,
		contactChangeLogService,
		ncmecSubmissionService,
		stripe,
	};
}
