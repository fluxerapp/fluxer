// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import type {IEmailService} from '@pkgs/email/src/IEmailService';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import type {RateLimitService} from '@pkgs/rate_limit/src/RateLimitService';
import type {IVirusScanService} from '@pkgs/virus_scan/src/IVirusScanService';
import type {IWorkerService} from '@pkgs/worker/src/contracts/IWorkerService';
import Stripe from 'stripe';
import type {AdminRepository} from '../admin/AdminRepository';
import type {AdminArchiveRepository} from '../admin/repositories/AdminArchiveRepository';
import {BillingRepository} from '../billing/repositories/BillingRepository';
import {Config} from '../Config';
import {createApiContext} from '../CreateApiContext';
import type {ChannelRepository} from '../channel/ChannelRepository';
import type {ChannelService} from '../channel/services/ChannelService';
import type {ConnectionRepository} from '../connection/ConnectionRepository';
import {ConnectionService} from '../connection/ConnectionService';
import type {NcmecSubmissionService} from '../csam/NcmecSubmissionService';
import {DonationRepository} from '../donation/DonationRepository';
import type {IDonationRepository} from '../donation/IDonationRepository';
import type {FavoriteMemeRepository} from '../favorite_meme/FavoriteMemeRepository';
import type {GuildAuditLogService} from '../guild/GuildAuditLogService';
import type {GuildRepository} from '../guild/repositories/GuildRepository';
import type {GuildService} from '../guild/services/GuildService';
import type {AvatarService} from '../infrastructure/AvatarService';
import type {IPurgeQueue} from '../infrastructure/BunnyPurgeQueue';
import {DisabledLiveKitService} from '../infrastructure/DisabledLiveKitService';
import type {DiscriminatorService} from '../infrastructure/DiscriminatorService';
import type {EmbedService} from '../infrastructure/EmbedService';
import type {IAssetDeletionQueue} from '../infrastructure/IAssetDeletionQueue';
import type {IGatewayService} from '../infrastructure/IGatewayService';
import type {ILiveKitService} from '../infrastructure/ILiveKitService';
import type {IMediaService} from '../infrastructure/IMediaService';
import {InMemoryVoiceRoomStore} from '../infrastructure/InMemoryVoiceRoomStore';
import type {ISnowflakeService} from '../infrastructure/ISnowflakeService';
import type {IStorageService} from '../infrastructure/IStorageService';
import type {IUnfurlerService} from '../infrastructure/IUnfurlerService';
import type {IVoiceRoomStore} from '../infrastructure/IVoiceRoomStore';
import type {KVAccountDeletionQueueService} from '../infrastructure/KVAccountDeletionQueueService';
import type {KVActivityTracker} from '../infrastructure/KVActivityTracker';
import type {KVBulkMessageDeletionQueueService} from '../infrastructure/KVBulkMessageDeletionQueueService';
import type {PremiumStateReconciliationQueueService} from '../infrastructure/PremiumStateReconciliationQueueService';
import type {UserCacheService} from '../infrastructure/UserCacheService';
import type {InstanceConfigRepository} from '../instance/InstanceConfigRepository';
import type {InviteService} from '../invite/InviteService';
import {Logger} from '../Logger';
import type {LimitConfigService} from '../limits/LimitConfigService';
import {createGuildStackServices} from '../middleware/GuildStackServiceFactory';
import {getIpInfoService} from '../middleware/ServiceMiddleware';
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
	resolveBlueskyOAuthService,
} from '../middleware/ServiceRegistry';
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
	getExpressionAssetPurger,
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
	getPackRepository,
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
} from '../middleware/ServiceSingletons';
import type {ApplicationRepository} from '../oauth/repositories/ApplicationRepository';
import type {OAuth2TokenRepository} from '../oauth/repositories/OAuth2TokenRepository';
import type {ReadStateRepository} from '../read_state/ReadStateRepository';
import type {ReadStateService} from '../read_state/ReadStateService';
import type {ReportRepository} from '../report/ReportRepository';
import {STRIPE_API_VERSION} from '../stripe/StripeApiVersion';
import {PaymentRepository} from '../user/repositories/PaymentRepository';
import type {UserRepository} from '../user/repositories/UserRepository';
import type {UserContactChangeLogService} from '../user/services/UserContactChangeLogService';
import {UserDeletionEligibilityService} from '../user/services/UserDeletionEligibilityService';
import {UserHarvestRepository} from '../user/UserHarvestRepository';
import type {UserPermissionUtils} from '../utils/UserPermissionUtils';
import {VoiceReconciliationWorker} from '../voice/VoiceReconciliationWorker';
import type {VoiceRepository} from '../voice/VoiceRepository';
import type {VoiceTopology} from '../voice/VoiceTopology';
import type {WorkerTaskName} from './WorkerLaneConfig';

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
	voiceReconciliationWorker: VoiceReconciliationWorker | null;
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
	const limitConfigService = getLimitConfigService();
	await limitConfigService.initialize();
	limitConfigService.setAsGlobalInstance();
	const userCacheService = createUserCacheService();
	const storageService = getStorageService();
	const assetDeletionQueue = getAssetDeletionQueue();
	const purgeQueue = getPurgeQueue();
	const gatewayService = getGatewayService();
	const instanceConfigRepository = getInstanceConfigRepository();
	const blueskyOAuthService = await resolveBlueskyOAuthService(instanceConfigRepository);
	const connectionService = new ConnectionService(connectionRepository, gatewayService, blueskyOAuthService);
	const mediaService = getMediaService();
	const discriminatorService = getDiscriminatorService();
	const ncmecSubmissionService = getNcmecSubmissionService();
	const avatarService = getAvatarService();
	const entityAssetService = getEntityAssetService();
	await ensureVirusScanInitialized();
	const virusScanService = getVirusScanServiceInstance();
	const rateLimitService = getRateLimitService();
	const packRepository = getPackRepository();
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
	const voiceRoomStore = getVoiceRoomStoreInstance() ?? new InMemoryVoiceRoomStore();
	const liveKitService = getLiveKitServiceInstance() ?? new DisabledLiveKitService();
	const voiceAvailabilityService = getVoiceAvailabilityService();
	const voiceReconciliationEnabled = Config.worker.enableVoiceReconciliation;
	const voiceReconciliationWorker =
		Config.voice.enabled && voiceTopology !== null && voiceReconciliationEnabled
			? new VoiceReconciliationWorker({
					gatewayService,
					liveKitService,
					voiceRoomStore,
					kvClient,
					logger: Logger,
					intervalMs: Config.worker.voiceReconciliation.intervalMs,
					staggerDelayMs: Config.worker.voiceReconciliation.staggerDelayMs,
					lockTtlSeconds: Config.worker.voiceReconciliation.lockTtlSeconds,
					cadenceTtlSeconds: Config.worker.voiceReconciliation.cadenceTtlSeconds,
					gatewayOnlyGraceMs: Config.worker.voiceReconciliation.gatewayOnlyGraceMs,
					liveKitOnlyGraceMs: Config.worker.voiceReconciliation.liveKitOnlyGraceMs,
				})
			: null;
	if (Config.voice.enabled && voiceTopology !== null) {
		Logger.info({reconciliationEnabled: voiceReconciliationEnabled}, 'Voice services initialized');
	}
	const inviteRepository = getInviteRepository();
	const webhookRepository = getWebhookRepository();
	const ipInfoService = getIpInfoService();
	const contactChangeLogService = getContactChangeLogService();
	const apiContext = createApiContext();
	const {channelService, guildService, inviteService} = createGuildStackServices({
		apiContext,
		packRepository,
		channelRepository,
		userRepository,
		guildRepository,
		inviteRepository,
		webhookRepository,
		favoriteMemeRepository,
		avatarService,
		entityAssetService,
		assetDeletionQueue,
		expressionAssetPurger: getExpressionAssetPurger(),
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
		voiceReconciliationWorker,
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

export async function shutdownWorkerDependencies(deps: WorkerDependencies): Promise<void> {
	Logger.info('Shutting down worker dependencies...');
	if (deps.voiceReconciliationWorker !== null) {
		deps.voiceReconciliationWorker.stop();
	}
	Logger.info('Worker dependencies shut down successfully');
}
