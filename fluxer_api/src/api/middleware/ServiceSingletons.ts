// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminRepository} from '@app/api/admin/AdminRepository';
import {AdminApiKeyRepository} from '@app/api/admin/repositories/AdminApiKeyRepository';
import {AdminArchiveRepository} from '@app/api/admin/repositories/AdminArchiveRepository';
import {AdminApiKeyService} from '@app/api/admin/services/AdminApiKeyService';
import {AdminArchiveService} from '@app/api/admin/services/AdminArchiveService';
import {AdminAuditService} from '@app/api/admin/services/AdminAuditService';
import {PhoneAttemptRiskService} from '@app/api/auth/services/PhoneAttemptRiskService';
import {PhoneFraudGraphService} from '@app/api/auth/services/PhoneFraudGraphService';
import type {UserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {ChannelRepository} from '@app/api/channel/ChannelRepository';
import {AttachmentUploadTraceRepository} from '@app/api/channel/repositories/message/AttachmentUploadTraceRepository';
import {StreamPreviewService} from '@app/api/channel/services/StreamPreviewService';
import type {APIConfig} from '@app/api/config/APIConfig';
import {ConnectionRepository} from '@app/api/connection/ConnectionRepository';
import {createNcmecApiConfig, NcmecReporter} from '@app/api/csam/NcmecReporter';
import {NcmecRepository} from '@app/api/csam/NcmecRepository';
import {NcmecSubmissionService} from '@app/api/csam/NcmecSubmissionService';
import {DonationRepository} from '@app/api/donation/DonationRepository';
import {createEmailProvider} from '@app/api/email/EmailProviderFactory';
import {FavoriteMemeRepository} from '@app/api/favorite_meme/FavoriteMemeRepository';
import {GatewayRequestService} from '@app/api/gateway/GatewayRequestService';
import {GifService} from '@app/api/gif/GifService';
import {createNatsGifProvider} from '@app/api/gif/NatsGifProvider';
import {GuildAuditLogService} from '@app/api/guild/GuildAuditLogService';
import {GuildDiscoveryRepository} from '@app/api/guild/repositories/GuildDiscoveryRepository';
import {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import {GuildDiscoveryService} from '@app/api/guild/services/GuildDiscoveryService';
import {AssetDeletionQueue} from '@app/api/infrastructure/AssetDeletionQueue';
import {AvatarService} from '@app/api/infrastructure/AvatarService';
import {CachePurgeQueue, type IPurgeQueue, NoopPurgeQueue} from '@app/api/infrastructure/CachePurgeQueue';
import {DisabledVirusScanService} from '@app/api/infrastructure/DisabledVirusScanService';
import {DiscriminatorService} from '@app/api/infrastructure/DiscriminatorService';
import {EmailDnsValidationService} from '@app/api/infrastructure/EmailDnsValidationService';
import {EmbedService} from '@app/api/infrastructure/EmbedService';
import {EntityAssetService} from '@app/api/infrastructure/EntityAssetService';
import {ErrorI18nService} from '@app/api/infrastructure/ErrorI18nService';
import type {IAssetDeletionQueue} from '@app/api/infrastructure/IAssetDeletionQueue';
import type {IStorageService} from '@app/api/infrastructure/IStorageService';
import type {IUnfurlerService} from '@app/api/infrastructure/IUnfurlerService';
import {KVAccountDeletionQueueService} from '@app/api/infrastructure/KVAccountDeletionQueueService';
import {KVActivityTracker} from '@app/api/infrastructure/KVActivityTracker';
import {KVBulkMessageDeletionQueueService} from '@app/api/infrastructure/KVBulkMessageDeletionQueueService';
import {NatsUnfurlerService} from '@app/api/infrastructure/NatsUnfurlerService';
import {PremiumStateReconciliationQueueService} from '@app/api/infrastructure/PremiumStateReconciliationQueueService';
import {createStorageService} from '@app/api/infrastructure/StorageServiceFactory';
import {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import {createUsersServiceClient} from '@app/api/infrastructure/UsersServiceClient';
import {VirusScanService} from '@app/api/infrastructure/VirusScanService';
import {GatewayRolloutConfigPublisher} from '@app/api/instance/GatewayRolloutConfigPublisher';
import {InstanceConfigRepository} from '@app/api/instance/InstanceConfigRepository';
import {PushServiceDeliveryConfigPublisher} from '@app/api/instance/PushServiceDeliveryConfigPublisher';
import {InviteRepository} from '@app/api/invite/InviteRepository';
import {Logger} from '@app/api/Logger';
import {LimitConfigService} from '@app/api/limits/LimitConfigService';
import {
	acquireSnowflakeService,
	getGatewayService,
	getKVClient,
	getMediaService,
	getSnowflakeService,
	getWorkerService,
	releaseSnowflakeService,
	type SnowflakeServiceHandle,
} from '@app/api/middleware/ServiceRegistry';
import {clearSingletonsForTesting, singleton} from '@app/api/middleware/Singleton';
import {BotAuthService} from '@app/api/oauth/BotAuthService';
import {BotMfaMirrorService} from '@app/api/oauth/BotMfaMirrorService';
import {ApplicationRepository} from '@app/api/oauth/repositories/ApplicationRepository';
import {OAuth2TokenRepository} from '@app/api/oauth/repositories/OAuth2TokenRepository';
import {ReadStateRepository} from '@app/api/read_state/ReadStateRepository';
import {ReadStateRequestService} from '@app/api/read_state/ReadStateRequestService';
import {ReadStateService} from '@app/api/read_state/ReadStateService';
import {ReportRepository} from '@app/api/report/ReportRepository';
import {getGuildSearchService} from '@app/api/SearchFactory';
import {ThemeService} from '@app/api/theme/ThemeService';
import {EntranceSoundPlayService} from '@app/api/user/entrance_sound/EntranceSoundPlayService';
import {EntranceSoundRepository} from '@app/api/user/entrance_sound/EntranceSoundRepository';
import {EntranceSoundService} from '@app/api/user/entrance_sound/EntranceSoundService';
import {EmailChangeRepository} from '@app/api/user/repositories/auth/EmailChangeRepository';
import {PasswordChangeRepository} from '@app/api/user/repositories/auth/PasswordChangeRepository';
import {UserContactChangeLogRepository} from '@app/api/user/repositories/UserContactChangeLogRepository';
import {UserRepository} from '@app/api/user/repositories/UserRepository';
import {VisionarySlotRepository} from '@app/api/user/repositories/VisionarySlotRepository';
import {UserActivityBuffer} from '@app/api/user/services/UserActivityBuffer';
import {UserContactChangeLogService} from '@app/api/user/services/UserContactChangeLogService';
import {awaitAll} from '@app/api/utils/ConcurrencyUtils';
import {UserPermissionUtils} from '@app/api/utils/UserPermissionUtils';
import {VoiceRepository} from '@app/api/voice/VoiceRepository';
import {SweegoWebhookService} from '@app/api/webhook/SweegoWebhookService';
import {WebhookRepository} from '@app/api/webhook/WebhookRepository';
import {createMockLogger} from '@fluxer/logger/src/mock';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import {KVCacheProvider} from '@pkgs/cache/src/providers/KVCacheProvider';
import {EmailI18nService} from '@pkgs/email/src/EmailI18nService';
import type {EmailConfig, UserBouncedEmailChecker} from '@pkgs/email/src/EmailProviderTypes';
import {EmailService} from '@pkgs/email/src/EmailService';
import type {IEmailService} from '@pkgs/email/src/IEmailService';
import {TestEmailService} from '@pkgs/email/src/TestEmailService';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import {NatsConnectionManager} from '@pkgs/nats/src/NatsConnectionManager';
import {RateLimitService} from '@pkgs/rate_limit/src/RateLimitService';
import type {ISmsProvider} from '@pkgs/sms/src/providers/ISmsProvider';
import {createSmsProvider} from '@pkgs/sms/src/providers/SmsProviderFactory';
import {SmsService} from '@pkgs/sms/src/SmsService';
import type {IVirusScanService} from '@pkgs/virus_scan/src/IVirusScanService';

export const getUserRepository = singleton(() => new UserRepository(getKVClient()));
export const getGuildRepository = singleton(() => new GuildRepository());
export const getChannelRepository = singleton(() => new ChannelRepository());
export const getInviteRepository = singleton(() => new InviteRepository());
export const getWebhookRepository = singleton(() => new WebhookRepository());
export const getReadStateRepository = singleton(() => new ReadStateRepository());
export const getFavoriteMemeRepository = singleton(() => new FavoriteMemeRepository());
export const getConnectionRepository = singleton(() => new ConnectionRepository());
export const getReportRepository = singleton(() => new ReportRepository());
export const getAdminRepository = singleton(() => new AdminRepository());
export const getAdminArchiveRepository = singleton(() => new AdminArchiveRepository());
export const getVoiceRepository = singleton(() => new VoiceRepository());
export const getApplicationRepository = singleton(() => new ApplicationRepository());
export const getOAuth2TokenRepository = singleton(() => new OAuth2TokenRepository());
export const getGuildDiscoveryRepository = singleton(() => new GuildDiscoveryRepository());
export const getEmailChangeRepository = singleton(() => new EmailChangeRepository());
export const getPasswordChangeRepository = singleton(() => new PasswordChangeRepository());
const getUserContactChangeLogRepository = singleton(() => new UserContactChangeLogRepository());
export const getDonationRepository = singleton(() => new DonationRepository());
const getAdminApiKeyRepository = singleton(() => new AdminApiKeyRepository());
let instanceConfigRepositoryInstance: InstanceConfigRepository | null = null;
export const getInstanceConfigRepository = singleton(
	() => {
		const repository = new InstanceConfigRepository(getKVClient());
		instanceConfigRepositoryInstance = repository;
		return repository;
	},
	(repository) => {
		if (instanceConfigRepositoryInstance === repository) instanceConfigRepositoryInstance = null;
		void repository.shutdown().catch((error) => {
			Logger.error({error}, 'Failed to shut down instance config repository');
		});
	},
);

export async function shutdownInstanceConfigRepository(): Promise<void> {
	await instanceConfigRepositoryInstance?.shutdown();
}

export const getGatewayRolloutConfigPublisher = singleton(
	() =>
		new GatewayRolloutConfigPublisher(
			new NatsConnectionManager({
				url: Config.nats.coreUrl,
				token: Config.nats.authToken || undefined,
				name: 'fluxer-api-gateway-rollout-config',
			}),
		),
);

export const getPushServiceDeliveryConfigPublisher = singleton(
	() =>
		new PushServiceDeliveryConfigPublisher(
			new NatsConnectionManager({
				url: Config.nats.coreUrl,
				token: Config.nats.authToken || undefined,
				name: 'fluxer-api-push-service-delivery-config',
			}),
		),
);

export const getVisionarySlotRepository = singleton(() => new VisionarySlotRepository());
export const getCacheService: () => ICacheService = singleton(() => new KVCacheProvider({client: getKVClient()}));
export const getRateLimitService = singleton(() => new RateLimitService(getKVClient()));
export const getPhoneFraudGraphService = singleton(
	() => new PhoneFraudGraphService(getKVClient(), getUserRepository()),
);
export const getPhoneAttemptRiskService = singleton(() => {
	const service = new PhoneAttemptRiskService(getCacheService(), getKVClient());
	const graph = getPhoneFraudGraphService();
	service.onHardBlock(({userId, clientIp}) => {
		void graph.propagateHardBlock(userId ? (BigInt(userId) as UserID) : null, clientIp);
	});
	return service;
});
export const getEmailDnsValidationService = singleton(() => new EmailDnsValidationService());

function createEmailServiceForConfig(
	emailConfigSource: APIConfig['email'],
	bouncedEmailChecker: UserBouncedEmailChecker,
	emailI18n: EmailI18nService,
): IEmailService {
	const emailConfig: EmailConfig = {
		enabled: emailConfigSource.enabled,
		fromEmail: emailConfigSource.fromEmail,
		fromName: emailConfigSource.fromName,
		appBaseUrl: emailConfigSource.appBaseUrl,
		marketingBaseUrl: Config.endpoints.marketing,
	};
	return new EmailService(emailConfig, emailI18n, createEmailProvider(emailConfigSource), bouncedEmailChecker);
}

function createRuntimeEmailService(bouncedEmailChecker: UserBouncedEmailChecker): IEmailService {
	const emailI18n = new EmailI18nService();
	return new Proxy({} as IEmailService, {
		get(_target, property) {
			return async (...args: Array<unknown>): Promise<boolean> => {
				const emailConfig = await getInstanceConfigRepository().getEffectiveEmailConfig();
				const delegate = createEmailServiceForConfig(emailConfig, bouncedEmailChecker, emailI18n);
				const method = delegate[property as keyof IEmailService];
				if (typeof method !== 'function') {
					throw new Error(`Unknown email service method: ${String(property)}`);
				}
				return (method as (this: IEmailService, ...methodArgs: Array<unknown>) => Promise<boolean>).apply(
					delegate,
					args,
				);
			};
		},
	});
}

let _injectedStorageService: IStorageService | undefined;

export function setInjectedStorageService(service: IStorageService | undefined): void {
	_injectedStorageService = service;
}

export const getStorageService: () => IStorageService = (() => {
	const fallback = singleton(() => createStorageService());
	return () => _injectedStorageService ?? fallback();
})();
export const getErrorI18nService = singleton(() => new ErrorI18nService());
let limitConfigServiceInstance: LimitConfigService | null = null;
export const getLimitConfigService = singleton(
	() => {
		const service = new LimitConfigService(getInstanceConfigRepository(), getCacheService(), getKVClient());
		limitConfigServiceInstance = service;
		return service;
	},
	(service) => {
		const shutdown = limitConfigServiceInstance === service ? shutdownServiceSingletons() : service.shutdown();
		if (limitConfigServiceInstance === service) {
			limitConfigServiceInstance = null;
		}
		void shutdown.catch((err) => {
			Logger.error({err}, 'Failed to shut down service singletons');
		});
	},
);
export const getPurgeQueue: () => IPurgeQueue = singleton(() =>
	Config.cachePurge.adapter === 'none' ? new NoopPurgeQueue() : new CachePurgeQueue(getKVClient()),
);
export const getAssetDeletionQueue: () => IAssetDeletionQueue = singleton(() => new AssetDeletionQueue(getKVClient()));

let bulkMessageDeletionQueueClient: IKVProvider | null = null;
let bulkMessageDeletionQueue: KVBulkMessageDeletionQueueService | null = null;

export function getKVBulkMessageDeletionQueue(): KVBulkMessageDeletionQueueService {
	const kvClient = getKVClient();
	if (!bulkMessageDeletionQueue || bulkMessageDeletionQueueClient !== kvClient) {
		bulkMessageDeletionQueue = new KVBulkMessageDeletionQueueService(kvClient, getUserRepository());
		bulkMessageDeletionQueueClient = kvClient;
	}
	return bulkMessageDeletionQueue;
}

let premiumStateQueueClient: IKVProvider | null = null;
let premiumStateQueue: PremiumStateReconciliationQueueService | null = null;

export function getPremiumStateReconciliationQueueService(): PremiumStateReconciliationQueueService {
	const kvClient = getKVClient();
	if (!premiumStateQueue || premiumStateQueueClient !== kvClient) {
		premiumStateQueue = new PremiumStateReconciliationQueueService(kvClient);
		premiumStateQueueClient = kvClient;
	}
	return premiumStateQueue;
}

let activityTrackerClient: IKVProvider | null = null;
let activityTracker: KVActivityTracker | null = null;

export function getKVActivityTracker(): KVActivityTracker {
	const kvClient = getKVClient();
	if (!activityTracker || activityTrackerClient !== kvClient) {
		activityTracker = new KVActivityTracker(kvClient);
		activityTrackerClient = kvClient;
	}
	return activityTracker;
}

let activityBufferClient: IKVProvider | null = null;
let activityBuffer: UserActivityBuffer | null = null;

export function getUserActivityBuffer(): UserActivityBuffer {
	const kvClient = getKVClient();
	if (!activityBuffer || activityBufferClient !== kvClient) {
		activityBuffer = new UserActivityBuffer(kvClient);
		activityBufferClient = kvClient;
	}
	return activityBuffer;
}

let accountDeletionQueueClient: IKVProvider | null = null;
let accountDeletionQueue: KVAccountDeletionQueueService | null = null;

export function getKVAccountDeletionQueue(): KVAccountDeletionQueueService {
	const kvClient = getKVClient();
	if (!accountDeletionQueue || accountDeletionQueueClient !== kvClient) {
		accountDeletionQueue = new KVAccountDeletionQueueService(kvClient, getUserRepository());
		accountDeletionQueueClient = kvClient;
	}
	return accountDeletionQueue;
}

export const getThemeService = singleton(() => new ThemeService(getStorageService()));
const getNcmecReporter = singleton(() => new NcmecReporter({config: createNcmecApiConfig(), fetch}));
const getNcmecRepository = singleton(() => new NcmecRepository());
export const getAttachmentUploadTraceRepository = singleton(() => new AttachmentUploadTraceRepository());
export const getNcmecSubmissionService = singleton(
	() =>
		new NcmecSubmissionService({
			reportRepository: getReportRepository(),
			ncmecApi: getNcmecReporter(),
			ncmecRepository: getNcmecRepository(),
			attachmentUploadTraceRepository: getAttachmentUploadTraceRepository(),
			storageService: getStorageService(),
			channelRepository: getChannelRepository(),
			userRepository: getUserRepository(),
			guildRepository: getGuildRepository(),
			gatewayService: getGatewayService(),
			userCacheService: createUserCacheService(),
			adminArchiveService: new AdminArchiveService(
				getAdminArchiveRepository(),
				getUserRepository(),
				getGuildRepository(),
				getStorageService(),
				getSnowflakeService(),
				getWorkerService(),
			),
			adminAuditService: new AdminAuditService(getAdminRepository(), getSnowflakeService(), {
				userRepository: getUserRepository(),
				guildRepository: getGuildRepository(),
				channelRepository: getChannelRepository(),
			}),
			purgeQueue: getPurgeQueue(),
			workerService: getWorkerService(),
			deletionQueue: getKVAccountDeletionQueue(),
		}),
);

let _virusScanInitPromise: Promise<void> | null = null;

export const getVirusScanServiceInstance: () => IVirusScanService = singleton(() => {
	const VirusScanServiceClass = Config.clamav.enabled ? VirusScanService : DisabledVirusScanService;
	const service = new VirusScanServiceClass(getCacheService());
	_virusScanInitPromise = service.initialize();
	return service;
});

export async function ensureVirusScanInitialized(): Promise<void> {
	getVirusScanServiceInstance();
	await _virusScanInitPromise;
}

const getSmsProvider: () => ISmsProvider = singleton(() => {
	if (Config.dev.testModeEnabled) {
		return createSmsProvider({mode: 'test', logger: createMockLogger()});
	}
	if (Config.sms.enabled && Config.sms.accountSid && Config.sms.authToken && Config.sms.verifyServiceSid) {
		return createSmsProvider({
			mode: 'twilio',
			config: {
				accountSid: Config.sms.accountSid,
				authToken: Config.sms.authToken,
				verifyServiceSid: Config.sms.verifyServiceSid,
			},
		});
	}
	return createSmsProvider({mode: 'unavailable'});
});
export const getSmsService = singleton(() => new SmsService(getSmsProvider()));
export const getEmailService: () => IEmailService = singleton(() => {
	if (Config.dev.testModeEnabled) return new TestEmailService();
	const userRepository = getUserRepository();
	const bouncedEmailChecker: UserBouncedEmailChecker = {
		isEmailBounced: async (email: string) => {
			const user = await userRepository.findByEmail(email);
			return user?.emailBounced ?? false;
		},
	};
	return createRuntimeEmailService(bouncedEmailChecker);
});
let _injectedUnfurlerService: IUnfurlerService | undefined;

export function setInjectedUnfurlerService(service: IUnfurlerService | undefined): void {
	_injectedUnfurlerService = service;
}

const getDefaultUnfurlerService = singleton(() => {
	const instanceConfigRepository = getInstanceConfigRepository();
	const manager = new NatsConnectionManager({
		url: Config.nats.coreUrl,
		token: Config.nats.authToken || undefined,
		name: 'fluxer-api-unfurl',
	});
	void manager.connect().catch((error) => {
		Logger.error({error}, '[nats-unfurl] Failed to establish NATS connection');
	});
	return new NatsUnfurlerService(
		manager,
		async () => instanceConfigRepository.getEffectiveYoutubeApiKey(),
		async () => (await instanceConfigRepository.getEffectiveGifConfig()).klipy_api_key,
	);
});

export function getUnfurlerService(): IUnfurlerService {
	return _injectedUnfurlerService ?? getDefaultUnfurlerService();
}

export const getEmbedService = singleton(
	() => new EmbedService(getChannelRepository(), getUnfurlerService(), getMediaService(), getWorkerService()),
);
export const getReadStateService = singleton(() => new ReadStateService(getReadStateRepository(), getGatewayService()));
export const getDiscriminatorService = singleton(
	() => new DiscriminatorService(getUserRepository(), getCacheService(), getLimitConfigService()),
);
export const getBotAuthService = singleton(() => new BotAuthService(getApplicationRepository()));
export const getBotMfaMirrorService = singleton(
	() => new BotMfaMirrorService(getApplicationRepository(), getUserRepository(), getGatewayService()),
);
export const getGifService = singleton(() => {
	const instanceConfigRepository = getInstanceConfigRepository();
	return new GifService(
		createNatsGifProvider(async () => (await instanceConfigRepository.getEffectiveGifConfig()).klipy_api_key),
	);
});
export const getGuildAuditLogService = singleton(
	() => new GuildAuditLogService(getGuildRepository(), getSnowflakeService(), getWorkerService(), getGatewayService()),
);
export const getUserPermissionUtils = singleton(
	() => new UserPermissionUtils(getUserRepository(), getGuildRepository()),
);
export const getContactChangeLogService = singleton(
	() => new UserContactChangeLogService(getUserContactChangeLogRepository()),
);
export const getSweegoWebhookService = singleton(
	() => new SweegoWebhookService(getUserRepository(), getGatewayService()),
);
export const getStreamPreviewService = singleton(
	() => new StreamPreviewService(getStorageService(), getCacheService()),
);
export const getAvatarService = singleton(
	() => new AvatarService(getStorageService(), getMediaService(), getLimitConfigService()),
);
export const getEntityAssetService = singleton(
	() =>
		new EntityAssetService(getStorageService(), getMediaService(), getAssetDeletionQueue(), getLimitConfigService()),
);
export const getAdminApiKeyService = singleton(
	() => new AdminApiKeyService(getAdminApiKeyRepository(), getSnowflakeService()),
);
export const getAdminArchiveService = singleton(
	() =>
		new AdminArchiveService(
			getAdminArchiveRepository(),
			getUserRepository(),
			getGuildRepository(),
			getStorageService(),
			getSnowflakeService(),
			getWorkerService(),
		),
);
const getEntranceSoundRepository = singleton(() => new EntranceSoundRepository());
export const getEntranceSoundService = singleton(
	() => new EntranceSoundService(getEntranceSoundRepository(), getStorageService(), getMediaService()),
);
export const getEntranceSoundPlayService = singleton(
	() => new EntranceSoundPlayService(getEntranceSoundService(), getGatewayService(), getChannelRepository()),
);
export const getGatewayRequestService = singleton(() => new GatewayRequestService(getBotAuthService()));
export const getGuildDiscoveryService = singleton(
	() =>
		new GuildDiscoveryService(
			getGuildDiscoveryRepository(),
			getGuildRepository(),
			getGatewayService(),
			getGuildSearchService(),
		),
);
export const getReadStateRequestService = singleton(() => new ReadStateRequestService(getReadStateService()));
export const getUserCacheService = singleton(() => createUserCacheService());

export function createUserCacheService(): UserCacheService {
	return new UserCacheService(createUsersServiceClient());
}

interface ServiceSingletonInitializationOwner {
	stopping: boolean;
	snowflake: SnowflakeServiceHandle;
	limitConfigService: LimitConfigService | null;
}

let serviceSingletonInitializationOwner: ServiceSingletonInitializationOwner | null = null;
let serviceSingletonInitializationPromise: Promise<void> | null = null;
let serviceSingletonShutdownPromise: Promise<void> | null = null;

function assertServiceSingletonInitializationActive(owner: ServiceSingletonInitializationOwner): void {
	if (owner.stopping) {
		throw new Error('Service singleton initialization was stopped');
	}
}

export async function initializeServiceSingletons(): Promise<void> {
	if (serviceSingletonShutdownPromise) {
		throw new Error('Service singletons are shutting down');
	}
	if (!serviceSingletonInitializationPromise) {
		const owner: ServiceSingletonInitializationOwner = {
			stopping: false,
			snowflake: acquireSnowflakeService(),
			limitConfigService: null,
		};
		serviceSingletonInitializationOwner = owner;
		serviceSingletonInitializationPromise = (async () => {
			await owner.snowflake.service.initialize();
			assertServiceSingletonInitializationActive(owner);
			const limitConfigService = getLimitConfigService();
			owner.limitConfigService = limitConfigService;
			await getInstanceConfigRepository().initialize();
			assertServiceSingletonInitializationActive(owner);
			await limitConfigService.initialize();
			assertServiceSingletonInitializationActive(owner);
			limitConfigService.setAsGlobalInstance();
		})();
	}
	const initialization = serviceSingletonInitializationPromise;
	try {
		await initialization;
	} catch (error) {
		if (serviceSingletonInitializationPromise === initialization) {
			try {
				await shutdownServiceSingletons();
			} catch (cleanupError) {
				throw new AggregateError([error, cleanupError], 'Service singleton initialization and cleanup failed');
			}
		}
		throw error;
	}
}

export async function shutdownServiceSingletons(): Promise<void> {
	if (serviceSingletonShutdownPromise) {
		return await serviceSingletonShutdownPromise;
	}
	const initialization = serviceSingletonInitializationPromise;
	const owner = serviceSingletonInitializationOwner;
	const service = owner?.limitConfigService ?? limitConfigServiceInstance;
	if (!initialization && !owner && !service) return;
	if (owner) owner.stopping = true;
	const shutdown = awaitAll(
		[
			Promise.resolve().then(() => service?.shutdown()),
			(async () => {
				await Promise.allSettled([initialization]);
				if (owner) await releaseSnowflakeService(owner.snowflake);
			})(),
		],
		'Failed to shut down service singletons',
	);
	serviceSingletonShutdownPromise = shutdown;
	try {
		await shutdown;
	} finally {
		if (serviceSingletonInitializationPromise === initialization) {
			serviceSingletonInitializationPromise = null;
		}
		if (serviceSingletonInitializationOwner === owner) {
			serviceSingletonInitializationOwner = null;
		}
		if (serviceSingletonShutdownPromise === shutdown) {
			serviceSingletonShutdownPromise = null;
		}
	}
}

export function resetServiceSingletonsForTesting(): void {
	void shutdownServiceSingletons().catch((error) => {
		Logger.error({error}, 'Failed to reset service singletons');
	});
	activityTracker?.shutdown();
	clearSingletonsForTesting();
	_virusScanInitPromise = null;
	bulkMessageDeletionQueue = null;
	bulkMessageDeletionQueueClient = null;
	premiumStateQueue = null;
	premiumStateQueueClient = null;
	activityTracker = null;
	activityTrackerClient = null;
	activityBuffer = null;
	activityBufferClient = null;
	accountDeletionQueue = null;
	accountDeletionQueueClient = null;
}
