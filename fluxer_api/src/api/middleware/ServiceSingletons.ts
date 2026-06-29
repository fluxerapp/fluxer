// SPDX-License-Identifier: AGPL-3.0-or-later

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
import {AdminRepository} from '../admin/AdminRepository';
import {AdminApiKeyRepository} from '../admin/repositories/AdminApiKeyRepository';
import {AdminArchiveRepository} from '../admin/repositories/AdminArchiveRepository';
import {AdminApiKeyService} from '../admin/services/AdminApiKeyService';
import {AdminArchiveService} from '../admin/services/AdminArchiveService';
import {AdminAuditService} from '../admin/services/AdminAuditService';
import {PhoneAttemptRiskService} from '../auth/services/PhoneAttemptRiskService';
import {PhoneFraudGraphService} from '../auth/services/PhoneFraudGraphService';
import type {UserID} from '../BrandedTypes';
import {Config} from '../Config';
import {ChannelRepository} from '../channel/ChannelRepository';
import {AttachmentUploadTraceRepository} from '../channel/repositories/message/AttachmentUploadTraceRepository';
import {StreamPreviewService} from '../channel/services/StreamPreviewService';
import type {APIConfig} from '../config/APIConfig';
import {ConnectionRepository} from '../connection/ConnectionRepository';
import {createNcmecApiConfig, NcmecReporter} from '../csam/NcmecReporter';
import {NcmecRepository} from '../csam/NcmecRepository';
import {NcmecSubmissionService} from '../csam/NcmecSubmissionService';
import {DonationRepository} from '../donation/DonationRepository';
import {DownloadService} from '../download/DownloadService';
import {createEmailProvider} from '../email/EmailProviderFactory';
import {FavoriteMemeRepository} from '../favorite_meme/FavoriteMemeRepository';
import {GifService} from '../gif/GifService';
import {createNatsGifProvider} from '../gif/NatsGifProvider';
import {GuildAuditLogService} from '../guild/GuildAuditLogService';
import {GuildDiscoveryRepository} from '../guild/repositories/GuildDiscoveryRepository';
import {GuildRepository} from '../guild/repositories/GuildRepository';
import {ExpressionAssetPurger} from '../guild/services/content/ExpressionAssetPurger';
import {AssetDeletionQueue} from '../infrastructure/AssetDeletionQueue';
import {AvatarService} from '../infrastructure/AvatarService';
import {BunnyPurgeQueue, type IPurgeQueue, NoopPurgeQueue} from '../infrastructure/BunnyPurgeQueue';
import {DisabledVirusScanService} from '../infrastructure/DisabledVirusScanService';
import {DiscriminatorService} from '../infrastructure/DiscriminatorService';
import {EmailDnsValidationService} from '../infrastructure/EmailDnsValidationService';
import {EmbedService} from '../infrastructure/EmbedService';
import {EntityAssetService} from '../infrastructure/EntityAssetService';
import {ErrorI18nService} from '../infrastructure/ErrorI18nService';
import type {IAssetDeletionQueue} from '../infrastructure/IAssetDeletionQueue';
import type {IStorageService} from '../infrastructure/IStorageService';
import type {IUnfurlerService} from '../infrastructure/IUnfurlerService';
import {KVAccountDeletionQueueService} from '../infrastructure/KVAccountDeletionQueueService';
import {KVActivityTracker} from '../infrastructure/KVActivityTracker';
import {KVBulkMessageDeletionQueueService} from '../infrastructure/KVBulkMessageDeletionQueueService';
import {NatsUnfurlerService} from '../infrastructure/NatsUnfurlerService';
import {PremiumStateReconciliationQueueService} from '../infrastructure/PremiumStateReconciliationQueueService';
import {createStorageService} from '../infrastructure/StorageServiceFactory';
import {UserCacheService} from '../infrastructure/UserCacheService';
import {createUsersServiceClient} from '../infrastructure/UsersServiceClient';
import {VirusScanService} from '../infrastructure/VirusScanService';
import {GatewayRolloutConfigPublisher} from '../instance/GatewayRolloutConfigPublisher';
import {InstanceConfigRepository} from '../instance/InstanceConfigRepository';
import {InviteRepository} from '../invite/InviteRepository';
import {Logger} from '../Logger';
import {LimitConfigService} from '../limits/LimitConfigService';
import {BotAuthService} from '../oauth/BotAuthService';
import {BotMfaMirrorService} from '../oauth/BotMfaMirrorService';
import {ApplicationRepository} from '../oauth/repositories/ApplicationRepository';
import {OAuth2TokenRepository} from '../oauth/repositories/OAuth2TokenRepository';
import {PackRepository} from '../pack/PackRepository';
import {ReadStateRepository} from '../read_state/ReadStateRepository';
import {ReadStateService} from '../read_state/ReadStateService';
import {ReportRepository} from '../report/ReportRepository';
import {ThemeService} from '../theme/ThemeService';
import {EmailChangeRepository} from '../user/repositories/auth/EmailChangeRepository';
import {PasswordChangeRepository} from '../user/repositories/auth/PasswordChangeRepository';
import {ScheduledMessageRepository} from '../user/repositories/ScheduledMessageRepository';
import {UserContactChangeLogRepository} from '../user/repositories/UserContactChangeLogRepository';
import {UserRepository} from '../user/repositories/UserRepository';
import {VisionarySlotRepository} from '../user/repositories/VisionarySlotRepository';
import {UserActivityBuffer} from '../user/services/UserActivityBuffer';
import {UserContactChangeLogService} from '../user/services/UserContactChangeLogService';
import {UserPermissionUtils} from '../utils/UserPermissionUtils';
import {VoiceRepository} from '../voice/VoiceRepository';
import {SweegoWebhookService} from '../webhook/SweegoWebhookService';
import {WebhookRepository} from '../webhook/WebhookRepository';
import {
	getGatewayService,
	getKVClient,
	getMediaService,
	getSnowflakeService,
	getWorkerService,
} from './ServiceRegistry';
import {singleton} from './Singleton';

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
export const getPackRepository = singleton(() => new PackRepository());
export const getGuildDiscoveryRepository = singleton(() => new GuildDiscoveryRepository());
export const getScheduledMessageRepository = singleton(() => new ScheduledMessageRepository());
export const getEmailChangeRepository = singleton(() => new EmailChangeRepository());
export const getPasswordChangeRepository = singleton(() => new PasswordChangeRepository());
const getUserContactChangeLogRepository = singleton(() => new UserContactChangeLogRepository());
export const getDonationRepository = singleton(() => new DonationRepository());
const getAdminApiKeyRepository = singleton(() => new AdminApiKeyRepository());
export const getInstanceConfigRepository = singleton(() => new InstanceConfigRepository(getKVClient()));
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
		appBaseUrl: Config.endpoints.webApp,
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
export const getLimitConfigService = singleton(
	() => new LimitConfigService(getInstanceConfigRepository(), getCacheService(), getKVClient()),
);
export const getPurgeQueue: () => IPurgeQueue = singleton(() =>
	Config.bunny.purgeEnabled ? new BunnyPurgeQueue(getKVClient()) : new NoopPurgeQueue(),
);
export const getAssetDeletionQueue: () => IAssetDeletionQueue = singleton(() => new AssetDeletionQueue(getKVClient()));

let bulkMessageDeletionQueueClient: IKVProvider | null = null;
let bulkMessageDeletionQueue: KVBulkMessageDeletionQueueService | null = null;

export function getKVBulkMessageDeletionQueue(): KVBulkMessageDeletionQueueService {
	const kvClient = getKVClient();
	if (!bulkMessageDeletionQueue || bulkMessageDeletionQueueClient !== kvClient) {
		bulkMessageDeletionQueue = new KVBulkMessageDeletionQueueService(kvClient);
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

export const getDownloadService = singleton(() => new DownloadService(getStorageService()));
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
export const getExpressionAssetPurger = singleton(() => new ExpressionAssetPurger(getAssetDeletionQueue()));
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

export function createUserCacheService(): UserCacheService {
	return new UserCacheService(createUsersServiceClient());
}

let serviceSingletonInitializationPromise: Promise<void> | null = null;

export async function initializeServiceSingletons(): Promise<void> {
	if (!serviceSingletonInitializationPromise) {
		serviceSingletonInitializationPromise = (async () => {
			const snowflakeService = getSnowflakeService();
			await snowflakeService.initialize();
			const limitConfigService = getLimitConfigService();
			await limitConfigService.initialize();
			limitConfigService.setAsGlobalInstance();
		})();
	}
	await serviceSingletonInitializationPromise;
}
