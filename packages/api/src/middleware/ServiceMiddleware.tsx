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

import {AdminRepository} from '@fluxer/api/src/admin/AdminRepository';
import {AdminService} from '@fluxer/api/src/admin/AdminService';
import {AdminApiKeyRepository} from '@fluxer/api/src/admin/repositories/AdminApiKeyRepository';
import {AdminArchiveRepository} from '@fluxer/api/src/admin/repositories/AdminArchiveRepository';
import {AdminApiKeyService} from '@fluxer/api/src/admin/services/AdminApiKeyService';
import {AdminArchiveService} from '@fluxer/api/src/admin/services/AdminArchiveService';
import {AlertService} from '@fluxer/api/src/alert/AlertService';
import {AuthRequestService} from '@fluxer/api/src/auth/AuthRequestService';
import {AuthService} from '@fluxer/api/src/auth/AuthService';
import {AuthMfaService} from '@fluxer/api/src/auth/services/AuthMfaService';
import {DesktopHandoffService} from '@fluxer/api/src/auth/services/DesktopHandoffService';
import {SsoService} from '@fluxer/api/src/auth/services/SsoService';
import {BlueskyOAuthService} from '@fluxer/api/src/bluesky/BlueskyOAuthService';
import type {IBlueskyOAuthService} from '@fluxer/api/src/bluesky/IBlueskyOAuthService';
import {Config} from '@fluxer/api/src/Config';
import {ChannelRepository} from '@fluxer/api/src/channel/ChannelRepository';
import {ChannelRequestService} from '@fluxer/api/src/channel/services/ChannelRequestService';
import {ChannelService} from '@fluxer/api/src/channel/services/ChannelService';
import {MessageRequestService} from '@fluxer/api/src/channel/services/message/MessageRequestService';
import {ScheduledMessageService} from '@fluxer/api/src/channel/services/ScheduledMessageService';
import {StreamPreviewService} from '@fluxer/api/src/channel/services/StreamPreviewService';
import {StreamService} from '@fluxer/api/src/channel/services/StreamService';
import {ConnectionRepository} from '@fluxer/api/src/connection/ConnectionRepository';
import {ConnectionRequestService} from '@fluxer/api/src/connection/ConnectionRequestService';
import {ConnectionService} from '@fluxer/api/src/connection/ConnectionService';
import {CsamEvidenceRetentionService} from '@fluxer/api/src/csam/CsamEvidenceRetentionService';
import {CsamLegalHoldService} from '@fluxer/api/src/csam/CsamLegalHoldService';
import {createNcmecApiConfig, NcmecReporter} from '@fluxer/api/src/csam/NcmecReporter';
import {NcmecSubmissionService} from '@fluxer/api/src/csam/NcmecSubmissionService';
import {DonationRepository} from '@fluxer/api/src/donation/DonationRepository';
import {DonationService} from '@fluxer/api/src/donation/DonationService';
import {DonationCheckoutService} from '@fluxer/api/src/donation/services/DonationCheckoutService';
import {DonationMagicLinkService} from '@fluxer/api/src/donation/services/DonationMagicLinkService';
import {DownloadService} from '@fluxer/api/src/download/DownloadService';
import {createEmailProvider} from '@fluxer/api/src/email/EmailProviderFactory';
import {FavoriteMemeRepository} from '@fluxer/api/src/favorite_meme/FavoriteMemeRepository';
import {FavoriteMemeRequestService} from '@fluxer/api/src/favorite_meme/FavoriteMemeRequestService';
import {FavoriteMemeService} from '@fluxer/api/src/favorite_meme/FavoriteMemeService';
import {GatewayRequestService} from '@fluxer/api/src/gateway/GatewayRequestService';
import {GuildAuditLogService} from '@fluxer/api/src/guild/GuildAuditLogService';
import {GuildDiscoveryRepository} from '@fluxer/api/src/guild/repositories/GuildDiscoveryRepository';
import {GuildRepository} from '@fluxer/api/src/guild/repositories/GuildRepository';
import {ExpressionAssetPurger} from '@fluxer/api/src/guild/services/content/ExpressionAssetPurger';
import {GuildDiscoveryService} from '@fluxer/api/src/guild/services/GuildDiscoveryService';
import {GuildService} from '@fluxer/api/src/guild/services/GuildService';
import {AssetDeletionQueue} from '@fluxer/api/src/infrastructure/AssetDeletionQueue';
import {AvatarService} from '@fluxer/api/src/infrastructure/AvatarService';
import {
	CloudflarePurgeQueue,
	type IPurgeQueue,
	NoopPurgeQueue,
} from '@fluxer/api/src/infrastructure/CloudflarePurgeQueue';
import {DisabledLiveKitService} from '@fluxer/api/src/infrastructure/DisabledLiveKitService';
import {DisabledVirusScanService} from '@fluxer/api/src/infrastructure/DisabledVirusScanService';
import {DiscriminatorService} from '@fluxer/api/src/infrastructure/DiscriminatorService';
import {EmailDnsValidationService} from '@fluxer/api/src/infrastructure/EmailDnsValidationService';
import {EmbedService} from '@fluxer/api/src/infrastructure/EmbedService';
import {EntityAssetService} from '@fluxer/api/src/infrastructure/EntityAssetService';
import {ErrorI18nService} from '@fluxer/api/src/infrastructure/ErrorI18nService';
import type {IAssetDeletionQueue} from '@fluxer/api/src/infrastructure/IAssetDeletionQueue';
import type {ILiveKitService} from '@fluxer/api/src/infrastructure/ILiveKitService';
import {InMemoryVoiceRoomStore} from '@fluxer/api/src/infrastructure/InMemoryVoiceRoomStore';
import type {IVoiceRoomStore} from '@fluxer/api/src/infrastructure/IVoiceRoomStore';
import {KVAccountDeletionQueueService} from '@fluxer/api/src/infrastructure/KVAccountDeletionQueueService';
import {KVActivityTracker} from '@fluxer/api/src/infrastructure/KVActivityTracker';
import {KVBulkMessageDeletionQueueService} from '@fluxer/api/src/infrastructure/KVBulkMessageDeletionQueueService';
import {LiveKitService} from '@fluxer/api/src/infrastructure/LiveKitService';
import {LiveKitWebhookService} from '@fluxer/api/src/infrastructure/LiveKitWebhookService';
import {createStorageService} from '@fluxer/api/src/infrastructure/StorageServiceFactory';
import {UnfurlerService} from '@fluxer/api/src/infrastructure/UnfurlerService';
import {UserCacheService} from '@fluxer/api/src/infrastructure/UserCacheService';
import {VirusScanService} from '@fluxer/api/src/infrastructure/VirusScanService';
import {VoiceRoomStore} from '@fluxer/api/src/infrastructure/VoiceRoomStore';
import {InstanceConfigRepository} from '@fluxer/api/src/instance/InstanceConfigRepository';
import {SnowflakeReservationRepository} from '@fluxer/api/src/instance/SnowflakeReservationRepository';
import {SnowflakeReservationService} from '@fluxer/api/src/instance/SnowflakeReservationService';
import {InviteRepository} from '@fluxer/api/src/invite/InviteRepository';
import {InviteRequestService} from '@fluxer/api/src/invite/InviteRequestService';
import {InviteService} from '@fluxer/api/src/invite/InviteService';
import {KlipyService} from '@fluxer/api/src/klipy/KlipyService';
import {LimitConfigService} from '@fluxer/api/src/limits/LimitConfigService';
import {
	ensureVoiceResourcesInitialized,
	getGatewayService,
	getInjectedBlueskyOAuthService,
	getInjectedS3Service,
	getKVClient,
	getLiveKitServiceInstance,
	getMediaService,
	getSnowflakeService,
	getVoiceAvailabilityService,
	getVoiceRoomStoreInstance,
	getVoiceTopology,
	getWorkerService,
} from '@fluxer/api/src/middleware/ServiceRegistry';
import {ApplicationService} from '@fluxer/api/src/oauth/ApplicationService';
import {BotAuthService} from '@fluxer/api/src/oauth/BotAuthService';
import {BotMfaMirrorService} from '@fluxer/api/src/oauth/BotMfaMirrorService';
import {OAuth2ApplicationsRequestService} from '@fluxer/api/src/oauth/OAuth2ApplicationsRequestService';
import {OAuth2RequestService} from '@fluxer/api/src/oauth/OAuth2RequestService';
import {OAuth2Service} from '@fluxer/api/src/oauth/OAuth2Service';
import {ApplicationRepository} from '@fluxer/api/src/oauth/repositories/ApplicationRepository';
import {OAuth2TokenRepository} from '@fluxer/api/src/oauth/repositories/OAuth2TokenRepository';
import {PackRepository} from '@fluxer/api/src/pack/PackRepository';
import {PackRequestService} from '@fluxer/api/src/pack/PackRequestService';
import {PackService} from '@fluxer/api/src/pack/PackService';
import {ReadStateRepository} from '@fluxer/api/src/read_state/ReadStateRepository';
import {ReadStateRequestService} from '@fluxer/api/src/read_state/ReadStateRequestService';
import {ReadStateService} from '@fluxer/api/src/read_state/ReadStateService';
import {ReportRepository} from '@fluxer/api/src/report/ReportRepository';
import {ReportRequestService} from '@fluxer/api/src/report/ReportRequestService';
import {ReportService} from '@fluxer/api/src/report/ReportService';
import {RpcService} from '@fluxer/api/src/rpc/RpcService';
import {getGuildSearchService, getReportSearchService} from '@fluxer/api/src/SearchFactory';
import {SearchService} from '@fluxer/api/src/search/SearchService';
import {StripeService} from '@fluxer/api/src/stripe/StripeService';
import {TenorService} from '@fluxer/api/src/tenor/TenorService';
import {ThemeService} from '@fluxer/api/src/theme/ThemeService';
import {GuildManagedTraitService} from '@fluxer/api/src/traits/GuildManagedTraitService';
import type {HonoEnv} from '@fluxer/api/src/types/HonoEnv';
import {EmailChangeRepository} from '@fluxer/api/src/user/repositories/auth/EmailChangeRepository';
import {PasswordChangeRepository} from '@fluxer/api/src/user/repositories/auth/PasswordChangeRepository';
import {ScheduledMessageRepository} from '@fluxer/api/src/user/repositories/ScheduledMessageRepository';
import {UserContactChangeLogRepository} from '@fluxer/api/src/user/repositories/UserContactChangeLogRepository';
import {UserRepository} from '@fluxer/api/src/user/repositories/UserRepository';
import {EmailChangeService} from '@fluxer/api/src/user/services/EmailChangeService';
import {PasswordChangeService} from '@fluxer/api/src/user/services/PasswordChangeService';
import {UserAccountRequestService} from '@fluxer/api/src/user/services/UserAccountRequestService';
import {UserAuthRequestService} from '@fluxer/api/src/user/services/UserAuthRequestService';
import {UserChannelRequestService} from '@fluxer/api/src/user/services/UserChannelRequestService';
import {UserContactChangeLogService} from '@fluxer/api/src/user/services/UserContactChangeLogService';
import {UserContentRequestService} from '@fluxer/api/src/user/services/UserContentRequestService';
import {UserRelationshipRequestService} from '@fluxer/api/src/user/services/UserRelationshipRequestService';
import {UserService} from '@fluxer/api/src/user/services/UserService';
import {UserPermissionUtils} from '@fluxer/api/src/utils/UserPermissionUtils';
import {VoiceRepository} from '@fluxer/api/src/voice/VoiceRepository';
import {VoiceService} from '@fluxer/api/src/voice/VoiceService';
import {SweegoWebhookService} from '@fluxer/api/src/webhook/SweegoWebhookService';
import {WebhookRepository} from '@fluxer/api/src/webhook/WebhookRepository';
import {WebhookRequestService} from '@fluxer/api/src/webhook/WebhookRequestService';
import {WebhookService} from '@fluxer/api/src/webhook/WebhookService';
import type {ICacheService} from '@fluxer/cache/src/ICacheService';
import {KVCacheProvider} from '@fluxer/cache/src/providers/KVCacheProvider';
import {EmailI18nService} from '@fluxer/email/src/EmailI18nService';
import type {EmailConfig, UserBouncedEmailChecker} from '@fluxer/email/src/EmailProviderTypes';
import {EmailService} from '@fluxer/email/src/EmailService';
import type {IEmailService} from '@fluxer/email/src/IEmailService';
import {TestEmailService} from '@fluxer/email/src/TestEmailService';
import {createMockLogger} from '@fluxer/logger/src/mock';
import {RateLimitService} from '@fluxer/rate_limit/src/RateLimitService';
import type {ISmsProvider} from '@fluxer/sms/src/providers/ISmsProvider';
import {createSmsProvider} from '@fluxer/sms/src/providers/SmsProviderFactory';
import {SmsService} from '@fluxer/sms/src/SmsService';
import {createMiddleware} from 'hono/factory';

const errorI18nService = new ErrorI18nService();

let _testEmailService: TestEmailService | null = null;
function getTestEmailService(): TestEmailService {
	if (!_testEmailService) {
		_testEmailService = new TestEmailService();
	}
	return _testEmailService;
}

function getVirusScanService() {
	return Config.clamav.enabled ? VirusScanService : DisabledVirusScanService;
}

let _cacheService: ICacheService | null = null;
function getCacheService(): ICacheService {
	if (!_cacheService) {
		_cacheService = new KVCacheProvider({client: getKVClient()});
	}
	return _cacheService;
}

let _rateLimitService: RateLimitService | null = null;
function getRateLimitService(): RateLimitService {
	if (!_rateLimitService) {
		_rateLimitService = new RateLimitService(getCacheService());
	}
	return _rateLimitService;
}

let _emailDnsValidationService: EmailDnsValidationService | null = null;
function getEmailDnsValidationService(): EmailDnsValidationService {
	if (!_emailDnsValidationService) {
		_emailDnsValidationService = new EmailDnsValidationService();
	}
	return _emailDnsValidationService;
}

function createRuntimeSmsProvider(): ISmsProvider {
	if (Config.dev.testModeEnabled) {
		return createSmsProvider({
			mode: 'test',
			logger: createMockLogger(),
		});
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

	return createSmsProvider({
		mode: 'unavailable',
	});
}

let _purgeQueue: IPurgeQueue | null = null;
function getPurgeQueue(): IPurgeQueue {
	if (!_purgeQueue) {
		_purgeQueue = Config.cloudflare.purgeEnabled ? new CloudflarePurgeQueue(getKVClient()) : new NoopPurgeQueue();
	}
	return _purgeQueue;
}

let _assetDeletionQueue: IAssetDeletionQueue | null = null;
function getAssetDeletionQueue(): IAssetDeletionQueue {
	if (!_assetDeletionQueue) {
		_assetDeletionQueue = new AssetDeletionQueue(getKVClient());
	}
	return _assetDeletionQueue;
}

let _csamLegalHoldService: CsamLegalHoldService | null = null;
function getCsamLegalHoldService(): CsamLegalHoldService {
	if (!_csamLegalHoldService) {
		_csamLegalHoldService = new CsamLegalHoldService();
	}
	return _csamLegalHoldService;
}

const instanceConfigRepository = new InstanceConfigRepository();
let _limitConfigService: LimitConfigService | null = null;
function getLimitConfigService(): LimitConfigService {
	if (!_limitConfigService) {
		_limitConfigService = new LimitConfigService(instanceConfigRepository, getCacheService(), getKVClient());
	}
	return _limitConfigService;
}

const snowflakeReservationRepository = new SnowflakeReservationRepository();
let _snowflakeReservationService: SnowflakeReservationService | null = null;
function getSnowflakeReservationService(): SnowflakeReservationService {
	if (!_snowflakeReservationService) {
		_snowflakeReservationService = new SnowflakeReservationService(snowflakeReservationRepository, getKVClient());
	}
	return _snowflakeReservationService;
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

			const snowflakeReservationService = getSnowflakeReservationService();
			await snowflakeReservationService.initialize();
		})();
	}

	await serviceSingletonInitializationPromise;
}

let _alertService: AlertService | null = null;
function getAlertService(): AlertService {
	if (!_alertService) {
		_alertService = new AlertService(Config.alerts.webhookUrl);
	}
	return _alertService;
}

let _liveKitWebhookService: LiveKitWebhookService | null = null;
function getLiveKitWebhookService(): LiveKitWebhookService | null {
	if (!_liveKitWebhookService) {
		const voiceTopology = getVoiceTopology();
		if (!voiceTopology) return null;

		const gatewayService = getGatewayService();
		const userRepository = new UserRepository();
		const liveKitService: ILiveKitService = getLiveKitServiceInstance() ?? new DisabledLiveKitService();
		const voiceRoomStore: IVoiceRoomStore = getVoiceRoomStoreInstance() ?? new InMemoryVoiceRoomStore();
		const limitConfigService = getLimitConfigService();

		const hasVoiceInfrastructure =
			Config.voice.enabled &&
			voiceTopology !== null &&
			liveKitService instanceof LiveKitService &&
			voiceRoomStore instanceof VoiceRoomStore;

		if (hasVoiceInfrastructure && voiceTopology) {
			_liveKitWebhookService = new LiveKitWebhookService(
				voiceRoomStore,
				gatewayService,
				userRepository,
				liveKitService,
				voiceTopology,
				limitConfigService,
			);
		}
	}
	return _liveKitWebhookService;
}

export const ServiceMiddleware = createMiddleware<HonoEnv>(async (ctx, next) => {
	const snowflakeService = getSnowflakeService();
	const limitConfigService = getLimitConfigService();

	const snowflakeReservationService = getSnowflakeReservationService();

	const userRepository = new UserRepository();
	const guildRepository = new GuildRepository();
	const channelRepository = new ChannelRepository();
	const inviteRepository = new InviteRepository();
	const webhookRepository = new WebhookRepository();
	const readStateRepository = new ReadStateRepository();
	const favoriteMemeRepository = new FavoriteMemeRepository();
	const connectionRepository = new ConnectionRepository();
	const reportRepository = new ReportRepository();
	const adminRepository = new AdminRepository();
	const adminArchiveRepository = new AdminArchiveRepository();
	const voiceRepository = new VoiceRepository();
	const applicationRepository = new ApplicationRepository();
	const oauth2TokenRepository = new OAuth2TokenRepository();

	const cacheService = getCacheService();
	const kvClient = getKVClient();
	const rateLimitService = getRateLimitService();
	const emailDnsValidationService = getEmailDnsValidationService();
	const purgeQueue = getPurgeQueue();
	const assetDeletionQueue = getAssetDeletionQueue();
	const csamLegalHoldService = getCsamLegalHoldService();

	const userCacheService = new UserCacheService(cacheService, userRepository);
	const kvAccountDeletionQueue = new KVAccountDeletionQueueService(kvClient, userRepository);
	const kvBulkMessageDeletionQueue = new KVBulkMessageDeletionQueueService(kvClient);
	const kvActivityTracker = new KVActivityTracker(kvClient);
	const mediaService = getMediaService();
	const storageService = createStorageService({s3Service: getInjectedS3Service()});
	const downloadService = new DownloadService(storageService);
	const themeService = new ThemeService(storageService);
	const csamEvidenceRetentionService = new CsamEvidenceRetentionService(storageService);
	const gatewayService = getGatewayService();
	const guildManagedTraitService = new GuildManagedTraitService({
		userRepository,
		guildRepository,
		gatewayService,
		userCacheService,
	});
	const alertService = getAlertService();
	const workerService = getWorkerService();
	const botMfaMirrorService = new BotMfaMirrorService(applicationRepository, userRepository, gatewayService);

	const unfurlerService = new UnfurlerService(cacheService, mediaService);
	const embedService = new EmbedService(channelRepository, cacheService, unfurlerService, mediaService, workerService);
	const readStateService = new ReadStateService(readStateRepository, gatewayService);
	const avatarService = new AvatarService(storageService, mediaService, limitConfigService);
	const entityAssetService = new EntityAssetService(
		storageService,
		mediaService,
		assetDeletionQueue,
		limitConfigService,
	);

	const emailConfig: EmailConfig = {
		enabled: Config.email.enabled,
		fromEmail: Config.email.fromEmail,
		fromName: Config.email.fromName,
		appBaseUrl: Config.endpoints.webApp,
		marketingBaseUrl: Config.endpoints.marketing,
	};

	const bouncedEmailChecker: UserBouncedEmailChecker = {
		isEmailBounced: async (email: string) => {
			const user = await userRepository.findByEmail(email);
			return user?.emailBounced ?? false;
		},
	};

	let emailService: IEmailService;
	if (Config.dev.testModeEnabled) {
		emailService = getTestEmailService();
	} else {
		const emailI18n = new EmailI18nService();
		const emailProvider = createEmailProvider(Config.email);
		emailService = new EmailService(emailConfig, emailI18n, emailProvider, bouncedEmailChecker);
	}

	const smsProvider = createRuntimeSmsProvider();
	const smsService = new SmsService(smsProvider);
	const VirusScanServiceClass = getVirusScanService();
	const virusScanService = new VirusScanServiceClass(cacheService);
	await virusScanService.initialize();

	await ensureVoiceResourcesInitialized();
	const liveKitService: ILiveKitService = getLiveKitServiceInstance() ?? new DisabledLiveKitService();
	const voiceRoomStore: IVoiceRoomStore = getVoiceRoomStoreInstance() ?? new InMemoryVoiceRoomStore();

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
		limitConfigService,
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
		purgeQueue,
		favoriteMemeRepository,
		guildAuditLogService,
		voiceRoomStore,
		liveKitService,
		inviteRepository,
		webhookRepository,
		limitConfigService,
		getVoiceAvailabilityService() ?? undefined,
	);
	const channelRequestService = new ChannelRequestService(channelService, userCacheService);
	const messageRequestService = new MessageRequestService(
		channelService,
		channelRepository,
		userCacheService,
		mediaService,
	);

	const scheduledMessageRepository = new ScheduledMessageRepository();
	const scheduledMessageService = new ScheduledMessageService(
		channelService,
		scheduledMessageRepository,
		workerService,
		snowflakeService,
	);

	const streamPreviewService = new StreamPreviewService(storageService, cacheService);
	const streamService = new StreamService(cacheService, channelService, gatewayService, streamPreviewService);

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
		limitConfigService,
		guildManagedTraitService,
	);

	const discoveryRepository = new GuildDiscoveryRepository();
	const discoveryService = new GuildDiscoveryService(
		discoveryRepository,
		guildRepository,
		gatewayService,
		getGuildSearchService(),
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
		limitConfigService,
	);
	const inviteRequestService = new InviteRequestService(
		inviteService,
		channelService,
		guildService,
		gatewayService,
		packRepository,
		userCacheService,
	);

	const injectedBlueskyOAuth = getInjectedBlueskyOAuthService();
	let blueskyOAuthService: IBlueskyOAuthService | null = injectedBlueskyOAuth ?? null;
	if (!blueskyOAuthService && Config.auth.bluesky.enabled) {
		blueskyOAuthService = await BlueskyOAuthService.create(Config.auth.bluesky, kvClient, Config.endpoints.apiPublic);
	}

	const connectionService = new ConnectionService(connectionRepository, gatewayService, blueskyOAuthService);

	const favoriteMemeService = new FavoriteMemeService(
		favoriteMemeRepository,
		channelService,
		storageService,
		mediaService,
		snowflakeService,
		gatewayService,
		unfurlerService,
		limitConfigService,
	);

	const discriminatorService = new DiscriminatorService(userRepository, cacheService, limitConfigService);

	const voiceTopology = getVoiceTopology();
	const voiceAvailabilityService = getVoiceAvailabilityService();
	const hasVoiceInfrastructure =
		Config.voice.enabled &&
		voiceTopology !== null &&
		liveKitService instanceof LiveKitService &&
		voiceRoomStore instanceof VoiceRoomStore;

	const liveKitWebhookService = hasVoiceInfrastructure ? getLiveKitWebhookService() : undefined;

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
		emailDnsValidationService,
		smsService,
		snowflakeService,
		snowflakeReservationService,
		discriminatorService,
		kvAccountDeletionQueue,
		kvActivityTracker,
		contactChangeLogService,
		botMfaMirrorService,
		authMfaService,
	);
	const ssoService = new SsoService(
		instanceConfigRepository,
		cacheService,
		userRepository,
		discriminatorService,
		snowflakeService,
		authService,
		kvActivityTracker,
	);
	const desktopHandoffService = new DesktopHandoffService(cacheService);
	const authRequestService = new AuthRequestService(authService, ssoService, cacheService, desktopHandoffService);

	const reportSearchService = getReportSearchService();
	const reportService = new ReportService(
		reportRepository,
		channelRepository,
		guildRepository,
		userRepository,
		inviteRepository,
		emailService,
		emailDnsValidationService,
		snowflakeService,
		storageService,
		reportSearchService,
	);
	const reportRequestService = new ReportRequestService(reportService);

	const adminService = new AdminService(
		userRepository,
		guildRepository,
		channelRepository,
		adminRepository,
		inviteRepository,
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
		kvBulkMessageDeletionQueue,
	);

	const adminArchiveService = new AdminArchiveService(
		adminArchiveRepository,
		userRepository,
		guildRepository,
		storageService,
		snowflakeService,
		workerService,
	);

	const adminApiKeyRepository = new AdminApiKeyRepository();
	const adminApiKeyService = new AdminApiKeyService(adminApiKeyRepository, snowflakeService);

	const botAuthService = new BotAuthService(applicationRepository);
	const gatewayRequestService = new GatewayRequestService(botAuthService);

	const rpcService = new RpcService(
		userRepository,
		applicationRepository,
		guildRepository,
		channelRepository,
		userCacheService,
		readStateService,
		authService,
		gatewayService,
		alertService,
		discriminatorService,
		favoriteMemeRepository,
		packService,
		botAuthService,
		inviteRepository,
		webhookRepository,
		storageService,
		avatarService,
		rateLimitService,
		mediaService,
		limitConfigService,
		voiceService,
		voiceAvailabilityService ?? undefined,
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
		limitConfigService,
	);

	const klipyService = new KlipyService(cacheService, mediaService);
	const tenorService = new TenorService(cacheService, mediaService);

	const emailChangeRepository = new EmailChangeRepository();
	const emailChangeService = new EmailChangeService(
		emailChangeRepository,
		emailService,
		userRepository,
		rateLimitService,
		emailDnsValidationService,
	);

	const passwordChangeRepository = new PasswordChangeRepository();
	const passwordChangeService = new PasswordChangeService(
		passwordChangeRepository,
		emailService,
		authService,
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
		kvAccountDeletionQueue,
		kvBulkMessageDeletionQueue,
		botMfaMirrorService,
		contactChangeLogService,
		connectionRepository,
		limitConfigService,
	);
	const userAccountRequestService = new UserAccountRequestService(
		authService,
		authMfaService,
		emailChangeService,
		userService,
		userRepository,
		userCacheService,
		mediaService,
	);
	const userAuthRequestService = new UserAuthRequestService(authService, authMfaService, userService, userRepository);
	const userChannelRequestService = new UserChannelRequestService(userService, userCacheService);
	const userContentRequestService = new UserContentRequestService(userService, userCacheService, mediaService);
	const userRelationshipRequestService = new UserRelationshipRequestService(userService, userCacheService);

	const donationRepository = new DonationRepository();
	let stripeService: StripeService | null = null;
	let donationService: DonationService | null = null;
	if (!Config.instance.selfHosted) {
		stripeService = new StripeService(
			userRepository,
			userCacheService,
			authService,
			gatewayService,
			emailService,
			guildRepository,
			guildService,
			cacheService,
			donationRepository,
		);

		const donationMagicLinkService = new DonationMagicLinkService(
			donationRepository,
			emailService,
			emailDnsValidationService,
		);
		const donationCheckoutService = new DonationCheckoutService(
			stripeService.getStripe(),
			donationRepository,
			emailDnsValidationService,
		);
		donationService = new DonationService(donationMagicLinkService, donationCheckoutService);
	}

	const sweegoWebhookService = new SweegoWebhookService(userRepository, gatewayService);

	const applicationService = new ApplicationService({
		applicationRepository,
		channelRepository,
		userRepository,
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
	const oauth2RequestService = new OAuth2RequestService(
		oauth2Service,
		applicationRepository,
		oauth2TokenRepository,
		userRepository,
		botAuthService,
		applicationService,
		gatewayService,
		guildService,
		authService,
		authMfaService,
	);
	const oauth2ApplicationsRequestService = new OAuth2ApplicationsRequestService(
		applicationService,
		applicationRepository,
		userRepository,
		authService,
		authMfaService,
	);

	const searchService = new SearchService({
		channelRepository,
		channelService,
		guildService,
		userRepository,
		userCacheService,
		mediaService,
		workerService,
	});
	const connectionRequestService = new ConnectionRequestService(
		connectionService,
		Config.auth.connectionInitiationSecret,
	);
	const favoriteMemeRequestService = new FavoriteMemeRequestService(favoriteMemeService);
	const webhookRequestService = new WebhookRequestService(
		webhookService,
		channelRepository,
		userCacheService,
		mediaService,
		liveKitWebhookService ?? null,
		sweegoWebhookService,
	);
	const packRequestService = new PackRequestService(packService);

	ctx.set('adminService', adminService);
	ctx.set('adminArchiveService', adminArchiveService);
	ctx.set('adminApiKeyService', adminApiKeyService);
	ctx.set('applicationRepository', applicationRepository);
	ctx.set('applicationService', applicationService);
	ctx.set('authMfaService', authMfaService);
	ctx.set('authService', authService);
	ctx.set('authRequestService', authRequestService);
	ctx.set('ssoService', ssoService);
	ctx.set('botAuthService', botAuthService);
	ctx.set('cacheService', cacheService);
	ctx.set('channelService', channelService);
	ctx.set('channelRequestService', channelRequestService);
	ctx.set('messageRequestService', messageRequestService);
	ctx.set('channelRepository', channelRepository);
	ctx.set('connectionService', connectionService);
	ctx.set('connectionRequestService', connectionRequestService);
	ctx.set('blueskyOAuthService', blueskyOAuthService);
	ctx.set('streamPreviewService', streamPreviewService);
	ctx.set('streamService', streamService);
	ctx.set('downloadService', downloadService);
	ctx.set('desktopHandoffService', desktopHandoffService);
	ctx.set('emailService', emailService);
	ctx.set('embedService', embedService);
	ctx.set('entityAssetService', entityAssetService);
	ctx.set('favoriteMemeService', favoriteMemeService);
	ctx.set('favoriteMemeRequestService', favoriteMemeRequestService);
	ctx.set('gatewayService', gatewayService);
	ctx.set('gatewayRequestService', gatewayRequestService);
	ctx.set('alertService', alertService);
	ctx.set('guildService', guildService);
	ctx.set('discoveryService', discoveryService);
	ctx.set('emailChangeService', emailChangeService);
	ctx.set('passwordChangeService', passwordChangeService);
	ctx.set('inviteService', inviteService);
	ctx.set('inviteRequestService', inviteRequestService);
	ctx.set('packService', packService);
	ctx.set('packRequestService', packRequestService);
	ctx.set('packRepository', packRepository);
	if (liveKitWebhookService) {
		ctx.set('liveKitWebhookService', liveKitWebhookService);
	}
	ctx.set('mediaService', mediaService);
	ctx.set('oauth2Service', oauth2Service);
	ctx.set('oauth2RequestService', oauth2RequestService);
	ctx.set('oauth2ApplicationsRequestService', oauth2ApplicationsRequestService);
	ctx.set('oauth2TokenRepository', oauth2TokenRepository);
	ctx.set('rateLimitService', rateLimitService);
	ctx.set('readStateService', readStateService);
	ctx.set('readStateRequestService', new ReadStateRequestService(readStateService));
	ctx.set('kvActivityTracker', kvActivityTracker);
	ctx.set('reportService', reportService);
	ctx.set('reportRequestService', reportRequestService);
	ctx.set('rpcService', rpcService);
	ctx.set('searchService', searchService);
	ctx.set('sweegoWebhookService', sweegoWebhookService);
	ctx.set('snowflakeService', snowflakeService);
	ctx.set('storageService', storageService);
	ctx.set('themeService', themeService);
	if (stripeService) {
		ctx.set('stripeService', stripeService);
	}
	if (donationService) {
		ctx.set('donationService', donationService);
	}
	ctx.set('sudoModeValid', false);
	ctx.set('klipyService', klipyService);
	ctx.set('tenorService', tenorService);
	ctx.set('userCacheService', userCacheService);
	ctx.set('userRepository', userRepository);
	ctx.set('userService', userService);
	ctx.set('userAccountRequestService', userAccountRequestService);
	ctx.set('userAuthRequestService', userAuthRequestService);
	ctx.set('userChannelRequestService', userChannelRequestService);
	ctx.set('userContentRequestService', userContentRequestService);
	ctx.set('userRelationshipRequestService', userRelationshipRequestService);
	ctx.set('scheduledMessageService', scheduledMessageService);
	ctx.set('webhookService', webhookService);
	ctx.set('webhookRequestService', webhookRequestService);
	ctx.set('workerService', workerService);
	ctx.set('contactChangeLogService', contactChangeLogService);
	ctx.set('csamLegalHoldService', csamLegalHoldService);
	ctx.set('csamEvidenceRetentionService', csamEvidenceRetentionService);
	ctx.set('instanceConfigRepository', instanceConfigRepository);
	ctx.set('limitConfigService', limitConfigService);
	ctx.set('guildManagedTraitService', guildManagedTraitService);
	ctx.set('errorI18nService', errorI18nService);

	const ncmecReporter = new NcmecReporter({config: createNcmecApiConfig(), fetch});
	const ncmecSubmissionService = new NcmecSubmissionService({
		reportRepository,
		ncmecApi: ncmecReporter,
		storageService,
	});
	ctx.set('ncmecSubmissionService', ncmecSubmissionService);

	await next();
});
