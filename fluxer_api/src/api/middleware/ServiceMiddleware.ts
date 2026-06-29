// SPDX-License-Identifier: AGPL-3.0-or-later

import crypto from 'node:crypto';
import {createIpInfoService, createUnavailableIpInfoService, type IpInfoService} from '@pkgs/geoip/src/IpInfoService';
import {createMiddleware} from 'hono/factory';
import {AdminService} from '../admin/AdminService';
import {AdminArchiveService} from '../admin/services/AdminArchiveService';
import {AuthRequestService} from '../auth/AuthRequestService';
import {DesktopHandoffService} from '../auth/services/DesktopHandoffService';
import {
	CassandraInboundSmsChallengeRepository,
	InboundSmsChallengeService,
} from '../auth/services/InboundSmsChallengeService';
import type {IRegistrationRiskEvaluator} from '../auth/services/IRegistrationRiskEvaluator';
import {noopRegistrationRiskEvaluator, RegistrationRiskEvaluator} from '../auth/services/RegistrationRiskEvaluator';
import {SsoService} from '../auth/services/SsoService';
import {Config} from '../Config';
import {createApiContext} from '../CreateApiContext';
import {ChannelRequestService} from '../channel/services/ChannelRequestService';
import {MessageRequestService} from '../channel/services/message/MessageRequestService';
import {createMessageResponseDataService} from '../channel/services/message/MessageResponseDataService';
import {ScheduledMessageService} from '../channel/services/ScheduledMessageService';
import {StreamService} from '../channel/services/StreamService';
import {ConnectionRequestService} from '../connection/ConnectionRequestService';
import {ConnectionService} from '../connection/ConnectionService';
import {DonationService} from '../donation/DonationService';
import {DonationCheckoutService} from '../donation/services/DonationCheckoutService';
import {DonationMagicLinkService} from '../donation/services/DonationMagicLinkService';
import {FavoriteMemeRequestService} from '../favorite_meme/FavoriteMemeRequestService';
import {FavoriteMemeService} from '../favorite_meme/FavoriteMemeService';
import {GatewayRequestService} from '../gateway/GatewayRequestService';
import {GuildDiscoveryService} from '../guild/services/GuildDiscoveryService';
import {DisabledLiveKitService} from '../infrastructure/DisabledLiveKitService';
import type {ILiveKitService} from '../infrastructure/ILiveKitService';
import {InMemoryVoiceRoomStore} from '../infrastructure/InMemoryVoiceRoomStore';
import type {IVoiceRoomStore} from '../infrastructure/IVoiceRoomStore';
import {LiveKitService} from '../infrastructure/LiveKitService';
import {LiveKitWebhookService} from '../infrastructure/LiveKitWebhookService';
import {VoiceRoomStore} from '../infrastructure/VoiceRoomStore';
import {SingleCommunityService} from '../instance/SingleCommunityService';
import {InviteRequestService} from '../invite/InviteRequestService';
import {JobLedgerRepository} from '../jobs/JobLedgerRepository';
import {Logger} from '../Logger';
import {ApplicationService} from '../oauth/ApplicationService';
import {OAuth2ApplicationsRequestService} from '../oauth/OAuth2ApplicationsRequestService';
import {OAuth2RequestService} from '../oauth/OAuth2RequestService';
import {OAuth2Service} from '../oauth/OAuth2Service';
import {ReadStateRequestService} from '../read_state/ReadStateRequestService';
import {ReportRequestService} from '../report/ReportRequestService';
import {ReportService} from '../report/ReportService';
import type {IAccountPolicyEvaluator} from '../risk/AccountPolicyEvaluator';
import {
	getAccountPolicyEvaluator,
	setInjectedAccountPolicyEvaluator as setInjectedAccountPolicyEvaluatorInService,
} from '../risk/AccountPolicyService';
import {createIpInfoChecker} from '../risk/adapters/IpInfoAdapter';
import {createReverseDnsLookup} from '../risk/adapters/ReverseDnsAdapter';
import {DeterministicRiskEngine} from '../risk/DeterministicRiskEngine';
import {CassandraHistoricalOutcomeRepository} from '../risk/HistoricalOutcomeRepository';
import {buildIpInfoCache, buildIpInfoRequestAuditLogger} from '../risk/IpInfoCacheFactory';
import {CassandraRegistrationEventsRepository} from '../risk/RegistrationEventsRepository';
import {CassandraRiskAssessmentRepository} from '../risk/RiskAssessmentRepository';
import {buildRiskCacheLoaders} from '../risk/RiskCacheLoaders';
import {RiskCacheManager} from '../risk/RiskCacheManager';
import {createRiskToolbox} from '../risk/RiskToolboxFactory';
import {CassandraSuspiciousIpRepository} from '../risk/SuspiciousIpRepository';
import {RpcService} from '../rpc/RpcService';
import {getGuildSearchService, getReportSearchService} from '../SearchFactory';
import {SearchService} from '../search/SearchService';
import {StripeService} from '../stripe/StripeService';
import {AgeVerificationService} from '../stripe/services/AgeVerificationService';
import type {HonoEnv} from '../types/HonoEnv';
import {EntranceSoundPlayService} from '../user/entrance_sound/EntranceSoundPlayService';
import {EntranceSoundRepository} from '../user/entrance_sound/EntranceSoundRepository';
import {EntranceSoundService} from '../user/entrance_sound/EntranceSoundService';
import type {UserRepository} from '../user/repositories/UserRepository';
import {EmailChangeService} from '../user/services/EmailChangeService';
import {PasswordChangeService} from '../user/services/PasswordChangeService';
import {UserAccountRequestService} from '../user/services/UserAccountRequestService';
import {UserAuthRequestService} from '../user/services/UserAuthRequestService';
import {UserChannelRequestService} from '../user/services/UserChannelRequestService';
import {UserContentRequestService} from '../user/services/UserContentRequestService';
import {UserRelationshipRequestService} from '../user/services/UserRelationshipRequestService';
import {UserService} from '../user/services/UserService';
import {resolveRequestClientIp} from '../utils/IpUtils';
import {VoicePresenceHeartbeatStore} from '../voice/VoicePresenceHeartbeatStore';
import {VoiceService} from '../voice/VoiceService';
import {WebhookRequestService} from '../webhook/WebhookRequestService';
import {WebhookService} from '../webhook/WebhookService';
import {createGuildStackServices} from './GuildStackServiceFactory';
import {
	ensureVoiceResourcesInitialized,
	getBillingRepository,
	getGatewayService,
	getKVClient,
	getLiveKitServiceInstance,
	getMediaService,
	getSnowflakeService,
	getVoiceAvailabilityService,
	getVoiceRoomStoreInstance,
	getVoiceTopology,
	getWorkerService,
	resolveBlueskyOAuthService,
} from './ServiceRegistry';
import {
	createUserCacheService,
	ensureVirusScanInitialized,
	getAdminApiKeyService,
	getAdminArchiveRepository,
	getAdminRepository,
	getApplicationRepository,
	getAssetDeletionQueue,
	getAttachmentUploadTraceRepository,
	getAvatarService,
	getBotAuthService,
	getCacheService,
	getChannelRepository,
	getConnectionRepository,
	getContactChangeLogService,
	getDiscriminatorService,
	getDonationRepository,
	getDownloadService,
	getEmailChangeRepository,
	getEmailDnsValidationService,
	getEmailService,
	getEmbedService,
	getEntityAssetService,
	getErrorI18nService,
	getExpressionAssetPurger,
	getFavoriteMemeRepository,
	getGifService,
	getGuildAuditLogService,
	getGuildDiscoveryRepository,
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
	getPasswordChangeRepository,
	getPremiumStateReconciliationQueueService,
	getPurgeQueue,
	getRateLimitService,
	getReadStateService,
	getReportRepository,
	getScheduledMessageRepository,
	getStorageService,
	getStreamPreviewService,
	getSweegoWebhookService,
	getThemeService,
	getUnfurlerService,
	getUserActivityBuffer,
	getUserPermissionUtils,
	getUserRepository,
	getVirusScanServiceInstance,
	getVoiceRepository,
	getWebhookRepository,
} from './ServiceSingletons';

export {initializeServiceSingletons} from './ServiceSingletons';

let _reportService: ReportService | null = null;

function getReportServiceInstance(): ReportService {
	if (!_reportService) {
		_reportService = new ReportService(
			getReportRepository(),
			getChannelRepository(),
			getGuildRepository(),
			getUserRepository(),
			getInviteRepository(),
			getEmailService(),
			getEmailDnsValidationService(),
			getSnowflakeService(),
			getStorageService(),
			getGatewayService(),
			getRateLimitService(),
			getReportSearchService(),
		);
	}
	return _reportService;
}

export function shutdownReportService(): void {
	if (_reportService) {
		_reportService.shutdown();
		_reportService = null;
	}
}

let _riskCacheManager: RiskCacheManager | null = null;

function getRiskCacheManager(): RiskCacheManager {
	if (!_riskCacheManager) {
		_riskCacheManager = new RiskCacheManager({
			logger: Logger,
			...buildRiskCacheLoaders({
				adminRepository: getAdminRepository(),
			}),
		});
	}
	return _riskCacheManager;
}

export function getRiskCacheManagerInstance(): RiskCacheManager {
	return getRiskCacheManager();
}

let _inboundSmsChallengeService: InboundSmsChallengeService | null = null;

function getInboundSmsChallengeService(): InboundSmsChallengeService {
	if (!_inboundSmsChallengeService) {
		_inboundSmsChallengeService = new InboundSmsChallengeService(
			new CassandraInboundSmsChallengeRepository(),
			getKVClient(),
		);
	}
	return _inboundSmsChallengeService;
}

export function getInboundSmsChallengeServiceInstance(): InboundSmsChallengeService {
	return getInboundSmsChallengeService();
}

export function getUserRepositoryInstance(): UserRepository {
	return getUserRepository();
}

let _registrationEventsRepository: CassandraRegistrationEventsRepository | null = null;

function getRegistrationEventsRepository(): CassandraRegistrationEventsRepository {
	if (!_registrationEventsRepository) {
		_registrationEventsRepository = new CassandraRegistrationEventsRepository();
	}
	return _registrationEventsRepository;
}

let _riskAssessmentRepository: CassandraRiskAssessmentRepository | null = null;

function getRiskAssessmentRepository(): CassandraRiskAssessmentRepository {
	if (!_riskAssessmentRepository) {
		_riskAssessmentRepository = new CassandraRiskAssessmentRepository();
	}
	return _riskAssessmentRepository;
}

let _historicalOutcomeRepository: CassandraHistoricalOutcomeRepository | null = null;

function getHistoricalOutcomeRepository(): CassandraHistoricalOutcomeRepository {
	if (_historicalOutcomeRepository) return _historicalOutcomeRepository;
	_historicalOutcomeRepository = new CassandraHistoricalOutcomeRepository();
	return _historicalOutcomeRepository;
}

let _suspiciousIpRepository: CassandraSuspiciousIpRepository | null = null;

function getSuspiciousIpRepository(): CassandraSuspiciousIpRepository {
	if (_suspiciousIpRepository) return _suspiciousIpRepository;
	_suspiciousIpRepository = new CassandraSuspiciousIpRepository();
	return _suspiciousIpRepository;
}

let _ipInfoService: IpInfoService | null = null;
let _injectedIpInfoService: IpInfoService | undefined;

export function setInjectedIpInfoService(service: IpInfoService | undefined): void {
	_injectedIpInfoService = service;
}

export function getIpInfoService(): IpInfoService {
	if (_injectedIpInfoService) {
		return _injectedIpInfoService;
	}
	if (_ipInfoService) return _ipInfoService;
	if (!Config.risk.ipinfoApiKey) {
		_ipInfoService = createUnavailableIpInfoService('IPInfo API key not configured');
		return _ipInfoService;
	}
	const cache = buildIpInfoCache({
		hot: getCacheService(),
	});
	_ipInfoService = createIpInfoService({
		apiKey: Config.risk.ipinfoApiKey,
		cache,
		auditLogger: buildIpInfoRequestAuditLogger(),
	});
	return _ipInfoService;
}

let _registrationRiskEvaluator: IRegistrationRiskEvaluator | null = null;

export function setInjectedRegistrationRiskEvaluator(evaluator: IRegistrationRiskEvaluator | undefined): void {
	_registrationRiskEvaluator = evaluator ?? null;
}

export function setInjectedAccountPolicyEvaluator(evaluator: IAccountPolicyEvaluator | undefined): void {
	setInjectedAccountPolicyEvaluatorInService(evaluator);
}

function getRegistrationRiskEvaluator(): IRegistrationRiskEvaluator {
	if (_registrationRiskEvaluator) return _registrationRiskEvaluator;
	if (!Config.risk.enabled) {
		Logger.warn(
			{},
			'[ServiceMiddleware] integrations.risk_integration.enabled is false — account risk scoring is disabled',
		);
		_registrationRiskEvaluator = noopRegistrationRiskEvaluator;
		return _registrationRiskEvaluator;
	}
	const cacheManager = getRiskCacheManager();
	const ipInfoService = getIpInfoService();
	const ipInfoChecker = Config.risk.ipinfoApiKey ? createIpInfoChecker({ipInfoService}) : undefined;
	const cacheService = getCacheService();
	const reverseDnsLookup = createReverseDnsLookup({cacheService});
	const toolbox = createRiskToolbox({
		disposableDomainsRef: cacheManager.disposableDomainsRef,
		ipInfoChecker,
		reverseDnsLookup,
		ipInfoService,
		registrationEventsRepository: getRegistrationEventsRepository(),
		historicalOutcomeRepository: getHistoricalOutcomeRepository(),
		suspiciousIpRepository: getSuspiciousIpRepository(),
		cacheService,
	});
	const engine = new DeterministicRiskEngine(toolbox, {
		logger: Logger,
	});
	const evaluator = new RegistrationRiskEvaluator(engine);
	_registrationRiskEvaluator = evaluator;
	return _registrationRiskEvaluator;
}

let _liveKitWebhookService: LiveKitWebhookService | null = null;

function getLiveKitWebhookService(): LiveKitWebhookService | null {
	if (!_liveKitWebhookService) {
		const voiceTopology = getVoiceTopology();
		if (!voiceTopology) return null;
		const liveKitService: ILiveKitService = getLiveKitServiceInstance() ?? new DisabledLiveKitService();
		const voiceRoomStore: IVoiceRoomStore = getVoiceRoomStoreInstance() ?? new InMemoryVoiceRoomStore();
		const hasVoiceInfrastructure =
			Config.voice.enabled &&
			voiceTopology !== null &&
			liveKitService instanceof LiveKitService &&
			voiceRoomStore instanceof VoiceRoomStore;
		if (hasVoiceInfrastructure && voiceTopology) {
			_liveKitWebhookService = new LiveKitWebhookService(
				voiceRoomStore,
				getGatewayService(),
				getUserRepository(),
				liveKitService,
				voiceTopology,
				getLimitConfigService(),
				new VoicePresenceHeartbeatStore(getKVClient()),
			);
		}
	}
	return _liveKitWebhookService;
}

export const ServiceMiddleware = createMiddleware<HonoEnv>(async (ctx, next) => {
	const apiContext = createApiContext({
		requestId: ctx.get('requestId') ?? crypto.randomUUID(),
		clientIp: resolveRequestClientIp(ctx.req.raw),
		userAgent: ctx.req.header('user-agent') ?? null,
	});
	ctx.set('apiContext', apiContext);
	const snowflakeService = getSnowflakeService();
	const limitConfigService = getLimitConfigService();
	const userRepository = getUserRepository();
	const guildRepository = getGuildRepository();
	const channelRepository = getChannelRepository();
	const inviteRepository = getInviteRepository();
	const webhookRepository = getWebhookRepository();
	const connectionRepository = getConnectionRepository();
	const packRepository = getPackRepository();
	const favoriteMemeRepository = getFavoriteMemeRepository();
	const applicationRepository = getApplicationRepository();
	const oauth2TokenRepository = getOAuth2TokenRepository();
	const cacheService = getCacheService();
	const kvClient = getKVClient();
	const ipInfoService = getIpInfoService();
	const rateLimitService = getRateLimitService();
	const emailDnsValidationService = getEmailDnsValidationService();
	const assetDeletionQueue = getAssetDeletionQueue();
	const storageService = getStorageService();
	const mediaService = getMediaService();
	const gatewayService = getGatewayService();
	const workerService = getWorkerService();
	const emailService = getEmailService();
	const avatarService = getAvatarService();
	const entityAssetService = getEntityAssetService();
	const embedService = getEmbedService();
	const readStateService = getReadStateService();
	const guildAuditLogService = getGuildAuditLogService();
	const botAuthService = getBotAuthService();
	const discriminatorService = getDiscriminatorService();
	const userCacheService = createUserCacheService();
	await ensureVirusScanInitialized();
	const virusScanService = getVirusScanServiceInstance();
	await ensureVoiceResourcesInitialized();
	const liveKitService: ILiveKitService = getLiveKitServiceInstance() ?? new DisabledLiveKitService();
	const voiceRoomStore: IVoiceRoomStore = getVoiceRoomStoreInstance() ?? new InMemoryVoiceRoomStore();
	const voiceAvailabilityService = getVoiceAvailabilityService();
	const {packService, channelService, guildService, inviteService} = createGuildStackServices({
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
		purgeQueue: getPurgeQueue(),
		guildAuditLogService,
		voiceRoomStore,
		liveKitService,
		voiceAvailabilityService,
		ipInfoService,
	});
	const blueskyOAuthService = await resolveBlueskyOAuthService(getInstanceConfigRepository());
	const connectionService = new ConnectionService(connectionRepository, gatewayService, blueskyOAuthService);
	const favoriteMemeService = new FavoriteMemeService(
		apiContext,
		favoriteMemeRepository,
		channelService,
		storageService,
		getUnfurlerService(),
		limitConfigService,
		getGifService(),
	);
	const contactChangeLogService = getContactChangeLogService();
	const registrationRiskEvaluator = getRegistrationRiskEvaluator();
	const accountPolicyEvaluator = getAccountPolicyEvaluator();
	const adminRepo = getAdminRepository();
	const singleCommunityService = new SingleCommunityService(
		getInstanceConfigRepository(),
		guildService.data,
		guildService.members,
	);
	const registrationDependencies = {
		inviteService,
		instanceConfigRepository: getInstanceConfigRepository(),
		singleCommunityService,
		discriminatorService,
		kvActivityTracker: getKVActivityTracker(),
		registrationRiskEvaluator: registrationRiskEvaluator ?? noopRegistrationRiskEvaluator,
		accountPolicyEvaluator,
		isEmailDomainSuspicious: adminRepo.isEmailDomainSuspicious.bind(adminRepo),
		isEmailDomainDisposable: adminRepo.isEmailDomainDisposable.bind(adminRepo),
		registrationEventsRepository: getRegistrationEventsRepository(),
		riskAssessmentRepository: getRiskAssessmentRepository(),
		riskHistoryRepository: getHistoricalOutcomeRepository(),
	};
	const ssoService = new SsoService(
		apiContext,
		getInstanceConfigRepository(),
		discriminatorService,
		getKVActivityTracker(),
	);
	const desktopHandoffService = new DesktopHandoffService(apiContext);
	const authRequestService = new AuthRequestService(
		apiContext,
		ssoService,
		desktopHandoffService,
		registrationDependencies,
		{
			inviteService,
			kvDeletionQueue: getKVAccountDeletionQueue(),
			flutterClientGateMemberRepository: guildRepository,
		},
	);
	const reportService = getReportServiceInstance();
	const voiceTopology = getVoiceTopology();
	const hasVoiceInfrastructure =
		Config.voice.enabled &&
		voiceTopology !== null &&
		liveKitService instanceof LiveKitService &&
		voiceRoomStore instanceof VoiceRoomStore;
	const liveKitWebhookService = hasVoiceInfrastructure ? getLiveKitWebhookService() : undefined;
	const voiceService =
		hasVoiceInfrastructure && voiceAvailabilityService !== null
			? new VoiceService(
					liveKitService,
					guildRepository,
					userRepository,
					channelRepository,
					voiceRoomStore,
					voiceAvailabilityService,
				)
			: null;
	const emailChangeService = new EmailChangeService(apiContext, getEmailChangeRepository());
	const passwordChangeService = new PasswordChangeService(apiContext, getPasswordChangeRepository());
	const userPermissionUtils = getUserPermissionUtils();
	const userService = new UserService(
		apiContext,
		userCacheService,
		channelService,
		channelRepository,
		guildService,
		entityAssetService,
		discriminatorService,
		guildRepository,
		userPermissionUtils,
		getKVAccountDeletionQueue(),
		getKVBulkMessageDeletionQueue(),
		contactChangeLogService,
		getConnectionRepository(),
		limitConfigService,
	);
	const stripeService: StripeService | null = new StripeService(
		userRepository,
		gatewayService,
		guildRepository,
		guildService,
		cacheService,
		getBillingRepository(),
	);
	let ageVerificationService: AgeVerificationService | null = null;
	let donationService: DonationService | null = null;
	if (!Config.instance.selfHosted) {
		ageVerificationService = new AgeVerificationService(
			stripeService.getStripe(),
			userRepository,
			gatewayService,
			cacheService,
		);
		donationService = new DonationService(
			new DonationMagicLinkService(getDonationRepository(), emailService, emailDnsValidationService),
			new DonationCheckoutService(stripeService.getStripe(), getDonationRepository(), emailDnsValidationService),
		);
	}
	const adminService = new AdminService(
		apiContext,
		guildRepository,
		channelRepository,
		getAdminRepository(),
		inviteRepository,
		discriminatorService,
		guildService,
		userCacheService,
		channelService,
		userService,
		entityAssetService,
		assetDeletionQueue,
		storageService,
		reportService,
		getVoiceRepository(),
		getKVBulkMessageDeletionQueue(),
		applicationRepository,
		stripeService?.getStripe() ?? null,
		getHistoricalOutcomeRepository(),
		new JobLedgerRepository(),
		ipInfoService,
		getSuspiciousIpRepository(),
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
	const applicationService = new ApplicationService(apiContext, {
		applicationRepository,
		channelRepository,
		userCacheService,
		entityAssetService,
		discriminatorService,
		botAuthService,
	});
	const oauth2Service = new OAuth2Service(apiContext, {applicationRepository, oauth2TokenRepository});
	ctx.set('adminService', adminService);
	ctx.set(
		'adminArchiveService',
		new AdminArchiveService(
			getAdminArchiveRepository(),
			userRepository,
			guildRepository,
			storageService,
			snowflakeService,
			workerService,
		),
	);
	ctx.set('adminApiKeyService', getAdminApiKeyService());
	ctx.set('applicationRepository', applicationRepository);
	ctx.set('applicationService', applicationService);
	ctx.set('authRequestService', authRequestService);
	ctx.set('ssoService', ssoService);
	ctx.set('botAuthService', botAuthService);
	ctx.set('cacheService', cacheService);
	ctx.set('channelService', channelService);
	ctx.set('channelRequestService', new ChannelRequestService(channelService, userCacheService));
	ctx.set('messageRequestService', new MessageRequestService(channelService, createMessageResponseDataService()));
	ctx.set('channelRepository', channelRepository);
	ctx.set('connectionService', connectionService);
	ctx.set(
		'connectionRequestService',
		new ConnectionRequestService(connectionService, Config.auth.connectionInitiationSecret),
	);
	ctx.set('blueskyOAuthService', blueskyOAuthService);
	ctx.set('streamPreviewService', getStreamPreviewService());
	ctx.set('streamService', new StreamService(cacheService, channelService, gatewayService, getStreamPreviewService()));
	ctx.set('downloadService', getDownloadService());
	ctx.set('desktopHandoffService', desktopHandoffService);
	ctx.set('emailService', emailService);
	ctx.set('embedService', embedService);
	ctx.set('entityAssetService', entityAssetService);
	const entranceSoundRepository = new EntranceSoundRepository();
	const entranceSoundService = new EntranceSoundService(entranceSoundRepository, storageService, mediaService);
	ctx.set('entranceSoundService', entranceSoundService);
	ctx.set(
		'entranceSoundPlayService',
		new EntranceSoundPlayService(entranceSoundService, gatewayService, channelRepository),
	);
	ctx.set('favoriteMemeService', favoriteMemeService);
	ctx.set('favoriteMemeRequestService', new FavoriteMemeRequestService(favoriteMemeService));
	ctx.set('gatewayService', gatewayService);
	ctx.set('gatewayRequestService', new GatewayRequestService(botAuthService));
	ctx.set('guildService', guildService);
	ctx.set('singleCommunityService', singleCommunityService);
	ctx.set(
		'discoveryService',
		new GuildDiscoveryService(getGuildDiscoveryRepository(), guildRepository, gatewayService, getGuildSearchService()),
	);
	ctx.set('emailChangeService', emailChangeService);
	ctx.set('passwordChangeService', passwordChangeService);
	ctx.set('inviteService', inviteService);
	ctx.set(
		'inviteRequestService',
		new InviteRequestService(
			inviteService,
			channelService,
			guildService,
			gatewayService,
			packRepository,
			userCacheService,
		),
	);
	ctx.set('packService', packService);
	ctx.set('packRepository', packRepository);
	if (liveKitWebhookService) ctx.set('liveKitWebhookService', liveKitWebhookService);
	ctx.set('mediaService', mediaService);
	ctx.set('oauth2Service', oauth2Service);
	ctx.set(
		'oauth2RequestService',
		new OAuth2RequestService(
			apiContext,
			oauth2Service,
			applicationRepository,
			oauth2TokenRepository,
			botAuthService,
			applicationService,
			guildService,
			channelService,
		),
	);
	ctx.set(
		'oauth2ApplicationsRequestService',
		new OAuth2ApplicationsRequestService(apiContext, applicationService, applicationRepository),
	);
	ctx.set('oauth2TokenRepository', oauth2TokenRepository);
	ctx.set('rateLimitService', rateLimitService);
	ctx.set('readStateService', readStateService);
	ctx.set('readStateRequestService', new ReadStateRequestService(readStateService));
	ctx.set('kvActivityTracker', getKVActivityTracker());
	ctx.set('userActivityBuffer', getUserActivityBuffer());
	ctx.set('reportService', reportService);
	ctx.set('reportRequestService', new ReportRequestService(reportService));
	ctx.set(
		'rpcService',
		new RpcService(
			userRepository,
			guildRepository,
			channelRepository,
			userCacheService,
			readStateService,
			apiContext,
			gatewayService,
			discriminatorService,
			getFavoriteMemeRepository(),
			botAuthService,
			inviteRepository,
			webhookRepository,
			storageService,
			avatarService,
			channelService,
			userService.channelService,
			rateLimitService,
			limitConfigService,
			kvClient,
			workerService,
			getPremiumStateReconciliationQueueService(),
			getInstanceConfigRepository(),
			voiceService,
			voiceAvailabilityService,
		),
	);
	ctx.set(
		'searchService',
		new SearchService({
			channelRepository,
			channelService,
			guildService,
			userRepository,
			userCacheService,
			workerService,
		}),
	);
	ctx.set('sweegoWebhookService', getSweegoWebhookService());
	ctx.set('snowflakeService', snowflakeService);
	ctx.set('storageService', storageService);
	ctx.set('themeService', getThemeService());
	if (stripeService) ctx.set('stripeService', stripeService);
	if (ageVerificationService) ctx.set('ageVerificationService', ageVerificationService);
	if (donationService) ctx.set('donationService', donationService);
	ctx.set('sudoModeValid', false);
	ctx.set('gifService', getGifService());
	ctx.set('userCacheService', userCacheService);
	ctx.set('userRepository', userRepository);
	ctx.set('userService', userService);
	ctx.set(
		'userAccountRequestService',
		new UserAccountRequestService(
			emailChangeService,
			userService.accountService,
			userService.channelService,
			userRepository,
			userCacheService,
			adminRepo.isEmailDomainSuspicious.bind(adminRepo),
			adminRepo.isEmailDomainDisposable.bind(adminRepo),
			registrationRiskEvaluator,
			accountPolicyEvaluator,
			getRegistrationEventsRepository(),
			getRiskAssessmentRepository(),
			getHistoricalOutcomeRepository(),
		),
	);
	ctx.set('userAuthRequestService', new UserAuthRequestService(apiContext, userRepository, guildRepository));
	ctx.set('userChannelRequestService', new UserChannelRequestService(userService.channelService, userCacheService));
	ctx.set('userContentRequestService', new UserContentRequestService(userService.contentService, userCacheService));
	ctx.set(
		'userRelationshipRequestService',
		new UserRelationshipRequestService(userService.relationshipService, userService.channelService, userCacheService),
	);
	ctx.set(
		'scheduledMessageService',
		new ScheduledMessageService(channelService, getScheduledMessageRepository(), workerService, snowflakeService),
	);
	ctx.set('webhookService', webhookService);
	ctx.set(
		'webhookRequestService',
		new WebhookRequestService(
			webhookService,
			channelRepository,
			userCacheService,
			liveKitWebhookService ?? null,
			getSweegoWebhookService(),
		),
	);
	ctx.set('workerService', workerService);
	ctx.set('contactChangeLogService', contactChangeLogService);
	ctx.set('instanceConfigRepository', getInstanceConfigRepository());
	ctx.set('limitConfigService', limitConfigService);
	ctx.set('errorI18nService', getErrorI18nService());
	ctx.set('ncmecSubmissionService', getNcmecSubmissionService());
	await next();
});
