// SPDX-License-Identifier: AGPL-3.0-or-later

import crypto from 'node:crypto';
import {createIpInfoService, createUnavailableIpInfoService, type IpInfoService} from '@pkgs/geoip/src/IpInfoService';
import {createMiddleware} from 'hono/factory';
import type {ApiContext} from '../ApiContext';
import {AdminService} from '../admin/AdminService';
import {AuthRequestService} from '../auth/AuthRequestService';
import {DesktopHandoffService} from '../auth/services/DesktopHandoffService';
import {
	CassandraInboundSmsChallengeRepository,
	InboundSmsChallengeService,
} from '../auth/services/InboundSmsChallengeService';
import type {IRegistrationRiskEvaluator} from '../auth/services/IRegistrationRiskEvaluator';
import {noopRegistrationRiskEvaluator, RegistrationRiskEvaluator} from '../auth/services/RegistrationRiskEvaluator';
import {SsoService} from '../auth/services/SsoService';
import type {IBlueskyOAuthService} from '../bluesky/IBlueskyOAuthService';
import {Config} from '../Config';
import {createApiContext} from '../CreateApiContext';
import {ChannelRepository} from '../channel/ChannelRepository';
import {ChannelRequestService} from '../channel/services/ChannelRequestService';
import {MessageRequestService} from '../channel/services/message/MessageRequestService';
import {createMessageResponseDataService} from '../channel/services/message/MessageResponseDataService';
import {StreamService} from '../channel/services/StreamService';
import {ConnectionRequestService} from '../connection/ConnectionRequestService';
import {ConnectionService} from '../connection/ConnectionService';
import {DonationService} from '../donation/DonationService';
import {DonationCheckoutService} from '../donation/services/DonationCheckoutService';
import {DonationMagicLinkService} from '../donation/services/DonationMagicLinkService';
import {FavoriteMemeRequestService} from '../favorite_meme/FavoriteMemeRequestService';
import {FavoriteMemeService} from '../favorite_meme/FavoriteMemeService';
import {GuildRepository} from '../guild/repositories/GuildRepository';
import {DisabledLiveKitService} from '../infrastructure/DisabledLiveKitService';
import type {IGatewayService} from '../infrastructure/IGatewayService';
import type {ILiveKitService} from '../infrastructure/ILiveKitService';
import type {IMediaService} from '../infrastructure/IMediaService';
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
import {createRiskToolbox} from '../risk/RiskToolboxFactory';
import {CassandraSuspiciousIpRepository} from '../risk/SuspiciousIpRepository';
import {RpcService} from '../rpc/RpcService';
import {getReportSearchService} from '../SearchFactory';
import {SearchService} from '../search/SearchService';
import {StripeService} from '../stripe/StripeService';
import {AgeVerificationService} from '../stripe/services/AgeVerificationService';
import type {HonoEnv} from '../types/HonoEnv';
import type {UserRepository} from '../user/repositories/UserRepository';
import {EmailChangeService} from '../user/services/EmailChangeService';
import {MfaBackupCodesChallengeService} from '../user/services/MfaBackupCodesChallengeService';
import {PasswordChangeService} from '../user/services/PasswordChangeService';
import {UserAccountRequestService} from '../user/services/UserAccountRequestService';
import {UserAuthRequestService} from '../user/services/UserAuthRequestService';
import {UserChannelRequestService} from '../user/services/UserChannelRequestService';
import {UserContentRequestService} from '../user/services/UserContentRequestService';
import {UserRelationshipRequestService} from '../user/services/UserRelationshipRequestService';
import {UserService} from '../user/services/UserService';
import {getRequestClientIp} from '../utils/RequestClientIp';
import {VoiceService} from '../voice/VoiceService';
import {WebhookRequestService} from '../webhook/WebhookRequestService';
import {WebhookService} from '../webhook/WebhookService';
import {createGuildStackServices, type GuildStackServices} from './GuildStackServiceFactory';
import {installLazyServices, type RequestScopedServices} from './LazyServiceProvider';
import type {RequestCache} from './RequestCacheMiddleware';
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
	ensureVirusScanInitialized,
	getAdminApiKeyService,
	getAdminArchiveService,
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
	getEntranceSoundPlayService,
	getEntranceSoundService,
	getErrorI18nService,
	getFavoriteMemeRepository,
	getGatewayRequestService,
	getGifService,
	getGuildAuditLogService,
	getGuildDiscoveryService,
	getGuildRepository,
	getInstanceConfigRepository,
	getInviteRepository,
	getKVAccountDeletionQueue,
	getKVActivityTracker,
	getKVBulkMessageDeletionQueue,
	getLimitConfigService,
	getNcmecSubmissionService,
	getOAuth2TokenRepository,
	getPasswordChangeRepository,
	getPremiumStateReconciliationQueueService,
	getPurgeQueue,
	getRateLimitService,
	getReadStateRequestService,
	getReadStateService,
	getReportRepository,
	getStorageService,
	getStreamPreviewService,
	getSweegoWebhookService,
	getThemeService,
	getUnfurlerService,
	getUserActivityBuffer,
	getUserCacheService,
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
		Logger.info(
			{},
			'[ServiceMiddleware] integrations.risk_integration.enabled is false — account risk scoring is disabled',
		);
		_registrationRiskEvaluator = noopRegistrationRiskEvaluator;
		return _registrationRiskEvaluator;
	}
	const ipInfoService = getIpInfoService();
	const ipInfoChecker = Config.risk.ipinfoApiKey ? createIpInfoChecker({ipInfoService}) : undefined;
	const cacheService = getCacheService();
	const reverseDnsLookup = createReverseDnsLookup({cacheService});
	const toolbox = createRiskToolbox({
		adminRepository: getAdminRepository(),
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
			);
		}
	}
	return _liveKitWebhookService;
}

class RequestServices implements RequestScopedServices {
	private cachedGateway: IGatewayService | undefined;
	private cachedMedia: IMediaService | undefined;
	private cachedLiveKit: ILiveKitService | undefined;
	private cachedVoiceRooms: IVoiceRoomStore | undefined;
	private cachedVoice: VoiceService | null | undefined;
	private cachedGuildStack: GuildStackServices | undefined;
	private cachedChannelRepository: ChannelRepository | undefined;
	private cachedGuildRepository: GuildRepository | undefined;
	private cachedAdminService: AdminService | undefined;
	private cachedApplicationService: ApplicationService | undefined;
	private cachedAuthRequestService: AuthRequestService | undefined;
	private cachedSsoService: SsoService | undefined;
	private cachedDesktopHandoffService: DesktopHandoffService | undefined;
	private cachedChannelRequestService: ChannelRequestService | undefined;
	private cachedMessageRequestService: MessageRequestService | undefined;
	private cachedConnectionService: ConnectionService | undefined;
	private cachedConnectionRequestService: ConnectionRequestService | undefined;
	private cachedStreamService: StreamService | undefined;
	private cachedFavoriteMemeService: FavoriteMemeService | undefined;
	private cachedFavoriteMemeRequestService: FavoriteMemeRequestService | undefined;
	private cachedSingleCommunityService: SingleCommunityService | undefined;
	private cachedEmailChangeService: EmailChangeService | undefined;
	private cachedMfaBackupCodesChallengeService: MfaBackupCodesChallengeService | undefined;
	private cachedPasswordChangeService: PasswordChangeService | undefined;
	private cachedInviteRequestService: InviteRequestService | undefined;
	private cachedOAuth2Service: OAuth2Service | undefined;
	private cachedOAuth2RequestService: OAuth2RequestService | undefined;
	private cachedOAuth2ApplicationsRequestService: OAuth2ApplicationsRequestService | undefined;
	private cachedReportRequestService: ReportRequestService | undefined;
	private cachedRpcService: RpcService | undefined;
	private cachedSearchService: SearchService | undefined;
	private cachedStripeService: StripeService | undefined;
	private cachedAgeVerificationService: AgeVerificationService | undefined;
	private cachedDonationService: DonationService | undefined;
	private cachedUserService: UserService | undefined;
	private cachedUserAccountRequestService: UserAccountRequestService | undefined;
	private cachedUserAuthRequestService: UserAuthRequestService | undefined;
	private cachedUserChannelRequestService: UserChannelRequestService | undefined;
	private cachedUserContentRequestService: UserContentRequestService | undefined;
	private cachedUserRelationshipRequestService: UserRelationshipRequestService | undefined;
	private cachedWebhookService: WebhookService | undefined;
	private cachedWebhookRequestService: WebhookRequestService | undefined;

	constructor(
		private readonly context: ApiContext,
		private readonly bluesky: IBlueskyOAuthService,
		private readonly entityCache: RequestCache | undefined,
	) {}

	private get requestGuildRepository(): GuildRepository {
		this.cachedGuildRepository ??= this.entityCache ? new GuildRepository(this.entityCache) : getGuildRepository();
		return this.cachedGuildRepository;
	}

	get gatewayService(): IGatewayService {
		this.cachedGateway ??= getGatewayService();
		return this.cachedGateway;
	}

	get mediaService(): IMediaService {
		this.cachedMedia ??= getMediaService();
		return this.cachedMedia;
	}

	private get liveKit(): ILiveKitService {
		this.cachedLiveKit ??= getLiveKitServiceInstance() ?? new DisabledLiveKitService();
		return this.cachedLiveKit;
	}

	private get voiceRooms(): IVoiceRoomStore {
		this.cachedVoiceRooms ??= getVoiceRoomStoreInstance() ?? new InMemoryVoiceRoomStore();
		return this.cachedVoiceRooms;
	}

	private get voice(): VoiceService | null {
		if (this.cachedVoice === undefined) {
			const liveKitService = this.liveKit;
			const voiceRoomStore = this.voiceRooms;
			const voiceAvailabilityService = getVoiceAvailabilityService();
			const hasVoiceInfrastructure =
				Config.voice.enabled &&
				getVoiceTopology() !== null &&
				liveKitService instanceof LiveKitService &&
				voiceRoomStore instanceof VoiceRoomStore;
			this.cachedVoice =
				hasVoiceInfrastructure && voiceAvailabilityService !== null
					? new VoiceService(
							liveKitService,
							getGuildRepository(),
							getUserRepository(),
							getChannelRepository(),
							voiceRoomStore,
							voiceAvailabilityService,
						)
					: null;
		}
		return this.cachedVoice;
	}

	private get guildStack(): GuildStackServices {
		this.cachedGuildStack ??= createGuildStackServices({
			apiContext: this.context,
			channelRepository: this.channelRepository,
			userRepository: getUserRepository(),
			guildRepository: this.requestGuildRepository,
			inviteRepository: getInviteRepository(),
			webhookRepository: getWebhookRepository(),
			favoriteMemeRepository: getFavoriteMemeRepository(),
			avatarService: getAvatarService(),
			entityAssetService: getEntityAssetService(),
			assetDeletionQueue: getAssetDeletionQueue(),
			userCacheService: getUserCacheService(),
			limitConfigService: getLimitConfigService(),
			embedService: getEmbedService(),
			readStateService: getReadStateService(),
			storageService: getStorageService(),
			attachmentUploadTraceRepository: getAttachmentUploadTraceRepository(),
			virusScanService: getVirusScanServiceInstance(),
			purgeQueue: getPurgeQueue(),
			guildAuditLogService: getGuildAuditLogService(),
			voiceRoomStore: this.voiceRooms,
			liveKitService: this.liveKit,
			voiceAvailabilityService: getVoiceAvailabilityService(),
			ipInfoService: getIpInfoService(),
		});
		return this.cachedGuildStack;
	}

	get channelService() {
		return this.guildStack.channelService;
	}

	get guildService() {
		return this.guildStack.guildService;
	}

	get inviteService() {
		return this.guildStack.inviteService;
	}

	get blueskyOAuthService(): IBlueskyOAuthService {
		return this.bluesky;
	}

	get adminApiKeyService() {
		return getAdminApiKeyService();
	}

	get adminArchiveService() {
		return getAdminArchiveService();
	}

	get applicationRepository() {
		return getApplicationRepository();
	}

	get botAuthService() {
		return getBotAuthService();
	}

	get cacheService() {
		return getCacheService();
	}

	get channelRepository(): ChannelRepository {
		this.cachedChannelRepository ??= this.entityCache
			? new ChannelRepository(this.entityCache)
			: getChannelRepository();
		return this.cachedChannelRepository;
	}

	get contactChangeLogService() {
		return getContactChangeLogService();
	}

	get downloadService() {
		return getDownloadService();
	}

	get emailService() {
		return getEmailService();
	}

	get embedService() {
		return getEmbedService();
	}

	get entityAssetService() {
		return getEntityAssetService();
	}

	get entranceSoundService() {
		return getEntranceSoundService();
	}

	get entranceSoundPlayService() {
		return getEntranceSoundPlayService();
	}

	get errorI18nService() {
		return getErrorI18nService();
	}

	get gatewayRequestService() {
		return getGatewayRequestService();
	}

	get gifService() {
		return getGifService();
	}

	get discoveryService() {
		return getGuildDiscoveryService();
	}

	get instanceConfigRepository() {
		return getInstanceConfigRepository();
	}

	get kvActivityTracker() {
		return getKVActivityTracker();
	}

	get limitConfigService() {
		return getLimitConfigService();
	}

	get ncmecSubmissionService() {
		return getNcmecSubmissionService();
	}

	get oauth2TokenRepository() {
		return getOAuth2TokenRepository();
	}

	get rateLimitService() {
		return getRateLimitService();
	}

	get readStateService() {
		return getReadStateService();
	}

	get readStateRequestService() {
		return getReadStateRequestService();
	}

	get reportService() {
		return getReportServiceInstance();
	}

	get snowflakeService() {
		return getSnowflakeService();
	}

	get storageService() {
		return getStorageService();
	}

	get streamPreviewService() {
		return getStreamPreviewService();
	}

	get sweegoWebhookService() {
		return getSweegoWebhookService();
	}

	get themeService() {
		return getThemeService();
	}

	get userActivityBuffer() {
		return getUserActivityBuffer();
	}

	get userCacheService() {
		return getUserCacheService();
	}

	get userRepository() {
		return getUserRepository();
	}

	get workerService() {
		return getWorkerService();
	}

	get liveKitWebhookService(): LiveKitWebhookService | undefined {
		const liveKitService = this.liveKit;
		const voiceRoomStore = this.voiceRooms;
		const hasVoiceInfrastructure =
			Config.voice.enabled &&
			getVoiceTopology() !== null &&
			liveKitService instanceof LiveKitService &&
			voiceRoomStore instanceof VoiceRoomStore;
		if (!hasVoiceInfrastructure) {
			return undefined;
		}
		return getLiveKitWebhookService() ?? undefined;
	}

	get adminService(): AdminService {
		this.cachedAdminService ??= new AdminService(
			this.context,
			getGuildRepository(),
			getChannelRepository(),
			getAdminRepository(),
			getInviteRepository(),
			getDiscriminatorService(),
			this.guildService,
			getUserCacheService(),
			this.channelService,
			this.userService,
			getEntityAssetService(),
			getAssetDeletionQueue(),
			getStorageService(),
			getReportServiceInstance(),
			getVoiceRepository(),
			getKVBulkMessageDeletionQueue(),
			getApplicationRepository(),
			this.stripeService.getStripe(),
			getHistoricalOutcomeRepository(),
			new JobLedgerRepository(),
			getIpInfoService(),
			getSuspiciousIpRepository(),
		);
		return this.cachedAdminService;
	}

	get applicationService(): ApplicationService {
		this.cachedApplicationService ??= new ApplicationService(this.context, {
			applicationRepository: getApplicationRepository(),
			channelRepository: getChannelRepository(),
			userCacheService: getUserCacheService(),
			entityAssetService: getEntityAssetService(),
			discriminatorService: getDiscriminatorService(),
			botAuthService: getBotAuthService(),
		});
		return this.cachedApplicationService;
	}

	get ssoService(): SsoService {
		this.cachedSsoService ??= new SsoService(
			this.context,
			getInstanceConfigRepository(),
			getDiscriminatorService(),
			getKVActivityTracker(),
		);
		return this.cachedSsoService;
	}

	get desktopHandoffService(): DesktopHandoffService {
		this.cachedDesktopHandoffService ??= new DesktopHandoffService(this.context);
		return this.cachedDesktopHandoffService;
	}

	get authRequestService(): AuthRequestService {
		if (!this.cachedAuthRequestService) {
			const adminRepository = getAdminRepository();
			const registrationRiskEvaluator = getRegistrationRiskEvaluator();
			this.cachedAuthRequestService = new AuthRequestService(
				this.context,
				this.ssoService,
				this.desktopHandoffService,
				{
					inviteService: this.inviteService,
					instanceConfigRepository: getInstanceConfigRepository(),
					singleCommunityService: this.singleCommunityService,
					discriminatorService: getDiscriminatorService(),
					kvActivityTracker: getKVActivityTracker(),
					registrationRiskEvaluator: registrationRiskEvaluator ?? noopRegistrationRiskEvaluator,
					accountPolicyEvaluator: getAccountPolicyEvaluator(),
					isEmailDomainSuspicious: adminRepository.isEmailDomainSuspicious.bind(adminRepository),
					isEmailDomainDisposable: adminRepository.isEmailDomainDisposable.bind(adminRepository),
					registrationEventsRepository: getRegistrationEventsRepository(),
					riskAssessmentRepository: getRiskAssessmentRepository(),
					riskHistoryRepository: getHistoricalOutcomeRepository(),
				},
				{
					inviteService: this.inviteService,
					kvDeletionQueue: getKVAccountDeletionQueue(),
				},
			);
		}
		return this.cachedAuthRequestService;
	}

	get singleCommunityService(): SingleCommunityService {
		this.cachedSingleCommunityService ??= new SingleCommunityService(
			getInstanceConfigRepository(),
			this.guildService.data,
			this.guildService.members,
		);
		return this.cachedSingleCommunityService;
	}

	get channelRequestService(): ChannelRequestService {
		this.cachedChannelRequestService ??= new ChannelRequestService(this.channelService, getUserCacheService());
		return this.cachedChannelRequestService;
	}

	get messageRequestService(): MessageRequestService {
		this.cachedMessageRequestService ??= new MessageRequestService(
			this.channelService,
			createMessageResponseDataService(),
		);
		return this.cachedMessageRequestService;
	}

	get connectionService(): ConnectionService {
		this.cachedConnectionService ??= new ConnectionService(
			getConnectionRepository(),
			this.gatewayService,
			this.bluesky,
		);
		return this.cachedConnectionService;
	}

	get connectionRequestService(): ConnectionRequestService {
		this.cachedConnectionRequestService ??= new ConnectionRequestService(
			this.connectionService,
			Config.auth.connectionInitiationSecret,
		);
		return this.cachedConnectionRequestService;
	}

	get streamService(): StreamService {
		this.cachedStreamService ??= new StreamService(
			getCacheService(),
			this.channelService,
			this.gatewayService,
			getStreamPreviewService(),
		);
		return this.cachedStreamService;
	}

	get favoriteMemeService(): FavoriteMemeService {
		this.cachedFavoriteMemeService ??= new FavoriteMemeService(
			this.context,
			getFavoriteMemeRepository(),
			this.channelService,
			getStorageService(),
			getUnfurlerService(),
			getLimitConfigService(),
			getGifService(),
		);
		return this.cachedFavoriteMemeService;
	}

	get favoriteMemeRequestService(): FavoriteMemeRequestService {
		this.cachedFavoriteMemeRequestService ??= new FavoriteMemeRequestService(this.favoriteMemeService);
		return this.cachedFavoriteMemeRequestService;
	}

	get emailChangeService(): EmailChangeService {
		this.cachedEmailChangeService ??= new EmailChangeService(this.context, getEmailChangeRepository());
		return this.cachedEmailChangeService;
	}

	get mfaBackupCodesChallengeService(): MfaBackupCodesChallengeService {
		this.cachedMfaBackupCodesChallengeService ??= new MfaBackupCodesChallengeService(this.context);
		return this.cachedMfaBackupCodesChallengeService;
	}

	get passwordChangeService(): PasswordChangeService {
		this.cachedPasswordChangeService ??= new PasswordChangeService(this.context, getPasswordChangeRepository());
		return this.cachedPasswordChangeService;
	}

	get inviteRequestService(): InviteRequestService {
		this.cachedInviteRequestService ??= new InviteRequestService(
			this.inviteService,
			this.channelService,
			this.guildService,
			this.gatewayService,
			getUserCacheService(),
		);
		return this.cachedInviteRequestService;
	}

	get oauth2Service(): OAuth2Service {
		this.cachedOAuth2Service ??= new OAuth2Service(this.context, {
			applicationRepository: getApplicationRepository(),
			oauth2TokenRepository: getOAuth2TokenRepository(),
		});
		return this.cachedOAuth2Service;
	}

	get oauth2RequestService(): OAuth2RequestService {
		this.cachedOAuth2RequestService ??= new OAuth2RequestService(
			this.context,
			this.oauth2Service,
			getApplicationRepository(),
			getOAuth2TokenRepository(),
			getBotAuthService(),
			this.applicationService,
			this.guildService,
			this.channelService,
		);
		return this.cachedOAuth2RequestService;
	}

	get oauth2ApplicationsRequestService(): OAuth2ApplicationsRequestService {
		this.cachedOAuth2ApplicationsRequestService ??= new OAuth2ApplicationsRequestService(
			this.context,
			this.applicationService,
			getApplicationRepository(),
		);
		return this.cachedOAuth2ApplicationsRequestService;
	}

	get reportRequestService(): ReportRequestService {
		this.cachedReportRequestService ??= new ReportRequestService(getReportServiceInstance());
		return this.cachedReportRequestService;
	}

	get rpcService(): RpcService {
		this.cachedRpcService ??= new RpcService(
			getUserRepository(),
			getGuildRepository(),
			getChannelRepository(),
			getUserCacheService(),
			getReadStateService(),
			this.context,
			this.gatewayService,
			getDiscriminatorService(),
			getFavoriteMemeRepository(),
			getBotAuthService(),
			getInviteRepository(),
			getWebhookRepository(),
			getStorageService(),
			getAvatarService(),
			getRateLimitService(),
			getLimitConfigService(),
			getKVClient(),
			getWorkerService(),
			getPremiumStateReconciliationQueueService(),
			getInstanceConfigRepository(),
			this.voice,
			getVoiceAvailabilityService(),
		);
		return this.cachedRpcService;
	}

	get searchService(): SearchService {
		this.cachedSearchService ??= new SearchService({
			channelRepository: getChannelRepository(),
			channelService: this.channelService,
			guildService: this.guildService,
			userRepository: getUserRepository(),
			userCacheService: getUserCacheService(),
			workerService: getWorkerService(),
		});
		return this.cachedSearchService;
	}

	get stripeService(): StripeService {
		this.cachedStripeService ??= new StripeService(
			getUserRepository(),
			this.gatewayService,
			getGuildRepository(),
			this.guildService,
			getCacheService(),
			getBillingRepository(),
		);
		return this.cachedStripeService;
	}

	get ageVerificationService(): AgeVerificationService | undefined {
		if (Config.instance.selfHosted) {
			return undefined;
		}
		this.cachedAgeVerificationService ??= new AgeVerificationService(
			this.stripeService.getStripe(),
			getUserRepository(),
			this.gatewayService,
			getCacheService(),
		);
		return this.cachedAgeVerificationService;
	}

	get donationService(): DonationService | undefined {
		if (Config.instance.selfHosted) {
			return undefined;
		}
		this.cachedDonationService ??= new DonationService(
			new DonationMagicLinkService(getDonationRepository(), getEmailService(), getEmailDnsValidationService()),
			new DonationCheckoutService(
				this.stripeService.getStripe(),
				getDonationRepository(),
				getEmailDnsValidationService(),
			),
		);
		return this.cachedDonationService;
	}

	get userService(): UserService {
		this.cachedUserService ??= new UserService(
			this.context,
			getUserCacheService(),
			this.channelService,
			getChannelRepository(),
			this.guildService,
			getEntityAssetService(),
			getDiscriminatorService(),
			getGuildRepository(),
			getUserPermissionUtils(),
			getKVAccountDeletionQueue(),
			getKVBulkMessageDeletionQueue(),
			getContactChangeLogService(),
			getConnectionRepository(),
			getLimitConfigService(),
		);
		return this.cachedUserService;
	}

	get userAccountRequestService(): UserAccountRequestService {
		if (!this.cachedUserAccountRequestService) {
			const adminRepository = getAdminRepository();
			this.cachedUserAccountRequestService = new UserAccountRequestService(
				this.emailChangeService,
				this.userService.accountService,
				this.userService.channelService,
				getUserRepository(),
				getUserCacheService(),
				adminRepository.isEmailDomainSuspicious.bind(adminRepository),
				adminRepository.isEmailDomainDisposable.bind(adminRepository),
				getRegistrationRiskEvaluator(),
				getAccountPolicyEvaluator(),
				getRegistrationEventsRepository(),
				getRiskAssessmentRepository(),
				getHistoricalOutcomeRepository(),
			);
		}
		return this.cachedUserAccountRequestService;
	}

	get userAuthRequestService(): UserAuthRequestService {
		this.cachedUserAuthRequestService ??= new UserAuthRequestService(
			this.context,
			getUserRepository(),
			getGuildRepository(),
		);
		return this.cachedUserAuthRequestService;
	}

	get userChannelRequestService(): UserChannelRequestService {
		this.cachedUserChannelRequestService ??= new UserChannelRequestService(
			this.userService.channelService,
			getUserCacheService(),
		);
		return this.cachedUserChannelRequestService;
	}

	get userContentRequestService(): UserContentRequestService {
		this.cachedUserContentRequestService ??= new UserContentRequestService(
			this.userService.contentService,
			getUserCacheService(),
		);
		return this.cachedUserContentRequestService;
	}

	get userRelationshipRequestService(): UserRelationshipRequestService {
		this.cachedUserRelationshipRequestService ??= new UserRelationshipRequestService(
			this.userService.relationshipService,
			this.userService.channelService,
			getUserCacheService(),
		);
		return this.cachedUserRelationshipRequestService;
	}

	get webhookService(): WebhookService {
		this.cachedWebhookService ??= new WebhookService(
			getWebhookRepository(),
			this.guildService,
			this.channelService,
			getChannelRepository(),
			getCacheService(),
			this.gatewayService,
			getAvatarService(),
			this.mediaService,
			getSnowflakeService(),
			getGuildAuditLogService(),
			getLimitConfigService(),
		);
		return this.cachedWebhookService;
	}

	get webhookRequestService(): WebhookRequestService {
		this.cachedWebhookRequestService ??= new WebhookRequestService(
			this.webhookService,
			getChannelRepository(),
			getUserCacheService(),
			this.liveKitWebhookService ?? null,
			getSweegoWebhookService(),
		);
		return this.cachedWebhookRequestService;
	}
}

export const ServiceMiddleware = createMiddleware<HonoEnv>(async (ctx, next) => {
	const apiContext = createApiContext({
		requestId: ctx.get('requestId') ?? crypto.randomUUID(),
		clientIp: getRequestClientIp(ctx),
		userAgent: ctx.req.header('user-agent') ?? null,
	});
	ctx.set('apiContext', apiContext);
	ctx.set('sudoModeValid', false);
	await ensureVirusScanInitialized();
	await ensureVoiceResourcesInitialized();
	const blueskyOAuthService = await resolveBlueskyOAuthService(getInstanceConfigRepository());
	installLazyServices(ctx, new RequestServices(apiContext, blueskyOAuthService, ctx.get('requestCache')));
	await next();
});

export function resetServiceMiddlewareForTesting(): void {
	shutdownReportService();
	_inboundSmsChallengeService = null;
	_registrationEventsRepository = null;
	_riskAssessmentRepository = null;
	_historicalOutcomeRepository = null;
	_suspiciousIpRepository = null;
	_ipInfoService = null;
	_registrationRiskEvaluator = null;
	_liveKitWebhookService = null;
}
