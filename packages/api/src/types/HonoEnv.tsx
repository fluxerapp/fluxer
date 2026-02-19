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

import type {AdminService} from '@fluxer/api/src/admin/AdminService';
import type {AdminApiKeyService} from '@fluxer/api/src/admin/services/AdminApiKeyService';
import type {AdminArchiveService} from '@fluxer/api/src/admin/services/AdminArchiveService';
import type {AlertService} from '@fluxer/api/src/alert/AlertService';
import type {AuthRequestService} from '@fluxer/api/src/auth/AuthRequestService';
import type {AuthService} from '@fluxer/api/src/auth/AuthService';
import type {AuthMfaService} from '@fluxer/api/src/auth/services/AuthMfaService';
import type {DesktopHandoffService} from '@fluxer/api/src/auth/services/DesktopHandoffService';
import type {SsoService} from '@fluxer/api/src/auth/services/SsoService';
import type {UserID} from '@fluxer/api/src/BrandedTypes';
import type {IBlueskyOAuthService} from '@fluxer/api/src/bluesky/IBlueskyOAuthService';
import type {IChannelRepository} from '@fluxer/api/src/channel/IChannelRepository';
import type {ChannelRequestService} from '@fluxer/api/src/channel/services/ChannelRequestService';
import type {ChannelService} from '@fluxer/api/src/channel/services/ChannelService';
import type {MessageRequestService} from '@fluxer/api/src/channel/services/message/MessageRequestService';
import type {ScheduledMessageService} from '@fluxer/api/src/channel/services/ScheduledMessageService';
import type {StreamPreviewService} from '@fluxer/api/src/channel/services/StreamPreviewService';
import type {StreamService} from '@fluxer/api/src/channel/services/StreamService';
import type {ConnectionRequestService} from '@fluxer/api/src/connection/ConnectionRequestService';
import type {ConnectionService} from '@fluxer/api/src/connection/ConnectionService';
import type {CsamEvidenceRetentionService} from '@fluxer/api/src/csam/CsamEvidenceRetentionService';
import type {CsamLegalHoldService} from '@fluxer/api/src/csam/CsamLegalHoldService';
import type {NcmecSubmissionService} from '@fluxer/api/src/csam/NcmecSubmissionService';
import type {DonationService} from '@fluxer/api/src/donation/DonationService';
import type {DownloadService} from '@fluxer/api/src/download/DownloadService';
import type {FavoriteMemeRequestService} from '@fluxer/api/src/favorite_meme/FavoriteMemeRequestService';
import type {FavoriteMemeService} from '@fluxer/api/src/favorite_meme/FavoriteMemeService';
import type {GatewayRequestService} from '@fluxer/api/src/gateway/GatewayRequestService';
import type {IGuildDiscoveryService} from '@fluxer/api/src/guild/services/GuildDiscoveryService';
import type {GuildService} from '@fluxer/api/src/guild/services/GuildService';
import type {EmbedService} from '@fluxer/api/src/infrastructure/EmbedService';
import type {EntityAssetService} from '@fluxer/api/src/infrastructure/EntityAssetService';
import type {ErrorI18nService} from '@fluxer/api/src/infrastructure/ErrorI18nService';
import type {IGatewayService} from '@fluxer/api/src/infrastructure/IGatewayService';
import type {IKlipyService} from '@fluxer/api/src/infrastructure/IKlipyService';
import type {IMediaService} from '@fluxer/api/src/infrastructure/IMediaService';
import type {IStorageService} from '@fluxer/api/src/infrastructure/IStorageService';
import type {ITenorService} from '@fluxer/api/src/infrastructure/ITenorService';
import type {KVActivityTracker} from '@fluxer/api/src/infrastructure/KVActivityTracker';
import type {LiveKitWebhookService} from '@fluxer/api/src/infrastructure/LiveKitWebhookService';
import type {SnowflakeService} from '@fluxer/api/src/infrastructure/SnowflakeService';
import type {UserCacheService} from '@fluxer/api/src/infrastructure/UserCacheService';
import type {InstanceConfigRepository} from '@fluxer/api/src/instance/InstanceConfigRepository';
import type {InviteRequestService} from '@fluxer/api/src/invite/InviteRequestService';
import type {InviteService} from '@fluxer/api/src/invite/InviteService';
import type {LimitConfigService} from '@fluxer/api/src/limits/LimitConfigService';
import type {RequestCache} from '@fluxer/api/src/middleware/RequestCacheMiddleware';
import type {AuthSession} from '@fluxer/api/src/models/AuthSession';
import type {User} from '@fluxer/api/src/models/User';
import type {ApplicationService} from '@fluxer/api/src/oauth/ApplicationService';
import type {BotAuthService} from '@fluxer/api/src/oauth/BotAuthService';
import type {OAuth2ApplicationsRequestService} from '@fluxer/api/src/oauth/OAuth2ApplicationsRequestService';
import type {OAuth2RequestService} from '@fluxer/api/src/oauth/OAuth2RequestService';
import type {OAuth2Service} from '@fluxer/api/src/oauth/OAuth2Service';
import type {IApplicationRepository} from '@fluxer/api/src/oauth/repositories/IApplicationRepository';
import type {IOAuth2TokenRepository} from '@fluxer/api/src/oauth/repositories/IOAuth2TokenRepository';
import type {PackRepository} from '@fluxer/api/src/pack/PackRepository';
import type {PackRequestService} from '@fluxer/api/src/pack/PackRequestService';
import type {PackService} from '@fluxer/api/src/pack/PackService';
import type {ReadStateRequestService} from '@fluxer/api/src/read_state/ReadStateRequestService';
import type {ReadStateService} from '@fluxer/api/src/read_state/ReadStateService';
import type {ReportRequestService} from '@fluxer/api/src/report/ReportRequestService';
import type {ReportService} from '@fluxer/api/src/report/ReportService';
import type {RpcService} from '@fluxer/api/src/rpc/RpcService';
import type {SearchService} from '@fluxer/api/src/search/SearchService';
import type {StripeService} from '@fluxer/api/src/stripe/StripeService';
import type {ThemeService} from '@fluxer/api/src/theme/ThemeService';
import type {GuildManagedTraitService} from '@fluxer/api/src/traits/GuildManagedTraitService';
import type {IUserRepository} from '@fluxer/api/src/user/IUserRepository';
import type {EmailChangeService} from '@fluxer/api/src/user/services/EmailChangeService';
import type {PasswordChangeService} from '@fluxer/api/src/user/services/PasswordChangeService';
import type {UserAccountRequestService} from '@fluxer/api/src/user/services/UserAccountRequestService';
import type {UserAuthRequestService} from '@fluxer/api/src/user/services/UserAuthRequestService';
import type {UserChannelRequestService} from '@fluxer/api/src/user/services/UserChannelRequestService';
import type {UserContactChangeLogService} from '@fluxer/api/src/user/services/UserContactChangeLogService';
import type {UserContentRequestService} from '@fluxer/api/src/user/services/UserContentRequestService';
import type {UserRelationshipRequestService} from '@fluxer/api/src/user/services/UserRelationshipRequestService';
import type {UserService} from '@fluxer/api/src/user/services/UserService';
import type {SweegoWebhookService} from '@fluxer/api/src/webhook/SweegoWebhookService';
import type {WebhookRequestService} from '@fluxer/api/src/webhook/WebhookRequestService';
import type {WebhookService} from '@fluxer/api/src/webhook/WebhookService';
import type {ICacheService} from '@fluxer/cache/src/ICacheService';
import type {IEmailService} from '@fluxer/email/src/IEmailService';
import type {IRateLimitService} from '@fluxer/rate_limit/src/IRateLimitService';
import type {IWorkerService} from '@fluxer/worker/src/contracts/IWorkerService';
import type {Hono} from 'hono';

export interface HonoEnv {
	Variables: {
		user: User;
		responseSchema: unknown;
		adminService: AdminService;
		adminArchiveService: AdminArchiveService;
		adminApiKeyService: AdminApiKeyService;
		adminApiKey?: {keyId: bigint; createdById: UserID};
		adminApiKeyAcls: Set<string> | null;
		adminUserId: UserID;
		adminUserAcls: Set<string>;
		authTokenType?: 'session' | 'bearer' | 'bot' | 'admin_api_key';
		authViaCookie?: boolean;
		authToken?: string;
		authUserId?: string;
		oauthBearerAllowed?: boolean;
		oauthBearerToken?: string;
		oauthBearerScopes?: Set<string>;
		oauthBearerUserId?: UserID;
		auditLogReason: string | null;
		authMfaService: AuthMfaService;
		authService: AuthService;
		authRequestService: AuthRequestService;
		ssoService: SsoService;
		authSession: AuthSession;
		desktopHandoffService: DesktopHandoffService;
		cacheService: ICacheService;
		channelService: ChannelService;
		channelRequestService: ChannelRequestService;
		messageRequestService: MessageRequestService;
		channelRepository: IChannelRepository;
		connectionService: ConnectionService;
		connectionRequestService: ConnectionRequestService;
		blueskyOAuthService: IBlueskyOAuthService | null;
		donationService: DonationService;
		downloadService: DownloadService;
		streamPreviewService: StreamPreviewService;
		streamService: StreamService;
		emailService: IEmailService;
		emailChangeService: EmailChangeService;
		passwordChangeService: PasswordChangeService;
		embedService: EmbedService;
		entityAssetService: EntityAssetService;
		favoriteMemeService: FavoriteMemeService;
		favoriteMemeRequestService: FavoriteMemeRequestService;
		gatewayService: IGatewayService;
		gatewayRequestService: GatewayRequestService;
		alertService: AlertService;
		discoveryService: IGuildDiscoveryService;
		guildService: GuildService;
		guildManagedTraitService: GuildManagedTraitService;
		packService: PackService;
		packRequestService: PackRequestService;
		packRepository: PackRepository;
		inviteService: InviteService;
		inviteRequestService: InviteRequestService;
		liveKitWebhookService?: LiveKitWebhookService;
		mediaService: IMediaService;
		rateLimitService: IRateLimitService;
		readStateService: ReadStateService;
		readStateRequestService: ReadStateRequestService;
		kvActivityTracker: KVActivityTracker;
		reportService: ReportService;
		reportRequestService: ReportRequestService;
		csamEvidenceRetentionService: CsamEvidenceRetentionService;
		contactChangeLogService: UserContactChangeLogService;
		csamLegalHoldService: CsamLegalHoldService;
		ncmecSubmissionService: NcmecSubmissionService;
		requestCache: RequestCache;
		rpcService: RpcService;
		searchService: SearchService;
		snowflakeService: SnowflakeService;
		storageService: IStorageService;
		klipyService: IKlipyService;
		tenorService: ITenorService;
		themeService: ThemeService;
		userCacheService: UserCacheService;
		userRepository: IUserRepository;
		userService: UserService;
		userAccountRequestService: UserAccountRequestService;
		userAuthRequestService: UserAuthRequestService;
		userChannelRequestService: UserChannelRequestService;
		userContentRequestService: UserContentRequestService;
		userRelationshipRequestService: UserRelationshipRequestService;
		sweegoWebhookService: SweegoWebhookService;
		webhookService: WebhookService;
		webhookRequestService: WebhookRequestService;
		workerService: IWorkerService;
		scheduledMessageService: ScheduledMessageService;
		stripeService: StripeService;
		applicationService: ApplicationService;
		oauth2Service: OAuth2Service;
		oauth2RequestService: OAuth2RequestService;
		oauth2ApplicationsRequestService: OAuth2ApplicationsRequestService;
		applicationRepository: IApplicationRepository;
		oauth2TokenRepository: IOAuth2TokenRepository;
		botAuthService: BotAuthService;
		sudoModeValid: boolean;
		sudoModeToken: string | null;
		instanceConfigRepository: InstanceConfigRepository;
		limitConfigService: LimitConfigService;
		requestLocale: string;
		errorI18nService: ErrorI18nService;
		channelUpdateType?: number;
	};
}

export type HonoApp = Hono<HonoEnv>;
