// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import type {AdminRepository} from '@app/api/admin/AdminRepository';
import {AdminApplicationService} from '@app/api/admin/services/AdminApplicationService';
import {AdminAssetPurgeService} from '@app/api/admin/services/AdminAssetPurgeService';
import {AdminAuditService} from '@app/api/admin/services/AdminAuditService';
import {AdminBadgeService} from '@app/api/admin/services/AdminBadgeService';
import {AdminBanManagementService} from '@app/api/admin/services/AdminBanManagementService';
import {AdminCodeGenerationService} from '@app/api/admin/services/AdminCodeGenerationService';
import {AdminGuildService} from '@app/api/admin/services/AdminGuildService';
import {AdminMessageDeletionService} from '@app/api/admin/services/AdminMessageDeletionService';
import {AdminMessageService} from '@app/api/admin/services/AdminMessageService';
import {AdminMessageShredService} from '@app/api/admin/services/AdminMessageShredService';
import {AdminReportService} from '@app/api/admin/services/AdminReportService';
import {AdminSearchService} from '@app/api/admin/services/AdminSearchService';
import {AdminUserBanService} from '@app/api/admin/services/AdminUserBanService';
import {AdminUserRelationshipService} from '@app/api/admin/services/AdminUserRelationshipService';
import {AdminUserService} from '@app/api/admin/services/AdminUserService';
import {AdminVoiceService} from '@app/api/admin/services/AdminVoiceService';
import {ReporterResolutionNotifier} from '@app/api/admin/services/ReporterResolutionNotifier';
import type {UserID} from '@app/api/BrandedTypes';
import type {ChannelRepository} from '@app/api/channel/ChannelRepository';
import type {ChannelService} from '@app/api/channel/services/ChannelService';
import {EnforcementDecisionService} from '@app/api/enforcement/EnforcementDecisionService';
import {EnforcementNoticeDelivery} from '@app/api/enforcement/EnforcementNoticeDelivery';
import type {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import type {GuildService} from '@app/api/guild/services/GuildService';
import type {DiscriminatorService} from '@app/api/infrastructure/DiscriminatorService';
import type {EntityAssetService} from '@app/api/infrastructure/EntityAssetService';
import type {IAssetDeletionQueue} from '@app/api/infrastructure/IAssetDeletionQueue';
import type {IStorageService} from '@app/api/infrastructure/IStorageService';
import type {KVBulkMessageDeletionQueueService} from '@app/api/infrastructure/KVBulkMessageDeletionQueueService';
import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import type {InviteRepository} from '@app/api/invite/InviteRepository';
import {JobAdminService} from '@app/api/jobs/JobAdminService';
import type {JobLedgerRepository} from '@app/api/jobs/JobLedgerRepository';
import {
	getEnforcementDecisionRepository,
	getGuildDiscoveryRepository,
	getInstanceConfigRepository,
	getKVAccountDeletionQueue,
	getOAuth2TokenRepository,
	getPaymentDisputeRepository,
	getReportRepository,
} from '@app/api/middleware/ServiceSingletons';
import type {ApplicationRepository} from '@app/api/oauth/repositories/ApplicationRepository';
import {
	PaymentDisputeEnforcementService,
	queuePaymentDisputeClosures,
} from '@app/api/payment_disputes/PaymentDisputeEnforcement';
import type {ReportService} from '@app/api/report/ReportService';
import type {StoreEntitlementService} from '@app/api/store_billing/StoreEntitlementService';
import {systemDmsHidden} from '@app/api/user/services/SystemDmMessage';
import type {UserService} from '@app/api/user/services/UserService';
import type {VoiceRepository} from '@app/api/voice/VoiceRepository';
import {DirectMessagesDisabledError} from '@fluxer/errors/src/domains/channel/DirectMessagesDisabledError';
import type {SendSystemDmResponse} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import type Stripe from 'stripe';

export class AdminService {
	readonly auditService: AdminAuditService;
	readonly banManagementService: AdminBanManagementService;
	readonly userService: AdminUserService;
	readonly guildServiceAggregate: AdminGuildService;
	readonly messageService: AdminMessageService;
	readonly messageShredService: AdminMessageShredService;
	readonly messageDeletionService: AdminMessageDeletionService;
	readonly reportServiceAggregate: AdminReportService;
	readonly enforcementService: EnforcementDecisionService;
	readonly banService: AdminUserBanService;
	readonly voiceService: AdminVoiceService;
	readonly badgeService: AdminBadgeService;
	readonly searchService: AdminSearchService;
	readonly codeGenerationService: AdminCodeGenerationService;
	readonly assetPurgeService: AdminAssetPurgeService;
	readonly applicationService: AdminApplicationService;
	readonly jobAdminService: JobAdminService;
	readonly relationshipService: AdminUserRelationshipService;
	readonly paymentDisputeEnforcement: PaymentDisputeEnforcementService;

	constructor(
		private readonly apiContext: ApiContext,
		private readonly guildRepository: GuildRepository,
		private readonly channelRepository: ChannelRepository,
		private readonly adminRepository: AdminRepository,
		private readonly inviteRepository: InviteRepository,
		private readonly discriminatorService: DiscriminatorService,
		private readonly guildService: GuildService,
		private readonly userCacheService: UserCacheService,
		private readonly channelService: ChannelService,
		private readonly runtimeUserService: UserService,
		private readonly entityAssetService: EntityAssetService,
		private readonly assetDeletionQueue: IAssetDeletionQueue,
		private readonly storageService: IStorageService,
		private readonly reportService: ReportService,
		private readonly voiceRepository: VoiceRepository,
		private readonly bulkMessageDeletionQueue: KVBulkMessageDeletionQueueService,
		private readonly applicationRepository: ApplicationRepository,
		private readonly stripe: Stripe | null = null,
		private readonly jobLedger: JobLedgerRepository,
		private readonly storeEntitlementService: StoreEntitlementService,
	) {
		const {users, gateway, worker, snowflake} = this.apiContext.services;
		this.auditService = new AdminAuditService(this.adminRepository, snowflake, {
			userRepository: users,
			guildRepository: this.guildRepository,
			channelRepository: this.channelRepository.channelData,
		});
		this.banManagementService = new AdminBanManagementService({
			apiContext: this.apiContext,
			adminRepository: this.adminRepository,
			auditService: this.auditService,
		});
		const systemDm = {
			channelService: this.channelService,
			userChannelService: this.runtimeUserService.channelService,
			userCacheService: this.userCacheService,
		};
		const reporterResolutionNotifier = new ReporterResolutionNotifier({apiContext: this.apiContext, systemDm});
		this.assetPurgeService = new AdminAssetPurgeService({
			guildRepository: this.guildRepository,
			gatewayService: gateway,
			assetDeletionQueue: this.assetDeletionQueue,
			auditService: this.auditService,
		});
		this.messageService = new AdminMessageService({
			apiContext: this.apiContext,
			channelRepository: this.channelRepository,
			guildRepository: this.guildRepository,
			auditService: this.auditService,
		});
		this.reportServiceAggregate = new AdminReportService({
			apiContext: this.apiContext,
			reportService: this.reportService,
			reportRepository: getReportRepository(),
			guildRepository: this.guildRepository,
			channelRepository: this.channelRepository,
			storageService: this.storageService,
			auditService: this.auditService,
			userCacheService: this.userCacheService,
			reporterResolutionNotifier,
		});
		const userUpdateDeps = {
			userCacheService: this.userCacheService,
			userRepository: users,
			guildRepository: this.guildRepository,
			gatewayService: gateway,
		};
		const delivery = new EnforcementNoticeDelivery({
			apiContext: this.apiContext,
			applicationRepository: this.applicationRepository,
			systemDm,
		});
		this.enforcementService = new EnforcementDecisionService({
			apiContext: this.apiContext,
			userUpdateDeps,
			kvDeletionQueue: getKVAccountDeletionQueue(),
			repository: getEnforcementDecisionRepository(),
			delivery,
			auditService: this.auditService,
			banManagementService: this.banManagementService,
			channelRepository: this.channelRepository,
			guildRepository: this.guildRepository,
			reportService: this.reportService,
			adminReportService: this.reportServiceAggregate,
			onClosureEnded: queuePaymentDisputeClosures,
		});
		this.guildServiceAggregate = new AdminGuildService({
			guildRepository: this.guildRepository,
			userRepository: users,
			channelRepository: this.channelRepository,
			inviteRepository: this.inviteRepository,
			guildService: this.guildService,
			gatewayService: gateway,
			entityAssetService: this.entityAssetService,
			auditService: this.auditService,
			discoveryRepository: getGuildDiscoveryRepository(),
			enforcement: this.enforcementService,
			instanceConfigRepository: getInstanceConfigRepository(),
		});
		this.messageShredService = new AdminMessageShredService({
			apiContext: this.apiContext,
			auditService: this.auditService,
			enforcement: this.enforcementService,
		});
		this.messageDeletionService = new AdminMessageDeletionService({
			apiContext: this.apiContext,
			channelRepository: this.channelRepository,
			auditService: this.auditService,
			enforcement: this.enforcementService,
		});
		this.paymentDisputeEnforcement = new PaymentDisputeEnforcementService({
			repository: getPaymentDisputeRepository(),
			userRepository: users,
			userCacheService: this.userCacheService,
			emailService: this.apiContext.services.email,
			cacheService: this.apiContext.services.cache,
			gatewayService: gateway,
			auditService: this.auditService,
			kvDeletionQueue: getKVAccountDeletionQueue(),
			oauth2Tokens: getOAuth2TokenRepository(),
			systemDm,
			enforcement: this.enforcementService,
		});
		this.userService = new AdminUserService({
			apiContext: this.apiContext,
			guildRepository: this.guildRepository,
			channelRepository: this.channelRepository,
			discriminatorService: this.discriminatorService,
			entityAssetService: this.entityAssetService,
			auditService: this.auditService,
			userCacheService: this.userCacheService,
			banManagementService: this.banManagementService,
			kvDeletionQueue: getKVAccountDeletionQueue(),
			bulkMessageDeletionQueue: this.bulkMessageDeletionQueue,
			stripe: this.stripe,
			reportService: this.reportService,
			storeEntitlementService: this.storeEntitlementService,
			reporterResolutionNotifier,
			enforcement: this.enforcementService,
			delivery,
		});
		this.banService = new AdminUserBanService({
			apiContext: this.apiContext,
			userUpdateDeps,
			kvDeletionQueue: getKVAccountDeletionQueue(),
			auditService: this.auditService,
			enforcement: this.enforcementService,
			delivery,
		});
		this.voiceService = new AdminVoiceService({
			apiContext: this.apiContext,
			voiceRepository: this.voiceRepository,
			auditService: this.auditService,
		});
		this.badgeService = new AdminBadgeService({
			instanceConfigRepository: getInstanceConfigRepository(),
			entityAssetService: this.entityAssetService,
			assetDeletionQueue: this.assetDeletionQueue,
			snowflakeService: snowflake,
			userRepository: users,
			cacheService: this.apiContext.services.cache,
		});
		this.searchService = new AdminSearchService({
			apiContext: this.apiContext,
			guildRepository: this.guildRepository,
			auditService: this.auditService,
		});
		this.codeGenerationService = new AdminCodeGenerationService(users);
		this.applicationService = new AdminApplicationService({
			apiContext: this.apiContext,
			applicationRepository: this.applicationRepository,
			auditService: this.auditService,
			guildRepository: this.guildRepository,
		});
		this.jobAdminService = new JobAdminService(this.jobLedger, worker);
		this.relationshipService = new AdminUserRelationshipService({
			apiContext: this.apiContext,
			auditService: this.auditService,
			userRelationshipService: this.runtimeUserService.relationshipService,
		});
	}

	async sendSystemDm(
		data: {content: string; recipients: {kind: 'all'} | {kind: 'list'; userIds: Array<string>}},
		adminUserId: UserID,
		auditLogReason: string | null,
	): Promise<SendSystemDmResponse> {
		if (await systemDmsHidden()) {
			throw new DirectMessagesDisabledError();
		}
		const recipientCount = data.recipients.kind === 'all' ? null : data.recipients.userIds.length;
		await this.apiContext.services.worker.addJob(
			'sendSystemDm',
			data.recipients.kind === 'all'
				? {content: data.content, all_users: true}
				: {content: data.content, user_ids: data.recipients.userIds},
			{requireLedger: true},
		);
		const metadata = new Map<string, string>([
			['recipient_count', recipientCount === null ? 'all' : recipientCount.toString()],
			['content_length', data.content.length.toString()],
		]);
		await this.auditService.createAuditLog({
			adminUserId,
			targetType: 'system_dm',
			targetId: 0n,
			action: 'system_dm.send',
			auditLogReason,
			metadata,
		});
		return {recipient_count: recipientCount};
	}
}
