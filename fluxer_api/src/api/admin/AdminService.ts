// SPDX-License-Identifier: AGPL-3.0-or-later

import type {SendSystemDmResponse} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import type {IpInfoService} from '@pkgs/geoip/src/IpInfoService';
import type Stripe from 'stripe';
import type {ApiContext} from '../ApiContext';
import type {UserID} from '../BrandedTypes';
import type {IChannelRepository} from '../channel/IChannelRepository';
import type {ChannelService} from '../channel/services/ChannelService';
import type {IGuildRepositoryAggregate} from '../guild/repositories/IGuildRepositoryAggregate';
import type {GuildService} from '../guild/services/GuildService';
import type {IDiscriminatorService} from '../infrastructure/DiscriminatorService';
import type {EntityAssetService} from '../infrastructure/EntityAssetService';
import type {IAssetDeletionQueue} from '../infrastructure/IAssetDeletionQueue';
import type {IStorageService} from '../infrastructure/IStorageService';
import type {KVBulkMessageDeletionQueueService} from '../infrastructure/KVBulkMessageDeletionQueueService';
import type {UserCacheService} from '../infrastructure/UserCacheService';
import type {InviteRepository} from '../invite/InviteRepository';
import type {IJobLedgerRepository} from '../jobs/IJobLedgerRepository';
import {JobAdminService} from '../jobs/JobAdminService';
import {
	getGuildDiscoveryRepository,
	getKVAccountDeletionQueue,
	getNcmecSubmissionService,
} from '../middleware/ServiceSingletons';
import type {IApplicationRepository} from '../oauth/repositories/IApplicationRepository';
import type {ReportService} from '../report/ReportService';
import type {IRiskHistoryRepository} from '../risk/HistoricalOutcomeRepository';
import type {ISuspiciousIpRepository} from '../risk/SuspiciousIpRepository';
import type {UserService} from '../user/services/UserService';
import type {VoiceRepository} from '../voice/VoiceRepository';
import type {IAdminRepository} from './IAdminRepository';
import {AdminApplicationService} from './services/AdminApplicationService';
import {AdminAssetPurgeService} from './services/AdminAssetPurgeService';
import {AdminAuditService} from './services/AdminAuditService';
import {AdminBanManagementService} from './services/AdminBanManagementService';
import {AdminCodeGenerationService} from './services/AdminCodeGenerationService';
import {AdminGuildService} from './services/AdminGuildService';
import {AdminMessageDeletionService} from './services/AdminMessageDeletionService';
import {AdminMessageService} from './services/AdminMessageService';
import {AdminMessageShredService} from './services/AdminMessageShredService';
import {AdminReportService} from './services/AdminReportService';
import {AdminSearchService} from './services/AdminSearchService';
import {AdminUserRelationshipService} from './services/AdminUserRelationshipService';
import {AdminUserService} from './services/AdminUserService';
import {AdminVoiceService} from './services/AdminVoiceService';

export class AdminService {
	readonly auditService: AdminAuditService;
	readonly banManagementService: AdminBanManagementService;
	readonly userService: AdminUserService;
	readonly guildServiceAggregate: AdminGuildService;
	readonly messageService: AdminMessageService;
	readonly messageShredService: AdminMessageShredService;
	readonly messageDeletionService: AdminMessageDeletionService;
	readonly reportServiceAggregate: AdminReportService;
	readonly voiceService: AdminVoiceService;
	readonly searchService: AdminSearchService;
	readonly codeGenerationService: AdminCodeGenerationService;
	readonly assetPurgeService: AdminAssetPurgeService;
	readonly applicationService: AdminApplicationService;
	readonly jobAdminService: JobAdminService;
	readonly relationshipService: AdminUserRelationshipService;

	constructor(
		private readonly apiContext: ApiContext,
		private readonly guildRepository: IGuildRepositoryAggregate,
		private readonly channelRepository: IChannelRepository,
		private readonly adminRepository: IAdminRepository,
		private readonly inviteRepository: InviteRepository,
		private readonly discriminatorService: IDiscriminatorService,
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
		private readonly applicationRepository: IApplicationRepository,
		private readonly stripe: Stripe | null = null,
		private readonly riskHistoryRepository: Pick<IRiskHistoryRepository, 'recordOutcomeForUser'>,
		private readonly jobLedger: IJobLedgerRepository,
		private readonly ipInfoService: IpInfoService,
		private readonly suspiciousIpRepository: ISuspiciousIpRepository,
	) {
		const {users, gateway, worker, snowflake} = this.apiContext.services;
		this.auditService = new AdminAuditService(this.adminRepository, snowflake, {
			userRepository: users,
			guildRepository: this.guildRepository,
			channelRepository: this.channelRepository,
		});
		this.banManagementService = new AdminBanManagementService({
			apiContext: this.apiContext,
			adminRepository: this.adminRepository,
			auditService: this.auditService,
			ipInfoService: this.ipInfoService,
			suspiciousIpRepository: this.suspiciousIpRepository,
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
			riskHistoryRepository: this.riskHistoryRepository,
			reportService: this.reportService,
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
		});
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
			ncmecSubmissionService: getNcmecSubmissionService(),
		});
		this.messageShredService = new AdminMessageShredService({
			apiContext: this.apiContext,
			auditService: this.auditService,
		});
		this.messageDeletionService = new AdminMessageDeletionService({
			channelRepository: this.channelRepository,
			messageShredService: this.messageShredService,
			auditService: this.auditService,
		});
		this.reportServiceAggregate = new AdminReportService({
			apiContext: this.apiContext,
			reportService: this.reportService,
			guildRepository: this.guildRepository,
			channelRepository: this.channelRepository,
			channelService: this.channelService,
			storageService: this.storageService,
			auditService: this.auditService,
			userCacheService: this.userCacheService,
			userChannelService: this.runtimeUserService.channelService,
			ncmecSubmissionService: getNcmecSubmissionService(),
		});
		this.voiceService = new AdminVoiceService({
			apiContext: this.apiContext,
			voiceRepository: this.voiceRepository,
			auditService: this.auditService,
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
		});
	}

	async sendSystemDm(
		data: {content: string; userIds: Array<string>},
		adminUserId: UserID,
		auditLogReason: string | null,
	): Promise<SendSystemDmResponse> {
		await this.apiContext.services.worker.addJob(
			'sendSystemDm',
			{
				content: data.content,
				user_ids: data.userIds,
			},
			{requireLedger: true},
		);
		const metadata = new Map<string, string>([
			['recipient_count', data.userIds.length.toString()],
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
		return {recipient_count: data.userIds.length};
	}
}
