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

import type {IAdminRepository} from '@fluxer/api/src/admin/IAdminRepository';
import {SystemDmJobRepository} from '@fluxer/api/src/admin/repositories/SystemDmJobRepository';
import {AdminAssetPurgeService} from '@fluxer/api/src/admin/services/AdminAssetPurgeService';
import {AdminAuditService} from '@fluxer/api/src/admin/services/AdminAuditService';
import {AdminCodeGenerationService} from '@fluxer/api/src/admin/services/AdminCodeGenerationService';
import {AdminGuildService} from '@fluxer/api/src/admin/services/AdminGuildService';
import {AdminMessageDeletionService} from '@fluxer/api/src/admin/services/AdminMessageDeletionService';
import {AdminMessageService} from '@fluxer/api/src/admin/services/AdminMessageService';
import {AdminMessageShredService} from '@fluxer/api/src/admin/services/AdminMessageShredService';
import {AdminReportService} from '@fluxer/api/src/admin/services/AdminReportService';
import {AdminSearchService} from '@fluxer/api/src/admin/services/AdminSearchService';
import {AdminSnowflakeReservationService} from '@fluxer/api/src/admin/services/AdminSnowflakeReservationService';
import {AdminUserService} from '@fluxer/api/src/admin/services/AdminUserService';
import {AdminVisionarySlotService} from '@fluxer/api/src/admin/services/AdminVisionarySlotService';
import {AdminVoiceService} from '@fluxer/api/src/admin/services/AdminVoiceService';
import {SystemDmService} from '@fluxer/api/src/admin/services/SystemDmService';
import type {AuthService} from '@fluxer/api/src/auth/AuthService';
import type {AttachmentID, ChannelID, GuildID, ReportID, UserID} from '@fluxer/api/src/BrandedTypes';
import type {IChannelRepository} from '@fluxer/api/src/channel/IChannelRepository';
import {GuildDiscoveryRepository} from '@fluxer/api/src/guild/repositories/GuildDiscoveryRepository';
import type {IGuildRepositoryAggregate} from '@fluxer/api/src/guild/repositories/IGuildRepositoryAggregate';
import type {GuildService} from '@fluxer/api/src/guild/services/GuildService';
import type {IDiscriminatorService} from '@fluxer/api/src/infrastructure/DiscriminatorService';
import type {EntityAssetService} from '@fluxer/api/src/infrastructure/EntityAssetService';
import type {IAssetDeletionQueue} from '@fluxer/api/src/infrastructure/IAssetDeletionQueue';
import type {IGatewayService} from '@fluxer/api/src/infrastructure/IGatewayService';
import type {IMediaService} from '@fluxer/api/src/infrastructure/IMediaService';
import type {IStorageService} from '@fluxer/api/src/infrastructure/IStorageService';
import type {KVBulkMessageDeletionQueueService} from '@fluxer/api/src/infrastructure/KVBulkMessageDeletionQueueService';
import {getMetricsService} from '@fluxer/api/src/infrastructure/MetricsService';
import type {SnowflakeService} from '@fluxer/api/src/infrastructure/SnowflakeService';
import type {UserCacheService} from '@fluxer/api/src/infrastructure/UserCacheService';
import {SnowflakeReservationRepository} from '@fluxer/api/src/instance/SnowflakeReservationRepository';
import type {InviteRepository} from '@fluxer/api/src/invite/InviteRepository';
import type {RequestCache} from '@fluxer/api/src/middleware/RequestCacheMiddleware';
import type {BotMfaMirrorService} from '@fluxer/api/src/oauth/BotMfaMirrorService';
import type {ReportService} from '@fluxer/api/src/report/ReportService';
import type {IUserRepository} from '@fluxer/api/src/user/IUserRepository';
import {VisionarySlotRepository} from '@fluxer/api/src/user/repositories/VisionarySlotRepository';
import type {UserContactChangeLogService} from '@fluxer/api/src/user/services/UserContactChangeLogService';
import type {VoiceRepository} from '@fluxer/api/src/voice/VoiceRepository';
import type {ICacheService} from '@fluxer/cache/src/ICacheService';
import type {IEmailService} from '@fluxer/email/src/IEmailService';
import type {
	BanGuildMemberRequest,
	BulkAddGuildMembersRequest,
	BulkUpdateGuildFeaturesRequest,
	ClearGuildFieldsRequest,
	ForceAddUserToGuildRequest,
	KickGuildMemberRequest,
	ListGuildMembersRequest,
	ListUserGuildsRequest,
	LookupGuildRequest,
	TransferGuildOwnershipRequest,
	UpdateGuildNameRequest,
	UpdateGuildSettingsRequest,
	UpdateGuildVanityRequest,
} from '@fluxer/schema/src/domains/admin/AdminGuildSchemas';
import type {
	DeleteAllUserMessagesRequest,
	DeleteMessageRequest,
	LookupMessageByAttachmentRequest,
	LookupMessageRequest,
	MessageShredRequest,
} from '@fluxer/schema/src/domains/admin/AdminMessageSchemas';
import type {
	ListAuditLogsRequest,
	ListGuildEmojisResponse,
	ListGuildStickersResponse,
	PurgeGuildAssetsRequest,
	PurgeGuildAssetsResponse,
	SearchReportsRequest,
	SystemDmJobResponse,
} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import type {
	BulkScheduleUserDeletionRequest,
	BulkUpdateUserFlagsRequest,
	CancelBulkMessageDeletionRequest,
	ChangeDobRequest,
	ChangeEmailRequest,
	ChangeUsernameRequest,
	ClearUserFieldsRequest,
	DeleteWebAuthnCredentialRequest,
	DisableForSuspiciousActivityRequest,
	DisableMfaRequest,
	ListUserChangeLogRequest,
	ListUserDmChannelsRequest,
	ListWebAuthnCredentialsRequest,
	LookupUserRequest,
	ResendVerificationEmailRequest,
	SendPasswordResetRequest,
	SetUserAclsRequest,
	SetUserBotStatusRequest,
	SetUserSystemStatusRequest,
	SetUserTraitsRequest,
	TempBanUserRequest,
	TerminateSessionsRequest,
	UnlinkPhoneRequest,
	UpdateSuspiciousActivityFlagsRequest,
	VerifyUserEmailRequest,
} from '@fluxer/schema/src/domains/admin/AdminUserSchemas';
import type {
	CreateVoiceRegionRequest,
	CreateVoiceServerRequest,
	DeleteVoiceRegionRequest,
	DeleteVoiceServerRequest,
	GetVoiceRegionRequest,
	GetVoiceServerRequest,
	ListVoiceRegionsRequest,
	ListVoiceServersRequest,
	UpdateVoiceRegionRequest,
	UpdateVoiceServerRequest,
} from '@fluxer/schema/src/domains/admin/AdminVoiceSchemas';
import type {IWorkerService} from '@fluxer/worker/src/contracts/IWorkerService';

interface ForceAddUserToGuildParams {
	data: ForceAddUserToGuildRequest;
	requestCache: RequestCache;
}

interface LookupAttachmentParams {
	channelId: ChannelID;
	attachmentId: AttachmentID;
	filename: string;
}

export class AdminService {
	private readonly auditService: AdminAuditService;
	private readonly userService: AdminUserService;
	private readonly guildServiceAggregate: AdminGuildService;
	private readonly messageService: AdminMessageService;
	private readonly messageShredService: AdminMessageShredService;
	private readonly messageDeletionService: AdminMessageDeletionService;
	private readonly reportServiceAggregate: AdminReportService;
	private readonly voiceService: AdminVoiceService;
	private readonly searchService: AdminSearchService;
	private readonly codeGenerationService: AdminCodeGenerationService;
	private readonly systemDmService: SystemDmService;
	private readonly assetPurgeService: AdminAssetPurgeService;
	private readonly snowflakeReservationService: AdminSnowflakeReservationService;
	private readonly visionarySlotService: AdminVisionarySlotService;

	constructor(
		private readonly userRepository: IUserRepository,
		private readonly guildRepository: IGuildRepositoryAggregate,
		private readonly channelRepository: IChannelRepository,
		private readonly adminRepository: IAdminRepository,
		private readonly inviteRepository: InviteRepository,
		private readonly discriminatorService: IDiscriminatorService,
		private readonly snowflakeService: SnowflakeService,
		private readonly guildService: GuildService,
		private readonly authService: AuthService,
		private readonly gatewayService: IGatewayService,
		private readonly userCacheService: UserCacheService,
		private readonly entityAssetService: EntityAssetService,
		private readonly assetDeletionQueue: IAssetDeletionQueue,
		private readonly emailService: IEmailService,
		private readonly mediaService: IMediaService,
		private readonly storageService: IStorageService,
		private readonly reportService: ReportService,
		private readonly workerService: IWorkerService,
		private readonly cacheService: ICacheService,
		private readonly voiceRepository: VoiceRepository,
		private readonly botMfaMirrorService: BotMfaMirrorService,
		private readonly contactChangeLogService: UserContactChangeLogService,
		private readonly bulkMessageDeletionQueue: KVBulkMessageDeletionQueueService,
	) {
		this.auditService = new AdminAuditService(this.adminRepository, this.snowflakeService);
		this.userService = new AdminUserService({
			userRepository: this.userRepository,
			guildRepository: this.guildRepository,
			discriminatorService: this.discriminatorService,
			authService: this.authService,
			emailService: this.emailService,
			entityAssetService: this.entityAssetService,
			auditService: this.auditService,
			gatewayService: this.gatewayService,
			userCacheService: this.userCacheService,
			adminRepository: this.adminRepository,
			botMfaMirrorService: this.botMfaMirrorService,
			contactChangeLogService: this.contactChangeLogService,
			bulkMessageDeletionQueue: this.bulkMessageDeletionQueue,
			cacheService: this.cacheService,
		});
		this.guildServiceAggregate = new AdminGuildService({
			guildRepository: this.guildRepository,
			userRepository: this.userRepository,
			channelRepository: this.channelRepository,
			inviteRepository: this.inviteRepository,
			guildService: this.guildService,
			gatewayService: this.gatewayService,
			entityAssetService: this.entityAssetService,
			auditService: this.auditService,
			discoveryRepository: new GuildDiscoveryRepository(),
		});
		this.assetPurgeService = new AdminAssetPurgeService({
			guildRepository: this.guildRepository,
			gatewayService: this.gatewayService,
			assetDeletionQueue: this.assetDeletionQueue,
			auditService: this.auditService,
		});
		this.messageService = new AdminMessageService({
			channelRepository: this.channelRepository,
			userCacheService: this.userCacheService,
			mediaService: this.mediaService,
			gatewayService: this.gatewayService,
			auditService: this.auditService,
		});
		this.messageShredService = new AdminMessageShredService({
			workerService: this.workerService,
			cacheService: this.cacheService,
			snowflakeService: this.snowflakeService,
			auditService: this.auditService,
		});
		this.messageDeletionService = new AdminMessageDeletionService({
			channelRepository: this.channelRepository,
			messageShredService: this.messageShredService,
			auditService: this.auditService,
		});
		this.reportServiceAggregate = new AdminReportService({
			reportService: this.reportService,
			userRepository: this.userRepository,
			emailService: this.emailService,
			storageService: this.storageService,
			auditService: this.auditService,
			userCacheService: this.userCacheService,
		});
		this.voiceService = new AdminVoiceService({
			voiceRepository: this.voiceRepository,
			cacheService: this.cacheService,
			auditService: this.auditService,
		});
		this.searchService = new AdminSearchService({
			guildRepository: this.guildRepository,
			userRepository: this.userRepository,
			workerService: this.workerService,
			cacheService: this.cacheService,
			snowflakeService: this.snowflakeService,
			auditService: this.auditService,
		});
		const systemDmJobRepository = new SystemDmJobRepository();
		this.systemDmService = new SystemDmService(
			this.userRepository,
			systemDmJobRepository,
			this.auditService,
			this.workerService,
			this.snowflakeService,
		);

		this.snowflakeReservationService = new AdminSnowflakeReservationService({
			repository: new SnowflakeReservationRepository(),
			cacheService: this.cacheService,
			auditService: this.auditService,
		});
		this.visionarySlotService = new AdminVisionarySlotService({
			repository: new VisionarySlotRepository(),
			auditService: this.auditService,
		});
		this.codeGenerationService = new AdminCodeGenerationService(this.userRepository);
	}

	async lookupUser(data: LookupUserRequest) {
		return this.userService.lookupUser(data);
	}

	async listSnowflakeReservations() {
		return this.snowflakeReservationService.listReservations();
	}

	async setSnowflakeReservation(
		data: {email: string; snowflake: string},
		adminUserId: UserID,
		auditLogReason: string | null,
	) {
		return this.snowflakeReservationService.setReservation(data, adminUserId, auditLogReason);
	}

	async deleteSnowflakeReservation(data: {email: string}, adminUserId: UserID, auditLogReason: string | null) {
		return this.snowflakeReservationService.deleteReservation(data, adminUserId, auditLogReason);
	}

	async expandVisionarySlots(data: {count: number}, adminUserId: UserID, auditLogReason: string | null) {
		return this.visionarySlotService.expandSlots(data, adminUserId, auditLogReason);
	}

	async shrinkVisionarySlots(data: {targetCount: number}, adminUserId: UserID, auditLogReason: string | null) {
		return this.visionarySlotService.shrinkSlots(data, adminUserId, auditLogReason);
	}

	async setVisionarySlotReservation(
		data: {slotIndex: number; userId: UserID | null},
		adminUserId: UserID,
		auditLogReason: string | null,
	) {
		return this.visionarySlotService.setSlotReservation(data, adminUserId, auditLogReason);
	}

	async swapVisionarySlots(
		data: {slotIndexA: number; slotIndexB: number},
		adminUserId: UserID,
		auditLogReason: string | null,
	) {
		return this.visionarySlotService.swapSlots(data, adminUserId, auditLogReason);
	}

	async createSystemDmJob(
		data: {
			content: string;
			registrationStart?: Date;
			registrationEnd?: Date;
			excludedGuildIds: Array<GuildID>;
		},
		adminUserId: UserID,
		auditLogReason: string | null,
	): Promise<SystemDmJobResponse> {
		return this.systemDmService.createJob(data, adminUserId, auditLogReason);
	}

	async listSystemDmJobs(
		limit: number,
		beforeJobId?: bigint,
	): Promise<{jobs: Array<SystemDmJobResponse>; next_cursor: string | null}> {
		return this.systemDmService.listJobs(limit, beforeJobId);
	}

	async approveSystemDmJob(
		jobId: bigint,
		adminUserId: UserID,
		auditLogReason: string | null,
	): Promise<SystemDmJobResponse> {
		return this.systemDmService.approveJob(jobId, adminUserId, auditLogReason);
	}

	async updateUserFlags(args: {
		userId: UserID;
		data: {addFlags: Array<bigint>; removeFlags: Array<bigint>};
		adminUserId: UserID;
		auditLogReason: string | null;
	}) {
		return this.userService.updateUserFlags(args);
	}

	async disableMfa(data: DisableMfaRequest, adminUserId: UserID, auditLogReason: string | null) {
		return this.userService.disableMfa(data, adminUserId, auditLogReason);
	}

	async clearUserFields(data: ClearUserFieldsRequest, adminUserId: UserID, auditLogReason: string | null) {
		return this.userService.clearUserFields(data, adminUserId, auditLogReason);
	}

	async setUserBotStatus(data: SetUserBotStatusRequest, adminUserId: UserID, auditLogReason: string | null) {
		return this.userService.setUserBotStatus(data, adminUserId, auditLogReason);
	}

	async setUserSystemStatus(data: SetUserSystemStatusRequest, adminUserId: UserID, auditLogReason: string | null) {
		return this.userService.setUserSystemStatus(data, adminUserId, auditLogReason);
	}

	async verifyUserEmail(data: VerifyUserEmailRequest, adminUserId: UserID, auditLogReason: string | null) {
		return this.userService.verifyUserEmail(data, adminUserId, auditLogReason);
	}

	async resendVerificationEmail(
		data: ResendVerificationEmailRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
	) {
		return this.userService.resendVerificationEmail(data, adminUserId, auditLogReason);
	}

	async sendPasswordReset(data: SendPasswordResetRequest, adminUserId: UserID, auditLogReason: string | null) {
		return this.userService.sendPasswordReset(data, adminUserId, auditLogReason);
	}

	async changeUsername(data: ChangeUsernameRequest, adminUserId: UserID, auditLogReason: string | null) {
		return this.userService.changeUsername(data, adminUserId, auditLogReason);
	}

	async changeEmail(data: ChangeEmailRequest, adminUserId: UserID, auditLogReason: string | null) {
		return this.userService.changeEmail(data, adminUserId, auditLogReason);
	}

	async terminateSessions(data: TerminateSessionsRequest, adminUserId: UserID, auditLogReason: string | null) {
		return this.userService.terminateSessions(data, adminUserId, auditLogReason);
	}

	async tempBanUser(data: TempBanUserRequest, adminUserId: UserID, auditLogReason: string | null) {
		return this.userService.tempBanUser(data, adminUserId, auditLogReason);
	}

	async unbanUser(data: {user_id: bigint}, adminUserId: UserID, auditLogReason: string | null) {
		return this.userService.unbanUser(data, adminUserId, auditLogReason);
	}

	async scheduleAccountDeletion(
		data:
			| BulkScheduleUserDeletionRequest
			| {user_id: bigint; reason_code: number; public_reason?: string | null; days_until_deletion: number},
		adminUserId: UserID,
		auditLogReason: string | null,
	) {
		if ('user_ids' in data) {
			return this.userService.bulkScheduleUserDeletion(data, adminUserId, auditLogReason);
		}
		return this.userService.scheduleAccountDeletion(
			{
				user_id: data.user_id,
				reason_code: data.reason_code,
				public_reason: data.public_reason ?? undefined,
				days_until_deletion: data.days_until_deletion,
			},
			adminUserId,
			auditLogReason,
		);
	}

	async cancelAccountDeletion(data: {user_id: bigint}, adminUserId: UserID, auditLogReason: string | null) {
		return this.userService.cancelAccountDeletion(data, adminUserId, auditLogReason);
	}

	async cancelBulkMessageDeletion(
		data: CancelBulkMessageDeletionRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
	) {
		return this.userService.cancelBulkMessageDeletion(data, adminUserId, auditLogReason);
	}

	async banIp(data: {ip: string}, adminUserId: UserID, auditLogReason: string | null) {
		const result = await this.userService.banIp(data, adminUserId, auditLogReason);
		getMetricsService().counter({
			name: 'fluxer.admin_bans.ip.created',
			dimensions: {
				reason_type: auditLogReason ? 'provided' : 'none',
			},
		});
		return result;
	}

	async unbanIp(data: {ip: string}, adminUserId: UserID, auditLogReason: string | null) {
		const result = await this.userService.unbanIp(data, adminUserId, auditLogReason);
		getMetricsService().counter({
			name: 'fluxer.admin_bans.ip.deleted',
			dimensions: {
				reason_type: auditLogReason ? 'provided' : 'none',
			},
		});
		return result;
	}

	async checkIpBan(data: {ip: string}) {
		return this.userService.checkIpBan(data);
	}

	async listIpBans(data: {limit: number}) {
		return this.userService.listIpBans(data);
	}

	async banEmail(data: {email: string}, adminUserId: UserID, auditLogReason: string | null) {
		const result = await this.userService.banEmail(data, adminUserId, auditLogReason);
		getMetricsService().counter({
			name: 'fluxer.admin_bans.email.created',
			dimensions: {
				reason_type: auditLogReason ? 'provided' : 'none',
			},
		});
		return result;
	}

	async unbanEmail(data: {email: string}, adminUserId: UserID, auditLogReason: string | null) {
		const result = await this.userService.unbanEmail(data, adminUserId, auditLogReason);
		getMetricsService().counter({
			name: 'fluxer.admin_bans.email.deleted',
			dimensions: {
				reason_type: auditLogReason ? 'provided' : 'none',
			},
		});
		return result;
	}

	async checkEmailBan(data: {email: string}) {
		return this.userService.checkEmailBan(data);
	}

	async listEmailBans(data: {limit: number}) {
		return this.userService.listEmailBans(data);
	}

	async banPhone(data: {phone: string}, adminUserId: UserID, auditLogReason: string | null) {
		const result = await this.userService.banPhone(data, adminUserId, auditLogReason);
		getMetricsService().counter({
			name: 'fluxer.admin_bans.phone.created',
			dimensions: {
				reason_type: auditLogReason ? 'provided' : 'none',
			},
		});
		return result;
	}

	async unbanPhone(data: {phone: string}, adminUserId: UserID, auditLogReason: string | null) {
		const result = await this.userService.unbanPhone(data, adminUserId, auditLogReason);
		getMetricsService().counter({
			name: 'fluxer.admin_bans.phone.deleted',
			dimensions: {
				reason_type: auditLogReason ? 'provided' : 'none',
			},
		});
		return result;
	}

	async checkPhoneBan(data: {phone: string}) {
		return this.userService.checkPhoneBan(data);
	}

	async listPhoneBans(data: {limit: number}) {
		return this.userService.listPhoneBans(data);
	}

	async setUserAcls(data: SetUserAclsRequest, adminUserId: UserID, auditLogReason: string | null) {
		return this.userService.setUserAcls(data, adminUserId, auditLogReason);
	}

	async setUserTraits(data: SetUserTraitsRequest, adminUserId: UserID, auditLogReason: string | null) {
		return this.userService.setUserTraits(data, adminUserId, auditLogReason);
	}

	async unlinkPhone(data: UnlinkPhoneRequest, adminUserId: UserID, auditLogReason: string | null) {
		return this.userService.unlinkPhone(data, adminUserId, auditLogReason);
	}

	async changeDob(data: ChangeDobRequest, adminUserId: UserID, auditLogReason: string | null) {
		return this.userService.changeDob(data, adminUserId, auditLogReason);
	}

	async updateSuspiciousActivityFlags(
		data: UpdateSuspiciousActivityFlagsRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
	) {
		return this.userService.updateSuspiciousActivityFlags(data, adminUserId, auditLogReason);
	}

	async disableForSuspiciousActivity(
		data: DisableForSuspiciousActivityRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
	) {
		return this.userService.disableForSuspiciousActivity(data, adminUserId, auditLogReason);
	}

	async bulkUpdateUserFlags(data: BulkUpdateUserFlagsRequest, adminUserId: UserID, auditLogReason: string | null) {
		return this.userService.bulkUpdateUserFlags(data, adminUserId, auditLogReason);
	}

	async bulkScheduleUserDeletion(
		data: BulkScheduleUserDeletionRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
	) {
		return this.userService.bulkScheduleUserDeletion(data, adminUserId, auditLogReason);
	}

	async listUserSessions(userId: bigint, adminUserId: UserID, auditLogReason: string | null) {
		return this.userService.listUserSessions(userId, adminUserId, auditLogReason);
	}

	async listWebAuthnCredentials(
		data: ListWebAuthnCredentialsRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
	) {
		return this.userService.listWebAuthnCredentials(data, adminUserId, auditLogReason);
	}

	async deleteWebAuthnCredential(
		data: DeleteWebAuthnCredentialRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
	) {
		return this.userService.deleteWebAuthnCredential(data, adminUserId, auditLogReason);
	}

	async listUserDmChannels(data: ListUserDmChannelsRequest) {
		return this.userService.listUserDmChannels(data);
	}

	async listUserChangeLog(data: ListUserChangeLogRequest) {
		return this.userService.listUserChangeLog(data);
	}

	async updateGuildFeatures(args: {
		guildId: GuildID;
		addFeatures: Array<string>;
		removeFeatures: Array<string>;
		adminUserId: UserID;
		auditLogReason: string | null;
	}) {
		return this.guildServiceAggregate.updateGuildFeatures(args);
	}

	async forceAddUserToGuild({
		data,
		requestCache,
		adminUserId,
		auditLogReason,
	}: ForceAddUserToGuildParams & {adminUserId: UserID; auditLogReason: string | null}) {
		return this.guildServiceAggregate.forceAddUserToGuild({data, requestCache, adminUserId, auditLogReason});
	}

	async lookupGuild(data: LookupGuildRequest) {
		return this.guildServiceAggregate.lookupGuild(data);
	}

	async listUserGuilds(data: ListUserGuildsRequest) {
		return this.guildServiceAggregate.listUserGuilds(data);
	}

	async listGuildMembers(data: ListGuildMembersRequest) {
		return this.guildServiceAggregate.listGuildMembers(data);
	}

	async banGuildMember(data: BanGuildMemberRequest, adminUserId: UserID, auditLogReason: string | null) {
		return this.guildServiceAggregate.banGuildMember(data, adminUserId, auditLogReason);
	}

	async kickGuildMember(data: KickGuildMemberRequest, adminUserId: UserID, auditLogReason: string | null) {
		return this.guildServiceAggregate.kickGuildMember(data, adminUserId, auditLogReason);
	}

	async listGuildEmojis(guildId: GuildID): Promise<ListGuildEmojisResponse> {
		return this.guildServiceAggregate.listGuildEmojis(guildId);
	}

	async listGuildStickers(guildId: GuildID): Promise<ListGuildStickersResponse> {
		return this.guildServiceAggregate.listGuildStickers(guildId);
	}

	async purgeGuildAssets(
		data: PurgeGuildAssetsRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
	): Promise<PurgeGuildAssetsResponse> {
		return this.assetPurgeService.purgeGuildAssets({
			ids: data.ids,
			adminUserId,
			auditLogReason,
		});
	}

	async clearGuildFields(data: ClearGuildFieldsRequest, adminUserId: UserID, auditLogReason: string | null) {
		return this.guildServiceAggregate.clearGuildFields(data, adminUserId, auditLogReason);
	}

	async updateGuildName(data: UpdateGuildNameRequest, adminUserId: UserID, auditLogReason: string | null) {
		return this.guildServiceAggregate.updateGuildName(data, adminUserId, auditLogReason);
	}

	async updateGuildSettings(data: UpdateGuildSettingsRequest, adminUserId: UserID, auditLogReason: string | null) {
		return this.guildServiceAggregate.updateGuildSettings(data, adminUserId, auditLogReason);
	}

	async updateGuildVanity(data: UpdateGuildVanityRequest, adminUserId: UserID, auditLogReason: string | null) {
		return this.guildServiceAggregate.updateGuildVanity(data, adminUserId, auditLogReason);
	}

	async transferGuildOwnership(
		data: TransferGuildOwnershipRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
	) {
		return this.guildServiceAggregate.transferGuildOwnership(data, adminUserId, auditLogReason);
	}

	async bulkUpdateGuildFeatures(
		data: BulkUpdateGuildFeaturesRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
	) {
		return this.guildServiceAggregate.bulkUpdateGuildFeatures(data, adminUserId, auditLogReason);
	}

	async bulkAddGuildMembers(data: BulkAddGuildMembersRequest, adminUserId: UserID, auditLogReason: string | null) {
		return this.guildServiceAggregate.bulkAddGuildMembers(data, adminUserId, auditLogReason);
	}

	async reloadGuild(guildId: bigint, adminUserId: UserID, auditLogReason: string | null) {
		return this.guildServiceAggregate.reloadGuild(guildId, adminUserId, auditLogReason);
	}

	async shutdownGuild(guildId: bigint, adminUserId: UserID, auditLogReason: string | null) {
		return this.guildServiceAggregate.shutdownGuild(guildId, adminUserId, auditLogReason);
	}

	async deleteGuild(guildId: bigint, adminUserId: UserID, auditLogReason: string | null) {
		return this.guildServiceAggregate.deleteGuild(guildId, adminUserId, auditLogReason);
	}

	async getGuildMemoryStats(limit: number) {
		return this.guildServiceAggregate.getGuildMemoryStats(limit);
	}

	async reloadAllGuilds(guildIds: Array<GuildID>) {
		return this.guildServiceAggregate.reloadAllGuilds(guildIds);
	}

	async getNodeStats() {
		return this.guildServiceAggregate.getNodeStats();
	}

	async lookupAttachment(params: LookupAttachmentParams) {
		return this.messageService.lookupAttachment(params);
	}

	async lookupMessage(data: LookupMessageRequest) {
		return this.messageService.lookupMessage(data);
	}

	async lookupMessageByAttachment(data: LookupMessageByAttachmentRequest) {
		return this.messageService.lookupMessageByAttachment(data);
	}

	async deleteMessage(data: DeleteMessageRequest, adminUserId: UserID, auditLogReason: string | null) {
		return this.messageService.deleteMessage(data, adminUserId, auditLogReason);
	}

	async deleteAllUserMessages(data: DeleteAllUserMessagesRequest, adminUserId: UserID, auditLogReason: string | null) {
		return this.messageDeletionService.deleteAllUserMessages(data, adminUserId, auditLogReason);
	}

	async queueMessageShred(data: MessageShredRequest, adminUserId: UserID, auditLogReason: string | null) {
		return this.messageShredService.queueMessageShred(data, adminUserId, auditLogReason);
	}

	async getMessageShredStatus(jobId: string) {
		return this.messageShredService.getMessageShredStatus(jobId);
	}

	async listAuditLogs(data: ListAuditLogsRequest) {
		return this.auditService.listAuditLogs(data);
	}

	async searchAuditLogs(data: {
		query?: string;
		admin_user_id?: bigint;
		target_id?: string;
		sort_by?: 'createdAt' | 'relevance';
		sort_order?: 'asc' | 'desc';
		limit?: number;
		offset?: number;
	}) {
		return this.auditService.searchAuditLogs(data);
	}

	async listReports(status: number, limit?: number, offset?: number) {
		return this.reportServiceAggregate.listReports(status, limit, offset);
	}

	async getReport(reportId: ReportID) {
		return this.reportServiceAggregate.getReport(reportId);
	}

	async resolveReport(
		reportId: ReportID,
		adminUserId: UserID,
		publicComment: string | null,
		auditLogReason: string | null,
	) {
		return this.reportServiceAggregate.resolveReport(reportId, adminUserId, publicComment, auditLogReason);
	}

	async searchReports(data: SearchReportsRequest) {
		return this.reportServiceAggregate.searchReports(data);
	}

	async searchGuilds(data: {query?: string; limit: number; offset: number}) {
		return this.searchService.searchGuilds(data);
	}

	async searchUsers(data: {query?: string; limit: number; offset: number}) {
		return this.searchService.searchUsers(data);
	}

	async refreshSearchIndex(
		data: {
			index_type:
				| 'guilds'
				| 'users'
				| 'reports'
				| 'audit_logs'
				| 'channel_messages'
				| 'guild_members'
				| 'favorite_memes'
				| 'discovery';
			guild_id?: bigint;
			user_id?: bigint;
		},
		adminUserId: UserID,
		auditLogReason: string | null,
	) {
		return this.searchService.refreshSearchIndex(data, adminUserId, auditLogReason);
	}

	async getIndexRefreshStatus(jobId: string) {
		return this.searchService.getIndexRefreshStatus(jobId);
	}

	async listVoiceRegions(data: ListVoiceRegionsRequest, _adminUserId: UserID, _auditLogReason: string | null) {
		return this.voiceService.listVoiceRegions(data);
	}

	async getVoiceRegion(data: GetVoiceRegionRequest, _adminUserId: UserID, _auditLogReason: string | null) {
		return this.voiceService.getVoiceRegion(data);
	}

	async createVoiceRegion(data: CreateVoiceRegionRequest, adminUserId: UserID, auditLogReason: string | null) {
		return this.voiceService.createVoiceRegion(data, adminUserId, auditLogReason);
	}

	async updateVoiceRegion(data: UpdateVoiceRegionRequest, adminUserId: UserID, auditLogReason: string | null) {
		return this.voiceService.updateVoiceRegion(data, adminUserId, auditLogReason);
	}

	async deleteVoiceRegion(data: DeleteVoiceRegionRequest, adminUserId: UserID, auditLogReason: string | null) {
		return this.voiceService.deleteVoiceRegion(data, adminUserId, auditLogReason);
	}

	async listVoiceServers(data: ListVoiceServersRequest, _adminUserId: UserID, _auditLogReason: string | null) {
		return this.voiceService.listVoiceServers(data);
	}

	async getVoiceServer(data: GetVoiceServerRequest, _adminUserId: UserID, _auditLogReason: string | null) {
		return this.voiceService.getVoiceServer(data);
	}

	async createVoiceServer(data: CreateVoiceServerRequest, adminUserId: UserID, auditLogReason: string | null) {
		return this.voiceService.createVoiceServer(data, adminUserId, auditLogReason);
	}

	async updateVoiceServer(data: UpdateVoiceServerRequest, adminUserId: UserID, auditLogReason: string | null) {
		return this.voiceService.updateVoiceServer(data, adminUserId, auditLogReason);
	}

	async deleteVoiceServer(data: DeleteVoiceServerRequest, adminUserId: UserID, auditLogReason: string | null) {
		return this.voiceService.deleteVoiceServer(data, adminUserId, auditLogReason);
	}

	async generateGiftCodes(count: number, durationMonths: number) {
		return this.codeGenerationService.generateGiftCodes(count, durationMonths);
	}
}
