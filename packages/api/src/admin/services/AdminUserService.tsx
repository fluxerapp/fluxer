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
import type {AdminAuditService} from '@fluxer/api/src/admin/services/AdminAuditService';
import {AdminBanManagementService} from '@fluxer/api/src/admin/services/AdminBanManagementService';
import {AdminUserBanService} from '@fluxer/api/src/admin/services/AdminUserBanService';
import {AdminUserDeletionService} from '@fluxer/api/src/admin/services/AdminUserDeletionService';
import {AdminUserLookupService} from '@fluxer/api/src/admin/services/AdminUserLookupService';
import {AdminUserProfileService} from '@fluxer/api/src/admin/services/AdminUserProfileService';
import {AdminUserSecurityService} from '@fluxer/api/src/admin/services/AdminUserSecurityService';
import {AdminUserUpdatePropagator} from '@fluxer/api/src/admin/services/AdminUserUpdatePropagator';
import type {AuthService} from '@fluxer/api/src/auth/AuthService';
import {createChannelID, createUserID, type UserID} from '@fluxer/api/src/BrandedTypes';
import type {IGuildRepositoryAggregate} from '@fluxer/api/src/guild/repositories/IGuildRepositoryAggregate';
import type {IDiscriminatorService} from '@fluxer/api/src/infrastructure/DiscriminatorService';
import type {EntityAssetService} from '@fluxer/api/src/infrastructure/EntityAssetService';
import type {IGatewayService} from '@fluxer/api/src/infrastructure/IGatewayService';
import type {KVBulkMessageDeletionQueueService} from '@fluxer/api/src/infrastructure/KVBulkMessageDeletionQueueService';
import type {UserCacheService} from '@fluxer/api/src/infrastructure/UserCacheService';
import type {BotMfaMirrorService} from '@fluxer/api/src/oauth/BotMfaMirrorService';
import type {IUserRepository} from '@fluxer/api/src/user/IUserRepository';
import type {UserContactChangeLogService} from '@fluxer/api/src/user/services/UserContactChangeLogService';
import type {ICacheService} from '@fluxer/cache/src/ICacheService';
import type {IEmailService} from '@fluxer/email/src/IEmailService';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
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
	ScheduleAccountDeletionRequest,
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
import {mapUserToAdminResponse} from '../models/UserTypes';

interface AdminUserServiceDeps {
	userRepository: IUserRepository;
	guildRepository: IGuildRepositoryAggregate;
	discriminatorService: IDiscriminatorService;
	authService: AuthService;
	emailService: IEmailService;
	entityAssetService: EntityAssetService;
	auditService: AdminAuditService;
	gatewayService: IGatewayService;
	userCacheService: UserCacheService;
	adminRepository: IAdminRepository;
	botMfaMirrorService: BotMfaMirrorService;
	contactChangeLogService: UserContactChangeLogService;
	bulkMessageDeletionQueue: KVBulkMessageDeletionQueueService;
	cacheService: ICacheService;
}

export class AdminUserService {
	private readonly lookupService: AdminUserLookupService;
	private readonly profileService: AdminUserProfileService;
	private readonly securityService: AdminUserSecurityService;
	private readonly banService: AdminUserBanService;
	private readonly deletionService: AdminUserDeletionService;
	private readonly banManagementService: AdminBanManagementService;
	private readonly updatePropagator: AdminUserUpdatePropagator;
	private readonly contactChangeLogService: UserContactChangeLogService;
	private readonly auditService: AdminAuditService;
	private readonly userRepository: IUserRepository;
	private readonly bulkMessageDeletionQueue: KVBulkMessageDeletionQueueService;
	private readonly cacheService: ICacheService;

	constructor(deps: AdminUserServiceDeps) {
		this.updatePropagator = new AdminUserUpdatePropagator({
			userCacheService: deps.userCacheService,
			userRepository: deps.userRepository,
			guildRepository: deps.guildRepository,
			gatewayService: deps.gatewayService,
		});

		this.userRepository = deps.userRepository;
		this.auditService = deps.auditService;
		this.bulkMessageDeletionQueue = deps.bulkMessageDeletionQueue;
		this.cacheService = deps.cacheService;

		this.lookupService = new AdminUserLookupService({
			userRepository: deps.userRepository,
			cacheService: deps.cacheService,
		});

		this.profileService = new AdminUserProfileService({
			userRepository: deps.userRepository,
			discriminatorService: deps.discriminatorService,
			entityAssetService: deps.entityAssetService,
			auditService: deps.auditService,
			updatePropagator: this.updatePropagator,
			contactChangeLogService: deps.contactChangeLogService,
			cacheService: deps.cacheService,
			guildRepository: deps.guildRepository,
		});

		this.securityService = new AdminUserSecurityService({
			userRepository: deps.userRepository,
			authService: deps.authService,
			emailService: deps.emailService,
			auditService: deps.auditService,
			updatePropagator: this.updatePropagator,
			botMfaMirrorService: deps.botMfaMirrorService,
			contactChangeLogService: deps.contactChangeLogService,
			cacheService: deps.cacheService,
		});

		this.banService = new AdminUserBanService({
			userRepository: deps.userRepository,
			authService: deps.authService,
			emailService: deps.emailService,
			auditService: deps.auditService,
			updatePropagator: this.updatePropagator,
			cacheService: deps.cacheService,
		});

		this.deletionService = new AdminUserDeletionService({
			userRepository: deps.userRepository,
			authService: deps.authService,
			emailService: deps.emailService,
			auditService: deps.auditService,
			updatePropagator: this.updatePropagator,
			cacheService: deps.cacheService,
		});

		this.banManagementService = new AdminBanManagementService({
			adminRepository: deps.adminRepository,
			auditService: deps.auditService,
			cacheService: deps.cacheService,
		});

		this.contactChangeLogService = deps.contactChangeLogService;
	}

	async lookupUser(data: LookupUserRequest) {
		return this.lookupService.lookupUser(data);
	}

	async clearUserFields(data: ClearUserFieldsRequest, adminUserId: UserID, auditLogReason: string | null) {
		return this.profileService.clearUserFields(data, adminUserId, auditLogReason);
	}

	async setUserBotStatus(data: SetUserBotStatusRequest, adminUserId: UserID, auditLogReason: string | null) {
		return this.profileService.setUserBotStatus(data, adminUserId, auditLogReason);
	}

	async setUserSystemStatus(data: SetUserSystemStatusRequest, adminUserId: UserID, auditLogReason: string | null) {
		return this.profileService.setUserSystemStatus(data, adminUserId, auditLogReason);
	}

	async verifyUserEmail(data: VerifyUserEmailRequest, adminUserId: UserID, auditLogReason: string | null) {
		return this.profileService.verifyUserEmail(data, adminUserId, auditLogReason);
	}

	async changeUsername(data: ChangeUsernameRequest, adminUserId: UserID, auditLogReason: string | null) {
		return this.profileService.changeUsername(data, adminUserId, auditLogReason);
	}

	async changeEmail(data: ChangeEmailRequest, adminUserId: UserID, auditLogReason: string | null) {
		return this.profileService.changeEmail(data, adminUserId, auditLogReason);
	}

	async changeDob(data: ChangeDobRequest, adminUserId: UserID, auditLogReason: string | null) {
		return this.profileService.changeDob(data, adminUserId, auditLogReason);
	}

	async updateUserFlags({
		userId,
		data,
		adminUserId,
		auditLogReason,
	}: {
		userId: UserID;
		data: {addFlags: Array<bigint>; removeFlags: Array<bigint>};
		adminUserId: UserID;
		auditLogReason: string | null;
	}) {
		return this.securityService.updateUserFlags({userId, data, adminUserId, auditLogReason});
	}

	async disableMfa(data: DisableMfaRequest, adminUserId: UserID, auditLogReason: string | null) {
		return this.securityService.disableMfa(data, adminUserId, auditLogReason);
	}

	async sendPasswordReset(data: SendPasswordResetRequest, adminUserId: UserID, auditLogReason: string | null) {
		return this.securityService.sendPasswordReset(data, adminUserId, auditLogReason);
	}

	async resendVerificationEmail(
		data: ResendVerificationEmailRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
	) {
		return this.securityService.resendVerificationEmail(data, adminUserId, auditLogReason);
	}

	async terminateSessions(data: TerminateSessionsRequest, adminUserId: UserID, auditLogReason: string | null) {
		return this.securityService.terminateSessions(data, adminUserId, auditLogReason);
	}

	async setUserAcls(data: SetUserAclsRequest, adminUserId: UserID, auditLogReason: string | null) {
		return this.securityService.setUserAcls(data, adminUserId, auditLogReason);
	}

	async setUserTraits(data: SetUserTraitsRequest, adminUserId: UserID, auditLogReason: string | null) {
		return this.securityService.setUserTraits(data, adminUserId, auditLogReason);
	}

	async unlinkPhone(data: UnlinkPhoneRequest, adminUserId: UserID, auditLogReason: string | null) {
		return this.securityService.unlinkPhone(data, adminUserId, auditLogReason);
	}

	async updateSuspiciousActivityFlags(
		data: UpdateSuspiciousActivityFlagsRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
	) {
		return this.securityService.updateSuspiciousActivityFlags(data, adminUserId, auditLogReason);
	}

	async disableForSuspiciousActivity(
		data: DisableForSuspiciousActivityRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
	) {
		return this.securityService.disableForSuspiciousActivity(data, adminUserId, auditLogReason);
	}

	async bulkUpdateUserFlags(data: BulkUpdateUserFlagsRequest, adminUserId: UserID, auditLogReason: string | null) {
		return this.securityService.bulkUpdateUserFlags(data, adminUserId, auditLogReason);
	}

	async listUserSessions(userId: bigint, adminUserId: UserID, auditLogReason: string | null) {
		return this.securityService.listUserSessions(userId, adminUserId, auditLogReason);
	}

	async listWebAuthnCredentials(
		data: ListWebAuthnCredentialsRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
	) {
		return this.securityService.listWebAuthnCredentials(data, adminUserId, auditLogReason);
	}

	async deleteWebAuthnCredential(
		data: DeleteWebAuthnCredentialRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
	) {
		return this.securityService.deleteWebAuthnCredential(data, adminUserId, auditLogReason);
	}

	async listUserDmChannels(data: ListUserDmChannelsRequest) {
		const userId = createUserID(data.user_id);
		const user = await this.userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}

		const channels = await this.userRepository.listHistoricalDmChannelsPaginated(userId, {
			limit: data.limit,
			beforeChannelId: data.before ? createChannelID(data.before) : undefined,
			afterChannelId: data.after ? createChannelID(data.after) : undefined,
		});

		return {
			channels: channels.map((channel) => ({
				channel_id: channel.channelId.toString(),
				channel_type: channel.channelType,
				recipient_ids: channel.recipientIds.map((recipientId) => recipientId.toString()),
				last_message_id: channel.lastMessageId?.toString() ?? null,
				is_open: channel.open,
			})),
		};
	}

	async listUserChangeLog(data: ListUserChangeLogRequest) {
		const entries = await this.contactChangeLogService.listLogs({
			userId: createUserID(data.user_id),
			limit: data.limit,
			beforeEventId: data.page_token,
		});

		const nextPageToken =
			entries.length === data.limit && entries.length > 0 ? entries.at(-1)!.event_id.toString() : null;

		return {
			entries: entries.map((entry) => ({
				event_id: entry.event_id.toString(),
				field: entry.field,
				old_value: entry.old_value ?? null,
				new_value: entry.new_value ?? null,
				reason: entry.reason,
				actor_user_id: entry.actor_user_id ? entry.actor_user_id.toString() : null,
				event_at: entry.event_at.toISOString(),
			})),
			next_page_token: nextPageToken,
		};
	}

	async tempBanUser(data: TempBanUserRequest, adminUserId: UserID, auditLogReason: string | null) {
		return this.banService.tempBanUser(data, adminUserId, auditLogReason);
	}

	async unbanUser(data: {user_id: bigint}, adminUserId: UserID, auditLogReason: string | null) {
		return this.banService.unbanUser(data, adminUserId, auditLogReason);
	}

	async scheduleAccountDeletion(
		data: ScheduleAccountDeletionRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
	) {
		return this.deletionService.scheduleAccountDeletion(data, adminUserId, auditLogReason);
	}

	async cancelAccountDeletion(data: {user_id: bigint}, adminUserId: UserID, auditLogReason: string | null) {
		return this.deletionService.cancelAccountDeletion(data, adminUserId, auditLogReason);
	}

	async cancelBulkMessageDeletion(
		data: CancelBulkMessageDeletionRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
	) {
		const userId = createUserID(data.user_id);
		const user = await this.userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}

		const updatedUser = await this.userRepository.patchUpsert(
			userId,
			{
				pending_bulk_message_deletion_at: null,
				pending_bulk_message_deletion_channel_count: null,
				pending_bulk_message_deletion_message_count: null,
			},
			user.toRow(),
		);

		await this.bulkMessageDeletionQueue.removeFromQueue(userId);

		await this.auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'cancel_bulk_message_deletion',
			auditLogReason,
			metadata: new Map(),
		});

		return {
			user: await mapUserToAdminResponse(updatedUser, this.cacheService),
		};
	}

	async bulkScheduleUserDeletion(
		data: BulkScheduleUserDeletionRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
	) {
		return this.deletionService.bulkScheduleUserDeletion(data, adminUserId, auditLogReason);
	}

	async banIp(data: {ip: string}, adminUserId: UserID, auditLogReason: string | null) {
		return this.banManagementService.banIp(data, adminUserId, auditLogReason);
	}

	async unbanIp(data: {ip: string}, adminUserId: UserID, auditLogReason: string | null) {
		return this.banManagementService.unbanIp(data, adminUserId, auditLogReason);
	}

	async checkIpBan(data: {ip: string}) {
		return this.banManagementService.checkIpBan(data);
	}

	async banEmail(data: {email: string}, adminUserId: UserID, auditLogReason: string | null) {
		return this.banManagementService.banEmail(data, adminUserId, auditLogReason);
	}

	async unbanEmail(data: {email: string}, adminUserId: UserID, auditLogReason: string | null) {
		return this.banManagementService.unbanEmail(data, adminUserId, auditLogReason);
	}

	async checkEmailBan(data: {email: string}) {
		return this.banManagementService.checkEmailBan(data);
	}

	async banPhone(data: {phone: string}, adminUserId: UserID, auditLogReason: string | null) {
		return this.banManagementService.banPhone(data, adminUserId, auditLogReason);
	}

	async unbanPhone(data: {phone: string}, adminUserId: UserID, auditLogReason: string | null) {
		return this.banManagementService.unbanPhone(data, adminUserId, auditLogReason);
	}

	async checkPhoneBan(data: {phone: string}) {
		return this.banManagementService.checkPhoneBan(data);
	}

	async listIpBans(data: {limit: number}) {
		return this.banManagementService.listIpBans(data);
	}

	async listEmailBans(data: {limit: number}) {
		return this.banManagementService.listEmailBans(data);
	}

	async listPhoneBans(data: {limit: number}) {
		return this.banManagementService.listPhoneBans(data);
	}
}
