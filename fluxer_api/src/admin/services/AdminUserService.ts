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

import type {IAdminRepository} from '~/admin/IAdminRepository';
import type {AuthService} from '~/auth/AuthService';
import {createUserID, type UserID} from '~/BrandedTypes';
import {UnknownUserError} from '~/Errors';
import type {IGuildRepository} from '~/guild/IGuildRepository';
import type {IDiscriminatorService} from '~/infrastructure/DiscriminatorService';
import type {EntityAssetService} from '~/infrastructure/EntityAssetService';
import type {ICacheService} from '~/infrastructure/ICacheService';
import type {IEmailService} from '~/infrastructure/IEmailService';
import type {IGatewayService} from '~/infrastructure/IGatewayService';
import type {PendingJoinInviteStore} from '~/infrastructure/PendingJoinInviteStore';
import type {RedisBulkMessageDeletionQueueService} from '~/infrastructure/RedisBulkMessageDeletionQueueService';
import type {UserCacheService} from '~/infrastructure/UserCacheService';
import type {InviteService} from '~/invite/InviteService';
import type {BotMfaMirrorService} from '~/oauth/BotMfaMirrorService';
import type {IUserRepository} from '~/user/IUserRepository';
import type {UserContactChangeLogService} from '~/user/services/UserContactChangeLogService';
import type {
	BulkScheduleUserDeletionRequest,
	BulkUpdateUserFlagsRequest,
	CancelBulkMessageDeletionRequest,
	ChangeDobRequest,
	ChangeEmailRequest,
	ChangeUsernameRequest,
	ClearUserFieldsRequest,
	DisableForSuspiciousActivityRequest,
	DisableMfaRequest,
	ListUserChangeLogRequest,
	ScheduleAccountDeletionRequest,
	SendPasswordResetRequest,
	SetUserAclsRequest,
	SetUserBotStatusRequest,
	SetUserSystemStatusRequest,
	TempBanUserRequest,
	TerminateSessionsRequest,
	UnlinkPhoneRequest,
	UpdateSuspiciousActivityFlagsRequest,
	VerifyUserEmailRequest,
} from '../AdminModel';
import type {AdminAuditService} from './AdminAuditService';
import {AdminBanManagementService} from './AdminBanManagementService';
import {AdminUserBanService} from './AdminUserBanService';
import {AdminUserDeletionService} from './AdminUserDeletionService';
import {AdminUserLookupService} from './AdminUserLookupService';
import {AdminUserProfileService} from './AdminUserProfileService';
import {AdminUserRegistrationService} from './AdminUserRegistrationService';
import {AdminUserSecurityService} from './AdminUserSecurityService';
import {AdminUserUpdatePropagator} from './AdminUserUpdatePropagator';

interface LookupUserRequest {
	query: string;
}

interface AdminUserServiceDeps {
	userRepository: IUserRepository;
	guildRepository: IGuildRepository;
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
	bulkMessageDeletionQueue: RedisBulkMessageDeletionQueueService;
	inviteService: InviteService;
	pendingJoinInviteStore: PendingJoinInviteStore;
	cacheService: ICacheService;
}

export class AdminUserService {
	private readonly lookupService: AdminUserLookupService;
	private readonly profileService: AdminUserProfileService;
	private readonly securityService: AdminUserSecurityService;
	private readonly banService: AdminUserBanService;
	private readonly deletionService: AdminUserDeletionService;
	private readonly banManagementService: AdminBanManagementService;
	private readonly registrationService: AdminUserRegistrationService;
	private readonly updatePropagator: AdminUserUpdatePropagator;
	private readonly contactChangeLogService: UserContactChangeLogService;
	private readonly auditService: AdminAuditService;
	private readonly userRepository: IUserRepository;
	private readonly bulkMessageDeletionQueue: RedisBulkMessageDeletionQueueService;

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
		});

		this.deletionService = new AdminUserDeletionService({
			userRepository: deps.userRepository,
			authService: deps.authService,
			emailService: deps.emailService,
			auditService: deps.auditService,
			updatePropagator: this.updatePropagator,
		});

		this.banManagementService = new AdminBanManagementService({
			adminRepository: deps.adminRepository,
			auditService: deps.auditService,
		});

		this.registrationService = new AdminUserRegistrationService({
			userRepository: deps.userRepository,
			adminRepository: deps.adminRepository,
			emailService: deps.emailService,
			auditService: deps.auditService,
			updatePropagator: this.updatePropagator,
			inviteService: deps.inviteService,
			pendingJoinInviteStore: deps.pendingJoinInviteStore,
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

	async terminateSessions(data: TerminateSessionsRequest, adminUserId: UserID, auditLogReason: string | null) {
		return this.securityService.terminateSessions(data, adminUserId, auditLogReason);
	}

	async setUserAcls(data: SetUserAclsRequest, adminUserId: UserID, auditLogReason: string | null) {
		return this.securityService.setUserAcls(data, adminUserId, auditLogReason);
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

		await this.userRepository.patchUpsert(userId, {
			pending_bulk_message_deletion_at: null,
			pending_bulk_message_deletion_channel_count: null,
			pending_bulk_message_deletion_message_count: null,
		});

		await this.bulkMessageDeletionQueue.removeFromQueue(userId);

		await this.auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'cancel_bulk_message_deletion',
			auditLogReason,
			metadata: new Map(),
		});
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

	async listPendingVerifications(limit: number = 100) {
		return this.registrationService.listPendingVerifications(limit);
	}

	async approveRegistration(userId: UserID, adminUserId: UserID, auditLogReason: string | null) {
		return this.registrationService.approveRegistration(userId, adminUserId, auditLogReason);
	}

	async rejectRegistration(userId: UserID, adminUserId: UserID, auditLogReason: string | null) {
		return this.registrationService.rejectRegistration(userId, adminUserId, auditLogReason);
	}

	async bulkApproveRegistrations(userIds: Array<UserID>, adminUserId: UserID, auditLogReason: string | null) {
		return this.registrationService.bulkApproveRegistrations(userIds, adminUserId, auditLogReason);
	}

	async bulkRejectRegistrations(userIds: Array<UserID>, adminUserId: UserID, auditLogReason: string | null) {
		return this.registrationService.bulkRejectRegistrations(userIds, adminUserId, auditLogReason);
	}
}
