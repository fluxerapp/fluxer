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

import {mapUserToAdminResponse} from '~/admin/AdminModel';
import type {IAdminRepository} from '~/admin/IAdminRepository';
import {createInviteCode, type UserID} from '~/BrandedTypes';
import {UserFlags} from '~/Constants';
import {InputValidationError, UnknownUserError} from '~/Errors';
import type {ICacheService} from '~/infrastructure/ICacheService';
import type {IEmailService} from '~/infrastructure/IEmailService';
import type {PendingJoinInviteStore} from '~/infrastructure/PendingJoinInviteStore';
import type {InviteService} from '~/invite/InviteService';
import {Logger} from '~/Logger';
import {createRequestCache} from '~/middleware/RequestCacheMiddleware';
import type {IUserRepository} from '~/user/IUserRepository';
import type {AdminAuditService} from './AdminAuditService';
import type {AdminUserUpdatePropagator} from './AdminUserUpdatePropagator';

interface AdminUserRegistrationServiceDeps {
	userRepository: IUserRepository;
	adminRepository: IAdminRepository;
	emailService: IEmailService;
	auditService: AdminAuditService;
	updatePropagator: AdminUserUpdatePropagator;
	inviteService: InviteService;
	pendingJoinInviteStore: PendingJoinInviteStore;
	cacheService: ICacheService;
}

export class AdminUserRegistrationService {
	constructor(private readonly deps: AdminUserRegistrationServiceDeps) {}

	async listPendingVerifications(limit: number = 100) {
		const {adminRepository, userRepository, cacheService} = this.deps;
		const pendingVerifications = await adminRepository.listPendingVerifications(limit);
		const userIds = pendingVerifications.map((pv) => pv.userId);
		const users = await userRepository.listUsers(userIds);
		const userMap = new Map(users.map((u) => [u.id.toString(), u]));

		const mappedVerifications = await Promise.all(
			pendingVerifications.map(async (pv) => {
				const metadataEntries = Array.from((pv.metadata ?? new Map()).entries()).map(([key, value]) => ({
					key,
					value,
				}));

				return {
					user_id: pv.userId.toString(),
					created_at: pv.createdAt.toISOString(),
					user: await mapUserToAdminResponse(userMap.get(pv.userId.toString())!, cacheService),
					metadata: metadataEntries,
				};
			}),
		);

		return {
			pending_verifications: mappedVerifications,
		};
	}

	async approveRegistration(userId: UserID, adminUserId: UserID, auditLogReason: string | null) {
		const {
			userRepository,
			adminRepository,
			emailService,
			auditService,
			updatePropagator,
			inviteService,
			pendingJoinInviteStore,
		} = this.deps;
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}

		if ((user.flags & UserFlags.PENDING_MANUAL_VERIFICATION) === 0n) {
			throw InputValidationError.create('user_id', 'User is not pending verification');
		}

		await adminRepository.removePendingVerification(userId);
		const updatedUser = await userRepository.patchUpsert(userId, {
			flags: user.flags & ~UserFlags.PENDING_MANUAL_VERIFICATION,
		});

		await updatePropagator.propagateUserUpdate({userId, oldUser: user, updatedUser: updatedUser!});

		if (user.email) {
			await emailService.sendRegistrationApprovedEmail(user.email, user.username, user.locale);
		}

		const pendingInviteCode = await pendingJoinInviteStore.getPendingInvite(userId);
		if (pendingInviteCode) {
			try {
				await inviteService.acceptInvite({
					userId,
					inviteCode: createInviteCode(pendingInviteCode),
					requestCache: createRequestCache(),
				});
			} catch (error) {
				Logger.warn(
					{userId, inviteCode: pendingInviteCode, error},
					'Failed to auto-join invite after approving registration',
				);
			} finally {
				await pendingJoinInviteStore.deletePendingInvite(userId);
			}
		}

		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'approve_registration',
			auditLogReason,
			metadata: new Map(),
		});

		return {success: true};
	}

	async rejectRegistration(userId: UserID, adminUserId: UserID, auditLogReason: string | null) {
		const {userRepository, adminRepository, auditService} = this.deps;
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}

		if ((user.flags & UserFlags.PENDING_MANUAL_VERIFICATION) === 0n) {
			throw InputValidationError.create('user_id', 'User is not pending verification');
		}

		await adminRepository.removePendingVerification(userId);

		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'reject_registration',
			auditLogReason,
			metadata: new Map(),
		});

		return {success: true};
	}

	async bulkApproveRegistrations(userIds: Array<UserID>, adminUserId: UserID, auditLogReason: string | null) {
		for (const userId of userIds) {
			await this.approveRegistration(userId, adminUserId, auditLogReason);
		}

		return {
			success: true,
			processed: userIds.length,
		};
	}

	async bulkRejectRegistrations(userIds: Array<UserID>, adminUserId: UserID, auditLogReason: string | null) {
		for (const userId of userIds) {
			await this.rejectRegistration(userId, adminUserId, auditLogReason);
		}

		return {
			success: true,
			processed: userIds.length,
		};
	}
}
