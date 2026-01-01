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

import type {AuthService} from '~/auth/AuthService';
import {createUserID, type UserID} from '~/BrandedTypes';
import {DeletionReasons, UserFlags} from '~/Constants';
import {UnknownUserError} from '~/Errors';
import type {IEmailService} from '~/infrastructure/IEmailService';
import type {IUserRepository} from '~/user/IUserRepository';
import type {BulkScheduleUserDeletionRequest, ScheduleAccountDeletionRequest} from '../AdminModel';
import type {AdminAuditService} from './AdminAuditService';
import type {AdminUserUpdatePropagator} from './AdminUserUpdatePropagator';

interface AdminUserDeletionServiceDeps {
	userRepository: IUserRepository;
	authService: AuthService;
	emailService: IEmailService;
	auditService: AdminAuditService;
	updatePropagator: AdminUserUpdatePropagator;
}

const minUserRequestedDeletionDays = 14;
const minStandardDeletionDays = 60;

export class AdminUserDeletionService {
	constructor(private readonly deps: AdminUserDeletionServiceDeps) {}

	async scheduleAccountDeletion(
		data: ScheduleAccountDeletionRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
	) {
		const {userRepository, authService, emailService, auditService, updatePropagator} = this.deps;
		const userId = createUserID(data.user_id);
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}

		const minDays =
			data.reason_code === DeletionReasons.USER_REQUESTED ? minUserRequestedDeletionDays : minStandardDeletionDays;
		const daysUntilDeletion = Math.max(data.days_until_deletion, minDays);
		const pendingDeletionAt = new Date();
		pendingDeletionAt.setDate(pendingDeletionAt.getDate() + daysUntilDeletion);

		const updatedUser = await userRepository.patchUpsert(userId, {
			flags: user.flags | UserFlags.DELETED,
			pending_deletion_at: pendingDeletionAt,
			deletion_reason_code: data.reason_code,
			deletion_public_reason: data.public_reason ?? null,
			deletion_audit_log_reason: auditLogReason,
		});

		await userRepository.addPendingDeletion(userId, pendingDeletionAt, data.reason_code);

		await authService.terminateAllUserSessions(userId);
		await updatePropagator.propagateUserUpdate({userId, oldUser: user, updatedUser: updatedUser!});

		if (user.email) {
			await emailService.sendAccountScheduledForDeletionEmail(
				user.email,
				user.username,
				data.public_reason ?? null,
				pendingDeletionAt,
				user.locale,
			);
		}

		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: data.user_id,
			action: 'schedule_deletion',
			auditLogReason,
			metadata: new Map([['days', daysUntilDeletion.toString()]]),
		});
	}

	async cancelAccountDeletion(data: {user_id: bigint}, adminUserId: UserID, auditLogReason: string | null) {
		const {userRepository, emailService, auditService, updatePropagator} = this.deps;
		const userId = createUserID(data.user_id);
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}

		if (user.pendingDeletionAt) {
			await userRepository.removePendingDeletion(userId, user.pendingDeletionAt);
		}

		const updatedUser = await userRepository.patchUpsert(userId, {
			flags: user.flags & ~UserFlags.DELETED & ~UserFlags.SELF_DELETED,
			pending_deletion_at: null,
			deletion_reason_code: null,
			deletion_public_reason: null,
			deletion_audit_log_reason: null,
		});
		await updatePropagator.propagateUserUpdate({userId, oldUser: user, updatedUser: updatedUser!});

		if (user.email) {
			await emailService.sendUnbanNotification(
				user.email,
				user.username,
				auditLogReason || 'deletion canceled',
				user.locale,
			);
		}

		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'cancel_deletion',
			auditLogReason,
			metadata: new Map(),
		});
	}

	async bulkScheduleUserDeletion(
		data: BulkScheduleUserDeletionRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
	) {
		const {auditService} = this.deps;
		const successful: Array<string> = [];
		const failed: Array<{id: string; error: string}> = [];

		for (const userIdBigInt of data.user_ids) {
			try {
				await this.scheduleAccountDeletion(
					{
						user_id: userIdBigInt,
						reason_code: data.reason_code,
						public_reason: data.public_reason,
						days_until_deletion: data.days_until_deletion,
					},
					adminUserId,
					null,
				);
				successful.push(userIdBigInt.toString());
			} catch (error) {
				failed.push({
					id: userIdBigInt.toString(),
					error: error instanceof Error ? error.message : 'Unknown error',
				});
			}
		}

		const bulkMinDays =
			data.reason_code === DeletionReasons.USER_REQUESTED ? minUserRequestedDeletionDays : minStandardDeletionDays;
		const bulkDaysUntilDeletion = Math.max(data.days_until_deletion, bulkMinDays);

		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(0),
			action: 'bulk_schedule_deletion',
			auditLogReason,
			metadata: new Map([
				['user_count', data.user_ids.length.toString()],
				['reason_code', data.reason_code.toString()],
				['days', bulkDaysUntilDeletion.toString()],
			]),
		});

		return {
			successful,
			failed,
		};
	}
}
