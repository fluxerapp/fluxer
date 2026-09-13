// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminAuditService} from '@app/api/admin/services/AdminAuditService';
import {AdminUserUpdatePropagator} from '@app/api/admin/services/AdminUserUpdatePropagator';
import {createUserID} from '@app/api/BrandedTypes';
import {reschedulePendingDeletion} from '@app/api/user/services/PendingDeletionCoordinator';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import {DeletionReasons} from '@fluxer/constants/src/Core';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {JobCancelledError} from '@pkgs/worker/src/contracts/WorkerTask';

interface Payload {
	user_ids: Array<string>;
	reason_code: number;
	days_until_deletion: number;
	public_reason: string | null;
	admin_user_id: string;
	audit_log_reason: string | null;
}

const MIN_USER_REQUESTED_DAYS = 14;
const MIN_STANDARD_DAYS = 60;
const handler: WorkerTaskHandler = async (rawPayload, helpers) => {
	const payload: Payload = {
		user_ids: rawPayload.user_ids as Array<string>,
		reason_code: rawPayload.reason_code as number,
		days_until_deletion: rawPayload.days_until_deletion as number,
		public_reason: (rawPayload.public_reason as string | null) ?? null,
		admin_user_id: rawPayload.admin_user_id as string,
		audit_log_reason: (rawPayload.audit_log_reason as string | null) ?? null,
	};
	const deps = getWorkerDependencies();
	const auditService = new AdminAuditService(deps.adminRepository, deps.snowflakeService);
	const propagator = new AdminUserUpdatePropagator({
		userCacheService: deps.userCacheService,
		userRepository: deps.userRepository,
		guildRepository: deps.guildRepository,
		gatewayService: deps.gatewayService,
	});
	const adminUserId = createUserID(BigInt(payload.admin_user_id));
	const userIds = payload.user_ids.map((id) => BigInt(id));
	const minDays = payload.reason_code === DeletionReasons.USER_REQUESTED ? MIN_USER_REQUESTED_DAYS : MIN_STANDARD_DAYS;
	const daysUntilDeletion = Math.max(payload.days_until_deletion, minDays);
	const total = userIds.length;
	const successful: Array<string> = [];
	const failed: Array<{
		id: string;
		error: string;
	}> = [];
	await helpers.setContextLink(`/users?ids=${userIds.slice(0, 50).join(',')}`);
	await helpers.reportProgress(0, total, `Scheduling deletion of ${total} users in ${daysUntilDeletion} days`);
	for (let i = 0; i < userIds.length; i++) {
		if (await helpers.shouldCancel()) throw new JobCancelledError();
		const userIdBigInt = userIds[i]!;
		const userId = createUserID(userIdBigInt);
		try {
			const user = await deps.userRepository.findUnique(userId);
			if (!user) throw new Error('user_not_found');
			const pendingDeletionAt = new Date();
			pendingDeletionAt.setDate(pendingDeletionAt.getDate() + daysUntilDeletion);
			const updatedUser = await deps.userRepository.updateDeletionSchedule(user, {
				flags: user.flags | UserFlags.DELETED,
				pending_deletion_at: pendingDeletionAt,
				deletion_reason_code: payload.reason_code,
				deletion_public_reason: payload.public_reason ?? null,
				deletion_audit_log_reason: payload.audit_log_reason ?? null,
			});
			await reschedulePendingDeletion({
				userId,
				currentPendingDeletionAt: user.pendingDeletionAt,
				nextPendingDeletionAt: pendingDeletionAt,
				deletionReasonCode: payload.reason_code,
				userRepository: deps.userRepository,
				deletionQueue: deps.deletionQueueService,
			});
			await propagator.propagateUserUpdate({userId, oldUser: user, updatedUser});
			if (user.email) {
				try {
					await deps.emailService.sendAccountScheduledForDeletionEmail(
						user.email,
						user.username,
						payload.public_reason ?? null,
						pendingDeletionAt,
						user.locale,
					);
				} catch (emailErr) {
					helpers.logger.warn({err: emailErr, userId: userId.toString()}, 'Failed to send deletion email');
				}
			}
			await auditService.createAuditLog({
				adminUserId,
				targetType: 'user',
				targetId: BigInt(userId),
				action: 'schedule_deletion',
				auditLogReason: null,
				metadata: new Map([['days', daysUntilDeletion.toString()]]),
			});
			successful.push(userId.toString());
		} catch (err) {
			failed.push({id: userIdBigInt.toString(), error: err instanceof Error ? err.message : String(err)});
		}
		if ((i + 1) % 10 === 0) {
			await helpers.reportProgress(i + 1, total, null);
		}
	}
	await auditService.createAuditLog({
		adminUserId,
		targetType: 'user',
		targetId: BigInt(0),
		action: 'bulk_schedule_deletion',
		auditLogReason: payload.audit_log_reason,
		metadata: new Map([
			['user_count', total.toString()],
			['reason_code', payload.reason_code.toString()],
			['days', daysUntilDeletion.toString()],
			['successful', successful.length.toString()],
			['failed', failed.length.toString()],
		]),
	});
	await helpers.reportProgress(total, total, `+${successful.length} ok, ${failed.length} failed`);
	helpers.logger.info({successful: successful.length, failed: failed.length}, 'bulkScheduleUserDeletion complete');
};

export default handler;
