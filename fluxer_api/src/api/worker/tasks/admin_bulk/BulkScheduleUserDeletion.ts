// SPDX-License-Identifier: AGPL-3.0-or-later

import {resolveDeletionDays} from '@app/api/admin/services/AdminUserDeletionService';
import {createUserID} from '@app/api/BrandedTypes';
import {runAdminBulkJob} from '@app/api/worker/tasks/admin_bulk/AdminBulkJob';
import {createAdminBulkServices} from '@app/api/worker/tasks/admin_bulk/AdminBulkServices';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {ScheduleAccountDeletionRequest} from '@fluxer/schema/src/domains/admin/AdminUserSchemas';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';

interface Payload {
	user_ids: Array<string>;
	reason_code: ScheduleAccountDeletionRequest['reason_code'];
	days_until_deletion: number;
	public_reason: string | null;
	admin_user_id: string;
	audit_log_reason: string | null;
}

const handler: WorkerTaskHandler = async (rawPayload, helpers) => {
	const payload: Payload = {
		user_ids: rawPayload.user_ids as Array<string>,
		reason_code: rawPayload.reason_code as ScheduleAccountDeletionRequest['reason_code'],
		days_until_deletion: rawPayload.days_until_deletion as number,
		public_reason: (rawPayload.public_reason as string | null) ?? null,
		admin_user_id: rawPayload.admin_user_id as string,
		audit_log_reason: (rawPayload.audit_log_reason as string | null) ?? null,
	};
	const {auditService, userService} = createAdminBulkServices(getWorkerDependencies());
	const adminUserId = createUserID(BigInt(payload.admin_user_id));
	const total = payload.user_ids.length;
	const daysUntilDeletion = resolveDeletionDays(payload.reason_code, payload.days_until_deletion);
	await helpers.setContextLink(`/users?ids=${payload.user_ids.slice(0, 50).join(',')}`);
	await helpers.reportProgress(0, total, `Scheduling deletion of ${total} users in ${daysUntilDeletion} days`);
	return await runAdminBulkJob({
		helpers,
		ids: payload.user_ids,
		progressEvery: 10,
		apply: async (userId) => {
			await userService.deletionService.applyScheduledDeletion(
				{
					user_id: BigInt(userId),
					reason_code: payload.reason_code,
					public_reason: payload.public_reason ?? undefined,
					days_until_deletion: payload.days_until_deletion,
				},
				adminUserId,
				payload.audit_log_reason,
			);
		},
		summary: {
			auditService,
			adminUserId,
			action: 'bulk_schedule_deletion',
			auditLogReason: payload.audit_log_reason,
			metadata: [
				['user_count', total.toString()],
				['reason_code', payload.reason_code.toString()],
				['days', daysUntilDeletion.toString()],
			],
		},
	});
};

export default handler;
