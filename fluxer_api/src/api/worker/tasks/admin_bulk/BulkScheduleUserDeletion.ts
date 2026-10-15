// SPDX-License-Identifier: AGPL-3.0-or-later

import {resolveDeletionDays} from '@app/api/admin/services/AdminUserDeletionService';
import {createUserID} from '@app/api/BrandedTypes';
import {runAdminBulkJob} from '@app/api/worker/tasks/admin_bulk/AdminBulkJob';
import {createAdminBulkServices} from '@app/api/worker/tasks/admin_bulk/AdminBulkServices';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import {BulkModerationBasis} from '@fluxer/schema/src/domains/admin/EnforcementSchemas';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {z} from 'zod';

const BulkScheduleUserDeletionPayload = z.object({
	user_ids: z.array(z.string()),
	days_until_deletion: z.number().int(),
	basis: BulkModerationBasis,
	admin_user_id: z.string(),
	audit_log_reason: z.string().nullable(),
});

const handler: WorkerTaskHandler = async (rawPayload, helpers) => {
	const payload = BulkScheduleUserDeletionPayload.parse(rawPayload);
	const {auditService, userService} = createAdminBulkServices(getWorkerDependencies());
	const adminUserId = createUserID(BigInt(payload.admin_user_id));
	const actor = {adminUserId, acls: new Set<string>(), internalNote: payload.audit_log_reason};
	const total = payload.user_ids.length;
	const daysUntilDeletion = resolveDeletionDays(payload.basis.type, payload.days_until_deletion);
	await helpers.setContextLink(`/users?ids=${payload.user_ids.slice(0, 50).join(',')}`);
	await helpers.reportProgress(0, total, `Scheduling deletion of ${total} users in ${daysUntilDeletion} days`);
	return await runAdminBulkJob({
		helpers,
		ids: payload.user_ids,
		progressEvery: 10,
		apply: async (userId) => {
			await userService.deletionService.applyScheduledDeletion(
				createUserID(BigInt(userId)),
				{days_until_deletion: payload.days_until_deletion, basis: payload.basis},
				actor,
			);
		},
		summary: {
			auditService,
			adminUserId,
			action: 'bulk_schedule_deletion',
			auditLogReason: payload.audit_log_reason,
			metadata: [
				['user_count', total.toString()],
				['basis', payload.basis.type],
				...(payload.basis.type === 'enforcement'
					? ([['reason_code', payload.basis.decision.reason_code.toString()]] as Array<[string, string]>)
					: []),
				['days', daysUntilDeletion.toString()],
			],
		},
	});
};

export default handler;
