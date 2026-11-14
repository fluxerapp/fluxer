// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID} from '@app/api/BrandedTypes';
import {runAdminBulkJob} from '@app/api/worker/tasks/admin_bulk/AdminBulkJob';
import {createAdminBulkServices} from '@app/api/worker/tasks/admin_bulk/AdminBulkServices';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';

interface BulkUpdateUserFlagsPayload {
	user_ids: Array<string>;
	add_flags: Array<string>;
	remove_flags: Array<string>;
	admin_user_id: string;
	audit_log_reason: string | null;
}

const PROGRESS_EVERY = 25;

const handler: WorkerTaskHandler = async (rawPayload, helpers) => {
	const payload: BulkUpdateUserFlagsPayload = {
		user_ids: rawPayload.user_ids as Array<string>,
		add_flags: rawPayload.add_flags as Array<string>,
		remove_flags: rawPayload.remove_flags as Array<string>,
		admin_user_id: rawPayload.admin_user_id as string,
		audit_log_reason: (rawPayload.audit_log_reason as string | null) ?? null,
	};
	const deps = getWorkerDependencies();
	const {auditService, userService} = createAdminBulkServices(deps);
	const adminUserId = createUserID(BigInt(payload.admin_user_id));
	const addFlags = payload.add_flags.map((flag) => BigInt(flag));
	const removeFlags = payload.remove_flags.map((flag) => BigInt(flag));
	const acls = new Set<string>();
	const total = payload.user_ids.length;
	await helpers.setContextLink(`/users?ids=${payload.user_ids.slice(0, 50).join(',')}`);
	await helpers.reportProgress(0, total, `Updating flags on ${total} users`);
	return await runAdminBulkJob({
		helpers,
		ids: payload.user_ids,
		progressEvery: PROGRESS_EVERY,
		apply: async (id) => {
			await userService.securityService.updateUserFlags({
				userId: createUserID(BigInt(id)),
				data: {addFlags, removeFlags},
				adminUserId,
				auditLogReason: payload.audit_log_reason,
				acls,
			});
		},
		summary: {
			auditService,
			adminUserId,
			action: 'bulk_update_user_flags',
			auditLogReason: payload.audit_log_reason,
			metadata: (
				[
					['user_count', total.toString()],
					['add_flags', addFlags.map((flag) => flag.toString()).join(',')],
					['remove_flags', removeFlags.map((flag) => flag.toString()).join(',')],
				] as Array<[string, string]>
			).filter(([, value]) => value.length > 0),
		},
	});
};

export default handler;
