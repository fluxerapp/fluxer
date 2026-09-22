// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID} from '@app/api/BrandedTypes';
import {runAdminBulkJob} from '@app/api/worker/tasks/admin_bulk/AdminBulkJob';
import {createAdminBulkServices} from '@app/api/worker/tasks/admin_bulk/AdminBulkServices';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';

const SHA256_RE = /^[0-9a-fA-F]{64}$/;

interface Payload {
	sha256_list: Array<string>;
	admin_user_id: string;
	audit_log_reason: string | null;
}

const handler: WorkerTaskHandler = async (rawPayload, helpers) => {
	const payload: Payload = {
		sha256_list: rawPayload.sha256_list as Array<string>,
		admin_user_id: rawPayload.admin_user_id as string,
		audit_log_reason: (rawPayload.audit_log_reason as string | null) ?? null,
	};
	const {auditService, banManagementService} = createAdminBulkServices(getWorkerDependencies());
	const adminUserId = createUserID(BigInt(payload.admin_user_id));
	const shas = payload.sha256_list.map((sha) => sha.toLowerCase());
	await helpers.setContextLink('/file-sha-bans');
	await helpers.reportProgress(0, shas.length, `Banning ${shas.length} file SHAs`);
	return runAdminBulkJob({
		helpers,
		ids: shas,
		progressEvery: 50,
		apply: async (sha) => {
			if (!SHA256_RE.test(sha)) {
				throw new Error('invalid_sha256');
			}
			await banManagementService.banFileSha({sha256_hex: sha}, adminUserId, payload.audit_log_reason, {
				deferRefresh: true,
			});
		},
		afterItems: async () => {
			await banManagementService.publishFileShaRefresh();
			return [];
		},
		summary: {
			auditService,
			adminUserId,
			action: 'bulk_ban_file_shas',
			auditLogReason: payload.audit_log_reason,
			metadata: [['count', shas.length.toString()]],
		},
	});
};

export default handler;
