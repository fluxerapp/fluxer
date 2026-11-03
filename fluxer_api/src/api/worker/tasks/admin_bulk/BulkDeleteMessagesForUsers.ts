// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminMessageDeletionService} from '@app/api/admin/services/AdminMessageDeletionService';
import {createUserID} from '@app/api/BrandedTypes';
import {runAdminBulkJob} from '@app/api/worker/tasks/admin_bulk/AdminBulkJob';
import {createWorkerEnforcementServices} from '@app/api/worker/tasks/admin_bulk/AdminBulkServices';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import {BulkModerationBasis} from '@fluxer/schema/src/domains/admin/EnforcementSchemas';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {z} from 'zod';

const BulkDeleteMessagesForUsersPayload = z.object({
	user_ids: z.array(z.string()),
	basis: BulkModerationBasis,
	admin_user_id: z.string(),
	audit_log_reason: z.string().nullable(),
});

const handler: WorkerTaskHandler = async (rawPayload, helpers) => {
	const payload = BulkDeleteMessagesForUsersPayload.parse(rawPayload);
	const deps = getWorkerDependencies();
	const {apiContext, auditService, enforcementService} = createWorkerEnforcementServices(deps);
	const deletionService = new AdminMessageDeletionService({
		apiContext,
		channelRepository: deps.channelRepository,
		auditService,
		enforcement: enforcementService,
	});
	const adminUserId = createUserID(BigInt(payload.admin_user_id));
	const actor = {adminUserId, acls: new Set<string>(), internalNote: payload.audit_log_reason};
	const total = payload.user_ids.length;
	let deletedMessages = 0;
	await helpers.setContextLink(`/users?ids=${payload.user_ids.slice(0, 50).join(',')}`);
	await helpers.reportProgress(0, total, `Deleting all messages from ${total} users`);
	return runAdminBulkJob({
		helpers,
		ids: payload.user_ids,
		progressEvery: 1,
		apply: async (rawUserId) => {
			const {message_count} = await deletionService.deleteAllUserMessages(
				createUserID(BigInt(rawUserId)),
				{dryRun: false, basis: payload.basis},
				actor,
			);
			deletedMessages += message_count;
		},
		afterItems: async () => [['message_count', deletedMessages.toString()]] as Array<[string, string]>,
		summary: {
			auditService,
			adminUserId,
			action: 'bulk_delete_user_messages',
			auditLogReason: payload.audit_log_reason,
			metadata: [
				['user_count', total.toString()],
				['basis', payload.basis.type],
				...(payload.basis.type === 'enforcement'
					? ([['reason_code', payload.basis.decision.reason_code.toString()]] as Array<[string, string]>)
					: []),
			],
		},
	});
};

export default handler;
