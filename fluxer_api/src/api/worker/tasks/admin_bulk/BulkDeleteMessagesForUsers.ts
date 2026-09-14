// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminAuditService} from '@app/api/admin/services/AdminAuditService';
import {createUserID} from '@app/api/BrandedTypes';
import {UserMessageDeletionService} from '@app/api/channel/services/message/UserMessageDeletionService';
import {runAdminBulkJob} from '@app/api/worker/tasks/admin_bulk/AdminBulkJob';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';

interface Payload {
	user_ids: Array<string>;
	admin_user_id: string;
	audit_log_reason: string | null;
}

const handler: WorkerTaskHandler = async (rawPayload, helpers) => {
	const payload: Payload = {
		user_ids: rawPayload.user_ids as Array<string>,
		admin_user_id: rawPayload.admin_user_id as string,
		audit_log_reason: (rawPayload.audit_log_reason as string | null) ?? null,
	};
	const deps = getWorkerDependencies();
	const auditService = new AdminAuditService(deps.adminRepository, deps.snowflakeService);
	const deletionService = new UserMessageDeletionService({
		channelRepository: deps.channelRepository,
		gatewayService: deps.gatewayService,
		storageService: deps.storageService,
		purgeQueue: deps.purgeQueue,
	});
	const adminUserId = createUserID(BigInt(payload.admin_user_id));
	const total = payload.user_ids.length;
	let deletedMessages = 0;
	await helpers.setContextLink(`/users?ids=${payload.user_ids.slice(0, 50).join(',')}`);
	await helpers.reportProgress(0, total, `Deleting all messages from ${total} users`);
	return runAdminBulkJob({
		helpers,
		ids: payload.user_ids,
		progressEvery: 1,
		apply: async (rawUserId) => {
			const userId = createUserID(BigInt(rawUserId));
			const deleted = await deletionService.deleteUserMessagesBulk(userId);
			deletedMessages += deleted;
			await auditService.createAuditLog({
				adminUserId,
				targetType: 'message_deletion',
				targetId: BigInt(userId),
				action: 'delete_all_user_messages',
				auditLogReason: payload.audit_log_reason,
				metadata: new Map([
					['user_id', userId.toString()],
					['message_count', deleted.toString()],
				]),
			});
		},
		afterItems: async () => [['message_count', deletedMessages.toString()]] as Array<[string, string]>,
		summary: {
			auditService,
			adminUserId,
			action: 'bulk_delete_user_messages',
			auditLogReason: payload.audit_log_reason,
			metadata: [['user_count', total.toString()]],
		},
	});
};

export default handler;
