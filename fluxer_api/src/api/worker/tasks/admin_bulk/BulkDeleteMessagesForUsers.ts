// SPDX-License-Identifier: AGPL-3.0-or-later

import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {JobCancelledError} from '@pkgs/worker/src/contracts/WorkerTask';
import {AdminAuditService} from '../../../admin/services/AdminAuditService';
import {createUserID} from '../../../BrandedTypes';
import {UserMessageDeletionService} from '../../../channel/services/message/UserMessageDeletionService';
import {getWorkerDependencies} from '../../WorkerContext';

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
	const successful: Array<string> = [];
	const failed: Array<{
		id: string;
		error: string;
	}> = [];
	let deletedMessages = 0;
	await helpers.setContextLink(`/users?ids=${payload.user_ids.slice(0, 50).join(',')}`);
	await helpers.reportProgress(0, total, `Deleting all messages from ${total} users`);
	for (let i = 0; i < payload.user_ids.length; i++) {
		if (await helpers.shouldCancel()) throw new JobCancelledError();
		const rawUserId = payload.user_ids[i]!;
		try {
			const userId = createUserID(BigInt(rawUserId));
			const deleted = await deletionService.deleteUserMessagesBulk(userId);
			deletedMessages += deleted;
			await auditService.createAuditLog({
				adminUserId,
				targetType: 'message_deletion',
				targetId: BigInt(userId),
				action: 'delete_all_user_messages',
				auditLogReason: null,
				metadata: new Map([['message_count', deleted.toString()]]),
			});
			successful.push(rawUserId);
		} catch (err) {
			failed.push({id: rawUserId, error: err instanceof Error ? err.message : String(err)});
		}
		await helpers.reportProgress(i + 1, total, `${deletedMessages} messages deleted`);
	}
	await auditService.createAuditLog({
		adminUserId,
		targetType: 'message_deletion',
		targetId: BigInt(0),
		action: 'bulk_delete_user_messages',
		auditLogReason: payload.audit_log_reason,
		metadata: new Map([
			['user_count', total.toString()],
			['message_count', deletedMessages.toString()],
			['successful', successful.length.toString()],
			['failed', failed.length.toString()],
		]),
	});
	await helpers.reportProgress(total, total, `${deletedMessages} messages deleted, ${failed.length} users failed`);
	helpers.logger.info(
		{successful: successful.length, failed: failed.length, deletedMessages},
		'bulkDeleteMessagesForUsers complete',
	);
};

export default handler;
