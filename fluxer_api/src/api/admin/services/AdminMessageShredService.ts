// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import type {AdminAuditService} from '@app/api/admin/services/AdminAuditService';
import type {UserID} from '@app/api/BrandedTypes';
import {Logger} from '@app/api/Logger';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import type {MessageShredRequest} from '@fluxer/schema/src/domains/admin/AdminMessageSchemas';
import type {WorkerJobPayload} from '@pkgs/worker/src/contracts/WorkerTypes';

type MessageShredStatusCacheEntry = {
	status: 'in_progress' | 'completed' | 'failed';
	requested: number;
	total: number;
	processed: number;
	skipped: number;
	started_at?: string;
	completed_at?: string;
	failed_at?: string;
	error?: string;
};
type MessageShredStatusResult =
	| MessageShredStatusCacheEntry
	| {
			status: 'not_found';
	  };

interface AdminMessageShredServiceDeps {
	apiContext: ApiContext;
	auditService: AdminAuditService;
}

interface QueueMessageShredJobPayload extends WorkerJobPayload {
	job_id: string;
	admin_user_id: string;
	target_user_id: string;
	entries: Array<{
		channel_id: string;
		message_id: string;
	}>;
}

export class AdminMessageShredService {
	constructor(private readonly deps: AdminMessageShredServiceDeps) {}

	async queueMessageShred(
		data: MessageShredRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
	): Promise<{
		success: true;
		job_id: string;
		requested: number;
	}> {
		if (data.entries.length === 0) {
			throw InputValidationError.fromCode('entries', ValidationErrorCodes.AT_LEAST_ONE_ENTRY_IS_REQUIRED);
		}
		const {snowflake: snowflakeService, worker: workerService} = this.deps.apiContext.services;
		const jobId = (await snowflakeService.generate()).toString();
		const payload: QueueMessageShredJobPayload = {
			job_id: jobId,
			admin_user_id: adminUserId.toString(),
			target_user_id: data.user_id.toString(),
			entries: data.entries.map((entry) => ({
				channel_id: entry.channel_id.toString(),
				message_id: entry.message_id.toString(),
			})),
		};
		await workerService.addJob('messageShred', payload, {
			jobKey: `message_shred_${data.user_id.toString()}_${jobId}`,
			maxAttempts: 1,
		});
		Logger.debug({target_user_id: data.user_id, job_id: jobId}, 'Queued message shred job');
		const metadata = new Map<string, string>([
			['user_id', data.user_id.toString()],
			['job_id', jobId],
			['requested_entries', data.entries.length.toString()],
		]);
		await this.deps.auditService.createAuditLog({
			adminUserId,
			targetType: 'message_shred',
			targetId: data.user_id,
			action: 'queue_message_shred',
			auditLogReason,
			metadata,
		});
		return {
			success: true,
			job_id: jobId,
			requested: data.entries.length,
		};
	}

	async getMessageShredStatus(jobId: string): Promise<MessageShredStatusResult> {
		const {cache: cacheService} = this.deps.apiContext.services;
		const statusKey = `message_shred_status:${jobId}`;
		const status = await cacheService.get<MessageShredStatusCacheEntry>(statusKey);
		if (!status) {
			return {
				status: 'not_found',
			};
		}
		return status;
	}
}
