// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import type {AdminAuditService} from '@app/api/admin/services/AdminAuditService';
import type {UserID} from '@app/api/BrandedTypes';
import type {EnforcementDecisionRow} from '@app/api/database/types/EnforcementTypes';
import type {EnforcementActor} from '@app/api/enforcement/EnforcementActor';
import {decisionAuditMetadata, type EnforcementDecisionService} from '@app/api/enforcement/EnforcementDecisionService';
import {Logger} from '@app/api/Logger';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import type {
	AdminUserMessageShredRequest,
	MessageShredResponse,
} from '@fluxer/schema/src/domains/admin/AdminMessageSchemas';
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
	enforcement: EnforcementDecisionService;
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

export async function queueMessageShredJob(
	apiContext: ApiContext,
	adminUserId: UserID,
	targetUserId: bigint,
	entries: ReadonlyArray<{channel_id: bigint; message_id: bigint}>,
): Promise<string> {
	const {snowflake: snowflakeService, worker: workerService} = apiContext.services;
	const jobId = (await snowflakeService.generate()).toString();
	const payload: QueueMessageShredJobPayload = {
		job_id: jobId,
		admin_user_id: adminUserId.toString(),
		target_user_id: targetUserId.toString(),
		entries: entries.map((entry) => ({
			channel_id: entry.channel_id.toString(),
			message_id: entry.message_id.toString(),
		})),
	};
	await workerService.addJob('messageShred', payload, {
		jobKey: `message_shred_${targetUserId.toString()}_${jobId}`,
		maxAttempts: 1,
	});
	Logger.debug({target_user_id: targetUserId, job_id: jobId}, 'Queued message shred job');
	return jobId;
}

export class AdminMessageShredService {
	constructor(private readonly deps: AdminMessageShredServiceDeps) {}

	async queueMessageShred(
		userId: UserID,
		{entries, basis}: AdminUserMessageShredRequest,
		actor: EnforcementActor,
	): Promise<MessageShredResponse> {
		const {apiContext, auditService, enforcement} = this.deps;
		let jobId = '';
		const queue = async () => {
			jobId = await queueMessageShredJob(apiContext, actor.adminUserId, userId, entries);
		};
		let decision: EnforcementDecisionRow | null = null;
		if (basis.type === 'enforcement') {
			const user = await apiContext.services.users.findUnique(userId);
			if (!user) {
				throw new UnknownUserError();
			}
			({decision} = await enforcement.record(
				{
					subject: {type: 'user', user},
					kind: 'content_removal',
					input: basis.decision,
					removal: {type: 'messages', entries},
					actor,
				},
				queue,
			));
		} else {
			await queue();
		}
		await auditService.createAuditLog({
			adminUserId: actor.adminUserId,
			targetType: 'message_shred',
			targetId: userId,
			action: 'queue_message_shred',
			auditLogReason: actor.internalNote,
			metadata: new Map([
				['user_id', userId.toString()],
				['job_id', jobId],
				['requested_entries', entries.length.toString()],
				['basis', basis.type],
				...(decision ? decisionAuditMetadata(decision) : []),
			]),
		});
		return {
			success: true,
			job_id: jobId,
			requested: entries.length,
			...(decision ? {decision_id: decision.decision_id.toString()} : {}),
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
