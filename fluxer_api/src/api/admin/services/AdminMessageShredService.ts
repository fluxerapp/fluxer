// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import type {ApiContext} from '@app/api/ApiContext';
import type {AdminAuditService} from '@app/api/admin/services/AdminAuditService';
import {createChannelID, createMessageID, type UserID} from '@app/api/BrandedTypes';
import type {ChannelRepository} from '@app/api/channel/ChannelRepository';
import type {EnforcementDecisionRow} from '@app/api/database/types/EnforcementTypes';
import type {EnforcementActor} from '@app/api/enforcement/EnforcementActor';
import {decisionAuditMetadata, type EnforcementDecisionService} from '@app/api/enforcement/EnforcementDecisionService';
import {Logger} from '@app/api/Logger';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import type {
	AdminUserMessageShredPreviewRequest,
	AdminUserMessageShredRequest,
	MessageShredPreviewResponse,
	MessageShredResponse,
} from '@fluxer/schema/src/domains/admin/AdminMessageSchemas';
import {snowflakeToDate} from '@fluxer/snowflake/src/Snowflake';
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

type ShredEntry = {channel_id: bigint; message_id: bigint};

const PREVIEW_CONTENT_LENGTH = 200;

interface ShredPlan {
	deletable: Array<ShredEntry>;
	preview: MessageShredPreviewResponse;
}

interface AdminMessageShredServiceDeps {
	apiContext: ApiContext;
	channelRepository: ChannelRepository;
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

function previewToken(userId: UserID, deletable: ReadonlyArray<ShredEntry>): string {
	const hash = createHash('sha256').update(userId.toString());
	for (const entry of deletable) {
		hash.update(`\n${entry.channel_id}:${entry.message_id}`);
	}
	return hash.digest('hex');
}

export class AdminMessageShredService {
	constructor(private readonly deps: AdminMessageShredServiceDeps) {}

	private async plan(userId: UserID, entries: ReadonlyArray<ShredEntry>): Promise<ShredPlan> {
		const seen = new Set<string>();
		const deletable: Array<ShredEntry> = [];
		const messages: MessageShredPreviewResponse['messages'] = [];
		const skipped: MessageShredPreviewResponse['skipped'] = [];
		for (const entry of entries) {
			const ids = {channel_id: entry.channel_id.toString(), message_id: entry.message_id.toString()};
			const key = `${ids.channel_id}:${ids.message_id}`;
			if (seen.has(key)) {
				skipped.push({...ids, reason: 'duplicate'});
				continue;
			}
			seen.add(key);
			const message = await this.deps.channelRepository.messages.getMessage(
				createChannelID(entry.channel_id),
				createMessageID(entry.message_id),
			);
			if (!message || message.channelId !== entry.channel_id || message.id !== entry.message_id) {
				skipped.push({...ids, reason: 'not_found'});
				continue;
			}
			if (message.authorId !== userId) {
				skipped.push({...ids, reason: 'other_author'});
				continue;
			}
			deletable.push({channel_id: entry.channel_id, message_id: entry.message_id});
			messages.push({
				...ids,
				timestamp: snowflakeToDate(entry.message_id).toISOString(),
				content: [...(message.content ?? '')].slice(0, PREVIEW_CONTENT_LENGTH).join(''),
				attachment_count: message.attachments?.length ?? 0,
			});
		}
		return {
			deletable,
			preview: {
				requested: entries.length,
				message_count: deletable.length,
				messages,
				skipped,
				preview_token: previewToken(userId, deletable),
			},
		};
	}

	async previewMessageShred(
		userId: UserID,
		{entries}: AdminUserMessageShredPreviewRequest,
	): Promise<MessageShredPreviewResponse> {
		return (await this.plan(userId, entries)).preview;
	}

	async queueMessageShred(
		userId: UserID,
		{entries: requested, basis, preview_token}: AdminUserMessageShredRequest,
		actor: EnforcementActor,
	): Promise<MessageShredResponse> {
		const {apiContext, auditService, enforcement} = this.deps;
		let entries: ReadonlyArray<ShredEntry> = requested;
		if (preview_token !== undefined) {
			const {deletable, preview} = await this.plan(userId, requested);
			if (preview.preview_token !== preview_token || deletable.length === 0) {
				throw InputValidationError.fromCode('preview_token', ValidationErrorCodes.MESSAGE_SHRED_PREVIEW_CHANGED);
			}
			entries = deletable;
		}
		let jobId = '';
		const queue = async () => {
			jobId = await queueMessageShredJob(apiContext, actor.adminUserId, userId, entries);
		};
		let decision: EnforcementDecisionRow | null = null;
		if (basis.type === 'enforcement') {
			const user = await apiContext.services.users.findUniqueAssert(userId);
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
				['requested_entries', requested.length.toString()],
				...(preview_token === undefined ? [] : [['previewed_messages', entries.length.toString()] as [string, string]]),
				['basis', basis.type],
				...(decision ? decisionAuditMetadata(decision) : []),
			]),
		});
		return {
			success: true,
			job_id: jobId,
			requested: requested.length,
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
