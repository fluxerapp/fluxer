// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import type {AdminAuditService} from '@app/api/admin/services/AdminAuditService';
import {queueMessageShredJob} from '@app/api/admin/services/AdminMessageShredService';
import type {ChannelID, MessageID, UserID} from '@app/api/BrandedTypes';
import type {ChannelRepository} from '@app/api/channel/ChannelRepository';
import type {EnforcementDecisionRow} from '@app/api/database/types/EnforcementTypes';
import type {EnforcementActor} from '@app/api/enforcement/EnforcementActor';
import {decisionAuditMetadata, type EnforcementDecisionService} from '@app/api/enforcement/EnforcementDecisionService';
import type {User} from '@app/api/models/User';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import type {DeleteAllUserMessagesResponse} from '@fluxer/schema/src/domains/admin/AdminMessageSchemas';
import type {ModerationBasis} from '@fluxer/schema/src/domains/admin/EnforcementSchemas';

interface AdminMessageDeletionServiceDeps {
	apiContext: ApiContext;
	channelRepository: ChannelRepository;
	auditService: AdminAuditService;
	enforcement: EnforcementDecisionService;
}

interface DeleteAllUserMessages {
	dryRun: boolean;
	basis: ModerationBasis | undefined;
}

const FETCH_CHUNK_SIZE = 200;

export class AdminMessageDeletionService {
	constructor(private readonly deps: AdminMessageDeletionServiceDeps) {}

	async deleteAllUserMessages(
		userId: UserID,
		{dryRun, basis}: DeleteAllUserMessages,
		actor: EnforcementActor,
	): Promise<DeleteAllUserMessagesResponse> {
		if (!dryRun && !basis) {
			throw InputValidationError.fromCode('basis', ValidationErrorCodes.MODERATION_BASIS_REQUIRED);
		}
		const {entries, channelCount, messageCount} = await this.collectMessageRefs(userId, !dryRun);
		const response: DeleteAllUserMessagesResponse = {
			success: true,
			dry_run: dryRun,
			channel_count: channelCount,
			message_count: messageCount,
		};
		let decision: EnforcementDecisionRow | null = null;
		if (basis && !dryRun && messageCount > 0) {
			const queue = async () => {
				response.job_id = await queueMessageShredJob(this.deps.apiContext, actor.adminUserId, userId, entries);
			};
			if (basis.type === 'enforcement') {
				({decision} = await this.deps.enforcement.record(
					{
						subject: {type: 'user', user: await this.requireUser(userId)},
						kind: 'content_removal',
						input: basis.decision,
						removal: {type: 'all_messages'},
						actor,
					},
					queue,
				));
				response.decision_id = decision.decision_id.toString();
			} else {
				await queue();
			}
		}
		await this.deps.auditService.createAuditLog({
			adminUserId: actor.adminUserId,
			targetType: 'message_deletion',
			targetId: userId,
			action: dryRun ? 'delete_all_user_messages_dry_run' : 'delete_all_user_messages',
			auditLogReason: actor.internalNote,
			metadata: new Map([
				['user_id', userId.toString()],
				['channel_count', channelCount.toString()],
				['message_count', messageCount.toString()],
				['dry_run', dryRun ? 'true' : 'false'],
				...(basis && !dryRun ? ([['basis', basis.type]] as Array<[string, string]>) : []),
				...(response.job_id ? ([['job_id', response.job_id]] as Array<[string, string]>) : []),
				...(decision ? decisionAuditMetadata(decision) : []),
			]),
		});
		return response;
	}

	private async requireUser(userId: UserID): Promise<User> {
		const user = await this.deps.apiContext.services.users.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		return user;
	}

	private async collectMessageRefs(authorId: UserID, includeEntries: boolean) {
		let lastMessageId: MessageID | undefined;
		const entries: Array<{
			channel_id: ChannelID;
			message_id: MessageID;
		}> = [];
		let messageCount = 0;
		let channelCount = 0;
		while (true) {
			const messageRefs = await this.deps.channelRepository.messages.listMessagesByAuthor(
				authorId,
				FETCH_CHUNK_SIZE,
				lastMessageId,
			);
			if (messageRefs.length === 0) {
				break;
			}
			const channelsInChunk = new Set<string>();
			for (const {channelId, messageId} of messageRefs) {
				channelsInChunk.add(channelId.toString());
				messageCount += 1;
				if (includeEntries) {
					entries.push({
						channel_id: channelId,
						message_id: messageId,
					});
				}
			}
			channelCount += channelsInChunk.size;
			lastMessageId = messageRefs[messageRefs.length - 1].messageId;
			if (messageRefs.length < FETCH_CHUNK_SIZE) {
				break;
			}
		}
		return {
			entries,
			channelCount,
			messageCount,
		};
	}
}
