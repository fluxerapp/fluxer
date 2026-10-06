// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AdminAuditService} from '@app/api/admin/services/AdminAuditService';
import type {AdminMessageShredService} from '@app/api/admin/services/AdminMessageShredService';
import type {ChannelID, MessageID, UserID} from '@app/api/BrandedTypes';
import {createUserID} from '@app/api/BrandedTypes';
import type {IChannelRepository} from '@app/api/channel/IChannelRepository';
import {Logger} from '@app/api/Logger';
import type {
	DeleteAllUserMessagesRequest,
	DeleteAllUserMessagesResponse,
} from '@fluxer/schema/src/domains/admin/AdminMessageSchemas';

interface AdminMessageDeletionServiceDeps {
	channelRepository: IChannelRepository;
	messageShredService: AdminMessageShredService;
	auditService: AdminAuditService;
}

const FETCH_CHUNK_SIZE = 200;

export class AdminMessageDeletionService {
	constructor(private readonly deps: AdminMessageDeletionServiceDeps) {}

	async deleteAllUserMessages(
		data: DeleteAllUserMessagesRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
	): Promise<DeleteAllUserMessagesResponse> {
		const authorId = createUserID(data.user_id);
		const {entries, channelCount, messageCount} = await this.collectMessageRefs(authorId, !data.dry_run);
		const metadata = new Map<string, string>([
			['user_id', data.user_id.toString()],
			['channel_count', channelCount.toString()],
			['message_count', messageCount.toString()],
			['dry_run', data.dry_run ? 'true' : 'false'],
		]);
		const action = data.dry_run ? 'delete_all_user_messages_dry_run' : 'delete_all_user_messages';
		await this.deps.auditService.createAuditLog({
			adminUserId,
			targetType: 'message_deletion',
			targetId: data.user_id,
			action,
			auditLogReason,
			metadata,
		});
		Logger.debug(
			{user_id: data.user_id, channel_count: channelCount, message_count: messageCount, dry_run: data.dry_run},
			'Computed delete-all-messages stats',
		);
		const response: DeleteAllUserMessagesResponse = {
			success: true,
			dry_run: data.dry_run,
			channel_count: channelCount,
			message_count: messageCount,
		};
		if (data.dry_run || messageCount === 0) {
			return response;
		}
		const shredResult = await this.deps.messageShredService.queueMessageShred(
			{
				user_id: data.user_id,
				entries,
			},
			adminUserId,
			auditLogReason,
		);
		response.job_id = shredResult.job_id;
		return response;
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
			const messageRefs = await this.deps.channelRepository.listMessagesByAuthor(
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
