/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {
	type AttachmentID,
	type ChannelID,
	createChannelID,
	createMessageID,
	createUserID,
	type MessageID,
	type UserID,
} from '~/BrandedTypes';
import {type MessageResponse, mapMessageToResponse} from '~/channel/ChannelModel';
import type {IChannelRepository} from '~/channel/IChannelRepository';
import type {IGatewayService} from '~/infrastructure/IGatewayService';
import type {IMediaService} from '~/infrastructure/IMediaService';
import type {UserCacheService} from '~/infrastructure/UserCacheService';
import type {RequestCache} from '~/middleware/RequestCacheMiddleware';
import type {DeleteMessageRequest, LookupMessageByAttachmentRequest, LookupMessageRequest} from '../AdminModel';
import type {AdminAuditService} from './AdminAuditService';

interface AdminMessageServiceDeps {
	channelRepository: IChannelRepository;
	userCacheService: UserCacheService;
	mediaService: IMediaService;
	gatewayService: IGatewayService;
	auditService: AdminAuditService;
}

export class AdminMessageService {
	constructor(private readonly deps: AdminMessageServiceDeps) {}

	async lookupAttachment({
		channelId,
		attachmentId,
		filename,
	}: {
		channelId: ChannelID;
		attachmentId: AttachmentID;
		filename: string;
	}): Promise<{message_id: MessageID | null}> {
		const {channelRepository} = this.deps;
		const messageId = await channelRepository.lookupAttachmentByChannelAndFilename(channelId, attachmentId, filename);
		return {
			message_id: messageId,
		};
	}

	async lookupMessage(data: LookupMessageRequest) {
		const {channelRepository, userCacheService, mediaService} = this.deps;
		const channelId = createChannelID(data.channel_id);
		const messageId = createMessageID(data.message_id);
		const contextPerSide = Math.floor(data.context_limit / 2);

		const [targetMessage, messagesBefore, messagesAfter] = await Promise.all([
			channelRepository.getMessage(channelId, messageId),
			channelRepository.listMessages(channelId, messageId, contextPerSide),
			channelRepository.listMessages(channelId, undefined, contextPerSide, messageId),
		]);

		const allMessages = [...messagesBefore.reverse(), ...(targetMessage ? [targetMessage] : []), ...messagesAfter];

		const requestCache: RequestCache = {
			userPartials: new Map(),
			clear: () => {},
		};

		const messageResponses = await Promise.all(
			allMessages.map((message) =>
				mapMessageToResponse({
					message,
					currentUserId: undefined,
					userCacheService,
					requestCache,
					mediaService,
				}),
			),
		);

		return {
			messages: messageResponses.map((message) => this.mapMessageResponseToAdminMessage(message)),
			message_id: messageId.toString(),
		};
	}

	async lookupMessageByAttachment(data: LookupMessageByAttachmentRequest) {
		const channelId = createChannelID(data.channel_id);
		const attachmentId = data.attachment_id as AttachmentID;

		const messageId = await this.deps.channelRepository.lookupAttachmentByChannelAndFilename(
			channelId,
			attachmentId,
			data.filename,
		);

		if (!messageId) {
			return {
				messages: [],
				message_id: null,
			};
		}

		const result = await this.lookupMessage({
			channel_id: data.channel_id,
			message_id: BigInt(messageId),
			context_limit: data.context_limit,
		});

		return {
			messages: result.messages,
			message_id: messageId.toString(),
		};
	}

	async deleteMessage(data: DeleteMessageRequest, adminUserId: UserID, auditLogReason: string | null) {
		const {channelRepository, gatewayService, auditService} = this.deps;
		const channelId = createChannelID(data.channel_id);
		const messageId = createMessageID(data.message_id);

		const channel = await channelRepository.findUnique(channelId);
		const message = await channelRepository.getMessage(channelId, messageId);

		if (message) {
			await channelRepository.deleteMessage(
				channelId,
				messageId,
				message.authorId || createUserID(0n),
				message.pinnedTimestamp || undefined,
			);

			if (channel) {
				if (channel.guildId) {
					await gatewayService.dispatchGuild({
						guildId: channel.guildId,
						event: 'MESSAGE_DELETE',
						data: {
							channel_id: channelId.toString(),
							id: messageId.toString(),
						},
					});
				} else {
					for (const recipientId of channel.recipientIds) {
						await gatewayService.dispatchPresence({
							userId: recipientId,
							event: 'MESSAGE_DELETE',
							data: {
								channel_id: channelId.toString(),
								id: messageId.toString(),
							},
						});
					}
				}
			}
		}

		await auditService.createAuditLog({
			adminUserId,
			targetType: 'message',
			targetId: BigInt(messageId),
			action: 'delete_message',
			auditLogReason,
			metadata: new Map([
				['channel_id', channelId.toString()],
				['message_id', messageId.toString()],
			]),
		});

		return {
			success: true,
		};
	}

	private mapMessageResponseToAdminMessage(message: MessageResponse) {
		return {
			id: message.id,
			channel_id: message.channel_id ?? '',
			author_id: message.author.id,
			author_username: message.author.username,
			content: message.content ?? '',
			timestamp: message.timestamp,
			attachments:
				message.attachments?.map((attachment) => ({
					filename: attachment.filename,
					url: attachment.url,
				})) ?? [],
		};
	}
}
