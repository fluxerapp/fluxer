// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import type {AdminAuditService} from '@app/api/admin/services/AdminAuditService';
import {
	type ChannelID,
	createAttachmentID,
	createChannelID,
	createMessageID,
	createUserID,
	type MessageID,
	type UserID,
} from '@app/api/BrandedTypes';
import type {IChannelRepository} from '@app/api/channel/IChannelRepository';
import {withThreadContext} from '@app/api/channel/services/ChannelGatewayDispatch';
import {
	enqueueCrosspostFamilyPurgeFromCopies,
	enqueueCrosspostSourceRemoval,
} from '@app/api/channel/services/message/CrosspostPropagation';
import {decrementThreadMessageCount, purgeMessageAttachments} from '@app/api/channel/services/message/MessageHelpers';
import {
	createMessageResponseDataService,
	type MessageResponseAccessContext,
	messageResponseAccessForChannel,
	messageResponseAccessForGuild,
} from '@app/api/channel/services/message/MessageResponseDataService';
import {resolveNsfwScopeChannel} from '@app/api/channel/utils/ThreadNsfwScope';
import type {IGuildRepositoryAggregate} from '@app/api/guild/repositories/IGuildRepositoryAggregate';
import {getPurgeQueue, getStorageService} from '@app/api/middleware/ServiceSingletons';
import {getMessageSearchService} from '@app/api/SearchFactory';
import {deleteMessageSearchDocuments} from '@app/api/search/MessageSearchIndexCleanup';
import {searchExistingMessages} from '@app/api/search/MessageSearchResultReconciler';
import {assertSafeByteSize} from '@app/api/utils/ByteSizeUtils';
import type {
	BrowseChannelRequest,
	SearchChannelMessagesRequest,
} from '@fluxer/schema/src/domains/admin/AdminMessageBrowseSchemas';
import type {
	DeleteMessageRequest,
	LookupMessageByAttachmentRequest,
	LookupMessageRequest,
} from '@fluxer/schema/src/domains/admin/AdminMessageSchemas';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';

interface AdminMessageServiceDeps {
	apiContext: ApiContext;
	channelRepository: IChannelRepository;
	guildRepository: IGuildRepositoryAggregate;
	auditService: AdminAuditService;
}

interface ChannelNsfwContext {
	channelNsfw: boolean | null;
	guildNsfwLevel: number | null;
	channelName: string | null;
	guildId: string | null;
	guildName: string | null;
}

export class AdminMessageService {
	constructor(private readonly deps: AdminMessageServiceDeps) {}

	async lookupMessage(data: LookupMessageRequest) {
		const channelId = createChannelID(data.channel_id);
		const messageId = createMessageID(data.message_id);
		const channelNsfwContext = await this.getChannelNsfwContext(channelId);
		const messageResponses = (
			await this.listMessageResponsesForAdmin({
				channelId,
				limit: data.context_limit,
				around: messageId,
			})
		).reverse();
		const adminMessages = messageResponses.map((message) =>
			this.mapMessageResponseToAdminMessage(message, channelNsfwContext),
		);
		return {
			messages: adminMessages,
			message_responses: messageResponses,
			message_id: messageId.toString(),
		};
	}

	async lookupMessageByAttachment(data: LookupMessageByAttachmentRequest) {
		const channelId = createChannelID(data.channel_id);
		const attachmentId = createAttachmentID(data.attachment_id);
		const messageId = await this.deps.channelRepository.lookupAttachmentByChannelAndFilename(
			channelId,
			attachmentId,
			data.filename,
		);
		if (!messageId) {
			return {
				messages: [],
				message_responses: [],
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
			message_responses: result.message_responses,
			message_id: messageId.toString(),
		};
	}

	async deleteMessage(data: DeleteMessageRequest, adminUserId: UserID, auditLogReason: string | null) {
		const {channelRepository, auditService} = this.deps;
		const {gateway: gatewayService, worker: workerService} = this.deps.apiContext.services;
		const channelId = createChannelID(data.channel_id);
		const messageId = createMessageID(data.message_id);
		const channel = await channelRepository.findUnique(channelId);
		const message = await channelRepository.getMessage(channelId, messageId);
		if (message) {
			await purgeMessageAttachments(message, getStorageService(), getPurgeQueue());
			await channelRepository.deleteMessage(
				channelId,
				messageId,
				message.authorId || createUserID(0n),
				message.pinnedTimestamp || undefined,
			);
			await decrementThreadMessageCount(channelRepository, channel, [messageId]);
			if (channel) {
				if (channel.guildId) {
					await gatewayService.dispatchGuild({
						guildId: channel.guildId,
						event: 'MESSAGE_DELETE',
						data: withThreadContext(channel, {
							channel_id: channelId.toString(),
							id: messageId.toString(),
						}),
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
			await deleteMessageSearchDocuments([messageId], {context: {source: 'admin_message_delete'}});
			await enqueueCrosspostSourceRemoval(workerService, {messages: [message], mode: 'purge'});
			await enqueueCrosspostFamilyPurgeFromCopies(workerService, {messages: [message]});
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

	async browseChannel(data: BrowseChannelRequest) {
		const {snowflake: snowflakeService} = this.deps.apiContext.services;
		const channelId = createChannelID(data.channel_id);
		const limit = data.limit ?? 50;
		const channelNsfwContext = await this.getChannelNsfwContext(channelId);
		let beforeId = data.before ? createMessageID(data.before) : undefined;
		const afterId = data.after ? createMessageID(data.after) : undefined;
		if (!beforeId && !afterId) {
			beforeId = createMessageID(await snowflakeService.generate());
		}
		const messages = await this.listMessageResponsesForAdmin({
			channelId,
			limit,
			before: beforeId,
			after: afterId,
		});
		const messageResponses = afterId ? messages : [...messages].reverse();
		const adminMessages = messageResponses.map((message) =>
			this.mapMessageResponseToAdminMessage(message, channelNsfwContext),
		);
		return {
			messages: adminMessages,
			message_responses: messageResponses,
			has_more: messages.length >= limit,
		};
	}

	async searchChannelMessages(data: SearchChannelMessagesRequest) {
		const {channelRepository} = this.deps;
		const channelId = createChannelID(data.channel_id);
		const limit = data.limit ?? 25;
		const channelNsfwContext = await this.getChannelNsfwContext(channelId);
		const searchService = getMessageSearchService();
		if (!searchService) {
			return {messages: [], message_responses: [], total: 0};
		}
		const result = await searchExistingMessages({
			searchService,
			messageRepository: channelRepository,
			query: data.query,
			filters: {channelIds: [channelId.toString()]},
			hitsPerPage: limit,
			page: 1,
		});
		const messageResponses = await createMessageResponseDataService().buildMessages({
			userId: createUserID(0n),
			messages: result.messages,
			access: await this.getMessageResponseAccessForAdmin(channelId),
		});
		const adminMessages = messageResponses.map((message) =>
			this.mapMessageResponseToAdminMessage(message, channelNsfwContext),
		);
		return {
			messages: adminMessages,
			message_responses: messageResponses,
			total: result.total,
		};
	}

	private async getMessageResponseAccessForAdmin(channelId: ChannelID): Promise<MessageResponseAccessContext> {
		const channel = await this.deps.channelRepository.findUnique(channelId);
		const access = channel ? messageResponseAccessForChannel(channel) : messageResponseAccessForGuild(null);
		return {...access, includeHidden: true};
	}

	private async listMessageResponsesForAdmin(params: {
		channelId: ChannelID;
		limit: number;
		before?: MessageID;
		after?: MessageID;
		around?: MessageID;
	}): Promise<Array<MessageResponse>> {
		const access = await this.getMessageResponseAccessForAdmin(params.channelId);
		return createMessageResponseDataService().listMessages({
			userId: createUserID(0n),
			channelId: params.channelId,
			limit: params.limit,
			before: params.before,
			after: params.after,
			around: params.around,
			access,
		});
	}

	private mapMessageResponseToAdminMessage(message: MessageResponse, channelNsfwContext: ChannelNsfwContext) {
		return {
			id: message.id,
			channel_id: message.channel_id ?? '',
			channel_name: channelNsfwContext.channelName,
			channel_nsfw: channelNsfwContext.channelNsfw,
			guild_id: channelNsfwContext.guildId,
			guild_name: channelNsfwContext.guildName,
			guild_nsfw_level: channelNsfwContext.guildNsfwLevel,
			author_id: message.author.id,
			author_username: message.author.username,
			author_global_name: message.author.global_name ?? null,
			author_discriminator: message.author.discriminator,
			author_avatar: message.author.avatar,
			content: message.content ?? '',
			timestamp: message.timestamp,
			poll: message.poll,
			attachments:
				message.attachments?.map((attachment) => ({
					id: attachment.id,
					filename: attachment.filename,
					url: attachment.url,
					nsfw: attachment.nsfw ?? null,
					content_type: attachment.content_type ?? null,
					width: attachment.width ?? null,
					height: attachment.height ?? null,
					size: attachment.size == null ? null : assertSafeByteSize(attachment.size, 'admin message attachment size'),
				})) ?? [],
		};
	}

	private async getChannelNsfwContext(channelId: ChannelID): Promise<ChannelNsfwContext> {
		const {channelRepository, guildRepository} = this.deps;
		const channel = await channelRepository.findUnique(channelId);
		if (!channel) {
			return {
				channelNsfw: null,
				guildNsfwLevel: null,
				channelName: null,
				guildId: null,
				guildName: null,
			};
		}
		if (!channel.guildId) {
			return {
				channelNsfw: channel.isNsfw,
				guildNsfwLevel: null,
				channelName: channel.name ?? null,
				guildId: null,
				guildName: null,
			};
		}
		const [guild, scope] = await Promise.all([
			guildRepository.findUnique(channel.guildId),
			resolveNsfwScopeChannel(channel, (id) => channelRepository.findUnique(id)),
		]);
		return {
			channelNsfw: scope.isNsfw,
			guildNsfwLevel: guild?.nsfwLevel ?? null,
			channelName: channel.name ?? null,
			guildId: channel.guildId.toString(),
			guildName: guild?.name ?? null,
		};
	}
}
