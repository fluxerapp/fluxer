// SPDX-License-Identifier: AGPL-3.0-or-later

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
import type {ApiContext} from '../../ApiContext';
import {
	type AttachmentID,
	type ChannelID,
	createAttachmentID,
	createChannelID,
	createMessageID,
	createUserID,
	type MessageID,
	type UserID,
} from '../../BrandedTypes';
import type {IChannelRepository} from '../../channel/IChannelRepository';
import {purgeMessageAttachments} from '../../channel/services/message/MessageHelpers';
import {
	createMessageResponseDataService,
	type MessageResponseAccessContext,
	messageResponseAccessForChannel,
	messageResponseAccessForGuild,
} from '../../channel/services/message/MessageResponseDataService';
import type {NcmecAttachmentStatusResponse, NcmecSubmissionService} from '../../csam/NcmecSubmissionService';
import type {IGuildRepositoryAggregate} from '../../guild/repositories/IGuildRepositoryAggregate';
import {getPurgeQueue, getStorageService} from '../../middleware/ServiceSingletons';
import {getMessageSearchService} from '../../SearchFactory';
import {deleteMessageSearchDocuments} from '../../search/MessageSearchIndexCleanup';
import {searchExistingMessages} from '../../search/MessageSearchResultReconciler';
import {assertSafeByteSize} from '../../utils/ByteSizeUtils';
import type {AdminAuditService} from './AdminAuditService';

interface AdminMessageServiceDeps {
	apiContext: ApiContext;
	channelRepository: IChannelRepository;
	guildRepository: IGuildRepositoryAggregate;
	auditService: AdminAuditService;
	ncmecSubmissionService: NcmecSubmissionService;
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

	async lookupAttachment({
		channelId,
		attachmentId,
		filename,
	}: {
		channelId: ChannelID;
		attachmentId: AttachmentID;
		filename: string;
	}): Promise<{
		message_id: MessageID | null;
	}> {
		const {channelRepository} = this.deps;
		const messageId = await channelRepository.lookupAttachmentByChannelAndFilename(channelId, attachmentId, filename);
		return {
			message_id: messageId,
		};
	}

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
		const attachmentStatuses = await this.getAttachmentStatusesForMessages(messageResponses);
		const priorReports = await this.getPriorReportsForMessages(messageResponses);
		const adminMessages = messageResponses.map((message) =>
			this.mapMessageResponseToAdminMessage(message, channelNsfwContext, attachmentStatuses, priorReports),
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
		const {gateway: gatewayService} = this.deps.apiContext.services;
		const channelId = createChannelID(data.channel_id);
		const messageId = createMessageID(data.message_id);
		const channel = await channelRepository.findUnique(channelId);
		const message = await channelRepository.getMessage(channelId, messageId);
		if (message) {
			if (message.attachments.length > 0) {
				await purgeMessageAttachments(message, getStorageService(), getPurgeQueue());
			}
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
			await deleteMessageSearchDocuments([messageId], {context: {source: 'admin_message_delete'}});
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
		const attachmentStatuses = await this.getAttachmentStatusesForMessages(messageResponses);
		const priorReports = await this.getPriorReportsForMessages(messageResponses);
		const adminMessages = messageResponses.map((message) =>
			this.mapMessageResponseToAdminMessage(message, channelNsfwContext, attachmentStatuses, priorReports),
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
		const messageEntries = result.hits.map((hit) => ({
			channelId: createChannelID(BigInt(hit.channelId)),
			messageId: createMessageID(BigInt(hit.id)),
		}));
		const resolvedMessages = await Promise.all(
			messageEntries.map(({channelId, messageId}) => this.getMessageResponseForAdmin(channelId, messageId)),
		);
		const messageResponses = resolvedMessages.filter((message): message is MessageResponse => message !== null);
		const attachmentStatuses = await this.getAttachmentStatusesForMessages(messageResponses);
		const priorReports = await this.getPriorReportsForMessages(messageResponses);
		const adminMessages = messageResponses.map((message) =>
			this.mapMessageResponseToAdminMessage(message, channelNsfwContext, attachmentStatuses, priorReports),
		);
		return {
			messages: adminMessages,
			message_responses: messageResponses,
			total: result.total,
		};
	}

	private async getAttachmentStatusesForMessages(
		messages: Array<MessageResponse>,
	): Promise<Map<string, NcmecAttachmentStatusResponse>> {
		const attachmentIds = messages.flatMap((message) =>
			(message.attachments ?? []).map((attachment) => createAttachmentID(BigInt(attachment.id))),
		);
		return this.deps.ncmecSubmissionService.getAttachmentStatuses(attachmentIds);
	}

	private async getMessageResponseAccessForAdmin(channelId: ChannelID): Promise<MessageResponseAccessContext> {
		const channel = await this.deps.channelRepository.findUnique(channelId);
		return channel ? messageResponseAccessForChannel(channel) : messageResponseAccessForGuild(null);
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

	private async getMessageResponseForAdmin(
		channelId: ChannelID,
		messageId: MessageID,
	): Promise<MessageResponse | null> {
		const access = await this.getMessageResponseAccessForAdmin(channelId);
		return createMessageResponseDataService().getMessage({
			userId: createUserID(0n),
			channelId,
			messageId,
			access,
		});
	}

	private async getPriorReportsForMessages(messages: Array<MessageResponse>): Promise<Map<string, Array<string>>> {
		const authorIds = messages.map((message) => createUserID(BigInt(message.author.id)));
		return this.deps.ncmecSubmissionService.getUserPriorReportIds(authorIds);
	}

	private mapMessageResponseToAdminMessage(
		message: MessageResponse,
		channelNsfwContext: ChannelNsfwContext,
		attachmentStatuses: Map<string, NcmecAttachmentStatusResponse>,
		priorReports: Map<string, Array<string>>,
	) {
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
			user_prior_ncmec_report_ids: priorReports.get(message.author.id) ?? [],
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
					ncmec_status: attachmentStatuses.get(attachment.id)?.status ?? 'not_submitted',
					ncmec_report_id: attachmentStatuses.get(attachment.id)?.ncmec_report_id ?? null,
					ncmec_failure_reason: attachmentStatuses.get(attachment.id)?.failure_reason ?? null,
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
		const guild = await guildRepository.findUnique(channel.guildId);
		return {
			channelNsfw: channel.isNsfw,
			guildNsfwLevel: guild?.nsfwLevel ?? null,
			channelName: channel.name ?? null,
			guildId: channel.guildId.toString(),
			guildName: guild?.name ?? null,
		};
	}
}
