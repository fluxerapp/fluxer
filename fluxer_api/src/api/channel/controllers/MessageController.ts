// SPDX-License-Identifier: AGPL-3.0-or-later

import {requireSudoMode} from '@app/api/auth/services/SudoVerificationService';
import {createAttachmentID, createChannelID, createMessageID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import type {MessageRequest, MessageUpdateRequest} from '@app/api/channel/MessageTypes';
import {normalizeMessageRequestPayload} from '@app/api/channel/services/message/MessageRequestCompatibility';
import {parseMultipartMessageData} from '@app/api/channel/services/message/MessageRequestParser';
import {viewerFromCtx} from '@app/api/experiment/ChannelThreadsGate';
import {DefaultUserOnly, LoginRequired} from '@app/api/middleware/AuthMiddleware';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import {SudoModeMiddleware} from '@app/api/middleware/SudoModeMiddleware';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import {readStateCapable} from '@app/api/read_state/ReadStateChannelMeta';
import type {HonoApp} from '@app/api/types/HonoEnv';
import {assertAccountNotLimited} from '@app/api/user/AccountLimit';
import {parseJsonPreservingLargeIntegers} from '@app/api/utils/LosslessJsonParser';
import {Validator} from '@app/api/Validator';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {requireClientIp} from '@fluxer/ip_utils/src/ClientIp';
import {SudoVerificationSchema} from '@fluxer/schema/src/domains/auth/AuthSchemas';
import {
	ChannelIdMessageIdAttachmentIdParam,
	ChannelIdMessageIdParam,
	ChannelIdParam,
} from '@fluxer/schema/src/domains/common/CommonParamSchemas';
import {
	CompleteMultipartAttachmentUploadRequest,
	CompleteMultipartAttachmentUploadResponse,
	PresignedAttachmentUploadRequest,
	PresignedAttachmentUploadResponse,
} from '@fluxer/schema/src/domains/message/AttachmentUploadSchemas';
import {CrosspostSourceResponse} from '@fluxer/schema/src/domains/message/CrosspostSourceSchemas';
import {
	BulkDeleteMessagesRequest,
	BulkMessageFetchRequest,
	MessageAckRequest,
	MessageRequestSchema,
	MessagesQuery,
	MessageUpdateRequestSchema,
} from '@fluxer/schema/src/domains/message/MessageRequestSchemas';
import {
	BulkMessageFetchResponse,
	MessageListResponse,
	MessagePurgeResponse,
	MessageResponseSchema,
} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';

export function MessageController(app: HonoApp) {
	app.get(
		'/channels/:channel_id/messages',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_MESSAGES_GET),
		LoginRequired,
		Validator('param', ChannelIdParam),
		Validator('query', MessagesQuery),
		OpenAPI({
			operationId: 'list_messages',
			summary: 'List messages in a channel',
			responseSchema: MessageListResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Channels', 'Messages'],
			description:
				'Retrieves a paginated list of messages from a channel. User must have permission to view the channel. Supports pagination via limit, before, after, and around parameters. Returns messages in reverse chronological order (newest first).',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const channelId = createChannelID(ctx.req.valid('param').channel_id);
			const {limit, before, after, around} = ctx.req.valid('query');
			const requestCache = ctx.get('requestCache');
			const messageRequestService = ctx.get('messageRequestService');
			return ctx.json(
				await messageRequestService.listMessages({
					viewer: viewerFromCtx(ctx),
					userId,
					channelId,
					query: {
						limit,
						before: before ? createMessageID(before) : undefined,
						after: after ? createMessageID(after) : undefined,
						around: around ? createMessageID(around) : undefined,
					},
					requestCache,
				}),
			);
		},
	);
	app.post(
		'/channels/messages/bulk',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_MESSAGES_BULK_GET),
		LoginRequired,
		DefaultUserOnly,
		Validator('json', BulkMessageFetchRequest),
		OpenAPI({
			operationId: 'bulk_list_channel_messages',
			summary: 'List messages from multiple channels',
			responseSchema: BulkMessageFetchResponse,
			requestSchema: BulkMessageFetchRequest,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Channels', 'Messages'],
			description:
				'Fetches bounded message windows from multiple channels in one request. Each entry uses the same pagination semantics as the single-channel message list endpoint.',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const {requests} = ctx.req.valid('json');
			const requestCache = ctx.get('requestCache');
			const messageRequestService = ctx.get('messageRequestService');
			return ctx.json(
				await messageRequestService.listMessagesBulk({
					viewer: viewerFromCtx(ctx),
					userId,
					requests: requests.map((request) => ({
						channelId: createChannelID(request.channel_id),
						query: {
							limit: request.limit,
							before: request.before ? createMessageID(request.before) : undefined,
							after: request.after ? createMessageID(request.after) : undefined,
							around: request.around ? createMessageID(request.around) : undefined,
						},
					})),
					requestCache,
				}),
			);
		},
	);
	app.get(
		'/channels/:channel_id/messages/:message_id',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_MESSAGE_GET),
		LoginRequired,
		Validator('param', ChannelIdMessageIdParam),
		OpenAPI({
			operationId: 'get_message',
			summary: 'Fetch a message',
			responseSchema: MessageResponseSchema,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Channels', 'Messages'],
			description:
				'Retrieves a specific message by ID. User must have permission to view the channel and the message must exist. Returns full message details including content, author, reactions, and attachments.',
		}),
		async (ctx) => {
			const {channel_id, message_id} = ctx.req.valid('param');
			const user = ctx.get('user');
			const userId = user.id;
			const channelId = createChannelID(channel_id);
			const messageId = createMessageID(message_id);
			const requestCache = ctx.get('requestCache');
			const messageRequestService = ctx.get('messageRequestService');
			return ctx.json(
				await messageRequestService.getMessage({
					viewer: viewerFromCtx(ctx),
					userId,
					channelId,
					messageId,
					requestCache,
				}),
			);
		},
	);
	app.post(
		'/channels/:channel_id/messages',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_MESSAGE_CREATE),
		LoginRequired,
		Validator('param', ChannelIdParam),
		OpenAPI({
			operationId: 'send_message',
			summary: 'Send a message',
			responseSchema: MessageResponseSchema,
			requestSchema: MessageRequestSchema,
			requestFormSchema: MessageRequestSchema,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Channels', 'Messages'],
			description:
				'Sends a new message to a channel. Requires permission to send messages in the target channel. Supports text content, embeds, attachments (multipart), and mentions. Returns the created message object with full details.',
		}),
		async (ctx) => {
			const user = ctx.get('user');
			const channelId = createChannelID(ctx.req.valid('param').channel_id);
			const requestCache = ctx.get('requestCache');
			const messageRequestService = ctx.get('messageRequestService');
			const contentType = ctx.req.header('content-type');
			const validatedData = contentType?.includes('multipart/form-data')
				? ((await parseMultipartMessageData(ctx, user, channelId, MessageRequestSchema)) as MessageRequest)
				: await (async () => {
						let data: unknown;
						try {
							const raw = await ctx.req.text();
							data = raw.trim().length === 0 ? {} : parseJsonPreservingLargeIntegers(raw);
						} catch {
							throw InputValidationError.fromCode('message_data', ValidationErrorCodes.INVALID_MESSAGE_DATA);
						}
						const validationResult = MessageRequestSchema.safeParse(normalizeMessageRequestPayload(data));
						if (!validationResult.success) {
							throw InputValidationError.fromCode('message_data', ValidationErrorCodes.INVALID_MESSAGE_DATA);
						}
						return validationResult.data;
					})();
			const response = await messageRequestService.sendMessage({
				viewer: viewerFromCtx(ctx),
				user,
				channelId,
				data: validatedData as MessageRequest,
				requestCache,
			});
			return ctx.json(response);
		},
	);
	app.post(
		'/channels/:channel_id/attachments',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_ATTACHMENT_UPLOAD),
		LoginRequired,
		Validator('param', ChannelIdParam),
		Validator('json', PresignedAttachmentUploadRequest),
		OpenAPI({
			operationId: 'request_presigned_message_attachment_uploads',
			summary: 'Request presigned attachment upload URLs',
			responseSchema: PresignedAttachmentUploadResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Channels', 'Messages'],
			description:
				'Returns presigned upload URLs for message attachments in the target channel. Small files (<=10MB) return a singlepart PUT URL; larger files return a multipart plan (upload_id + per-part URLs) that the client should complete via the matching /attachments/complete endpoint. Requires message send and attachment permissions in guild channels.',
		}),
		async (ctx) => {
			const clientIp = requireClientIp(ctx.req.raw, {
				trustClientIpHeader: Config.proxy.trust_client_ip_header,
				clientIpHeaderName: Config.proxy.client_ip_header,
			});
			const channelId = createChannelID(ctx.req.valid('param').channel_id);
			const {attachments} = ctx.req.valid('json');
			return ctx.json({
				attachments: await ctx.get('channelService').attachments.requestPresignedAttachmentUploadUrls({
					viewer: viewerFromCtx(ctx),
					userId: ctx.get('user').id,
					channelId,
					clientIp,
					attachments,
				}),
			});
		},
	);
	app.post(
		'/channels/:channel_id/attachments/complete',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_ATTACHMENT_UPLOAD),
		LoginRequired,
		Validator('param', ChannelIdParam),
		Validator('json', CompleteMultipartAttachmentUploadRequest),
		OpenAPI({
			operationId: 'complete_multipart_message_attachment_uploads',
			summary: 'Finalize multipart attachment uploads',
			responseSchema: CompleteMultipartAttachmentUploadResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Channels', 'Messages'],
			description:
				'Finalizes one or more multipart attachment uploads. Called after all chunks have been PUT to their presigned URLs. The server lists the uploaded parts and issues S3 CompleteMultipartUpload. Returns the finalized upload keys, which can be referenced in a subsequent message create request.',
		}),
		async (ctx) => {
			const clientIp = requireClientIp(ctx.req.raw, {
				trustClientIpHeader: Config.proxy.trust_client_ip_header,
				clientIpHeaderName: Config.proxy.client_ip_header,
			});
			const channelId = createChannelID(ctx.req.valid('param').channel_id);
			const {uploads} = ctx.req.valid('json');
			return ctx.json({
				uploads: await ctx.get('channelService').attachments.completeMultipartAttachmentUploads({
					viewer: viewerFromCtx(ctx),
					userId: ctx.get('user').id,
					channelId,
					clientIp,
					uploads,
				}),
			});
		},
	);
	app.patch(
		'/channels/:channel_id/messages/:message_id',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_MESSAGE_UPDATE),
		LoginRequired,
		Validator('param', ChannelIdMessageIdParam),
		OpenAPI({
			operationId: 'edit_message',
			summary: 'Edit a message',
			responseSchema: MessageResponseSchema,
			requestSchema: MessageUpdateRequestSchema,
			requestFormSchema: MessageUpdateRequestSchema,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Channels', 'Messages'],
			description:
				'Updates an existing message. Only the message author can edit messages (or admins with proper permissions). Supports updating content, embeds, and attachments. Returns the updated message object. Maintains original message ID and timestamps.',
		}),
		async (ctx) => {
			const {channel_id, message_id} = ctx.req.valid('param');
			const user = ctx.get('user');
			const userId = user.id;
			const channelId = createChannelID(channel_id);
			const messageId = createMessageID(message_id);
			const requestCache = ctx.get('requestCache');
			const messageRequestService = ctx.get('messageRequestService');
			const contentType = ctx.req.header('content-type');
			const validatedData = contentType?.includes('multipart/form-data')
				? ((await parseMultipartMessageData(ctx, user, channelId, MessageUpdateRequestSchema)) as MessageUpdateRequest)
				: await (async () => {
						let data: unknown;
						try {
							const raw = await ctx.req.text();
							data = raw.trim().length === 0 ? {} : parseJsonPreservingLargeIntegers(raw);
						} catch {
							throw InputValidationError.fromCode('message_data', ValidationErrorCodes.INVALID_MESSAGE_DATA);
						}
						const validationResult = MessageUpdateRequestSchema.safeParse(normalizeMessageRequestPayload(data));
						if (!validationResult.success) {
							throw InputValidationError.fromCode('message_data', ValidationErrorCodes.INVALID_MESSAGE_DATA);
						}
						return validationResult.data;
					})();
			return ctx.json(
				await messageRequestService.editMessage({
					viewer: viewerFromCtx(ctx),
					userId,
					channelId,
					messageId,
					data: validatedData as MessageUpdateRequest,
					requestCache,
				}),
			);
		},
	);
	app.delete(
		'/channels/:channel_id/messages/ack',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_READ_STATE_DELETE),
		LoginRequired,
		Validator('param', ChannelIdParam),
		OpenAPI({
			operationId: 'clear_channel_read_state',
			summary: 'Clear channel read state',
			responseSchema: null,
			statusCode: 204,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Channels', 'Messages'],
			deprecated: true,
			description: 'Deprecated. Has no effect on the read state. Returns 204 No Content.',
		}),
		async (ctx) => {
			return ctx.body(null, 204);
		},
	);
	app.delete(
		'/channels/:channel_id/messages/:message_id',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_MESSAGE_DELETE),
		LoginRequired,
		Validator('param', ChannelIdMessageIdParam),
		OpenAPI({
			operationId: 'delete_message',
			summary: 'Delete a message',
			responseSchema: null,
			statusCode: 204,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Channels', 'Messages'],
			description:
				'Deletes a message permanently. Only the message author can delete messages (or admins/moderators with proper permissions). Cannot be undone. Returns 204 No Content on success.',
		}),
		async (ctx) => {
			const {channel_id, message_id} = ctx.req.valid('param');
			const userId = ctx.get('user').id;
			const channelId = createChannelID(channel_id);
			const messageId = createMessageID(message_id);
			const requestCache = ctx.get('requestCache');
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			await ctx.get('channelService').messages.deletion.deleteMessage({
				viewer: viewerFromCtx(ctx),
				userId,
				channelId,
				messageId,
				requestCache,
				auditLogReason,
			});
			return ctx.body(null, 204);
		},
	);
	app.delete(
		'/channels/:channel_id/messages/:message_id/attachments/:attachment_id',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_MESSAGE_DELETE),
		LoginRequired,
		Validator('param', ChannelIdMessageIdAttachmentIdParam),
		OpenAPI({
			operationId: 'delete_message_attachment',
			summary: 'Delete a message attachment',
			responseSchema: null,
			statusCode: 204,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Channels', 'Messages'],
			description:
				'Removes a specific attachment from a message while keeping the message intact. Only the message author can remove attachments (or admins/moderators). Returns 204 No Content on success.',
		}),
		async (ctx) => {
			const {channel_id, message_id, attachment_id} = ctx.req.valid('param');
			const userId = ctx.get('user').id;
			const channelId = createChannelID(channel_id);
			const messageId = createMessageID(message_id);
			const attachmentId = createAttachmentID(attachment_id);
			const requestCache = ctx.get('requestCache');
			await ctx.get('channelService').attachments.deleteAttachment({
				viewer: viewerFromCtx(ctx),
				userId,
				channelId,
				messageId: messageId,
				attachmentId,
				requestCache,
			});
			return ctx.body(null, 204);
		},
	);
	app.post(
		'/channels/:channel_id/messages/bulk-delete',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_MESSAGE_BULK_DELETE),
		LoginRequired,
		Validator('param', ChannelIdParam),
		Validator('json', BulkDeleteMessagesRequest),
		OpenAPI({
			operationId: 'bulk_delete_messages',
			summary: 'Bulk delete messages',
			responseSchema: null,
			statusCode: 204,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Channels', 'Messages'],
			description:
				'Deletes multiple messages at once. Requires moderation or admin permissions. Commonly used for message cleanup. Messages from different authors can be deleted together. Returns 204 No Content on success.',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const channelId = createChannelID(ctx.req.valid('param').channel_id);
			const messageIds = ctx.req.valid('json').message_ids.map(createMessageID);
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			await ctx.get('channelService').messages.deletion.bulkDeleteMessages({
				viewer: viewerFromCtx(ctx),
				userId,
				channelId,
				messageIds,
				auditLogReason,
			});
			return ctx.body(null, 204);
		},
	);
	app.post(
		'/channels/:channel_id/messages/purge',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_MESSAGE_PURGE),
		LoginRequired,
		Validator('param', ChannelIdParam),
		OpenAPI({
			operationId: 'purge_personal_notes_messages',
			summary: 'Purge all messages in personal notes',
			responseSchema: MessagePurgeResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Channels', 'Messages'],
			description:
				"Deletes every message in the caller's personal notes channel. Only allowed on the authenticated user's DM_PERSONAL_NOTES channel. Returns the total number of deleted messages.",
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const channelId = createChannelID(ctx.req.valid('param').channel_id);
			const {deletedCount} = await ctx
				.get('channelService')
				.messages.deletion.purgePersonalNotesMessages({viewer: viewerFromCtx(ctx), userId, channelId});
			return ctx.json({deleted_count: deletedCount});
		},
	);
	app.post(
		'/channels/:channel_id/messages/bulk-delete-mine',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_MESSAGE_BULK_DELETE),
		LoginRequired,
		Validator('param', ChannelIdParam),
		SudoModeMiddleware,
		Validator('json', SudoVerificationSchema),
		OpenAPI({
			operationId: 'bulk_delete_my_messages_in_channel',
			summary: 'Bulk delete my messages in channel',
			responseSchema: null,
			statusCode: 202,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Channels', 'Messages'],
			description:
				'Deletes every message the caller has authored in the specified channel. Requires sudo mode verification. Returns 202 Accepted once matching messages have been removed.',
		}),
		async (ctx) => {
			const user = ctx.get('user');
			const userId = user.id;
			const channelId = createChannelID(ctx.req.valid('param').channel_id);
			const body = ctx.req.valid('json');
			await ctx
				.get('channelService')
				.channelData.operations.getChannel({viewer: viewerFromCtx(ctx), userId, channelId});
			await requireSudoMode(ctx, user, body);
			await ctx.get('channelService').userMessageDeletion.deleteUserMessagesInScope(userId, {
				channelIds: [channelId],
			});
			return ctx.body(null, 202);
		},
	);
	app.post(
		'/channels/:channel_id/typing',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_TYPING),
		LoginRequired,
		Validator('param', ChannelIdParam),
		OpenAPI({
			operationId: 'indicate_typing',
			summary: 'Indicate typing activity',
			responseSchema: null,
			statusCode: [200, 204],
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Channels', 'Messages'],
			description:
				'Notifies other users in the channel that you are actively typing. Typing indicators typically expire after a short period (usually 10 seconds). Returns 204 No Content, or 200 with a JSON body holding the remaining slowmode cooldowns in message_send_cooldown_ms and thread_create_cooldown_ms when the user is rate limited. Commonly called repeatedly while the user is composing a message.',
		}),
		async (ctx) => {
			const user = ctx.get('user');
			const viewer = viewerFromCtx(ctx);
			const channelId = createChannelID(ctx.req.valid('param').channel_id);
			const channelService = ctx.get('channelService');
			const auth = await channelService.interactions.startTyping({viewer, userId: user.id, channelId});
			const cooldown = await channelService.getTypingCooldown({user, viewer, auth});
			return cooldown ? ctx.json(cooldown, 200) : ctx.body(null, 204);
		},
	);
	app.post(
		'/channels/:channel_id/messages/:message_id/crosspost',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_MESSAGE_CROSSPOST),
		LoginRequired,
		Validator('param', ChannelIdMessageIdParam),
		OpenAPI({
			operationId: 'crosspost_message',
			summary: 'Publish a message to following channels',
			responseSchema: MessageResponseSchema,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Channels', 'Messages'],
			description:
				'Publishes a message in an announcement channel to every channel that follows it. The author needs Send Messages. Anyone else needs Send Messages and Manage Messages. Only default messages that are not replies, forwards or copies can be published, and each message can be published once. Copies are delivered asynchronously. Publishing is limited per channel (10 in a row, then one every 6 minutes) and per community (30 in a row, then one every 2 minutes). Returns the updated message with the CROSSPOSTED flag set.',
		}),
		async (ctx) => {
			const {channel_id, message_id} = ctx.req.valid('param');
			assertAccountNotLimited(ctx.get('user'));
			return ctx.json(
				await ctx.get('messageRequestService').crosspostMessage({
					userId: ctx.get('user').id,
					viewer: viewerFromCtx(ctx),
					channelId: createChannelID(channel_id),
					messageId: createMessageID(message_id),
					requestCache: ctx.get('requestCache'),
				}),
			);
		},
	);
	app.get(
		'/channels/:channel_id/messages/:message_id/crosspost-source',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_MESSAGE_CROSSPOST_SOURCE),
		LoginRequired,
		Validator('param', ChannelIdMessageIdParam),
		OpenAPI({
			operationId: 'get_message_crosspost_source',
			summary: 'Get the source community of a published message copy',
			responseSchema: CrosspostSourceResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Channels', 'Messages'],
			description:
				'Returns the public profile of the community a message copy was published from. Works on copies delivered to a following channel and on the system message posted when a channel starts following. Needs the same access as fetching the message. The response holds the community name, icon, banner, badge features, approximate counts and whether it can be joined through discovery.',
		}),
		async (ctx) => {
			const {channel_id, message_id} = ctx.req.valid('param');
			return ctx.json(
				await ctx.get('messageRequestService').getCrosspostSource({
					userId: ctx.get('user').id,
					viewer: viewerFromCtx(ctx),
					channelId: createChannelID(channel_id),
					messageId: createMessageID(message_id),
					requestCache: ctx.get('requestCache'),
				}),
			);
		},
	);
	app.post(
		'/channels/:channel_id/messages/:message_id/ack',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_MESSAGE_ACK),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', ChannelIdMessageIdParam),
		Validator('json', MessageAckRequest),
		OpenAPI({
			operationId: 'acknowledge_message',
			summary: 'Acknowledge a message',
			responseSchema: null,
			statusCode: 204,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Channels', 'Messages'],
			description:
				'Marks a message as read and records acknowledgement state. Only available for regular user accounts. Updates mention count if provided. Returns 204 No Content on success.',
		}),
		async (ctx) => {
			const {channel_id, message_id} = ctx.req.valid('param');
			const userId = ctx.get('user').id;
			const channelId = createChannelID(channel_id);
			const messageId = createMessageID(message_id);
			const {mention_count: mentionCount, manual} = ctx.req.valid('json');
			await ctx.get('readStateService').ackMessage({
				userId,
				channelId,
				messageId,
				mentionCount: mentionCount ?? 0,
				manual,
				capable: readStateCapable(ctx),
			});
			return ctx.body(null, 204);
		},
	);
}
