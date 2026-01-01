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

import type {Context} from 'hono';
import type {HonoApp, HonoEnv} from '~/App';
import {AttachmentDecayService} from '~/attachment/AttachmentDecayService';
import type {ChannelID, UserID} from '~/BrandedTypes';
import {createAttachmentID, createChannelID, createMessageID} from '~/BrandedTypes';
import {MAX_ATTACHMENTS_PER_MESSAGE} from '~/Constants';
import {
	type AttachmentRequestData,
	type ClientAttachmentReferenceRequest,
	type ClientAttachmentRequest,
	mergeUploadWithClientData,
	type UploadedAttachment,
} from '~/channel/AttachmentDTOs';
import {MessageRequest, MessageUpdateRequest, mapMessageToResponse} from '~/channel/ChannelModel';
import {collectMessageAttachments, isPersonalNotesChannel} from '~/channel/services/message/MessageHelpers';
import {InputValidationError, UnclaimedAccountRestrictedError} from '~/Errors';
import {DefaultUserOnly, LoginRequired} from '~/middleware/AuthMiddleware';
import {RateLimitMiddleware} from '~/middleware/RateLimitMiddleware';
import {RateLimitConfigs} from '~/RateLimitConfig';
import {createQueryIntegerType, Int32Type, Int64Type, z} from '~/Schema';
import {Validator} from '~/Validator';

const DEFAULT_ATTACHMENT_UPLOAD_TTL_MS = 7 * 24 * 60 * 60 * 1000;

export interface ParseMultipartMessageDataOptions {
	uploadExpiresAt?: Date;
	onPayloadParsed?: (payload: unknown) => void;
}

export async function parseMultipartMessageData(
	ctx: Context<HonoEnv>,
	userId: UserID,
	channelId: ChannelID,
	schema: z.ZodTypeAny,
	options?: ParseMultipartMessageDataOptions,
): Promise<MessageRequest | MessageUpdateRequest> {
	let body: Record<string, string | File | Array<string | File>>;
	try {
		body = await ctx.req.parseBody();
	} catch (_error) {
		throw InputValidationError.create(
			'multipart_form',
			'Failed to parse multipart form data. Please check that all field names and filenames are properly formatted.',
		);
	}

	if (!body.payload_json || typeof body.payload_json !== 'string') {
		throw InputValidationError.create('payload_json', 'payload_json field is required for multipart messages');
	}

	let jsonData: unknown;
	try {
		jsonData = JSON.parse(body.payload_json);
	} catch (_error) {
		throw InputValidationError.create('payload_json', 'Invalid JSON in payload_json');
	}

	options?.onPayloadParsed?.(jsonData);
	const validationResult = schema.safeParse(jsonData);
	if (!validationResult.success) {
		throw InputValidationError.create('message_data', 'Invalid message data');
	}

	const data = validationResult.data as Partial<MessageRequest> &
		Partial<MessageUpdateRequest> & {
			attachments?: Array<AttachmentRequestData>;
		};

	const filesWithIndices: Array<{file: File; index: number}> = [];
	const seenIndices = new Set<number>();
	const fieldNamePattern = /^files\[(\d+)\]$/;

	Object.keys(body).forEach((key) => {
		if (key.startsWith('files[')) {
			const match = fieldNamePattern.exec(key);
			if (!match) {
				throw InputValidationError.create(
					'files',
					`Invalid file field name: ${key}. Expected format: files[N] where N is a number`,
				);
			}

			const index = parseInt(match[1], 10);

			if (index >= MAX_ATTACHMENTS_PER_MESSAGE) {
				throw InputValidationError.create(
					'files',
					`File index ${index} exceeds maximum allowed index of ${MAX_ATTACHMENTS_PER_MESSAGE - 1}`,
				);
			}

			if (seenIndices.has(index)) {
				throw InputValidationError.create('files', `Duplicate file index: ${index}`);
			}

			const file = body[key];
			if (file instanceof File) {
				filesWithIndices.push({file, index});
				seenIndices.add(index);
			} else if (Array.isArray(file)) {
				const validFiles = file.filter((f) => f instanceof File);
				if (validFiles.length > 0) {
					throw InputValidationError.create('files', `Multiple files for index ${index} not allowed`);
				}
			}
		}
	});

	if (filesWithIndices.length > MAX_ATTACHMENTS_PER_MESSAGE) {
		throw InputValidationError.create('files', `Too many files. Maximum ${MAX_ATTACHMENTS_PER_MESSAGE} files allowed`);
	}

	if (filesWithIndices.length > 0) {
		if (!data.attachments || !Array.isArray(data.attachments) || data.attachments.length === 0) {
			throw InputValidationError.create('attachments', 'Attachments metadata array is required when uploading files');
		}

		type AttachmentMetadata = ClientAttachmentRequest | ClientAttachmentReferenceRequest;
		const attachmentMetadata = data.attachments as Array<AttachmentMetadata>;

		const newAttachments = attachmentMetadata.filter(
			(a): a is ClientAttachmentRequest => 'filename' in a && a.filename !== undefined,
		);
		const existingAttachments = attachmentMetadata.filter(
			(a): a is ClientAttachmentReferenceRequest => !('filename' in a) || a.filename === undefined,
		);

		const metadataIds = new Set(newAttachments.map((a) => a.id));
		const fileIds = new Set(filesWithIndices.map((f) => f.index));

		for (const fileId of fileIds) {
			if (!metadataIds.has(fileId)) {
				throw InputValidationError.create('attachments', `No metadata provided for file with ID ${fileId}`);
			}
		}

		for (const att of newAttachments) {
			if (!fileIds.has(att.id)) {
				throw InputValidationError.create('attachments', `No file uploaded for attachment metadata with ID ${att.id}`);
			}
		}

		const uploadExpiresAt = options?.uploadExpiresAt ?? new Date(Date.now() + DEFAULT_ATTACHMENT_UPLOAD_TTL_MS);

		const uploadedAttachments: Array<UploadedAttachment> = await ctx.get('channelService').uploadFormDataAttachments({
			userId,
			channelId,
			files: filesWithIndices,
			attachmentMetadata: newAttachments,
			expiresAt: uploadExpiresAt,
		});

		const uploadedMap = new Map(uploadedAttachments.map((u) => [u.id, u]));

		const processedNewAttachments = newAttachments.map((clientData) => {
			const uploaded = uploadedMap.get(clientData.id);
			if (!uploaded) {
				throw InputValidationError.create('attachments', `No file uploaded for attachment with ID ${clientData.id}`);
			}

			if (clientData.filename !== uploaded.filename) {
				throw InputValidationError.create(
					'attachments',
					`Filename mismatch for attachment ${clientData.id}: metadata specifies "${clientData.filename}" but this doesn't match`,
				);
			}

			return mergeUploadWithClientData(uploaded, clientData);
		});

		data.attachments = [...existingAttachments, ...processedNewAttachments];
	} else if (
		data.attachments?.some((a: unknown) => {
			const attachment = a as ClientAttachmentRequest | ClientAttachmentReferenceRequest;
			return 'filename' in attachment && attachment.filename;
		})
	) {
		throw InputValidationError.create(
			'attachments',
			'Attachment metadata with filename provided but no files uploaded',
		);
	}

	return data as MessageRequest | MessageUpdateRequest;
}

export const MessageController = (app: HonoApp) => {
	const decayService = new AttachmentDecayService();

	app.get(
		'/channels/:channel_id/messages',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_MESSAGES_GET),
		LoginRequired,
		Validator('param', z.object({channel_id: Int64Type})),
		Validator(
			'query',
			z.object({
				limit: createQueryIntegerType({defaultValue: 50, minValue: 1, maxValue: 100}),
				before: z.optional(Int64Type),
				after: z.optional(Int64Type),
				around: z.optional(Int64Type),
			}),
		),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const channelId = createChannelID(ctx.req.valid('param').channel_id);
			const {limit, before, after, around} = ctx.req.valid('query');
			const requestCache = ctx.get('requestCache');
			const messages = await ctx.get('channelService').getMessages({
				userId,
				channelId,
				limit,
				before: before ? createMessageID(before) : undefined,
				after: after ? createMessageID(after) : undefined,
				around: around ? createMessageID(around) : undefined,
			});
			const allAttachments = messages.flatMap((message) => collectMessageAttachments(message));
			const attachmentDecayMap =
				allAttachments.length > 0
					? await decayService.fetchMetadata(allAttachments.map((att) => ({attachmentId: att.id})))
					: undefined;
			return ctx.json(
				await Promise.all(
					messages.map((message) =>
						mapMessageToResponse({
							message,
							currentUserId: userId,
							userCacheService: ctx.get('userCacheService'),
							requestCache,
							mediaService: ctx.get('mediaService'),
							attachmentDecayMap,
							getReactions: (channelId, messageId) =>
								ctx.get('channelService').getMessageReactions({userId, channelId, messageId}),
							setHasReaction: (channelId, messageId, hasReaction) =>
								ctx.get('channelService').setHasReaction(channelId, messageId, hasReaction),
							getReferencedMessage: (channelId, messageId) =>
								ctx.get('channelRepository').getMessage(channelId, messageId),
						}),
					),
				),
			);
		},
	);

	app.get(
		'/channels/:channel_id/messages/:message_id',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_MESSAGE_GET),
		LoginRequired,
		Validator('param', z.object({channel_id: Int64Type, message_id: Int64Type})),
		async (ctx) => {
			const {channel_id, message_id} = ctx.req.valid('param');
			const userId = ctx.get('user').id;
			const channelId = createChannelID(channel_id);
			const messageId = createMessageID(message_id);
			const requestCache = ctx.get('requestCache');
			const message = await ctx.get('channelService').getMessage({userId, channelId, messageId});
			const messageAttachments = collectMessageAttachments(message);
			const attachmentDecayMap =
				messageAttachments.length > 0
					? await decayService.fetchMetadata(messageAttachments.map((att) => ({attachmentId: att.id})))
					: undefined;
			return ctx.json(
				await mapMessageToResponse({
					message,
					currentUserId: userId,
					userCacheService: ctx.get('userCacheService'),
					requestCache,
					mediaService: ctx.get('mediaService'),
					attachmentDecayMap,
					getReactions: (channelId, messageId) =>
						ctx.get('channelService').getMessageReactions({userId, channelId, messageId}),
					setHasReaction: (channelId, messageId, hasReaction) =>
						ctx.get('channelService').setHasReaction(channelId, messageId, hasReaction),
					getReferencedMessage: (channelId, messageId) => ctx.get('channelRepository').getMessage(channelId, messageId),
				}),
			);
		},
	);

	app.post(
		'/channels/:channel_id/messages',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_MESSAGE_CREATE),
		LoginRequired,
		Validator('param', z.object({channel_id: Int64Type})),
		async (ctx) => {
			const user = ctx.get('user');
			const channelId = createChannelID(ctx.req.valid('param').channel_id);
			const requestCache = ctx.get('requestCache');

			if (!user.passwordHash && !isPersonalNotesChannel({userId: user.id, channelId})) {
				throw new UnclaimedAccountRestrictedError('send messages');
			}

			const contentType = ctx.req.header('content-type');
			const validatedData = contentType?.includes('multipart/form-data')
				? ((await parseMultipartMessageData(ctx, user.id, channelId, MessageRequest)) as MessageRequest)
				: await (async () => {
						const data: unknown = await ctx.req.json();
						const validationResult = MessageRequest.safeParse(data);
						if (!validationResult.success) {
							throw InputValidationError.create('message_data', 'Invalid message data');
						}
						return validationResult.data;
					})();
			const message = await ctx
				.get('channelService')
				.sendMessage({user, channelId, data: validatedData as MessageRequest, requestCache});
			const messageAttachments = collectMessageAttachments(message);
			const attachmentDecayMap =
				messageAttachments.length > 0
					? await decayService.fetchMetadata(messageAttachments.map((att) => ({attachmentId: att.id})))
					: undefined;
			return ctx.json(
				await mapMessageToResponse({
					message,
					currentUserId: user.id,
					nonce: validatedData.nonce,
					userCacheService: ctx.get('userCacheService'),
					requestCache,
					mediaService: ctx.get('mediaService'),
					attachmentDecayMap,
					getReferencedMessage: (channelId, messageId) => ctx.get('channelRepository').getMessage(channelId, messageId),
				}),
			);
		},
	);

	app.patch(
		'/channels/:channel_id/messages/:message_id',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_MESSAGE_UPDATE),
		LoginRequired,
		Validator('param', z.object({channel_id: Int64Type, message_id: Int64Type})),
		async (ctx) => {
			const {channel_id, message_id} = ctx.req.valid('param');
			const userId = ctx.get('user').id;
			const channelId = createChannelID(channel_id);
			const messageId = createMessageID(message_id);
			const requestCache = ctx.get('requestCache');

			const contentType = ctx.req.header('content-type');
			const validatedData = contentType?.includes('multipart/form-data')
				? ((await parseMultipartMessageData(ctx, userId, channelId, MessageUpdateRequest)) as MessageUpdateRequest)
				: await (async () => {
						const data: unknown = await ctx.req.json();
						const validationResult = MessageUpdateRequest.safeParse(data);
						if (!validationResult.success) {
							throw InputValidationError.create('message_data', 'Invalid message data');
						}
						return validationResult.data;
					})();
			const message = await ctx.get('channelService').editMessage({
				userId,
				channelId,
				messageId,
				data: validatedData as MessageUpdateRequest,
				requestCache,
			});
			const messageAttachments = collectMessageAttachments(message);
			const attachmentDecayMap =
				messageAttachments.length > 0
					? await decayService.fetchMetadata(messageAttachments.map((att) => ({attachmentId: att.id})))
					: undefined;
			return ctx.json(
				await mapMessageToResponse({
					message,
					currentUserId: userId,
					userCacheService: ctx.get('userCacheService'),
					requestCache,
					mediaService: ctx.get('mediaService'),
					attachmentDecayMap,
					getReferencedMessage: (channelId, messageId) => ctx.get('channelRepository').getMessage(channelId, messageId),
				}),
			);
		},
	);

	app.delete(
		'/channels/:channel_id/messages/ack',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_READ_STATE_DELETE),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', z.object({channel_id: Int64Type})),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const channelId = createChannelID(ctx.req.valid('param').channel_id);
			await ctx.get('channelService').deleteReadState({userId, channelId});
			return ctx.body(null, 204);
		},
	);

	app.delete(
		'/channels/:channel_id/messages/:message_id',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_MESSAGE_DELETE),
		LoginRequired,
		Validator('param', z.object({channel_id: Int64Type, message_id: Int64Type})),
		async (ctx) => {
			const {channel_id, message_id} = ctx.req.valid('param');
			const userId = ctx.get('user').id;
			const channelId = createChannelID(channel_id);
			const messageId = createMessageID(message_id);
			const requestCache = ctx.get('requestCache');
			await ctx.get('channelService').deleteMessage({userId, channelId, messageId, requestCache});
			return ctx.body(null, 204);
		},
	);

	app.delete(
		'/channels/:channel_id/messages/:message_id/attachments/:attachment_id',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_MESSAGE_DELETE),
		LoginRequired,
		Validator('param', z.object({channel_id: Int64Type, message_id: Int64Type, attachment_id: Int64Type})),
		async (ctx) => {
			const {channel_id, message_id, attachment_id} = ctx.req.valid('param');
			const userId = ctx.get('user').id;
			const channelId = createChannelID(channel_id);
			const messageId = createMessageID(message_id);
			const attachmentId = createAttachmentID(attachment_id);
			const requestCache = ctx.get('requestCache');
			await ctx.get('channelService').deleteAttachment({
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
		Validator('param', z.object({channel_id: Int64Type})),
		Validator('json', z.object({message_ids: z.array(Int64Type)})),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const channelId = createChannelID(ctx.req.valid('param').channel_id);
			const messageIds = ctx.req.valid('json').message_ids.map(createMessageID);
			await ctx.get('channelService').bulkDeleteMessages({userId, channelId, messageIds});
			return ctx.body(null, 204);
		},
	);

	app.post(
		'/channels/:channel_id/typing',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_TYPING),
		LoginRequired,
		Validator('param', z.object({channel_id: Int64Type})),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const channelId = createChannelID(ctx.req.valid('param').channel_id);
			await ctx.get('channelService').startTyping({userId, channelId});
			return ctx.body(null, 204);
		},
	);

	app.post(
		'/channels/:channel_id/messages/:message_id/ack',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_MESSAGE_ACK),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', z.object({channel_id: Int64Type, message_id: Int64Type})),
		Validator('json', z.object({mention_count: Int32Type.optional(), manual: z.optional(z.boolean())})),
		async (ctx) => {
			const {channel_id, message_id} = ctx.req.valid('param');
			const userId = ctx.get('user').id;
			const channelId = createChannelID(channel_id);
			const messageId = createMessageID(message_id);
			const {mention_count: mentionCount, manual} = ctx.req.valid('json');
			await ctx.get('channelService').ackMessage({
				userId,
				channelId,
				messageId,
				mentionCount: mentionCount ?? 0,
				manual,
			});
			return ctx.body(null, 204);
		},
	);
};
