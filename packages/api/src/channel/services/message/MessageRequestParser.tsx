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

import type {ChannelID} from '@fluxer/api/src/BrandedTypes';
import {
	type AttachmentRequestData,
	mergeUploadWithClientData,
	type UploadedAttachment,
} from '@fluxer/api/src/channel/AttachmentDTOs';
import type {IChannelRepository} from '@fluxer/api/src/channel/IChannelRepository';
import type {MessageRequest, MessageUpdateRequest} from '@fluxer/api/src/channel/MessageTypes';
import type {GuildService} from '@fluxer/api/src/guild/services/GuildService';
import type {LimitConfigService} from '@fluxer/api/src/limits/LimitConfigService';
import {resolveLimitSafe} from '@fluxer/api/src/limits/LimitConfigUtils';
import {createLimitMatchContext} from '@fluxer/api/src/limits/LimitMatchContextBuilder';
import type {User} from '@fluxer/api/src/models/User';
import type {HonoEnv} from '@fluxer/api/src/types/HonoEnv';
import {parseJsonPreservingLargeIntegers} from '@fluxer/api/src/utils/LosslessJsonParser';
import {MAX_ATTACHMENTS_PER_MESSAGE} from '@fluxer/constants/src/LimitConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import type {
	ClientAttachmentReferenceRequest,
	ClientAttachmentRequest,
} from '@fluxer/schema/src/domains/message/AttachmentSchemas';
import type {Context} from 'hono';
import type {z} from 'zod';

const FIELD_NAME_PATTERN = /^files\[(\d+)\]$/;

export interface ParseMultipartMessageDataOptions {
	uploadExpiresAt?: Date;
	onPayloadParsed?: (payload: unknown) => void;
}

export async function parseMultipartMessageData(
	ctx: Context<HonoEnv>,
	user: User,
	channelId: ChannelID,
	schema: z.ZodTypeAny,
	options?: ParseMultipartMessageDataOptions,
): Promise<MessageRequest | MessageUpdateRequest> {
	let body: Record<string, string | File | Array<string | File>>;
	try {
		body = await ctx.req.parseBody();
	} catch (_error) {
		throw InputValidationError.fromCode('multipart_form', ValidationErrorCodes.FAILED_TO_PARSE_MULTIPART_FORM_DATA);
	}

	if (!body['payload_json'] || typeof body['payload_json'] !== 'string') {
		throw InputValidationError.fromCode('payload_json', ValidationErrorCodes.PAYLOAD_JSON_REQUIRED_FOR_MULTIPART);
	}

	let jsonData: unknown;
	try {
		jsonData = parseJsonPreservingLargeIntegers(body['payload_json']);
	} catch (_error) {
		throw InputValidationError.fromCode('payload_json', ValidationErrorCodes.INVALID_JSON_IN_PAYLOAD_JSON);
	}

	options?.onPayloadParsed?.(jsonData);
	const validationResult = schema.safeParse(jsonData);
	if (!validationResult.success) {
		throw InputValidationError.fromCode('message_data', ValidationErrorCodes.INVALID_MESSAGE_DATA);
	}

	const data = validationResult.data as Partial<MessageRequest> &
		Partial<MessageUpdateRequest> & {
			attachments?: Array<AttachmentRequestData>;
		};

	const maxAttachments = await resolveMessageAttachmentLimit(ctx, user, channelId);
	const maxIndexLabel = maxAttachments > 0 ? maxAttachments - 1 : 0;

	const filesWithIndices: Array<{file: File; index: number}> = [];
	const seenIndices = new Set<number>();

	Object.keys(body).forEach((key) => {
		if (key.startsWith('files[')) {
			const match = FIELD_NAME_PATTERN.exec(key);
			if (!match) {
				throw InputValidationError.fromCode('files', ValidationErrorCodes.INVALID_FILE_FIELD_NAME, {
					key,
				});
			}

			const index = parseInt(match[1], 10);

			if (Number.isNaN(index) || index < 0 || index > 10000) {
				throw InputValidationError.fromCode('files', ValidationErrorCodes.FILE_INDEX_EXCEEDS_MAXIMUM, {
					index,
					maxIndex: 10000,
				});
			}

			if (maxAttachments <= 0) {
				throw InputValidationError.fromCode('files', ValidationErrorCodes.ATTACHMENTS_NOT_ALLOWED_FOR_MESSAGE);
			}

			if (index >= maxAttachments) {
				throw InputValidationError.fromCode('files', ValidationErrorCodes.FILE_INDEX_EXCEEDS_MAXIMUM, {
					index,
					maxIndex: maxIndexLabel,
				});
			}

			if (seenIndices.has(index)) {
				throw InputValidationError.fromCode('files', ValidationErrorCodes.DUPLICATE_FILE_INDEX, {
					index,
				});
			}

			const file = body[key];
			if (file instanceof File) {
				filesWithIndices.push({file, index});
				seenIndices.add(index);
			} else if (Array.isArray(file)) {
				const validFiles = file.filter((f) => f instanceof File);
				if (validFiles.length > 0) {
					throw InputValidationError.fromCode('files', ValidationErrorCodes.MULTIPLE_FILES_FOR_INDEX_NOT_ALLOWED, {
						index,
					});
				}
			}
		}
	});

	if (filesWithIndices.length > maxAttachments) {
		throw InputValidationError.fromCode('files', ValidationErrorCodes.TOO_MANY_FILES, {maxFiles: maxAttachments});
	}

	if (filesWithIndices.length > 0) {
		if (!data.attachments || !Array.isArray(data.attachments) || data.attachments.length === 0) {
			throw InputValidationError.fromCode(
				'attachments',
				ValidationErrorCodes.ATTACHMENTS_METADATA_REQUIRED_WHEN_UPLOADING,
			);
		}
	}

	const hasAttachmentMetadata = data.attachments && Array.isArray(data.attachments) && data.attachments.length > 0;

	if (hasAttachmentMetadata) {
		type AttachmentMetadata = ClientAttachmentRequest | ClientAttachmentReferenceRequest;
		const attachmentMetadata = data.attachments as Array<AttachmentMetadata>;

		const newAttachments = attachmentMetadata.filter(
			(a): a is ClientAttachmentRequest => 'filename' in a && a.filename !== undefined,
		);
		const existingAttachments = attachmentMetadata.filter(
			(a): a is ClientAttachmentReferenceRequest => !('filename' in a) || a.filename === undefined,
		);

		const fileIds = new Set(filesWithIndices.map((f) => f.index));

		const inlineNewAttachments: Array<ClientAttachmentRequest> = [];

		for (const att of newAttachments) {
			const id = typeof att.id === 'string' ? parseInt(att.id, 10) : att.id;
			if (fileIds.has(id)) {
				inlineNewAttachments.push(att);
			} else {
				throw InputValidationError.fromCode('attachments', ValidationErrorCodes.NO_FILE_FOR_ATTACHMENT_METADATA, {
					attachmentId: att.id,
				});
			}
		}

		const inlineMetadataIds = new Set(
			inlineNewAttachments.map((a) => {
				const id = a.id;
				return typeof id === 'string' ? parseInt(id, 10) : id;
			}),
		);

		for (const fileId of fileIds) {
			if (!inlineMetadataIds.has(fileId)) {
				throw InputValidationError.fromCode('attachments', ValidationErrorCodes.NO_METADATA_FOR_FILE, {fileId});
			}
		}

		let processedInlineAttachments: Array<AttachmentRequestData> = [];
		if (inlineNewAttachments.length > 0) {
			const uploadedAttachments: Array<UploadedAttachment> = await ctx.get('channelService').uploadFormDataAttachments({
				userId: user.id,
				channelId,
				files: filesWithIndices,
				attachmentMetadata: inlineNewAttachments,
				expiresAt: options?.uploadExpiresAt,
			});

			const uploadedMap = new Map(uploadedAttachments.map((u) => [u.id, u]));

			processedInlineAttachments = inlineNewAttachments.map((clientData) => {
				const id = typeof clientData.id === 'string' ? parseInt(clientData.id, 10) : clientData.id;
				const uploaded = uploadedMap.get(id);
				if (!uploaded) {
					throw InputValidationError.fromCode('attachments', ValidationErrorCodes.NO_FILE_FOR_ATTACHMENT, {
						attachmentId: clientData.id,
					});
				}

				if (clientData.filename !== uploaded.filename) {
					throw InputValidationError.fromCode('attachments', ValidationErrorCodes.FILENAME_MISMATCH_FOR_ATTACHMENT, {
						attachmentId: clientData.id,
						expectedFilename: clientData.filename,
					});
				}

				return mergeUploadWithClientData(uploaded, clientData);
			});
		}

		data.attachments = [...existingAttachments, ...processedInlineAttachments];
	}

	return data as MessageRequest | MessageUpdateRequest;
}

async function resolveMessageAttachmentLimit(ctx: Context<HonoEnv>, user: User, channelId: ChannelID): Promise<number> {
	const limitConfigService = ctx.get('limitConfigService') as LimitConfigService | undefined;
	if (!limitConfigService) {
		return MAX_ATTACHMENTS_PER_MESSAGE;
	}

	let guildFeatures: Iterable<string> | null = null;
	const channelRepository = ctx.get('channelRepository') as IChannelRepository | undefined;
	const guildService = ctx.get('guildService') as GuildService | undefined;

	if (channelRepository) {
		try {
			const channel = await channelRepository.findUnique(channelId);
			if (channel?.guildId && guildService) {
				const guild = await guildService.getGuildSystem(channel.guildId);
				guildFeatures = guild.features;
			}
		} catch {
			guildFeatures = null;
		}
	}

	const ctxLimits = createLimitMatchContext({
		user,
		guildFeatures,
	});
	const limitValue = resolveLimitSafe(
		limitConfigService.getConfigSnapshot(),
		ctxLimits,
		'max_attachments_per_message',
		MAX_ATTACHMENTS_PER_MESSAGE,
	);

	return Math.floor(limitValue);
}
