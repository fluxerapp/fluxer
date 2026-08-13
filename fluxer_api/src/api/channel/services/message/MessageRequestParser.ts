// SPDX-License-Identifier: AGPL-3.0-or-later

import {MAX_ATTACHMENTS_PER_MESSAGE} from '@fluxer/constants/src/LimitConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {requireClientIp} from '@fluxer/ip_utils/src/ClientIp';
import type {
	ClientAttachmentReferenceRequest,
	ClientAttachmentRequest,
	ClientUploadedAttachmentRequest,
} from '@fluxer/schema/src/domains/message/AttachmentSchemas';
import type {Context} from 'hono';
import type {z} from 'zod';
import type {ChannelID} from '../../../BrandedTypes';
import {Config} from '../../../Config';
import type {GuildService} from '../../../guild/services/GuildService';
import type {LimitConfigService} from '../../../limits/LimitConfigService';
import {resolveLimitSafe} from '../../../limits/LimitConfigUtils';
import {createLimitMatchContext} from '../../../limits/LimitMatchContextBuilder';
import type {User} from '../../../models/User';
import type {HonoEnv} from '../../../types/HonoEnv';
import {parseJsonPreservingLargeIntegers} from '../../../utils/LosslessJsonParser';
import {inputValidationErrorFromZodIssues} from '../../../Validator';
import {type AttachmentRequestData, mergeUploadWithClientData, type UploadedAttachment} from '../../AttachmentDTOs';
import type {IChannelRepository} from '../../IChannelRepository';
import type {MessageRequest, MessageUpdateRequest} from '../../MessageTypes';
import {normalizeMessageRequestPayload} from './MessageRequestCompatibility';

const FIELD_NAME_PATTERN = /^files\[(\d+)\]$/;
const LEGACY_FILE_FIELD_NAME_PATTERN = /^file(\d+)?$/;

type MultipartBody = Record<string, string | File | Array<string | File>>;
type AttachmentMetadata = ClientAttachmentRequest | ClientUploadedAttachmentRequest | ClientAttachmentReferenceRequest;

interface ParseMultipartMessageDataOptions {
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
	let body: MultipartBody;
	try {
		body = await ctx.req.parseBody();
	} catch (_error) {
		throw InputValidationError.fromCode('multipart_form', ValidationErrorCodes.FAILED_TO_PARSE_MULTIPART_FORM_DATA);
	}
	const jsonData = parseMultipartPayload(body);
	const mergedJsonData = normalizeMessageRequestPayload(mergeMultipartScalarFields(body, jsonData));
	options?.onPayloadParsed?.(mergedJsonData);
	const validationResult = schema.safeParse(mergedJsonData);
	if (!validationResult.success) {
		throw inputValidationErrorFromZodIssues(validationResult.error.issues);
	}
	const data = validationResult.data as Partial<MessageRequest> &
		Partial<MessageUpdateRequest> & {
			attachments?: Array<AttachmentRequestData>;
		};
	const maxAttachments = await resolveMessageAttachmentLimit(ctx, user, channelId);
	const maxIndexLabel = maxAttachments > 0 ? maxAttachments - 1 : 0;
	const filesWithIndices: Array<{
		file: File;
		index: number;
	}> = [];
	const seenIndices = new Set<number>();
	let nextLegacyFileIndex = 0;
	for (const [key, fieldValue] of Object.entries(body)) {
		const fileIndex = getMultipartFileFieldIndex(key, nextLegacyFileIndex);
		if (fileIndex === null) {
			continue;
		}
		if (fileIndex.isLegacy) {
			nextLegacyFileIndex = Math.max(nextLegacyFileIndex, fileIndex.index + 1);
		}
		const {index} = fileIndex;
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
		if (fieldValue instanceof File) {
			filesWithIndices.push({file: fieldValue, index});
			seenIndices.add(index);
		} else if (Array.isArray(fieldValue)) {
			const validFiles = fieldValue.filter((f) => f instanceof File);
			if (validFiles.length > 0) {
				if (fileIndex.isLegacy && validFiles.length === 1) {
					filesWithIndices.push({file: validFiles[0], index});
					seenIndices.add(index);
					continue;
				}
				throw InputValidationError.fromCode('files', ValidationErrorCodes.MULTIPLE_FILES_FOR_INDEX_NOT_ALLOWED, {
					index,
				});
			}
		}
	}
	filesWithIndices.sort((left, right) => left.index - right.index);
	if (filesWithIndices.length > maxAttachments) {
		throw InputValidationError.fromCode('files', ValidationErrorCodes.TOO_MANY_FILES, {maxFiles: maxAttachments});
	}
	const attachmentMetadata = Array.isArray(data.attachments) ? (data.attachments as Array<AttachmentMetadata>) : [];
	const fileIds = new Set(filesWithIndices.map((file) => file.index));
	const inlineAttachmentMetadata = new Map<number, AttachmentMetadata>();
	const existingAttachments: Array<ClientAttachmentReferenceRequest> = [];
	for (const metadata of attachmentMetadata) {
		const matchingFileIndex = getMatchingMultipartFileIndex(metadata.id, fileIds);
		if (matchingFileIndex !== null) {
			if (inlineAttachmentMetadata.has(matchingFileIndex)) {
				throw InputValidationError.fromCode('attachments', ValidationErrorCodes.DUPLICATE_ATTACHMENT_IDS_NOT_ALLOWED);
			}
			inlineAttachmentMetadata.set(matchingFileIndex, metadata);
			continue;
		}
		if ('filename' in metadata && metadata.filename !== undefined) {
			throw InputValidationError.fromCode('attachments', ValidationErrorCodes.NO_FILE_FOR_ATTACHMENT_METADATA, {
				attachmentId: metadata.id,
			});
		}
		existingAttachments.push(metadata);
	}
	if (filesWithIndices.length > 0) {
		const clientIp = requireClientIp(ctx.req.raw, {
			trustClientIpHeader: Config.proxy.trust_client_ip_header,
			clientIpHeaderName: Config.proxy.client_ip_header,
		});
		const inlineNewAttachments = filesWithIndices.map(({file, index}) =>
			buildInlineAttachmentMetadata({
				file,
				index,
				metadata: inlineAttachmentMetadata.get(index),
			}),
		);
		const uploadedAttachments: Array<UploadedAttachment> = await ctx
			.get('channelService')
			.attachments.uploadFormDataAttachments({
				userId: user.id,
				channelId,
				clientIp,
				files: filesWithIndices,
				attachmentMetadata: inlineNewAttachments,
				expiresAt: options?.uploadExpiresAt,
			});
		const uploadedMap = new Map(uploadedAttachments.map((attachment) => [attachment.id, attachment]));
		const processedInlineAttachments = inlineNewAttachments.map((clientData) => {
			const uploaded = uploadedMap.get(clientData.id);
			if (!uploaded) {
				throw InputValidationError.fromCode('attachments', ValidationErrorCodes.NO_FILE_FOR_ATTACHMENT, {
					attachmentId: clientData.id,
				});
			}
			return mergeUploadWithClientData(uploaded, clientData);
		});
		data.attachments = [...existingAttachments, ...processedInlineAttachments];
	} else if (existingAttachments.length > 0) {
		data.attachments = existingAttachments;
	}
	return data as MessageRequest | MessageUpdateRequest;
}

function parseMultipartPayload(body: MultipartBody): unknown {
	const payloadJson = body['payload_json'];
	if (payloadJson === undefined) {
		return {};
	}
	if (typeof payloadJson !== 'string') {
		throw InputValidationError.fromCode('payload_json', ValidationErrorCodes.INVALID_JSON_IN_PAYLOAD_JSON);
	}
	try {
		return parseJsonPreservingLargeIntegers(payloadJson);
	} catch (_error) {
		throw InputValidationError.fromCode('payload_json', ValidationErrorCodes.INVALID_JSON_IN_PAYLOAD_JSON);
	}
}

function mergeMultipartScalarFields(body: MultipartBody, payload: unknown): unknown {
	if (!payload || typeof payload !== 'object' || Array.isArray(payload)) {
		return payload;
	}
	const mergedPayload = {...(payload as Record<string, unknown>)};
	for (const [key, value] of Object.entries(body)) {
		if (key === 'payload_json' || getMultipartFileFieldIndex(key, 0) !== null) {
			continue;
		}
		const values = getMultipartStringValues(value);
		if (values.length === 0) {
			continue;
		}
		switch (key) {
			case 'content':
			case 'nonce':
				mergedPayload[key] = values[values.length - 1];
				break;
			case 'tts':
				mergedPayload.tts = parseMultipartBoolean(values[values.length - 1]!);
				break;
			case 'flags':
				mergedPayload.flags = parseMultipartInteger(values[values.length - 1]!);
				break;
			case 'favorite_meme_id':
				mergedPayload.favorite_meme_id = values[values.length - 1];
				break;
			case 'sticker_ids':
				mergedPayload.sticker_ids = values;
				break;
			default:
				break;
		}
	}
	return mergedPayload;
}

function getMultipartFileFieldIndex(
	key: string,
	nextLegacyFileIndex: number,
): {index: number; isLegacy: boolean} | null {
	const compatMatch = FIELD_NAME_PATTERN.exec(key);
	if (compatMatch) {
		return {
			index: parseInt(compatMatch[1], 10),
			isLegacy: false,
		};
	}
	if (key.startsWith('files[')) {
		throw InputValidationError.fromCode('files', ValidationErrorCodes.INVALID_FILE_FIELD_NAME, {
			key,
		});
	}
	const legacyMatch = LEGACY_FILE_FIELD_NAME_PATTERN.exec(key);
	if (!legacyMatch) {
		return null;
	}
	return {
		index: legacyMatch[1] === undefined ? nextLegacyFileIndex : parseInt(legacyMatch[1], 10),
		isLegacy: true,
	};
}

function getMultipartStringValues(value: MultipartBody[string]): Array<string> {
	if (typeof value === 'string') {
		return [value];
	}
	if (value instanceof File) {
		return [];
	}
	return value.filter((entry): entry is string => typeof entry === 'string');
}

function parseMultipartBoolean(value: string): boolean | string {
	const normalized = value.trim().toLowerCase();
	if (normalized === 'true') {
		return true;
	}
	if (normalized === 'false') {
		return false;
	}
	return value;
}

function parseMultipartInteger(value: string): number | string {
	const trimmed = value.trim();
	if (!/^-?\d+$/.test(trimmed)) {
		return value;
	}
	const parsed = Number(trimmed);
	if (!Number.isSafeInteger(parsed)) {
		return value;
	}
	return parsed;
}

function getMatchingMultipartFileIndex(id: number | bigint, fileIds: Set<number>): number | null {
	if (typeof id === 'number') {
		return fileIds.has(id) ? id : null;
	}
	if (id < 0n || id > 10000n) {
		return null;
	}
	const numericId = Number(id);
	return fileIds.has(numericId) ? numericId : null;
}

function buildInlineAttachmentMetadata(params: {
	file: File;
	index: number;
	metadata?: AttachmentMetadata;
}): ClientAttachmentRequest {
	const {file, index, metadata} = params;
	return {
		id: index,
		filename: metadata && 'filename' in metadata && metadata.filename ? metadata.filename : file.name,
		content_type:
			metadata && 'content_type' in metadata ? (metadata as ClientUploadedAttachmentRequest).content_type : undefined,
		title: metadata?.title ?? null,
		description: metadata?.description ?? null,
		flags: metadata?.flags ?? 0,
		duration: metadata?.duration ?? null,
		waveform: metadata?.waveform ?? null,
	};
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
				const guild = await guildService.data.getGuildSystem(channel.guildId);
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
