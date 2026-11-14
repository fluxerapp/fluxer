// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID} from '@app/api/BrandedTypes';
import {
	type AttachmentRequestData,
	mergeUploadWithClientData,
	type UploadedAttachment,
} from '@app/api/channel/AttachmentDTOs';
import type {ChannelRepository} from '@app/api/channel/ChannelRepository';
import type {MessageRequest, MessageUpdateRequest} from '@app/api/channel/MessageTypes';
import type {FormDataUpload} from '@app/api/channel/services/AttachmentUploadService';
import {normalizeMessageRequestPayload} from '@app/api/channel/services/message/MessageRequestCompatibility';
import {type MultipartFilePart, readMultipartStream} from '@app/api/channel/services/message/MultipartStreamReader';
import {SYSTEM_THREAD_VIEWER, viewerFromCtx} from '@app/api/experiment/ChannelThreadsGate';
import type {GuildService} from '@app/api/guild/services/GuildService';
import type {LimitConfigService} from '@app/api/limits/LimitConfigService';
import {resolveLimitSafe} from '@app/api/limits/LimitConfigUtils';
import {createLimitMatchContext} from '@app/api/limits/LimitMatchContextBuilder';
import {DEFAULT_REQUEST_BODY_LIMIT, isMultipartFormData} from '@app/api/middleware/RequestBodyLimitMiddleware';
import type {User} from '@app/api/models/User';
import type {HonoEnv} from '@app/api/types/HonoEnv';
import {parseJsonPreservingLargeIntegers} from '@app/api/utils/LosslessJsonParser';
import {requireRequestClientIp} from '@app/api/utils/RequestClientIp';
import {inputValidationErrorFromZodIssues} from '@app/api/Validator';
import {MAX_ATTACHMENTS_PER_MESSAGE} from '@fluxer/constants/src/LimitConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import type {
	ClientAttachmentReferenceRequest,
	ClientAttachmentRequest,
	ClientUploadedAttachmentRequest,
} from '@fluxer/schema/src/domains/message/AttachmentSchemas';
import type {Context} from 'hono';
import type {z} from 'zod';

const FIELD_NAME_PATTERN = /^files\[(\d+)\]$/;
const LEGACY_FILE_FIELD_NAME_PATTERN = /^file(\d+)?$/;

type MultipartBody = Record<string, string>;
type AttachmentMetadata = ClientAttachmentRequest | ClientUploadedAttachmentRequest | ClientAttachmentReferenceRequest;

interface ParseMultipartMessageDataOptions {
	onPayloadParsed?: (payload: unknown) => void;
	actor?: 'member' | 'webhook';
}

export async function parseMultipartMessageData(
	ctx: Context<HonoEnv>,
	user: User,
	channelId: ChannelID,
	schema: z.ZodType<MessageRequest | MessageUpdateRequest>,
	options?: ParseMultipartMessageDataOptions,
): Promise<MessageRequest | MessageUpdateRequest> {
	const attachmentService = ctx.get('channelService').attachments;
	const maxAttachments = await resolveMessageAttachmentLimit(ctx, user, channelId);
	const upload = await attachmentService.prepareFormDataUpload({
		userId: user.id,
		viewer: options?.actor === 'webhook' ? SYSTEM_THREAD_VIEWER : viewerFromCtx(ctx),
		channelId,
		actor: options?.actor,
	});
	const sender = options?.actor === 'webhook' ? `webhook:${ctx.req.param('webhook_id')}` : `user:${user.id.toString()}`;
	const body: MultipartBody = {};
	const storedFiles: Array<StoredMultipartFile> = [];
	try {
		const data = await readMultipartMessageBody(ctx, {
			body,
			storedFiles,
			maxAttachments,
			upload,
			storeFile: (stream, filename) =>
				attachmentService.storeFormDataFile({
					body: stream,
					filename,
					holder: {sender, clientIp: requireRequestClientIp(ctx)},
				}),
		});
		const mergedJsonData = normalizeMessageRequestPayload(mergeMultipartScalarFields(body, data));
		options?.onPayloadParsed?.(mergedJsonData);
		const validationResult = schema.safeParse(mergedJsonData);
		if (!validationResult.success) {
			throw inputValidationErrorFromZodIssues(validationResult.error.issues);
		}
		return await attachStoredFiles(ctx, user, channelId, validationResult.data, storedFiles);
	} catch (error) {
		await attachmentService.deleteFormDataFiles(storedFiles.map((file) => file.uploadKey));
		throw error;
	}
}

interface StoredMultipartFile {
	index: number;
	filename: string;
	uploadKey: string;
	size: number;
}

async function readMultipartMessageBody(
	ctx: Context<HonoEnv>,
	params: {
		body: MultipartBody;
		storedFiles: Array<StoredMultipartFile>;
		maxAttachments: number;
		upload: FormDataUpload;
		storeFile: (stream: MultipartFilePart['stream'], filename: string) => Promise<string>;
	},
): Promise<unknown> {
	const {body, storedFiles, maxAttachments, upload, storeFile} = params;
	const maxIndexLabel = maxAttachments > 0 ? maxAttachments - 1 : 0;
	const seenIndices = new Set<number>();
	let nextLegacyFileIndex = 0;
	let canAttachFiles = false;
	let payload: unknown = {};
	await readMultipartStream(ctx, {
		maxFiles: maxAttachments,
		maxFileSize: upload.maxFileSize,
		maxFieldBytes: DEFAULT_REQUEST_BODY_LIMIT,
		onField(name, value) {
			if (getMultipartFileFieldIndex(name, 0) !== null) {
				return;
			}
			body[name] = value;
			if (name === 'payload_json') {
				payload = parseMultipartPayload(value);
			}
		},
		async onFile({fieldName, filename, stream}) {
			const fileIndex = getMultipartFileFieldIndex(fieldName, nextLegacyFileIndex);
			if (fileIndex === null) {
				stream.resume();
				return;
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
			seenIndices.add(index);
			if (!canAttachFiles) {
				await upload.assertCanAttachFiles();
				canAttachFiles = true;
			}
			const uploadKey = await storeFile(stream, filename);
			storedFiles.push({index, filename, uploadKey, size: stream.bytesRead});
		},
	});
	return payload;
}

async function attachStoredFiles(
	ctx: Context<HonoEnv>,
	user: User,
	channelId: ChannelID,
	validated: MessageRequest | MessageUpdateRequest,
	storedFiles: Array<StoredMultipartFile>,
): Promise<MessageRequest | MessageUpdateRequest> {
	const data = validated as Partial<MessageRequest> &
		Partial<MessageUpdateRequest> & {
			attachments?: Array<AttachmentRequestData>;
		};
	storedFiles.sort((left, right) => left.index - right.index);
	const attachmentMetadata = Array.isArray(data.attachments) ? (data.attachments as Array<AttachmentMetadata>) : [];
	const fileIds = new Set(storedFiles.map((file) => file.index));
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
	if (storedFiles.length > 0) {
		const clientIp = requireRequestClientIp(ctx);
		const inlineNewAttachments = storedFiles.map(({filename, index}) =>
			buildInlineAttachmentMetadata({
				filename,
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
				files: storedFiles,
				attachmentMetadata: inlineNewAttachments,
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

function parseMultipartPayload(payloadJson: string): unknown {
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
		switch (key) {
			case 'content':
			case 'nonce':
				mergedPayload[key] = value;
				break;
			case 'tts':
				mergedPayload.tts = parseMultipartBoolean(value);
				break;
			case 'flags':
				mergedPayload.flags = parseMultipartInteger(value);
				break;
			case 'favorite_meme_id':
				mergedPayload.favorite_meme_id = value;
				break;
			case 'sticker_ids':
				mergedPayload.sticker_ids = [value];
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
	filename: string;
	index: number;
	metadata?: AttachmentMetadata;
}): ClientAttachmentRequest {
	const {filename, index, metadata} = params;
	return {
		id: index,
		filename: metadata && 'filename' in metadata && metadata.filename ? metadata.filename : filename,
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
	const channelRepository = ctx.get('channelRepository') as ChannelRepository | undefined;
	const guildService = ctx.get('guildService') as GuildService | undefined;
	if (channelRepository) {
		try {
			const channel = await channelRepository.channelData.findUnique(channelId);
			if (channel?.guildId && guildService) {
				const guild = await guildService.data.operations.getGuildSystem(channel.guildId);
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

export async function readMessageJsonBody(ctx: Context<HonoEnv>): Promise<unknown> {
	try {
		const raw = await ctx.req.text();
		return raw.trim().length === 0 ? {} : parseJsonPreservingLargeIntegers(raw);
	} catch {
		throw InputValidationError.fromCode('message_data', ValidationErrorCodes.INVALID_MESSAGE_DATA);
	}
}

export async function parseMessageRequestBody<T extends z.ZodType<MessageRequest | MessageUpdateRequest>>(
	ctx: Context<HonoEnv>,
	user: User,
	channelId: ChannelID,
	schema: T,
): Promise<z.output<T>> {
	if (isMultipartFormData(ctx.req.header('content-type')))
		return (await parseMultipartMessageData(ctx, user, channelId, schema)) as z.output<T>;
	const result = schema.safeParse(normalizeMessageRequestPayload(await readMessageJsonBody(ctx)));
	if (!result.success) throw inputValidationErrorFromZodIssues(result.error.issues);
	return result.data;
}
