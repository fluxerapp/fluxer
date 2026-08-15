// SPDX-License-Identifier: AGPL-3.0-or-later

import {FeatureTemporarilyDisabledModal} from '@app/features/app/components/alerts/FeatureTemporarilyDisabledModal';
import {Endpoints} from '@app/features/app/constants/Endpoints';
import * as DraftCommands from '@app/features/messaging/commands/DraftCommands';
import * as MessageCommands from '@app/features/messaging/commands/MessageCommands';
import {FileSizeTooLargeModal} from '@app/features/messaging/components/alerts/FileSizeTooLargeModal';
import {MessageSendFailedModal} from '@app/features/messaging/components/alerts/MessageSendFailedModal';
import {MessageSendTooQuickModal} from '@app/features/messaging/components/alerts/MessageSendTooQuickModal';
import type {
	ScheduledAttachment,
	ScheduledMessagePayload,
	ScheduledMessageResponse,
} from '@app/features/messaging/models/ScheduledMessage';
import {ScheduledMessage} from '@app/features/messaging/models/ScheduledMessage';
import ScheduledMessages from '@app/features/messaging/state/ScheduledMessages';
import {CloudUpload} from '@app/features/messaging/upload/CloudUpload';
import {prepareAttachmentsForNonce} from '@app/features/messaging/utils/MessageAttachmentUtils';
import {
	type ApiAttachmentMetadata,
	buildMessageCreateRequest,
	type MessageCreateRequest,
	type NormalizedMessageContent,
	normalizeMessageContent,
} from '@app/features/messaging/utils/MessageRequestUtils';
import * as MessageSubmitUtils from '@app/features/messaging/utils/MessageSubmitUtils';
import {MatureContentRejectedModal} from '@app/features/moderation/components/alerts/MatureContentRejectedModal';
import {http} from '@app/features/platform/transport/RestTransport';
import {HttpError} from '@app/features/platform/types/EndpointError';
import type {RestResponse} from '@app/features/platform/types/TransportTypes';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as SlowmodeCommands from '@app/features/slowmode/commands/SlowmodeCommands';
import {SlowmodeRateLimitedModal} from '@app/features/slowmode/components/alerts/SlowmodeRateLimitedModal';
import {TypingUtils} from '@app/features/typing/utils/TypingUtils';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import type {
	AllowedMentions,
	MessageReference,
	MessageStickerItem,
} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import * as SnowflakeUtils from '@fluxer/snowflake/src/SnowflakeUtils';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const SCHEDULED_MESSAGE_FOR_DESCRIPTOR = msg({
	message: 'Scheduled message for {scheduledTimeLabel}',
	comment: 'Label in the messaging commands. {scheduledTimeLabel} is the localized scheduled date and time.',
});
const UPDATED_SCHEDULED_MESSAGE_FOR_DESCRIPTOR = msg({
	message: 'Updated scheduled message for {scheduledTimeLabel}',
	comment: 'Label in the messaging commands. {scheduledTimeLabel} is the localized scheduled date and time.',
});
const REMOVED_SCHEDULED_MESSAGE_DESCRIPTOR = msg({
	message: 'Removed scheduled message',
	comment: 'Button or menu action label in the messaging commands. Keep it concise. Keep the tone plain and specific.',
});
const logger = new Logger('ScheduledMessages');

type ScheduledMessageRequest = MessageCreateRequest & {
	scheduled_local_at: string;
	timezone: string;
};

interface ApiErrorBody {
	code?: number | string;
	retry_after?: number;
	message?: string;
	details?: unknown;
}

export interface ScheduleMessageParams {
	channelId: string;
	content: string;
	scheduledLocalAt: string;
	timezone: string;
	messageReference?: MessageReference;
	replyMentioning?: boolean;
	favoriteMemeId?: string;
	stickers?: Array<MessageStickerItem>;
	tts?: boolean;
	hasAttachments: boolean;
}

interface UpdateScheduledMessageParams {
	channelId: string;
	scheduledMessageId: string;
	scheduledLocalAt: string;
	timezone: string;
	normalized: NormalizedMessageContent;
	payload: ScheduledMessagePayload;
	replyMentioning?: boolean;
}

interface PreparedScheduledMessage {
	nonce: string;
	payload: ScheduledMessageRequest;
	files?: Array<File>;
	normalized: NormalizedMessageContent;
}

const formatScheduledLabel = (local: string, timezone: string): string => {
	return `${local.replace('T', ' ')} (${timezone})`;
};

function mapScheduledAttachments(
	attachments?: ReadonlyArray<ScheduledAttachment>,
): Array<ApiAttachmentMetadata> | undefined {
	if (!attachments || attachments.length === 0) {
		return undefined;
	}
	return attachments.map((attachment) => ({
		id: attachment.id,
		filename: attachment.filename,
		title: attachment.title ?? attachment.filename,
		description: attachment.description ?? undefined,
		flags: attachment.flags,
	}));
}

function scheduleAllowedMentions(params: ScheduleMessageParams): AllowedMentions {
	return {replied_user: params.replyMentioning ?? true};
}

function claimScheduleAttachments(params: ScheduleMessageParams, nonce: string): void {
	if (!params.hasAttachments) {
		return;
	}
	MessageSubmitUtils.claimMessageAttachments(
		params.channelId,
		nonce,
		params.content,
		params.messageReference,
		params.replyMentioning,
		params.favoriteMemeId,
	);
}

async function prepareScheduledMessage(params: ScheduleMessageParams): Promise<PreparedScheduledMessage> {
	const nonce = SnowflakeUtils.fromTimestamp(Date.now());
	const normalized = normalizeMessageContent(params.content, params.favoriteMemeId);
	claimScheduleAttachments(params, nonce);
	let attachments: Array<ApiAttachmentMetadata> | undefined;
	let files: Array<File> | undefined;
	if (params.hasAttachments) {
		const result = await prepareAttachmentsForNonce(nonce, params.favoriteMemeId);
		attachments = result.attachments;
		files = result.files;
	}
	const requestBody = buildMessageCreateRequest({
		content: normalized.content,
		nonce,
		attachments,
		allowedMentions: scheduleAllowedMentions(params),
		messageReference: params.messageReference,
		flags: normalized.flags,
		favoriteMemeId: params.favoriteMemeId,
		stickers: params.stickers,
		tts: params.tts,
	});
	return {
		nonce,
		normalized,
		files,
		payload: {
			...requestBody,
			scheduled_local_at: params.scheduledLocalAt,
			timezone: params.timezone,
		},
	};
}

function handleScheduleSuccess(
	i18n: I18n,
	params: ScheduleMessageParams,
	nonce: string,
	response: ScheduledMessageResponse,
): ScheduledMessage {
	const record = ScheduledMessage.fromResponse(response);
	ScheduledMessages.upsert(record);
	DraftCommands.deleteDraft(params.channelId);
	TypingUtils.clear(params.channelId);
	MessageCommands.stopReply(params.channelId);
	if (params.hasAttachments) {
		CloudUpload.removeMessageUpload(nonce);
	}
	ToastCommands.createToast({
		type: 'success',
		children: i18n._(SCHEDULED_MESSAGE_FOR_DESCRIPTOR, {
			scheduledTimeLabel: formatScheduledLabel(params.scheduledLocalAt, params.timezone),
		}),
	});
	return record;
}

function scheduledMessageUpdateRequest(params: UpdateScheduledMessageParams): ScheduledMessageRequest {
	return {
		content: params.normalized.content,
		attachments: mapScheduledAttachments(params.payload.attachments),
		allowed_mentions: params.payload.allowed_mentions ?? (params.replyMentioning ? {replied_user: true} : undefined),
		message_reference:
			params.payload.message_reference?.channel_id && params.payload.message_reference.message_id
				? {
						channel_id: params.payload.message_reference.channel_id,
						message_id: params.payload.message_reference.message_id,
						guild_id: params.payload.message_reference.guild_id,
						type: params.payload.message_reference.type,
					}
				: undefined,
		flags: params.normalized.flags,
		favorite_meme_id: params.payload.favorite_meme_id ?? undefined,
		sticker_ids: params.payload.sticker_ids,
		tts: params.payload.tts ? true : undefined,
		scheduled_local_at: params.scheduledLocalAt,
		timezone: params.timezone,
	};
}

async function requestScheduledMessageUpdate(
	scheduledMessageId: string,
	requestBody: ScheduledMessageRequest,
): Promise<ScheduledMessageResponse> {
	const response = await http.patch<ScheduledMessageResponse>(Endpoints.USER_SCHEDULED_MESSAGE(scheduledMessageId), {
		body: requestBody,
	});
	return response.body;
}

export async function fetchScheduledMessages(): Promise<Array<ScheduledMessage>> {
	logger.debug('Fetching scheduled messages');
	ScheduledMessages.fetchStart();
	try {
		const response = await http.get<Array<ScheduledMessageResponse>>(Endpoints.USER_SCHEDULED_MESSAGES);
		const data = response.body ?? [];
		const messages = data.map(ScheduledMessage.fromResponse);
		ScheduledMessages.fetchSuccess(messages);
		logger.debug('Scheduled messages fetched successfully');
		return messages;
	} catch (error) {
		ScheduledMessages.fetchError();
		logger.error('Failed to fetch scheduled messages:', error);
		throw error;
	}
}

export async function scheduleMessage(i18n: I18n, params: ScheduleMessageParams): Promise<ScheduledMessage> {
	logger.debug('Scheduling message', params);
	const prepared = await prepareScheduledMessage(params);
	try {
		const response = await scheduleMessageRequest(params.channelId, prepared.payload, prepared.files, prepared.nonce);
		return handleScheduleSuccess(i18n, params, prepared.nonce, response.body);
	} catch (error) {
		handleScheduleError(
			i18n,
			error,
			params.channelId,
			prepared.nonce,
			params.content,
			params.messageReference,
			params.replyMentioning,
			params.hasAttachments,
		);
		throw error;
	}
}

export async function updateScheduledMessage(
	i18n: I18n,
	params: UpdateScheduledMessageParams,
): Promise<ScheduledMessage> {
	logger.debug('Updating scheduled message', params);
	try {
		const record = ScheduledMessage.fromResponse(
			await requestScheduledMessageUpdate(params.scheduledMessageId, scheduledMessageUpdateRequest(params)),
		);
		ScheduledMessages.upsert(record);
		DraftCommands.deleteDraft(params.channelId);
		TypingUtils.clear(params.channelId);
		MessageCommands.stopReply(params.channelId);
		ToastCommands.createToast({
			type: 'success',
			children: i18n._(UPDATED_SCHEDULED_MESSAGE_FOR_DESCRIPTOR, {
				scheduledTimeLabel: formatScheduledLabel(params.scheduledLocalAt, params.timezone),
			}),
		});
		return record;
	} catch (error) {
		logger.error('Failed to update scheduled message', error);
		throw error;
	}
}

export async function cancelScheduledMessage(i18n: I18n, scheduledMessageId: string): Promise<void> {
	logger.debug('Canceling scheduled message', scheduledMessageId);
	try {
		await http.delete(Endpoints.USER_SCHEDULED_MESSAGE(scheduledMessageId));
		ScheduledMessages.remove(scheduledMessageId);
		ToastCommands.createToast({
			type: 'success',
			children: i18n._(REMOVED_SCHEDULED_MESSAGE_DESCRIPTOR),
		});
	} catch (error) {
		logger.error('Failed to cancel scheduled message', error);
		throw error;
	}
}

function restoreDraftAfterScheduleFailure(
	channelId: string,
	nonce: string,
	content: string,
	messageReference?: MessageReference,
	replyMentioning?: boolean,
	hadAttachments?: boolean,
): void {
	if (hadAttachments) {
		CloudUpload.restoreAttachmentsToTextarea(nonce);
	}
	DraftCommands.createDraft(channelId, content);
	if (messageReference && replyMentioning !== undefined) {
		MessageCommands.startReply(channelId, messageReference.message_id, replyMentioning);
	}
}

async function scheduleMessageRequest(
	channelId: string,
	payload: ScheduledMessageRequest,
	files?: Array<File>,
	nonce?: string,
): Promise<RestResponse<ScheduledMessageResponse>> {
	const abortController = new AbortController();
	try {
		if (files?.length) {
			return await scheduleMultipartMessage(channelId, payload, files, abortController.signal, nonce);
		}
		return await http.post<ScheduledMessageResponse>(Endpoints.CHANNEL_MESSAGE_SCHEDULE(channelId), {
			body: payload,
			signal: abortController.signal,
		});
	} finally {
		abortController.abort();
	}
}

async function scheduleMultipartMessage(
	channelId: string,
	payload: ScheduledMessageRequest,
	files: Array<File>,
	signal: AbortSignal,
	nonce?: string,
): Promise<RestResponse<ScheduledMessageResponse>> {
	return http.post<ScheduledMessageResponse>(Endpoints.CHANNEL_MESSAGE_SCHEDULE(channelId), {
		multipart: {
			files: files.map((file, index) => ({name: `files[${index}]`, file, filename: file.name})),
			fields: {payload_json: JSON.stringify(payload)},
		},
		signal,
		onProgress: nonce
			? (event) => {
					if (event.lengthComputable && event.total > 0) {
						const progress = (event.loaded / event.total) * 100;
						CloudUpload.updateSendingProgress(nonce, progress);
					}
				}
			: undefined,
	});
}

const getApiErrorBody = (error: HttpError): ApiErrorBody | undefined => {
	return typeof error.body === 'object' && error.body !== null ? (error.body as ApiErrorBody) : undefined;
};

function parseNestedRetryAfterSeconds(body: ApiErrorBody | undefined): number | undefined {
	if (body === undefined || typeof body.details !== 'object' || body.details === null || Array.isArray(body.details)) {
		return undefined;
	}
	const details = body.details as Record<string, unknown>;
	const retry = details.retry;
	if (typeof retry !== 'object' || retry === null || Array.isArray(retry)) return undefined;
	const afterSeconds = (retry as Record<string, unknown>).after_seconds;
	if (typeof afterSeconds !== 'number' || !Number.isFinite(afterSeconds) || afterSeconds <= 0) return undefined;
	return afterSeconds;
}

function resolveRetryAfterSeconds(error: HttpError): number | undefined {
	const body = getApiErrorBody(error);
	const nestedRetryAfter = parseNestedRetryAfterSeconds(body);
	if (nestedRetryAfter !== undefined) return nestedRetryAfter;
	const bodyRetryAfter = body === undefined ? undefined : body.retry_after;
	if (typeof bodyRetryAfter === 'number' && Number.isFinite(bodyRetryAfter) && bodyRetryAfter > 0) {
		return bodyRetryAfter;
	}
	const responseHeaders: Record<string, string> | undefined = error.responseHeaders;
	const header = responseHeaders === undefined ? undefined : responseHeaders['retry-after'];
	if (header === undefined || header.trim() === '') return undefined;
	const numeric = Number(header);
	if (Number.isFinite(numeric) && numeric > 0) return numeric;
	const deadline = Date.parse(header);
	if (!Number.isFinite(deadline)) return undefined;
	const remainingSeconds = (deadline - Date.now()) / 1000;
	return remainingSeconds > 0 ? remainingSeconds : undefined;
}

function handleScheduleError(
	i18n: I18n,
	error: unknown,
	channelId: string,
	nonce: string,
	content: string,
	messageReference?: MessageReference,
	replyMentioning?: boolean,
	hadAttachments?: boolean,
): void {
	restoreDraftAfterScheduleFailure(channelId, nonce, content, messageReference, replyMentioning, hadAttachments);
	if (!(error instanceof HttpError)) {
		ModalCommands.push(
			modal(() => (
				<MessageSendFailedModal
					hasAttachments={hadAttachments}
					data-flx="messaging.scheduled-message-commands.handle-schedule-error.message-send-failed-modal--request"
				/>
			)),
		);
		return;
	}
	if (isRateLimitError(error)) {
		handleScheduleRateLimit(i18n, error);
		return;
	}
	if (isSlowmodeError(error)) {
		const retryAfterSeconds = resolveRetryAfterSeconds(error);
		const retryAfterMs = SlowmodeCommands.retryAfterSecondsToMs(retryAfterSeconds);
		if (retryAfterMs <= 0) {
			ModalCommands.push(
				modal(() => (
					<MessageSendFailedModal
						hasAttachments={hadAttachments}
						data-flx="messaging.scheduled-message-commands.handle-schedule-error.message-send-failed-modal"
					/>
				)),
			);
			return;
		}
		const retryAfter = Math.ceil(retryAfterMs / 1000);
		SlowmodeCommands.updateSlowmodeRemaining(channelId, retryAfterMs);
		ModalCommands.push(
			modal(() => (
				<SlowmodeRateLimitedModal
					retryAfter={retryAfter}
					data-flx="messaging.scheduled-message-commands.handle-schedule-error.slowmode-rate-limited-modal"
				/>
			)),
		);
		return;
	}
	if (isFeatureDisabledError(error)) {
		ModalCommands.push(
			modal(() => (
				<FeatureTemporarilyDisabledModal data-flx="messaging.scheduled-message-commands.handle-schedule-error.feature-temporarily-disabled-modal" />
			)),
		);
		return;
	}
	if (isExplicitContentError(error)) {
		ModalCommands.push(
			modal(() => (
				<MatureContentRejectedModal data-flx="messaging.scheduled-message-commands.handle-schedule-error.mature-content-rejected-modal" />
			)),
		);
		return;
	}
	if (isFileTooLargeError(error)) {
		ModalCommands.push(
			modal(() => (
				<FileSizeTooLargeModal data-flx="messaging.scheduled-message-commands.handle-schedule-error.file-size-too-large-modal" />
			)),
		);
		return;
	}
	ModalCommands.push(
		modal(() => (
			<MessageSendFailedModal
				hasAttachments={hadAttachments}
				data-flx="messaging.scheduled-message-commands.handle-schedule-error.message-send-failed-modal--http"
			/>
		)),
	);
}

function handleScheduleRateLimit(_i18n: I18n, error: HttpError): void {
	const retryAfterSecondsValue = resolveRetryAfterSeconds(error);
	const retryAfterSeconds = retryAfterSecondsValue === undefined ? undefined : Math.ceil(retryAfterSecondsValue);
	ModalCommands.push(
		modal(() => (
			<MessageSendTooQuickModal
				retryAfter={retryAfterSeconds}
				onRetry={undefined}
				data-flx="messaging.scheduled-message-commands.handle-schedule-rate-limit.message-send-too-quick-modal"
			/>
		)),
	);
	logger.warn('Scheduled message rate limited, retry after', retryAfterSeconds);
}

function isRateLimitError(error: HttpError): boolean {
	if (error.status !== 429) return false;
	return !isSlowmodeError(error);
}

function isSlowmodeError(error: HttpError): boolean {
	if (error.status !== 400 && error.status !== 429) return false;
	const body = getApiErrorBody(error);
	if (body === undefined) return false;
	return body.code === APIErrorCodes.SLOWMODE_RATE_LIMITED;
}

function isFeatureDisabledError(error: HttpError): boolean {
	return error?.status === 403 && getApiErrorBody(error)?.code === APIErrorCodes.FEATURE_TEMPORARILY_DISABLED;
}

function isExplicitContentError(error: HttpError): boolean {
	return getApiErrorBody(error)?.code === APIErrorCodes.EXPLICIT_CONTENT_CANNOT_BE_SENT;
}

function isFileTooLargeError(error: HttpError): boolean {
	return getApiErrorBody(error)?.code === APIErrorCodes.FILE_SIZE_TOO_LARGE;
}
