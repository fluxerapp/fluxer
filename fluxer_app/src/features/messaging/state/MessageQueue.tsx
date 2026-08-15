// SPDX-License-Identifier: AGPL-3.0-or-later

import i18n from '@app/app/I18n';
import {FeatureTemporarilyDisabledModal} from '@app/features/app/components/alerts/FeatureTemporarilyDisabledModal';
import {Endpoints} from '@app/features/app/constants/Endpoints';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';
import {createSystemMessage} from '@app/features/devtools/utils/CommandUtils';
import * as DraftCommands from '@app/features/messaging/commands/DraftCommands';
import * as MessageCommands from '@app/features/messaging/commands/MessageCommands';
import {AttachmentUploadConnectivityModal} from '@app/features/messaging/components/alerts/AttachmentUploadConnectivityModal';
import {FileSizeTooLargeModal} from '@app/features/messaging/components/alerts/FileSizeTooLargeModal';
import {MessageEditFailedModal} from '@app/features/messaging/components/alerts/MessageEditFailedModal';
import {MessageEditTooQuickModal} from '@app/features/messaging/components/alerts/MessageEditTooQuickModal';
import {MessageSendFailedModal} from '@app/features/messaging/components/alerts/MessageSendFailedModal';
import {MessageSendTooQuickModal} from '@app/features/messaging/components/alerts/MessageSendTooQuickModal';
import {
	type MessageLocalSendRateLimitState,
	type MessageQueueRequestOutcomeStatus,
	resolveMessageLocalSendRateLimitDecision,
	resolveMessageQueuePayloadRouteDecision,
	resolveMessageQueueRequestOutcomeDecision,
	resolveMessageQueueSendExecutionDecision,
} from '@app/features/messaging/state/MessageQueueStateMachine';
import {
	type ChunkedUploadPart,
	type ChunkedUploadPlan,
	uploadFileInChunks,
} from '@app/features/messaging/upload/ChunkedAttachmentUploader';
import {type CloudAttachment, CloudUpload} from '@app/features/messaging/upload/CloudUpload';
import {exceedsMultipartFallbackRequestSize} from '@app/features/messaging/utils/AttachmentUploadFallbackUtils';
import {prepareAttachmentsForNonce} from '@app/features/messaging/utils/MessageAttachmentUtils';
import {
	type ApiAttachmentMetadata,
	type ApiMessageEditAttachmentMetadata,
	buildMessageCreateRequest,
	type MessageCreateRequest,
	type MessageEditRequest,
	normalizeMessageEditContent,
} from '@app/features/messaging/utils/MessageRequestUtils';
import {MatureContentRejectedModal} from '@app/features/moderation/components/alerts/MatureContentRejectedModal';
import {http} from '@app/features/platform/transport/RestTransport';
import {HttpError} from '@app/features/platform/types/EndpointError';
import type {RestResponse} from '@app/features/platform/types/TransportTypes';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as SlowmodeCommands from '@app/features/slowmode/commands/SlowmodeCommands';
import {SlowmodeRateLimitedModal} from '@app/features/slowmode/components/alerts/SlowmodeRateLimitedModal';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {formatUserSettingsPath} from '@app/features/user/components/settings_utils/SettingsConstants';
import PrivacyPreferences from '@app/features/user/state/PrivacyPreferences';
import {Queue, type QueueEntry} from '@app/lib/list/ListQueue';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import type {
	AllowedMentions,
	Message,
	MessageReference,
	MessageStickerItem,
} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {reaction} from 'mobx';

const YOUR_MESSAGE_COULD_NOT_BE_DELIVERED_THIS_IS_DESCRIPTOR = msg({
	message:
		"Your message could not be delivered. This is usually because you don't share a community with the recipient or the recipient is only accepting direct messages from friends. You may also need to adjust your own direct message privacy settings in {directMessagePrivacySettingsPath}.",
	comment: 'Label in the message queue state.',
});
const YOUR_MESSAGE_COULD_NOT_BE_DELIVERED_YOU_NEED_DESCRIPTOR = msg({
	message: 'Your message could not be delivered. You need to claim your account to send direct messages.',
	comment: 'Description text in the message queue state.',
});
const YOUR_MESSAGE_COULD_NOT_BE_DELIVERED_YOU_NEED_2_DESCRIPTOR = msg({
	message: 'Your message could not be delivered. You need to claim your account to send messages.',
	comment: 'Description text in the message queue state.',
});
const YOUR_MESSAGE_COULD_NOT_BE_DELIVERED_BECAUSE_IT_DESCRIPTOR = msg({
	message:
		'Your message could not be delivered because it was flagged by our safety systems. If you believe this is a mistake, please contact support.',
	comment: 'Label in the message queue state.',
});
const YOUR_MESSAGE_COULD_NOT_BE_DELIVERED_BECAUSE_IT_2_DESCRIPTOR = msg({
	message:
		'Your message could not be delivered because it contains mature emoji or stickers that are not allowed in this context.',
	comment: 'Label in the message queue state.',
});
const logger = new Logger('MessageQueue');
const DEFAULT_MAX_SIZE = 5;
const DEV_MESSAGE_DELAY = 3000;
const LOCAL_SEND_RATE_LIMIT_MAX_SENDS = 5;
const LOCAL_SEND_RATE_LIMIT_WINDOW_MS = 2000;
const LOCAL_SEND_RATE_LIMIT_BLOCK_MS = 3000;
const LOCAL_SEND_RATE_LIMIT_TRACKED_CHANNELS_MAX = 128;
const LOCAL_SEND_RESERVATION_TTL_MS = 30 * 1000;
const LOCAL_SEND_RESERVATIONS_MAX = 128;
const LOCAL_SEND_RATE_LIMIT_MODAL_KEY_PREFIX = 'message-local-send-rate-limit';
const TEXTAREA_ATTACHMENT_UPLOAD_CACHE_TTL_MS = 5 * 60 * 1000;
const MESSAGE_SEND_RATE_LIMIT_MAX_AUTOMATIC_RETRIES = 2;
const MESSAGE_SEND_RATE_LIMIT_MAX_AUTOMATIC_DELAY_MS = 30 * 1000;

interface BaseMessagePayload {
	channelId: string;
}

interface SendMessagePayload extends BaseMessagePayload {
	type: 'send';
	nonce: string;
	rateLimitRetryCount?: number;
	content: string;
	hasAttachments?: boolean;
	preparedAttachments?: Array<ApiAttachmentMetadata>;
	preparedFiles?: Array<File>;
	allowedMentions?: AllowedMentions;
	messageReference?: MessageReference;
	flags?: number;
	favoriteMemeId?: string;
	stickers?: Array<MessageStickerItem>;
	tts?: boolean;
}

interface EditMessagePayload extends BaseMessagePayload {
	type: 'edit';
	messageId: string;
	rateLimitRetryCount?: number;
	content?: string;
	allowedMentions?: AllowedMentions;
	flags?: number;
	attachments?: Array<ApiMessageEditAttachmentMetadata>;
}

export type MessageQueuePayload = SendMessagePayload | EditMessagePayload;
type MessageQueueCompletion<TResult> = {
	retry: RetryError | null;
	result?: TResult;
	error?: unknown;
};

export interface RetryError {
	retryAfter?: number;
}

export interface ApiErrorBody {
	code?: number | string;
	retry_after?: number;
	message?: string;
	details?: unknown;
}

interface PresignedAttachmentUploadSinglepartResponse {
	upload_mode: 'singlepart';
	id: string | number;
	filename: string;
	upload_filename: string;
	upload_url: string;
	file_size: number;
	content_type: string;
}

interface PresignedAttachmentUploadMultipartResponse {
	upload_mode: 'multipart';
	id: string | number;
	filename: string;
	upload_filename: string;
	file_size: number;
	content_type: string;
	upload_id: string;
	part_size: number;
	parts: Array<{part_number: number; upload_url: string}>;
}

type PresignedAttachmentUploadResponseAttachment =
	| PresignedAttachmentUploadSinglepartResponse
	| PresignedAttachmentUploadMultipartResponse;

interface PresignedAttachmentUploadRequestFile {
	id: string;
	filename: string;
	file_size: number;
	content_type: string;
}

interface PresignedAttachmentUploadRequestBody {
	attachments: Array<PresignedAttachmentUploadRequestFile>;
}

interface PresignedAttachmentUploadResponseBody {
	attachments: Array<PresignedAttachmentUploadResponseAttachment>;
}

interface CompleteMultipartAttachmentUploadRequestBody {
	uploads: Array<{upload_filename: string; upload_id: string}>;
}

interface CompleteMultipartAttachmentUploadResponseBody {
	uploads: Array<{upload_filename: string}>;
}

export interface PreparedSendAttachments {
	attachments?: Array<ApiAttachmentMetadata>;
	files?: Array<File>;
}

interface TextareaAttachmentUploadResult {
	uploadFilename: string;
	fileSize: number;
	contentType: string;
}

interface TextareaAttachmentUpload {
	channelId: string;
	attachmentId: number;
	startedAt: number;
	requestAbortController: AbortController;
	abortController: AbortController;
	promise: Promise<TextareaAttachmentUploadResult>;
	settled: boolean;
}

function createAbortError(): DOMException {
	return new DOMException('Upload aborted', 'AbortError');
}

const getApiErrorBody = (error: HttpError): ApiErrorBody | undefined => {
	return typeof error.body === 'object' && error.body !== null ? (error.body as ApiErrorBody) : undefined;
};

interface MessageRateLimitRetry {
	retryAfterSeconds: number | null;
	automaticRetryDelayMs: number | null;
}

function parsePositiveRetryAfterSeconds(value: unknown): number | null {
	if (typeof value !== 'number' || !Number.isFinite(value) || value <= 0) return null;
	return value;
}

function parseRetryAfterHeaderSeconds(value: string | undefined): number | null {
	if (value === undefined || value.trim() === '') return null;
	const numeric = Number(value);
	if (Number.isFinite(numeric) && numeric > 0) return numeric;
	const deadline = Date.parse(value);
	if (!Number.isFinite(deadline)) return null;
	const remainingSeconds = (deadline - Date.now()) / 1000;
	return remainingSeconds > 0 ? remainingSeconds : null;
}

function parseNestedRetryAfterSeconds(body: ApiErrorBody | undefined): number | null {
	if (body === undefined || typeof body.details !== 'object' || body.details === null || Array.isArray(body.details)) {
		return null;
	}
	const details = body.details as Record<string, unknown>;
	const retry = details.retry;
	if (typeof retry !== 'object' || retry === null || Array.isArray(retry)) return null;
	const afterSeconds = (retry as Record<string, unknown>).after_seconds;
	if (typeof afterSeconds !== 'number' || !Number.isFinite(afterSeconds) || afterSeconds <= 0) return null;
	return afterSeconds;
}

function readRateLimitHeader(error: HttpError, name: string): string | undefined {
	return error.responseHeaders[name.toLowerCase()];
}

function resolveMessageRateLimitRetry(error: HttpError): MessageRateLimitRetry {
	const body = getApiErrorBody(error);
	const candidates: Array<number> = [];
	const nestedRetryAfter = parseNestedRetryAfterSeconds(body);
	if (nestedRetryAfter !== null) candidates.push(nestedRetryAfter);
	const bodyRetryAfter = body === undefined ? undefined : body.retry_after;
	const parsedBodyRetryAfter = parsePositiveRetryAfterSeconds(bodyRetryAfter);
	if (parsedBodyRetryAfter !== null) candidates.push(parsedBodyRetryAfter);
	const parsedHeaderRetryAfter = parseRetryAfterHeaderSeconds(readRateLimitHeader(error, 'retry-after'));
	if (parsedHeaderRetryAfter !== null) candidates.push(parsedHeaderRetryAfter);
	const resetAfterHeader = readRateLimitHeader(error, 'x-ratelimit-reset-after');
	if (resetAfterHeader !== undefined) {
		const parsedResetAfter = parsePositiveRetryAfterSeconds(Number(resetAfterHeader));
		if (parsedResetAfter !== null) candidates.push(parsedResetAfter);
	}
	if (candidates.length === 0) {
		return {retryAfterSeconds: null, automaticRetryDelayMs: null};
	}
	const rawRetryAfter = Math.max(...candidates);
	const retryAfterSeconds = Math.ceil(rawRetryAfter);
	const retryAfterMs = Math.ceil(rawRetryAfter * 1000);
	if (!Number.isSafeInteger(retryAfterSeconds) || !Number.isSafeInteger(retryAfterMs)) {
		return {retryAfterSeconds: null, automaticRetryDelayMs: null};
	}
	let automaticRetryDelayMs: number | null = null;
	if (retryAfterMs <= MESSAGE_SEND_RATE_LIMIT_MAX_AUTOMATIC_DELAY_MS) {
		automaticRetryDelayMs = retryAfterMs;
	}
	return {retryAfterSeconds, automaticRetryDelayMs};
}
const isAbortError = (error: unknown): boolean => {
	return error instanceof DOMException && error.name === 'AbortError';
};
const isTimeoutError = (error: unknown): boolean => {
	return error instanceof DOMException && error.name === 'TimeoutError';
};
const isNetworkRequestError = (error: unknown): boolean => {
	return error instanceof Error && error.message === 'Network error during request';
};

function isSendPayload(payload: MessageQueuePayload): payload is SendMessagePayload {
	return payload.type === 'send';
}

function isRateLimitError(error: HttpError): boolean {
	if (error.status !== 429) return false;
	return !isSlowmodeError(error);
}

function getRequestErrorOutcomeStatus(error: HttpError): MessageQueueRequestOutcomeStatus {
	if (isRateLimitError(error)) return 'rateLimit';
	return 'failure';
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

function isDMRestrictedError(error: HttpError): boolean {
	return getApiErrorBody(error)?.code === APIErrorCodes.CANNOT_SEND_MESSAGES_TO_USER;
}

function getUnclaimedAccountErrorCode(error: HttpError): string | undefined {
	const code = getApiErrorBody(error)?.code;
	if (
		code === APIErrorCodes.UNCLAIMED_ACCOUNT_CANNOT_SEND_MESSAGES ||
		code === APIErrorCodes.UNCLAIMED_ACCOUNT_CANNOT_SEND_DIRECT_MESSAGES
	) {
		return code;
	}
	return undefined;
}

function isPresignedUploadEndpointUnreachable(error: unknown): boolean {
	if (isTimeoutError(error)) {
		return true;
	}
	if (error instanceof HttpError) {
		return typeof error.status === 'number' && error.status >= 500;
	}
	return isNetworkRequestError(error);
}

class PresignedUploadFallbackUnavailableError extends Error {
	constructor() {
		super('Presigned attachment upload request failed and multipart fallback is unavailable');
		this.name = 'PresignedUploadFallbackUnavailableError';
	}
}

export class MessageQueue extends Queue<MessageQueuePayload, RestResponse<Message> | undefined> {
	private readonly maxSize: number;
	private readonly abortControllers = new Map<string, AbortController>();
	private readonly textareaAttachmentUploads = new Map<number, TextareaAttachmentUpload>();
	private readonly textareaAttachmentUploadDisposers = new Map<string, () => void>();
	private readonly localSendLimiters = new Map<string, MessageLocalSendRateLimitState>();
	private readonly localSendReservations = new Map<string, number>();

	constructor(maxSize = DEFAULT_MAX_SIZE) {
		super({logger, defaultRetryAfter: 100});
		this.maxSize = maxSize;
		reaction(
			() => PrivacyPreferences.getPreuploadMessageAttachments(),
			(enabled) => {
				if (!enabled) {
					this.cancelAllTextareaAttachmentUploads();
				}
			},
		);
	}

	isFull(): boolean {
		return this.queueLength >= this.maxSize;
	}

	override enqueue(
		message: MessageQueuePayload,
		success: (result?: RestResponse<Message>, error?: unknown) => void,
	): void {
		if (!this.isFull()) {
			super.enqueue(message, success);
			return;
		}
		const error = new Error(`Message queue capacity of ${this.maxSize} entries was reached`);
		logger.error('Rejected message queue entry because capacity was reached', error);
		if (message.type === 'send') {
			this.handleSendError(message.channelId, message.nonce, error, i18n, message.hasAttachments);
		} else {
			ModalCommands.push(
				modal(() => <MessageEditFailedModal data-flx="messaging.message-queue.message-edit-failed-modal--capacity" />),
			);
		}
		try {
			success(undefined, error);
		} catch (callbackError) {
			logger.error('Message queue rejection callback failed', callbackError);
		}
	}

	reserveLocalSend(channelId: string, nonce: string): boolean {
		const reservationKey = this.getLocalSendReservationKey(channelId, nonce);
		const now = Date.now();
		this.pruneExpiredLocalSendReservations(now);
		const existingExpiry = this.localSendReservations.get(reservationKey);
		if (existingExpiry !== undefined) {
			if (existingExpiry > now) return true;
			this.localSendReservations.delete(reservationKey);
		}
		if (!this.consumeLocalSendAllowance(channelId)) {
			return false;
		}
		while (this.localSendReservations.size >= LOCAL_SEND_RESERVATIONS_MAX) {
			const oldestReservationKey = this.localSendReservations.keys().next().value;
			if (oldestReservationKey === undefined) break;
			this.localSendReservations.delete(oldestReservationKey);
		}
		this.localSendReservations.set(reservationKey, now + LOCAL_SEND_RESERVATION_TTL_MS);
		return true;
	}

	consumeLocalSendReservation(channelId: string, nonce: string): boolean {
		const reservationKey = this.getLocalSendReservationKey(channelId, nonce);
		const expiresAt = this.localSendReservations.get(reservationKey);
		if (expiresAt !== undefined) {
			this.localSendReservations.delete(reservationKey);
			if (expiresAt > Date.now()) return true;
		}
		return this.consumeLocalSendAllowance(channelId);
	}

	rejectLocalRateLimitedSend(channelId: string, nonce: string, hasAttachments?: boolean): void {
		MessageCommands.sendError(channelId, nonce);
		if (hasAttachments) {
			this.restoreFailedMessage(channelId, nonce);
		}
	}

	drain(
		message: MessageQueuePayload,
		completed: (err: RetryError | null, result?: RestResponse<Message>, error?: unknown) => void,
	): Promise<unknown> | undefined {
		const route = resolveMessageQueuePayloadRouteDecision({
			payloadType: (message as {type?: string}).type,
		});
		switch (route.type) {
			case 'send':
				return this.handleSend(message as SendMessagePayload, completed);
			case 'edit':
				return this.handleEdit(message as EditMessagePayload, completed);
			case 'unknown':
				logger.error('Unknown message type, completing with null');
				completed(null, undefined, new Error('Unknown message queue payload'));
				return undefined;
		}
	}

	private consumeLocalSendAllowance(channelId: string): boolean {
		const now = Date.now();
		const previous = this.localSendLimiters.get(channelId);
		let windowStartedAt: number | null = null;
		let sentCount = 0;
		let blockedUntil: number | null = null;
		if (previous !== undefined) {
			windowStartedAt = previous.windowStartedAt;
			sentCount = previous.sentCount;
			blockedUntil = previous.blockedUntil;
		}
		const decision = resolveMessageLocalSendRateLimitDecision({
			windowStartedAt,
			sentCount,
			blockedUntil,
			now,
			maxSends: LOCAL_SEND_RATE_LIMIT_MAX_SENDS,
			windowMs: LOCAL_SEND_RATE_LIMIT_WINDOW_MS,
			blockMs: LOCAL_SEND_RATE_LIMIT_BLOCK_MS,
		});
		this.localSendLimiters.delete(channelId);
		if (this.localSendLimiters.size >= LOCAL_SEND_RATE_LIMIT_TRACKED_CHANNELS_MAX) {
			this.removeExpiredLocalSendLimiters(now);
		}
		while (this.localSendLimiters.size >= LOCAL_SEND_RATE_LIMIT_TRACKED_CHANNELS_MAX) {
			const oldestChannelId = this.localSendLimiters.keys().next().value;
			if (oldestChannelId === undefined) break;
			this.localSendLimiters.delete(oldestChannelId);
		}
		this.localSendLimiters.set(channelId, decision.next);
		switch (decision.type) {
			case 'allow':
				return true;
			case 'block':
				this.showLocalSendRateLimitModal(channelId, decision.retryAfterMs);
				return false;
		}
	}

	private showLocalSendRateLimitModal(channelId: string, retryAfterMs: number): void {
		const key = this.getLocalSendRateLimitModalKey(channelId);
		const retryAfter = this.getLocalSendRateLimitRetryAfterSeconds(retryAfterMs);
		ModalCommands.pushWithKey(
			modal(() => (
				<MessageSendTooQuickModal
					retryAfter={retryAfter}
					data-flx="messaging.message-queue.local-message-send-too-quick-modal"
				/>
			)),
			key,
		);
	}

	private getLocalSendRateLimitModalKey(channelId: string): string {
		return `${LOCAL_SEND_RATE_LIMIT_MODAL_KEY_PREFIX}:${channelId}`;
	}

	private getLocalSendRateLimitRetryAfterSeconds(retryAfterMs: number): number {
		return Math.max(1, Math.ceil(retryAfterMs / 1000));
	}

	private getLocalSendReservationKey(channelId: string, nonce: string): string {
		return `${channelId}:${nonce}`;
	}

	private removeExpiredLocalSendLimiters(now: number): void {
		for (const [channelId, state] of this.localSendLimiters) {
			const windowExpired =
				state.windowStartedAt === null || now - state.windowStartedAt >= LOCAL_SEND_RATE_LIMIT_WINDOW_MS;
			const blockExpired = state.blockedUntil === null || now >= state.blockedUntil;
			if (windowExpired && blockExpired) this.localSendLimiters.delete(channelId);
		}
	}

	private pruneExpiredLocalSendReservations(now: number): void {
		for (const [reservationKey, expiresAt] of this.localSendReservations) {
			if (expiresAt <= now) this.localSendReservations.delete(reservationKey);
		}
	}

	cancelRequest(nonce: string): void {
		logger.info('Cancel message send:', nonce);
		const messageUpload = CloudUpload.getMessageUpload(nonce);
		if (messageUpload) {
			this.cancelTextareaAttachmentUploads(messageUpload.attachments.map((attachment) => attachment.id));
		}
		const controller = this.abortControllers.get(nonce);
		controller?.abort();
		this.abortControllers.delete(nonce);
	}

	cancelPendingSendRequests(channelId: string): Array<SendMessagePayload> {
		const cancelled: Array<SendMessagePayload> = [];
		const remaining: Array<QueueEntry<MessageQueuePayload, RestResponse<Message> | undefined>> = [];
		while (this.queue.length > 0) {
			const entry = this.queue.shift()!;
			if (isSendPayload(entry.message) && entry.message.channelId === channelId) {
				cancelled.push(entry.message);
				this.cancelRequest(entry.message.nonce);
			} else {
				remaining.push(entry);
			}
		}
		this.queue.push(...remaining);
		logger.info('Cancel pending send requests', cancelled.length);
		return cancelled;
	}

	async sendImmediately(payload: SendMessagePayload): Promise<RestResponse<Message> | undefined> {
		while (true) {
			const completion = await this.drainSendImmediately(payload);
			if (completion.retry === null) {
				return completion.result;
			}
			const retryAfter = completion.retry.retryAfter;
			const delay = retryAfter === undefined ? this.defaultRetryAfter : retryAfter;
			logger.info(`Pausing immediate send retry for ${delay}ms due to retry request`);
			await new Promise<void>((resolve) => window.setTimeout(resolve, delay));
		}
	}

	private drainSendImmediately(payload: SendMessagePayload): Promise<MessageQueueCompletion<RestResponse<Message>>> {
		return new Promise((resolve) => {
			let hasCompleted = false;
			const complete = (retry: RetryError | null, result?: RestResponse<Message>, error?: unknown): void => {
				if (hasCompleted) {
					logger.warn('Immediate send completion callback invoked more than once; ignoring extra call');
					return;
				}
				hasCompleted = true;
				resolve({retry, result, error});
			};
			try {
				void this.handleSend(payload, complete).catch((error) => {
					logger.error('Unhandled error while sending immediate message', error);
					if (!hasCompleted) {
						complete(null, undefined, error);
					}
				});
			} catch (error) {
				logger.error('Unhandled error while sending immediate message', error);
				if (!hasCompleted) {
					complete(null, undefined, error);
				}
			}
		});
	}

	startTextareaAttachmentUploads(channelId: string, attachments: ReadonlyArray<CloudAttachment>): void {
		if (!PrivacyPreferences.getPreuploadMessageAttachments()) {
			return;
		}
		const pendingAttachments = attachments.filter((attachment) => !this.textareaAttachmentUploads.has(attachment.id));
		if (pendingAttachments.length === 0) {
			return;
		}
		const files = pendingAttachments.map((attachment) => attachment.file);
		if (!this.canUsePresignedAttachmentUploads(files)) {
			return;
		}
		this.ensureTextareaAttachmentUploadPruner(channelId);
		const requestAttachments: Array<ApiAttachmentMetadata> = pendingAttachments.map((attachment) => ({
			id: String(attachment.id),
			filename: attachment.filename,
			title: attachment.filename,
			description: attachment.description,
			flags: attachment.flags,
			duration: attachment.duration != null ? Math.ceil(attachment.duration) : undefined,
			waveform: attachment.waveform ?? undefined,
		}));
		const requestAbortController = new AbortController();
		const plansPromise = this.requestPresignedAttachmentUploads(
			channelId,
			requestAttachments,
			files,
			requestAbortController.signal,
		)
			.then((plans) => {
				const planIndexById = new Map<string, number>();
				plans.forEach((entry, index) => planIndexById.set(String(entry.id), index));
				return {plans, planIndexById};
			})
			.catch((error) => {
				if (!isAbortError(error)) {
					logger.warn('Failed to start background attachment upload; will upload when sending', error);
				}
				throw error;
			});
		pendingAttachments.forEach((attachment, index) => {
			const file = files[index];
			const abortController = new AbortController();
			const promise = (async (): Promise<TextareaAttachmentUploadResult> => {
				try {
					CloudUpload.updateAttachment(channelId, attachment.id, {status: 'uploading', uploadProgress: 0});
					const {plans, planIndexById} = await plansPromise;
					if (abortController.signal.aborted) {
						throw createAbortError();
					}
					const planIndex = planIndexById.get(String(attachment.id));
					const plan = planIndex == null ? undefined : plans[planIndex];
					if (!plan) {
						throw new Error(`Missing presigned upload metadata for attachment ${attachment.id}`);
					}
					const result = await this.uploadTextareaAttachmentViaPlan({
						channelId,
						attachmentId: attachment.id,
						file,
						plan,
						signal: abortController.signal,
					});
					CloudUpload.updateAttachment(channelId, attachment.id, {status: 'sending', uploadProgress: 100});
					return result;
				} catch (error) {
					CloudUpload.updateAttachment(channelId, attachment.id, {status: 'pending', uploadProgress: 0});
					throw error;
				}
			})();
			const entry: TextareaAttachmentUpload = {
				channelId,
				attachmentId: attachment.id,
				startedAt: Date.now(),
				requestAbortController,
				abortController,
				promise,
				settled: false,
			};
			this.textareaAttachmentUploads.set(attachment.id, entry);
			void promise
				.catch(() => undefined)
				.finally(() => {
					const current = this.textareaAttachmentUploads.get(attachment.id);
					if (current === entry) {
						current.settled = true;
					}
				});
			this.scheduleTextareaAttachmentUploadCleanup(attachment.id);
		});
	}

	private scheduleTextareaAttachmentUploadCleanup(attachmentId: number): void {
		window.setTimeout(() => {
			const entry = this.textareaAttachmentUploads.get(attachmentId);
			if (!entry || !entry.settled) {
				return;
			}
			if (Date.now() - entry.startedAt >= TEXTAREA_ATTACHMENT_UPLOAD_CACHE_TTL_MS) {
				this.textareaAttachmentUploads.delete(attachmentId);
				this.disposeTextareaAttachmentUploadPrunerIfIdle(entry.channelId);
			}
		}, TEXTAREA_ATTACHMENT_UPLOAD_CACHE_TTL_MS);
	}

	private ensureTextareaAttachmentUploadPruner(channelId: string): void {
		if (this.textareaAttachmentUploadDisposers.has(channelId)) {
			return;
		}
		const dispose = CloudUpload.subscribeToTextarea(channelId, () => {
			this.cancelDetachedTextareaAttachmentUploads(channelId);
		});
		this.textareaAttachmentUploadDisposers.set(channelId, dispose);
	}

	private cancelDetachedTextareaAttachmentUploads(channelId: string): void {
		const attachmentIds: Array<number> = [];
		for (const [attachmentId, upload] of this.textareaAttachmentUploads.entries()) {
			if (upload.channelId === channelId && !CloudUpload.hasAttachment(attachmentId)) {
				attachmentIds.push(attachmentId);
			}
		}
		this.cancelTextareaAttachmentUploads(attachmentIds);
		this.disposeTextareaAttachmentUploadPrunerIfIdle(channelId);
	}

	private disposeTextareaAttachmentUploadPrunerIfIdle(channelId: string): void {
		for (const upload of this.textareaAttachmentUploads.values()) {
			if (upload.channelId === channelId) {
				return;
			}
		}
		const dispose = this.textareaAttachmentUploadDisposers.get(channelId);
		if (!dispose) {
			return;
		}
		dispose();
		this.textareaAttachmentUploadDisposers.delete(channelId);
	}

	private cancelTextareaAttachmentUploads(attachmentIds: ReadonlyArray<number>): void {
		const channelIds = new Set<string>();
		for (const attachmentId of attachmentIds) {
			const upload = this.textareaAttachmentUploads.get(attachmentId);
			if (!upload) continue;
			channelIds.add(upload.channelId);
			upload.requestAbortController.abort();
			upload.abortController.abort();
			this.textareaAttachmentUploads.delete(attachmentId);
		}
		for (const channelId of channelIds) {
			this.disposeTextareaAttachmentUploadPrunerIfIdle(channelId);
		}
	}

	private cancelAllTextareaAttachmentUploads(): void {
		this.cancelTextareaAttachmentUploads(Array.from(this.textareaAttachmentUploads.keys()));
	}

	private deleteTextareaAttachmentUploads(attachmentIds: ReadonlyArray<number>): void {
		const channelIds = new Set<string>();
		for (const attachmentId of attachmentIds) {
			const upload = this.textareaAttachmentUploads.get(attachmentId);
			if (upload) {
				channelIds.add(upload.channelId);
			}
			this.textareaAttachmentUploads.delete(attachmentId);
		}
		for (const channelId of channelIds) {
			this.disposeTextareaAttachmentUploadPrunerIfIdle(channelId);
		}
	}

	private async tryPrepareTextareaAttachmentUploads(params: {
		channelId: string;
		nonce: string;
		rawAttachments: Array<ApiAttachmentMetadata>;
		files: Array<File>;
	}): Promise<PreparedSendAttachments | null | undefined> {
		const {channelId, nonce, rawAttachments, files} = params;
		const messageUpload = CloudUpload.getMessageUpload(nonce);
		if (!messageUpload || messageUpload.attachments.length !== rawAttachments.length) {
			return undefined;
		}
		if (messageUpload.attachments.length !== files.length) {
			return undefined;
		}
		const attachmentIds = messageUpload.attachments.map((attachment) => attachment.id);
		if (!PrivacyPreferences.getPreuploadMessageAttachments()) {
			this.cancelTextareaAttachmentUploads(attachmentIds);
			return undefined;
		}
		const uploads: Array<TextareaAttachmentUpload> = [];
		for (const attachment of messageUpload.attachments) {
			const upload = this.textareaAttachmentUploads.get(attachment.id);
			if (!upload || upload.channelId !== channelId) {
				this.cancelTextareaAttachmentUploads(attachmentIds);
				return undefined;
			}
			uploads.push(upload);
		}
		try {
			CloudUpload.startSendingProgress(nonce);
			const results = await Promise.all(uploads.map((upload) => upload.promise));
			CloudUpload.updateSendingProgress(nonce, 100);
			return {
				attachments: rawAttachments.map((attachment, index) => {
					const result = results[index];
					return {
						...attachment,
						upload_filename: result.uploadFilename,
						file_size: result.fileSize,
						content_type: result.contentType,
					};
				}),
				files: undefined,
			};
		} catch (error) {
			if (isAbortError(error)) {
				return null;
			}
			logger.warn('Background attachment upload unavailable; uploading at send time', error);
			this.cancelTextareaAttachmentUploads(attachmentIds);
			CloudUpload.updateSendingProgress(nonce, 0);
			return undefined;
		}
	}

	async prepareAttachmentsForSend(params: {
		channelId: string;
		nonce: string;
		favoriteMemeId?: string;
	}): Promise<PreparedSendAttachments | null> {
		const {channelId, nonce, favoriteMemeId} = params;
		const abortController = new AbortController();
		this.abortControllers.set(nonce, abortController);
		try {
			const {attachments: rawAttachments, files} = await prepareAttachmentsForNonce(nonce, favoriteMemeId);
			if (!files?.length || !rawAttachments?.length) {
				return {attachments: rawAttachments, files};
			}
			if (!this.canUsePresignedAttachmentUploads(files)) {
				return {attachments: rawAttachments, files};
			}
			if (rawAttachments.length !== files.length) {
				throw new Error(
					`Attachment metadata mismatch for presigned uploads: expected ${files.length} entries but got ${rawAttachments.length}`,
				);
			}
			const backgroundUploadResult = await this.tryPrepareTextareaAttachmentUploads({
				channelId,
				nonce,
				rawAttachments,
				files,
			});
			if (backgroundUploadResult !== undefined) {
				return backgroundUploadResult;
			}
			let plans: Array<PresignedAttachmentUploadResponseAttachment>;
			try {
				plans = await this.requestPresignedAttachmentUploads(channelId, rawAttachments, files, abortController.signal);
			} catch (error) {
				if (isAbortError(error)) {
					return null;
				}
				if (isPresignedUploadEndpointUnreachable(error)) {
					if (exceedsMultipartFallbackRequestSize(files)) {
						logger.warn(
							'Presigned attachment upload URL request failed because the endpoint was unreachable and multipart fallback cannot be used for oversized requests',
							error,
						);
						this.handleSendError(channelId, nonce, new PresignedUploadFallbackUnavailableError(), i18n, true);
						return null;
					}
					logger.warn(
						'Presigned attachment upload URL request failed because the endpoint was unreachable; falling back to multipart message upload',
						error,
					);
					return {attachments: rawAttachments, files};
				}
				logger.warn('Presigned attachment upload URL request failed; falling back to multipart message upload', error);
				return {attachments: rawAttachments, files};
			}
			const planIndexById = new Map<string, number>();
			plans.forEach((entry, index) => planIndexById.set(String(entry.id), index));
			for (const attachment of rawAttachments) {
				if (!planIndexById.has(String(attachment.id))) {
					this.handleSendError(
						channelId,
						nonce,
						new Error(`Missing presigned upload metadata for attachment ${attachment.id}`),
						i18n,
						true,
					);
					return null;
				}
			}
			const multipartUploadsToComplete: Array<{upload_filename: string; upload_id: string}> = [];
			let finalized: Array<ApiAttachmentMetadata>;
			try {
				finalized = await this.uploadAttachmentsViaPlans({
					nonce,
					attachments: rawAttachments,
					files,
					plans,
					planIndexById,
					multipartUploadsToComplete,
					signal: abortController.signal,
				});
			} catch (error) {
				if (isAbortError(error)) {
					this.abortRemainingMultipartUploads(channelId, multipartUploadsToComplete);
					return null;
				}
				if (exceedsMultipartFallbackRequestSize(files)) {
					logger.warn(
						'Presigned attachment upload failed and multipart fallback cannot be used for oversized requests',
						error,
					);
					this.handleSendError(channelId, nonce, new PresignedUploadFallbackUnavailableError(), i18n, true);
					return null;
				}
				logger.warn('Presigned attachment upload failed; falling back to multipart message upload', error);
				CloudUpload.updateSendingProgress(nonce, 0);
				this.abortRemainingMultipartUploads(channelId, multipartUploadsToComplete);
				return {attachments: rawAttachments, files};
			}
			if (multipartUploadsToComplete.length > 0) {
				try {
					await this.completeMultipartAttachmentUploads(channelId, multipartUploadsToComplete, abortController.signal);
				} catch (error) {
					if (isAbortError(error)) {
						return null;
					}
					logger.error(`Failed to finalize multipart attachment uploads for channel ${channelId}`, error);
					this.handleSendError(channelId, nonce, error, i18n, true);
					return null;
				}
			}
			return {attachments: finalized, files: undefined};
		} catch (error) {
			if (isAbortError(error)) {
				return null;
			}
			logger.error(`Failed to prepare attachments for channel ${channelId}:`, error);
			this.handleSendError(channelId, nonce, error, i18n, true);
			return null;
		} finally {
			if (this.abortControllers.get(nonce) === abortController && abortController.signal.aborted) {
				this.abortControllers.delete(nonce);
			}
		}
	}

	private async handleSend(
		payload: SendMessagePayload,
		completed: (err: RetryError | null, result?: RestResponse<Message>, error?: unknown) => void,
	): Promise<void> {
		const {channelId, nonce, hasAttachments} = payload;
		await this.applyDevDelay();
		const executionDecision = resolveMessageQueueSendExecutionDecision({
			forceFailure: DeveloperOptions.forceFailMessageSends,
		});
		switch (executionDecision.type) {
			case 'simulateFailure': {
				const forcedError = new Error('Forced message send failure');
				logger.error(`Failed to send message to channel ${channelId}:`, forcedError);
				this.handleSendError(channelId, nonce, forcedError as HttpError, i18n, payload.hasAttachments);
				completed(null, undefined, forcedError);
				return;
			}
			case 'requestNetwork':
				break;
		}
		const requestBody = buildMessageCreateRequest({
			content: payload.content,
			nonce,
			attachments: payload.preparedAttachments,
			allowedMentions: payload.allowedMentions,
			messageReference: payload.messageReference,
			flags: payload.flags,
			favoriteMemeId: payload.favoriteMemeId,
			stickers: payload.stickers,
			tts: payload.tts,
		});
		logger.debug(`Sending message to channel ${channelId}`);
		const outcome = await this.attemptMessageSend(channelId, nonce, requestBody, payload.preparedFiles);
		const outcomeDecision = resolveMessageQueueRequestOutcomeDecision({status: outcome.status});
		switch (outcomeDecision.type) {
			case 'completeSuccess': {
				const successOutcome = outcome as {status: 'success'; response: RestResponse<Message>};
				logger.debug(`Successfully sent message to channel ${channelId}`);
				if (hasAttachments) {
					const messageUpload = CloudUpload.getMessageUpload(nonce);
					if (messageUpload) {
						this.deleteTextareaAttachmentUploads(messageUpload.attachments.map((attachment) => attachment.id));
					}
					CloudUpload.removeMessageUpload(nonce);
				}
				completed(null, successOutcome.response);
				return;
			}
			case 'retryRateLimit': {
				const rateLimitOutcome = outcome as {status: 'rateLimit'; error: HttpError};
				logger.error(`Failed to send message to channel ${channelId}:`, rateLimitOutcome.error);
				this.handleSendRateLimit(payload, rateLimitOutcome.error, completed);
				return;
			}
			case 'completeFailure': {
				const failureOutcome = outcome as {status: 'failure'; error: unknown};
				logger.error(`Failed to send message to channel ${channelId}:`, failureOutcome.error);
				this.handleSendError(channelId, nonce, failureOutcome.error, i18n, payload.hasAttachments);
				completed(null, undefined, failureOutcome.error);
				return;
			}
		}
	}

	private async applyDevDelay(): Promise<void> {
		if (!DeveloperOptions.slowMessageSend) return;
		logger.debug(`Slow message send enabled, delaying by ${DEV_MESSAGE_DELAY}ms`);
		await new Promise((resolve) => setTimeout(resolve, DEV_MESSAGE_DELAY));
	}

	private async sendMessageRequest(
		channelId: string,
		nonce: string,
		requestBody: MessageCreateRequest,
		files?: Array<File>,
	): Promise<RestResponse<Message>> {
		const existing = this.abortControllers.get(nonce);
		const abortController = existing ?? new AbortController();
		if (!existing) {
			this.abortControllers.set(nonce, abortController);
		}
		try {
			if (files?.length) {
				logger.debug('Sending message with multipart form data');
				return await this.sendMultipartMessage(channelId, requestBody, files, abortController.signal, nonce);
			}
			return await http.post<Message>(Endpoints.CHANNEL_MESSAGES(channelId), {
				body: requestBody,
				signal: abortController.signal,
				suppressContentBlockedModal: true,
			});
		} finally {
			this.abortControllers.delete(nonce);
		}
	}

	private canUsePresignedAttachmentUploads(files?: Array<File>): files is Array<File> {
		return RuntimeConfig.features.presigned_attachment_uploads && Boolean(files?.length);
	}

	private async requestPresignedAttachmentUploads(
		channelId: string,
		attachments: Array<ApiAttachmentMetadata>,
		files: Array<File>,
		signal: AbortSignal,
	): Promise<Array<PresignedAttachmentUploadResponseAttachment>> {
		const requestBody: PresignedAttachmentUploadRequestBody = {
			attachments: attachments.map((attachment, index) => ({
				id: attachment.id,
				filename: attachment.filename,
				file_size: files[index].size,
				content_type: files[index].type || 'application/octet-stream',
			})),
		};
		const response = await http.post<PresignedAttachmentUploadResponseBody>(Endpoints.CHANNEL_ATTACHMENTS(channelId), {
			body: requestBody,
			signal,
		});
		const plans = response.body?.attachments ?? [];
		for (const entry of plans) {
			if (!entry || !entry.upload_mode || !entry.upload_filename || !entry.filename) {
				throw new Error('Invalid presigned attachment upload response');
			}
			if (entry.upload_mode === 'singlepart') {
				if (!entry.upload_url) {
					throw new Error(`Missing upload_url for singlepart attachment ${entry.id}`);
				}
			} else if (entry.upload_mode === 'multipart') {
				if (!entry.upload_id || !Array.isArray(entry.parts) || entry.parts.length === 0) {
					throw new Error(`Missing multipart metadata for attachment ${entry.id}`);
				}
			} else {
				throw new Error(`Unknown upload_mode for attachment ${(entry as {id: string}).id}`);
			}
		}
		return plans;
	}

	private async uploadAttachmentsViaPlans(params: {
		nonce: string;
		attachments: Array<ApiAttachmentMetadata>;
		files: Array<File>;
		plans: Array<PresignedAttachmentUploadResponseAttachment>;
		planIndexById: Map<string, number>;
		multipartUploadsToComplete: Array<{upload_filename: string; upload_id: string}>;
		signal: AbortSignal;
	}): Promise<Array<ApiAttachmentMetadata>> {
		const {nonce, attachments, files, plans, planIndexById, multipartUploadsToComplete, signal} = params;
		const totalBytes = files.reduce((sum, file) => sum + file.size, 0);
		const loadedBytesByIndex = new Array<number>(files.length).fill(0);
		let completedUploads = 0;
		const reportProgress = (): void => {
			if (totalBytes > 0) {
				const uploadedBytes = loadedBytesByIndex.reduce((sum, loaded) => sum + loaded, 0);
				CloudUpload.updateSendingProgress(nonce, (uploadedBytes / totalBytes) * 100);
				return;
			}
			if (files.length > 0) {
				CloudUpload.updateSendingProgress(nonce, (completedUploads / files.length) * 100);
			}
		};
		for (let index = 0; index < attachments.length; index += 1) {
			const attachment = attachments[index];
			const file = files[index];
			const planIndex = planIndexById.get(String(attachment.id));
			if (planIndex == null) {
				throw new Error(`Missing presigned upload metadata for attachment ${attachment.id}`);
			}
			const plan = plans[planIndex];
			if (plan.upload_mode === 'singlepart') {
				await http.put(plan.upload_url, {
					body: file,
					headers: {
						'Content-Type': plan.content_type,
					},
					signal,
					onProgress: (event) => {
						const loaded = Math.min(file.size, event.loaded);
						if (loaded > loadedBytesByIndex[index]) {
							loadedBytesByIndex[index] = loaded;
							reportProgress();
						}
					},
				});
				loadedBytesByIndex[index] = file.size;
			} else {
				multipartUploadsToComplete.push({
					upload_filename: plan.upload_filename,
					upload_id: plan.upload_id,
				});
				const parts: Array<ChunkedUploadPart> = plan.parts.map((entry) => ({
					partNumber: entry.part_number,
					uploadUrl: entry.upload_url,
				}));
				const uploadPlan: ChunkedUploadPlan = {file, contentType: plan.content_type, partSize: plan.part_size, parts};
				await uploadFileInChunks(uploadPlan, {
					signal,
					onProgress: (uploaded) => {
						if (uploaded > loadedBytesByIndex[index]) {
							loadedBytesByIndex[index] = uploaded;
							reportProgress();
						}
					},
				});
				loadedBytesByIndex[index] = file.size;
			}
			completedUploads += 1;
			reportProgress();
		}
		return attachments.map((attachment) => {
			const planIndex = planIndexById.get(String(attachment.id));
			if (planIndex == null) {
				throw new Error(`Missing presigned upload metadata for attachment ${attachment.id}`);
			}
			const plan = plans[planIndex];
			return {
				...attachment,
				upload_filename: plan.upload_filename,
				file_size: plan.file_size,
				content_type: plan.content_type,
			};
		});
	}

	private async uploadTextareaAttachmentViaPlan(params: {
		channelId: string;
		attachmentId: number;
		file: File;
		plan: PresignedAttachmentUploadResponseAttachment;
		signal: AbortSignal;
	}): Promise<TextareaAttachmentUploadResult> {
		const {channelId, attachmentId, file, plan, signal} = params;
		const reportProgress = (uploadedBytes: number): void => {
			if (file.size <= 0) {
				CloudUpload.updateAttachment(channelId, attachmentId, {status: 'uploading', uploadProgress: 0});
				return;
			}
			const uploadProgress = Math.round((Math.min(file.size, uploadedBytes) / file.size) * 100);
			CloudUpload.updateAttachment(channelId, attachmentId, {status: 'uploading', uploadProgress});
		};
		if (plan.upload_mode === 'singlepart') {
			await http.put(plan.upload_url, {
				body: file,
				headers: {
					'Content-Type': plan.content_type,
				},
				signal,
				onProgress: (event) => {
					reportProgress(event.loaded);
				},
			});
		} else {
			const parts: Array<ChunkedUploadPart> = plan.parts.map((entry) => ({
				partNumber: entry.part_number,
				uploadUrl: entry.upload_url,
			}));
			await uploadFileInChunks(
				{file, contentType: plan.content_type, partSize: plan.part_size, parts},
				{
					signal,
					onProgress: (uploadedBytes) => {
						reportProgress(uploadedBytes);
					},
				},
			);
			await this.completeMultipartAttachmentUploads(
				channelId,
				[{upload_filename: plan.upload_filename, upload_id: plan.upload_id}],
				signal,
			);
		}
		return {
			uploadFilename: plan.upload_filename,
			fileSize: plan.file_size,
			contentType: plan.content_type,
		};
	}

	private async completeMultipartAttachmentUploads(
		channelId: string,
		uploads: Array<{upload_filename: string; upload_id: string}>,
		signal: AbortSignal,
	): Promise<void> {
		const body: CompleteMultipartAttachmentUploadRequestBody = {uploads};
		await http.post<CompleteMultipartAttachmentUploadResponseBody>(Endpoints.CHANNEL_ATTACHMENTS_COMPLETE(channelId), {
			body,
			signal,
		});
	}

	private abortRemainingMultipartUploads(
		channelId: string,
		uploads: Array<{upload_filename: string; upload_id: string}>,
	): void {
		if (uploads.length === 0) return;
		logger.debug(
			`Leaving ${uploads.length} multipart ${uploads.length === 1 ? 'upload' : 'uploads'} for server to GC in channel ${channelId}`,
		);
	}

	private async sendMultipartMessage(
		channelId: string,
		requestBody: MessageCreateRequest,
		files: Array<File>,
		signal: AbortSignal,
		nonce?: string,
	): Promise<RestResponse<Message>> {
		const formData = new FormData();
		formData['append']('payload_json', JSON.stringify(requestBody));
		files.forEach((file, index) => {
			formData['append'](`files[${index}]`, file);
		});
		return http.post<Message>(Endpoints.CHANNEL_MESSAGES(channelId), {
			body: formData,
			signal,
			suppressContentBlockedModal: true,
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

	private async attemptMessageSend(
		channelId: string,
		nonce: string,
		requestBody: MessageCreateRequest,
		files?: Array<File>,
	): Promise<
		| {status: 'success'; response: RestResponse<Message>}
		| {status: 'rateLimit'; error: HttpError}
		| {status: 'failure'; error: unknown}
	> {
		try {
			const response = await this.sendMessageRequest(channelId, nonce, requestBody, files);
			return {status: 'success', response};
		} catch (error) {
			return this.buildSendOutcome(error);
		}
	}

	private buildSendOutcome(
		error: unknown,
	): {status: 'rateLimit'; error: HttpError} | {status: 'failure'; error: unknown} {
		const responseErr = error instanceof HttpError ? error : null;
		if (responseErr && isRateLimitError(responseErr)) {
			return {status: 'rateLimit', error: responseErr};
		}
		return {status: 'failure', error};
	}

	private handleSendRateLimit(
		payload: SendMessagePayload,
		error: HttpError,
		completed: (err: RetryError | null, result?: RestResponse<Message>, error?: unknown) => void,
	): void {
		const retry = resolveMessageRateLimitRetry(error);
		const retryCount = payload.rateLimitRetryCount === undefined ? 0 : payload.rateLimitRetryCount;
		if (retry.automaticRetryDelayMs !== null && retryCount < MESSAGE_SEND_RATE_LIMIT_MAX_AUTOMATIC_RETRIES) {
			payload.rateLimitRetryCount = retryCount + 1;
			completed({retryAfter: retry.automaticRetryDelayMs}, undefined, error);
			return;
		}
		MessageCommands.sendError(payload.channelId, payload.nonce);
		if (payload.hasAttachments) {
			this.restoreFailedMessage(payload.channelId, payload.nonce);
		}
		completed(null, undefined, error);
		this.handleRateLimitError(retry.retryAfterSeconds);
	}

	private handleSendError(
		channelId: string,
		nonce: string,
		error: unknown,
		i18n: I18n,
		hasAttachments?: boolean,
	): void {
		MessageCommands.sendError(channelId, nonce);
		if (hasAttachments) {
			this.restoreFailedMessage(channelId, nonce);
		}
		if (!(error instanceof HttpError)) {
			this.showErrorModal(error, channelId, hasAttachments);
			return;
		}
		if (isDMRestrictedError(error)) {
			const directMessagePrivacySettingsPath = formatUserSettingsPath(i18n, 'privacy_safety', 'communication');
			const systemMessage = createSystemMessage(
				channelId,
				i18n._(YOUR_MESSAGE_COULD_NOT_BE_DELIVERED_THIS_IS_DESCRIPTOR, {directMessagePrivacySettingsPath}),
			);
			MessageCommands.createOptimistic(channelId, systemMessage.toJSON());
			return;
		}
		const unclaimedErrorCode = getUnclaimedAccountErrorCode(error);
		if (unclaimedErrorCode) {
			const systemMessage = createSystemMessage(
				channelId,
				unclaimedErrorCode === APIErrorCodes.UNCLAIMED_ACCOUNT_CANNOT_SEND_DIRECT_MESSAGES
					? i18n._(YOUR_MESSAGE_COULD_NOT_BE_DELIVERED_YOU_NEED_DESCRIPTOR)
					: i18n._(YOUR_MESSAGE_COULD_NOT_BE_DELIVERED_YOU_NEED_2_DESCRIPTOR),
			);
			MessageCommands.createOptimistic(channelId, systemMessage.toJSON());
			return;
		}
		if (getApiErrorBody(error)?.code === APIErrorCodes.CONTENT_BLOCKED) {
			const systemMessage = createSystemMessage(
				channelId,
				i18n._(YOUR_MESSAGE_COULD_NOT_BE_DELIVERED_BECAUSE_IT_DESCRIPTOR),
			);
			MessageCommands.createOptimistic(channelId, systemMessage.toJSON());
			return;
		}
		if (getApiErrorBody(error)?.code === APIErrorCodes.NSFW_EMOJI_STICKER_BLOCKED) {
			const systemMessage = createSystemMessage(
				channelId,
				i18n._(YOUR_MESSAGE_COULD_NOT_BE_DELIVERED_BECAUSE_IT_2_DESCRIPTOR),
			);
			MessageCommands.createOptimistic(channelId, systemMessage.toJSON());
			return;
		}
		this.showErrorModal(error, channelId, hasAttachments);
	}

	private restoreFailedMessage(channelId: string, nonce: string): void {
		const messageUpload = CloudUpload.getMessageUpload(nonce);
		CloudUpload.restoreAttachmentsToTextarea(nonce);
		const contentToRestore = messageUpload?.content ?? '';
		DraftCommands.createDraft(channelId, contentToRestore);
		if (messageUpload?.messageReference) {
			MessageCommands.startReply(
				channelId,
				messageUpload.messageReference.message_id,
				messageUpload.allowedMentions?.replied_user ?? true,
			);
		}
		MessageCommands.deleteOptimistic(channelId, nonce);
	}

	private showErrorModal(error: unknown, channelId?: string, hasAttachments?: boolean): void {
		if (error instanceof PresignedUploadFallbackUnavailableError) {
			ModalCommands.push(
				modal(() => (
					<AttachmentUploadConnectivityModal data-flx="messaging.message-queue.attachment-upload-connectivity-modal" />
				)),
			);
		} else if (error instanceof HttpError && isSlowmodeError(error)) {
			const retry = resolveMessageRateLimitRetry(error);
			const retryAfterMs = SlowmodeCommands.retryAfterSecondsToMs(
				retry.retryAfterSeconds === null ? undefined : retry.retryAfterSeconds,
			);
			if (retryAfterMs <= 0) {
				ModalCommands.push(
					modal(() => (
						<MessageSendFailedModal
							hasAttachments={hasAttachments}
							data-flx="messaging.message-queue.message-send-failed-modal--invalid-slowmode-retry"
						/>
					)),
				);
				return;
			}
			const retryAfter = Math.ceil(retryAfterMs / 1000);
			if (channelId) {
				SlowmodeCommands.updateSlowmodeRemaining(channelId, retryAfterMs);
			}
			ModalCommands.push(
				modal(() => (
					<SlowmodeRateLimitedModal
						retryAfter={retryAfter}
						data-flx="messaging.message-queue.slowmode-rate-limited-modal"
					/>
				)),
			);
		} else if (error instanceof HttpError && isFeatureDisabledError(error)) {
			ModalCommands.push(
				modal(() => (
					<FeatureTemporarilyDisabledModal data-flx="messaging.message-queue.feature-temporarily-disabled-modal" />
				)),
			);
		} else if (error instanceof HttpError && isExplicitContentError(error)) {
			ModalCommands.push(
				modal(() => <MatureContentRejectedModal data-flx="messaging.message-queue.mature-content-rejected-modal" />),
			);
		} else if (error instanceof HttpError && isFileTooLargeError(error)) {
			ModalCommands.push(
				modal(() => <FileSizeTooLargeModal data-flx="messaging.message-queue.file-size-too-large-modal" />),
			);
		} else if (!isAbortError(error)) {
			ModalCommands.push(
				modal(() => (
					<MessageSendFailedModal
						hasAttachments={hasAttachments}
						data-flx="messaging.message-queue.message-send-failed-modal"
					/>
				)),
			);
		}
	}

	private handleRateLimitError(retryAfter: number | null, onRetry?: () => void): void {
		ModalCommands.push(
			modal(() => {
				if (retryAfter === null) {
					return (
						<MessageSendTooQuickModal
							onRetry={onRetry}
							data-flx="messaging.message-queue.message-send-too-quick-modal"
						/>
					);
				}
				return (
					<MessageSendTooQuickModal
						retryAfter={retryAfter}
						onRetry={onRetry}
						data-flx="messaging.message-queue.message-send-too-quick-modal"
					/>
				);
			}),
		);
	}

	private async handleEdit(
		payload: EditMessagePayload,
		completed: (err: RetryError | null, result?: RestResponse<Message>, error?: unknown) => void,
	): Promise<void> {
		const {channelId, messageId, content, allowedMentions, flags, attachments} = payload;
		const abortController = new AbortController();
		this.abortControllers.set(messageId, abortController);
		try {
			logger.debug(`Editing message ${messageId} in channel ${channelId}`);
			const body = this.buildEditRequestBody(content, allowedMentions, flags, attachments);
			const response = await http.patch<Message>(Endpoints.CHANNEL_MESSAGE(channelId, messageId), {
				body,
				signal: abortController.signal,
				suppressContentBlockedModal: true,
			});
			logger.debug(`Successfully edited message ${messageId} in channel ${channelId}`);
			completed(null, response);
		} catch (error) {
			logger.error(`Failed to edit message ${messageId} in channel ${channelId}:`, error);
			if (!(error instanceof HttpError)) {
				ModalCommands.push(
					modal(() => <MessageEditFailedModal data-flx="messaging.message-queue.message-edit-failed-modal--request" />),
				);
				completed(null, undefined, error);
				return;
			}
			const responseErr = error;
			const outcomeDecision = resolveMessageQueueRequestOutcomeDecision({
				status: getRequestErrorOutcomeStatus(responseErr),
			});
			switch (outcomeDecision.type) {
				case 'retryRateLimit':
					this.handleEditRateLimit(payload, responseErr, completed);
					break;
				case 'completeFailure':
				case 'completeSuccess':
					this.showEditErrorModal(responseErr);
					completed(null, undefined, responseErr);
					break;
			}
		} finally {
			this.abortControllers.delete(messageId);
		}
	}

	private buildEditRequestBody(
		content?: string,
		allowedMentions?: AllowedMentions,
		flags?: number,
		attachments?: Array<ApiMessageEditAttachmentMetadata>,
	): MessageEditRequest {
		const body: MessageEditRequest = {};
		if (content !== undefined) {
			body.content = normalizeMessageEditContent(content);
		}
		if (allowedMentions !== undefined) {
			body.allowed_mentions = allowedMentions;
		}
		if (flags !== undefined) {
			body.flags = flags;
		}
		if (attachments !== undefined) {
			body.attachments = attachments;
		}
		return body;
	}

	private handleEditRateLimit(
		payload: EditMessagePayload,
		error: HttpError,
		completed: (err: RetryError | null, result?: RestResponse<Message>, error?: unknown) => void,
	): void {
		const retry = resolveMessageRateLimitRetry(error);
		const retryCount = payload.rateLimitRetryCount === undefined ? 0 : payload.rateLimitRetryCount;
		if (retry.automaticRetryDelayMs !== null && retryCount < MESSAGE_SEND_RATE_LIMIT_MAX_AUTOMATIC_RETRIES) {
			payload.rateLimitRetryCount = retryCount + 1;
			completed({retryAfter: retry.automaticRetryDelayMs}, undefined, error);
			return;
		}
		completed(null, undefined, error);
		this.handleEditRateLimitError(retry.retryAfterSeconds);
	}

	private showEditErrorModal(error: HttpError): void {
		if (isFeatureDisabledError(error)) {
			ModalCommands.push(
				modal(() => (
					<FeatureTemporarilyDisabledModal data-flx="messaging.message-queue.feature-temporarily-disabled-modal--2" />
				)),
			);
		} else if (getApiErrorBody(error)?.code === APIErrorCodes.CONTENT_BLOCKED) {
			void import('@app/features/auth/components/ContentBlockedHandler').then((m) => m.showContentBlockedModal());
		} else {
			ModalCommands.push(
				modal(() => <MessageEditFailedModal data-flx="messaging.message-queue.message-edit-failed-modal" />),
			);
		}
	}

	private handleEditRateLimitError(retryAfter: number | null, onRetry?: () => void): void {
		ModalCommands.push(
			modal(() => {
				if (retryAfter === null) {
					return (
						<MessageEditTooQuickModal
							onRetry={onRetry}
							data-flx="messaging.message-queue.message-edit-too-quick-modal"
						/>
					);
				}
				return (
					<MessageEditTooQuickModal
						retryAfter={retryAfter}
						onRetry={onRetry}
						data-flx="messaging.message-queue.message-edit-too-quick-modal"
					/>
				);
			}),
		);
	}
}

export default new MessageQueue();
