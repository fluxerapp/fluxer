// SPDX-License-Identifier: AGPL-3.0-or-later

import i18n from '@app/app/I18n';
import {showDmActionErrorModal} from '@app/features/app/components/alerts/DmActionErrorModal';
import {FeatureTemporarilyDisabledModal} from '@app/features/app/components/alerts/FeatureTemporarilyDisabledModal';
import {Endpoints} from '@app/features/app/constants/Endpoints';
import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';
import {createSystemMessage} from '@app/features/devtools/utils/CommandUtils';
import * as DraftCommands from '@app/features/messaging/commands/DraftCommands';
import * as MessageCommands from '@app/features/messaging/commands/MessageCommands';
import {AttachmentUploadConnectivityModal} from '@app/features/messaging/components/alerts/AttachmentUploadConnectivityModal';
import {FileSizeTooLargeModal} from '@app/features/messaging/components/alerts/FileSizeTooLargeModal';
import {MessageSendFailedModal} from '@app/features/messaging/components/alerts/MessageSendFailedModal';
import {MessageSendTooQuickModal} from '@app/features/messaging/components/alerts/MessageSendTooQuickModal';
import {
	type MessageLocalSendRateLimitState,
	resolveMessageLocalSendRateLimitDecision,
	resolveMessageQueueRequestOutcomeDecision,
	resolveMessageQueueSendExecutionDecision,
} from '@app/features/messaging/state/MessageQueueStateMachine';
import {planTextareaAttachmentCancellation} from '@app/features/messaging/state/TextareaAttachmentUploadCancellation';
import {
	canUsePresignedAttachmentUploads,
	completeMultipartAttachmentUploads,
	type MultipartAttachmentUpload,
	type PresignedAttachmentUploadResponseAttachment,
	requestPresignedAttachmentUploads,
	type TextareaAttachmentUploadResult,
	uploadAttachmentsViaPlans,
	uploadTextareaAttachmentViaPlan,
} from '@app/features/messaging/upload/AttachmentUploadPlan';
import {type CloudAttachment, CloudUpload} from '@app/features/messaging/upload/CloudUpload';
import {exceedsMultipartFallbackRequestSize} from '@app/features/messaging/utils/AttachmentUploadFallbackUtils';
import {prepareAttachmentsForNonce} from '@app/features/messaging/utils/MessageAttachmentUtils';
import {
	type ApiAttachmentMetadata,
	buildMessageCreateRequest,
	type MessageCreateRequest,
} from '@app/features/messaging/utils/MessageRequestUtils';
import {resolveRetryAfterMs} from '@app/features/messaging/utils/RetryAfterUtils';
import {MatureContentRejectedModal} from '@app/features/moderation/components/alerts/MatureContentRejectedModal';
import SelectedChannel from '@app/features/navigation/state/SelectedChannel';
import {AccountScopedWork, accountScopedWorkAbortError} from '@app/features/platform/state/AccountScopedWork';
import {isAccountTransitionAbortError} from '@app/features/platform/state/AccountTransitionAbort';
import SessionManager from '@app/features/platform/state/AuthSession';
import {http} from '@app/features/platform/transport/RestTransport';
import {HttpError} from '@app/features/platform/types/EndpointError';
import type {RestResponse} from '@app/features/platform/types/TransportTypes';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as SlowmodeCommands from '@app/features/slowmode/commands/SlowmodeCommands';
import {SlowmodeRateLimitedModal} from '@app/features/slowmode/components/alerts/SlowmodeRateLimitedModal';
import ThreadGuilds from '@app/features/threads/state/ThreadGuilds';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {formatUserSettingsPath} from '@app/features/user/components/settings_utils/SettingsConstants';
import PrivacyPreferences from '@app/features/user/state/PrivacyPreferences';
import {Queue} from '@app/lib/list/ListQueue';
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
	accountKey: string | null;
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

export type MessageQueuePayload = SendMessagePayload;
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
	message?: string;
}

export interface PreparedSendAttachments {
	attachments?: Array<ApiAttachmentMetadata>;
	files?: Array<File>;
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
	retryAfterMs: number | null;
	automaticRetryDelayMs: number | null;
}

function resolveMessageRateLimitRetry(error: HttpError): MessageRateLimitRetry {
	const retryAfterMs = resolveRetryAfterMs(error);
	if (retryAfterMs === null) {
		return {retryAfterMs: null, automaticRetryDelayMs: null};
	}
	let automaticRetryDelayMs: number | null = null;
	if (retryAfterMs <= MESSAGE_SEND_RATE_LIMIT_MAX_AUTOMATIC_DELAY_MS) {
		automaticRetryDelayMs = retryAfterMs;
	}
	return {retryAfterMs, automaticRetryDelayMs};
}

function retryAfterMsToWholeSeconds(retryAfterMs: number | null): number | null {
	if (retryAfterMs === null) return null;
	return Math.ceil(retryAfterMs / 1000);
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
		AccountScopedWork.registerCancellation(() => this.handleAccountTransition());
	}

	private handleAccountTransition(): void {
		for (const controller of this.abortControllers.values()) {
			controller.abort(accountScopedWorkAbortError());
		}
		this.abortControllers.clear();
		this.cancelAllTextareaAttachmentUploads();
		this.localSendLimiters.clear();
		this.localSendReservations.clear();
		CloudUpload.clearAll();
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
		this.handleSendError(message.channelId, message.nonce, error, i18n, message.hasAttachments);
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
		return this.handleSend(message, completed);
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
		if (!canUsePresignedAttachmentUploads(files)) {
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
		const plansPromise = requestPresignedAttachmentUploads(
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
					const result = await uploadTextareaAttachmentViaPlan({
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
			if (!entry?.settled) {
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
		const plan = planTextareaAttachmentCancellation(this.textareaAttachmentUploads, attachmentIds);
		for (const {attachmentId, upload} of plan.cancelled) {
			upload.abortController.abort();
			this.textareaAttachmentUploads.delete(attachmentId);
		}
		for (const controller of plan.requestControllersToAbort) {
			controller.abort();
		}
		for (const channelId of plan.channelIds) {
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
		if (ThreadGuilds.purgedThreadIds.has(channelId)) {
			this.discardPurgedThreadSend(nonce);
			return null;
		}
		const abortController = new AbortController();
		this.abortControllers.set(nonce, abortController);
		try {
			const {attachments: rawAttachments, files} = await prepareAttachmentsForNonce(nonce, favoriteMemeId);
			if (!files?.length || !rawAttachments?.length) {
				return {attachments: rawAttachments, files};
			}
			if (!canUsePresignedAttachmentUploads(files)) {
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
				plans = await requestPresignedAttachmentUploads(channelId, rawAttachments, files, abortController.signal);
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
			const multipartUploadsToComplete: Array<MultipartAttachmentUpload> = [];
			let finalized: Array<ApiAttachmentMetadata>;
			try {
				finalized = await uploadAttachmentsViaPlans({
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
					await completeMultipartAttachmentUploads(channelId, multipartUploadsToComplete, abortController.signal);
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
		if (payload.accountKey !== SessionManager.currentAccountKey) {
			logger.debug(`Discarding a send to channel ${channelId} queued by another account`);
			CloudUpload.removeMessageUpload(nonce);
			completed(null, undefined, accountScopedWorkAbortError());
			return;
		}
		if (ThreadGuilds.purgedThreadIds.has(channelId)) {
			logger.debug(`Dropping message send to purged thread ${channelId}`);
			this.discardPurgedThreadSend(nonce);
			completed(null, undefined, new Error('Thread is no longer available'));
			return;
		}
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

	private discardPurgedThreadSend(nonce: string): void {
		const messageUpload = CloudUpload.getMessageUpload(nonce);
		if (!messageUpload) return;
		this.cancelTextareaAttachmentUploads(messageUpload.attachments.map((attachment) => attachment.id));
		CloudUpload.removeMessageUpload(nonce);
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

	private abortRemainingMultipartUploads(channelId: string, uploads: Array<MultipartAttachmentUpload>): void {
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
		this.handleRateLimitError(retryAfterMsToWholeSeconds(retry.retryAfterMs));
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
		if (isAccountTransitionAbortError(error)) {
			return;
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
		const conversationLimit = getApiErrorBody(error);
		if (
			(conversationLimit?.code === APIErrorCodes.NEW_CONVERSATIONS_LIMITED ||
				conversationLimit?.code === APIErrorCodes.ACCOUNT_LIMITED) &&
			conversationLimit.message
		) {
			const systemMessage = createSystemMessage(channelId, conversationLimit.message);
			MessageCommands.createOptimistic(channelId, systemMessage.toJSON());
			if (
				conversationLimit.code === APIErrorCodes.NEW_CONVERSATIONS_LIMITED &&
				channelId !== SelectedChannel.currentChannelId
			) {
				showDmActionErrorModal(error);
			}
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
		this.showErrorModal(error, channelId, hasAttachments);
	}

	private restoreFailedMessage(channelId: string, nonce: string): void {
		const messageUpload = CloudUpload.getMessageUpload(nonce);
		if (messageUpload === null) {
			MessageCommands.deleteOptimistic(channelId, nonce);
			return;
		}
		CloudUpload.restoreAttachmentsToTextarea(nonce);
		DraftCommands.createDraft(SessionManager.currentAccountKey, channelId, messageUpload.content ?? '');
		if (messageUpload.messageReference) {
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
			const retryAfterMs = SlowmodeCommands.clampSlowmodeRetryAfterMs(retry.retryAfterMs);
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
}

export default new MessageQueue();
