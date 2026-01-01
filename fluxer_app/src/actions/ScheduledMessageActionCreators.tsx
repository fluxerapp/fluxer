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

import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import * as DraftActionCreators from '~/actions/DraftActionCreators';
import * as MessageActionCreators from '~/actions/MessageActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as SlowmodeActionCreators from '~/actions/SlowmodeActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import {APIErrorCodes} from '~/Constants';
import {FeatureTemporarilyDisabledModal} from '~/components/alerts/FeatureTemporarilyDisabledModal';
import {FileSizeTooLargeModal} from '~/components/alerts/FileSizeTooLargeModal';
import {MessageSendFailedModal} from '~/components/alerts/MessageSendFailedModal';
import {MessageSendTooQuickModal} from '~/components/alerts/MessageSendTooQuickModal';
import {NSFWContentRejectedModal} from '~/components/alerts/NSFWContentRejectedModal';
import {SlowmodeRateLimitedModal} from '~/components/alerts/SlowmodeRateLimitedModal';
import {Endpoints} from '~/Endpoints';
import {CloudUpload} from '~/lib/CloudUpload';
import http, {type HttpError, type HttpResponse} from '~/lib/HttpClient';
import {Logger} from '~/lib/Logger';
import type {AllowedMentions} from '~/records/MessageRecord';
import {
	type ScheduledAttachment,
	type ScheduledMessagePayload,
	ScheduledMessageRecord,
	type ScheduledMessageResponse,
} from '~/records/ScheduledMessageRecord';
import ScheduledMessagesStore from '~/stores/ScheduledMessagesStore';
import {prepareAttachmentsForNonce} from '~/utils/MessageAttachmentUtils';
import {
	type ApiAttachmentMetadata,
	buildMessageCreateRequest,
	type MessageCreateRequest,
	type MessageReference,
	type MessageStickerItem,
	type NormalizedMessageContent,
	normalizeMessageContent,
} from '~/utils/MessageRequestUtils';
import * as MessageSubmitUtils from '~/utils/MessageSubmitUtils';
import * as SnowflakeUtils from '~/utils/SnowflakeUtils';
import {TypingUtils} from '~/utils/TypingUtils';

const logger = new Logger('ScheduledMessages');

type ScheduledMessageRequest = MessageCreateRequest & {
	scheduled_local_at: string;
	timezone: string;
};

interface ApiErrorBody {
	code?: number | string;
	retry_after?: number;
	message?: string;
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

export const fetchScheduledMessages = async (): Promise<Array<ScheduledMessageRecord>> => {
	logger.debug('Fetching scheduled messages');
	ScheduledMessagesStore.fetchStart();

	try {
		const response = await http.get<Array<ScheduledMessageResponse>>({
			url: Endpoints.USER_SCHEDULED_MESSAGES,
		});
		const data = response.body ?? [];
		const messages = data.map(ScheduledMessageRecord.fromResponse);
		ScheduledMessagesStore.fetchSuccess(messages);
		logger.debug('Scheduled messages fetched successfully');
		return messages;
	} catch (error) {
		ScheduledMessagesStore.fetchError();
		logger.error('Failed to fetch scheduled messages:', error);
		throw error;
	}
};

export const scheduleMessage = async (i18n: I18n, params: ScheduleMessageParams): Promise<ScheduledMessageRecord> => {
	logger.debug('Scheduling message', params);
	const nonce = SnowflakeUtils.fromTimestamp(Date.now());
	const normalized = normalizeMessageContent(params.content, params.favoriteMemeId);
	const allowedMentions: AllowedMentions = {replied_user: params.replyMentioning ?? true};

	if (params.hasAttachments) {
		MessageSubmitUtils.claimMessageAttachments(
			params.channelId,
			nonce,
			params.content,
			params.messageReference,
			params.replyMentioning,
			params.favoriteMemeId,
		);
	}

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
		allowedMentions,
		messageReference: params.messageReference,
		flags: normalized.flags,
		favoriteMemeId: params.favoriteMemeId,
		stickers: params.stickers,
		tts: params.tts,
	});

	const payload: ScheduledMessageRequest = {
		...requestBody,
		scheduled_local_at: params.scheduledLocalAt,
		timezone: params.timezone,
	};

	try {
		const response = await scheduleMessageRequest(params.channelId, payload, files, nonce);
		const record = ScheduledMessageRecord.fromResponse(response.body);
		ScheduledMessagesStore.upsert(record);
		DraftActionCreators.deleteDraft(params.channelId);
		TypingUtils.clear(params.channelId);
		MessageActionCreators.stopReply(params.channelId);
		if (params.hasAttachments) {
			CloudUpload.removeMessageUpload(nonce);
		}

		ToastActionCreators.createToast({
			type: 'success',
			children: i18n._(msg`Scheduled message for ${formatScheduledLabel(params.scheduledLocalAt, params.timezone)}`),
		});

		return record;
	} catch (error) {
		handleScheduleError(
			i18n,
			error as HttpError,
			params.channelId,
			nonce,
			params.content,
			params.messageReference,
			params.replyMentioning,
			params.hasAttachments,
		);
		throw error;
	}
};

export const updateScheduledMessage = async (
	i18n: I18n,
	params: UpdateScheduledMessageParams,
): Promise<ScheduledMessageRecord> => {
	logger.debug('Updating scheduled message', params);
	const requestBody: ScheduledMessageRequest = {
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

	try {
		const response = await http.patch<ScheduledMessageResponse>({
			url: Endpoints.USER_SCHEDULED_MESSAGE(params.scheduledMessageId),
			body: requestBody,
			rejectWithError: true,
		});
		const record = ScheduledMessageRecord.fromResponse(response.body);
		ScheduledMessagesStore.upsert(record);
		DraftActionCreators.deleteDraft(params.channelId);
		TypingUtils.clear(params.channelId);
		MessageActionCreators.stopReply(params.channelId);
		ToastActionCreators.createToast({
			type: 'success',
			children: i18n._(
				msg`Updated scheduled message for ${formatScheduledLabel(params.scheduledLocalAt, params.timezone)}`,
			),
		});
		return record;
	} catch (error) {
		logger.error('Failed to update scheduled message', error);
		throw error;
	}
};

export const cancelScheduledMessage = async (i18n: I18n, scheduledMessageId: string): Promise<void> => {
	logger.debug('Canceling scheduled message', scheduledMessageId);
	try {
		await http.delete({url: Endpoints.USER_SCHEDULED_MESSAGE(scheduledMessageId)});
		ScheduledMessagesStore.remove(scheduledMessageId);
		ToastActionCreators.createToast({
			type: 'success',
			children: i18n._(msg`Removed scheduled message`),
		});
	} catch (error) {
		logger.error('Failed to cancel scheduled message', error);
		throw error;
	}
};

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
	DraftActionCreators.createDraft(channelId, content);
	if (messageReference && replyMentioning !== undefined) {
		MessageActionCreators.startReply(channelId, messageReference.message_id, replyMentioning);
	}
}

async function scheduleMessageRequest(
	channelId: string,
	payload: ScheduledMessageRequest,
	files?: Array<File>,
	nonce?: string,
): Promise<HttpResponse<ScheduledMessageResponse>> {
	const abortController = new AbortController();
	try {
		if (files?.length) {
			return await scheduleMultipartMessage(channelId, payload, files, abortController.signal, nonce);
		}
		return await http.post<ScheduledMessageResponse>({
			url: Endpoints.CHANNEL_MESSAGE_SCHEDULE(channelId),
			body: payload,
			signal: abortController.signal,
			rejectWithError: true,
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
): Promise<HttpResponse<ScheduledMessageResponse>> {
	const formData = new FormData();
	formData.append('payload_json', JSON.stringify(payload));

	files.forEach((file, index) => {
		formData.append(`files[${index}]`, file);
	});

	return http.post<ScheduledMessageResponse>({
		url: Endpoints.CHANNEL_MESSAGE_SCHEDULE(channelId),
		body: formData,
		signal,
		rejectWithError: true,
		onRequestProgress: nonce
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
	return typeof error?.body === 'object' && error.body !== null ? (error.body as ApiErrorBody) : undefined;
};

function handleScheduleError(
	i18n: I18n,
	error: HttpError,
	channelId: string,
	nonce: string,
	content: string,
	messageReference?: MessageReference,
	replyMentioning?: boolean,
	hadAttachments?: boolean,
): void {
	restoreDraftAfterScheduleFailure(channelId, nonce, content, messageReference, replyMentioning, hadAttachments);

	if (isRateLimitError(error)) {
		handleScheduleRateLimit(i18n, error);
		return;
	}

	if (isSlowmodeError(error)) {
		const retryAfter = Math.ceil(getApiErrorBody(error)?.retry_after ?? 0);
		const timestamp = Date.now() - retryAfter * 1000;
		SlowmodeActionCreators.updateSlowmodeTimestamp(channelId, timestamp);
		ModalActionCreators.push(modal(() => <SlowmodeRateLimitedModal retryAfter={retryAfter} />));
		return;
	}

	if (isFeatureDisabledError(error)) {
		ModalActionCreators.push(modal(() => <FeatureTemporarilyDisabledModal />));
		return;
	}

	if (isExplicitContentError(error)) {
		ModalActionCreators.push(modal(() => <NSFWContentRejectedModal />));
		return;
	}

	if (isFileTooLargeError(error)) {
		ModalActionCreators.push(modal(() => <FileSizeTooLargeModal />));
		return;
	}

	ModalActionCreators.push(modal(() => <MessageSendFailedModal />));
}

function handleScheduleRateLimit(_i18n: I18n, error: HttpError): void {
	const retryAfterSeconds = getApiErrorBody(error)?.retry_after ?? 0;
	ModalActionCreators.push(
		modal(() => <MessageSendTooQuickModal retryAfter={retryAfterSeconds} onRetry={undefined} />),
	);
	logger.warn('Scheduled message rate limited, retry after', retryAfterSeconds);
}

function isRateLimitError(error: HttpError): boolean {
	return error?.status === 429;
}

function isSlowmodeError(error: HttpError): boolean {
	return error?.status === 400 && getApiErrorBody(error)?.code === APIErrorCodes.SLOWMODE_RATE_LIMITED;
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
