// SPDX-License-Identifier: AGPL-3.0-or-later

import ChatInputSettings from '@app/features/messaging/state/ChatInputSettings';
import {convertEmoticonsToEmoji} from '@app/features/messaging/utils/EmoticonConversionUtils';
import {maybeSanitizeOutgoingMessage} from '@app/features/messaging/utils/UrlSanitizationUtils';
import {hasVisibleMessageContent} from '@app/features/messaging/utils/VisibleMessageContent';
import {MessageFlags} from '@fluxer/constants/src/ChannelConstants';
import type {
	AllowedMentions,
	MessageReference,
	MessageStickerItem,
} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';

const DEFAULT_ALLOWED_MENTIONS: AllowedMentions = {replied_user: true};

export {hasVisibleMessageContent} from '@app/features/messaging/utils/VisibleMessageContent';

export interface ApiAttachmentMetadata {
	id: string;
	filename: string;
	upload_filename?: string;
	file_size?: number;
	content_type?: string;
	title: string;
	description?: string;
	flags?: number;
	duration?: number;
	waveform?: string;
}

export interface ApiAttachmentReferenceMetadata {
	id: string;
	filename?: string;
	title?: string | null;
	description?: string | null;
	flags?: number;
}

export type ApiMessageEditAttachmentMetadata = ApiAttachmentMetadata | ApiAttachmentReferenceMetadata;

export interface MessageCreateRequest {
	content?: string | null;
	nonce?: string;
	attachments?: Array<ApiAttachmentMetadata>;
	allowed_mentions?: AllowedMentions;
	message_reference?: MessageReference;
	flags?: number;
	favorite_meme_id?: string;
	sticker_ids?: Array<string>;
	tts?: true;
}

export interface MessageEditRequest {
	content?: string;
	attachments?: Array<ApiMessageEditAttachmentMetadata>;
	allowed_mentions?: AllowedMentions;
	flags?: number;
}

export interface MessageEditPayload {
	content?: string;
	attachments?: Array<ApiMessageEditAttachmentMetadata>;
	allowedMentions?: AllowedMentions;
	flags?: number;
}

export interface MessageCreatePayload {
	content?: string | null;
	nonce?: string;
	attachments?: Array<ApiAttachmentMetadata>;
	allowedMentions?: AllowedMentions;
	messageReference?: MessageReference;
	flags?: number;
	favoriteMemeId?: string;
	stickers?: Array<MessageStickerItem>;
	tts?: boolean;
}

export interface NormalizedMessageContent {
	content: string;
	flags: number;
}

export function normalizeMessageContent(content: string): NormalizedMessageContent {
	const withoutSilent = removeSilentFlag(content);
	const converted = applyOutgoingEmoticonConversion(withoutSilent);
	const sanitized = maybeSanitizeOutgoingMessage(converted);
	const normalizedContent = hasVisibleMessageContent(sanitized) ? sanitized : '';
	const flags = getMessageFlags(content);
	return {content: normalizedContent, flags};
}

export function normalizeMessageEditContent(content: string): string {
	return applyOutgoingEmoticonConversion(content);
}

export function buildMessageCreateRequest(payload: MessageCreatePayload): MessageCreateRequest {
	const {content, nonce, attachments, allowedMentions, messageReference, flags, favoriteMemeId, stickers, tts} =
		payload;
	const requestBody: MessageCreateRequest = {};
	if (content != null && hasVisibleMessageContent(content)) {
		requestBody.content = content;
	}
	if (nonce != null) {
		requestBody.nonce = nonce;
	}
	if (attachments?.length) {
		requestBody.attachments = attachments;
	}
	if (messageReference != null || shouldIncludeAllowedMentions(allowedMentions)) {
		requestBody.allowed_mentions = allowedMentions;
	}
	if (messageReference) {
		requestBody.message_reference = messageReference;
	}
	if (flags != null) {
		requestBody.flags = flags;
	}
	if (favoriteMemeId) {
		requestBody.favorite_meme_id = favoriteMemeId;
	}
	if (stickers?.length) {
		requestBody.sticker_ids = stickers.map((sticker) => sticker.id);
	}
	if (tts) {
		requestBody.tts = true;
	}
	return requestBody;
}

export function buildMessageEditRequest(payload: MessageEditPayload): MessageEditRequest {
	const {content, attachments, allowedMentions, flags} = payload;
	const requestBody: MessageEditRequest = {};
	if (content !== undefined) {
		requestBody.content = normalizeMessageEditContent(content);
	}
	if (attachments !== undefined) {
		requestBody.attachments = attachments;
	}
	if (allowedMentions !== undefined) {
		requestBody.allowed_mentions = allowedMentions;
	}
	if (flags !== undefined) {
		requestBody.flags = flags;
	}
	return requestBody;
}

const isSilentMessage = (content: string): boolean => {
	return content.startsWith('@silent ');
};
const removeSilentFlag = (content: string): string => {
	return content.startsWith('@silent ') ? content.replace('@silent ', '') : content;
};
const applyOutgoingEmoticonConversion = (content: string): string => {
	return ChatInputSettings.convertEmoticons ? convertEmoticonsToEmoji(content) : content;
};
const getMessageFlags = (content: string): number => {
	let flags = 0;
	if (isSilentMessage(content)) {
		flags |= MessageFlags.SUPPRESS_NOTIFICATIONS;
	}
	return flags;
};
const shouldIncludeAllowedMentions = (allowedMentions?: AllowedMentions): boolean => {
	if (!allowedMentions) {
		return false;
	}
	const allowedKeys = Object.keys(allowedMentions) as Array<keyof AllowedMentions>;
	if (allowedKeys.length !== Object.keys(DEFAULT_ALLOWED_MENTIONS).length) {
		return true;
	}
	for (const key of allowedKeys) {
		if (allowedMentions[key] !== DEFAULT_ALLOWED_MENTIONS[key]) {
			return true;
		}
	}
	return false;
};
