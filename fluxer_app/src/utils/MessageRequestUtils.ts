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

import {MessageFlags} from '~/Constants';
import type {AllowedMentions, MessageReference, MessageStickerItem} from '~/records/MessageRecord';

export type {MessageReference, MessageStickerItem};

const DEFAULT_ALLOWED_MENTIONS: AllowedMentions = {replied_user: true};

export interface ApiAttachmentMetadata {
	id: string;
	filename: string;
	title: string;
	description?: string;
	flags?: number;
}

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
	attachments?: Array<ApiAttachmentMetadata>;
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

export const normalizeMessageContent = (content: string, favoriteMemeId?: string): NormalizedMessageContent => {
	const sanitized = removeSilentFlag(content);
	const flags = getMessageFlags(content, favoriteMemeId);
	return {content: sanitized, flags};
};

export const buildMessageCreateRequest = (payload: MessageCreatePayload): MessageCreateRequest => {
	const {content, nonce, attachments, allowedMentions, messageReference, flags, favoriteMemeId, stickers, tts} =
		payload;

	const requestBody: MessageCreateRequest = {};

	if (content != null) {
		requestBody.content = content;
	}

	if (nonce != null) {
		requestBody.nonce = nonce;
	}

	if (attachments?.length) {
		requestBody.attachments = attachments;
	}

	if (shouldIncludeAllowedMentions(allowedMentions)) {
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
};

const isSilentMessage = (content: string): boolean => {
	return content.startsWith('@silent ');
};

const removeSilentFlag = (content: string): string => {
	return content.startsWith('@silent ') ? content.replace('@silent ', '') : content;
};

const getMessageFlags = (content: string, favoriteMemeId?: string): number => {
	let flags = 0;

	if (isSilentMessage(content)) {
		flags |= MessageFlags.SUPPRESS_NOTIFICATIONS;
	}

	if (favoriteMemeId) {
		flags |= MessageFlags.COMPACT_ATTACHMENTS;
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
