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

import {MessageStates, MessageTypes} from '~/Constants';
import {CloudUpload} from '~/lib/CloudUpload';
import type {AllowedMentions, MessageReference, MessageStickerItem} from '~/records/MessageRecord';
import {MessageRecord} from '~/records/MessageRecord';
import type {UserRecord} from '~/records/UserRecord';
import {normalizeMessageContent} from '~/utils/MessageRequestUtils';

interface MessageSubmitData {
	content: string;
	channelId: string;
	nonce: string;
	currentUser: UserRecord;
	referencedMessage?: MessageRecord | null;
	replyMentioning?: boolean;
	stickers?: Array<MessageStickerItem>;
	favoriteMemeId?: string;
}

interface UploadingAttachment {
	id: string;
	filename: string;
	title?: string;
	size: number;
	url: string;
	proxy_url: string;
	content_type: string;
	flags: number;
}

export function createUploadingAttachments(
	claimedAttachments: Array<{filename: string; file: {size: number}}>,
): Array<UploadingAttachment> {
	if (claimedAttachments.length === 0) {
		return [];
	}

	return [
		{
			id: 'uploading',
			filename:
				claimedAttachments.length === 1
					? claimedAttachments[0].filename
					: `Uploading ${claimedAttachments.length} Files`,
			title: claimedAttachments.length === 1 ? claimedAttachments[0].filename : undefined,
			size: claimedAttachments.reduce((total, att) => total + att.file.size, 0),
			url: '',
			proxy_url: '',
			content_type: 'application/octet-stream',
			flags: 0x1000,
		},
	];
}

export function createOptimisticMessage(
	data: MessageSubmitData,
	attachments: Array<UploadingAttachment>,
): MessageRecord {
	const normalized = normalizeMessageContent(data.content, data.favoriteMemeId);
	const content = normalized.content;
	const flags = normalized.flags;

	return new MessageRecord({
		id: data.nonce,
		channel_id: data.channelId,
		author: data.currentUser,
		type: data.referencedMessage ? MessageTypes.REPLY : MessageTypes.DEFAULT,
		flags,
		pinned: false,
		mention_everyone: false,
		content,
		timestamp: new Date().toISOString(),
		mentions: [...(data.referencedMessage && data.replyMentioning ? [data.referencedMessage.author] : [])],
		message_reference: data.referencedMessage
			? {channel_id: data.channelId, message_id: data.referencedMessage.id, type: 0}
			: undefined,
		state: MessageStates.SENDING,
		nonce: data.nonce,
		attachments,
	});
}

export function prepareMessageReference(
	channelId: string,
	referencedMessage?: MessageRecord | null,
): MessageReference | undefined {
	return referencedMessage ? {channel_id: channelId, message_id: referencedMessage.id, type: 0} : undefined;
}

export function claimMessageAttachments(
	channelId: string,
	nonce: string,
	content: string,
	messageReference?: MessageReference,
	replyMentioning?: boolean,
	favoriteMemeId?: string,
): Array<{filename: string; file: {size: number}}> {
	const normalized = normalizeMessageContent(content, favoriteMemeId);
	const allowedMentions: AllowedMentions = {replied_user: replyMentioning ?? true};
	return CloudUpload.claimAttachmentsForMessage(channelId, nonce, undefined, {
		content: normalized.content,
		messageReference,
		allowedMentions,
		flags: normalized.flags,
	});
}
