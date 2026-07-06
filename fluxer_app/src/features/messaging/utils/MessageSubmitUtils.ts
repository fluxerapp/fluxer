// SPDX-License-Identifier: AGPL-3.0-or-later

import {Message} from '@app/features/messaging/models/MessagingMessage';
import {
	UploadingAttachment,
	type UploadingAttachmentListOptions,
} from '@app/features/messaging/models/UploadingAttachment';
import {CloudUpload} from '@app/features/messaging/upload/CloudUpload';
import {normalizeMessageContent} from '@app/features/messaging/utils/MessageRequestUtils';
import type {User} from '@app/features/user/models/User';
import {MessageStates, MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import type {
	AllowedMentions,
	MessageAttachment,
	MessageReference,
	MessageStickerItem,
} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import type {PollRequest, PollResponse} from '@fluxer/schema/src/domains/message/PollSchemas';
import * as SnowflakeUtils from '@fluxer/snowflake/src/SnowflakeUtils';

interface MessageSubmitData {
	content: string;
	channelId: string;
	nonce: string;
	currentUser: User;
	referencedMessage?: Message | null;
	replyMentioning?: boolean;
	stickers?: Array<MessageStickerItem>;
	favoriteMemeId?: string;
	poll?: PollRequest;
}

function createOptimisticPoll(nonce: string, poll: PollRequest): PollResponse {
	const now = Date.now();
	const expiresAt =
		poll.expires_at instanceof Date
			? poll.expires_at.toISOString()
			: (poll.expires_at ?? new Date(now + (poll.duration_seconds ?? 0) * 1000).toISOString());
	return {
		id: nonce,
		title: poll.title,
		options: poll.options.map((option, index) => ({
			id: SnowflakeUtils.fromTimestamp(now + index + 1),
			text: option.text,
			attachment_id: option.attachment_id != null ? option.attachment_id.toString() : null,
			vote_count: 0,
		})),
		expires_at: expiresAt,
		closed: false,
		anonymous: poll.anonymous ?? false,
		allow_ranked_choice: poll.allow_ranked_choice ?? false,
		allow_custom_answers: poll.allow_custom_answers ?? false,
	};
}

export function createUploadingAttachments(
	claimedAttachments: Array<{
		filename: string;
		file: {
			size: number;
		};
	}>,
	options: UploadingAttachmentListOptions,
): Array<MessageAttachment> {
	return UploadingAttachment.fromClaimedAttachments(claimedAttachments, options);
}

export function createOptimisticMessage(data: MessageSubmitData, attachments: Array<MessageAttachment>): Message {
	const normalized = normalizeMessageContent(data.content, data.favoriteMemeId);
	const content = normalized.content;
	const flags = normalized.flags;
	return new Message({
		id: data.nonce,
		channel_id: data.channelId,
		author: data.currentUser.toJSON(),
		type: data.referencedMessage ? MessageTypes.REPLY : MessageTypes.DEFAULT,
		flags,
		pinned: false,
		mention_everyone: false,
		content,
		timestamp: new Date().toISOString(),
		mentions: [...(data.referencedMessage && data.replyMentioning ? [data.referencedMessage.author.toJSON()] : [])],
		message_reference: data.referencedMessage
			? {channel_id: data.channelId, message_id: data.referencedMessage.id, type: 0}
			: undefined,
		state: MessageStates.SENDING,
		nonce: data.nonce,
		attachments,
		poll: data.poll ? createOptimisticPoll(data.nonce, data.poll) : undefined,
		_allowedMentions: data.referencedMessage ? {replied_user: data.replyMentioning ?? true} : undefined,
	});
}

export function prepareMessageReference(
	channelId: string,
	referencedMessage?: Message | null,
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
): Array<{
	filename: string;
	file: {
		size: number;
	};
}> {
	const normalized = normalizeMessageContent(content, favoriteMemeId);
	const allowedMentions: AllowedMentions = {replied_user: replyMentioning ?? true};
	return CloudUpload.claimAttachmentsForMessage(channelId, nonce, undefined, {
		content: normalized.content,
		messageReference,
		allowedMentions,
		flags: normalized.flags,
	});
}
