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

import {type ChannelID, createChannelID, type MessageID, type UserID} from '~/BrandedTypes';
import {
	isMessageTypeDeletable,
	MAX_MESSAGE_LENGTH_NON_PREMIUM,
	MAX_MESSAGE_LENGTH_PREMIUM,
	MessageFlags,
	MessageTypes,
	Permissions,
	SENDABLE_MESSAGE_FLAGS,
	TEXT_BASED_CHANNEL_TYPES,
} from '~/Constants';
import type {MessageRequest, MessageUpdateRequest} from '~/channel/ChannelModel';
import {
	CannotEditSystemMessageError,
	CannotSendEmptyMessageError,
	CannotSendMessageToNonTextChannelError,
	FileSizeTooLargeError,
	InputValidationError,
	UnknownMessageError,
} from '~/Errors';
import type {GuildResponse} from '~/guild/GuildModel';
import type {ICacheService} from '~/infrastructure/ICacheService';
import type {Channel, Message, User} from '~/Models';
import {getAttachmentMaxSize} from '~/utils/AttachmentUtils';
import {MESSAGE_NONCE_TTL} from './MessageHelpers';

export class MessageValidationService {
	constructor(private cacheService: ICacheService) {}

	ensureTextChannel(channel: Channel): void {
		if (!TEXT_BASED_CHANNEL_TYPES.has(channel.type)) {
			throw new CannotSendMessageToNonTextChannelError();
		}
	}

	validateMessageContent(data: MessageRequest | MessageUpdateRequest, user: User | null, isUpdate = false): void {
		const hasContent = data.content != null && data.content.trim().length > 0;
		const hasEmbeds = data.embeds && data.embeds.length > 0;
		const hasAttachments = data.attachments && data.attachments.length > 0;
		const hasFavoriteMeme = 'favorite_meme_id' in data && data.favorite_meme_id != null;
		const hasStickers = 'sticker_ids' in data && data.sticker_ids != null && data.sticker_ids.length > 0;
		const hasFlags = data.flags !== undefined && data.flags !== null;

		if (!hasContent && !hasEmbeds && !hasAttachments && !hasFavoriteMeme && !hasStickers && (!isUpdate || !hasFlags)) {
			throw new CannotSendEmptyMessageError();
		}

		this.validateContentLength(data.content, user);
	}

	validateContentLength(content: string | null | undefined, user: User | null): void {
		if (content == null) return;

		const maxLength = user?.isPremium() ? MAX_MESSAGE_LENGTH_PREMIUM : MAX_MESSAGE_LENGTH_NON_PREMIUM;
		if (content.length > maxLength) {
			throw InputValidationError.create('content', `Content must not exceed ${maxLength} characters`);
		}
	}

	validateMessageEditable(message: Message): void {
		const editableTypes: ReadonlySet<Message['type']> = new Set([MessageTypes.DEFAULT, MessageTypes.REPLY]);
		if (!editableTypes.has(message.type)) {
			throw new CannotEditSystemMessageError();
		}
	}

	calculateMessageFlags(data: {flags?: number; favorite_meme_id?: bigint | null}): number {
		let flags = data.flags ? data.flags & SENDABLE_MESSAGE_FLAGS : 0;

		if (data.favorite_meme_id) {
			flags |= MessageFlags.COMPACT_ATTACHMENTS;
		}

		return flags;
	}

	validateAttachmentSizes(attachments: Array<{size: number | bigint}>, user: User): void {
		const maxAttachmentSize = getAttachmentMaxSize(user.isPremium());

		for (const attachment of attachments) {
			if (Number(attachment.size) > maxAttachmentSize) {
				throw new FileSizeTooLargeError();
			}
		}
	}

	async findExistingMessage({
		userId,
		nonce,
		expectedChannelId,
	}: {
		userId: UserID;
		nonce?: string;
		expectedChannelId: ChannelID;
	}): Promise<Message | null> {
		if (!nonce) return null;

		const existingNonce = await this.cacheService.get<{channel_id: string; message_id: string}>(
			`message-nonce:${userId}:${nonce}`,
		);

		if (!existingNonce) return null;

		const cachedChannelId = createChannelID(BigInt(existingNonce.channel_id));
		if (cachedChannelId !== expectedChannelId) {
			throw new UnknownMessageError();
		}

		return null;
	}

	async cacheMessageNonce({
		userId,
		nonce,
		channelId,
		messageId,
	}: {
		userId: UserID;
		nonce: string;
		channelId: ChannelID;
		messageId: MessageID;
	}): Promise<void> {
		await this.cacheService.set(
			`message-nonce:${userId}:${nonce}`,
			{
				channel_id: channelId.toString(),
				message_id: messageId.toString(),
			},
			MESSAGE_NONCE_TTL,
		);
	}

	async canDeleteMessage({
		message,
		userId,
		guild,
		hasPermission,
	}: {
		message: Message;
		userId: UserID;
		guild: GuildResponse | null;
		hasPermission: (permission: bigint) => Promise<boolean>;
	}): Promise<boolean> {
		if (!isMessageTypeDeletable(message.type)) {
			return false;
		}

		const isAuthor = message.authorId === userId;
		if (!guild) return isAuthor;

		const canManageMessages =
			(await hasPermission(Permissions.SEND_MESSAGES)) && (await hasPermission(Permissions.MANAGE_MESSAGES));
		return isAuthor || canManageMessages;
	}
}
