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

import type {AttachmentID, ChannelID, MessageID, UserID} from '~/BrandedTypes';
import type {AttachmentLookupRow, MessageRow} from '~/database/CassandraTypes';
import type {Message} from '~/Models';
import type {ChannelDataRepository} from './ChannelDataRepository';
import {IMessageRepository, type ListMessagesOptions} from './IMessageRepository';
import {MessageAttachmentRepository} from './message/MessageAttachmentRepository';
import {MessageAuthorRepository} from './message/MessageAuthorRepository';
import {MessageDataRepository} from './message/MessageDataRepository';
import {MessageDeletionRepository} from './message/MessageDeletionRepository';

export class MessageRepository extends IMessageRepository {
	private dataRepo: MessageDataRepository;
	private deletionRepo: MessageDeletionRepository;
	private attachmentRepo: MessageAttachmentRepository;
	private authorRepo: MessageAuthorRepository;
	private channelDataRepo: ChannelDataRepository;

	constructor(channelDataRepo: ChannelDataRepository) {
		super();
		this.dataRepo = new MessageDataRepository();
		this.deletionRepo = new MessageDeletionRepository(this.dataRepo);
		this.attachmentRepo = new MessageAttachmentRepository();
		this.authorRepo = new MessageAuthorRepository(this.dataRepo, this.deletionRepo);
		this.channelDataRepo = channelDataRepo;
	}

	async listMessages(
		channelId: ChannelID,
		beforeMessageId?: MessageID,
		limit?: number,
		afterMessageId?: MessageID,
		options?: ListMessagesOptions,
	): Promise<Array<Message>> {
		return this.dataRepo.listMessages(channelId, beforeMessageId, limit, afterMessageId, options);
	}

	async getMessage(channelId: ChannelID, messageId: MessageID): Promise<Message | null> {
		return this.dataRepo.getMessage(channelId, messageId);
	}

	async upsertMessage(data: MessageRow, oldData?: MessageRow | null): Promise<Message> {
		const message = await this.dataRepo.upsertMessage(data, oldData);
		if (!oldData) {
			void this.channelDataRepo.updateLastMessageId(data.channel_id, data.message_id);
		}
		return message;
	}

	async deleteMessage(
		channelId: ChannelID,
		messageId: MessageID,
		authorId: UserID,
		pinnedTimestamp?: Date,
	): Promise<void> {
		return this.deletionRepo.deleteMessage(channelId, messageId, authorId, pinnedTimestamp);
	}

	async bulkDeleteMessages(channelId: ChannelID, messageIds: Array<MessageID>): Promise<void> {
		return this.deletionRepo.bulkDeleteMessages(channelId, messageIds);
	}

	async deleteAllChannelMessages(channelId: ChannelID): Promise<void> {
		return this.deletionRepo.deleteAllChannelMessages(channelId);
	}

	async listMessagesByAuthor(
		authorId: UserID,
		limit?: number,
		lastChannelId?: ChannelID,
		lastMessageId?: MessageID,
	): Promise<Array<{channelId: ChannelID; messageId: MessageID}>> {
		return this.authorRepo.listMessagesByAuthor(authorId, limit, lastChannelId, lastMessageId);
	}

	async deleteMessagesByAuthor(
		authorId: UserID,
		channelIds?: Array<ChannelID>,
		messageIds?: Array<MessageID>,
	): Promise<void> {
		return this.authorRepo.deleteMessagesByAuthor(authorId, channelIds, messageIds);
	}

	async anonymizeMessage(channelId: ChannelID, messageId: MessageID, newAuthorId: UserID): Promise<void> {
		return this.authorRepo.anonymizeMessage(channelId, messageId, newAuthorId);
	}

	async authorHasMessage(authorId: UserID, channelId: ChannelID, messageId: MessageID): Promise<boolean> {
		return this.authorRepo.hasMessageByAuthor(authorId, channelId, messageId);
	}

	async lookupAttachmentByChannelAndFilename(
		channelId: ChannelID,
		attachmentId: AttachmentID,
		filename: string,
	): Promise<MessageID | null> {
		return this.attachmentRepo.lookupAttachmentByChannelAndFilename(channelId, attachmentId, filename);
	}

	async listChannelAttachments(channelId: ChannelID): Promise<Array<AttachmentLookupRow>> {
		return this.attachmentRepo.listChannelAttachments(channelId);
	}
}
