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

export interface ListMessagesOptions {
	restrictToBeforeBucket?: boolean;
	immediateAfter?: boolean;
}

export abstract class IMessageRepository {
	abstract listMessages(
		channelId: ChannelID,
		beforeMessageId?: MessageID,
		limit?: number,
		afterMessageId?: MessageID,
		options?: ListMessagesOptions,
	): Promise<Array<Message>>;
	abstract getMessage(channelId: ChannelID, messageId: MessageID): Promise<Message | null>;
	abstract upsertMessage(data: MessageRow, oldData?: MessageRow | null): Promise<Message>;
	abstract deleteMessage(
		channelId: ChannelID,
		messageId: MessageID,
		authorId: UserID,
		pinnedTimestamp?: Date,
	): Promise<void>;
	abstract bulkDeleteMessages(channelId: ChannelID, messageIds: Array<MessageID>): Promise<void>;
	abstract deleteAllChannelMessages(channelId: ChannelID): Promise<void>;
	abstract listMessagesByAuthor(
		authorId: UserID,
		limit?: number,
		lastChannelId?: ChannelID,
		lastMessageId?: MessageID,
	): Promise<Array<{channelId: ChannelID; messageId: MessageID}>>;
	abstract deleteMessagesByAuthor(
		authorId: UserID,
		channelIds?: Array<ChannelID>,
		messageIds?: Array<MessageID>,
	): Promise<void>;
	abstract anonymizeMessage(channelId: ChannelID, messageId: MessageID, newAuthorId: UserID): Promise<void>;
	abstract authorHasMessage(authorId: UserID, channelId: ChannelID, messageId: MessageID): Promise<boolean>;
	abstract lookupAttachmentByChannelAndFilename(
		channelId: ChannelID,
		attachmentId: AttachmentID,
		filename: string,
	): Promise<MessageID | null>;
	abstract listChannelAttachments(channelId: ChannelID): Promise<Array<AttachmentLookupRow>>;
}
