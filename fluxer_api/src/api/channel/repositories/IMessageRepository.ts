// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AttachmentID, ChannelID, MessageID, UserID} from '@app/api/BrandedTypes';
import type {MessageRow} from '@app/api/database/types/MessageTypes';
import type {Message} from '@app/api/models/Message';

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

	abstract updateEmbeds(message: Message): Promise<void>;

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
		lastMessageId?: MessageID,
	): Promise<
		Array<{
			channelId: ChannelID;
			messageId: MessageID;
		}>
	>;

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
}
