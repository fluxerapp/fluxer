// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AttachmentID, ChannelID, GuildID, MessageID, UserID} from '@app/api/BrandedTypes';
import type {GuildChannelListMode} from '@app/api/channel/repositories/IChannelDataRepository';
import {IChannelRepositoryAggregate} from '@app/api/channel/repositories/IChannelRepositoryAggregate';
import type {UpsertMessageOptions} from '@app/api/channel/repositories/IMessageRepository';
import type {ChannelRow} from '@app/api/database/types/ChannelTypes';
import type {MessageRow} from '@app/api/database/types/MessageTypes';
import type {Channel} from '@app/api/models/Channel';
import type {Message} from '@app/api/models/Message';

export abstract class IChannelRepository extends IChannelRepositoryAggregate {
	abstract findUnique(channelId: ChannelID): Promise<Channel | null>;

	abstract upsert(data: ChannelRow): Promise<Channel>;

	abstract delete(channelId: ChannelID, guildId?: GuildID, type?: number): Promise<void>;

	abstract listGuildChannels(guildId: GuildID, mode: GuildChannelListMode): Promise<Array<Channel>>;

	abstract listChannels(channelIds: Array<ChannelID>): Promise<Array<Channel>>;

	abstract listMessages(
		channelId: ChannelID,
		beforeMessageId?: MessageID,
		limit?: number,
		afterMessageId?: MessageID,
	): Promise<Array<Message>>;

	abstract getMessage(channelId: ChannelID, messageId: MessageID): Promise<Message | null>;

	abstract upsertMessage(data: MessageRow, oldData?: MessageRow | null, opts?: UpsertMessageOptions): Promise<Message>;

	abstract deleteMessage(
		channelId: ChannelID,
		messageId: MessageID,
		authorId: UserID,
		pinnedTimestamp?: Date,
	): Promise<void>;

	abstract bulkDeleteMessages(channelId: ChannelID, messageIds: Array<MessageID>): Promise<void>;

	abstract lookupAttachmentByChannelAndFilename(
		channelId: ChannelID,
		attachmentId: AttachmentID,
		filename: string,
	): Promise<MessageID | null>;

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

	abstract anonymizeMessage(channelId: ChannelID, messageId: MessageID, newAuthorId: UserID): Promise<void>;

	abstract deleteAllChannelMessages(channelId: ChannelID): Promise<void>;

	abstract updateEmbeds(message: Message): Promise<void>;
}
