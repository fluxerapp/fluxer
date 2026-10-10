// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AttachmentID, ChannelID, GuildID, MessageID, UserID} from '@app/api/BrandedTypes';
import {IChannelRepository} from '@app/api/channel/IChannelRepository';
import {ChannelRepository as NewChannelRepository} from '@app/api/channel/repositories/ChannelRepository';
import type {GuildChannelListMode} from '@app/api/channel/repositories/IChannelDataRepository';
import type {UpsertMessageOptions} from '@app/api/channel/repositories/IMessageRepository';
import type {ChannelRow} from '@app/api/database/types/ChannelTypes';
import type {MessageRow} from '@app/api/database/types/MessageTypes';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {Channel} from '@app/api/models/Channel';
import type {Message} from '@app/api/models/Message';

export class ChannelRepository extends IChannelRepository {
	private repository: NewChannelRepository;

	constructor(requestCache?: RequestCache) {
		super();
		this.repository = new NewChannelRepository(requestCache);
	}

	get channelData() {
		return this.repository.channelData;
	}

	get messages() {
		return this.repository.messages;
	}

	get messageInteractions() {
		return this.repository.messageInteractions;
	}

	get threads() {
		return this.repository.threads;
	}

	get crossposts() {
		return this.repository.crossposts;
	}

	async findUnique(channelId: ChannelID): Promise<Channel | null> {
		return this.repository.channelData.findUnique(channelId);
	}

	async upsert(data: ChannelRow): Promise<Channel> {
		return this.repository.channelData.upsert(data);
	}

	async delete(channelId: ChannelID, guildId?: GuildID, type?: number): Promise<void> {
		return this.repository.channelData.delete(channelId, guildId, type);
	}

	async listMessages(
		channelId: ChannelID,
		beforeMessageId?: MessageID,
		limit?: number,
		afterMessageId?: MessageID,
	): Promise<Array<Message>> {
		return this.repository.messages.listMessages(channelId, beforeMessageId, limit, afterMessageId);
	}

	async getMessage(channelId: ChannelID, messageId: MessageID): Promise<Message | null> {
		return this.repository.messages.getMessage(channelId, messageId);
	}

	async upsertMessage(data: MessageRow, oldData?: MessageRow | null, opts?: UpsertMessageOptions): Promise<Message> {
		return this.repository.messages.upsertMessage(data, oldData, opts);
	}

	async deleteMessage(
		channelId: ChannelID,
		messageId: MessageID,
		authorId: UserID,
		pinnedTimestamp?: Date,
	): Promise<void> {
		return this.repository.messages.deleteMessage(channelId, messageId, authorId, pinnedTimestamp);
	}

	async bulkDeleteMessages(channelId: ChannelID, messageIds: Array<MessageID>): Promise<void> {
		return this.repository.messages.bulkDeleteMessages(channelId, messageIds);
	}

	async listGuildChannels(guildId: GuildID, mode: GuildChannelListMode): Promise<Array<Channel>> {
		return this.repository.channelData.listGuildChannels(guildId, mode);
	}

	async listChannels(channelIds: Array<ChannelID>): Promise<Array<Channel>> {
		return this.repository.channelData.listChannels(channelIds);
	}

	async lookupAttachmentByChannelAndFilename(
		channelId: ChannelID,
		attachmentId: AttachmentID,
		filename: string,
	): Promise<MessageID | null> {
		return this.repository.messages.lookupAttachmentByChannelAndFilename(channelId, attachmentId, filename);
	}

	async listMessagesByAuthor(
		authorId: UserID,
		limit?: number,
		lastMessageId?: MessageID,
	): Promise<
		Array<{
			channelId: ChannelID;
			messageId: MessageID;
		}>
	> {
		return this.repository.messages.listMessagesByAuthor(authorId, limit, lastMessageId);
	}

	async anonymizeMessage(channelId: ChannelID, messageId: MessageID, newAuthorId: UserID): Promise<void> {
		return this.repository.messages.anonymizeMessage(channelId, messageId, newAuthorId);
	}

	async deleteAllChannelMessages(channelId: ChannelID): Promise<void> {
		return this.repository.messages.deleteAllChannelMessages(channelId);
	}

	async updateEmbeds(message: Message): Promise<void> {
		return this.repository.messages.updateEmbeds(message);
	}
}
