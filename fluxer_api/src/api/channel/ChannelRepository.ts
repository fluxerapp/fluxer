// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AttachmentID, ChannelID, EmojiID, GuildID, MessageID, UserID} from '../BrandedTypes';
import type {ChannelRow} from '../database/types/ChannelTypes';
import type {MessageRow} from '../database/types/MessageTypes';
import type {RequestCache} from '../middleware/RequestCacheMiddleware';
import type {Channel} from '../models/Channel';
import type {Message} from '../models/Message';
import type {MessageReaction} from '../models/MessageReaction';
import {IChannelRepository} from './IChannelRepository';
import {ChannelRepository as NewChannelRepository} from './repositories/ChannelRepository';

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

	async findUnique(channelId: ChannelID): Promise<Channel | null> {
		return this.repository.channelData.findUnique(channelId);
	}

	async upsert(data: ChannelRow): Promise<Channel> {
		return this.repository.channelData.upsert(data);
	}

	async updateLastMessageId(channelId: ChannelID, messageId: MessageID): Promise<void> {
		return this.repository.channelData.updateLastMessageId(channelId, messageId);
	}

	async delete(channelId: ChannelID, guildId?: GuildID): Promise<void> {
		return this.repository.channelData.delete(channelId, guildId);
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

	async upsertMessage(data: MessageRow, oldData?: MessageRow | null): Promise<Message> {
		return this.repository.messages.upsertMessage(data, oldData);
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

	async listChannelPins(channelId: ChannelID, beforePinnedTimestamp: Date, limit?: number): Promise<Array<Message>> {
		return this.repository.messageInteractions.listChannelPins(channelId, beforePinnedTimestamp, limit);
	}

	async listMessageReactions(channelId: ChannelID, messageId: MessageID): Promise<Array<MessageReaction>> {
		return this.repository.messageInteractions.listMessageReactions(channelId, messageId);
	}

	async listReactionUsers(
		channelId: ChannelID,
		messageId: MessageID,
		emojiName: string,
		limit?: number,
		after?: UserID,
		emojiId?: EmojiID,
	): Promise<Array<MessageReaction>> {
		return this.repository.messageInteractions.listReactionUsers(
			channelId,
			messageId,
			emojiName,
			limit,
			after,
			emojiId,
		);
	}

	async addReaction(
		channelId: ChannelID,
		messageId: MessageID,
		userId: UserID,
		emojiName: string,
		emojiId?: EmojiID,
		emojiAnimated?: boolean,
	): Promise<MessageReaction> {
		return this.repository.messageInteractions.addReaction(
			channelId,
			messageId,
			userId,
			emojiName,
			emojiId,
			emojiAnimated,
		);
	}

	async removeReaction(
		channelId: ChannelID,
		messageId: MessageID,
		userId: UserID,
		emojiName: string,
		emojiId?: EmojiID,
	): Promise<void> {
		return this.repository.messageInteractions.removeReaction(channelId, messageId, userId, emojiName, emojiId);
	}

	async removeAllReactions(channelId: ChannelID, messageId: MessageID): Promise<void> {
		return this.repository.messageInteractions.removeAllReactions(channelId, messageId);
	}

	async removeAllReactionsForEmoji(
		channelId: ChannelID,
		messageId: MessageID,
		emojiName: string,
		emojiId?: EmojiID,
	): Promise<void> {
		return this.repository.messageInteractions.removeAllReactionsForEmoji(channelId, messageId, emojiName, emojiId);
	}

	async countReactionUsers(
		channelId: ChannelID,
		messageId: MessageID,
		emojiName: string,
		emojiId?: EmojiID,
	): Promise<number> {
		return this.repository.messageInteractions.countReactionUsers(channelId, messageId, emojiName, emojiId);
	}

	async countUniqueReactions(channelId: ChannelID, messageId: MessageID): Promise<number> {
		return this.repository.messageInteractions.countUniqueReactions(channelId, messageId);
	}

	async checkUserReactionExists(
		channelId: ChannelID,
		messageId: MessageID,
		userId: UserID,
		emojiName: string,
		emojiId?: EmojiID,
	): Promise<boolean> {
		return this.repository.messageInteractions.checkUserReactionExists(
			channelId,
			messageId,
			userId,
			emojiName,
			emojiId,
		);
	}

	async listGuildChannels(guildId: GuildID): Promise<Array<Channel>> {
		return this.repository.channelData.listGuildChannels(guildId);
	}

	async listChannels(channelIds: Array<ChannelID>): Promise<Array<Channel>> {
		return this.repository.channelData.listChannels(channelIds);
	}

	async countGuildChannels(guildId: GuildID): Promise<number> {
		return this.repository.channelData.countGuildChannels(guildId);
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

	async deleteMessagesByAuthor(
		authorId: UserID,
		channelIds?: Array<ChannelID>,
		messageIds?: Array<MessageID>,
	): Promise<void> {
		return this.repository.messages.deleteMessagesByAuthor(authorId, channelIds, messageIds);
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
