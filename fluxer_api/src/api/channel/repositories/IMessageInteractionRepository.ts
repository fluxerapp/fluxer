// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, EmojiID, MessageID, UserID} from '../../BrandedTypes';
import type {Message} from '../../models/Message';
import type {MessageReaction} from '../../models/MessageReaction';

export abstract class IMessageInteractionRepository {
	abstract listChannelPins(channelId: ChannelID, beforePinnedTimestamp: Date, limit?: number): Promise<Array<Message>>;

	abstract addChannelPin(channelId: ChannelID, messageId: MessageID, pinnedTimestamp: Date): Promise<void>;

	abstract removeChannelPin(channelId: ChannelID, messageId: MessageID): Promise<void>;

	abstract listMessageReactions(channelId: ChannelID, messageId: MessageID): Promise<Array<MessageReaction>>;

	abstract listReactionUsers(
		channelId: ChannelID,
		messageId: MessageID,
		emojiName: string,
		limit?: number,
		after?: UserID,
		emojiId?: EmojiID,
	): Promise<Array<MessageReaction>>;

	abstract addReaction(
		channelId: ChannelID,
		messageId: MessageID,
		userId: UserID,
		emojiName: string,
		emojiId?: EmojiID,
		emojiAnimated?: boolean,
	): Promise<MessageReaction>;

	abstract removeReaction(
		channelId: ChannelID,
		messageId: MessageID,
		userId: UserID,
		emojiName: string,
		emojiId?: EmojiID,
	): Promise<void>;

	abstract removeAllReactions(channelId: ChannelID, messageId: MessageID): Promise<void>;

	abstract removeAllReactionsForEmoji(
		channelId: ChannelID,
		messageId: MessageID,
		emojiName: string,
		emojiId?: EmojiID,
	): Promise<void>;

	abstract countReactionUsers(
		channelId: ChannelID,
		messageId: MessageID,
		emojiName: string,
		emojiId?: EmojiID,
	): Promise<number>;

	abstract countUniqueReactions(channelId: ChannelID, messageId: MessageID): Promise<number>;

	abstract checkUserReactionExists(
		channelId: ChannelID,
		messageId: MessageID,
		userId: UserID,
		emojiName: string,
		emojiId?: EmojiID,
	): Promise<boolean>;

	abstract setHasReaction(channelId: ChannelID, messageId: MessageID, hasReaction: boolean): Promise<void>;
}
