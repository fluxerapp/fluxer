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

import type {ChannelID, EmojiID, MessageID, UserID} from '~/BrandedTypes';
import type {Message, MessageReaction} from '~/Models';

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
