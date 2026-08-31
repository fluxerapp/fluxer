// SPDX-License-Identifier: AGPL-3.0-or-later

import type {MessagePollAnswerVotersPage, MessagePollSelectedAnswer} from '@app/api/database/types/PollTypes';
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
		knownHasReaction?: boolean | null,
	): Promise<MessageReaction>;

	abstract removeReaction(
		channelId: ChannelID,
		messageId: MessageID,
		userId: UserID,
		emojiName: string,
		emojiId?: EmojiID,
	): Promise<void>;

	abstract getVoteAnswers(
		channelId: ChannelID,
		messageId: MessageID,
		userId: UserID,
	): Promise<Array<MessagePollSelectedAnswer>>;

	abstract getVotesForAnswer(
		channelId: ChannelID,
		messageId: MessageID,
		answerId: number,
		limit?: number,
		after?: UserID,
	): Promise<MessagePollAnswerVotersPage>;

	abstract addVote(channelId: ChannelID, messageId: MessageID, userId: UserID, answerId: number): Promise<void>;

	abstract removeVote(channelId: ChannelID, messageId: MessageID, userId: UserID, answerId: number): Promise<void>;

	abstract removeAllVotes(channelId: ChannelID, messageId: MessageID): Promise<void>;

	abstract removeAllVotesBulk(channelId: ChannelID, messageIds: Array<MessageID>): Promise<void>;

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
