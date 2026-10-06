// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, EmojiID, MessageID, UserID} from '@app/api/BrandedTypes';
import type {MessageReactionRow} from '@app/api/database/types/MessageTypes';

export class MessageReaction {
	readonly channelId: ChannelID;
	readonly bucket: number;
	readonly messageId: MessageID;
	readonly userId: UserID;
	readonly emojiId: EmojiID;
	readonly emojiName: string;
	readonly isEmojiAnimated: boolean;
	readonly createdAt: Date | null;

	constructor(row: MessageReactionRow) {
		this.channelId = row.channel_id;
		this.bucket = row.bucket;
		this.messageId = row.message_id;
		this.userId = row.user_id;
		this.emojiId = row.emoji_id;
		this.emojiName = row.emoji_name;
		this.isEmojiAnimated = row.emoji_animated ?? false;
		this.createdAt = row.created_at ?? null;
	}

	toRow(): MessageReactionRow {
		return {
			channel_id: this.channelId,
			bucket: this.bucket,
			message_id: this.messageId,
			user_id: this.userId,
			emoji_id: this.emojiId,
			emoji_name: this.emojiName,
			emoji_animated: this.isEmojiAnimated,
			created_at: this.createdAt,
		};
	}
}
