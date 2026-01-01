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

import type {MessageReactionRow} from '~/database/CassandraTypes';
import type {ChannelID, EmojiID, MessageID, UserID} from '../BrandedTypes';

export class MessageReaction {
	readonly channelId: ChannelID;
	readonly bucket: number;
	readonly messageId: MessageID;
	readonly userId: UserID;
	readonly emojiId: EmojiID;
	readonly emojiName: string;
	readonly isEmojiAnimated: boolean;

	constructor(row: MessageReactionRow) {
		this.channelId = row.channel_id;
		this.bucket = row.bucket;
		this.messageId = row.message_id;
		this.userId = row.user_id;
		this.emojiId = row.emoji_id;
		this.emojiName = row.emoji_name;
		this.isEmojiAnimated = row.emoji_animated ?? false;
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
		};
	}
}
