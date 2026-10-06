// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, MessageID, UserID} from '@app/api/BrandedTypes';
import type {SavedMessageRow} from '@app/api/database/types/UserTypes';

export class SavedMessage {
	readonly userId: UserID;
	readonly channelId: ChannelID;
	readonly messageId: MessageID;
	readonly savedAt: Date;

	constructor(row: SavedMessageRow) {
		this.userId = row.user_id;
		this.channelId = row.channel_id;
		this.messageId = row.message_id;
		this.savedAt = row.saved_at;
	}

	toRow(): SavedMessageRow {
		return {
			user_id: this.userId,
			channel_id: this.channelId,
			message_id: this.messageId,
			saved_at: this.savedAt,
		};
	}
}
