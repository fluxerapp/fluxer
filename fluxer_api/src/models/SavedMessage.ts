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

import type {SavedMessageRow} from '~/database/CassandraTypes';
import type {ChannelID, MessageID, UserID} from '../BrandedTypes';

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
