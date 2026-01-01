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

import type {ReadStateRow} from '~/database/CassandraTypes';
import type {ChannelID, MessageID, UserID} from '../BrandedTypes';

export class ReadState {
	readonly userId: UserID;
	readonly channelId: ChannelID;
	readonly lastMessageId: MessageID | null;
	readonly mentionCount: number;
	readonly lastPinTimestamp: Date | null;

	constructor(row: ReadStateRow) {
		this.userId = row.user_id;
		this.channelId = row.channel_id;
		this.lastMessageId = row.message_id ?? null;
		this.mentionCount = row.mention_count ?? 0;
		this.lastPinTimestamp = row.last_pin_timestamp ?? null;
	}

	toRow(): ReadStateRow {
		return {
			user_id: this.userId,
			channel_id: this.channelId,
			message_id: this.lastMessageId,
			mention_count: this.mentionCount,
			last_pin_timestamp: this.lastPinTimestamp,
		};
	}
}
