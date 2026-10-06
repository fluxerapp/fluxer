// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, MessageID, UserID} from '@app/api/BrandedTypes';
import type {ReadStateRow} from '@app/api/database/types/ChannelTypes';

type ReadStateSourceRow = ReadStateRow & {version?: bigint | number | null};

export class ReadState {
	readonly userId: UserID;
	readonly channelId: ChannelID;
	readonly lastMessageId: MessageID | null;
	readonly mentionCount: number;
	readonly lastPinTimestamp: Date | null;
	readonly version: bigint;

	constructor(row: ReadStateSourceRow) {
		this.userId = row.user_id;
		this.channelId = row.channel_id;
		this.lastMessageId = row.message_id ?? null;
		this.mentionCount = row.mention_count ?? 0;
		this.lastPinTimestamp = row.last_pin_timestamp ?? null;
		this.version = row.version == null ? 0n : BigInt(row.version);
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
