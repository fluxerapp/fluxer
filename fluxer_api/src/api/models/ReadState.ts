// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID, MessageID, UserID} from '@app/api/BrandedTypes';
import type {ReadStateRow} from '@app/api/database/types/ChannelTypes';

type ReadStateSourceRow = ReadStateRow & {version?: bigint | number | null};

export class ReadState {
	readonly userId: UserID;
	readonly channelId: ChannelID;
	readonly lastMessageId: MessageID | null;
	readonly mentionCount: number;
	readonly lastPinTimestamp: Date | null;
	readonly version: bigint;
	readonly flags: number | null;
	readonly guildId: GuildID | null;

	constructor(row: ReadStateSourceRow) {
		this.userId = row.user_id;
		this.channelId = row.channel_id;
		this.lastMessageId = row.message_id ?? null;
		this.mentionCount = row.mention_count ?? 0;
		this.lastPinTimestamp = row.last_pin_timestamp ?? null;
		this.version = row.version == null ? 0n : BigInt(row.version);
		this.flags = row.flags ?? null;
		this.guildId = row.guild_id ?? null;
	}

	get isMarked(): boolean {
		return this.guildId !== null;
	}

	toRow(): ReadStateRow {
		return {
			user_id: this.userId,
			channel_id: this.channelId,
			message_id: this.lastMessageId,
			mention_count: this.mentionCount,
			last_pin_timestamp: this.lastPinTimestamp,
			...(this.guildId !== null ? {flags: this.flags, guild_id: this.guildId} : {}),
		};
	}
}
