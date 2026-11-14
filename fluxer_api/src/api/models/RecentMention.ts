// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID, MessageID, UserID} from '@app/api/BrandedTypes';
import type {RecentMentionRow} from '@app/api/database/types/UserTypes';

export class RecentMention {
	readonly userId: UserID;
	readonly channelId: ChannelID;
	readonly messageId: MessageID;
	readonly guildId: GuildID;
	readonly isEveryone: boolean;
	readonly isRole: boolean;

	constructor(row: RecentMentionRow) {
		this.userId = row.user_id;
		this.channelId = row.channel_id;
		this.messageId = row.message_id;
		this.guildId = row.guild_id;
		this.isEveryone = row.is_everyone;
		this.isRole = row.is_role;
	}

	toRow(): RecentMentionRow {
		return {
			user_id: this.userId,
			channel_id: this.channelId,
			message_id: this.messageId,
			guild_id: this.guildId,
			is_everyone: this.isEveryone,
			is_role: this.isRole,
		};
	}
}
