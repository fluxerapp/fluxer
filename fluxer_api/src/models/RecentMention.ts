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

import type {RecentMentionRow} from '~/database/CassandraTypes';
import type {ChannelID, GuildID, MessageID, UserID} from '../BrandedTypes';

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
