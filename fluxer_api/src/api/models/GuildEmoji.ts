// SPDX-License-Identifier: AGPL-3.0-or-later

import type {EmojiID, GuildID, UserID} from '../BrandedTypes';
import type {GuildEmojiRow} from '../database/types/GuildTypes';

export class GuildEmoji {
	readonly guildId: GuildID;
	readonly id: EmojiID;
	readonly name: string;
	readonly creatorId: UserID;
	readonly isAnimated: boolean;
	readonly version: number;

	constructor(row: GuildEmojiRow) {
		this.guildId = row.guild_id;
		this.id = row.emoji_id;
		this.name = row.name;
		this.creatorId = row.creator_id;
		this.isAnimated = row.animated ?? false;
		this.version = row.version;
	}

	toRow(): GuildEmojiRow {
		return {
			guild_id: this.guildId,
			emoji_id: this.id,
			name: this.name,
			creator_id: this.creatorId,
			animated: this.isAnimated,
			version: this.version,
		};
	}
}
