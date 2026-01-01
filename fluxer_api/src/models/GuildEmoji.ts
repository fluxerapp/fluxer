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

import type {GuildEmojiRow} from '~/database/CassandraTypes';
import type {EmojiID, GuildID, UserID} from '../BrandedTypes';

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
