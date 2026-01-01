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

import type {GuildStickerRow} from '~/database/CassandraTypes';
import type {GuildID, StickerID, UserID} from '../BrandedTypes';

export class GuildSticker {
	readonly guildId: GuildID;
	readonly id: StickerID;
	readonly name: string;
	readonly description: string | null;
	readonly formatType: number;
	readonly tags: Array<string>;
	readonly creatorId: UserID;
	readonly version: number;

	constructor(row: GuildStickerRow) {
		this.guildId = row.guild_id;
		this.id = row.sticker_id;
		this.name = row.name;
		this.description = row.description ?? null;
		this.formatType = row.format_type;
		this.tags = row.tags ?? [];
		this.creatorId = row.creator_id;
		this.version = row.version;
	}

	toRow(): GuildStickerRow {
		return {
			guild_id: this.guildId,
			sticker_id: this.id,
			name: this.name,
			description: this.description,
			format_type: this.formatType,
			tags: this.tags.length > 0 ? this.tags : null,
			creator_id: this.creatorId,
			version: this.version,
		};
	}
}
