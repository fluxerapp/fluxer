// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildID, StickerID, UserID} from '../BrandedTypes';
import type {GuildStickerRow} from '../database/types/GuildTypes';

export class GuildSticker {
	readonly guildId: GuildID;
	readonly id: StickerID;
	readonly name: string;
	readonly description: string | null;
	readonly animated: boolean;
	readonly tags: Array<string>;
	readonly creatorId: UserID;
	readonly version: number;

	constructor(row: GuildStickerRow) {
		this.guildId = row.guild_id;
		this.id = row.sticker_id;
		this.name = row.name;
		this.description = row.description ?? null;
		this.animated = row.animated;
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
			animated: this.animated,
			tags: this.tags.length > 0 ? this.tags : null,
			creator_id: this.creatorId,
			version: this.version,
		};
	}
}
