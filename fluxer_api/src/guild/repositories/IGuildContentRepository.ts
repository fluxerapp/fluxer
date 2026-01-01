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

import type {EmojiID, GuildID, StickerID} from '~/BrandedTypes';
import type {GuildEmojiRow, GuildStickerRow} from '~/database/CassandraTypes';
import type {GuildEmoji, GuildSticker} from '~/Models';

export abstract class IGuildContentRepository {
	abstract getEmoji(emojiId: EmojiID, guildId: GuildID): Promise<GuildEmoji | null>;
	abstract getEmojiById(emojiId: EmojiID): Promise<GuildEmoji | null>;
	abstract listEmojis(guildId: GuildID): Promise<Array<GuildEmoji>>;
	abstract countEmojis(guildId: GuildID): Promise<number>;
	abstract upsertEmoji(data: GuildEmojiRow): Promise<GuildEmoji>;
	abstract deleteEmoji(guildId: GuildID, emojiId: EmojiID): Promise<void>;
	abstract getSticker(stickerId: StickerID, guildId: GuildID): Promise<GuildSticker | null>;
	abstract getStickerById(stickerId: StickerID): Promise<GuildSticker | null>;
	abstract listStickers(guildId: GuildID): Promise<Array<GuildSticker>>;
	abstract countStickers(guildId: GuildID): Promise<number>;
	abstract upsertSticker(data: GuildStickerRow): Promise<GuildSticker>;
	abstract deleteSticker(guildId: GuildID, stickerId: StickerID): Promise<void>;
}
