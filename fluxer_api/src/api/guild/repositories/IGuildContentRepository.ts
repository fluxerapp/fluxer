// SPDX-License-Identifier: AGPL-3.0-or-later

import type {EmojiID, GuildID, StickerID} from '@app/api/BrandedTypes';
import type {GuildEmojiRow, GuildStickerRow} from '@app/api/database/types/GuildTypes';
import type {GuildEmoji} from '@app/api/models/GuildEmoji';
import type {GuildSticker} from '@app/api/models/GuildSticker';

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
