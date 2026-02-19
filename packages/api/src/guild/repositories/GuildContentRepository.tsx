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

import type {EmojiID, GuildID, StickerID} from '@fluxer/api/src/BrandedTypes';
import {
	BatchBuilder,
	buildPatchFromData,
	executeVersionedUpdate,
	fetchMany,
	fetchOne,
} from '@fluxer/api/src/database/Cassandra';
import {
	GUILD_EMOJI_COLUMNS,
	GUILD_STICKER_COLUMNS,
	type GuildEmojiRow,
	type GuildStickerRow,
} from '@fluxer/api/src/database/types/GuildTypes';
import {IGuildContentRepository} from '@fluxer/api/src/guild/repositories/IGuildContentRepository';
import {GuildEmoji} from '@fluxer/api/src/models/GuildEmoji';
import {GuildSticker} from '@fluxer/api/src/models/GuildSticker';
import {GuildEmojis, GuildEmojisByEmojiId, GuildStickers, GuildStickersByStickerId} from '@fluxer/api/src/Tables';

const FETCH_GUILD_EMOJIS_BY_GUILD_ID_QUERY = GuildEmojis.selectCql({
	where: GuildEmojis.where.eq('guild_id'),
});

const FETCH_GUILD_EMOJI_BY_ID_QUERY = GuildEmojis.selectCql({
	where: [GuildEmojis.where.eq('guild_id'), GuildEmojis.where.eq('emoji_id')],
	limit: 1,
});

const FETCH_GUILD_EMOJI_BY_EMOJI_ID_ONLY_QUERY = GuildEmojisByEmojiId.selectCql({
	where: GuildEmojisByEmojiId.where.eq('emoji_id'),
	limit: 1,
});

const FETCH_GUILD_STICKERS_BY_GUILD_ID_QUERY = GuildStickers.selectCql({
	where: GuildStickers.where.eq('guild_id'),
});

const FETCH_GUILD_STICKER_BY_ID_QUERY = GuildStickers.selectCql({
	where: [GuildStickers.where.eq('guild_id'), GuildStickers.where.eq('sticker_id')],
	limit: 1,
});

const FETCH_GUILD_STICKER_BY_STICKER_ID_ONLY_QUERY = GuildStickersByStickerId.selectCql({
	where: GuildStickersByStickerId.where.eq('sticker_id'),
	limit: 1,
});

export class GuildContentRepository extends IGuildContentRepository {
	async getEmoji(emojiId: EmojiID, guildId: GuildID): Promise<GuildEmoji | null> {
		const emoji = await fetchOne<GuildEmojiRow>(FETCH_GUILD_EMOJI_BY_ID_QUERY, {
			guild_id: guildId,
			emoji_id: emojiId,
		});
		return emoji ? new GuildEmoji(emoji) : null;
	}

	async getEmojiById(emojiId: EmojiID): Promise<GuildEmoji | null> {
		const emoji = await fetchOne<GuildEmojiRow>(FETCH_GUILD_EMOJI_BY_EMOJI_ID_ONLY_QUERY, {
			emoji_id: emojiId,
		});
		return emoji ? new GuildEmoji(emoji) : null;
	}

	async listEmojis(guildId: GuildID): Promise<Array<GuildEmoji>> {
		const emojis = await fetchMany<GuildEmojiRow>(FETCH_GUILD_EMOJIS_BY_GUILD_ID_QUERY, {
			guild_id: guildId,
		});
		return emojis.map((emoji) => new GuildEmoji(emoji));
	}

	async countEmojis(guildId: GuildID): Promise<number> {
		const emojis = await fetchMany<GuildEmojiRow>(FETCH_GUILD_EMOJIS_BY_GUILD_ID_QUERY, {
			guild_id: guildId,
		});
		return emojis.length;
	}

	async upsertEmoji(data: GuildEmojiRow, oldData?: GuildEmojiRow | null): Promise<GuildEmoji> {
		const guildId = data.guild_id;
		const emojiId = data.emoji_id;

		const result = await executeVersionedUpdate<GuildEmojiRow, 'guild_id' | 'emoji_id'>(
			async () =>
				fetchOne<GuildEmojiRow>(FETCH_GUILD_EMOJI_BY_ID_QUERY, {
					guild_id: guildId,
					emoji_id: emojiId,
				}),
			(current) => ({
				pk: {guild_id: guildId, emoji_id: emojiId},
				patch: buildPatchFromData(data, current, GUILD_EMOJI_COLUMNS, ['guild_id', 'emoji_id']),
			}),
			GuildEmojis,
			{initialData: oldData},
		);

		await fetchOne(GuildEmojisByEmojiId.insert(data));

		return new GuildEmoji({...data, version: result.finalVersion ?? 1});
	}

	async deleteEmoji(guildId: GuildID, emojiId: EmojiID): Promise<void> {
		const batch = new BatchBuilder();
		batch.addPrepared(
			GuildEmojis.deleteByPk({
				guild_id: guildId,
				emoji_id: emojiId,
			}),
		);
		batch.addPrepared(GuildEmojisByEmojiId.deleteByPk({emoji_id: emojiId}));
		await batch.execute();
	}

	async getSticker(stickerId: StickerID, guildId: GuildID): Promise<GuildSticker | null> {
		const sticker = await fetchOne<GuildStickerRow>(FETCH_GUILD_STICKER_BY_ID_QUERY, {
			guild_id: guildId,
			sticker_id: stickerId,
		});
		return sticker ? new GuildSticker(sticker) : null;
	}

	async getStickerById(stickerId: StickerID): Promise<GuildSticker | null> {
		const sticker = await fetchOne<GuildStickerRow>(FETCH_GUILD_STICKER_BY_STICKER_ID_ONLY_QUERY, {
			sticker_id: stickerId,
		});
		return sticker ? new GuildSticker(sticker) : null;
	}

	async listStickers(guildId: GuildID): Promise<Array<GuildSticker>> {
		const stickers = await fetchMany<GuildStickerRow>(FETCH_GUILD_STICKERS_BY_GUILD_ID_QUERY, {
			guild_id: guildId,
		});
		return stickers.map((sticker) => new GuildSticker(sticker));
	}

	async countStickers(guildId: GuildID): Promise<number> {
		const stickers = await fetchMany<GuildStickerRow>(FETCH_GUILD_STICKERS_BY_GUILD_ID_QUERY, {
			guild_id: guildId,
		});
		return stickers.length;
	}

	async upsertSticker(data: GuildStickerRow, oldData?: GuildStickerRow | null): Promise<GuildSticker> {
		const guildId = data.guild_id;
		const stickerId = data.sticker_id;

		const result = await executeVersionedUpdate<GuildStickerRow, 'guild_id' | 'sticker_id'>(
			async () =>
				fetchOne<GuildStickerRow>(FETCH_GUILD_STICKER_BY_ID_QUERY, {
					guild_id: guildId,
					sticker_id: stickerId,
				}),
			(current) => ({
				pk: {guild_id: guildId, sticker_id: stickerId},
				patch: buildPatchFromData(data, current, GUILD_STICKER_COLUMNS, ['guild_id', 'sticker_id']),
			}),
			GuildStickers,
			{initialData: oldData},
		);

		await fetchOne(GuildStickersByStickerId.insert(data));

		return new GuildSticker({...data, version: result.finalVersion ?? 1});
	}

	async deleteSticker(guildId: GuildID, stickerId: StickerID): Promise<void> {
		const batch = new BatchBuilder();
		batch.addPrepared(
			GuildStickers.deleteByPk({
				guild_id: guildId,
				sticker_id: stickerId,
			}),
		);
		batch.addPrepared(GuildStickersByStickerId.deleteByPk({sticker_id: stickerId}));
		await batch.execute();
	}
}
