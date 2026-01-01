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

import React from 'react';
import UnicodeEmojis, {type UnicodeEmoji} from '~/lib/UnicodeEmojis';
import EmojiPickerStore from '~/stores/EmojiPickerStore';
import type {Emoji} from '~/stores/EmojiStore';
import GuildListStore from '~/stores/GuildListStore';

export const useEmojiCategories = (renderedEmojis: Array<Emoji>) => {
	const guilds = GuildListStore.guilds;

	const favoriteEmojis = EmojiPickerStore.getFavoriteEmojis(renderedEmojis);

	const frequentlyUsedEmojis = EmojiPickerStore.getFrecentEmojis(renderedEmojis, 42);

	const customEmojisByGuildId = React.useMemo(() => {
		const guildEmojis = renderedEmojis.filter((emoji) => emoji.guildId != null);
		const guildEmojisByGuildId = new Map<string, Array<Emoji>>();

		for (const guildEmoji of guildEmojis) {
			if (!guildEmojisByGuildId.has(guildEmoji.guildId!)) {
				guildEmojisByGuildId.set(guildEmoji.guildId!, []);
			}
			guildEmojisByGuildId.get(guildEmoji.guildId!)?.push(guildEmoji);
		}

		const sortedGuildIds = guilds.map((guild) => guild.id);
		const sortedGuildEmojisByGuildId = new Map<string, Array<Emoji>>();
		for (const guildId of sortedGuildIds) {
			if (guildEmojisByGuildId.has(guildId)) {
				sortedGuildEmojisByGuildId.set(guildId, guildEmojisByGuildId.get(guildId)!);
			}
		}

		return sortedGuildEmojisByGuildId;
	}, [renderedEmojis, guilds]);

	const unicodeEmojisByCategory = React.useMemo(() => {
		const unicodeEmojis = renderedEmojis.filter((emoji) => emoji.guildId == null);
		const unicodeEmojisByCategory = new Map<string, Array<Emoji>>();

		for (const emoji of unicodeEmojis) {
			const category = UnicodeEmojis.getCategoryForEmoji(emoji as UnicodeEmoji)!;
			if (!unicodeEmojisByCategory.has(category)) {
				unicodeEmojisByCategory.set(category, []);
			}
			unicodeEmojisByCategory.get(category)?.push(emoji);
		}

		const categories = UnicodeEmojis.getCategories();
		const sortedUnicodeEmojisByCategory = new Map<string, Array<Emoji>>();
		for (const category of categories) {
			if (unicodeEmojisByCategory.has(category)) {
				sortedUnicodeEmojisByCategory.set(
					category,
					unicodeEmojisByCategory.get(category)!.sort((a, b) => a.index! - b.index!),
				);
			}
		}

		return sortedUnicodeEmojisByCategory;
	}, [renderedEmojis]);

	return {
		favoriteEmojis,
		frequentlyUsedEmojis,
		customEmojisByGuildId,
		unicodeEmojisByCategory,
	};
};
