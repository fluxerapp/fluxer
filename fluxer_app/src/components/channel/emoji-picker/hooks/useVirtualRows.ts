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

import {useLingui} from '@lingui/react/macro';
import React from 'react';
import {EMOJIS_PER_ROW} from '~/components/channel/emoji-picker/EmojiPickerConstants';
import type {VirtualRow} from '~/components/channel/emoji-picker/VirtualRow';
import UnicodeEmojis from '~/lib/UnicodeEmojis';
import EmojiPickerStore from '~/stores/EmojiPickerStore';
import type {Emoji} from '~/stores/EmojiStore';
import GuildStore from '~/stores/GuildStore';

export const useVirtualRows = (
	searchTerm: string,
	renderedEmojis: Array<Emoji>,
	favoriteEmojis: Array<Emoji>,
	frequentlyUsedEmojis: Array<Emoji>,
	customEmojisByGuildId: Map<string, Array<Emoji>>,
	unicodeEmojisByCategory: Map<string, Array<Emoji>>,
	emojisPerRow: number = EMOJIS_PER_ROW,
) => {
	const {t, i18n} = useLingui();
	const collapsedCategories = EmojiPickerStore.collapsedCategories;

	return React.useMemo(() => {
		const rows: Array<VirtualRow> = [];
		let currentIndex = 0;

		if (searchTerm) {
			for (let i = 0; i < renderedEmojis.length; i += emojisPerRow) {
				rows.push({
					type: 'emoji-row',
					emojis: renderedEmojis.slice(i, i + emojisPerRow),
					index: currentIndex++,
				});
			}
		} else {
			if (favoriteEmojis.length > 0) {
				const isFavoritesCollapsed = EmojiPickerStore.isCategoryCollapsed('favorites');
				rows.push({
					type: 'header',
					category: 'favorites',
					name: t`Favorited`,
					index: currentIndex++,
				});

				if (!isFavoritesCollapsed) {
					for (let i = 0; i < favoriteEmojis.length; i += emojisPerRow) {
						rows.push({
							type: 'emoji-row',
							emojis: favoriteEmojis.slice(i, i + emojisPerRow),
							index: currentIndex++,
						});
					}
				}
			}

			if (frequentlyUsedEmojis.length > 0) {
				const isFrequentlyUsedCollapsed = EmojiPickerStore.isCategoryCollapsed('frequently-used');
				rows.push({
					type: 'header',
					category: 'frequently-used',
					name: t`Frequently Used`,
					index: currentIndex++,
				});

				if (!isFrequentlyUsedCollapsed) {
					for (let i = 0; i < frequentlyUsedEmojis.length; i += emojisPerRow) {
						rows.push({
							type: 'emoji-row',
							emojis: frequentlyUsedEmojis.slice(i, i + emojisPerRow),
							index: currentIndex++,
						});
					}
				}
			}

			for (const [guildId, emojis] of customEmojisByGuildId.entries()) {
				const guild = GuildStore.getGuild(guildId)!;
				const isGuildCollapsed = EmojiPickerStore.isCategoryCollapsed(guildId);

				rows.push({
					type: 'header',
					category: guildId,
					name: guild.name,
					guildId,
					index: currentIndex++,
				});

				if (!isGuildCollapsed) {
					for (let i = 0; i < emojis.length; i += emojisPerRow) {
						rows.push({
							type: 'emoji-row',
							emojis: emojis.slice(i, i + emojisPerRow),
							index: currentIndex++,
							isCustomEmoji: true,
							guildId,
						});
					}
				}
			}

			for (const [category, emojis] of unicodeEmojisByCategory.entries()) {
				const isCategoryCollapsed = EmojiPickerStore.isCategoryCollapsed(category);

				rows.push({
					type: 'header',
					category,
					name: UnicodeEmojis.getCategoryLabel(category, i18n),
					index: currentIndex++,
				});

				if (!isCategoryCollapsed) {
					for (let i = 0; i < emojis.length; i += emojisPerRow) {
						rows.push({
							type: 'emoji-row',
							emojis: emojis.slice(i, i + emojisPerRow),
							index: currentIndex++,
						});
					}
				}
			}
		}

		return rows;
	}, [
		searchTerm,
		renderedEmojis,
		favoriteEmojis,
		frequentlyUsedEmojis,
		customEmojisByGuildId,
		unicodeEmojisByCategory,
		emojisPerRow,
		collapsedCategories,
	]);
};
