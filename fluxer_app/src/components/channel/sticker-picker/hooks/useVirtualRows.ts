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
import type {GuildStickerRecord} from '~/records/GuildStickerRecord';
import GuildStore from '~/stores/GuildStore';
import StickerPickerStore from '~/stores/StickerPickerStore';

export type VirtualRow =
	| {
			type: 'header';
			category: string;
			name: string;
			guildId?: string;
			index: number;
	  }
	| {
			type: 'sticker-row';
			stickers: Array<GuildStickerRecord>;
			index: number;
			guildId?: string;
	  };

export const useVirtualRows = (
	searchTerm: string,
	renderedStickers: ReadonlyArray<GuildStickerRecord>,
	favoriteStickers: ReadonlyArray<GuildStickerRecord>,
	frequentlyUsedStickers: ReadonlyArray<GuildStickerRecord>,
	stickersByGuildId: ReadonlyMap<string, ReadonlyArray<GuildStickerRecord>>,
	stickersPerRow: number = 4,
) => {
	const {t} = useLingui();
	const collapsedCategories = StickerPickerStore.collapsedCategories;

	return React.useMemo(() => {
		const rows: Array<VirtualRow> = [];
		let currentIndex = 0;

		if (searchTerm) {
			for (let i = 0; i < renderedStickers.length; i += stickersPerRow) {
				rows.push({
					type: 'sticker-row',
					stickers: renderedStickers.slice(i, i + stickersPerRow),
					index: currentIndex++,
				});
			}
		} else {
			if (favoriteStickers.length > 0) {
				const isFavoritesCollapsed = StickerPickerStore.isCategoryCollapsed('favorites');
				rows.push({
					type: 'header',
					category: 'favorites',
					name: t`Favorited`,
					index: currentIndex++,
				});

				if (!isFavoritesCollapsed) {
					for (let i = 0; i < favoriteStickers.length; i += stickersPerRow) {
						rows.push({
							type: 'sticker-row',
							stickers: favoriteStickers.slice(i, i + stickersPerRow),
							index: currentIndex++,
						});
					}
				}
			}

			if (frequentlyUsedStickers.length > 0) {
				const isFrequentlyUsedCollapsed = StickerPickerStore.isCategoryCollapsed('frequently-used');
				rows.push({
					type: 'header',
					category: 'frequently-used',
					name: t`Frequently Used`,
					index: currentIndex++,
				});

				if (!isFrequentlyUsedCollapsed) {
					for (let i = 0; i < frequentlyUsedStickers.length; i += stickersPerRow) {
						rows.push({
							type: 'sticker-row',
							stickers: frequentlyUsedStickers.slice(i, i + stickersPerRow),
							index: currentIndex++,
						});
					}
				}
			}

			for (const [guildId, stickers] of stickersByGuildId.entries()) {
				const guild = GuildStore.getGuild(guildId)!;
				const isGuildCollapsed = StickerPickerStore.isCategoryCollapsed(guildId);

				rows.push({
					type: 'header',
					category: guildId,
					name: guild.name,
					guildId,
					index: currentIndex++,
				});

				if (!isGuildCollapsed) {
					for (let i = 0; i < stickers.length; i += stickersPerRow) {
						rows.push({
							type: 'sticker-row',
							stickers: stickers.slice(i, i + stickersPerRow),
							index: currentIndex++,
							guildId,
						});
					}
				}
			}
		}

		return rows;
	}, [
		searchTerm,
		renderedStickers,
		favoriteStickers,
		frequentlyUsedStickers,
		stickersByGuildId,
		stickersPerRow,
		collapsedCategories,
	]);
};
