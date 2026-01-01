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
import type {GuildStickerRecord} from '~/records/GuildStickerRecord';
import GuildListStore from '~/stores/GuildListStore';
import StickerPickerStore from '~/stores/StickerPickerStore';

export const useStickerCategories = (renderedStickers: ReadonlyArray<GuildStickerRecord>) => {
	const guilds = GuildListStore.guilds;
	const stickerPickerState = StickerPickerStore;

	const favoriteStickers = React.useMemo(() => {
		return StickerPickerStore.getFavoriteStickers(renderedStickers);
	}, [renderedStickers, stickerPickerState.favoriteStickers]);

	const frequentlyUsedStickers = React.useMemo(() => {
		return StickerPickerStore.getFrecentStickers(renderedStickers, 42);
	}, [renderedStickers, stickerPickerState.stickerUsage]);

	const stickersByGuildId = React.useMemo(() => {
		const guildStickersMap = new Map<string, Array<GuildStickerRecord>>();

		for (const sticker of renderedStickers) {
			if (!guildStickersMap.has(sticker.guildId)) {
				guildStickersMap.set(sticker.guildId, []);
			}
			guildStickersMap.get(sticker.guildId)?.push(sticker);
		}

		const sortedGuildIds = guilds.map((guild) => guild.id);
		const sortedGuildStickersMap = new Map<string, ReadonlyArray<GuildStickerRecord>>();
		for (const guildId of sortedGuildIds) {
			if (guildStickersMap.has(guildId)) {
				sortedGuildStickersMap.set(guildId, guildStickersMap.get(guildId)!);
			}
		}

		return sortedGuildStickersMap;
	}, [renderedStickers, guilds]);

	return {
		favoriteStickers,
		frequentlyUsedStickers,
		stickersByGuildId,
	};
};
