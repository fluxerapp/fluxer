// SPDX-License-Identifier: AGPL-3.0-or-later

import {buildPickerRows} from '@app/features/channel/components/pickers/shared/PickerRows';
import StickerPicker from '@app/features/emoji/state/StickerPicker';
import type {GuildSticker} from '@app/features/expressions/models/GuildSticker';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {useMemo} from 'react';

const FAVORITED_DESCRIPTOR = msg({
	message: 'Favorited',
	comment: 'Short label in the use virtual rows hook. Keep it concise.',
});
const FREQUENTLY_USED_DESCRIPTOR = msg({
	message: 'Frequently used',
	comment: 'Short label in the use virtual rows hook. Keep it concise.',
});
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
			stickers: Array<GuildSticker>;
			index: number;
			guildId?: string;
	  };
export const useVirtualRows = (
	searchTerm: string,
	renderedStickers: ReadonlyArray<GuildSticker>,
	favoriteStickers: ReadonlyArray<GuildSticker>,
	frequentlyUsedStickers: ReadonlyArray<GuildSticker>,
	stickersByGuildId: ReadonlyMap<string, ReadonlyArray<GuildSticker>>,
	stickersPerRow: number = 4,
) => {
	const {i18n} = useLingui();
	const collapsedCategories = StickerPicker.collapsedCategories;
	return useMemo(
		() =>
			buildPickerRows({
				searchTerm,
				rendered: renderedStickers,
				favorites: favoriteStickers,
				frequent: frequentlyUsedStickers,
				byGuild: stickersByGuildId,
				perRow: stickersPerRow,
				isCollapsed: (category) => StickerPicker.isCategoryCollapsed(category),
				favoritesLabel: i18n._(FAVORITED_DESCRIPTOR),
				frequentLabel: i18n._(FREQUENTLY_USED_DESCRIPTOR),
				createRow: (stickers, index, guildId): Exclude<VirtualRow, {type: 'header'}> => ({
					type: 'sticker-row',
					stickers,
					index,
					guildId,
				}),
			}),
		[
			searchTerm,
			renderedStickers,
			favoriteStickers,
			frequentlyUsedStickers,
			stickersByGuildId,
			stickersPerRow,
			collapsedCategories,
			i18n.locale,
		],
	);
};

export const getPickerRowItems = (row: VirtualRow) => (row.type === 'header' ? null : row.stickers);
