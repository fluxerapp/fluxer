// SPDX-License-Identifier: AGPL-3.0-or-later
import {EMOJIS_PER_ROW} from '@app/features/channel/components/emoji_picker/EmojiPickerConstants';
import type {VirtualRow} from '@app/features/channel/components/emoji_picker/VirtualRow';
import {buildPickerRows} from '@app/features/channel/components/pickers/shared/PickerRows';
import EmojiPicker from '@app/features/emoji/state/EmojiPicker';
import type {FlatEmoji} from '@app/features/emoji/types/EmojiTypes';
import UnicodeEmojis from '@app/features/expressions/utils/UnicodeEmojis';
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
export function useVirtualRows(
	searchTerm: string,
	renderedEmojis: Array<FlatEmoji>,
	favoriteEmojis: Array<FlatEmoji>,
	frequentlyUsedEmojis: Array<FlatEmoji>,
	customEmojisByGuildId: Map<string, Array<FlatEmoji>>,
	unicodeEmojisByCategory: Map<string, Array<FlatEmoji>>,
	emojisPerRow: number = EMOJIS_PER_ROW,
) {
	const {i18n} = useLingui();
	const collapsedCategories = EmojiPicker.collapsedCategories;
	return useMemo(
		() =>
			buildPickerRows({
				searchTerm,
				rendered: renderedEmojis,
				favorites: favoriteEmojis,
				frequent: frequentlyUsedEmojis,
				byGuild: customEmojisByGuildId,
				perRow: emojisPerRow,
				isCollapsed: (category) => EmojiPicker.isCategoryCollapsed(category),
				favoritesLabel: i18n._(FAVORITED_DESCRIPTOR),
				frequentLabel: i18n._(FREQUENTLY_USED_DESCRIPTOR),
				extraSections: Array.from(unicodeEmojisByCategory, ([category, items]) => ({
					category,
					name: UnicodeEmojis.getCategoryLabel(category, i18n),
					items,
				})),
				createRow: (emojis, index, guildId): Exclude<VirtualRow, {type: 'header'}> => ({
					type: 'emoji-row',
					emojis,
					index,
					isCustomEmoji: Boolean(guildId),
					guildId,
				}),
			}),
		[
			searchTerm,
			renderedEmojis,
			favoriteEmojis,
			frequentlyUsedEmojis,
			customEmojisByGuildId,
			unicodeEmojisByCategory,
			emojisPerRow,
			collapsedCategories,
			i18n.locale,
		],
	);
}

export const getPickerRowItems = (row: VirtualRow) => (row.type === 'header' ? null : row.emojis);
