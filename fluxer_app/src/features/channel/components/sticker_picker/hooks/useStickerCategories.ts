// SPDX-License-Identifier: AGPL-3.0-or-later

import {groupByGuildInGuildListOrder} from '@app/features/channel/components/pickers/shared/PickerRows';
import StickerPicker from '@app/features/emoji/state/StickerPicker';
import type {GuildSticker} from '@app/features/expressions/models/GuildSticker';
import GuildList from '@app/features/guild/state/GuildList';
import SelectedGuild from '@app/features/navigation/state/SelectedGuild';
import {useMemo} from 'react';

export function useStickerCategories(
	allStickers: ReadonlyArray<GuildSticker>,
	_renderedStickers: ReadonlyArray<GuildSticker>,
) {
	const guilds = GuildList.guilds;
	const selectedGuildId = SelectedGuild.selectedGuildId;
	const stickerPickerState = StickerPicker;
	const favoriteStickers = useMemo(() => {
		return StickerPicker.getFavoriteStickers(allStickers);
	}, [allStickers, stickerPickerState.favoriteStickers]);
	const frequentlyUsedStickers = useMemo(() => {
		return StickerPicker.getFrecentStickers(allStickers, 42);
	}, [allStickers, stickerPickerState.stickerUsage]);
	const stickersByGuildId = useMemo(
		() => groupByGuildInGuildListOrder(allStickers),
		[allStickers, guilds, selectedGuildId],
	);
	return {
		favoriteStickers,
		frequentlyUsedStickers,
		stickersByGuildId,
	};
}
