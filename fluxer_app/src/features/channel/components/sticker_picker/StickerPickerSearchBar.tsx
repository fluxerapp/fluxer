// SPDX-License-Identifier: AGPL-3.0-or-later

import {PickerSearchInput} from '@app/features/channel/components/shared/PickerSearchInput';
import {
	type PickerGridKeyboardOptions,
	usePickerGridKeyboard,
} from '@app/features/channel/components/shared/usePickerGridKeyboard';
import type {GuildSticker} from '@app/features/expressions/models/GuildSticker';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const FIND_THE_PERFECT_STICKER_DESCRIPTOR = msg({
	message: 'Find the perfect sticker',
	comment: 'Label in the channel and chat sticker picker search bar.',
});

interface StickerPickerSearchBarProps extends PickerGridKeyboardOptions {
	searchTerm: string;
	setSearchTerm: (term: string) => void;
	hoveredSticker: GuildSticker | null;
	inputRef?: React.RefObject<HTMLInputElement | null> | React.RefObject<HTMLInputElement>;
}

export const StickerPickerSearchBar = observer(
	({searchTerm, setSearchTerm, hoveredSticker, inputRef, ...keyboard}: StickerPickerSearchBarProps) => {
		const {i18n} = useLingui();
		const handleKeyDown = usePickerGridKeyboard(keyboard);
		const placeholder = hoveredSticker ? hoveredSticker.name : i18n._(FIND_THE_PERFECT_STICKER_DESCRIPTOR);
		return (
			<PickerSearchInput
				value={searchTerm}
				onChange={setSearchTerm}
				placeholder={placeholder}
				inputRef={inputRef}
				onKeyDown={handleKeyDown}
				data-flx="channel.sticker-picker.sticker-picker-search-bar.picker-search-input.set-search-term"
			/>
		);
	},
);
