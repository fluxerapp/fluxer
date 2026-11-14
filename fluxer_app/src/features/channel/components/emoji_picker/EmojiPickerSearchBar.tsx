// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/emoji_picker/EmojiPickerSearchBar.module.css';
import {SkinToneSelector} from '@app/features/channel/components/emoji_picker/SkinToneSelector';
import {PickerSearchInput} from '@app/features/channel/components/shared/PickerSearchInput';
import {
	type PickerGridKeyboardOptions,
	usePickerGridKeyboard,
} from '@app/features/channel/components/shared/usePickerGridKeyboard';
import type {FlatEmoji} from '@app/features/emoji/types/EmojiTypes';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const FIND_THE_EMOJI_OF_YOUR_DREAMS_DESCRIPTOR = msg({
	message: 'Find the emoji of your dreams',
	comment: 'Label in the channel and chat emoji picker search bar.',
});

interface EmojiPickerSearchBarProps extends PickerGridKeyboardOptions {
	searchTerm: string;
	setSearchTerm: (term: string) => void;
	hoveredEmoji: FlatEmoji | null;
	inputRef?: React.RefObject<HTMLInputElement | null> | React.RefObject<HTMLInputElement>;
}

export const EmojiPickerSearchBar = observer(
	({searchTerm, setSearchTerm, hoveredEmoji, inputRef, ...keyboard}: EmojiPickerSearchBarProps) => {
		const {i18n} = useLingui();
		const handleKeyDown = usePickerGridKeyboard(keyboard);
		const placeholder = hoveredEmoji
			? hoveredEmoji.allNamesString.toString()
			: i18n._(FIND_THE_EMOJI_OF_YOUR_DREAMS_DESCRIPTOR);
		return (
			<div className={styles.container} data-flx="channel.emoji-picker.emoji-picker-search-bar.container">
				<PickerSearchInput
					value={searchTerm}
					onChange={setSearchTerm}
					placeholder={placeholder}
					inputRef={inputRef}
					onKeyDown={handleKeyDown}
					data-flx="channel.emoji-picker.emoji-picker-search-bar.picker-search-input.set-search-term"
				/>
				<SkinToneSelector data-flx="channel.emoji-picker.emoji-picker-search-bar.skin-tone-selector" />
			</div>
		);
	},
);
