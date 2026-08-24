// SPDX-License-Identifier: AGPL-3.0-or-later

import {type AutocompleteOption, isMeme} from '@app/features/channel/components/Autocomplete';
import styles from '@app/features/channel/components/AutocompleteEmoji.module.css';
import {AutocompleteItem} from '@app/features/channel/components/AutocompleteItem';
import {AutocompleteMemePreview} from '@app/features/channel/components/AutocompleteMemePreview';
import {observer} from 'mobx-react-lite';
import type React from 'react';

export const AutocompleteMeme = observer(
	({
		onSelect,
		keyboardFocusIndex,
		hoverIndex,
		options,
		onMouseEnter,
		onMouseLeave,
		rowRefs,
		getOptionId,
	}: {
		onSelect: (option: AutocompleteOption) => void;
		keyboardFocusIndex: number;
		hoverIndex: number;
		options: Array<AutocompleteOption>;
		onMouseEnter: (index: number) => void;
		onMouseLeave: () => void;
		rowRefs?: React.MutableRefObject<Array<HTMLButtonElement | null>>;
		getOptionId?: (index: number) => string;
	}) => {
		const memes = options.filter(isMeme);
		return memes.map((option, index) => (
			<AutocompleteItem
				key={option.meme.id}
				id={getOptionId?.(index)}
				name={option.meme.name}
				description={option.meme.tags.length > 0 ? option.meme.tags.join(', ') : undefined}
				icon={
					<div className={styles.memeIconWrapper} data-flx="channel.autocomplete-meme.meme-icon-wrapper">
						<AutocompleteMemePreview
							meme={option.meme}
							data-flx="channel.autocomplete-meme.autocomplete-meme-preview"
						/>
					</div>
				}
				isKeyboardSelected={index === keyboardFocusIndex}
				isHovered={index === hoverIndex}
				onSelect={() => onSelect(option)}
				onMouseEnter={() => onMouseEnter(index)}
				onMouseLeave={onMouseLeave}
				innerRef={
					rowRefs
						? (node) => {
								rowRefs.current[index] = node;
							}
						: undefined
				}
				data-flx="channel.autocomplete-meme.autocomplete-item.select"
			/>
		));
	},
);
