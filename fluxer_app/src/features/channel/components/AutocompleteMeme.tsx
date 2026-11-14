// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/AutocompleteEmoji.module.css';
import {AutocompleteItem} from '@app/features/channel/components/AutocompleteItem';
import {AutocompleteMemePreview} from '@app/features/channel/components/AutocompleteMemePreview';
import type {AutocompleteListProps} from '@app/features/channel/components/AutocompleteTypes';
import {isMeme} from '@app/features/channel/components/AutocompleteTypes';
import {observer} from 'mobx-react-lite';

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
	}: AutocompleteListProps) => {
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
