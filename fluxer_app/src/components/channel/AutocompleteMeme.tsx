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

import {MusicNoteIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {type AutocompleteOption, isMeme} from './Autocomplete';
import styles from './AutocompleteEmoji.module.css';
import {AutocompleteItem} from './AutocompleteItem';

export const AutocompleteMeme = observer(
	({
		onSelect,
		keyboardFocusIndex,
		hoverIndex,
		options,
		onMouseEnter,
		onMouseLeave,
		rowRefs,
	}: {
		onSelect: (option: AutocompleteOption) => void;
		keyboardFocusIndex: number;
		hoverIndex: number;
		options: Array<AutocompleteOption>;
		onMouseEnter: (index: number) => void;
		onMouseLeave: () => void;
		rowRefs?: React.MutableRefObject<Array<HTMLButtonElement | null>>;
	}) => {
		const memes = options.filter(isMeme);
		return memes.map((option, index) => (
			<AutocompleteItem
				key={option.meme.id}
				name={option.meme.name}
				description={option.meme.tags.length > 0 ? option.meme.tags.join(', ') : undefined}
				icon={
					<div className={styles.memeIconWrapper}>
						{option.meme.contentType.startsWith('video/') || option.meme.contentType.includes('gif') ? (
							<video src={option.meme.url} className={styles.memeVideo} muted autoPlay loop playsInline />
						) : option.meme.contentType.startsWith('audio/') ? (
							<div className={styles.audioIconWrapper}>
								<MusicNoteIcon className={styles.audioIcon} weight="fill" />
							</div>
						) : (
							<img draggable={false} className={styles.memeIcon} src={option.meme.url} alt={option.meme.name} />
						)}
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
			/>
		));
	},
);
