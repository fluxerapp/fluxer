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

import {observer} from 'mobx-react-lite';
import type React from 'react';
import type {AutocompleteOption} from './Autocomplete';
import {isSticker} from './Autocomplete';
import styles from './AutocompleteEmoji.module.css';
import {AutocompleteItem} from './AutocompleteItem';

export const AutocompleteSticker = observer(
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
		const stickers = options.filter(isSticker);
		return stickers.map((option, index) => (
			<AutocompleteItem
				key={option.sticker.id}
				name={option.sticker.name}
				description={
					option.sticker.tags.length > 0 ? option.sticker.tags.join(', ') : option.sticker.description || undefined
				}
				icon={
					<div className={styles.stickerIconWrapper}>
						<img draggable={false} className={styles.stickerIcon} src={option.sticker.url} alt={option.sticker.name} />
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
