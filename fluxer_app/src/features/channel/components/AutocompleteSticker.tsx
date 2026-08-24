// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AutocompleteOption} from '@app/features/channel/components/Autocomplete';
import {isSticker} from '@app/features/channel/components/Autocomplete';
import styles from '@app/features/channel/components/AutocompleteEmoji.module.css';
import {AutocompleteItem} from '@app/features/channel/components/AutocompleteItem';
import {useStickerAnimation} from '@app/features/emoji/hooks/useStickerAnimation';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const AutocompleteStickerIcon = observer(
	({id, name, animated, isInteracting}: {id: string; name: string; animated: boolean; isInteracting: boolean}) => {
		const {shouldAnimate} = useStickerAnimation({isInteracting, isAnimated: animated});
		return (
			<div className={styles.stickerIconWrapper} data-flx="channel.autocomplete-sticker.sticker-icon-wrapper">
				<img
					draggable={false}
					className={styles.stickerIcon}
					src={AvatarUtils.getStickerURL({id, animated: shouldAnimate, isAnimatable: animated, size: 320})}
					alt={name}
					aria-hidden={true}
					data-flx="channel.autocomplete-sticker.sticker-icon"
				/>
			</div>
		);
	},
);
export const AutocompleteSticker = observer(
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
		const stickers = options.filter(isSticker);
		return stickers.map((option, index) => (
			<AutocompleteItem
				key={option.sticker.id}
				id={getOptionId?.(index)}
				name={option.sticker.name}
				description={
					option.sticker.tags.length > 0 ? option.sticker.tags.join(', ') : option.sticker.description || undefined
				}
				icon={
					<AutocompleteStickerIcon
						id={option.sticker.id}
						name={option.sticker.name}
						animated={option.sticker.animated}
						isInteracting={index === keyboardFocusIndex || index === hoverIndex}
						data-flx="channel.autocomplete-sticker.autocomplete-sticker-icon"
					/>
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
				data-flx="channel.autocomplete-sticker.autocomplete-item.select"
			/>
		));
	},
);
