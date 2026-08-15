// SPDX-License-Identifier: AGPL-3.0-or-later

import {AutocompleteItem} from '@app/features/channel/components/AutocompleteItem';
import {type AutocompleteOption, isCommandOptionalAdd} from '@app/features/channel/components/AutocompleteTypes';
import type React from 'react';

interface AutocompleteCommandOptionalAddProps {
	onSelect: (option: AutocompleteOption) => void;
	keyboardFocusIndex: number;
	hoverIndex: number;
	options: Array<AutocompleteOption>;
	onMouseEnter: (index: number) => void;
	onMouseLeave: () => void;
	rowRefs?: React.MutableRefObject<Array<HTMLButtonElement | null>>;
	getOptionId?: (index: number) => string;
}

export function AutocompleteCommandOptionalAdd({
	onSelect,
	keyboardFocusIndex,
	hoverIndex,
	options,
	onMouseEnter,
	onMouseLeave,
	rowRefs,
	getOptionId,
}: AutocompleteCommandOptionalAddProps): React.ReactNode {
	const additions = options.filter(isCommandOptionalAdd);
	return additions.map((option, index) => (
		<AutocompleteItem
			key={option.name}
			id={getOptionId ? getOptionId(index) : undefined}
			name={option.name}
			description={option.description}
			isKeyboardSelected={index === keyboardFocusIndex}
			isHovered={index === hoverIndex}
			onSelect={() => onSelect(option)}
			onMouseEnter={() => onMouseEnter(index)}
			onMouseLeave={onMouseLeave}
			innerRef={
				rowRefs
					? (node: HTMLButtonElement | null) => {
							rowRefs.current[index] = node;
						}
					: undefined
			}
			data-flx="channel.autocomplete-command-optional-add.autocomplete-item.select"
		/>
	));
}
