// SPDX-License-Identifier: AGPL-3.0-or-later

import {AutocompleteItem} from '@app/features/channel/components/AutocompleteItem';
import {type AutocompleteOption, isCommandChoice} from '@app/features/channel/components/AutocompleteTypes';
import type React from 'react';

interface AutocompleteCommandChoiceProps {
	onSelect: (option: AutocompleteOption) => void;
	keyboardFocusIndex: number;
	hoverIndex: number;
	options: Array<AutocompleteOption>;
	onMouseEnter: (index: number) => void;
	onMouseLeave: () => void;
	rowRefs?: React.MutableRefObject<Array<HTMLButtonElement | null>>;
	getOptionId?: (index: number) => string;
}

export function AutocompleteCommandChoice({
	onSelect,
	keyboardFocusIndex,
	hoverIndex,
	options,
	onMouseEnter,
	onMouseLeave,
	rowRefs,
	getOptionId,
}: AutocompleteCommandChoiceProps): React.ReactNode {
	const choices = options.filter(isCommandChoice);
	return choices.map((option, index) => (
		<AutocompleteItem
			key={option.choice.value}
			id={getOptionId ? getOptionId(index) : undefined}
			name={option.choice.name}
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
			data-flx="channel.autocomplete-command-choice.autocomplete-item.select"
		/>
	));
}
