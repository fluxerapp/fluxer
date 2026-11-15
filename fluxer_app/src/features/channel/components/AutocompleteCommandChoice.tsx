// SPDX-License-Identifier: AGPL-3.0-or-later

import {AutocompleteItem} from '@app/features/channel/components/AutocompleteItem';
import type {AutocompleteListProps} from '@app/features/channel/components/AutocompleteTypes';
import {isCommandChoice} from '@app/features/channel/components/AutocompleteTypes';
import type React from 'react';

export function AutocompleteCommandChoice({
	onSelect,
	keyboardFocusIndex,
	hoverIndex,
	options,
	onMouseEnter,
	onMouseLeave,
	rowRefs,
	getOptionId,
}: AutocompleteListProps): React.ReactNode {
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
