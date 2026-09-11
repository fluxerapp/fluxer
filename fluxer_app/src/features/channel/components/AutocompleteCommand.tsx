// SPDX-License-Identifier: AGPL-3.0-or-later

import {type AutocompleteOption, isCommand} from '@app/features/channel/components/Autocomplete';
import {AutocompleteItem} from '@app/features/channel/components/AutocompleteItem';
import {observer} from 'mobx-react-lite';
import type {RefObject} from 'react';

interface Props {
	onSelect: (option: AutocompleteOption) => void;
	keyboardFocusIndex: number;
	hoverIndex: number;
	options: Array<AutocompleteOption>;
	onMouseEnter: (index: number) => void;
	onMouseLeave: () => void;
	rowRefs?: RefObject<Array<HTMLButtonElement | null>>;
	getOptionId?: (index: number) => string;
}

export const AutocompleteCommand = observer(
	({onSelect, keyboardFocusIndex, hoverIndex, options, onMouseEnter, onMouseLeave, rowRefs, getOptionId}: Props) => {
		const commands = options.filter(isCommand);
		return commands.map((option, index) => (
			<AutocompleteItem
				key={option.command.name}
				id={getOptionId?.(index)}
				name={option.command.name}
				description={option.command.description}
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
				data-flx="channel.autocomplete-command.autocomplete-item.select"
			/>
		));
	},
);
