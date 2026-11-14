// SPDX-License-Identifier: AGPL-3.0-or-later

import {AutocompleteItem} from '@app/features/channel/components/AutocompleteItem';
import type {AutocompleteListProps} from '@app/features/channel/components/AutocompleteTypes';
import {isCommand} from '@app/features/channel/components/AutocompleteTypes';
import {observer} from 'mobx-react-lite';

export const AutocompleteCommand = observer(
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
