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

import React from 'react';
import type {AutocompleteOption} from '~/components/channel/Autocomplete';

export function useTextareaAutocompleteKeyboard({
	isAutocompleteAttached,
	autocompleteOptions,
	selectedIndex,
	setSelectedIndex,
	handleSelect,
}: {
	isAutocompleteAttached: boolean;
	autocompleteOptions: Array<AutocompleteOption>;
	selectedIndex: number;
	setSelectedIndex: React.Dispatch<React.SetStateAction<number>>;
	handleSelect: (option: AutocompleteOption) => void;
}) {
	const handleKeyDown = React.useCallback(
		(event: React.KeyboardEvent<HTMLTextAreaElement>) => {
			if (!isAutocompleteAttached || !autocompleteOptions.length) {
				return;
			}

			if (event.key === 'ArrowUp' || event.key === 'ArrowDown') {
				event.preventDefault();
				setSelectedIndex((prevIndex) => {
					const newIndex = event.key === 'ArrowUp' ? prevIndex - 1 : prevIndex + 1;
					return (newIndex + autocompleteOptions.length) % autocompleteOptions.length;
				});
			} else if (event.key === 'Tab' || event.key === 'Enter') {
				event.preventDefault();
				const selectedOption = autocompleteOptions[selectedIndex];
				if (selectedOption) {
					handleSelect(selectedOption);
				}
			}
		},
		[isAutocompleteAttached, autocompleteOptions, selectedIndex, setSelectedIndex, handleSelect],
	);

	return {handleKeyDown};
}
