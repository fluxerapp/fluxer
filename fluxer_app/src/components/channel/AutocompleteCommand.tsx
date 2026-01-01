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

import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type {MutableRefObject} from 'react';

import {type AutocompleteOption, type Command, isCommand} from './Autocomplete';
import {AutocompleteItem} from './AutocompleteItem';

type Props = {
	onSelect: (option: AutocompleteOption) => void;
	keyboardFocusIndex: number;
	hoverIndex: number;
	options: Array<AutocompleteOption>;
	onMouseEnter: (index: number) => void;
	onMouseLeave: () => void;
	rowRefs?: MutableRefObject<Array<HTMLButtonElement | null>>;
};

export const AutocompleteCommand = observer(
	({onSelect, keyboardFocusIndex, hoverIndex, options, onMouseEnter, onMouseLeave, rowRefs}: Props) => {
		const {t} = useLingui();

		const getCommandDescription = (command: Command): string | undefined => {
			if (command.type === 'simple') {
				const content = command.content;
				return t`Appends ${content} to your message.`;
			}

			switch (command.name) {
				case '/nick':
					return t`Change your nickname in this community.`;
				case '/kick':
					return t`Kick a member from this community.`;
				case '/ban':
					return t`Ban a member from this community.`;
				case '/msg':
					return t`Send a direct message to a user.`;
				case '/saved':
					return t`Send a saved media item.`;
				case '/sticker':
					return t`Send a sticker.`;
				case '/me':
					return t`Send an action message (wraps in italics).`;
				case '/spoiler':
					return t`Send a spoiler message (wraps in spoiler tags).`;
				case '/gif':
					return t`Search for and send a GIF.`;
				case '/tenor':
					return t`Search for and send a GIF from Tenor.`;
				default:
					return undefined;
			}
		};

		const commands = options.filter(isCommand);

		return commands.map((option, index) => {
			const description = getCommandDescription(option.command);

			return (
				<AutocompleteItem
					key={option.command.name}
					name={option.command.name}
					description={description}
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
				/>
			);
		});
	},
);
