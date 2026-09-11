// SPDX-License-Identifier: AGPL-3.0-or-later

import {type AutocompleteOption, isChannel} from '@app/features/channel/components/Autocomplete';
import styles from '@app/features/channel/components/AutocompleteChannel.module.css';
import {AutocompleteItem} from '@app/features/channel/components/AutocompleteItem';
import * as ChannelUtils from '@app/features/channel/utils/ChannelUtils';
import * as HighlightCommands from '@app/features/messaging/commands/HighlightCommands';
import {observer} from 'mobx-react-lite';
import type React from 'react';

export const AutocompleteChannel = observer(
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
		rowRefs?: React.RefObject<Array<HTMLButtonElement | null>>;
		getOptionId?: (index: number) => string;
	}) => {
		const channels = options.filter(isChannel);
		return channels.map((option, index) => (
			<AutocompleteItem
				key={option.channel.id}
				id={getOptionId?.(index)}
				icon={ChannelUtils.getIcon(option.channel, {className: styles.channelIcon})}
				name={option.channel.name}
				isKeyboardSelected={index === keyboardFocusIndex}
				isHovered={index === hoverIndex}
				onSelect={() => onSelect(option)}
				onMouseEnter={() => {
					HighlightCommands.highlightChannel(option.channel.id);
					onMouseEnter(index);
				}}
				onMouseLeave={onMouseLeave}
				innerRef={
					rowRefs
						? (node) => {
								rowRefs.current[index] = node;
							}
						: undefined
				}
				data-flx="channel.autocomplete-channel.autocomplete-item.select"
			/>
		));
	},
);
