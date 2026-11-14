// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/AutocompleteChannel.module.css';
import {AutocompleteItem} from '@app/features/channel/components/AutocompleteItem';
import type {AutocompleteListProps} from '@app/features/channel/components/AutocompleteTypes';
import {isChannel, isGuildNavigation} from '@app/features/channel/components/AutocompleteTypes';
import * as ChannelUtils from '@app/features/channel/utils/ChannelUtils';
import * as HighlightCommands from '@app/features/messaging/commands/HighlightCommands';
import {ShieldIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';

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
	}: AutocompleteListProps) => {
		const rowRef = (index: number) =>
			rowRefs
				? (node: HTMLButtonElement | null) => {
						rowRefs.current[index] = node;
					}
				: undefined;
		return options.map((option, index) => {
			if (isGuildNavigation(option)) {
				return (
					<AutocompleteItem
						key={`navigation-${option.navigation}`}
						id={getOptionId?.(index)}
						icon={<ShieldIcon className={styles.channelIcon} data-flx="channel.autocomplete-channel.shield-icon" />}
						name={option.label}
						isKeyboardSelected={index === keyboardFocusIndex}
						isHovered={index === hoverIndex}
						onSelect={() => onSelect(option)}
						onMouseEnter={() => onMouseEnter(index)}
						onMouseLeave={onMouseLeave}
						innerRef={rowRef(index)}
						data-flx="channel.autocomplete-channel.autocomplete-item.select-navigation"
					/>
				);
			}
			if (!isChannel(option)) {
				return null;
			}
			return (
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
					innerRef={rowRef(index)}
					data-flx="channel.autocomplete-channel.autocomplete-item.select"
				/>
			);
		});
	},
);
