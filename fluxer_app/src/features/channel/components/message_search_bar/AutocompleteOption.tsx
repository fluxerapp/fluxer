// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/message_search_bar/MessageSearchBar.module.css';
import {isKeyboardActivationKey} from '@app/features/input/utils/KeyboardUtils';
import {PlusIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback} from 'react';

interface AutocompleteOptionProps {
	index: number;
	isSelected: boolean;
	isHovered: boolean;
	onSelect: () => void;
	onMouseEnter?: () => void;
	onMouseLeave?: () => void;
	children: React.ReactNode;
	listboxId: string;
	metaIcon?: React.ReactNode;
}

export const AutocompleteOption: React.FC<AutocompleteOptionProps> = observer(
	({index, isSelected, isHovered, onSelect, onMouseEnter, onMouseLeave, children, listboxId, metaIcon}) => {
		const handleKeyDown = useCallback(
			(e: React.KeyboardEvent) => {
				if (isKeyboardActivationKey(e.key)) {
					e.preventDefault();
					onSelect();
				}
			},
			[onSelect],
		);
		const isActive = isSelected || isHovered;
		const showIcon = isSelected || isHovered;
		return (
			<div
				role="option"
				id={`${listboxId}-opt-${index}`}
				aria-selected={isSelected}
				tabIndex={-1}
				onMouseEnter={onMouseEnter}
				onMouseLeave={onMouseLeave}
				onMouseDown={(ev) => {
					if (ev.button === 0) ev.preventDefault();
				}}
				onClick={onSelect}
				onKeyDown={handleKeyDown}
				className={`${styles.option} ${isActive ? styles.optionActive : ''} ${isSelected ? styles.optionKeyboardFocus : ''}`}
				data-flx="channel.message-search-bar.autocomplete-option.option.select"
			>
				{children}
				{metaIcon == null ? (
					<PlusIcon
						weight="bold"
						className={`${styles.optionMetaIcon} ${showIcon ? '' : styles.optionMetaIconInactive}`}
						data-flx="channel.message-search-bar.autocomplete-option.option-meta-icon"
					/>
				) : (
					<span
						className={styles.optionMetaIcon}
						data-flx="channel.message-search-bar.autocomplete-option.option-meta-slot"
					>
						{metaIcon}
					</span>
				)}
			</div>
		);
	},
);
