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

import {PlusIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import styles from './MessageSearchBar.module.css';

interface AutocompleteOptionProps {
	index: number;
	isSelected: boolean;
	isHovered: boolean;
	onSelect: () => void;
	onMouseEnter?: () => void;
	onMouseLeave?: () => void;
	children: React.ReactNode;
	listboxId: string;
}

export const AutocompleteOption: React.FC<AutocompleteOptionProps> = observer(
	({index, isSelected, isHovered, onSelect, onMouseEnter, onMouseLeave, children, listboxId}) => {
		const handleKeyDown = React.useCallback(
			(e: React.KeyboardEvent) => {
				if (e.key === 'Enter' || e.key === ' ') {
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
				tabIndex={isSelected ? 0 : -1}
				onMouseEnter={onMouseEnter}
				onMouseLeave={onMouseLeave}
				onMouseDown={(ev) => ev.preventDefault()}
				onClick={onSelect}
				onKeyDown={handleKeyDown}
				className={`${styles.option} ${isActive ? styles.optionActive : ''} ${isSelected ? styles.optionKeyboardFocus : ''}`}
			>
				{children}
				<PlusIcon
					weight="bold"
					className={`${styles.optionMetaIcon} ${showIcon ? '' : styles.optionMetaIconInactive}`}
				/>
			</div>
		);
	},
);
