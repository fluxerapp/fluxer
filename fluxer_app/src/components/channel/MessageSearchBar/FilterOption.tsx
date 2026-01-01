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
import {FunnelIcon, PlusIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import type {SearchFilterOption} from '~/utils/SearchUtils';
import styles from './MessageSearchBar.module.css';

interface FilterOptionProps {
	option: SearchFilterOption;
	index: number;
	isSelected: boolean;
	isHovered: boolean;
	onSelect: () => void;
	onMouseEnter: () => void;
	onMouseLeave?: () => void;
	listboxId: string;
}

export const FilterOption: React.FC<FilterOptionProps> = observer(
	({option, index, isSelected, isHovered, onSelect, onMouseEnter, onMouseLeave, listboxId}) => {
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
				<div className={styles.optionLabel}>
					<div className={styles.optionContent}>
						<div className={styles.optionText}>
							<span className={styles.optionTitle}>
								<span className={styles.searchFilter}>{option.label}</span>
								<span className={styles.optionDescription}> — {option.description}</span>
							</span>
						</div>
					</div>
				</div>
				<PlusIcon
					weight="bold"
					className={`${styles.optionMetaIcon} ${showIcon ? '' : styles.optionMetaIconInactive}`}
				/>
			</div>
		);
	},
);

interface FiltersSectionProps {
	options: Array<SearchFilterOption>;
	selectedIndex: number;
	hoverIndex: number;
	onSelect: (option: SearchFilterOption) => void;
	onMouseEnter: (index: number) => void;
	onMouseLeave?: () => void;
	listboxId: string;
	title?: string;
}

export const FiltersSection: React.FC<FiltersSectionProps> = observer(
	({options, selectedIndex, hoverIndex, onSelect, onMouseEnter, onMouseLeave, listboxId, title}) => {
		const {t} = useLingui();
		if (options.length === 0) return null;

		return (
			<div className={styles.popoutSection}>
				<div className={styles.popoutSectionHeader}>
					<span className={`${styles.flex} ${styles.itemsCenter} ${styles.gap2}`}>
						<FunnelIcon weight="regular" size={12} />
						{title || t`Search Filters`}
					</span>
				</div>
				{options.map((option: SearchFilterOption, index) => (
					<FilterOption
						key={option.key}
						option={option}
						index={index}
						isSelected={index === selectedIndex}
						onSelect={() => onSelect(option)}
						isHovered={index === hoverIndex}
						onMouseEnter={() => onMouseEnter(index)}
						onMouseLeave={onMouseLeave}
						listboxId={listboxId}
					/>
				))}
			</div>
		);
	},
);
