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
import {ClockIcon, FunnelIcon, TrashIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import type {SearchHistoryEntry} from '~/stores/SearchHistoryStore';
import SearchHistoryStore from '~/stores/SearchHistoryStore';
import type {SearchFilterOption} from '~/utils/SearchUtils';
import {AutocompleteOption} from './AutocompleteOption';
import {FilterOption} from './FilterOption';
import styles from './MessageSearchBar.module.css';

interface HistorySectionProps {
	selectedIndex: number;
	hoverIndex: number;
	onSelect: (entry: SearchHistoryEntry) => void;
	onMouseEnter: (index: number) => void;
	onMouseLeave?: () => void;
	listboxId: string;
	isInGuild: boolean;
	channelId?: string;
	onHistoryClear: () => void;
	onFilterSelect: (filter: SearchFilterOption, index: number) => void;
	onFilterMouseEnter: (index: number) => void;
	onFilterMouseLeave?: () => void;
	filterOptions: Array<SearchFilterOption>;
}

export const HistorySection: React.FC<HistorySectionProps> = observer(
	({
		selectedIndex,
		hoverIndex,
		onSelect,
		onMouseEnter,
		onMouseLeave,
		listboxId,
		isInGuild,
		channelId,
		onHistoryClear,
		onFilterSelect,
		onFilterMouseEnter,
		onFilterMouseLeave,
		filterOptions,
	}) => {
		const {t} = useLingui();

		const historyOptions = SearchHistoryStore.search('', channelId).slice(0, 5);
		const commonFilters = filterOptions
			.filter((opt) => !opt.requiresGuild || isInGuild)
			.filter((opt) => !opt.key.startsWith('-'));

		return (
			<>
				<div className={styles.popoutSection}>
					<div className={styles.popoutSectionHeader}>
						<span className={`${styles.flex} ${styles.itemsCenter} ${styles.gap2}`}>
							<FunnelIcon weight="regular" size={12} />
							{t`Search Filters`}
						</span>
					</div>
					{commonFilters.map((option: SearchFilterOption, index) => (
						<FilterOption
							key={option.key}
							option={option}
							index={index}
							isSelected={selectedIndex === index}
							isHovered={index === hoverIndex}
							onSelect={() => onFilterSelect(option, index)}
							onMouseEnter={() => onFilterMouseEnter(index)}
							onMouseLeave={onFilterMouseLeave}
							listboxId={listboxId}
						/>
					))}
				</div>

				{historyOptions.length > 0 && (
					<div className={styles.popoutSection}>
						<div className={styles.popoutSectionHeader}>
							<span className={`${styles.flex} ${styles.itemsCenter} ${styles.gap2}`}>
								<ClockIcon weight="regular" size={12} />
								{t`Recent Searches`}
							</span>
							<button
								type="button"
								onClick={(e) => {
									e.preventDefault();
									e.stopPropagation();
									onHistoryClear();
								}}
								className={`${styles.flex} ${styles.itemsCenter} ${styles.gap1}`}
							>
								<TrashIcon weight="regular" size={10} />
								{t`Clear`}
							</button>
						</div>
						{historyOptions.map((entry: SearchHistoryEntry, index) => (
							<AutocompleteOption
								key={`${entry.query}:${entry.ts}`}
								index={commonFilters.length + index}
								isSelected={selectedIndex === commonFilters.length + index}
								isHovered={commonFilters.length + index === hoverIndex}
								onSelect={() => onSelect(entry)}
								onMouseEnter={() => onMouseEnter(commonFilters.length + index)}
								onMouseLeave={onMouseLeave}
								listboxId={listboxId}
							>
								<div className={styles.optionLabel}>
									<div className={styles.optionContent}>
										<div className={styles.optionText}>
											<span className={`${styles.optionTitle} ${styles.historyOptionTitle}`}>{entry.query}</span>
										</div>
									</div>
								</div>
							</AutocompleteOption>
						))}
					</div>
				)}
			</>
		);
	},
);
