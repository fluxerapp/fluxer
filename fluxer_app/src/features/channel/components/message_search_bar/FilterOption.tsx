// SPDX-License-Identifier: AGPL-3.0-or-later

import {AutocompleteOption} from '@app/features/channel/components/message_search_bar/AutocompleteOption';

import styles from '@app/features/channel/components/message_search_bar/MessageSearchBar.module.css';

import type {SearchFilterOption} from '@app/features/search/utils/SearchUtils';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {FunnelIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const SEARCH_FILTERS_DESCRIPTOR = msg({
	message: 'Search filters',
	comment: 'Section header in the message search popout listing available filter operators.',
});

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
		return (
			<AutocompleteOption
				index={index}
				isSelected={isSelected}
				isHovered={isHovered}
				onSelect={onSelect}
				onMouseEnter={onMouseEnter}
				onMouseLeave={onMouseLeave}
				listboxId={listboxId}
				data-flx="channel.message-search-bar.filter-option.autocomplete-option"
			>
				<div className={styles.optionLabel} data-flx="channel.message-search-bar.filter-option.option-label">
					<div className={styles.optionContent} data-flx="channel.message-search-bar.filter-option.option-content">
						<div className={styles.optionText} data-flx="channel.message-search-bar.filter-option.option-text">
							<span className={styles.optionTitle} data-flx="channel.message-search-bar.filter-option.option-title">
								<span className={styles.searchFilter} data-flx="channel.message-search-bar.filter-option.search-filter">
									{option.label}
								</span>
								<span
									className={styles.optionDescription}
									data-flx="channel.message-search-bar.filter-option.option-description"
								>
									{option.description}
								</span>
							</span>
						</div>
					</div>
				</div>
			</AutocompleteOption>
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
		const {i18n} = useLingui();
		if (options.length === 0) return null;
		return (
			<div
				className={styles.popoutSection}
				data-flx="channel.message-search-bar.filter-option.filters-section.popout-section"
			>
				<div
					className={styles.popoutSectionHeader}
					data-flx="channel.message-search-bar.filter-option.filters-section.popout-section-header"
				>
					<span
						className={`${styles.flex} ${styles.itemsCenter} ${styles.gap2}`}
						data-flx="channel.message-search-bar.filter-option.filters-section.flex"
					>
						<FunnelIcon
							weight="regular"
							size={remFromPx(12)}
							data-flx="channel.message-search-bar.filter-option.filters-section.funnel-icon"
						/>
						<flx-i18n data-flx="channel.message-search-bar.filter-option.filters-section.flx-i18n">
							{title || i18n._(SEARCH_FILTERS_DESCRIPTOR)}
						</flx-i18n>
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
						data-flx="channel.message-search-bar.filter-option.filters-section.filter-option.select"
					/>
				))}
			</div>
		);
	},
);
