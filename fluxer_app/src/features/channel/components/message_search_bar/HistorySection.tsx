// SPDX-License-Identifier: AGPL-3.0-or-later

import {AutocompleteOption} from '@app/features/channel/components/message_search_bar/AutocompleteOption';
import {FilterOption} from '@app/features/channel/components/message_search_bar/FilterOption';
import styles from '@app/features/channel/components/message_search_bar/MessageSearchBar.module.css';
import type {SearchHistoryEntry} from '@app/features/search/state/SearchHistory';
import SearchHistory from '@app/features/search/state/SearchHistory';
import {formatSearchHistoryEntryForStreamerMode} from '@app/features/search/utils/SearchPrivacyUtils';
import type {SearchFilterOption} from '@app/features/search/utils/SearchUtils';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {ClockIcon, FunnelIcon, TrashIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const SEARCH_FILTERS_DESCRIPTOR = msg({
	message: 'Search filters',
	comment: 'Section header in the message search popout listing available filter operators.',
});
const RECENT_SEARCHES_DESCRIPTOR = msg({
	message: 'Recent searches',
	comment: 'Section header in the message search popout listing previously-run searches.',
});
const CLEAR_DESCRIPTOR = msg({
	message: 'Clear',
	comment: 'Inline button in the Recent searches section that wipes the search history list.',
});

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
		const {i18n} = useLingui();
		const historyOptions = SearchHistory.search('', channelId).slice(0, 5);
		const commonFilters = filterOptions
			.filter((opt) => !opt.requiresGuild || isInGuild)
			.filter((opt) => !opt.key.startsWith('-'));
		return (
			<>
				<div className={styles.popoutSection} data-flx="channel.message-search-bar.history-section.popout-section">
					<div
						className={styles.popoutSectionHeader}
						data-flx="channel.message-search-bar.history-section.popout-section-header"
					>
						<span
							className={`${styles.flex} ${styles.itemsCenter} ${styles.gap2}`}
							data-flx="channel.message-search-bar.history-section.flex"
						>
							<FunnelIcon
								weight="regular"
								size={remFromPx(12)}
								data-flx="channel.message-search-bar.history-section.funnel-icon"
							/>
							{i18n._(SEARCH_FILTERS_DESCRIPTOR)}
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
							data-flx="channel.message-search-bar.history-section.filter-option.filter-select"
						/>
					))}
				</div>
				{historyOptions.length > 0 && (
					<div className={styles.popoutSection} data-flx="channel.message-search-bar.history-section.popout-section--2">
						<div
							className={styles.popoutSectionHeader}
							data-flx="channel.message-search-bar.history-section.popout-section-header--2"
						>
							<span
								className={`${styles.flex} ${styles.itemsCenter} ${styles.gap2}`}
								data-flx="channel.message-search-bar.history-section.flex--2"
							>
								<ClockIcon
									weight="regular"
									size={remFromPx(12)}
									data-flx="channel.message-search-bar.history-section.clock-icon"
								/>
								{i18n._(RECENT_SEARCHES_DESCRIPTOR)}
							</span>
							<button
								type="button"
								onClick={(e) => {
									e.preventDefault();
									e.stopPropagation();
									onHistoryClear();
								}}
								className={`${styles.flex} ${styles.itemsCenter} ${styles.gap1}`}
								data-flx="channel.message-search-bar.history-section.flex.prevent-default.button"
							>
								<TrashIcon
									weight="regular"
									size={remFromPx(10)}
									data-flx="channel.message-search-bar.history-section.trash-icon"
								/>
								{i18n._(CLEAR_DESCRIPTOR)}
							</button>
						</div>
						{historyOptions.map((entry: SearchHistoryEntry, index) => {
							const displayEntry = formatSearchHistoryEntryForStreamerMode(entry);
							return (
								<AutocompleteOption
									key={`${entry.query}:${entry.ts}`}
									index={commonFilters.length + index}
									isSelected={selectedIndex === commonFilters.length + index}
									isHovered={commonFilters.length + index === hoverIndex}
									onSelect={() => onSelect(entry)}
									onMouseEnter={() => onMouseEnter(commonFilters.length + index)}
									onMouseLeave={onMouseLeave}
									listboxId={listboxId}
									data-flx="channel.message-search-bar.history-section.autocomplete-option.select"
								>
									<div
										className={styles.optionLabel}
										data-flx="channel.message-search-bar.history-section.option-label"
									>
										<div
											className={styles.optionContent}
											data-flx="channel.message-search-bar.history-section.option-content"
										>
											<div
												className={styles.optionText}
												data-flx="channel.message-search-bar.history-section.option-text"
											>
												<span
													className={`${styles.optionTitle} ${styles.historyOptionTitle}`}
													data-flx="channel.message-search-bar.history-section.option-title"
												>
													{displayEntry.query}
												</span>
											</div>
										</div>
									</div>
								</AutocompleteOption>
							);
						})}
					</div>
				)}
			</>
		);
	},
);
